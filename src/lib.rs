use std::{collections::HashMap, fmt::Display, io::stdout, path::{Path, PathBuf}, sync::mpsc::{Receiver, Sender}, time::Duration, usize};
use crossterm::{ExecutableCommand, QueueableCommand, cursor, event::{self, KeyCode, KeyEvent, KeyModifiers}, style::Print, terminal};
use notify::Watcher;
use unicode_segmentation::UnicodeSegmentation;

use crate::{bok::ParseError, ui::{Drawer, GridDirection, Logger, Rect, TextLayout, WrapMode, grapheme_count}};

pub mod ui;
pub mod bok;

pub struct FolderWatcher {
    sender: Sender<Result<notify::Event, notify::Error>>,
    receiver: Receiver<Result<notify::Event, notify::Error>>,
    path: PathBuf,
    _watcher: notify::RecommendedWatcher,
}

impl FolderWatcher {
    pub fn new(path: PathBuf) -> Result<Self, Error> {
        let (sender, receiver) = std::sync::mpsc::channel();
        let mut _watcher = notify::recommended_watcher(sender.clone())?;
        _watcher
            .watch(&path, notify::RecursiveMode::Recursive)?;

        Ok(Self { receiver, path, _watcher, sender })
    }

    pub fn changes(&self) -> Vec<FileEvent> {
        let mut events = Vec::new();
        for res in self.receiver.try_iter() {
            let Ok(notify::Event { kind, paths, .. }) = res else {
                continue;
            };
            for path in paths {
                if !path.is_file() {
                    continue;
                }
                events.push(match kind {
                    notify::EventKind::Any => FileEvent::Modify(path),
                    notify::EventKind::Access(_) => FileEvent::Modify(path),
                    notify::EventKind::Create(_) => FileEvent::Modify(path),
                    notify::EventKind::Modify(_) => FileEvent::Modify(path),
                    notify::EventKind::Remove(_) => FileEvent::Delete(path),
                    notify::EventKind::Other => FileEvent::Modify(path),
                });
            }
        }
        events
    }
    
    pub fn files(&self) -> Result<Vec<PathBuf>, Error> {
        let mut vec = Vec::new();
        list_files(&mut vec, &self.path)?;
        Ok(vec)
    }
    
    pub fn touch(&mut self, path: PathBuf) {
        let _ = self.sender.send(Ok(notify::Event::new(notify::EventKind::Modify(notify::event::ModifyKind::Any)).add_path(path)));
    }
}

fn list_files(vec: &mut Vec<PathBuf>, path: &Path) -> std::io::Result<()> {
    if path.is_dir() {
        let paths = std::fs::read_dir(&path)?;
        for path_result in paths {
            let full_path = path_result?.path();
            if full_path.is_dir() {
                list_files(vec, &full_path)?
            } else {
                vec.push(full_path);
            }
        }
    }
    Ok(())
}

pub enum FileEvent {
    Modify(PathBuf),
    Delete(PathBuf),
}

pub trait Config: Clone + PartialEq + serde::Serialize + for<'a> serde::Deserialize<'a> {
    fn new() -> Option<Self>;
}

pub struct ConfigLoader<C: Config> {
    config: C,
    path: PathBuf,
    receiver: Receiver<Result<notify::Event, notify::Error>>,
    _watcher: notify::RecommendedWatcher,
}

impl<C: Config> ConfigLoader<C> {
    pub fn new(ident: &str) -> Result<Self, Error> {
        let project_dirs = directories::ProjectDirs::from(
            "", 
            "", 
            ident
        ).ok_or(Error::NoProjectDir)?;
        let base_dir = project_dirs.config_dir().to_path_buf();
        std::fs::create_dir_all(&base_dir)?;

        let path = base_dir.join("config.toml");
        let config = match Self::load(&path) {
            Ok(Ok(config)) => config,
            _ => C::new().ok_or(Error::CancelledByUser)?,
        };
        Self::save(&path, &config)?;
        let (sender, receiver) = std::sync::mpsc::channel();
        let mut _watcher = notify::recommended_watcher(sender)?;
        _watcher
            .watch(&path, notify::RecursiveMode::NonRecursive)?;

        Ok(Self { config, path, receiver, _watcher })
    }

    fn load(path: &Path) -> Result<Result<C, toml::de::Error>, std::io::Error> {
        use std::io::Read;
        let mut buf = String::new();
        std::fs::File::open(&path)?.read_to_string(&mut buf)?;
        Ok(toml::from_str(&buf))
    }

    fn save(path: &Path, config: &C) -> Result<(), Error> {
        if let Ok(file) = toml::to_string_pretty(config) {
            std::fs::write(path, file)?;
        }
        Ok(())
    }

    pub fn set(&mut self, new: C) -> Result<(), Error> {
        if new != self.config {
            self.config = new;
            Self::save(&self.path, &self.config)?;
        }
        Ok(())
    }

    pub fn get(&mut self) -> Result<&C, Error> {
        for event in self.receiver.try_iter() {
            let Ok(event) = event else {
                continue;
            };
            for changed in event.paths {
                if self.path != changed {
                    continue;
                }
                if let Ok(Ok(new)) = Self::load(&self.path) {
                    self.config = new;
                }
                Self::save(&self.path, &self.config)?;
            }
        }
        Ok(&self.config)
    }
}

#[derive(Debug)]
pub enum Error {
    CancelledByUser,
    NoProjectDir,
    Notify(notify::Error),
    Io(std::io::Error),
    ParseError(ParseError),
    Trash(trash::Error),
}

impl From<ParseError> for Error {
    fn from(value: ParseError) -> Self {
        Self::ParseError(value)
    }
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Self::Io(err)
    }
}

impl From<trash::Error> for Error {
    fn from(err: trash::Error) -> Self {
        Self::Trash(err)
    }
}

impl From<notify::Error> for Error {
    fn from(err: notify::Error) -> Self {
        Self::Notify(err)
    }
}


fn init() -> std::io::Result<()> {
    stdout().execute(terminal::EnterAlternateScreen)?;
    stdout().execute(cursor::Hide)?;
    terminal::enable_raw_mode()?;
    Ok(())
}

fn exit() -> std::io::Result<()> {
    terminal::disable_raw_mode()?;
    stdout().execute(terminal::LeaveAlternateScreen)?;
    stdout().execute(crossterm::cursor::Show)?;
    Ok(())
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd)]
pub struct CommandSignature {
    key: KeyCode,
    hidden: bool,
    ctrl: bool,
}

impl Display for CommandSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.ctrl {
            f.write_str("^")?;
        }
        f.write_str(&format_key(self.key))?;
        Ok(())
    }
}

impl CommandSignature {
    pub fn new(key: KeyCode) -> Self {
        Self { key, hidden: false, ctrl: false }
    }

    pub fn hidden(self) -> Self {
        CommandSignature { hidden: true, ..self }
    }

    pub fn ctrl(self) -> Self {
        Self { ctrl: true, ..self }
    }
}

pub struct Commands<C: Display> {
    rects: Vec<CommandList<C>>,
}
impl<C: Display> Commands<C> {
    fn new() -> Self {
        Self { rects: Vec::new() }
    }
    
    fn reset(&mut self) {
        self.rects.clear();
    }

    pub fn new_rect(&mut self, commands: CommandList<C>) {
        self.rects.push(commands);
    }
    
    fn all(&self) -> Vec<(&CommandSignature, &C)> {
        let mut commands = Vec::new();
        for rect in &self.rects {
            commands.extend(rect.public.iter().map(|(s, c)| (s, &c.command)));
            commands.extend(rect.hidden.iter().map(|(s, c)| (s, &c.command)));
        }
        commands
    }

    
}

struct CommandEntry<C: Display> {
    command: C,
    i: usize,
}

pub struct CommandList<C: Display> {
    rect: Rect,
    public: HashMap<CommandSignature, CommandEntry<C>>,
    hidden: HashMap<CommandSignature, CommandEntry<C>>,
}

impl<C: Display> CommandList<C> {
    pub fn new(rect: Rect) -> Self {
        Self { public: HashMap::new(), hidden: HashMap::new(), rect }
    }

    fn with_public(self, signature: CommandSignature, command: C) -> Self {
        let mut public = self.public;
        public.insert(signature, CommandEntry { command, i: public.len() });
        Self { public, ..self }
    }

    fn with_hidden(self, signature: CommandSignature, command: C) -> Self {
        let mut hidden = self.hidden;
        hidden.insert(signature, CommandEntry { command, i: hidden.len() });
        Self { hidden, ..self }
    }

    pub fn command(self, key: KeyCode, command: C) -> Self {
        self.with_public(CommandSignature::new(key), command)
    }

    pub fn hidden(self, key: KeyCode, command: C) -> Self {
        self.with_hidden(CommandSignature::new(key), command)
    }

    pub fn ctrl(self, key: KeyCode, command: C) -> Self {
        self.with_public(CommandSignature::new(key).ctrl(), command)
    }

    pub fn hidden_ctrl(self, key: KeyCode, command: C) -> Self {
        self.with_hidden(CommandSignature::new(key).ctrl(), command)
    }
}

pub enum TextEditResponse {
    Escape,
    Return,
}

pub enum TextEditEvent {
    Char(char),
    Left,
    Right,
    Up,
    Down,
    Backspace { word: bool },
    Enter,
    Delete,
    Escape,
}

#[cfg(test)]
impl TextEditEvent {
    fn backspace() -> TextEditEvent {
        Self::Backspace { word: false }
    }
    
    fn backspace_word() -> TextEditEvent {
        Self::Backspace { word: true }
    }
}

pub trait App<C: Display> {
    fn draw(&self, target: &mut Drawer, commands: &mut Commands<C>);
    fn execute_command(&mut self, command: &C, log: &mut Logger) -> Result<(), Error>;
    fn refresh(&mut self, log: &mut Logger) -> bool;
    fn edit_text(&mut self, edit_event: TextEditEvent, log: &mut Logger);
    fn is_editing_text(&self) -> bool;
}

pub fn run<C: Display, A: App<C>>(app: A) -> Result<(), Error> {
    init()?;
    let result = main_loop(app);
    exit()?;
    result
}

fn main_loop<C: Display, A: App<C>>(mut app: A) -> Result<(), Error> {
    let mut redraw = true;
    let (w, h) = terminal::size()?;
    let mut buf = Drawer::new(w, h, 0, 0);
    let mut log = Logger::new();
    let mut commands = Commands::new();
    loop {
        if redraw {
            redraw = false;

            let (w, h) = terminal::size()?;
            buf.reset(w, h);
            commands.reset();

            let mut command_bar = buf.right_margin();
            command_bar.inset_mut(1, 0);

            stdout().queue(cursor::Hide)?;
            app.draw(&mut buf, &mut commands);

            // TextLayout::new().with_items(log.items(), 0).with_wrap_mode(WrapMode::CutOff { marker: "...", cut_start: false }).render(buf.right_margin(), &mut buf);
            
            for cmd_rect in &commands.rects {
                let mut keys: Vec<(&CommandSignature, &C, usize)> = cmd_rect.public.iter().map(|(k, v)| (k, &v.command, v.i)).collect();
                keys.sort_by_key(|(_, _, i)| *i);
                for (index, slot) in cmd_rect.rect.to_slots(20, 1, 2, 0, GridDirection::LeftUp).into_iter().enumerate() {
                    let Some((sign, command, _)) = keys.get(index) else {
                        break;
                    };
                    if sign.hidden {
                        continue;
                    }
                    TextLayout::new().with_text(format!("{sign}: {command}"), 0).render(slot, &mut buf);
                }
            }

            stdout()
                .execute(terminal::BeginSynchronizedUpdate)?
                .execute(cursor::MoveTo(0, 0))?
                .execute(Print(buf.to_string()))?
                .execute(cursor::Hide)?;
            
            if let Some((x, y)) = buf.cursor() {
                stdout()
                    .execute(cursor::MoveTo(x, y))?
                    .execute(cursor::Show)?;
            }
            stdout()
                .execute(terminal::EndSynchronizedUpdate)?;
        }
        if let Ok(can_read) = event::poll(Duration::from_secs(1)) && can_read {
            match event::read()? {
                event::Event::Key(key_event) => {
                    if matches!(key_event, KeyEvent { code: KeyCode::Char('c'), modifiers: KeyModifiers::CONTROL, .. }) {
                        return Ok(());
                    }
                    let mut consumed = false;
                    if !app.is_editing_text() || key_event.modifiers == KeyModifiers::CONTROL || !matches!(key_event.code, KeyCode::Char(_) | KeyCode::Enter) {
                        // Only check commands if not editing text or if event has CTRL modifier or is otherwise not a written character
                        for (CommandSignature { key, hidden: _, ctrl }, command) in commands.all() {
                            if 
                                key_event.is_press() && 
                                key_event.code.to_string().eq_ignore_ascii_case(&key.to_string()) && 
                                (key_event.modifiers == KeyModifiers::CONTROL) == *ctrl // CTRL status equal to that of command 
                            {
                                redraw = true;
                                consumed = true;
                                app.execute_command(command, &mut log)?;
                            }
                        }
                    }
                    if !consumed && app.is_editing_text() && let Some(edit_event) = text_edit_event(key_event) {
                        app.edit_text(edit_event, &mut log);
                        redraw = true;
                    }
                },
                event::Event::Resize(..) => redraw = true,
                _ => ()
            }
        }
        redraw = redraw | app.refresh(&mut log);
    }
}

fn text_edit_event(key_event: event::KeyEvent) -> Option<TextEditEvent> {
    if key_event.is_release() {
        return None;
    }
    let word = key_event.modifiers == KeyModifiers::ALT;
    match key_event.code {
        KeyCode::Backspace => Some(TextEditEvent::Backspace { word }),
        KeyCode::Enter => Some(TextEditEvent::Enter),
        KeyCode::Left => Some(TextEditEvent::Left),
        KeyCode::Right => Some(TextEditEvent::Right),
        KeyCode::Up => Some(TextEditEvent::Up),
        KeyCode::Down => Some(TextEditEvent::Down),
        KeyCode::Delete => Some(TextEditEvent::Delete),
        KeyCode::Esc => Some(TextEditEvent::Escape),
        KeyCode::Char(s) => Some(TextEditEvent::Char(s)),
        _ => None,
    }
}

fn format_key(key: KeyCode) -> String {
    match key {
        KeyCode::Left => "←".to_string(),
        KeyCode::Right => "→".to_string(),
        KeyCode::Up => "↑".to_string(),
        KeyCode::Down => "↓".to_string(),
        KeyCode::Char(' ') => "SPACE".to_string(),
        KeyCode::Enter => "RETUR".to_string(),
        KeyCode::Backspace => "DEL".to_string(),
        other => other.to_string().to_uppercase(),
    }
}

#[derive(Default, Clone)]
pub struct TextEditor {
    lines: Vec<String>,
    line_i: usize,
    grapheme_i: usize,
    single_line_mode: bool,
}

fn type_of_str(s: &str) -> CharType {
    let chars = s.chars();
    if chars.clone().all(char::is_whitespace) {
        CharType::Whitespace
    } else if chars.clone().all(char::is_alphanumeric) {
        CharType::AlphaNumeric
    } else {
        CharType::Other
    }
}

impl TextEditor {
    pub fn single_line() -> Self {
        Self::default().single_line_mode()
    }
    pub fn from_string(s: String) -> Self {
        let lines = s.lines().map(|s| s.to_string()).collect();
        Self { lines, ..Default::default() }
    }

    pub fn set_cursor(&mut self, x: usize, y: usize) {
        self.grapheme_i = x;
        self.line_i = y;
    }

    fn delete(&mut self, n: isize) {
        let mut remaining = if n < 0 {
            n.abs() as usize
        } else {
            self.move_cursor_h(n) as usize
        };

        while remaining > 0 {
            if self.grapheme_i == 0 {
                if self.line_i == 0 {
                    return;
                }
                let (pre, post) = self.lines.split_at_mut(self.line_i);
                let pre_count = grapheme_count(&pre[self.line_i - 1]);
                pre[self.line_i - 1].push_str(&post[0]);
                self.grapheme_i = pre_count;
                self.lines.remove(self.line_i);
                self.line_i -= 1;
                remaining -= 1;
            } else {
                let line = self.lines[self.line_i].clone();
                let mut graphemes = line.graphemes(true).collect::<Vec<&str>>();
                let num_graph = graphemes.len();
                self.grapheme_i = self.grapheme_i.min(num_graph);
                if self.grapheme_i >= num_graph {
                    let take = remaining.min(num_graph);
                    remaining -= take;
                    self.grapheme_i -= take;
                    self.lines[self.line_i] = graphemes[0..num_graph - take].join("");
                } else {
                    let (pre, post) = graphemes.split_at_mut(self.grapheme_i);
                    let take = remaining.min(pre.len());
                    remaining -= take;
                    self.grapheme_i -= take;
                    self.lines[self.line_i] = pre[0..pre.len() - take].join("");
                    self.lines[self.line_i].push_str(&post.join(""));
                }
            }
        }
    }

    fn seek_until_different(&self, forward: bool) -> usize {
        let Some(line) = self.lines.get(self.line_i) else {
            return 0;
        };
        let mut graphemes = line.graphemes(true).collect::<Vec<&str>>();
        let (pre, post) = graphemes.split_at_mut(self.grapheme_i);
        let mut graphemes = if forward {
            post.reverse();
            post
        } else {
            pre
        }.to_vec();
        graphemes.pop();
        graphemes.reverse();
        let mut num = 1;
        let mut delete_ty = None;
        for ch in graphemes {
            let ty = type_of_str(ch);
            if *delete_ty.get_or_insert(ty) != ty {
                break;
            }
            num += 1;
        }
        num
    }

    fn move_cursor_v(&mut self, n: isize) {
        let new_l_i = self.line_i as isize + n;
        if new_l_i < 0 {
            self.grapheme_i = 0;
            self.line_i = 0;
        } else if new_l_i >= self.lines.len() as isize {
            self.grapheme_i = self.line_grapheme_count();
            self.line_i = self.lines.len().saturating_sub(1);
        } else {
            self.line_i = new_l_i as usize;
        }
    }

    /// Returns distance actually moved
    fn move_cursor_h(&mut self, n: isize) -> isize {
        let mut remaining = n;
        let step = n.clamp(-1, 1);
        self.grapheme_i = self.grapheme_i.min(self.line_grapheme_count());
        while remaining.abs() > 0 {
            let mut new_g_i = self.grapheme_i as isize + step;
            if new_g_i < 0 {
                if self.line_i > 0 {
                    self.line_i -= 1;
                } else {
                    self.grapheme_i = 0;
                    break;
                }
            }
            let Some(line) = self.lines.get(self.line_i) else { break; };
            let line_len = grapheme_count(line);
            if new_g_i < 0 {
                new_g_i = line_len as isize;
            }
            if new_g_i > line_len as isize {
                if self.line_i + 1 < self.lines.len() {
                    self.line_i += 1;
                    new_g_i = 0;
                } else {
                    self.grapheme_i = line_len;
                    break;
                }
            }
            self.grapheme_i = new_g_i as usize;
            remaining -= step;
        }
        n - remaining
    }

    /// Returns true if an escape was indicated, either by Enter in single line mode, or by pressing Escape.
    pub fn edit(&mut self, edit_event: TextEditEvent) -> Option<TextEditResponse> {
        // Sanitize indices

        match edit_event {
            TextEditEvent::Char(ch) => {
                let ch = ch.to_string();
                if self.lines.is_empty() {
                    self.lines.push(String::new());
                }
                let line = self.lines.get_mut(self.line_i).unwrap();
                self.grapheme_i = self.grapheme_i.min(grapheme_count(line));
                let mut graphemes = line.graphemes(true).collect::<Vec<&str>>();
                graphemes.insert(self.grapheme_i, &ch);
                self.grapheme_i += 1;
                *line = graphemes.join("");
            },
            TextEditEvent::Enter => {
                if self.single_line_mode {
                    return Some(TextEditResponse::Return);
                }
                let new_line_i = (self.line_i + 1).min(self.lines.len());
                self.lines.insert(new_line_i, String::new());
                if let Some(old_line) = self.lines.get(self.line_i).cloned() {
                    let graphemes = old_line.graphemes(true).collect::<Vec<&str>>();
                    if self.grapheme_i < graphemes.len() {
                        let (pre, post) = graphemes.split_at(self.grapheme_i);
                        self.lines[self.line_i] = pre.join("");
                        self.lines[new_line_i].push_str(&post.join(""));
                    }
                }
                self.grapheme_i = 0;
                self.line_i = new_line_i;
            },
            TextEditEvent::Left => { self.move_cursor_h(-1); },
            TextEditEvent::Right => { self.move_cursor_h(1); },
            TextEditEvent::Up => self.move_cursor_v(-1),
            TextEditEvent::Down => self.move_cursor_v(1),
            TextEditEvent::Backspace { word } => {
                self.delete(word.then(|| self.seek_until_different(false) as isize * -1).unwrap_or(-1))
            }
            TextEditEvent::Delete => self.delete(1),
            TextEditEvent::Escape => return Some(TextEditResponse::Escape),
        }
        None
    }
    
    pub fn render(&self, placeholder: &str, rect: ui::Rect, buf: &mut Drawer) {
        let text = self.to_string();
        TextLayout::new().with_wrap_mode(WrapMode::CutOff { marker: "...", cut_start: false }).with_text(if text == "" { placeholder.to_string() } else { text }, 0).render(rect, buf);
        buf.show_cursor(rect.x + (self.grapheme_i as u16).min(rect.w).min(self.line_grapheme_count() as u16), rect.y + (self.line_i as u16).min(rect.h));
    }
    
    pub fn single_line_mode(self) -> TextEditor {
        Self { single_line_mode: true, ..self }
    }
    
    fn line_grapheme_count(&self) -> usize {
        self.lines.get(self.line_i).map_or(0, |s| grapheme_count(s))
    }
    
    pub fn with_cursor_last(mut self) -> TextEditor {
        self.set_cursor_last();
        self
    }
    
    pub fn set_cursor_last(&mut self) {
        let line_i = self.lines.len().saturating_sub(1);
        let grapheme_i = if let Some(line) = self.lines.get(line_i) {
            grapheme_count(line)
        } else {
            0
        };
        self.grapheme_i = grapheme_i;
        self.line_i = line_i;
    }

    pub fn set_text(&mut self, text: &str) {
        self.lines.clear();
        for line in text.lines() {
            self.lines.push(line.to_string());
        }
    }
}

#[derive(PartialEq, Clone, Copy)]
enum CharType {
    Whitespace,
    AlphaNumeric,
    Other,
}

impl Display for TextEditor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, line) in self.lines.iter().enumerate() {
            f.write_str(line)?;
            if i + 1 < self.lines.len() {
                f.write_str("\n")?;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod test_text_edit {
    use crate::{TextEditor, TextEditEvent};

    #[test]
    fn add_character() {
        let mut a = TextEditor::from_string([
            "Hej,",
            "Jag tänkte säga det."
        ].join("\n"));
        a.edit(TextEditEvent::Char('a'));
        assert_eq!(a.to_string(), [
            "aHej,",
            "Jag tänkte säga det."
        ].join("\n"));
        a.edit(TextEditEvent::Char(' '));
        assert_eq!(a.to_string(), [
            "a Hej,",
            "Jag tänkte säga det."
        ].join("\n"));
        a.set_cursor(10, 0);
        a.edit(TextEditEvent::Char('!'));
        assert_eq!(a.to_string(), [
            "a Hej,!",
            "Jag tänkte säga det."
        ].join("\n"));
        a.edit(TextEditEvent::Down);
        a.edit(TextEditEvent::Char('o'));
        assert_eq!(a.to_string(), [
            "a Hej,!",
            "Jag tänokte säga det."
        ].join("\n"));
        a.edit(TextEditEvent::Down);
        a.edit(TextEditEvent::Char('Y'));
        assert_eq!(a.to_string(), [
            "a Hej,!",
            "Jag tänokte säga det.Y"
        ].join("\n"));
        a.edit(TextEditEvent::Up);
        a.edit(TextEditEvent::Char('ô'));
        assert_eq!(a.to_string(), [
            "a Hej,!ô",
            "Jag tänokte säga det.Y"
        ].join("\n"));
        a.edit(TextEditEvent::Up);
        a.edit(TextEditEvent::Char('📜'));
        assert_eq!(a.to_string(), [
            "📜a Hej,!ô",
            "Jag tänokte säga det.Y"
        ].join("\n"));
    }

    #[test]
    fn backspace() {
        let mut a = TextEditor::from_string([
            "Mer text!",
            "",
            "Nu finns det blankrader."
        ].join("\n"));
        a.edit(TextEditEvent::backspace());
        assert_eq!(a.to_string(), [
            "Mer text!",
            "",
            "Nu finns det blankrader."
        ].join("\n"));
        a.edit(TextEditEvent::Left);
        a.edit(TextEditEvent::backspace());
        assert_eq!(a.to_string(), [
            "Mer text!",
            "",
            "Nu finns det blankrader."
        ].join("\n"));
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::backspace());
        assert_eq!(a.to_string(), [
            "er text!",
            "",
            "Nu finns det blankrader."
        ].join("\n"));
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::backspace());
        assert_eq!(a.to_string(), [
            "e text!",
            "",
            "Nu finns det blankrader."
        ].join("\n"));
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::backspace());
        assert_eq!(a.to_string(), [
            "e text!",
            "Nu finns det blankrader."
        ].join("\n"));
        a.edit(TextEditEvent::Enter);
        a.edit(TextEditEvent::backspace());
        assert_eq!(a.to_string(), [
            "e text!",
            "Nu finns det blankrader."
        ].join("\n"));
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Up);
        a.edit(TextEditEvent::backspace());
        assert_eq!(a.to_string(), [
            "e text",
            "Nu finns det blankrader."
        ].join("\n"));
        a.edit(TextEditEvent::Down);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Up);
        a.edit(TextEditEvent::Left);
        a.edit(TextEditEvent::backspace());
        assert_eq!(a.to_string(), [
            "e tet",
            "Nu finns det blankrader."
        ].join("\n"));
        a.edit(TextEditEvent::Down);
        a.edit(TextEditEvent::Left);
        a.edit(TextEditEvent::Left);
        a.edit(TextEditEvent::Left);
        a.edit(TextEditEvent::Left);
        a.edit(TextEditEvent::backspace());
        a.edit(TextEditEvent::backspace());
        assert_eq!(a.to_string(), [
            "e teNu finns det blankrader."
        ].join("\n"));
    }

    #[test]
    fn enter() {
        let mut a = TextEditor::default();
        a.edit(TextEditEvent::Enter);
        assert_eq!(a.to_string(), "");
        a.edit(TextEditEvent::Enter);
        assert_eq!(a.to_string(), "\n");

        let mut a = TextEditor::from_string([
            "Nu är det dags att testa returknappen!",
            "I teststrängen finns blankrader.",
            "",
            "Åtminstone en."
        ].join("\n"));
        a.edit(TextEditEvent::Enter);
        assert_eq!(a.to_string(), [
            "",
            "Nu är det dags att testa returknappen!",
            "I teststrängen finns blankrader.",
            "",
            "Åtminstone en."
        ].join("\n"));

        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Down);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Enter);
        assert_eq!(a.to_string(), [
            "",
            "Nu är det dags att testa returknappen!",
            "I ",
            "teststrängen finns blankrader.",
            "",
            "Åtminstone en."
        ].join("\n"));
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Enter);
        assert_eq!(a.to_string(), [
            "",
            "Nu är det dags att testa returknappen!",
            "I ",
            "t",
            "eststrängen finns blankrader.",
            "",
            "Åtminstone en."
        ].join("\n"));
        a.edit(TextEditEvent::Down);
        a.edit(TextEditEvent::Down);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Enter);
        assert_eq!(a.to_string(), [
            "",
            "Nu är det dags att testa returknappen!",
            "I ",
            "t",
            "eststrängen finns blankrader.",
            "",
            "Åtminstone en.",
            ""
        ].join("\n"));
        a.edit(TextEditEvent::Up);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Up);
        a.edit(TextEditEvent::Enter);
        assert_eq!(a.to_string(), [
            "",
            "Nu är det dags att testa returknappen!",
            "I ",
            "t",
            "eststrängen finns blankrader.",
            "",
            "",
            "Åtminstone en.",
            ""
        ].join("\n"));
    }

    #[test]
    fn delete() {
        let mut a = TextEditor::from_string([
            "Gillar inte detta testfall.",
            "",
            "Kommer nog inte själv att behöva detta."
        ].join("\n"));
        a.edit(TextEditEvent::Delete);
        assert_eq!(a.to_string(), [
            "illar inte detta testfall.",
            "",
            "Kommer nog inte själv att behöva detta."
        ].join("\n"));
        a.edit(TextEditEvent::Down);
        a.edit(TextEditEvent::Delete);
        assert_eq!(a.to_string(), [
            "illar inte detta testfall.",
            "Kommer nog inte själv att behöva detta."
        ].join("\n"));
        a.edit(TextEditEvent::Delete);
        a.edit(TextEditEvent::Delete);
        assert_eq!(a.to_string(), [
            "illar inte detta testfall.",
            "mmer nog inte själv att behöva detta."
        ].join("\n"));
        a.edit(TextEditEvent::Enter);
        a.edit(TextEditEvent::Up);
        a.edit(TextEditEvent::Char('L'));
        a.edit(TextEditEvent::Char('p'));
        a.edit(TextEditEvent::Delete);
        a.edit(TextEditEvent::Delete);
        assert_eq!(a.to_string(), [
            "illar inte detta testfall.",
            "Lpmer nog inte själv att behöva detta."
        ].join("\n"));
        a.edit(TextEditEvent::Down);
        a.edit(TextEditEvent::Delete);
        assert_eq!(a.to_string(), [
            "illar inte detta testfall.",
            "Lpmer nog inte själv att behöva detta."
        ].join("\n"));
    }

    #[test]
    fn delete_word() {
        let mut a = TextEditor::from_string([
            "Det här däremot är användbart.   ",
            "",
            "Kommer nog själv att behöva detta."
        ].join("\n"));
        a.edit(TextEditEvent::backspace_word());
        assert_eq!(a.to_string(), [
            "Det här däremot är användbart.   ",
            "",
            "Kommer nog själv att behöva detta."
        ].join("\n"));
        for _ in 0..3 {
            a.edit(TextEditEvent::Right);
        }
        a.edit(TextEditEvent::backspace_word());
        assert_eq!(a.to_string(), [
            " här däremot är användbart.   ",
            "",
            "Kommer nog själv att behöva detta."
        ].join("\n"));
        for _ in 0..30 {
            a.edit(TextEditEvent::Right);
        }
        a.edit(TextEditEvent::backspace_word());
        assert_eq!(a.to_string(), [
            " här däremot är användbart.",
            "",
            "Kommer nog själv att behöva detta."
        ].join("\n"));
        a.edit(TextEditEvent::backspace_word());
        assert_eq!(a.to_string(), [
            " här däremot är ",
            "",
            "Kommer nog själv att behöva detta."
        ].join("\n"));
    }
}