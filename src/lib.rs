use std::{fmt::Display, io::stdout, path::{Path, PathBuf}, sync::mpsc::Receiver, time::Duration};
use crossterm::{ExecutableCommand, QueueableCommand, cursor, event::{self, KeyCode, KeyEvent, KeyModifiers}, style::Print, terminal};
use notify::Watcher;
use unicode_segmentation::UnicodeSegmentation;

use crate::ui::{TextBuffer, TextLayout, VecBuffer, WrapMode, grapheme_count};

pub mod ui;
pub mod book;

pub struct FolderWatcher {
    receiver: Receiver<Result<notify::Event, notify::Error>>,
    path: PathBuf,
    _watcher: notify::RecommendedWatcher,
}

impl FolderWatcher {
    pub fn new(path: PathBuf) -> Result<Self, Error> {
        let (sender, receiver) = std::sync::mpsc::channel();
        let mut _watcher = notify::recommended_watcher(sender)?;
        _watcher
            .watch(&path, notify::RecursiveMode::Recursive)?;

        Ok(Self { receiver, path, _watcher })
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
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Self::Io(err)
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

pub struct CommandSignature<C: Display> {
    key: KeyCode,
    hidden: bool,
    ctrl: bool,
    command: C,
    name_override: Option<String>,
}

impl<C: Display> Display for CommandSignature<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.ctrl {
            f.write_str("^")?;
        }
        f.write_str(&format_key(self.key))?;
        f.write_str(": ")?;
        if let Some(alt) = &self.name_override {
            f.write_str(alt)?;
        } else {
            write!(f, "{}", self.command)?;
        }
        Ok(())
    }
}

impl<C: Display> CommandSignature<C> {
    pub fn new(key: KeyCode, command: C) -> Self {
        Self { key, hidden: false, command, ctrl: false, name_override: None }
    }

    pub fn hidden(self) -> Self {
        CommandSignature { hidden: true, ..self }
    }

    pub fn ctrl(self) -> Self {
        Self { ctrl: true, ..self }
    }
    
    fn with_override(self, name: String) -> Self {
        Self { name_override: Some(name), ..self }
    }
}

pub struct CommandList<C: Display> {
    public: Vec<Option<CommandSignature<C>>>,
    hidden: Vec<Option<CommandSignature<C>>>,
}

impl<C: Display> CommandList<C> {
    pub fn new() -> Self {
        Self { public: Vec::new(), hidden: Vec::new() }
    }

    fn with_public(self, signature: Option<CommandSignature<C>>) -> Self {
        let mut public = self.public;
        public.push(signature);
        Self { public, ..self }
    }

    fn with_hidden(self, signature: Option<CommandSignature<C>>) -> Self {
        let mut hidden = self.hidden;
        hidden.push(signature);
        Self { hidden, ..self }
    }

    pub fn command(self, key: KeyCode, command: C) -> Self {
        self.with_public(Some(CommandSignature::new(key, command)))
    }

    pub fn ctrl_if(self, cond: bool, key: KeyCode, command: C) -> Self {
        self.with_public(cond.then(|| CommandSignature::new(key, command).ctrl()))
    }

    pub fn command_if(self, cond: bool, key: KeyCode, command: C) -> Self {
        self.with_public(cond.then(|| CommandSignature::new(key, command)))
    }

    pub fn hidden(self, key: KeyCode, command: C) -> Self {
        self.with_hidden(Some(CommandSignature::new(key, command)))
    }

    pub fn ctrl(self, key: KeyCode, command: C) -> Self {
        self.with_public(Some(CommandSignature::new(key, command).ctrl()))
    }

    pub fn ctrl_with_name_override(self, key: KeyCode, command: C, name: String) -> Self {
        self.with_public(Some(CommandSignature::new(key, command).ctrl().with_override(name)))
    }

    pub fn empty_slot(self) -> Self {
        self.with_public(None)
    }
    
    pub fn hidden_ctrl(self, key: KeyCode, command: C) -> Self {
        self.with_hidden(Some(CommandSignature::new(key, command).ctrl()))
    }
}

pub enum TextEditEvent {
    Char(char),
    Left,
    Right,
    Up,
    Down,
    Backspace,
    Enter,
    Delete,
    DeleteWord,
}

pub trait App<C: Display> {
    fn draw<T: TextBuffer>(&self, target: &mut T) -> std::io::Result<()>;
    fn commands(&self) -> CommandList<C>;
    fn execute_command(&mut self, command: C);
    fn refresh(&mut self) -> bool;
    fn edit_text(&mut self, edit_event: TextEditEvent);
    fn is_editing_text(&self) -> bool;
}

pub fn run<C: Display, A: App<C>>(app: A) -> std::io::Result<()> {
    init()?;
    let result = main_loop(app);
    exit()?;
    result
}

fn main_loop<C: Display, A: App<C>>(mut app: A) -> std::io::Result<()> {
    let mut redraw = true;
    loop {
        let commands = app.commands();

        if redraw {
            redraw = false;

            let (w, h) = terminal::size()?;
            let mut buf = VecBuffer::new(w, h, 2);

            let mut command_bar = buf.margin_rect();
            command_bar.inset_mut(1, 0);

            buf.clear()?;
            stdout().queue(cursor::Hide)?;
            app.draw(&mut buf)?;
            for (index, slot) in command_bar.to_grid(5, 2, 0, true).into_iter().enumerate() {
                let Some(opt) = commands.public.get(index) else {
                    break;
                };
                let Some(command) = opt else {
                    continue;
                };
                if command.hidden {
                    continue;
                }
                TextLayout::new().with_text(command.to_string(), 0).render(slot, &mut buf)?;
            }
            buf.flush()?;

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
                    if event_is_quit(key_event) {
                        return Ok(());
                    }
                    if app.is_editing_text() && let Some(edit_event) = text_edit_event(key_event) {
                        app.edit_text(edit_event);
                        redraw = true;
                    } else {
                        for opt in commands.public.into_iter().chain(commands.hidden.into_iter()) {
                            let Some(CommandSignature { key, ctrl, command, .. }) = opt else {
                                continue;
                            };
                            if 
                                key_event.is_press() && 
                                key_event.code.to_string().eq_ignore_ascii_case(&key.to_string()) && 
                                (key_event.modifiers == KeyModifiers::CONTROL) == ctrl // CTRL status equal to that of command 
                            {
                                redraw = true;
                                app.execute_command(command);
                            }
                        }
                    }
                },
                event::Event::Resize(..) => redraw = true,
                _ => ()
            }
        }
        redraw = redraw | app.refresh();
    }
}

fn event_is_quit(key_event: event::KeyEvent) -> bool {
    matches!(key_event, KeyEvent { code: KeyCode::Char('c'), modifiers: KeyModifiers::CONTROL, ..})
}

fn text_edit_event(key_event: event::KeyEvent) -> Option<TextEditEvent> {
    if key_event.is_release() {
        return None;
    }
    match key_event.code {
        KeyCode::Backspace => Some(TextEditEvent::Backspace),
        KeyCode::Enter => Some(TextEditEvent::Enter),
        KeyCode::Left => Some(TextEditEvent::Left),
        KeyCode::Right => Some(TextEditEvent::Right),
        KeyCode::Up => Some(TextEditEvent::Up),
        KeyCode::Down => Some(TextEditEvent::Down),
        KeyCode::Delete => Some(TextEditEvent::Delete),
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

impl TextEditor {
    pub fn from_string(s: String) -> Self {
        let lines = s.lines().map(|s| s.to_string()).collect();
        Self { lines, ..Default::default() }
    }

    pub fn set_cursor(&mut self, x: usize, y: usize) {
        self.grapheme_i = x;
        self.line_i = y;
    }

    /// Returns true if enter was pressed and single line mode is active
    pub fn edit(&mut self, edit_event: TextEditEvent) -> bool {
        // Sanitize indices
        self.line_i = self.line_i.min(self.lines.len().saturating_sub(1));
        if let Some(line) = self.lines.get(self.line_i) {
            self.grapheme_i = self.grapheme_i.min(grapheme_count(line));
        } else {
            self.grapheme_i = 0;
        }
        match edit_event {
            TextEditEvent::Char(ch) => {
                let ch = ch.to_string();
                if self.lines.is_empty() {
                    self.lines.push(String::new());
                }
                let line = self.lines.get_mut(self.line_i).unwrap();
                let mut graphemes = line.graphemes(true).collect::<Vec<&str>>();
                graphemes.insert(self.grapheme_i, &ch);
                self.grapheme_i += 1;
                *line = graphemes.join("");
            },
            TextEditEvent::Left => if self.grapheme_i == 0 {
                if self.line_i != 0 {
                    self.line_i -= 1;
                    if let Some(line) = self.lines.get(self.line_i) {
                        self.grapheme_i = grapheme_count(line);
                    }
                }
            } else {
                self.grapheme_i -= 1;
            },
            TextEditEvent::Right => {
                self.grapheme_i += 1;
                if let Some(line) = self.lines.get(self.line_i) {
                    let line_len = grapheme_count(line);
                    if self.grapheme_i > line_len {
                        if self.lines.len() > self.line_i + 1 {
                            self.line_i += 1;
                            self.grapheme_i = 0;
                        } else {
                            self.grapheme_i = line_len;
                        }
                    }
                }
            },
            TextEditEvent::Up => {
                if self.line_i + 1 > self.lines.len() {
                    self.line_i = self.lines.len().saturating_sub(1);
                } else if self.line_i == 0 {
                    self.grapheme_i = 0;
                } else {
                    self.line_i -= 1;
                }
            },
            TextEditEvent::Down => {
                if self.line_i + 1 < self.lines.len() {
                    self.line_i += 1;
                } else if let Some(line) = self.lines.get(self.lines.len().saturating_sub(1)) {
                    self.line_i = self.lines.len().saturating_sub(1);
                    self.grapheme_i = grapheme_count(line);
                } else {
                    self.grapheme_i = 0;
                    self.line_i = 0;
                }
            },
            TextEditEvent::Backspace => if let Some(curr_line) = self.lines.get(self.line_i).cloned() {
                if self.grapheme_i == 0 {
                    if let Some(prev_line_i) = self.line_i.checked_sub(1) && let Some(prev_line) = self.lines.get_mut(self.line_i - 1) {
                        self.grapheme_i = grapheme_count(prev_line);
                        prev_line.push_str(&curr_line);
                        self.lines.remove(self.line_i);
                        self.line_i = prev_line_i;
                    }
                } else {
                    let mut graphemes = curr_line.graphemes(true).collect::<Vec<&str>>();
                    self.grapheme_i -= 1;
                    graphemes.remove(self.grapheme_i);
                    self.lines[self.line_i] = graphemes.join("");
                }
            },
            TextEditEvent::Enter => {
                if self.single_line_mode {
                    return true;
                }
                let new_line_i = (self.line_i + 1).min(self.lines.len());
                self.lines.insert(new_line_i, String::new());
                if let Some(old_line) = self.lines.get(self.line_i).cloned() {
                    let graphemes = old_line.graphemes(true).collect::<Vec<&str>>();
                    let (pre, post) = graphemes.split_at(self.grapheme_i);
                    self.lines[self.line_i] = pre.join("");
                    for string in post {
                        self.lines[new_line_i].push_str(string);
                    }
                }
                self.grapheme_i = 0;
                self.line_i = new_line_i;
            },
            TextEditEvent::Delete => if let Some(line) = self.lines.get(self.line_i) {
                let mut graphemes = line.graphemes(true).collect::<Vec<&str>>();
                if self.grapheme_i == graphemes.len() {
                    if self.line_i + 1 < self.lines.len() {
                        let (pre, post) = self.lines.split_at_mut(self.line_i + 1);
                        pre[self.line_i].push_str(&post[0]);
                        self.lines.remove(self.line_i + 1);
                    }
                } else {
                    graphemes.remove(self.grapheme_i);
                    self.lines[self.line_i] = graphemes.join("");
                }
            },
            TextEditEvent::DeleteWord => if let Some(line) = self.lines.get(self.line_i).cloned() {
                if self.grapheme_i > 0 {
                    let mut graphemes = line.graphemes(true).collect::<Vec<&str>>();
                    self.grapheme_i -= 1;
                    graphemes.remove(self.grapheme_i);
                    let mut delete_ty = None;
                    while self.grapheme_i > 0 {
                        
                        let chars = graphemes[self.grapheme_i - 1].chars();
                        let ty = if chars.clone().all(char::is_whitespace) {
                            CharType::Whitespace
                        } else if chars.clone().all(char::is_alphanumeric) {
                            CharType::AlphaNumeric
                        } else {
                            CharType::Other
                        };
                        if *delete_ty.get_or_insert(ty) != ty {
                            break;
                        }
                        self.grapheme_i -= 1;
                        graphemes.remove(self.grapheme_i);
                    }
                    self.lines[self.line_i] = graphemes.join("");
                } else if self.line_i > 0 {
                    self.lines[self.line_i - 1].push_str(&line);
                    self.lines.remove(self.line_i);
                    self.line_i -= 1;
                }
            },
        }
        false
    }
    
    pub fn render<T: TextBuffer>(&self, placeholder: &str, rect: ui::Rect, buf: &mut T) -> std::io::Result<()> {
        let text = self.to_string();
        TextLayout::new().with_wrap_mode(WrapMode::CutOff { marker: "...", cut_start: false }).with_text(if text == "" { placeholder.to_string() } else { text }, 0).render(rect, buf)?;
        buf.show_cursor(rect.x + (self.grapheme_i as u16).min(rect.w), rect.y + (self.line_i as u16).min(rect.h));
        Ok(())
    }
    
    pub fn single_line_mode(self) -> TextEditor {
        Self { single_line_mode: true, ..self }
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
        a.edit(TextEditEvent::Backspace);
        assert_eq!(a.to_string(), [
            "Mer text!",
            "",
            "Nu finns det blankrader."
        ].join("\n"));
        a.edit(TextEditEvent::Left);
        a.edit(TextEditEvent::Backspace);
        assert_eq!(a.to_string(), [
            "Mer text!",
            "",
            "Nu finns det blankrader."
        ].join("\n"));
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Backspace);
        assert_eq!(a.to_string(), [
            "er text!",
            "",
            "Nu finns det blankrader."
        ].join("\n"));
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Right);
        a.edit(TextEditEvent::Backspace);
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
        a.edit(TextEditEvent::Backspace);
        assert_eq!(a.to_string(), [
            "e text!",
            "Nu finns det blankrader."
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
        a.edit(TextEditEvent::Down);
        a.edit(TextEditEvent::Delete);
        assert_eq!(a.to_string(), [
            "illar inte detta testfall.",
            "Kommer nog inte själv att behöva detta."
        ].join("\n"));
    }

    #[test]
    fn delete_word() {
        let mut a = TextEditor::from_string([
            "Det här däremot är användbart.   ",
            "",
            "Kommer nog själv att behöva detta."
        ].join("\n"));
        a.edit(TextEditEvent::DeleteWord);
        assert_eq!(a.to_string(), [
            "Det här däremot är användbart.   ",
            "",
            "Kommer nog själv att behöva detta."
        ].join("\n"));
        for _ in 0..3 {
            a.edit(TextEditEvent::Right);
        }
        a.edit(TextEditEvent::DeleteWord);
        assert_eq!(a.to_string(), [
            " här däremot är användbart.   ",
            "",
            "Kommer nog själv att behöva detta."
        ].join("\n"));
        for _ in 0..30 {
            a.edit(TextEditEvent::Right);
        }
        a.edit(TextEditEvent::DeleteWord);
        assert_eq!(a.to_string(), [
            " här däremot är användbart.",
            "",
            "Kommer nog själv att behöva detta."
        ].join("\n"));
        a.edit(TextEditEvent::DeleteWord);
        assert_eq!(a.to_string(), [
            " här däremot är ",
            "",
            "Kommer nog själv att behöva detta."
        ].join("\n"));
    }
}