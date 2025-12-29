use std::{fmt::Display, io::stdout, path::{Path, PathBuf}, sync::mpsc::Receiver, time::Duration};
use crossterm::{ExecutableCommand, cursor, event::{self, KeyCode, KeyModifiers}, style::Print, terminal};
use notify::Watcher;

use crate::ui::{TextBuffer, TextLayout, VecBuffer};

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
}

impl<C: Display> CommandSignature<C> {
    pub fn new(key: KeyCode, command: C) -> Self {
        Self { key, hidden: false, command, ctrl: false }
    }

    pub fn hidden(self) -> Self {
        CommandSignature { hidden: true, ..self }
    }

    pub fn ctrl(self) -> Self {
        Self { ctrl: true, ..self }
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

    pub fn empty_slot(self) -> Self {
        self.with_public(None)
    }
    
    pub fn hidden_ctrl(self, key: KeyCode, command: C) -> Self {
        self.with_hidden(Some(CommandSignature::new(key, command).ctrl()))
    }
}

pub trait App<C: Display> {
    fn draw<T: TextBuffer>(&self, target: &mut T) -> std::io::Result<()>;
    fn commands(&self) -> CommandList<C>;
    fn execute_command(&mut self, command: C, quit: &mut bool);
    fn refresh(&mut self) -> bool;
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
            app.draw(&mut buf)?;
            for (index, slot) in command_bar.to_grid(5, 2, true).into_iter().enumerate() {
                let Some(opt) = commands.public.get(index) else {
                    break;
                };
                let Some(command) = opt else {
                    continue;
                };
                if command.hidden {
                    continue;
                }
                let key = command.key;

                TextLayout::new().with_text(format!("{}{}: {}", command.ctrl.then(||"^").unwrap_or_default(), format_key(key), command.command), 0).render(slot, &mut buf)?;
            }
            buf.flush()?;

            stdout().execute(terminal::BeginSynchronizedUpdate)?;
            stdout().execute(cursor::MoveTo(0, 0))?.execute(Print(buf.to_string()))?;
            stdout().execute(terminal::EndSynchronizedUpdate)?;
        }
        if let Ok(can_read) = event::poll(Duration::from_secs(1)) && can_read {
            match event::read()? {
                event::Event::Key(key_event) => for opt in commands.public.into_iter().chain(commands.hidden.into_iter()) {
                    let Some(CommandSignature { key, ctrl, command, .. }) = opt else {
                        continue;
                    };
                    if 
                        key_event.is_press() && 
                        key_event.code.to_string().eq_ignore_ascii_case(&key.to_string()) && 
                        (key_event.modifiers == KeyModifiers::CONTROL) == ctrl // CTRL status equal to that of command 
                    {
                        redraw = true;
                        let mut quit = false;
                        app.execute_command(command, &mut quit);
                        if quit {
                            return Ok(());
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