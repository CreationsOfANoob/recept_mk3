use std::{fmt::Display, io::stdout, panic::catch_unwind};
use crossterm::{ExecutableCommand, cursor, event::{self, KeyCode, KeyModifiers}, style::Print, terminal};

use crate::ui::{TextBuffer, TextLayout, VecBuffer};

pub mod ui;
pub mod book;

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
}

pub fn run<C: Display, A: App<C> + std::panic::UnwindSafe>(app: A) -> std::io::Result<()> {
    init()?;
    let result = catch_unwind(|| main_loop(app));
    exit()?;

    match result {
        Ok(Err(err)) => println!("{err}"),
        Err(_) => panic!("Hoppsan"),
        _ => (),
    }
    Ok(())
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