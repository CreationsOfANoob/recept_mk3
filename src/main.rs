use std::{collections::HashMap, fmt::Display, option::Option, path::PathBuf};

use crossterm::event::KeyCode;
use recept::{App, CommandList, Config, ConfigLoader, Error, FolderWatcher, book::Recept, ui::{Border, BorderStyle, ListLayout, Rect, Side, TextBuffer, TextLayout}};
use serde::{Deserialize, Serialize};

fn main() {
    let app = match ReceptApp::new() {
        Ok(app) => app,
        Err(err) => {
            println!("{err:?}");
            return;
        },
    };
    match recept::run(app) {
        Ok(_) => (),
        Err(err) => println!("{err:?}"),
    }
}

const DEFAULT_RECEPT: &str = "Exempelrubrik

Ingredienser:
1 dl lorem
100 g ipsum

Gör så här:
Dolor sit amet.
Consectetur adipiscing elit. 
Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.";

enum Command {
    ListaNed,
    ListaUpp,
    Visa,
    VisaINyFlik,
    FlikNästa,
    FlikFörra,
    Quit,
    StängFlik,
    Lås,
    LåsUpp,
    NyttRecept,
    SkapaRecept,
    ÖppnaSök,
    StängSök,
}

impl Display for Command {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Command::ListaNed => write!(f, "Nästa"),
            Command::ListaUpp => write!(f, "Föreg."),
            Command::Visa => write!(f, "Visa recept"),
            Command::VisaINyFlik => write!(f, "Ny flik"),
            Command::Quit => write!(f, "Avsluta"),
            Command::FlikNästa => write!(f, "Nästa flik"),
            Command::FlikFörra => write!(f, "Föreg. flik"),
            Command::StängFlik => write!(f, "Stäng flik"),
            Command::Lås => write!(f, "Lås"),
            Command::LåsUpp => write!(f, "Visa"),
            Command::NyttRecept => write!(f, "Nytt recept"),
            Command::SkapaRecept => write!(f, "Skapa"),
            Command::ÖppnaSök => write!(f, "Sök recept"),
            Command::StängSök => write!(f, "Avsluta sökning"),
        }
    }
}

enum UserMode {
    Locked,
    Normal,
    SkapaRecept(String),
    Sök(String),
}

#[derive(Serialize, Deserialize, PartialEq, Clone)]
struct ReceptConfig {
    receptmapp: PathBuf,
}

impl Config for ReceptConfig {
    fn new() -> Option<ReceptConfig> {
        let receptmapp = rfd::FileDialog::new().set_title("Välj receptmapp").pick_folder()?;
        Some(Self { receptmapp })
    }
}

struct ReceptApp {
    recept: HashMap<PathBuf, Recept>,
    listval: usize,
    flikval: usize,
    filtrerad_lista: Vec<PathBuf>,
    flikar: Vec<PathBuf>,
    mode: UserMode,
    config: ConfigLoader<ReceptConfig>,
    receptmapp: FolderWatcher,
}

impl ReceptApp {
    pub fn new() -> Result<Self, Error> {
        let mut config: ConfigLoader<ReceptConfig> = ConfigLoader::new("Receptkalkylator")?;
        let receptmapp = FolderWatcher::new(config.get()?.receptmapp.clone())?;
        let mut recept = HashMap::new();
        for recept_path in receptmapp.files()? {
            if let Ok(Some(tolkat)) = Recept::try_parse_file(&recept_path) {
                recept.insert(recept_path, tolkat);
            }
        }
        let filtrerad_lista = Self::filtrera_recept(&recept, None);
        Ok(Self { recept, listval: 0, flikval: 0, flikar: Vec::new(), mode: UserMode::Normal, config, receptmapp, filtrerad_lista })
    }

    fn filtrera_recept(receptbank: &HashMap<PathBuf, Recept>, filter: Option<&str>) -> Vec<PathBuf> {
        let mut träffar = Vec::new();
        for (path, recept) in receptbank {
            if recept.matches(filter) {
                träffar.push(path.to_path_buf());
            }
        }
        träffar
    }
    
    fn draw_recipes<T: TextBuffer>(&self, rect: Rect, buf: &mut T) -> std::io::Result<()> {
        for (flik_i, (mut flik_rect, recept_path)) in rect.to_grid(self.flikar.len() as u16, 1, false).into_iter().zip(self.flikar.iter()).enumerate() {
            if self.flikval == flik_i && self.flikar.len() > 1 && matches!(self.mode, UserMode::Normal){
                Border::new(flik_rect, BorderStyle::THIN).render(buf)?;
            }
            flik_rect.limit_w(100, true);
            flik_rect.inset_mut(2, 2);
            if let Some(recept) = self.recept.get(recept_path) {
                recept.render(flik_rect, buf)?;
            }
        }
        Ok(())
    }

    fn draw_sidebar<T: TextBuffer>(&self, mut rect: Rect, buf: &mut T) -> std::io::Result<()> {
        Border::new(rect.cut_mut(Side::Right, 1), BorderStyle::LIGHT_DOTTED_LINE_V).render(buf)?;
        rect.inset_mut(0, 1);
        if let UserMode::Sök(söksträng) = &self.mode && let Some((mut search, list)) = rect.split(Side::Top, 3) {
            search.cut_mut(Side::Right, 1);
            Border::new(search, BorderStyle::THIN).render(buf)?;
            search.inset_mut(2, 1);
            TextLayout::new().with_single_line(if söksträng == "" { "Sök recept..." } else { söksträng }).render(search, buf)?;
            rect = list;
            rect.cut_mut(Side::Top, 2);
        }
        rect.cut_mut(Side::Right, 2);
        ListLayout::new().with_items(self.filtrerad_lista.iter().map(|recept| self.recept.get(recept).unwrap().rubrik().to_string())).with_selected(self.listval).render(rect, buf)
    }
    
    fn draw_editor<T: TextBuffer>(&self, text: &str, mut rect: Rect, buf: &mut T) -> std::io::Result<()> {
        rect.inset_mut(2, 2);
        Border::new(rect, BorderStyle::THIN).render(buf)?;
        rect.inset_mut(3, 2);
        TextLayout::new().with_text(text.to_string(), 0).render(rect, buf)
    }
}

impl App<Command> for ReceptApp {
    fn draw<T: TextBuffer>(&self, buf: &mut T) -> std::io::Result<()> {
        let mut window_rect = buf.safe_rect();
        window_rect.inset_mut(1, 1);

        match &self.mode {
            UserMode::Locked => self.draw_recipes(window_rect, buf)?,
            UserMode::Normal | UserMode::Sök(_) => if let Some((sidebar, recipe_rect)) = window_rect.split(Side::Left, 30) {
                self.draw_recipes(recipe_rect, buf)?;
                self.draw_sidebar(sidebar, buf)?;
            } else {
                TextLayout::new().with_text("Fönstret är för litet!".to_string(), 0).render(window_rect, buf)?;
            },
            UserMode::SkapaRecept(text) => self.draw_editor(text, window_rect, buf)?,
        }
        Ok(())
    }
    
    fn commands(&self) -> recept::CommandList<Command>{
        match self.mode {
            UserMode::Locked => CommandList::new()
                .empty_slot()
                .command(KeyCode::Char('q'), Command::LåsUpp),
            UserMode::Normal => CommandList::new()
                .command(KeyCode::Char('n'), Command::NyttRecept)
                .hidden(KeyCode::Char('+'), Command::NyttRecept)
                .command(KeyCode::Char('f'), Command::ÖppnaSök)

                .command(KeyCode::Char(' '), Command::Visa)
                .hidden(KeyCode::Enter, Command::Visa)
                .command_if(!self.flikar.is_empty(), KeyCode::Char('q'), Command::Lås)

                .hidden(KeyCode::Char('s'), Command::ListaNed)
                .hidden(KeyCode::Char('w'), Command::ListaUpp)
                .hidden(KeyCode::Up, Command::ListaUpp)
                .hidden(KeyCode::Down, Command::ListaNed)

                .ctrl_if(self.flikval < self.flikar.len(), KeyCode::Char('w'), Command::StängFlik)
                .command(KeyCode::Char('t'), Command::VisaINyFlik)

                .hidden(KeyCode::Char('d'), Command::FlikNästa)
                .hidden(KeyCode::Char('a'), Command::FlikFörra)
                .hidden(KeyCode::Right, Command::FlikNästa)
                .hidden(KeyCode::Left, Command::FlikFörra),
            UserMode::SkapaRecept(_) => CommandList::new()
                .command(KeyCode::Enter, Command::SkapaRecept),
            UserMode::Sök(_) => CommandList::new()
                .command(KeyCode::Enter, Command::StängSök),
        }.hidden_ctrl(KeyCode::Char('c'), Command::Quit)
    }
    
    fn execute_command(&mut self, command: Command, quit: &mut bool) {
        match command {
            Command::Quit => *quit = true,
            Command::ListaNed => if !self.recept.is_empty() { 
                self.listval = (self.listval + 1).min(self.recept.len()) % self.recept.len();
            }
            Command::ListaUpp => self.listval = self.listval.wrapping_sub(1).min(self.recept.len().saturating_sub(1)),
            Command::Visa => if let Some(flik) = self.flikar.get_mut(self.flikval) {
                *flik = self.filtrerad_lista.get(self.listval).unwrap().to_path_buf();
            } else if let Some(flik) = self.flikar.first_mut() {
                *flik = self.filtrerad_lista.get(self.listval).unwrap().to_path_buf();
                self.flikval = 0;
            } else {
                self.flikar.push(self.filtrerad_lista.get(self.listval).unwrap().to_path_buf());
            },
            Command::VisaINyFlik => if self.flikar.len() < 4 {
                self.flikar.push(self.filtrerad_lista.get(self.listval).unwrap().to_path_buf());
            },
            Command::FlikNästa => if !self.flikar.is_empty() {
                self.flikval = (self.flikval + 1).min(self.flikar.len()) % self.flikar.len();
            },
            Command::FlikFörra => self.flikval = self.flikval.wrapping_sub(1).min(self.flikar.len().saturating_sub(1)),
            Command::StängFlik => if self.flikval <= self.flikar.len() {
                self.flikar.remove(self.flikval);
                self.flikval = self.flikval.min(self.flikar.len().saturating_sub(1));
            },
            Command::Lås => self.mode = UserMode::Locked,
            Command::LåsUpp => self.mode = UserMode::Normal,
            Command::NyttRecept => self.mode = UserMode::SkapaRecept(DEFAULT_RECEPT.to_string()),
            Command::SkapaRecept => todo!(),
            Command::ÖppnaSök => self.mode = UserMode::Sök(String::new()),
            Command::StängSök => self.mode = UserMode::Normal,
        }
    }
    
    fn refresh(&mut self) -> bool {
        let mut redraw = false;
        for event in self.receptmapp.changes() {
            match event {
                recept::FileEvent::Modify(path_buf) => {
                    let Ok(Some(new)) = Recept::try_parse_file(&path_buf) else {
                        continue;
                    };
                    *self.recept.entry(path_buf).or_default() = new;
                    redraw = true;
                },
                recept::FileEvent::Delete(path_buf) => {
                    self.recept.remove(&path_buf);
                    redraw = true;
                },
            }
        }
        if redraw {
            self.filtrerad_lista = Self::filtrera_recept(&self.recept, None);
        }
        redraw
    }
}