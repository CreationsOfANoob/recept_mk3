use std::{collections::HashMap, fmt::Display, option::Option, path::PathBuf};

use crossterm::event::KeyCode;
use recept::{App, CommandList, Config, ConfigLoader, TextEditor, Error, FolderWatcher, book::Recept, ui::{Border, BorderStyle, ListLayout, Rect, Side, TextBuffer, TextLayout}};
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

const TITEL: &str = 
"██░  ██░ ██████░ ██░  ██░
██░ ██░░██░░░░██░██░ ██░░
█████░░ ██░   ██░█████░░ 
██░░██░ ██░   ██░██░░██░ 
██░  ██░░██████░░██░  ██░
 ░░   ░░  ░░░░░░  ░░   ░░
                         
██████░  ██████░ ██░  ██░
██░░░██░██░░░░██░██░ ██░░
██████░░██░   ██░█████░░ 
██░░░██░██░   ██░██░░██░ 
██████░░░██████░░██░  ██░
 ░░░░░░   ░░░░░░  ░░   ░░"

// "██╗  ██╗ ██████╗ ██╗  ██╗
// ██║ ██╔╝██╔═══██╗██║ ██╔╝
// █████╔╝ ██║   ██║█████╔╝ 
// ██╔═██╗ ██║   ██║██╔═██╗ 
// ██║  ██╗╚██████╔╝██║  ██╗
// ╚═╝  ╚═╝ ╚═════╝ ╚═╝  ╚═╝
                         
// ██████╗  ██████╗ ██╗  ██╗
// ██╔══██╗██╔═══██╗██║ ██╔╝
// ██████╔╝██║   ██║█████╔╝ 
// ██╔══██╗██║   ██║██╔═██╗ 
// ██████╔╝╚██████╔╝██║  ██╗
// ╚═════╝  ╚═════╝ ╚═╝  ╚═╝"

// "▝▛ ▞▘     ▝▛   
//  ▙▞  ▗▞▀▚▖ ▌ ▗▀
//  ▛▚  ▌   ▐ ▙▄▘ 
// ▗▙ ▙▖▝▚▄▞▘▗▙ ▚▄

// ▝▛▀▚      ▝▛   
//  ▙▄▞ ▗▞▀▚▖ ▌ ▗▀
//  ▌ ▐ ▌   ▐ ▙▄▘ 
// ▗▙▄▞ ▝▚▄▞▘▗▙ ▚▄
// "


// r" █████   ████          █████     
// ░░███   ███░          ░░███      
//  ░███  ███     ██████  ░███ █████
//  ░███████     ███░░███ ░███░░███ 
//  ░███░░███   ░███ ░███ ░██████░  
//  ░███ ░░███  ░███ ░███ ░███░░███ 
//  █████ ░░████░░██████  ████ █████
// ░░░░░   ░░░░  ░░░░░░  ░░░░ ░░░░░ 
                                 
                                 
                                 
//  ███████████           █████     
// ░░███░░░░░███         ░░███      
//  ░███    ░███  ██████  ░███ █████
//  ░██████████  ███░░███ ░███░░███ 
//  ░███░░░░░███░███ ░███ ░██████░  
//  ░███    ░███░███ ░███ ░███░░███ 
//  ███████████ ░░██████  ████ █████
// ░░░░░░░░░░░   ░░░░░░  ░░░░ ░░░░░ "

;

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
    StängFlik,
    Lås,
    LåsUpp,
    NyttRecept,
    SkapaRecept,
    ÖppnaSök,
}

impl Display for Command {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Command::ListaNed => write!(f, "Nästa"),
            Command::ListaUpp => write!(f, "Föreg."),
            Command::Visa => write!(f, "Visa recept"),
            Command::VisaINyFlik => write!(f, "Ny flik"),
            Command::FlikNästa => write!(f, "Nästa flik"),
            Command::FlikFörra => write!(f, "Föreg. flik"),
            Command::StängFlik => write!(f, "Stäng flik"),
            Command::Lås => write!(f, "Lås"),
            Command::LåsUpp => write!(f, "Visa"),
            Command::NyttRecept => write!(f, "Nytt recept"),
            Command::SkapaRecept => write!(f, "Skapa"),
            Command::ÖppnaSök => write!(f, "Sök recept"),
        }
    }
}

#[derive(Default, Clone)]
enum UserMode {
    Locked,
    #[default]
    Normal,
    SkapaRecept(TextEditor),
    Sök(TextEditor),
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
    sökfilter: String,
    mode: UserMode,
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
        let mut slef = Self { recept, listval: 0, flikval: 0, flikar: Vec::new(), mode: UserMode::Normal, receptmapp, filtrerad_lista: Vec::new(), sökfilter: String::new() };
        slef.filtrera_recept();
        Ok(slef)
    }

    fn filtrera_recept(&mut self) {
        let mut träffar = Vec::new();
        for (path, recept) in &self.recept {
            if recept.matches(&self.sökfilter) {
                träffar.push((path.to_path_buf(), recept.rubrik()));
            }
        }
        träffar.sort_by_key(|(_, n)| *n);
        self.filtrerad_lista = träffar.into_iter().map(|(r, _)| r).collect();
    }
    
    fn draw_recipes<T: TextBuffer>(&self, rect: Rect, buf: &mut T) -> std::io::Result<()> {
        for (flik_i, (mut flik_rect, recept_path)) in rect.to_grid(self.flikar.len() as u16, 1, 1, false).into_iter().zip(self.flikar.iter()).enumerate() {
            flik_rect.limit_w(120, true);
            if self.flikval == flik_i && self.flikar.len() > 1 && matches!(self.mode, UserMode::Normal){
                Border::new(flik_rect, BorderStyle::THIN).render(buf)?;
            }
            if flik_i > 0 && self.flikar.len() > 1 && matches!(self.mode, UserMode::Locked){
                Border::new(flik_rect.cut(Side::Left, 1).moved(-1, 0), BorderStyle::LIGHT_DOTTED_LINE_V).render(buf)?;
            }
            flik_rect.inset_mut(2, 1);
            if let Some(recept) = self.recept.get(recept_path) {
                recept.render(flik_rect, buf)?;
            }
        }
        Ok(())
    }

    fn draw_sidebar<T: TextBuffer>(&self, mut rect: Rect, buf: &mut T) -> std::io::Result<()> {
        Border::new(rect.cut_mut(Side::Right, 1).with_cut(Side::Bottom, 1), BorderStyle::LIGHT_DOTTED_LINE_V).render(buf)?;
        if let UserMode::Sök(textedit) = &self.mode && let Some((mut search, list)) = rect.split(Side::Top, 3) {
            search.inset_mut(1, 0);
            Border::new(search, BorderStyle::THIN).render(buf)?;
            search.inset_mut(2, 1);
            search.cut_mut(Side::Right, 1);
            textedit.render("Sök recept...", search, buf)?;
            rect = list;
        } else {
            rect.cut_mut(Side::Top, 1);
        }
        rect.inset_mut(1, 0);
        rect.cut_mut(Side::Left, 1);
        if self.sökfilter != "" || matches!(self.mode, UserMode::Sök(_)) {
            TextLayout::new().with_text(format!("Sökresultat för \"{}\":", self.sökfilter), 0).render_cut(&mut rect, buf)?;
            rect.cut_mut(Side::Top, 1);
        }
        rect.cut_mut(Side::Right, 1);
        let mut lista = ListLayout::new().with_items(self.filtrerad_lista.iter().map(|recept| self.recept.get(recept).unwrap().rubrik().to_string()));
        if !matches!(self.mode, UserMode::Sök(_)) {
            lista = lista.with_selected(self.listval);
        }
        lista.render(rect.moved(-1, 0), buf)
    }
    
    fn draw_editor<T: TextBuffer>(&self, text: &TextEditor, mut rect: Rect, buf: &mut T) -> std::io::Result<()> {
        rect.inset_mut(2, 2);
        Border::new(rect, BorderStyle::THIN).render(buf)?;
        rect.inset_mut(3, 2);
        text.render(DEFAULT_RECEPT, rect, buf)
    }
}

impl App<Command> for ReceptApp {
    fn draw<T: TextBuffer>(&self, buf: &mut T) -> std::io::Result<()> {
        let mut window_rect = buf.safe_rect();
        window_rect.inset_mut(0, 0);

        match &self.mode {
            UserMode::Locked => self.draw_recipes(window_rect, buf)?,
            UserMode::Normal | UserMode::Sök { .. } => if let Some((sidebar, recipe_rect)) = window_rect.split(Side::Left, 30) {
                if self.flikar.is_empty() {
                    TextLayout::new().with_text(TITEL.to_string(), 0).centered().render(recipe_rect, buf)?;
                } else {
                    self.draw_recipes(recipe_rect, buf)?;
                }
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
            UserMode::Sök(_) => CommandList::new(),
        }
    }
    
    fn execute_command(&mut self, command: Command) {
        match command {
            Command::ListaNed => increment_wrap(&mut self.listval, self.filtrerad_lista.len()),
            Command::ListaUpp => decrement_wrap(&mut self.listval, self.filtrerad_lista.len()),
            Command::Visa => {
                let Some(recept) = self.filtrerad_lista.get(self.listval).cloned() else {
                    return;
                };
                if let Some(flik) = self.flikar.get_mut(self.flikval) {
                    *flik = recept;
                } else if let Some(flik) = self.flikar.first_mut() {
                    *flik = recept;
                    self.flikval = 0;
                } else {
                    self.flikar.push(recept);
                }
            },
            Command::VisaINyFlik => if self.flikar.len() < 4 && let Some(recept) = self.filtrerad_lista.get(self.listval).cloned() {
                self.flikar.push(recept);
                self.flikval = self.flikar.len().saturating_sub(1);
            },
            Command::FlikNästa => increment_wrap(&mut self.flikval, self.flikar.len()),
            Command::FlikFörra => decrement_wrap(&mut self.flikval, self.flikar.len()),
            Command::StängFlik => if !self.flikar.is_empty() {
                self.flikar.remove(self.flikval.min(self.flikar.len() - 1));
                self.flikval = self.flikval.min(self.flikar.len().saturating_sub(1));
            },
            Command::Lås => self.mode = UserMode::Locked,
            Command::LåsUpp => self.mode = UserMode::Normal,
            Command::NyttRecept => self.mode = UserMode::SkapaRecept(TextEditor::default()),
            Command::SkapaRecept => todo!(),
            Command::ÖppnaSök => {
                self.mode = UserMode::Sök(TextEditor::default().single_line_mode());
                self.sökfilter = String::new();
                self.filtrera_recept();
                self.listval = 0;
            }
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
            self.filtrera_recept();
        }
        redraw
    }
    
    fn edit_text(&mut self, edit_event: recept::TextEditEvent) {
        let mut exit_search = false;
        match &mut self.mode {
            UserMode::Sök(editor) => {
                if editor.edit(edit_event) {
                    exit_search = true;
                }
                self.sökfilter = editor.to_string();
            },
            _ => ()
        }
        self.filtrera_recept();
        if exit_search {
            self.mode = UserMode::Normal;
        }
    }
    
    fn is_editing_text(&self) -> bool {
        matches!(self.mode, UserMode::Sök(_))
    }
}

fn increment_wrap(i: &mut usize, len: usize) {
    if len == 0 {
        *i = 0;
    } else {
        *i = (*i + 1).min(len) % len;
    }
}

fn decrement_wrap(i: &mut usize, len: usize) {
    if len == 0 {
        *i = 0;
    } else {
        *i = (i.wrapping_sub(1)).min(len) % len;
    }
}