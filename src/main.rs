use std::{collections::HashMap, fmt::Display, option::Option, path::{Path, PathBuf}};

use crossterm::event::KeyCode;
use recept::{App, CommandList, Config, ConfigLoader, Error, FolderWatcher, TextEditor, book::{Recept, Storhet, Värde}, ui::{Border, BorderStyle, ListLayout, Rect, Side, TextBuffer, TextLayout}};
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
// "██░  ██░ ██████░ ██░  ██░
// ██░ ██░░██░░░░██░██░ ██░░
// █████░░ ██░   ██░█████░░ 
// ██░░██░ ██░   ██░██░░██░ 
// ██░  ██░░██████░░██░  ██░
//  ░░   ░░  ░░░░░░  ░░   ░░
                         
// ██████░  ██████░ ██░  ██░
// ██░░░██░██░░░░██░██░ ██░░
// ██████░░██░   ██░█████░░ 
// ██░░░██░██░   ██░██░░██░ 
// ██████░░░██████░░██░  ██░
//  ░░░░░░   ░░░░░░  ░░   ░░"

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


r" █████   ████          █████     
░░███   ███░          ░░███      
 ░███  ███     ██████  ░███ █████
 ░███████     ███░░███ ░███░░███ 
 ░███░░███   ░███ ░███ ░██████░  
 ░███ ░░███  ░███ ░███ ░███░░███ 
 █████ ░░████░░██████  ████ █████
░░░░░   ░░░░  ░░░░░░  ░░░░ ░░░░░ 
                                 
                                 
                                 
 ███████████           █████     
░░███░░░░░███         ░░███      
 ░███    ░███  ██████  ░███ █████
 ░██████████  ███░░███ ░███░░███ 
 ░███░░░░░███░███ ░███ ░██████░  
 ░███    ░███░███ ░███ ░███░░███ 
 ███████████ ░░██████  ████ █████
░░░░░░░░░░░   ░░░░░░  ░░░░ ░░░░░ "

;

const DEFAULT_RECEPT: &str = "Skriv eller klistra in ett recept...";

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
    Spara,
    ÖppnaSök,
    Cancel,
    Redigera,
    Portionsjustera,
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
            Command::Spara => write!(f, "Spara"),
            Command::ÖppnaSök => write!(f, "Sök recept"),
            Command::Cancel => write!(f, "Avbryt"),
            Command::Redigera => write!(f, "Redigera"),
            Command::Portionsjustera => write!(f, "Portionsjustera"),
        }
    }
}

#[derive(Default)]
enum UserMode {
    Locked,
    #[default]
    Normal,
    SkapaRecept { editor: TextEditor, source: Option<PathBuf>, result: Result<(), Error> },
    Sök(TextEditor),
    JusteraStorlek(TextEditor),
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
    flikar: Vec<(PathBuf, Option<Storhet>)>,
    sökfilter: String,
    mode: UserMode,
    receptmapp: FolderWatcher,
    config: ConfigLoader<ReceptConfig>,
}

impl ReceptApp {
    pub fn new() -> Result<Self, Error> {
        let mut config: ConfigLoader<ReceptConfig> = ConfigLoader::new("Receptkalkylator")?;
        let receptmapp = FolderWatcher::new(config.get()?.receptmapp.clone())?;
        let mut recept = HashMap::new();
        for recept_path in receptmapp.files()? {
            if let Ok(Ok(tolkat)) = Recept::try_parse_file(&recept_path) {
                recept.insert(recept_path, tolkat);
            }
        }
        let mut slef = Self { recept, listval: 0, flikval: 0, flikar: Vec::new(), mode: UserMode::Normal, receptmapp, filtrerad_lista: Vec::new(), sökfilter: String::new(), config };
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
        for (flik_i, (mut flik_rect, (recept_path, storlek))) in rect.to_grid(self.flikar.len() as u16, 1, 1, false).into_iter().zip(self.flikar.iter()).enumerate() {
            flik_rect.limit_w(120, true);
            if self.flikval == flik_i && self.flikar.len() > 1 && matches!(self.mode, UserMode::Normal | UserMode::JusteraStorlek(_)){
                Border::new(flik_rect, BorderStyle::THIN).render(buf)?;
            }
            if flik_i > 0 && self.flikar.len() > 1 && matches!(self.mode, UserMode::Locked){
                Border::new(flik_rect.cut(Side::Left, 1).moved(-1, 0), BorderStyle::LIGHT_DOTTED_LINE_V).render(buf)?;
            }
            flik_rect.inset_mut(2, 1);

            if let Some(recept) = self.recept.get(recept_path) {
                recept.render(flik_rect, buf, storlek)?;
            }
            if let UserMode::JusteraStorlek(editor) = &self.mode {
                let storlek_rect = flik_rect.cut_mut(Side::Top, 3);
                editor.render("Ny storlek...", storlek_rect.with_inset(1, 1), buf)?;
                Border::new(storlek_rect, BorderStyle::THIN).render(buf)?;
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
    
    fn draw_editor<T: TextBuffer>(&self, text: &TextEditor, source: &Option<PathBuf>, result: &Result<(), Error>, mut rect: Rect, buf: &mut T) -> std::io::Result<()> {
        rect.inset_mut(2, 2);
        if let Some(source) = source {
            let text = format!("Redigerar: {source:?}");
            TextLayout::new().with_text(text, 0).render(rect.cut_mut(Side::Top, 2), buf)?;
        }
        if let Err(err) = result {
            let err_text = format!("Fel när receptet skulle sparas: {err:?}");
            TextLayout::new().with_text(err_text, 0).render_cut(&mut rect, buf)?;
            rect.cut_mut(Side::Top, 1);
        }
        Border::new(rect, BorderStyle::LINE_H).render(buf)?;
        rect.inset_mut(2, 2);
        
        text.render(DEFAULT_RECEPT, rect, buf)
    }

    fn spara_recept(string: String, config: &mut ConfigLoader<ReceptConfig>, source: &Option<PathBuf>) -> Result<PathBuf, Error> {
        let recept = Recept::try_parse(string)?;
        let rubrik = recept.rubrik();
        let namn = if rubrik != "" {
            rubrik
        } else {
            "nytt recept"
        };
        let path = if let Some(source_path) = source {
            source_path.clone()
        } else {
            let base_path = config.get()?.receptmapp.clone();
            let mut path_candidate = base_path.join(namn).with_extension("txt");
            let mut i = 0;
            while path_candidate.exists() {
                path_candidate = base_path.join(format!("{namn}.{i:#03}")).with_extension("txt");
                i += 1;
            }
            path_candidate
        };
        recept.spara(&path).map(|_| path)
    }
    
    fn portionsstorlek(path: &Path) -> Option<Storhet> {
        if let Ok(Ok(recept)) = Recept::try_parse_file(path) {
            recept.storlek().cloned()
        } else {
            None
        }
    }
}

impl App<Command> for ReceptApp {
    fn draw<T: TextBuffer>(&self, buf: &mut T) -> std::io::Result<()> {
        let mut window_rect = buf.safe_rect();
        window_rect.inset_mut(0, 0);

        match &self.mode {
            UserMode::Locked => self.draw_recipes(window_rect, buf)?,
            UserMode::Normal | UserMode::Sök { .. } | UserMode::JusteraStorlek(_) => if let Some((sidebar, recipe_rect)) = window_rect.split(Side::Left, 30) {
                if self.flikar.is_empty() {
                    TextLayout::new().with_text(TITEL.to_string(), 0).centered().render(recipe_rect, buf)?;
                } else {
                    self.draw_recipes(recipe_rect, buf)?;
                }
                self.draw_sidebar(sidebar, buf)?;
            } else {
                TextLayout::new().with_text("Fönstret är för litet!".to_string(), 0).render(window_rect, buf)?;
            },
            UserMode::SkapaRecept { editor, result, source } => self.draw_editor(editor, source, result, window_rect, buf)?,
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
                .command_if(self.flikval < self.flikar.len(), KeyCode::Char('j'), Command::Portionsjustera)
                .command(KeyCode::Char('t'), Command::VisaINyFlik)

                .hidden(KeyCode::Char('d'), Command::FlikNästa)
                .hidden(KeyCode::Char('a'), Command::FlikFörra)
                .hidden(KeyCode::Right, Command::FlikNästa)
                .hidden(KeyCode::Left, Command::FlikFörra)

                .command_if(!self.filtrerad_lista.is_empty(), KeyCode::Char('e'), Command::Redigera),
            UserMode::SkapaRecept { .. } => CommandList::new()
                .ctrl_with_name_override(KeyCode::Char('x'), Command::Spara, "Spara recept".to_string())
                .command(KeyCode::Esc, Command::Cancel),
            UserMode::Sök(_) => CommandList::new()
                .command(KeyCode::Esc, Command::Cancel),
            UserMode::JusteraStorlek(_) => CommandList::new()
                .command(KeyCode::Esc, Command::Cancel)
        }
    }
    
    fn execute_command(&mut self, command: Command) {
        let mut reset = false;
        match command {
            Command::ListaNed => increment_wrap(&mut self.listval, self.filtrerad_lista.len()),
            Command::ListaUpp => decrement_wrap(&mut self.listval, self.filtrerad_lista.len()),
            Command::Visa => {
                let Some(path) = self.filtrerad_lista.get(self.listval).cloned() else {
                    return;
                };
                let portionsstorlek = Self::portionsstorlek(&path);
                if let Some(flik) = self.flikar.get_mut(self.flikval) {
                    *flik = (path, portionsstorlek);
                } else if let Some(flik) = self.flikar.first_mut() {
                    *flik = (path, portionsstorlek);
                    self.flikval = 0;
                } else {
                    self.flikar.push((path, portionsstorlek));
                }
            },
            Command::VisaINyFlik => if self.flikar.len() < 4 && let Some(path) = self.filtrerad_lista.get(self.listval).cloned() {
                let portionsstorlek = Self::portionsstorlek(&path);
                self.flikar.push((path, portionsstorlek));
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
            Command::NyttRecept => self.mode = UserMode::SkapaRecept { editor: TextEditor::default(), result: Ok(()), source: None },
            Command::Spara => if let UserMode::SkapaRecept { editor, result, source } = &mut self.mode {
                let res = Self::spara_recept(editor.to_string(), &mut self.config, source);
                match res {
                    Ok(path) => {
                        reset = true;
                        self.receptmapp.touch(path);
                        *result = Ok(());
                    },
                    Err(err) => *result = Err(err)
                }
            } else if let UserMode::JusteraStorlek(editor) = &self.mode && let Some((_, Some(storlek))) = self.flikar.get_mut(self.flikval) {
                if let Some(värde) = Värde::try_parse(&editor.to_string()) {
                    storlek.värde = värde;
                }
            },
            Command::ÖppnaSök => {
                self.mode = UserMode::Sök(TextEditor::default().single_line_mode());
                self.sökfilter = String::new();
                self.filtrera_recept();
                self.listval = 0;
            }
            Command::Cancel => match self.mode {
                UserMode::SkapaRecept { .. } => self.mode = UserMode::Normal,
                UserMode::Sök(_) => {
                    self.mode = UserMode::Normal
                }
                UserMode::JusteraStorlek(_) => self.mode = UserMode::Normal,
                _ => (),
            },
            Command::Redigera => if let Some(source) = self.filtrerad_lista.get(self.listval) {
                use std::io::Read;
                let mut buf = String::new();
                let result = match std::fs::File::open(source) {
                    Ok(mut f) => f.read_to_string(&mut buf).map(|_| ()).map_err(|err| Error::Io(err)),
                    Err(err) => Err(Error::Io(err)),
                };
                self.mode = UserMode::SkapaRecept { editor: TextEditor::from_string(buf), source: Some(source.to_path_buf()), result }
            },
            Command::Portionsjustera => if let Some((_, _)) = self.flikar.get_mut(self.flikval) {
                self.mode = UserMode::JusteraStorlek(TextEditor::default().single_line_mode())
            },
        }
        if reset {
            self.refresh();
            self.mode = UserMode::Normal;
        }
    }
    
    fn refresh(&mut self) -> bool {
        let mut redraw = false;
        for event in self.receptmapp.changes() {
            match event {
                recept::FileEvent::Modify(path_buf) => {
                    let Ok(Ok(new)) = Recept::try_parse_file(&path_buf) else {
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
            UserMode::SkapaRecept { editor, .. } => {
                editor.edit(edit_event);
            }
            UserMode::JusteraStorlek(editor) => if let Some((_, Some(storlek))) = self.flikar.get_mut(self.flikval) {
                let exit = editor.edit(edit_event);
                if let Some(värde) = Värde::try_parse(&editor.to_string()) {
                    storlek.värde = värde;
                }
                if exit {
                    exit_search = true;
                }
            }
            _ => ()
        }
        self.filtrera_recept();
        if exit_search {
            self.mode = UserMode::Normal;
        }
    }
    
    fn is_editing_text(&self) -> bool {
        matches!(self.mode, UserMode::Sök(_) | UserMode::SkapaRecept { .. } | UserMode::JusteraStorlek(_))
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
        *i = (i.wrapping_sub(1)).min(len.saturating_sub(1)) % len;
    }
}