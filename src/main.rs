use std::{collections::HashMap, fmt::Display, option::Option, path::{Path, PathBuf}, usize};

use crossterm::event::KeyCode;
use recept::{App, CommandList, Commands, Config, ConfigLoader, Error, FolderWatcher, TextEditResponse, TextEditor, bok::{Recept, Storhet, Värde}, ui::{Border, BorderStyle, Drawer, GridDirection, ListLayout, Logger, Rect, Side, TextLayout}};
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
" █████   ████          █████     
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
    ÄndraTal(bool),
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
    Redigera,
    Portionsjustera,
    Delete,
}

impl Display for Command {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Command::ListaNed => write!(f, "Nästa"),
            Command::ListaUpp => write!(f, "Föreg."),
            Command::ÄndraTal(true) => write!(f, "Öka"),
            Command::ÄndraTal(false) => write!(f, "Minska"),
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
            Command::Redigera => write!(f, "Redigera"),
            Command::Portionsjustera => write!(f, "Portionsjustera"),
            Command::Delete => write!(f, "Radera"),
        }
    }
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

enum Scen {
    Låst,
    ReceptBläddrare {
        editor: Option<(EditorType, TextEditor)>,
    },
    SkapaRecept {
        editor: TextEditor,
        källa: Option<PathBuf>,
        result: Result<(), Error>,
    },
}

impl Default for Scen {
    fn default() -> Self {
        Scen::ReceptBläddrare { editor: None }
    }
}

#[derive(PartialEq, Eq)]
enum EditorType {
    Sök,
    Portioner,
}

#[derive(Default)]
struct KokBok {
    recept: HashMap<PathBuf, Recept>,
    filtrerad_lista: Vec<(PathBuf, String)>,
}

impl KokBok {
    fn new() -> Self {
        Self::default()
    }
    
    fn filtrera(&mut self, filter: &str) {
        let mut träffar = Vec::new();
        for (path, recept) in &self.recept {
            if recept.matches(filter) {
                träffar.push((path.to_path_buf(), recept.rubrik().to_string()));
            }
        }
        träffar.sort_by(|(_, a), (_, b)| a.cmp(b));
        self.filtrerad_lista = träffar;
    }

    fn reset_filter(&mut self) {
        self.filtrera("");
    }
    
    fn insert(&mut self, k: PathBuf, v: Recept) {
        let r = v.rubrik().to_string();
        if let Err(insert_i) = self.filtrerad_lista.binary_search_by(|(_, k)| k.cmp(&r)) {
            self.filtrerad_lista.insert(insert_i, (k.clone(), v.rubrik().to_string()));
        }
        self.recept.insert(k, v);
    }

    fn get(&self, k: &PathBuf) -> Option<&Recept> {
        self.recept.get(k)
    }
    
    fn filtered_is_empty(&self) -> bool {
        self.filtrerad_lista.is_empty()
    }
    
    fn is_empty(&self) -> bool {
        self.recept.is_empty()
    }
    
    fn filtrerade_rubriker(&self) -> Vec<String> {
        self.filtrerad_lista.iter().map(|recept| recept.1.clone()).collect()
    }
    
    fn get_listindex(&self, index: usize) -> Option<&PathBuf> {
        self.filtrerad_lista.get(index).map(|(p, _)| p)
    }
}

struct ReceptApp {
    kokbok: KokBok,
    listval: usize,
    flikval: usize,
    
    flikar: Vec<(PathBuf, Option<Storhet>)>,
    scen: Scen,
    receptmapp: FolderWatcher,
    config: ConfigLoader<ReceptConfig>,
}

impl ReceptApp {
    pub fn new() -> Result<Self, Error> {
        let mut config: ConfigLoader<ReceptConfig> = ConfigLoader::new("Receptkalkylator")?;
        let receptmapp = FolderWatcher::new(config.get()?.receptmapp.clone())?;
        let mut kokbok = KokBok::new();
        for recept_path in receptmapp.files()? {
            if let Ok(Ok(tolkat)) = Recept::try_parse_file(&recept_path) {
                kokbok.insert(recept_path, tolkat);
            }
        }
        Ok(Self { kokbok, listval: 0, flikval: 0, flikar: Vec::new(), scen: Scen::ReceptBläddrare { editor: None }, receptmapp, config })
    }
    
    fn draw_recipes(&self, rect: Rect, buf: &mut Drawer, commands: &mut Commands<Command>) {
        for (flik_i, (mut flik_rect, (recept_path, storlek))) in rect.divide_grid(self.flikar.len() as u16, 1, 1, 1, GridDirection::LeftDown).into_iter().zip(self.flikar.iter()).enumerate() {
            flik_rect.limit_w(120, true);
            if self.flikval == flik_i {
                match &self.scen {
                    Scen::ReceptBläddrare { editor: Some((EditorType::Portioner, editor)) } => {
                        let mut portioner_rect = flik_rect.cut_mut(Side::Top, 4).with_inset(1, 0);
                        commands.new_rect(CommandList::new(portioner_rect.cut_mut(Side::Bottom, 1))
                            .command(KeyCode::Up, Command::ÄndraTal(true))
                            .command(KeyCode::Down, Command::ÄndraTal(false)));
                        editor.render("Ny storlek...", portioner_rect.with_inset(2, 1), buf);
                        Border::new(portioner_rect, BorderStyle::THIN).render(buf);
                    },
                    Scen::ReceptBläddrare { editor: None } => {
                        if self.flikar.len() > 1 {
                            Border::new(flik_rect, BorderStyle::THIN).render(buf);
                        }
                        let mut local_commands = CommandList::new(flik_rect.with_inset(2, 1));
                        if self.flikar.len() > 1 {
                            local_commands = local_commands
                                .hidden(KeyCode::Char('d'), Command::FlikNästa)
                                .hidden(KeyCode::Char('a'), Command::FlikFörra)
                                .command(KeyCode::Left, Command::FlikFörra)
                                .command(KeyCode::Right, Command::FlikNästa)
                                .ctrl(KeyCode::Char('w'), Command::StängFlik);
                        }
                        local_commands = local_commands.command(KeyCode::Char('j'), Command::Portionsjustera);
                        commands.new_rect(local_commands);
                    }
                    _ => (),
                }
            }
            if flik_i > 0 && self.flikar.len() > 1 && matches!(self.scen, Scen::Låst) {
                Border::new(flik_rect.cut(Side::Left, 1).moved(-1, -1), BorderStyle::LIGHT_DOTTED_LINE_V).render(buf);
            }
            flik_rect.inset_mut(2, 1);

            if let Some(recept) = self.kokbok.get(recept_path) {
                recept.render(flik_rect, buf, storlek);
            }
        }
    }

    fn draw_sidebar(&self, mut rect: Rect, buf: &mut Drawer, commands: &mut Commands<Command>, editor: &Option<(EditorType, TextEditor)>) {
        Border::new(rect.cut_mut(Side::Right, 1), BorderStyle::LIGHT_DOTTED_LINE_V).render(buf);
        rect.cut_mut(Side::Left, 1);
        let mut local_commands = CommandList::new(rect)
            .command(KeyCode::Down, Command::ListaNed)
            .command(KeyCode::Up, Command::ListaUpp)
            .command(KeyCode::Enter, Command::Visa);

        if let Some((EditorType::Sök, textedit)) = editor {
            if let Some((mut search, list)) = rect.split(Side::Top, 4) {
                rect = list;
                TextLayout::new().with_text("Sökresultat:".into(), 0).render(search.cut_mut(Side::Bottom, 1).with_cut(Side::Left, 1), buf);
                Border::new(search.moved(-1, 0), BorderStyle::THIN).render(buf);
                textedit.render("Sök recept...", search.with_inset(1, 2).with_cut(Side::Right, 1), buf);
            }
        } else {
            local_commands = local_commands
                .command(KeyCode::Char('n'), Command::NyttRecept)
                .hidden(KeyCode::Char('+'), Command::NyttRecept);
            if !self.kokbok.is_empty() {
                local_commands = local_commands.command(KeyCode::Char('f'), Command::ÖppnaSök);
                if self.flikar.len() < 4 {
                    local_commands = local_commands.command(KeyCode::Char('t'), Command::VisaINyFlik);
                }
            }
            if !self.flikar.is_empty() {
                local_commands = local_commands.command(KeyCode::Char('q'), Command::Lås);
            }
            if !self.kokbok.filtered_is_empty() {
                local_commands = local_commands.command(KeyCode::Char('e'), Command::Redigera);
                local_commands = local_commands.ctrl(KeyCode::Char('d'), Command::Delete);
            }
        }
        rect.inset_mut(1, 1);
        if !matches!(editor, Some((EditorType::Portioner, _))) {
            commands.new_rect(local_commands);
        }
        rect.cut_mut(Side::Right, 1);
        ListLayout::new().with_items(self.kokbok.filtrerade_rubriker().into_iter()).with_selected(self.listval).render(rect.moved(-1, 0), buf);
    }
    
    fn draw_editor(&self, text: &TextEditor, source: &Option<PathBuf>, result: &Result<(), Error>, mut rect: Rect, buf: &mut Drawer) {
        rect.inset_mut(2, 2);
        if let Some(source) = source {
            let text = format!("Redigerar: {source:?}");
            TextLayout::new().with_text(text, 0).render(rect.cut_mut(Side::Top, 2), buf);
        }
        if let Err(err) = result {
            let err_text = format!("Fel när receptet skulle sparas: {err:?}");
            TextLayout::new().with_text(err_text, 0).render_cut(&mut rect, buf);
            rect.cut_mut(Side::Top, 1);
        }
        Border::new(rect, BorderStyle::LINE_H).render(buf);
        rect.inset_mut(2, 2);
        
        text.render(DEFAULT_RECEPT, rect, buf)
    }
    
    fn portionsstorlek(path: &Path) -> Option<Storhet> {
        if let Ok(Ok(recept)) = Recept::try_parse_file(path) {
            recept.storlek().cloned()
        } else {
            None
        }
    }
    
    fn öppna_recept(&mut self, ny_flik: bool) {
        let Some(path) = self.kokbok.get_listindex(self.listval).cloned() else {
            return;
        };
        let portionsstorlek = Self::portionsstorlek(&path);
        if let Some(flik) = self.flikar.get_mut(self.flikval) && !ny_flik {
            *flik = (path, portionsstorlek);
        } else if let Some(flik) = self.flikar.first_mut() && !ny_flik {
            *flik = (path, portionsstorlek);
            self.flikval = 0;
        } else if self.flikar.len() < 4 {
            self.flikval = self.flikar.len();
            self.flikar.push((path, portionsstorlek));
        }
    }

    fn spara_recept(string: String, källa: &Option<PathBuf>, config: &mut ConfigLoader<ReceptConfig>, receptmapp: &mut FolderWatcher, result: &mut Result<(), Error>) {
        let base = match config.get() {
            Ok(ok) => &ok.receptmapp,
            Err(err) => {
                *result = Err(err);
                return;
            },
        };
        let res = spara_recept(string, base, källa);
        match res {
            Ok(path) => {
                receptmapp.touch(path);
                *result = Ok(());
            },
            Err(err) => *result = Err(err)
        }
    }
}

impl App<Command> for ReceptApp {
    fn draw(&self, buf: &mut Drawer, commands: &mut Commands<Command>) {
        let mut window_rect = buf.safe_rect();
        window_rect.inset_mut(0, 0);

        match &self.scen {
            Scen::Låst => {
                self.draw_recipes(window_rect, buf, commands);
                commands.new_rect(CommandList::new(window_rect)
                    .command(KeyCode::Char('q'), Command::LåsUpp));
            },
            Scen::ReceptBläddrare { editor } => if let Some((sidebar, recipe_rect)) = window_rect.split(Side::Left, 30) {
                if self.flikar.is_empty() {
                    TextLayout::new().with_text(TITEL.to_string(), 0).centered().render(recipe_rect, buf);
                } else {
                    self.draw_recipes(recipe_rect, buf, commands);
                }
                self.draw_sidebar(sidebar, buf, commands, editor);
            } else {
                TextLayout::new().with_text("Fönstret är för litet!".to_string(), 0).render(window_rect, buf);
            },
            Scen::SkapaRecept { editor, källa, result } => {
                self.draw_editor(editor, källa, result, window_rect, buf);
                commands.new_rect(CommandList::new(window_rect)
                    .ctrl(KeyCode::Char('x'), Command::Spara));
            },
        }
    }

    fn execute_command(&mut self, command: &Command, log: &mut Logger) -> Result<(), Error> {
        let mut save = false;
        match command {
            Command::ListaNed => increment_wrap(&mut self.listval, self.kokbok.filtrerad_lista.len()),
            Command::ListaUpp => decrement_wrap(&mut self.listval, self.kokbok.filtrerad_lista.len()),
            Command::ÄndraTal(upp) => if let Scen::ReceptBläddrare { editor: Some((EditorType::Portioner, editor)) } = &mut self.scen && let Some(storlek) = Värde::try_parse(&editor.to_string()){
                let nytt_värde = if *upp { storlek + 1.0 } else { storlek.saturating_sub(1.0) };
                editor.set_text(&nytt_värde.to_string());
                editor.set_cursor_last();
            }
            Command::Visa => {
                self.öppna_recept(false);
                self.scen = Scen::default();
            },
            Command::VisaINyFlik => self.öppna_recept(true),
            Command::FlikNästa => increment_wrap(&mut self.flikval, self.flikar.len()),
            Command::FlikFörra => decrement_wrap(&mut self.flikval, self.flikar.len()),
            Command::StängFlik => if !self.flikar.is_empty() {
                self.flikar.remove(self.flikval.min(self.flikar.len() - 1));
                self.flikval = self.flikval.min(self.flikar.len().saturating_sub(1));
            },
            Command::Lås => self.scen = Scen::Låst,
            Command::LåsUpp => self.scen = Scen::default(),
            Command::NyttRecept => self.scen = Scen::SkapaRecept { editor: TextEditor::default(), källa: None, result: Ok(()) },
            Command::Spara => save = matches!(self.scen, Scen::SkapaRecept { .. }),
            Command::ÖppnaSök => {
                self.scen = Scen::ReceptBläddrare { editor: Some((EditorType::Sök, TextEditor::single_line()))};
                self.listval = 0;
            },
            Command::Redigera => if let Some(source) = self.kokbok.get_listindex(self.listval) {
                use std::io::Read;
                let mut buf = String::new();
                let result = match std::fs::File::open(source) {
                    Ok(mut f) => f.read_to_string(&mut buf).map(|_| ()).map_err(|err| Error::Io(err)),
                    Err(err) => Err(Error::Io(err)),
                };
                self.scen = Scen::SkapaRecept { editor: TextEditor::from_string(buf), källa: Some(source.to_path_buf()), result }
            },
            Command::Portionsjustera => if let Some((_, storlek)) = self.flikar.get_mut(self.flikval) {
                let string = if let Some(strl) = storlek {
                    strl.värde.to_string()
                } else {
                    String::new()
                };
                self.scen = Scen::ReceptBläddrare { editor: Some((EditorType::Portioner, TextEditor::from_string(string).single_line_mode().with_cursor_last())) };
            },
            Command::Delete => if let Some(path) = self.kokbok.get_listindex(self.listval) {
                trash::delete(path)?;
                self.receptmapp.touch(path.to_path_buf());
            }
        }
        if save && let Scen::SkapaRecept { editor, result, källa } = &mut self.scen {
            Self::spara_recept(editor.to_string(), källa, &mut self.config, &mut self.receptmapp, result);
            if result.is_ok() {
                self.refresh(log);
                self.scen = Scen::default();
            }
        }
        Ok(())
    }
    
    fn refresh(&mut self, log: &mut Logger) -> bool {
        let mut redraw = false;

        let keys: Vec<PathBuf> = self.kokbok.recept.keys().cloned().collect();
        for key in keys {
            if !key.exists() {
                self.kokbok.recept.remove(key.as_path());
                redraw = true;
            }
        }
        for event in self.receptmapp.changes() {
            match event {
                recept::FileEvent::Modify(path_buf) => {
                    log.log(format!("{path_buf:?}: {:?}", self.kokbok.recept.remove(&path_buf)));
                    redraw = true;
                    let Ok(Ok(new)) = Recept::try_parse_file(&path_buf) else {
                        continue;
                    };
                    *self.kokbok.recept.entry(path_buf).or_default() = new;
                },
                recept::FileEvent::Delete(path_buf) => {
                    log.log(format!("{path_buf:?}: {:?}", self.kokbok.recept.remove(&path_buf)));
                    self.kokbok.recept.remove(&path_buf);
                    redraw = true;
                },
            }
        }
        if redraw {
            if let Scen::ReceptBläddrare { editor: Some((EditorType::Sök, editor)) } = &self.scen {
                self.kokbok.filtrera(&editor.to_string());
            } else {
                self.kokbok.filtrera("");
            }
        }
        redraw
    }
    
    fn edit_text(&mut self, edit_event: recept::TextEditEvent, log: &mut Logger) {
        let mut reset = false;
        match &mut self.scen {
            Scen::ReceptBläddrare { editor: Some((ty, editor)) } => {
                match editor.edit(edit_event) {
                    Some(TextEditResponse::Escape) => reset = true,
                    Some(TextEditResponse::Return) => match ty {
                        EditorType::Sök => {
                            self.öppna_recept(false);
                            reset = true;
                        },
                        EditorType::Portioner => if let Some((_, Some(storlek))) = self.flikar.get_mut(self.flikval) {
                            if let Some(värde) = Värde::try_parse(&editor.to_string()) {
                                storlek.värde = värde;
                            }
                            reset = true;
                        },
                    },
                    _ => if *ty == EditorType::Sök {
                        self.listval = 0;
                        self.kokbok.filtrera(&editor.to_string());
                    },
                }
            },
            Scen::SkapaRecept { editor, källa, result } => {
                match editor.edit(edit_event) {
                    Some(TextEditResponse::Escape) => reset = true,
                    Some(TextEditResponse::Return) => {
                        Self::spara_recept(editor.to_string(), källa, &mut self.config, &mut self.receptmapp, result);
                        if result.is_ok() {
                            self.refresh(log);
                            self.scen = Scen::default();
                        }
                    },
                    _ => (),
                }
            },
            _ => (),
        }
        if reset {
            self.scen = Scen::default();
            self.kokbok.reset_filter();
        }
    }
    
    fn is_editing_text(&self) -> bool {
        matches!(self.scen, Scen::SkapaRecept { .. } | Scen::ReceptBläddrare { editor: Some( .. )})
    }
}

fn spara_recept(string: String, receptmapp: &PathBuf, källa: &Option<PathBuf>) -> Result<PathBuf, Error> {
    let recept = Recept::try_parse(string)?;
    let rubrik = recept.rubrik();
    let namn = if rubrik != "" {
        rubrik
    } else {
        "nytt recept"
    };
    let path = if let Some(source_path) = källa {
        source_path.clone()
    } else {
        let mut path_candidate = receptmapp.join(namn).with_extension("txt");
        let mut i = 0;
        while path_candidate.exists() {
            path_candidate = receptmapp.join(format!("{namn}.{i:#03}")).with_extension("txt");
            i += 1;
        }
        path_candidate
    };
    recept.spara(&path).map(|_| path)
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