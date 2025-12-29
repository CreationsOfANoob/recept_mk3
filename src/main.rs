use std::{fmt::Display, io};

use crossterm::event::KeyCode;
use recept::{App, CommandList, book::Recept, ui::{Border, BorderStyle, ListLayout, Rect, Side, TextBuffer, TextLayout}};

fn main() -> io::Result<()> {
    let app = ReceptApp::new();
    match recept::run(app) {
        Ok(_) => (),
        Err(err) => println!("{err:?}"),
    }
    Ok(())
}

const DEFAULT_RECEPT: &str = "Exempelrubrik

Ingredienser:
1 dl lorem
100 g ipsum

Gör så här:
Dolor sit amet.
Consectetur adipiscing elit. 
Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.";

const TESTRECEPT: &[&str] = &[
    "Kladdkaka

            Ca 8 bitar
            Ugnstemperatur 200°C

            Ingredienser

            smör till formen 
            ströbröd till formen
            100 g smör
            2 st ägg
            2 1/2 dl strösocker
            3 msk kakao
            2 tsk vaniljsocker
            1 1/2 dl vetemjöl
            1 krm salt

            /Till servering
            florsocker till garnering
            2 dl vispgrädde 
            färska bär


            Gör så här

            Sätt ugnen på 200°C, under- och övervärme.

            Smörj och bröa en form med löstagbar kant, ca 24 cm i diameter (för 8 bitar) eller spänn fast ett bakplåtspapper på botten.

            Smält smöret i kastrull, låt svalna något.

            Vispa ihop ägg och socker (använd inte en elvisp).

            Blanda kakao, vaniljsocker, vetemjöl och salt i en bunke och rör ner i äggblandningen.

            Tillsätt det smälta smöret och blanda försiktigt ihop till en jämn smet och häll sedan över smeten i formen.

            Grädda mitt i ugnen i ca 10-15 minuter (öka eller minska tiden vid behov beroende på hur kladdig du vill ha den).

            Ta ut kladdkakan och låt svalna.

            Sikta florsocker över om så önskas.",
    "Spaghetti carbonara
    (Vår Kokbok)

    1 portion

    Ingredienser:
    ca 70 g spaghetti
    50 g rökt sidfläsk, skinka, bog eller bacon
    1 äggula
    nymalen svartpeppar
    ¾-1 dl riven ost

    Gör så här:
    Koka spaghettin.
    Strimla det rökta köttet och stek det knaprigt i en stekpanna.
    Lägg den heta nykokta spaghettin i en varm djup tallrik, strö över köttet. Lägg äggulan i en fördjupning i mitten.
    Mal över rikligt med peppar, strö över riven ost och blanda alltsammans på tallriken."
    ,
    "Spaghetti carbonara med ännu längre titel som inte får plats
    (Vår Kokbok)

    1 portion

    Ingredienser:
    ca 70 g spaghetti
    50 g rökt sidfläsk, skinka, bog eller bacon
    1 äggula
    nymalen svartpeppar
    ¾-1 dl riven ost

    Gör så här:
    Koka spaghettin.
    Strimla det rökta köttet och stek det knaprigt i en stekpanna.
    Lägg den heta nykokta spaghettin i en varm djup tallrik, strö över köttet. Lägg äggulan i en fördjupning i mitten.
    Mal över rikligt med peppar, strö över riven ost och blanda alltsammans på tallriken."
];

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
    VerkställSök,
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
            Command::VerkställSök => write!(f, "Sök"),
        }
    }
}

enum UserMode {
    Locked,
    Normal,
    SkapaRecept(String),
    Sök(String),
}

struct ReceptApp {
    recept: Vec<Recept>,
    listval: usize,
    flikval: usize,
    flikar: Vec<usize>,
    mode: UserMode,
}

impl ReceptApp {
    pub fn new() -> Self {
        let mut recept = Vec::new();
        for source in TESTRECEPT {
            let Some(tolkat) = Recept::try_parse(*source) else {
                continue;
            };
            recept.push(tolkat);
        }
        Self { recept, listval: 0, flikval: 0, flikar: Vec::new(), mode: UserMode::Normal }
    }
    
    fn draw_recipes<T: TextBuffer>(&self, rect: Rect, buf: &mut T) -> std::io::Result<()> {
        for (flik_i, (mut flik_rect, recept_i)) in rect.to_grid(self.flikar.len() as u16, 1, false).into_iter().zip(self.flikar.iter()).enumerate() {
            if self.flikval == flik_i && self.flikar.len() > 1 && matches!(self.mode, UserMode::Normal){
                Border::new(flik_rect, BorderStyle::THIN).render(buf)?;
            }
            flik_rect.limit_w(100, true);
            flik_rect.inset_mut(2, 2);
            if let Some(recept) = self.recept.get(*recept_i) {
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
        ListLayout::new().with_items(self.recept.iter().map(|recept| recept.rubrik().to_string())).with_selected(self.listval).render(rect, buf)
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
                .command(KeyCode::Enter, Command::VerkställSök),
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
                *flik = self.listval;
            } else if let Some(flik) = self.flikar.first_mut() {
                *flik = self.listval;
                self.flikval = 0;
            } else {
                self.flikar.push(self.listval);
            },
            Command::VisaINyFlik => if self.flikar.len() < 4 {
                self.flikar.push(self.listval);
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
            Command::VerkställSök => self.mode = UserMode::Normal,
        }
    }
}