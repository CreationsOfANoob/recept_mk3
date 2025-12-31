use core::f32;
use std::{fmt::Display, ops::{Add, Mul, Range}, path::Path};

use crate::{book::parse::{MåttEnhet, TEMPERATURENHETER}, ui::{Side, TextBuffer, TextLayout}};
mod parse;
pub use parse::ParseError;
use crate::book::parse::MÅTTENHETER;

#[derive(Debug, Default, PartialEq)]
pub struct Recept {
    namn: String,
    källa: Option<String>,
    ugnstemperatur: Option<Storhet>,
    tillagningstid: Option<Storhet>,
    storlek: Option<Storhet>,
    ingredienser: Vec<IngrediensPost>,
    steg: Vec<String>,
}

impl Recept {
    pub fn try_parse_file(path: &Path) -> Result<Result<Self, ParseError>, crate::Error> {
        use std::io::Read;
        let mut buf = String::new();
        std::fs::File::open(path)?.read_to_string(&mut buf)?;
        Ok(Self::try_parse(buf))
    }

    pub fn try_parse<S: Into<String>>(source: S) -> Result<Self, ParseError> {
        parse::recipe(source)
    }

    pub fn rubrik(&self) -> &str {
        &self.namn
    }

    pub fn källa(&self) -> Option<&str> {
        self.källa.as_deref()
    }

    pub fn ugnstemperatur(&self) -> Option<&Storhet> {
        self.ugnstemperatur.as_ref()
    }

    pub fn tillagningstid(&self) -> Option<&Storhet> {
        self.tillagningstid.as_ref()
    }

    pub fn storlek(&self) -> Option<&Storhet> {
        self.storlek.as_ref()
    }

    pub fn ingredienser(&self) -> &[IngrediensPost] {
        &self.ingredienser
    }

    pub fn steg(&self) -> &[String] {
        &self.steg
    }
    
    pub fn render(&self, rect: crate::ui::Rect, buf: &mut impl TextBuffer, storlek: &Option<Storhet>) -> std::io::Result<()> {
        let fac = if let Some(ny_storlek) = storlek && let Some(old_storlek) = self.storlek() {
            ny_storlek.värde.average() / old_storlek.värde.average()
        } else {
            1.0
        };

        let mut rect = rect;

        let storlek_string = if let Some(storlek) = storlek.as_ref() && let Some(old_storlek) = self.storlek() && storlek != old_storlek {
            Some(format!("{storlek} !*"))
        } else if let Some(storlek) = self.storlek() {
            Some(format!("{storlek}"))
        } else {
            None
        };
        let extra_info = [
            storlek_string,
            self.tillagningstid().map(|tid| format!("Tillagningstid {tid}"))
        ].into_iter().flatten().collect::<Vec<String>>().join(". ");

        let mut rubrikavsnitt = TextLayout::new()
            .with_header(self.rubrik())
            .with_space(1)
            .with_divider();

        if extra_info != "" {
            rubrikavsnitt = rubrikavsnitt
                .with_single_line(&extra_info)
                .with_divider();
        }
        rubrikavsnitt.with_space(1).render_cut(&mut rect, buf)?;

        let mut ingredients = self.ingredienser.clone();
        for ip in &mut ingredients {
            ip.scale(fac, true);
        }
        let ingredients = TextLayout::new()
            .indent_body()
            .with_items(&ingredients, 0)
            .with_space(2);

        if rect.w > 50 {
            let (mut ingredients_rect, steps_rect) = rect.split(Side::Left, 25).unwrap();
            ingredients_rect.cut_mut(Side::Right, 2);
            let steps = TextLayout::new()
                .with_items(self.steg(), 1);
            ingredients.render(ingredients_rect, buf)?;
            steps.render(steps_rect, buf)?;
        } else {
            let steps = TextLayout::new()
                .indent_first_line()
                .with_items(self.steg(), 0);
            ingredients.render_cut(&mut rect, buf)?;
            steps.render(rect, buf)?;
        }
        Ok(())
    }
    
    pub fn matches(&self, filter: &str) -> bool {
        if filter == "" {
            return true;
        }
        let filter = filter.to_lowercase();
        let parts = filter.split_whitespace();
        // Kolla först om rubriken matchar alla delar i söksträngen
        if parts.clone().all(|part| self.rubrik().to_lowercase().contains(part)) {
            // Isåfall har vi en träff
            return true;
        }
        // Annars, kolla ingredienserna
        for part in parts {
            if self.ingredienser().iter().all(|ing| !ing.namn().to_lowercase().contains(part)) {
                // Ingen ingrediens matchar den här delen i söksträngen
                return false;
            }
        }
        // Alla delar av söksträngen matchade åtminstone en ingrediens.
        true
    }
    
    pub fn spara<P: AsRef<Path>>(&self, path: P) -> Result<(), crate::Error> {
        Ok(std::fs::write(path, self.to_string())?)
    }
}

impl Display for Recept {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.rubrik())?;
        if let Some(källa) = self.källa() {
            writeln!(f, "({})", källa)?;
        }
        if let Some(ugntemp) = self.ugnstemperatur() {
            writeln!(f, "Ugnstemperatur: {ugntemp}")?;
        }
        if let Some(tid) = self.tillagningstid() {
            writeln!(f, "Tillagningstid: {tid}")?;
        }
        if let Some(storlek) = self.storlek() {
            writeln!(f, "{storlek}")?;
        }
        writeln!(f)?;
        writeln!(f, "Ingredienser:")?;
        for ingrediens in self.ingredienser() {
            match ingrediens {
                IngrediensPost::Ingrediens(storhet, namn) => if let Some(storhet) = storhet {
                    writeln!(f, "{storhet} {namn}")?
                } else {
                    writeln!(f, "{namn}")?
                }
                IngrediensPost::Underrubrik(text) => writeln!(f, "/{text}")?,
            }
            
        }
        writeln!(f)?;
        writeln!(f, "Gör så här:")?;
        for steg in self.steg() {
            writeln!(f, "{steg}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct Storhet {
    pub värde: Värde,
    enhet: MåttEnhet,
    cirka: bool,
}

impl Display for Storhet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if TEMPERATURENHETER.contains(&self.enhet.as_str()) {
            f.write_str(format!("{} {}{}",
                self.cirka.then(|| "ca").unwrap_or_default(),
                self.värde,
                self.enhet.as_str()).trim())
        } else {
            let pluralsträng = if self.värde.is_plural() {
                self.enhet.pluralform.unwrap_or(self.enhet.as_str())
            } else {
                self.enhet.as_str()
            };
            f.write_str(format!("{} {} {}",
                self.cirka.then(|| "ca").unwrap_or_default(),
                self.värde,
                pluralsträng).trim())
        }
    }
}

#[derive(Debug, Clone)]
pub enum Värde {
    Skalär(f32),
    Intervall(Range<f32>),
}
impl Värde {
    fn average(&self) -> f32 {
        match self {
            Värde::Skalär(v) => *v,
            Värde::Intervall(range) => (range.start + range.end) * 0.5,
        }
    }

    pub fn try_parse(s: &str) -> Option<Self> {
        Some(parse::parse_storhet(s)?.0.värde)
    }

    fn scale(&mut self, scale: &f32) {
        match self {
            Värde::Skalär(v) => *v *= scale,
            Värde::Intervall(range) => {
                range.start * range.start * scale..range.end * scale;
            }
        }
    }
    
    fn is_plural(&self) -> bool {
        match self {
            Värde::Skalär(v) => v.abs() >= 2.0,
            Värde::Intervall(range) => range.start.abs() >= 2.0 || range.end.abs() >= 2.0,
        }
    }
}

impl Display for Värde {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Värde::Skalär(v) => f.write_str(&format_number(*v)),
            Värde::Intervall(range) => f.write_str(&format!("{}-{}", 
                format_number(range.start),
                format_number(range.end)
            )),
        }
    }
}

fn replace_fraction(v: f32, n: u8, d: u8) -> Option<String> {
    let f = n as f32 / d as f32;
    if (f - v).abs() < 0.0001 {
        return Some(format!("{n}/{d}"));
    }
    None
}

fn format_number(v: f32) -> String {
    let dec = v.abs().rem_euclid(1.0);
    if let Some(frac) = replace_fraction(dec, 1, 2)
        .or_else(|| replace_fraction(dec, 1, 3))
        .or_else(|| replace_fraction(dec, 2, 3))
        .or_else(|| replace_fraction(dec, 1, 4))
        .or_else(|| replace_fraction(dec, 3, 4))
        // .or_else(|| replace_fraction(dec, 1, 5))
        // .or_else(|| replace_fraction(dec, 2, 5))
        // .or_else(|| replace_fraction(dec, 3, 5))
        // .or_else(|| replace_fraction(dec, 4, 5))
        // .or_else(|| replace_fraction(dec, 1, 6))
        // .or_else(|| replace_fraction(dec, 5, 6))
        // .or_else(|| replace_fraction(dec, 1, 7))
        // .or_else(|| replace_fraction(dec, 1, 8))
        // .or_else(|| replace_fraction(dec, 3, 8))
        // .or_else(|| replace_fraction(dec, 5, 8))
        // .or_else(|| replace_fraction(dec, 7, 8))
        // .or_else(|| replace_fraction(dec, 1, 9))
        // .or_else(|| replace_fraction(dec, 1, 10)) 
    {
        let int = (v.abs().floor() * v.signum()) as i32;
        if int != 0 {
            format!("{int} {frac}")
        } else {
            frac
        }
    } else {
        let decimals = format!("{v:.1}");
        let no_trailing_zeros = decimals.trim_end_matches("0");
        let clean = if let Some(no_trailing_dot) = 
        no_trailing_zeros.strip_suffix(".") {
            no_trailing_dot
        } else {
            no_trailing_zeros
        };
        clean.replace(".", ",")
    }
}

impl PartialEq for Värde {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Skalär(l0), Self::Skalär(r0)) => (l0 - r0).abs() < 0.0001,
            (Self::Intervall(l0), Self::Intervall(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl Default for Värde {
    fn default() -> Self {
        Self::Skalär(0.0)
    }
}

impl Mul<f32> for Värde {
    type Output = Self;

    fn mul(self, rhs: f32) -> Self {
        match self {
            Värde::Skalär(v) => Värde::Skalär(v * rhs),
            Värde::Intervall(range) => Värde::Intervall(range.start * rhs..range.end * rhs)
        }
    }
}

impl Add<f32> for Värde {
    type Output = Self;

    fn add(self, rhs: f32) -> Self {
        match self {
            Värde::Skalär(v) => Värde::Skalär(v + rhs),
            Värde::Intervall(range) => Värde::Intervall(range.start + rhs..range.end + rhs),
        }
    }
}

impl Storhet {
    pub fn värde(värde: f32) -> Self {
        Self {
            värde: Värde::Skalär(värde),
            ..Default::default()
        }
    }

    pub fn cirka_värde(värde: f32) -> Self {
        Self {
            värde: Värde::Skalär(värde),
            cirka: true,
            ..Default::default()
        }
    }

    pub fn intervall(värde: Range<f32>) -> Self {
        Self {
            värde: Värde::Intervall(värde),
            ..Default::default()
        }
    }

    pub fn cirka_intervall(värde: Range<f32>) -> Self {
        Self {
            värde: Värde::Intervall(värde),
            cirka: true,
            ..Default::default()
        }
    }

    pub fn med_enhet(self, enhet: &str) -> Self {
        let mut hittad_enhet = None;
        for måttenhet in MÅTTENHETER {
            if enhet == måttenhet.as_str() {
                hittad_enhet = Some(måttenhet);
                break;
            } else if let Some(plural) = måttenhet.pluralform && plural == enhet {
                hittad_enhet = Some(måttenhet);
                break;
            }
        }
        Self { enhet: *hittad_enhet.unwrap_or(&MåttEnhet::ENHETSLÖS), ..self }
    }

    #[must_use]
    fn skala(&self, skala: f32, optimal: bool) -> Self {
        let mut new = self.clone();
        new.värde.scale(&skala);
        if optimal {
            new.find_optimal_unit();
        }
        new
    }

    fn convert(self, enhet: MåttEnhet) -> Option<Self> {
        if enhet.dimension != self.enhet.dimension {
            return None;
        }
        let to_base = self.värde.add(self.enhet.noll).mul(self.enhet.faktor);
        let to_new = to_base.mul(1.0 / enhet.faktor).add(-enhet.noll);
        Some(Self { värde: to_new, enhet, ..self })
    }
    
    fn find_optimal_unit(&mut self) {
        let mut best_cand = self.clone();
        let mut best_cand_d = self.värde.average().log10();
        if best_cand_d < 0.0 {
            best_cand_d = f32::MAX;
        }
        for cand in MÅTTENHETER {
            if cand.dimension != self.enhet.dimension {
                continue;
            }
            let Some(converted) = self.clone().convert(*cand) else {
                continue;
            };
            let cand_d = converted.värde.average().log10();
            println!("source: {self}, cand: {cand:?}, converted: {converted}, best_fit: {best_cand_d}, this: {cand_d}");
            if cand_d < best_cand_d && cand_d >= 0.0 {
                best_cand_d = cand_d;
                best_cand = converted;
            }
        }
        *self = best_cand;
    }
}


#[derive(Debug, PartialEq, Clone)]
pub enum IngrediensPost {
    Ingrediens(Option<Storhet>, String),
    Underrubrik(String),
}

impl IngrediensPost {
    fn namn(&self) -> &str {
        match self {
            IngrediensPost::Ingrediens(_, namn) => namn,
            IngrediensPost::Underrubrik(namn) => namn,
        }
    }
    
    fn scale(&mut self, fac: f32, convert_unit: bool) {
        if let IngrediensPost::Ingrediens(Some(storhet), _) = self {
            *storhet = storhet.skala(fac, convert_unit);
        }
    }
}

pub fn ingrediens<S: Into<String>>(storhet: Option<Storhet>, namn: S) -> IngrediensPost {
    IngrediensPost::Ingrediens(storhet, namn.into())
}

pub fn ingrediensrubrik<S: Into<String>>(text: S) -> IngrediensPost {
    IngrediensPost::Underrubrik(text.into())
}

impl Display for IngrediensPost {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IngrediensPost::Ingrediens(storhet, namn) => f.write_str(format!("{} {}", storhet.clone().map_or(String::new(), |storhet| storhet.to_string()), namn).trim()),
            IngrediensPost::Underrubrik(text) => f.write_str(&format!("\n{text}")),
        }
    }
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use crate::book::{Recept, Storhet, ingrediens, ingrediensrubrik, parse::parse_storhet};

    fn two_way_convert(s: &str) -> Option<String> {
        let tolkat = parse_storhet(s)?.0.värde;
        Some(format!("{tolkat}"))
    }

    fn storhet(s: &str) -> Storhet {
        parse_storhet(s).unwrap().0
    }

    #[test]
    fn skala_storhet() {
        assert_eq!(storhet("10 dl").skala(10.0, true), storhet("10 l"));
        assert_eq!(storhet("15 dl").skala(0.001, true), storhet("1,5 ml"));
        assert_eq!(storhet("15 dl").skala(0.01, true), storhet("1 msk"));
        assert_eq!(storhet("1 kg").skala(0.5, true), storhet("500 g"));
    }

    #[test]
    fn format_värde() {
        for a in [
            "1",
            "2",
            "1 1/2",
            "10 1/3",
            "9 2/3",
            "-2,1",
        ] {
            assert_eq!(two_way_convert(a), Some(a.to_string()));
        }
    }

    #[test] 
    fn enkelt_recept() {
        let recept = Recept::try_parse("
            Spaghetti carbonara
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
            Mal över rikligt med peppar, strö över riven ost och blanda alltsammans på tallriken.
        ").unwrap();
        assert_eq!(recept.rubrik(), "Spaghetti carbonara");
        assert_eq!(recept.källa(), Some("Vår Kokbok"));
        assert_eq!(recept.ugnstemperatur(), None);
        assert_eq!(recept.tillagningstid(), None);
        assert_eq!(recept.storlek(), Some(&Storhet::värde(1.0).med_enhet("portion")));
        assert_eq!(recept.ingredienser(), &[
            ingrediens(Some(Storhet::cirka_värde(70.0).med_enhet("g")), "spaghetti"),
            ingrediens(Some(Storhet::värde(50.0).med_enhet("g")), "rökt sidfläsk, skinka, bog eller bacon"),
            ingrediens(Some(Storhet::värde(1.0)), "äggula"),
            ingrediens(None, "nymalen svartpeppar"),
            ingrediens(Some(Storhet::intervall(0.75..1.0).med_enhet("dl")), "riven ost")
        ]);
        assert_eq!(recept.steg(), vec![
            "Koka spaghettin.",
            "Strimla det rökta köttet och stek det knaprigt i en stekpanna.",
            "Lägg den heta nykokta spaghettin i en varm djup tallrik, strö över köttet. Lägg äggulan i en fördjupning i mitten.",
            "Mal över rikligt med peppar, strö över riven ost och blanda alltsammans på tallriken.",
        ]);
        assert_eq!(Recept::try_parse(recept.to_string()).unwrap(), recept);
    }

    #[test]
    fn kladdkaka() {
        let recept = Recept::try_parse("Kladdkaka

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

            Sikta florsocker över om så önskas.").unwrap();
        assert_eq!(recept.rubrik(), "Kladdkaka");
        assert_eq!(recept.källa(), None);
        assert_eq!(recept.ugnstemperatur(), Some(&Storhet::värde(200.0).med_enhet("°C")));
        assert_eq!(recept.tillagningstid(), None);
        assert_eq!(recept.storlek(), Some(&Storhet::cirka_värde(8.0).med_enhet("bitar")));
        assert_eq!(recept.ingredienser(), &[
            ingrediens(None, "smör till formen"),
            ingrediens(None, "ströbröd till formen"),
            ingrediens(Some(Storhet::värde(100.0).med_enhet("g")), "smör"),
            ingrediens(Some(Storhet::värde(2.0).med_enhet("st")), "ägg"),
            ingrediens(Some(Storhet::värde(2.5).med_enhet("dl")), "strösocker"),
            ingrediens(Some(Storhet::värde(3.0).med_enhet("msk")), "kakao"),
            ingrediens(Some(Storhet::värde(2.0).med_enhet("tsk")), "vaniljsocker"),
            ingrediens(Some(Storhet::värde(1.5).med_enhet("dl")), "vetemjöl"),
            ingrediens(Some(Storhet::värde(1.0).med_enhet("krm")), "salt"),
            ingrediensrubrik("Till servering"),
            ingrediens(None, "florsocker till garnering"),
            ingrediens(Some(Storhet::värde(2.0).med_enhet("dl")), "vispgrädde"),
            ingrediens(None, "färska bär"),
        ]);
        assert_eq!(recept.steg(), vec![
            "Sätt ugnen på 200°C, under- och övervärme.",
            "Smörj och bröa en form med löstagbar kant, ca 24 cm i diameter (för 8 bitar) eller spänn fast ett bakplåtspapper på botten.",
            "Smält smöret i kastrull, låt svalna något.",
            "Vispa ihop ägg och socker (använd inte en elvisp).",
            "Blanda kakao, vaniljsocker, vetemjöl och salt i en bunke och rör ner i äggblandningen.",
            "Tillsätt det smälta smöret och blanda försiktigt ihop till en jämn smet och häll sedan över smeten i formen.",
            "Grädda mitt i ugnen i ca 10-15 minuter (öka eller minska tiden vid behov beroende på hur kladdig du vill ha den).",
            "Ta ut kladdkakan och låt svalna.",
            "Sikta florsocker över om så önskas.",
        ]);
        assert_eq!(Recept::try_parse(recept.to_string()).unwrap(), recept);
    }
}