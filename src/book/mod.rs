use std::{fmt::Display, ops::Range};

use crate::ui::{Side, TextBuffer, TextLayout};

mod parse;

#[derive(Default)]
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
    pub fn try_parse<S: Into<String>>(source: S) -> Option<Self> {
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
    
    pub fn render(&self, rect: crate::ui::Rect, buf: &mut impl TextBuffer) -> std::io::Result<()> {
        let mut rect = rect;

        let extra_info = [
            self.storlek().map(|storlek| storlek.to_string()),
            self.ugnstemperatur().map(|temp| format!("Ugnstemperatur {temp}")),
            self.tillagningstid().map(|tid| format!("Tillagningstid {tid}"))
        ].into_iter().flatten().collect::<Vec<String>>().join(". ");

        TextLayout::new()
            .with_header(self.rubrik())
            .with_space(1)
            .with_divider()
            .with_single_line(&extra_info)
            .with_divider()
            .with_space(1)
            .render_cut(&mut rect, buf)?;

        let ingredients = TextLayout::new()
            .with_line_break_indentation()
            .with_items(self.ingredienser(), 0)
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
                .with_section_indentation()
                .with_items(self.steg(), 0);
            ingredients.render_cut(&mut rect, buf)?;
            steps.render(rect, buf)?;
        }
        Ok(())
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct Storhet {
    värde: Värde,
    enhet: String,
    cirka: bool,
}

impl Display for Storhet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(format!("{} {} {}",
            self.cirka.then(|| "ca").unwrap_or_default(),
            self.värde,
            self.enhet
        ).trim())
    }
}

#[derive(Debug, Clone)]
enum Värde {
    Skalär(f32),
    Intervall(Range<f32>),
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
        .or_else(|| replace_fraction(dec, 1, 5))
        .or_else(|| replace_fraction(dec, 2, 5))
        .or_else(|| replace_fraction(dec, 3, 5))
        .or_else(|| replace_fraction(dec, 4, 5))
        .or_else(|| replace_fraction(dec, 1, 6))
        .or_else(|| replace_fraction(dec, 5, 6))
        .or_else(|| replace_fraction(dec, 1, 7))
        .or_else(|| replace_fraction(dec, 1, 8))
        .or_else(|| replace_fraction(dec, 3, 8))
        .or_else(|| replace_fraction(dec, 5, 8))
        .or_else(|| replace_fraction(dec, 7, 8))
        .or_else(|| replace_fraction(dec, 1, 9))
        .or_else(|| replace_fraction(dec, 1, 10)) 
    {
        let int = (v.abs().floor() * v.signum()) as i32;
        if int != 0 {
            format!("{int} {frac}")
        } else {
            frac
        }
    } else {
        let decimals = format!("{v:.4}");
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

    pub fn med_enhet<T: Into<String>>(self, enhet: T) -> Self {
        Self { enhet: enhet.into(), ..self }
    }
}


#[derive(Debug, PartialEq)]
pub enum IngrediensPost {
    Ingrediens(Option<Storhet>, String),
    Underrubrik(String),
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

    #[test]
    fn format_värde() {
        for a in [
            "1",
            "2",
            "1 1/2",
            "10 1/3",
            "9 2/3",
            "-2 1/7",
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
    }
}