use unicode_segmentation::UnicodeSegmentation;

use crate::book::{Recept, Storhet, Värde, ingrediens, ingrediensrubrik};

const UNDERRUBRIK: &str = "/";
const INGREDIENSMARKÖR: &str = "ingrediens";
const MÅTTENHETER: &[&str] = &[
    "sats",
    "burkar",
    "bit",
    "bitar",
    "dl",
    "msk",
    "st",
    "l",
    "g",
    "kg",
    "tsk",
    "krm",
    "portion",
    "portioner",
    "°C",
    "°F",
    "°",
];
pub(crate) const TEMPERATURENHETER: &[&str] = &[
    "°C",
    "°F",
    "°",
];

const CIRKAMARKÖR: &[&str] = &[
    "ungefär",
    "cirka",
    "ca.",
    "ca",
    "Ungefär",
    "Cirka",
    "Ca.",
    "Ca",
];
const STEGMARKÖR: &[&str] = &[
    "gör så här",
    "steg",
    "instruktioner",
    "tillagning",
];

fn clean_numbers(s: &str) -> String {
    s.trim()
        .replace(",", ".")
        .replace("½", "0.5")
        .replace("⅓", "0.333333")
        .replace("⅔", "0.666667")
        .replace("¼", "0.25")
        .replace("¾", "0.75")
        .replace("⅕", "0.2")
        .replace("⅖", "0.4")
        .replace("⅗", "0.6")
        .replace("⅘", "0.8")
        .replace("⅙", "0.166667")
        .replace("⅚", "0.833333")
        .replace("⅐", "0.142857")
        .replace("⅛", "0.125")
        .replace("⅜", "0.375")
        .replace("⅝", "0.625")
        .replace("⅞", "0.875")
        .replace("⅑", "0.111111")
        .replace("⅒", "0.1")

        .replace("1/2", "0.5")
        .replace("1/3", "0.333333")
        .replace("2/3", "0.666667")
        .replace("1/4", "0.25")
        .replace("3/4", "0.75")
        .replace("1/5", "0.2")
        .replace("2/5", "0.4")
        .replace("3/5", "0.6")
        .replace("4/5", "0.8")
        .replace("1/6", "0.166667")
        .replace("5/6", "0.833333")
        .replace("1/7", "0.142857")
        .replace("1/8", "0.125")
        .replace("3/8", "0.375")
        .replace("5/8", "0.625")
        .replace("7/8", "0.875")
        .replace("1/9", "0.111111")
        .replace("1/10", "0.1")
}

// ^\-? ?[0-9]*(\.0*[1-9]+[0-9]*)?

fn is_approx(s: &str) -> Option<&str> {
    for cirka in CIRKAMARKÖR {
        if let Some(stripped) = s.strip_prefix(cirka) {
            return Some(stripped);
        }
    }
    None
}

pub(super) fn parse_storhet(s: &str) -> Option<(Storhet, String)> {
    let mut parsed = Storhet::default();
    let mut string = s.trim().to_string();

    if let Some(stripped) = is_approx(&string) {
        string = stripped.trim().to_string();
        parsed.cirka = true;
    }
    let mut sign = 1;
    if let Some(stripped) = string.strip_prefix("-") {
        string = stripped.to_string();
        sign = -1;
    }

    let remainder = if let Some(hyph_i) = string.strip_prefix("-").unwrap_or(&string).find("-") {
        let (pre, post) = string.split_at(hyph_i);
        let pre_num = parse_number(pre)?;
        let (post_num, remainder) = parse_number_remainder(post.strip_prefix("-").unwrap())?;
        parsed.värde = Värde::Intervall(pre_num * sign as f32..post_num);
        remainder
    } else {
        let (num, remainder) = parse_number_remainder(&string)?;
        parsed.värde = Värde::Skalär(num * sign as f32);
        remainder
    };

    if let Some((enhet, remainder)) = strip_enhet(&remainder) {
        parsed.enhet = enhet.to_string();
        Some((parsed, remainder.to_string()))
    } else {
        Some((parsed, remainder))
    }
}

fn strip_enhet(remainder: &str) -> Option<(&str, &str)> {
    let exact = remainder.split_whitespace().next()?;
    let mut candidate = None;
    for enhet in MÅTTENHETER {
        if *enhet == exact {
            // Exact match, return
            return Some((enhet, remainder.strip_prefix(enhet).unwrap().trim()));
        }
        if let Some(stripped) = remainder.strip_prefix(enhet) {
            candidate = Some((*enhet, stripped.trim()));
        }
    }
    candidate
}

/// Parse a string containing only a number
fn parse_number(s: &str) -> Option<f32> {
    let cleaned = clean_numbers(s);
    let mut parts = cleaned.split_whitespace();
    let n_parts = parts.clone().count();
    if n_parts == 1 {
        parts.next().unwrap().parse().ok()
    } else if n_parts == 2 {
        let i_part = parts.next()?.parse::<u32>().ok()?;
        let f_part = parts.next()?.parse::<f32>().ok()?;
        Some(i_part as f32 + f_part)
    } else {
        None
    }
}

/// Parse a string containing a number and possibly other text after it. Only looks for numbers in the first 30 characters.
fn parse_number_remainder(s: &str) -> Option<(f32, String)> {
    let graphemes = s.graphemes(true).collect::<Vec<&str>>();
    for search_n in (1..=graphemes.len().min(30)).rev() {
        let search_string = graphemes[0..search_n].join("");
        if let Some(parsed) = parse_number(&search_string) {
            return Some((parsed, graphemes[search_n..].join("").trim().to_string()));
        }
    }
    None
}

#[derive(Debug, Clone, Copy)]
pub enum ParseError {
    TomtDokument,
    SaknarIngredienser,
    SaknarSteg,
}

#[derive(Debug)]
enum ParseStage {
    Info,
    Ingredienser,
    Steg,
}

pub fn recipe<S: Into<String>>(source: S) -> Result<Recept, ParseError> {
    let mut stage = ParseStage::Info;
    let mut recept = Recept::default();
    let source = source.into();
    let mut lines = source.trim().lines();
    recept.namn = lines.next().ok_or(ParseError::TomtDokument)?.to_string();

    for line in lines {
        match stage {
            ParseStage::Info => {
                if let Some(ugnstemp) = parse_as_temperature(line) {
                    recept.ugnstemperatur = Some(ugnstemp);
                } else if let Some(tillagningstid) = parse_as_time(line) {
                    recept.tillagningstid = Some(tillagningstid);
                } else if let Some(källa) = parse_as_källa(line) {
                    recept.källa = Some(källa);
                } else if let Some((storlek, _)) = parse_storhet(line) {
                    recept.storlek = Some(storlek);
                } else if ingredients_start(line) {
                    stage = ParseStage::Ingredienser;
                }
            },
            ParseStage::Ingredienser => {
                if instructions_start(line) {
                    stage = ParseStage::Steg;
                    continue;
                }
                if line.trim() == "" {
                    continue;
                }
                if let Some(namn) = line.trim().strip_prefix(UNDERRUBRIK) {
                    recept.ingredienser.push(ingrediensrubrik(namn.to_string()));
                } else if let Some((storhet, namn)) = parse_storhet(line) {
                    recept.ingredienser.push(ingrediens(Some(storhet), namn.trim()));
                } else {
                    recept.ingredienser.push(ingrediens(None, line.trim()));
                }
            },
            ParseStage::Steg => {
                if line.trim() == "" {
                    continue;
                }
                recept.steg.push(line.trim().to_string())
            },
        }
    }
    match stage {
        ParseStage::Info => Err(ParseError::SaknarIngredienser),
        ParseStage::Ingredienser => Err(ParseError::SaknarSteg),
        ParseStage::Steg => Ok(recept),
    }
}

fn parse_as_källa(line: &str) -> Option<String> {
    let trimmed = line.trim();
    trimmed.strip_prefix("(")?.strip_suffix(")").map(|s|s.to_string())
}

fn parse_as_time(line: &str) -> Option<Storhet> {
    if line.to_lowercase().contains("tid") {
        return Some(parse_storhet(line)?.0);
    }
    None
}

fn parse_as_temperature(line: &str) -> Option<Storhet> {
    let part = line.split_whitespace().find(|part| part.contains("°"))?;
    let mut parts = part.split("°");
    let storhet = parse_storhet(parts.next()?)?.0;
    let unit = parts.next().unwrap_or("C");
    Some(storhet.med_enhet(format!("°{unit}")))
}

fn ingredients_start(line: &str) -> bool {
    line.to_lowercase().contains(INGREDIENSMARKÖR)
}

fn instructions_start(line: &str) -> bool {
    STEGMARKÖR.iter().any(|markör|line.to_lowercase().contains(markör))
}

#[cfg(test)]
mod tests {
    use crate::book::{Storhet, parse::parse_storhet};

    #[test]
    fn parse_storheter() {
        assert_eq!(parse_storhet("1 dl"), Some((Storhet::värde(1.0).med_enhet("dl"), "".to_string())));
        assert_eq!(parse_storhet("2dl"), Some((Storhet::värde(2.0).med_enhet("dl"), "".to_string())));
        assert_eq!(parse_storhet("200°C"), Some((Storhet::värde(200.0).med_enhet("°C"), "".to_string())));
        assert_eq!(parse_storhet("2 1/2 dl"), Some((Storhet::värde(2.5).med_enhet("dl"), "".to_string())));
        assert_eq!(parse_storhet("1-2 1/2 dl"), Some((Storhet::intervall(1.0..2.5).med_enhet("dl"), "".to_string())));
        assert_eq!(parse_storhet("1,5 msk"), Some((Storhet::värde(1.5).med_enhet("msk"), "".to_string())));
        assert_eq!(parse_storhet("10 bitar"), Some((Storhet::värde(10.0).med_enhet("bitar"), "".to_string())));
        assert_eq!(parse_storhet("2.25 l"), Some((Storhet::värde(2.25).med_enhet("l"), "".to_string())));
        assert_eq!(parse_storhet("300-400 g surströmming"), Some((Storhet::intervall(300.0..400.0).med_enhet("g"), "surströmming".to_string())));
        assert_eq!(parse_storhet("300 - 400g surströmming"), Some((Storhet::intervall(300.0..400.0).med_enhet("g"), "surströmming".to_string())));
        assert_eq!(parse_storhet("100"), Some((Storhet::värde(100.0), "".to_string())));
        assert_eq!(parse_storhet(" 100"), Some((Storhet::värde(100.0), "".to_string())));
        assert_eq!(parse_storhet("⅔ ägg"), Some((Storhet::värde(0.6666), "ägg".to_string())));
    }
}