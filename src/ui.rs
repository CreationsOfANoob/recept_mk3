use std::{fmt::{Debug, Display}, io::{Write, stdout}, mem::swap};

use crossterm::{QueueableCommand, cursor, style::{self}, terminal};
use unicode_segmentation::UnicodeSegmentation;

const SPACE_256: &str = "                                                                                                                                                                                                                                                                ";

pub struct BorderStyle {
    left: &'static str,
    right: &'static str,
    top_left: &'static str,
    top: &'static str,
    top_right: &'static str,
    bottom_left: &'static str,
    bottom: &'static str,
    bottom_right: &'static str,
}

impl Default for BorderStyle {
    fn default() -> Self {
        Self::THIN
    }
}

impl BorderStyle {
    pub const THIN: Self = BorderStyle {
        left: "│",
        right: "│",
        top_left: "┌",
        top: "─",
        top_right: "┐",
        bottom_left: "└",
        bottom: "─",
        bottom_right: "┘",
    };
    pub const LINE_LEFT: Self = BorderStyle {
        left: "│",
        right: " ",
        top_left: "│",
        top: " ",
        top_right: " ",
        bottom_left: "│",
        bottom: " ",
        bottom_right: " ",
    };
    pub const LINE_H: Self = BorderStyle {
        left: " ",
        right: " ",
        top_left: "─",
        top: "─",
        top_right: "─",
        bottom_left: "─",
        bottom: "─",
        bottom_right: "─",
    };
    pub const LIGHT_DOTTED_LINE_V: Self = BorderStyle {
        left: ".",
        right: ".",
        top_left: ".",
        top: " ",
        top_right: ".",
        bottom_left: ".",
        bottom: " ",
        bottom_right: ".",
    };
    pub const LIGHT_DOTTED: Self = BorderStyle {
        left: ".",
        right: ".",
        top_left: ".",
        top: ".",
        top_right: ".",
        bottom_left: ".",
        bottom: ".",
        bottom_right: ".",
    };
    pub const HEAVY_DOTTED_LINE_V: Self = BorderStyle {
        left: "*",
        right: "*",
        top_left: "*",
        top: " ",
        top_right: "*",
        bottom_left: "*",
        bottom: " ",
        bottom_right: "*",
    };
}

pub struct Border {
    rect: Rect,
    style: BorderStyle,
}

impl Border {
    pub fn new(rect: Rect, style: BorderStyle) -> Self {
        Self { rect, style }
    }
    
    pub fn render(&self, buf: &mut impl TextBuffer) -> std::io::Result<()> {
        
        let Rect { x: start_x, y: start_y, w, h } = self.rect;
        buf.write_line(start_x, start_y, self.style.top_left)?;
        buf.write_line(start_x + w - 1, start_y, self.style.top_right)?;
        buf.write_line(start_x, start_y + h - 1, self.style.bottom_left)?;
        buf.write_line(start_x + w - 1, start_y + h - 1, self.style.bottom_right)?;

        buf.write_line(start_x + 1, start_y, vec![self.style.top; w.saturating_sub(2) as usize].join(""))?;
        buf.write_line(start_x + 1, start_y + h - 1, vec![self.style.bottom; w.saturating_sub(2) as usize].join(""))?;

        for y in start_y + 1..start_y + h - 1 {
            buf.write_line(start_x, y, self.style.left)?;
            buf.write_line(start_x + w - 1, y, self.style.right)?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Rect {
    pub x: u16, 
    pub y: u16,
    pub w: u16,
    pub h: u16,
}

pub struct Size(u16, u16);

impl Into<Size> for (u16, u16) {
    fn into(self) -> Size {
        Size(self.0, self.1)
    }
}

/// Sides of a rectangle, assuming (0, 0) is in the upper left.
#[derive(Clone, Copy)]
pub enum Side {
    Left,
    Right,
    Top,
    Bottom,
}

impl Rect {
    pub fn new(x: u16, y: u16, w: u16, h: u16) -> Self {
        Self { x, y, w, h }
    }
    
    pub fn from_size<D: Into<Size>>(size: D) -> Rect {
        let Size(w, h) = size.into();
        Self::new(0, 0, w, h)
    }

    pub fn split(&self, side: Side, amount: u16) -> Option<(Rect, Rect)> {
        match side {
            Side::Left => Some((
                Rect::new(self.x, self.y, amount, self.h),
                Rect::new(self.x + amount, self.y, self.w.checked_sub(amount)?, self.h),
            )),
            Side::Right => Some((
                Rect::new((self.x + self.w).checked_sub(amount)?, self.y, amount, self.h),
                Rect::new(self.x, self.y, self.w.checked_sub(amount)?, self.h),
            )),
            Side::Top => Some((
                Rect::new(self.x, self.y, self.w, amount),
                Rect::new(self.x, self.y + amount, self.w, self.h.checked_sub(amount)?),
            )),
            Side::Bottom => Some((
                Rect::new(self.x, (self.y + self.h).checked_sub(amount)?, self.w, amount),
                Rect::new(self.x, self.y, self.w, self.h.checked_sub(amount)?),
            )),
        }
    }
    
    pub fn inset_mut(&mut self, w: u16, h: u16) {
        let inset_w = self.w.min(w * 2) / 2;
        let inset_h = self.h.min(h * 2) / 2;
        self.w -= inset_w * 2;
        self.h -= inset_h * 2;
        self.x += inset_w;
        self.y += inset_h;
    }

    #[must_use]
    pub fn cut(&self, side: Side, amount: u16) -> Rect {
        let mut new_old = *self;
        new_old.cut_mut(side, amount)
    }
    
    pub fn cut_mut(&mut self, side: Side, amount: u16) -> Rect {
        let (old, new) = match side {
            Side::Left => {
                let clamped = self.w.min(amount);
                (
                    rect(self.x + clamped, self.y, self.w - clamped, self.h),
                    rect(self.x, self.y, clamped, self.h),
                )
            },
            Side::Right => {
                let clamped = self.w.min(amount);
                (
                    rect(self.x, self.y, self.w - clamped, self.h),
                    rect(self.x + self.w - clamped, self.y, clamped, self.h),
                )
            },
            Side::Top => {
                let clamped = self.h.min(amount);
                (
                    rect(self.x , self.y + clamped, self.w, self.h - clamped),
                    rect(self.x, self.y, self.w, clamped),
                )
            },
            Side::Bottom => {
                let clamped = self.h.min(amount);
                (
                    rect(self.x, self.y, self.w, self.h - clamped),
                    rect(self.x, self.y + self.h - clamped, self.w, clamped),
                )
            },
        };
        *self = old;
        new
    }
    
    pub fn limit_w(&mut self, max: u16, center: bool) {
        let new_w = self.w.min(max);
        let inset = (self.w - new_w) / 2;
        self.x += center.then(|| inset).unwrap_or_default();
        self.w = new_w;
    }
    
    pub fn to_grid(&self, mut w: u16, mut h: u16, margin: u16, transpose: bool) -> Vec<Rect> {
        let mut rects = Vec::new();
        
        let w_margins = self.w.saturating_sub(w.saturating_sub(1) * margin) as f32;
        let h_margins = self.h.saturating_sub(h.saturating_sub(1) * margin) as f32;

        if transpose {
            swap(&mut w, &mut h);
        }

        for y in 0..h {
            for x in 0..w {
                let mut x = x as f32;
                let mut y = y as f32;
                let mut w = w as f32;
                let mut h = h as f32;
                if transpose {
                    swap(&mut x, &mut y);
                    swap(&mut w, &mut h);
                }
                let m_x = x as u16 * margin;
                let m_y = y as u16 * margin;

                let y0 = self.y + ((y / h) * h_margins) as u16 + m_y;
                let y1 = self.y + (((y + 1.0) / h) * h_margins) as u16 + m_y;
                
                let x0 = self.x + ((x / w) as f32 * w_margins) as u16 + m_x;
                let x1 = self.x + (((x + 1.0) / w) * w_margins) as u16 + m_x;
                
                rects.push(rect(x0, y0, x1 - x0, y1 - y0));
            }
        }
        rects
    }
    
    pub fn moved(&self, x: i16, y: i16) -> Rect {
        Self { x: self.x.saturating_add_signed(x), y: self.y.saturating_add_signed(y), ..*self }
    }
    
    pub fn with_cut(&self, side: Side, amount: u16) -> Rect {
        let mut new = *self;
        new.cut_mut(side, amount);
        new
    }
    
    pub fn with_inset(self, w: u16, h: u16) -> Rect {
        let mut new = self;
        new.inset_mut(w, h);
        new
    }
}

fn rect(x: u16, y: u16, w: u16, h: u16) -> Rect {
    Rect::new(x, y, w, h)
}

#[derive(Debug, Default)]
pub enum Align {
    #[default]
    Start,
    Center,
    End,
}


pub fn grapheme_count(value: &str) -> usize {
    value.graphemes(true).count()
}

pub struct ListLayout {
    items: Vec<String>,
    selected: Option<usize>,
}

impl ListLayout {
    pub fn new() -> Self {
        Self { items: Vec::new(), selected: None }
    }
    
    pub fn with_items(mut self, items: impl Iterator<Item = String>) -> Self {
        for item in items {
            self.items.push(item);
        }
        self
    }

    pub fn with_selected(self, index: usize) -> Self {
        Self {
            selected: Some(index),
            ..self
        }
    }
    
    pub fn render<T: TextBuffer>(&self, rect: Rect, buf: &mut T) -> std::io::Result<()> {
        let mut text = String::new();
        for (item, y) in self.items.iter().zip(0..rect.h as usize) {
            if y > 0 {
                text.push_str("\n");
            }
            if self.selected == Some(y) {
                text.push_str("* ");
            } else {
                text.push_str(" ");
            }
            text.push_str(item);
        }
        TextLayout::new().with_text(text, 0).with_wrap_mode(WrapMode::CutOff { marker: "...", cut_start: false }).render(rect, buf)
    }
}

#[derive(Default)]
pub struct TextLayout {
    sections: Vec<Section>,
    /// Whether to indent line breaks
    centered: bool,
    wrap_mode: WrapMode,
}

#[derive(Clone, Copy, Default)]
pub enum WrapMode {
    #[default]
    Word,
    WordIndentBody(u8),
    WordIndentFirstLine(u8),
    CutOff { marker: &'static str, cut_start: bool },
}

impl TextLayout {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_wrap_mode(self, wrap_mode: WrapMode) -> Self {
        Self { wrap_mode, ..self }
    }

    pub(crate) fn indent_body(self) -> Self {
        self.with_wrap_mode(WrapMode::WordIndentBody(1))
    }

    pub(crate) fn indent_first_line(self) -> Self {
        self.with_wrap_mode(WrapMode::WordIndentFirstLine(2))
    }

    fn with_section(mut self, section: Section) -> Self {
        self.sections.push(section);
        self
    }
    
    pub fn with_header(self, rubrik: &str) -> Self {
        self.with_section(Section::Heading(rubrik.to_uppercase()))
    }
    
    pub fn with_space(self, lines: u8) -> Self {
        self.with_section(Section::Blank(lines))
    }
    
    pub fn with_divider(self) -> Self {
        self.with_section(Section::Divider)
    }
    
    pub fn with_single_line(self, text: &str) -> Self {
        self.with_section(Section::Text{ text: text.lines().next().unwrap_or_default().to_string(), spacing: 0 })
    }
    
    pub fn with_text(self, text: String, spacing: u8) -> Self {
        self.with_section(Section::Text{ text, spacing })
    }
    
    pub fn with_items<D: Display>(self, items: &[D], spacing: u8) -> Self {
        let text = items
            .iter()
            .map(|i| format!("{i}"))
            .collect::<Vec<String>>()
            .join("\n");
        self.with_text(text, spacing)
    }

    pub fn render_cut(&self, rect: &mut Rect, buf: &mut impl TextBuffer) -> std::io::Result<()> {
        let lines: Vec<String> = self.sections.iter().flat_map(|section| section.to_lines(rect.w as usize, self.wrap_mode)).collect();
        let h = (lines.len() as u16).min(rect.h);

        let y_start = self.centered.then(|| (rect.h - h) / 2).unwrap_or_default();
        let text_height = rect.h.saturating_sub(y_start).min(h);

        for (line, y) in lines.into_iter().zip(0..text_height) {
            let w = (grapheme_count(&line) as u16).min(rect.w);
            let x = self.centered.then(|| (rect.w - w) / 2).unwrap_or_default();
            buf.write_line(rect.x + x, rect.y + y_start + y, line)?;
        }
        rect.cut_mut(Side::Top, y_start + text_height);
        Ok(())
    }
    
    pub fn render(&self, rect: Rect, buf: &mut impl TextBuffer) -> std::io::Result<()> {
        self.render_cut(&mut rect.clone(), buf)
    }
    
    pub fn centered(self) -> Self {
        Self { centered: true, ..self }
    }
}

fn line_break_string(s: &str, width: usize, wrap_mode: WrapMode, spacing: u8) -> Vec<String> {
    let mut lines = Vec::new();
    let mut line = String::new();
    let mut line_length = 0;
    let new_line_body = match wrap_mode {
        WrapMode::WordIndentBody(n) => &SPACE_256[0..n as usize],
        _ => "",
    };
    let new_first_line = match wrap_mode {
        WrapMode::WordIndentFirstLine(n) => &SPACE_256[0..n as usize],
        _ => "",
    };
    let mut last_line_broken = false;

    for input_line in s.lines() {
        if let WrapMode::CutOff { marker, cut_start } = wrap_mode {
            let marker_len = grapheme_count(marker);
            let line_graphemes = input_line.graphemes(true);
            let line_len = line_graphemes.clone().count();
            lines.push(if line_len > width {
                let display_width = width.saturating_sub(marker_len);
                if cut_start {
                    let mut chars = line_graphemes.skip(line_len.saturating_sub(display_width)).collect::<Vec<&str>>();
                    chars.insert(0, marker);
                    chars.join("")
                } else {
                    let mut remaining = line_graphemes.take(display_width).collect::<Vec<&str>>();
                    remaining.push(marker);
                    remaining.join("")
                }
            } else {
                input_line.to_string()
            });
            continue;
        }
        let indented_line = if last_line_broken {
            last_line_broken = false;
            format!("{new_first_line}{input_line}")
        } else {
            format!("{input_line}")
        };
        let mut words = Vec::new();
        let unicode_words_indices = indented_line.unicode_word_indices();
        let mut last_start = 0;
        for (this_start, _) in unicode_words_indices.skip(1) {
            words.push(&indented_line[last_start..this_start]);
            last_start = this_start;
        }
        words.push(&indented_line[last_start..]);
        
        // Fit string with wrapping
        'word: for word in words {
            let stripped_len = word.trim_end().graphemes(true).count();
            let graphemes = word.graphemes(true).collect::<Vec<&str>>();
            let length = graphemes.len();
            
            if line_length + stripped_len > width {
                // Part overflows
                last_line_broken = true;
                if stripped_len > width {
                    // Part is longer than can fit, split it between rows
                    let mut remaining_graphemes = graphemes.into_iter();
                    let mut i = 0;
                    loop {
                        let line_space = width.saturating_sub(line_length);
                        for _ in 0..line_space {
                            if i >= stripped_len && line_length >= width {
                                // Don't keep whitespace on end of line
                                break;
                            }
                            let Some(ch) = remaining_graphemes.next() else {
                                break;
                            };
                            line.push_str(ch);
                            line_length += 1;
                            i += 1;
                        }
                        if remaining_graphemes.clone().count() > 0 {
                            lines.push(line);
                            line = new_line_body.to_string();
                            line_length = grapheme_count(&line);
                        } else {
                            continue 'word;
                        }
                    }
                } else {
                    // New line
                    lines.push(line);
                    line = new_line_body.to_string();
                    line_length = grapheme_count(&line);
                }
            }
            line_length += length;
            line.push_str(word);
        }
        // Push last line
        lines.push(line);
        for _ in 0..spacing {
            lines.push(String::new());
        }
        line = String::new();
        line_length = 0;
    }
    lines
}

enum Section {
    Heading(String),
    Text { text: String, spacing: u8 },
    Blank(u8),
    Divider,
}

impl Section {
    fn to_lines(&self, width: usize, wrap: WrapMode) -> Vec<String> {
        match self {
            Section::Heading(s) => line_break_string(&s.to_uppercase(), width, wrap, 0).iter().map(|s|s.to_string()).collect(),
            Section::Text { text, spacing } => line_break_string(text, width, wrap, *spacing),
            Section::Blank(num) => vec!["".to_string(); *num as usize],
            Section::Divider => vec![vec!["─"; width].join("")],
            // Section::Divider => vec![vec!["-"; width].join("")],
        }
    }
}

pub trait TextBuffer {
    fn write_line<D: Display>(&mut self, x: u16, y: u16, line: D) -> std::io::Result<()>;
    fn show_cursor(&mut self, x: u16, y: u16);
    fn cursor(&self) -> Option<(u16, u16)>;
    fn safe_rect(&self) -> Rect;
    fn margin_rect(&self) -> Rect;
    fn clear(&mut self) -> std::io::Result<()>;
    fn flush(&mut self) -> std::io::Result<()>;
}

pub struct TerminalBuffer {
    w: u16,
    h: u16,
    bottom_margin: u16,
    cursor: Option<(u16, u16)>
}

impl TerminalBuffer {
    pub fn new(bottom_margin: u16) -> Self {
        let (w, h) = terminal::size().unwrap();
        Self { w, h, bottom_margin, cursor: None }
    }
}

impl TextBuffer for TerminalBuffer {
    fn write_line<D: Display>(&mut self, x: u16, y: u16, text: D) -> std::io::Result<()> {
        if y >= self.h || x >= self.w {
            return Ok(());
        }
        stdout()
            .queue(cursor::MoveTo(x, y))?
            .queue(style::Print(text))?;
        Ok(())
    }
    
    fn flush(&mut self) -> std::io::Result<()> {
        if let Some((x, y)) = self.cursor {
            stdout()
                .queue(cursor::MoveTo(x, y))?
                .queue(cursor::Show)?;
            self.cursor = None;
        }
        stdout().flush()
    }
    
    fn clear(&mut self) -> std::io::Result<()> {
        stdout().queue(terminal::Clear(terminal::ClearType::All))?;
        Ok(())
    }
    
    fn safe_rect(&self) -> Rect {
        rect(0, 0, self.w, self.h.saturating_sub(self.bottom_margin))
    }
    
    fn margin_rect(&self) -> Rect {
        rect(0, self.h.saturating_sub(self.bottom_margin), self.w, self.bottom_margin)
    }
    
    fn show_cursor(&mut self, x: u16, y: u16) {
        self.cursor = Some((x, y));
    }
    
    fn cursor(&self) -> Option<(u16, u16)> {
        self.cursor
    }
}

#[derive(PartialEq)]
pub struct VecBuffer {
    text: Vec<String>,
    w: u16,
    h: u16,
    bottom_margin: u16,
    cursor: Option<(u16, u16)>,
}

impl std::fmt::Display for VecBuffer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for y in 0..self.h as usize {
            for x in 0..self.w as usize {
                f.write_str(&self.text[y * self.w as usize + x])?;
            }
            if y < self.h.saturating_sub(1) as usize {
                f.write_str("\r\n")?;
            }
        }
        Ok(())
    }
}

impl std::fmt::Debug for VecBuffer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("VecBuffer").field("text", &self.to_string()).field("w", &self.w).field("h", &self.h).field("bottom_margin", &self.bottom_margin).finish()
    }
}

impl VecBuffer {
    pub(crate) fn new(w: u16, h: u16, bottom_margin: u16) -> Self {
        let text = vec![String::from(" "); (w * h) as usize];
        Self { text, w, h, bottom_margin, cursor: None }
    }

    #[cfg(test)]
    pub(crate) fn from_lines(lines: Vec<&str>) -> Option<Self> {
        let Some(first) = lines.get(0) else { return None; };
        let w = grapheme_count(&(Into::<String>::into(*first)));
        let h = lines.len();
        let mut text = Vec::new();
        for line in lines {
            if grapheme_count(&line) != w {
                return None;
            }
            for grapheme in line.graphemes(true) {
                text.push(grapheme.to_string());
            }
        }
        Some(Self { text, w: w as u16, h: h as u16, bottom_margin: 0, cursor: None })
    }
}

impl TextBuffer for VecBuffer {
    fn write_line<D: std::fmt::Display>(&mut self, x: u16, y: u16, text: D) -> std::io::Result<()> {
        for (i, ch) in text.to_string().graphemes(true).enumerate() {
            if x + i as u16 >= self.w || y >= self.h {
                return Ok(());
            }
            self.text[y as usize * self.w as usize + x as usize + i] = ch.to_string();
        }
        Ok(())
    }
    
    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
    
    fn clear(&mut self) -> std::io::Result<()> {
        self.text = vec![String::from(" "); (self.w * self.h) as usize];
        Ok(())
    }
    
    fn safe_rect(&self) -> Rect {
        rect(0, 0, self.w, self.h.saturating_sub(self.bottom_margin))
    }

    fn margin_rect(&self) -> Rect {
        rect(0, self.h.saturating_sub(self.bottom_margin), self.w, self.bottom_margin)
    }
    
    fn show_cursor(&mut self, x: u16, y: u16) {
        self.cursor = Some((x, y))
    }
    
    fn cursor(&self) -> Option<(u16, u16)> {
        self.cursor
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use crate::ui::{Border, BorderStyle, Side, TextBuffer, TextLayout, VecBuffer, WrapMode, rect};

    fn test_wrap_mode(wrap_mode: WrapMode, text: &str, w: u16, h: u16, spacing: u8) -> String {
        let mut buf = VecBuffer::new(w, h, 0);
        let rect = buf.safe_rect();
        TextLayout::new().with_wrap_mode(wrap_mode).with_text(text.to_string(), spacing).render(rect, &mut buf).unwrap();
        buf.to_string()
    }

    #[test]
    fn line_wrap_singleline() {
        let text = "Teststrängen innehåller bara en rad.";
        assert_eq!(test_wrap_mode(WrapMode::Word, &text, 10, 5, 0), [
            "Teststräng",
            "en        ",
            "innehåller",
            "bara en   ",
            "rad.      ",
        ].join("\r\n"));
        assert_eq!(test_wrap_mode(WrapMode::CutOff { marker: "...", cut_start: true }, &text, 10, 5, 0), [
            "...en rad.",
            "          ",
            "          ",
            "          ",
            "          ",
        ].join("\r\n"));
        assert_eq!(test_wrap_mode(WrapMode::CutOff { marker: "...", cut_start: true }, &text, 40, 1, 0), 
            "Teststrängen innehåller bara en rad.    "
        );
    }

    #[test]
    fn line_wrap_multiline() {
        let text = [
            "Denna teststräng innehåller en första mening på 9 ord.",
            "Den tredje raden i teststrängen är lite annorlunda. Apa, Flaggstångspolermedelsfabriksarbetarefackförbundsordföranderåd.",
            "Slutligen en sista rad."].join("\n");
        assert_eq!(test_wrap_mode(WrapMode::Word, &text, 10, 5, 0), [
            "Denna     ",
            "teststräng",
            "innehåller",
            "en första ",
            "mening på ",
        ].join("\r\n"));
        assert_eq!(test_wrap_mode(WrapMode::WordIndentBody(2), &text, 20, 10, 0), [
            "Denna teststräng    ",
            "  innehåller en     ",
            "  första mening på 9",
            "  ord.              ",
            "Den tredje raden i  ",
            "  teststrängen är   ",
            "  lite annorlunda.  ",
            "  Apa, Flaggstångspo",
            "  lermedelsfabriksar",
            "  betarefackförbunds"
        ].join("\r\n"));
        assert_eq!(test_wrap_mode(WrapMode::WordIndentFirstLine(2), &text, 20, 10, 0), [
            "Denna teststräng    ",
            "innehåller en första",
            "mening på 9 ord.    ",
            "  Den tredje raden i",
            "teststrängen är lite",
            "annorlunda. Apa, Fla",
            "ggstångspolermedelsf",
            "abriksarbetarefackfö",
            "rbundsordföranderåd.",
            "  Slutligen en sista",
        ].join("\r\n"));
        assert_eq!(test_wrap_mode(WrapMode::CutOff { marker: "", cut_start: false }, &text, 10, 5, 0), [
            "Denna test",
            "Den tredje",
            "Slutligen ",
            "          ",
            "          ",
        ].join("\r\n"));
        assert_eq!(test_wrap_mode(WrapMode::CutOff { marker: "...", cut_start: true }, &text, 15, 5, 0), [
            "...ng på 9 ord.",
            "...dföranderåd.",
            "...n sista rad.",
            "               ",
            "               ",
        ].join("\r\n"));
    }

    #[test]
    fn border() {
        let style = BorderStyle {
            left: "3",
            right: "4",
            top_left: "0",
            top: "1",
            top_right: "2",
            bottom_left: "5",
            bottom: "6",
            bottom_right: "7",
        };
        let mut buf = VecBuffer::new(4, 3, 0);
        Border::new(rect(0, 0, 4, 3), style).render(&mut buf).unwrap();
        assert_eq!(buf, VecBuffer::from_lines(vec![
            "0112",
            "3  4",
            "5667",
        ]).unwrap());
    }

    #[test]
    fn split_allowed() {
        assert_eq!(rect(0, 0, 100, 90).split(Side::Left, 10).unwrap(), (
            rect(0, 0, 10, 90),
            rect(10, 0, 90, 90)
        ));
        assert_eq!(rect(0, 0, 100, 90).split(Side::Left, 82).unwrap(), (
            rect(0, 0, 82, 90),
            rect(82, 0, 18, 90)
        ));
        assert_eq!(rect(0, 0, 200, 90).split(Side::Right, 15).unwrap(), (
            rect(185, 0, 15, 90),
            rect(0, 0, 185, 90)
        ));
        assert_eq!(rect(0, 0, 120, 90).split(Side::Top, 20).unwrap(), (
            rect(0, 0, 120, 20),
            rect(0, 20, 120, 70)
        ));
        assert_eq!(rect(0, 0, 120, 90).split(Side::Bottom, 21).unwrap(), (
            rect(0, 69, 120, 21),
            rect(0, 0, 120, 69)
        ));
        assert!(rect(0, 0, 0, 0).split(Side::Left, 0).is_some());
    }

    #[test]
    fn split_disallowed() {
        assert!(rect(0, 0, 0, 0).split(Side::Left, 1).is_none());
        assert!(rect(0, 0, 100, 0).split(Side::Left, 200).is_none());
        assert!(rect(0, 0, 100, 10).split(Side::Left, 200).is_none());
    }

    #[test]
    fn cut() {
        let a = rect(10, 20, 10, 10);
        assert_eq!(a.cut(Side::Left, 1), rect(10, 20, 1, 10));
        assert_eq!(a.cut(Side::Left, 20), rect(10, 20, 10, 10));
    }

    #[test]
    fn to_grid() {
        let a = rect(0, 0, 10, 6);
        assert_eq!(a.to_grid(2, 2, 0, false), vec![
            rect(0, 0, 5, 3),
            rect(5, 0, 5, 3),
            rect(0, 3, 5, 3),
            rect(5, 3, 5, 3),
        ]);
        assert_eq!(a.to_grid(2, 2, 0, true), vec![
            rect(0, 0, 5, 3),
            rect(0, 3, 5, 3),
            rect(5, 0, 5, 3),
            rect(5, 3, 5, 3),
        ]);
        assert_eq!(a.to_grid(5, 2, 0, false), vec![
            rect(0, 0, 2, 3),
            rect(2, 0, 2, 3),
            rect(4, 0, 2, 3),
            rect(6, 0, 2, 3),
            rect(8, 0, 2, 3),
            rect(0, 3, 2, 3),
            rect(2, 3, 2, 3),
            rect(4, 3, 2, 3),
            rect(6, 3, 2, 3),
            rect(8, 3, 2, 3),
        ]);

        let a = rect(0, 0, 12, 6);
        assert_eq!(a.to_grid(2, 2, 2, false), vec![
            rect(0, 0, 5, 2),
            rect(7, 0, 5, 2),
            rect(0, 4, 5, 2),
            rect(7, 4, 5, 2),
        ]);

    }
}