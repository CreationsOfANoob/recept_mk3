use std::{fmt::{Debug, Display}, io::{Write, stdout}, mem::swap};

use crossterm::{QueueableCommand, cursor, style::{self}, terminal};
use unicode_segmentation::UnicodeSegmentation;

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
    
    pub fn to_grid(&self, mut w: u16, mut h: u16, transpose: bool) -> Vec<Rect> {
        let mut rects = Vec::new();
        if transpose {
            swap(&mut w, &mut h);
        }
        for y in 0..h {
            for x in 0..w {
                let mut w = w as f32;
                let mut h = h as f32;
                let mut x = x as f32;
                let mut y = y as f32;

                if transpose {
                    swap(&mut x, &mut y);
                    swap(&mut w, &mut h);
                }
                let y0 = self.y + ((y / h) * self.h as f32) as u16;
                let y1 = self.y + (((y + 1.0) / h) * self.h as f32) as u16;
                
                let x0 = self.x + ((x / w) * self.w as f32) as u16;
                let x1 = self.x + (((x + 1.0) / w) * self.w as f32) as u16;
                
                rects.push(rect(x0, y0, x1 - x0, y1 - y0));
            }
        }
        rects
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

#[allow(unused)]
fn grapheme_count(value: &str) -> usize {
    value.graphemes(true).count()
}

pub struct ListLayout {
    items: Vec<String>,
    selected: usize,
}

impl ListLayout {
    pub fn new() -> Self {
        Self { items: Vec::new(), selected: 0 }
    }
    
    pub fn with_items(mut self, items: impl Iterator<Item = String>) -> Self {
        for item in items {
            self.items.push(item);
        }
        self
    }

    pub fn with_selected(self, index: usize) -> Self {
        Self {
            selected: index,
            ..self
        }
    }
    
    pub fn render<T: TextBuffer>(&self, rect: Rect, buf: &mut T) -> std::io::Result<()> {
        let mut y = rect.y;
        for (item, _) in self.items.iter().zip(0..rect.h) {
            let text = limit_line_width(item, rect.w.saturating_sub(2) as usize);
            buf.write_line(rect.x + 2, y, text)?;
            y += 1;
        }
        buf.write_line(rect.x, rect.y + self.selected as u16, ">")?;
        Ok(())
    }
}

fn limit_line_width(s: &str, max_width: usize) -> String {
    let single_line = s.lines().next().unwrap_or_default();
    let mut graphemes = single_line.graphemes(true);
    let num_graphemes = graphemes.clone().count();
    let mut output = String::new();
    if num_graphemes > max_width {
        output.push_str(&graphemes.by_ref().take(max_width.saturating_sub(3) as usize).collect::<Vec<&str>>().join(""));
        output.push_str("...");
    } else {
        output.push_str(single_line);
    }
    output
}

pub struct TextLayout {
    sections: Vec<Section>,
    /// Whether to indent line breaks
    indent_break: bool,
    indent_section: bool,
}
impl TextLayout {
    pub fn new() -> Self {
        Self { sections: Vec::new(), indent_break: false, indent_section: false }
    }

    pub(crate) fn with_line_break_indentation(mut self) -> Self {
        self.indent_break = true;
        self
    }

    pub(crate) fn with_section_indentation(mut self) -> Self {
        self.indent_section = true;
        self
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
        let mut y = 0;
        'section: for section in &self.sections {
            for text in section.to_lines(rect.w as usize, self.indent_section, self.indent_break) {
                buf.write_line(rect.x, rect.y + y, text)?;
                y += 1;
                if y >= rect.h {
                    break 'section;
                }
            }
        }
        rect.cut_mut(Side::Top, y);
        Ok(())
    }
    
    pub fn render(&self, rect: Rect, buf: &mut impl TextBuffer) -> std::io::Result<()> {
        self.render_cut(&mut rect.clone(), buf)
    }
}

fn line_break_string(s: &str, width: usize, indent_section: bool, indent_break: bool, spacing: u8) -> Vec<String> {
    let mut lines = Vec::new();
    let mut line = String::new();
    let mut line_length = 0;
    let new_line = if indent_break { " " } else { "" };
    let mut last_line_broken = false;

    for input_line in s.lines() {
        let indented_line = if last_line_broken && indent_section {
            last_line_broken = false;
            format!("  {input_line}")
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
        for word in words {
            let graphemes = word.graphemes(true).collect::<Vec<&str>>();
            let length = graphemes.len();
            if line_length + length > width {
                // Part overflows
                last_line_broken = true;
                if length > width {
                    // Part is longer than can fit, split it on two rows
                    line.push_str( &graphemes[0..width].join(""));
                    lines.push(line);

                    line = [new_line, &graphemes[width..].join("")].join("").to_string();
                    line_length = grapheme_count(&line);
                    continue;
                } else {
                    // New line
                    lines.push(line);
                    line = new_line.to_string();
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
    fn to_lines(&self, width: usize, indent_section: bool, indent_break: bool) -> Vec<String> {
        match self {
            Section::Heading(s) => line_break_string(&s.to_uppercase(), width, indent_section, indent_break, 0).iter().map(|s|s.to_string()).collect(),
            Section::Text { text, spacing } => line_break_string(text, width, indent_section, indent_break, *spacing),
            Section::Blank(num) => vec!["".to_string(); *num as usize],
            Section::Divider => vec![vec!["─"; width].join("")],
            // Section::Divider => vec![vec!["-"; width].join("")],
        }
    }
}

pub trait TextBuffer {
    fn write_line<D: Display>(&mut self, x: u16, y: u16, text: D) -> std::io::Result<()>;
    fn safe_rect(&self) -> Rect;
    fn margin_rect(&self) -> Rect;
    fn clear(&mut self) -> std::io::Result<()>;
    fn flush(&mut self) -> std::io::Result<()>;
}

pub struct TerminalBuffer {
    w: u16,
    h: u16,
    bottom_margin: u16
}

impl TerminalBuffer {
    pub fn new(bottom_margin: u16) -> Self {
        let (w, h) = terminal::size().unwrap();
        Self { w, h, bottom_margin }
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
}

#[derive(PartialEq)]
pub struct VecBuffer {
    text: Vec<String>,
    w: u16,
    h: u16,
    bottom_margin: u16
}

impl std::fmt::Display for VecBuffer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for y in 0..self.h as usize {
            for x in 0..self.w as usize {
                f.write_str(&self.text[y * self.w as usize + x])?;
            }
            f.write_str("\r\n")?;
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
        Self { text, w, h, bottom_margin }
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
        Some(Self { text, w: w as u16, h: h as u16, bottom_margin: 0 })
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
}

#[cfg(test)]
mod tests {
    use crate::ui::{Border, BorderStyle, Side, rect, VecBuffer};

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
        assert_eq!(a.to_grid(2, 2, false), vec![
            rect(0, 0, 5, 3),
            rect(5, 0, 5, 3),
            rect(0, 3, 5, 3),
            rect(5, 3, 5, 3),
        ]);
        assert_eq!(a.to_grid(2, 2, true), vec![
            rect(0, 0, 5, 3),
            rect(0, 3, 5, 3),
            rect(5, 0, 5, 3),
            rect(5, 3, 5, 3),
        ]);
        assert_eq!(a.to_grid(5, 2, false), vec![
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

    }
}