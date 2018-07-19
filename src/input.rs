
use unicode_segmentation::UnicodeSegmentation;
use std::ops::Deref;
use regex::Regex;
use files::FileID;
use location::Location;

#[derive(Debug,Clone)]
pub struct Input<'b> {
    name: FileID,
    text: &'b str,
    pos: usize,
    line: usize,
    column: usize
}

impl<'b> Input<'b> {
    pub fn new<'y>(name: FileID, text: &'y str) -> Input<'y> {
        Input {
            name: name,
            text: text,
            pos: 0,
            line: 1,
            column: 0
        }
    }

    pub fn peek(&self) -> Option<&str> {
        UnicodeSegmentation::graphemes(&self.text[self.pos..], true).next()
    }

    pub fn eof(&self) -> bool {
        self.pos >= self.text.len()
    }

    pub fn location(&self) -> Location {
        Location::new(self.name, self.pos, self.line, self.column)
    }

    pub fn as_str(&self) -> &'b str {
        &self.text[self.pos..]
    }

    pub fn starts_with(&self, text: &str) -> bool {
        self.as_str().starts_with(text)
    }

    pub fn name(&self) -> FileID {
        self.name
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn column(&self) -> usize {
        self.column
    }

    pub fn skip_whitespace(&self) -> Input<'b> {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"\s+").unwrap();
        }
        let mut skipped: usize = 0;
        match RE.captures_iter(&self.text[self.pos..]).next() {
            Some(cap) => skipped += cap[0].len(),
            None => ()
        }
        self.update(skipped)
    }

    /// Update the position information by advancing the current
    /// position by `num_bytes` bytes.
    pub fn update(&self, num_bytes: usize) -> Input<'b> {
        let mut input = self.clone();
        let start = input.pos;
        let mut graphemes = UnicodeSegmentation::graphemes(&input.text[input.pos..], true);
        loop {
            match graphemes.next() {
                Some(text) => {
                    if (input.pos - start) + text.len() > num_bytes {
                        break;
                    }
                    input.pos += text.len();
                    if text == "\r\n" || text == "\n" {
                        input.line += 1;
                        input.column = 0;
                    } else {
                        input.column += 1;
                    }
                },
                None => break
            }
        }
        input
    }
}

impl<'b> Deref for Input<'b> {
    type Target = str;

    fn deref(&self) -> &str {
        &self.text[self.pos..]
    }
}

#[cfg(test)]
mod tests {
    use input::Input;
    use files::FileID;

    #[test]
    fn test_update() {
        let input = Input::new(FileID::new(43), "some cool\n input \r\n and \n stuff");
        let input = input.update(20);
        assert!(input.pos == 20);
        assert!(input.line == 3);
        assert!(input.column == 1);
        assert!("and \n stuff" == &*input);
    }

    #[test]
    fn test_update_past_end() {
        let input = Input::new(FileID::new(43), "some cool\n input \r\n and \n stuff");
        let input = input.update(33);
        assert!(input.pos == 31);
        assert!(input.line == 4);
        assert!(input.column == 6);
        assert!("" == &*input);
    }

    #[test]
    fn test_skip_whitespace() {
        let input = Input::new(FileID::new(43), "   \n \r\n \r  \t some stuff");
        let input = input.skip_whitespace();
        assert!(input.pos == 13);
        assert!(input.line == 3);
        assert!(input.column == 6);
        assert!("some stuff" == &*input);
    }
}
