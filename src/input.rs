
use unicode_segmentation::UnicodeSegmentation;
use std::ops::Deref;

#[derive(Debug)]
struct Input {
    name: String,
    text: String,
    pos: usize,
    line: usize,
    column: usize
}

impl Input {
    pub fn new(name: String, text: String) -> Input {
        Input {
            name: name,
            text: text,
            pos: 0,
            line: 1,
            column: 0
        }
    }

    pub fn as_str(&self) -> &str {
        &self.text[self.pos..]
    }

    /// Update the position information by advancing the current
    /// position by `num_bytes` bytes.
    pub fn update(&mut self, num_bytes: usize) {
        let start = self.pos;
        let mut graphemes = UnicodeSegmentation::graphemes(&self.text[self.pos..], true);
        loop {
            match graphemes.next() {
                Some(text) => {
                    if (self.pos - start) + text.len() > num_bytes {
                        break;
                    }
                    self.pos += text.len();
                    if text == "\r\n" || text == "\n" {
                        self.line += 1;
                        self.column = 0;
                    } else {
                        self.column += 1;
                    }
                },
                None => break
            }
        }
    }
}

impl Deref for Input {
    type Target = str;

    fn deref(&self) -> &str {
        &self.text[self.pos..]
    }
}

#[cfg(test)]
mod tests {
    use asm::lexer::Input;
    #[test]
    fn test_update() {
        let mut input = Input::new("<unknown>".to_owned(), "some cool\n input \r\n and \n stuff".to_owned());
        input.update(20);
        assert!(input.pos == 20);
        assert!(input.line == 3);
        assert!(input.column == 1);
        assert!("and \n stuff" == &*input);
    }

    #[test]
    fn test_update_past_end() {
        let mut input = Input::new("<unknown>".to_owned(), "some cool\n input \r\n and \n stuff".to_owned());
        input.update(33);
        assert!(input.pos == 31);
        assert!(input.line == 4);
        assert!(input.column == 6);
        assert!("" == &*input);
    }
}
