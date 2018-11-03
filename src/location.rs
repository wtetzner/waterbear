
use std::fmt;
use files::FileID;

#[derive(Debug,Eq,PartialEq,Ord,PartialOrd,Hash,Clone)]
pub struct Location {
    file: FileID,
    pos: usize,
    line: usize,
    column: usize
}

impl Location {
    pub fn new(file: FileID, pos: usize, line: usize, column: usize) -> Location {
        Location {
            file: file,
            pos: pos,
            line: line,
            column: column
        }
    }

    pub fn default() -> Location {
        Location::new(
            FileID::new(0),
            0,
            1,
            0
        )
    }

    pub fn file(&self) -> FileID { self.file }
    pub fn pos(&self) -> usize { self.pos }
    pub fn line(&self) -> usize { self.line }
    pub fn column(&self) -> usize { self.column }

    pub fn to_span(&self) -> Span {
        Span::new(self.clone(), self.clone())
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{})", self.file, self.line, self.column)
    }    
}

#[derive(Debug,Eq,PartialEq,Ord,PartialOrd,Hash,Clone)]
pub struct Span {
    start: Location,
    end: Location,
    parent: Option<Box<Span>>
}

impl Span {
    pub fn new(start: Location, end: Location) -> Span {
        Span {
            start: start,
            end: end,
            parent: None
        }
    }

    pub fn with_parent(&self, parent: Span) -> Span {
        let mut new_span = self.clone();
        new_span.parent = Some(Box::new(parent));
        new_span
    }

    pub fn default() -> Span {
        Span::new(Location::default(),Location::default())
    }

    pub fn from(start: &Span, end: &Span) -> Span {
        Span::new(start.start().clone(), end.end().clone())
    }

    pub fn start(&self) -> &Location { &self.start }
    pub fn end(&self) -> &Location { &self.end }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.start.file == self.end.file {
            write!(f, "{}:{}:{}-{}:{})",
                   self.start.file,
                   self.start.line,
                   self.start.column,
                   self.end.line,
                   self.end.column)
        } else {
            write!(f, "{}-{})", self.start, self.end)
        }
    }    
}

pub trait Positioned {
    fn span(&self) -> Span;
}
