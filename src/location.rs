
interner!(Filenames, FileID);

#[derive(Debug,Eq,PartialEq,Clone,Copy)]
pub struct Location {
    file: FileID,
    line: usize,
    column: usize
}

impl Location {
    pub fn new(file: FileID, line: usize, column: usize) -> Location {
        Location {
            file: file,
            line: line,
            column: column
        }
    }

    pub fn filename<'a>(&self, filenames: &'a Filenames) -> Option<&'a str> {
        filenames.get(self.file)
    }

    pub fn file_id(&self) -> FileID {
        self.file
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn column(&self) -> usize {
        self.column
    }

    pub fn to_string(&self, filenames: &Filenames) -> String {
        format!("{}:{}:{}",
                self.filename(filenames).unwrap_or("<unknown>"),
                self.line(),
                self.column())
    }
}

#[derive(Debug,Eq,PartialEq,Clone)]
pub struct Span {
    file: FileID,
    start_line: usize,
    start_column: usize,
    end_line: usize,
    end_column: usize
}

impl Span {
    pub fn new(
        file: FileID,
        start_line: usize,
        start_column: usize,
        end_line: usize,
        end_column: usize
    ) -> Span {
        Span {
            file: file,
            start_line: start_line,
            start_column: start_column,
            end_line: end_line,
            end_column: end_column
        }
    }

    pub fn from_locations(start: Location, end: Location) -> Span {
        Span::new(start.file_id(), start.line(), start.column(), end.line(), end.column())
    }

    pub fn start(&self) -> Location {
        Location::new(self.file, self.start_line, self.start_column)
    }

    pub fn end(&self) -> Location {
        Location::new(self.file, self.end_line, self.end_column)
    }

    pub fn file_id(&self) -> FileID {
        self.file
    }

    pub fn start_line(&self) -> usize {
        self.start_line
    }

    pub fn start_column(&self) -> usize {
        self.start_column
    }

    pub fn end_line(&self) -> usize {
        self.end_line
    }

    pub fn end_column(&self) -> usize {
        self.end_column
    }

    pub fn filename<'a>(&self, filenames: &'a Filenames) -> Option<&'a str> {
        filenames.get(self.file)
    }

    pub fn to_string(&self, filenames: &Filenames) -> String {
        format!("{}:{}:{}-{}:{}",
                self.filename(filenames).unwrap_or("<unknown>"),
                self.start_line(),
                self.start_column(),
                self.end_line(),
                self.end_column())
    }
}
