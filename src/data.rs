use std::collections::BTreeMap;
use std::error;
use std::fmt;
use std::usize;

type Pointer = usize;
pub type LineNum = usize;
pub type InlinePointer = usize;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<MithraVal>,
}

#[repr(C)]
#[derive(Debug, Clone, PartialEq)]
pub enum MithraVal {
    Null,
    Atomic(String),
    List(Vec<MithraVal>),
    Dict(BTreeMap<String, MithraVal>),
    Int(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
    Function(Function),
}

#[derive(Debug, PartialEq)]
pub enum MithraError {
    TextConsumed,                                 // error when entire raw text is consumed
    NothingParsed(LineNum, InlinePointer),        // recoverable generic parsing error
    ParseError(String, LineNum, InlinePointer),   // non-recoverable parsing error with error context
    RuntimeError(String, LineNum, InlinePointer), // runtime error
}

impl fmt::Display for MithraError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MithraError::TextConsumed => write!(f, "TextConsumed"),
            MithraError::NothingParsed(line_num, inline_pos) => write!(
                f,
                "NothingParsedError: couldn't parse an expression (line: {}, char: {}).",
                line_num, inline_pos
            ),
            MithraError::ParseError(message, line_num, inline_pos) => write!(
                f,
                "ParseError: {} (line: {}, char: {}).",
                message, line_num, inline_pos
            ),
            MithraError::RuntimeError(message, line_num, inline_pointer) => write!(
                f,
                "RuntimeError: {} (line: {}, char: {}).",
                message, line_num, inline_pointer
            ),
        }
    }
}

impl error::Error for MithraError {}

#[derive(Debug)]
pub struct Text {
    // raw chars and pointers
    chars: Vec<char>,
    pub pointer: Pointer,
    // datastructures to keep track of
    // where we are in terms of line number
    // and inline position for error messages
    pointer_to_line_num: BTreeMap<Pointer, LineNum>,
    pointer_to_inline_position: BTreeMap<Pointer, InlinePointer>,
    // save final line number and inline position
    // in case we error on final character
    final_char_line_num: LineNum,
    final_char_inline_positon: InlinePointer,
}

impl Text {
    pub fn new(chars: Vec<char>) -> Text {
        let mut chars = chars.clone();
        let mut line_num: LineNum = 1;
        let mut inline_position: InlinePointer = 1;

        let mut pointer_to_line_num = BTreeMap::new();
        let mut pointer_to_inline_position = BTreeMap::new();

        let mut final_char_line_num: LineNum = 0;
        let mut final_char_inline_positon: InlinePointer = 0;
        // append closing newline to the end of script
        chars.push('\n');
        // fill up line number and inline_position maps
        for (i, &c) in chars.iter().enumerate() {
            pointer_to_line_num.insert(i, line_num);
            pointer_to_inline_position.insert(i, inline_position);
            inline_position += 1;
            if c == '\n' {
                line_num += 1;
                inline_position = 1;
            }
        }
        // get final line number
        match pointer_to_line_num.get(&(chars.len() - 1)) {
            Some(line_num) => {
                final_char_line_num = *line_num;
            }
            None => {}
        }
        // get final inline position
        match pointer_to_inline_position.get(&(chars.len() - 1)) {
            Some(inline_pos) => {
                final_char_inline_positon = *inline_pos;
            }
            None => {}
        }
        Text {
            chars: chars,
            pointer: 0,
            pointer_to_line_num: pointer_to_line_num,
            pointer_to_inline_position: pointer_to_inline_position,
            final_char_line_num: final_char_line_num,
            final_char_inline_positon: final_char_inline_positon,
        }
    }

    pub fn get_next(&self) -> Option<char> {
        match self.chars.get(self.pointer) {
            Some(char) => Some(*char),
            None => None,
        }
    }

    pub fn incr_pointer(&mut self) {
        self.pointer += 1
    }

    pub fn line_num(&self) -> LineNum {
        match self.pointer_to_line_num.get(&self.pointer) {
            Some(line_num) => *line_num,
            None => {
                if self.pointer >= self.chars.len() {
                    self.final_char_line_num
                } else {
                    0
                }
            }
        }
    }

    pub fn inline_position(&self) -> InlinePointer {
        match self.pointer_to_inline_position.get(&self.pointer) {
            Some(inline_pos) => *inline_pos,
            None => {
                if self.pointer >= self.chars.len() {
                    self.final_char_inline_positon
                } else {
                    0
                }
            }
        }
    }
}

pub type FnParser<T> = fn(&mut Text) -> Result<T, MithraError>;
pub type ClosureParser<T> = Box<dyn Fn(&mut Text) -> Result<T, MithraError>>;
