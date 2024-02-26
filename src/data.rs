use std::collections::BTreeMap;
use std::error;
use std::fmt;
use std::fmt::Debug;
use std::usize;

pub type Pointer = usize;
pub type LineNum = usize;
pub type InlinePointer = usize;

pub type FunctionName = String;
pub type CallArgs = Vec<MithraVal>;
pub type VarName = String;
pub type Expression = Box<MithraVal>;
pub type PredicateExpression = Box<MithraVal>;
pub type IfExpressions = Vec<MithraVal>;
pub type ElseExpressions = Vec<MithraVal>;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub args: Vec<String>,
    pub exprs: Vec<MithraVal>,
}

#[repr(C)]
#[derive(Debug, Clone, PartialEq)]
pub enum MithraVal {
    Pass,
    Null,
    Int(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
    Variable(LineNum, String),
    List(LineNum, Vec<MithraVal>),
    Dict(LineNum, BTreeMap<String, MithraVal>),
    Function(LineNum, Function),
    FunctionCall(LineNum, FunctionName, CallArgs),
    Assignment(LineNum, VarName, Expression),
    ReturnStatement(LineNum, Expression),
    IfBlock(LineNum, PredicateExpression, IfExpressions),
    IfElseBlock(LineNum, PredicateExpression, IfExpressions, ElseExpressions),
}

fn stringify_bool(val: &bool) -> String {
    let binding = val.to_string();
    let mut chars = binding.chars();
    match chars.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + chars.as_str(),
    }
}

fn stringify_list(list: &Vec<MithraVal>) -> String {
    let mut stringified_list = String::from("[");
    for (i, item) in list.iter().enumerate() {
        stringified_list.push_str(&format!("{}", item));
        if i < list.len() - 1 {
            stringified_list.push_str(&format!(", "));
        }
    }
    stringified_list.push(']');
    stringified_list
}

fn stringify_dict(dict: &BTreeMap<String, MithraVal>) -> String {
    let mut stringified_dict = String::from("{");
    for (i, (k, v)) in dict.iter().enumerate() {
        stringified_dict.push_str(&format!("\"{}\": {}", k, v));
        if i < dict.len() - 1 {
            stringified_dict.push_str(&format!(", "));
        }
    }
    stringified_dict.push('}');
    stringified_dict
}

impl fmt::Display for MithraVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MithraVal::Pass => todo!(),
            MithraVal::Null => write!(f, "None"),
            MithraVal::Int(int) => write!(f, "{}", int),
            MithraVal::Float(float) => write!(f, "{}", float),
            MithraVal::String(string) => write!(f, "\"{}\"", string),
            MithraVal::Char(char) => write!(f, "'{}'", char),
            MithraVal::Bool(bool) => write!(f, "{}", stringify_bool(bool)),
            MithraVal::List(_, list) => write!(f, "{}", stringify_list(list)),
            MithraVal::Dict(_, dict) => write!(f, "{}", stringify_dict(dict)),
            MithraVal::Function(
                _,
                Function {
                    name,
                    args,
                    exprs: _,
                },
            ) => write!(f, "Function '{}', args: {:?}.", name, args),
            MithraVal::Variable(_, _) => todo!(),
            MithraVal::FunctionCall(_, _, _) => todo!(),
            MithraVal::Assignment(_, _, _) => todo!(),
            MithraVal::ReturnStatement(_, _) => todo!(),
            MithraVal::IfBlock(_, _, _) => todo!(),
            MithraVal::IfElseBlock(_, _, _, _) => todo!(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum MithraError {
    TextConsumed,                               // error when entire raw text is consumed
    NothingParsed(LineNum, InlinePointer),      // recoverable generic parsing error
    ParseError(String, LineNum, InlinePointer), // non-recoverable parsing error with error context
    RuntimeError(String, LineNum),              // runtime error
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
            MithraError::RuntimeError(message, line_num) => {
                write!(f, "RuntimeError: {} (line: {}).", message, line_num)
            }
        }
    }
}

impl error::Error for MithraError {}

#[derive(Debug)]
pub struct Text {
    chars: Vec<char>,
    pub pointer: usize,
    pointer_to_line_num: BTreeMap<Pointer, LineNum>,
    pointer_to_inline_position: BTreeMap<Pointer, InlinePointer>,
    final_char_line_num: usize,
    final_char_inline_positon: usize,
}

impl Text {
    pub fn new(chars: Vec<char>) -> Text {
        let mut chars = chars.clone();
        let mut line_num: usize = 1;
        let mut inline_position: usize = 1;
        let mut pointer_to_line_num = BTreeMap::new();
        let mut pointer_to_inline_position = BTreeMap::new();

        // append closing newline to the end of script
        chars.push('\n');
        // fill up line number and inline position maps
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
        let final_char_line_num = *pointer_to_line_num.get(&(chars.len() - 1)).unwrap_or(&0);
        let final_char_inline_positon = *pointer_to_inline_position
            .get(&(chars.len() - 1))
            .unwrap_or(&0);
        Text {
            chars,
            pointer: 0,
            pointer_to_line_num,
            pointer_to_inline_position,
            final_char_line_num,
            final_char_inline_positon,
        }
    }

    pub fn get_next(&self) -> Option<char> {
        match self.chars.get(self.pointer) {
            Some(char) => Some(*char),
            None => None,
        }
    }

    pub fn incr_pointer(&mut self) {
        // TODO: use decr_poiner & incr pointer implicitly
        self.pointer += 1
    }

    pub fn line_num(&self) -> LineNum {
        *self
            .pointer_to_line_num
            .get(&self.pointer)
            .unwrap_or_else(|| {
                if self.pointer >= self.chars.len() {
                    &self.final_char_line_num
                } else {
                    &0
                }
            })
    }

    pub fn inline_position(&self) -> InlinePointer {
        *self
            .pointer_to_inline_position
            .get(&self.pointer)
            .unwrap_or_else(|| {
                if self.pointer >= self.chars.len() {
                    &self.final_char_inline_positon
                } else {
                    &0
                }
            })
    }
}

pub type FnParser<T> = fn(&mut Text) -> Result<T, MithraError>;
pub type ClosureParser<T> = Box<dyn Fn(&mut Text) -> Result<T, MithraError>>;
