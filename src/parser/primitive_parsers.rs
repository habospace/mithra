use crate::data::MithraError;
use crate::data::Text;
use lazy_static::lazy_static;

use super::errors::nothing_parsed_err;
use super::errors::reserved_word_err;
use super::errors::unterminated_string_err;

lazy_static! {
    pub static ref RESERVED_WORDS: Vec<String> = vec![
        format!("else"),
        format!("if"),
        format!("return"),
        format!("def"),
        format!("None"),
        format!("True"),
        format!("False"),
    ];
}

// parsers: 'char', 'stop_at', 'any_string', 'word', 'string', 'numeric_chars', 'skip_many'

pub fn char(c: char) -> Box<dyn Fn(&mut Text) -> Result<char, MithraError>> {
    Box::new(move |text: &mut Text| -> Result<char, MithraError> {
        match text.get_next() {
            Some(next_char) => {
                if c == next_char {
                    text.incr_pointer();
                    return Ok(c);
                }
            }
            None => {}
        }
        Err(nothing_parsed_err(text.line_num(), text.inline_position()))
    })
}

pub fn stop_at(stop_chars: Vec<char>) -> Box<dyn Fn(&mut Text) -> Result<String, MithraError>> {
    Box::new(move |text: &mut Text| -> Result<String, MithraError> {
        let mut parsed_string: String = String::new();
        loop {
            match text.get_next() {
                Some(c) => {
                    if stop_chars.contains(&c) {
                        text.incr_pointer();
                        return Ok(parsed_string);
                    } else {
                        parsed_string.push(c);
                        text.incr_pointer()
                    }
                }
                None => return Err(nothing_parsed_err(text.line_num(), text.inline_position())),
            }
        }
    })
}

pub fn any_string(text: &mut Text) -> Result<String, MithraError> {
    char('"')(text)?;
    let string = stop_at(vec!['"'])(text)
        .map_err(|_| unterminated_string_err(text.line_num(), text.inline_position()))?;
    Ok(string)
}

pub fn word(text: &mut Text) -> Result<String, MithraError> {
    let mut parsed = String::new();
    loop {
        match text.get_next() {
            Some(next_char) => {
                if '_' == next_char || next_char.is_alphabetic() {
                    parsed.push(next_char);
                    text.incr_pointer();
                } else {
                    break;
                }
            }
            None => break,
        }
    }
    if parsed.is_empty() {
        Err(nothing_parsed_err(text.line_num(), text.inline_position()))
    } else if RESERVED_WORDS.contains(&parsed) {
        Err(reserved_word_err(
            &parsed,
            text.line_num(),
            text.inline_position(),
        ))
    } else {
        Ok(parsed)
    }
}

pub fn string(string_chars: Vec<char>) -> Box<dyn Fn(&mut Text) -> Result<String, MithraError>> {
    Box::new(move |text: &mut Text| -> Result<String, MithraError> {
        let mut string_pointer: usize = 0;
        let mut parsed_string: String = String::new();
        loop {
            let next_string_char: Option<&char> = string_chars.get(string_pointer);
            if next_string_char.is_none() {
                return Ok(parsed_string);
            }
            match text.get_next() {
                Some(next_char) => {
                    if *next_string_char.unwrap() == next_char {
                        parsed_string.push(next_char);
                        string_pointer += 1;
                        text.incr_pointer()
                    } else {
                        break;
                    }
                }
                None => break,
            }
        }
        Err(nothing_parsed_err(text.line_num(), text.inline_position()))
    })
}

pub fn numeric_chars(text: &mut Text) -> Result<Vec<char>, MithraError> {
    let mut numerics: Vec<char> = Vec::new();
    loop {
        match text.get_next() {
            Some(next_char) => {
                if next_char.is_numeric() {
                    numerics.push(next_char);
                    text.incr_pointer()
                } else {
                    break;
                }
            }
            None => break,
        }
    }
    if numerics.is_empty() {
        Err(nothing_parsed_err(text.line_num(), text.inline_position()))
    } else {
        Ok(numerics)
    }
}

pub fn skip_many(skip_chars: Vec<char>) -> Box<dyn Fn(&mut Text) -> Result<String, MithraError>> {
    Box::new(move |text: &mut Text| -> Result<String, MithraError> {
        loop {
            match text.get_next() {
                Some(next_char) => {
                    if skip_chars.contains(&next_char) {
                        text.incr_pointer()
                    } else {
                        break;
                    }
                }
                None => break,
            }
        }
        Ok(String::from(""))
    })
}
