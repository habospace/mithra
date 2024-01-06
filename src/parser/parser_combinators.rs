use std::fmt::Debug;

use crate::data::ClosureParser;
use crate::data::MithraError;
use crate::data::Text;

use super::errors::unnecessary_separator_err;

pub fn run_parser<T: 'static>(parser: ClosureParser<T>) -> ClosureParser<T> {
    Box::new(move |text: &mut Text| -> Result<T, MithraError> {
        let before_pointer = text.pointer;
        match parser(text) {
            Ok(result) => Ok(result),
            Err(err) => {
                text.pointer = before_pointer;
                return Err(err);
            }
        }
    })
}

pub fn combine<T: 'static>(
    parsers: Vec<ClosureParser<T>>,
    selector: fn(&mut Vec<T>) -> T,
) -> ClosureParser<T>
where
    T: Debug,
{
    let parser = Box::new(move |text: &mut Text| -> Result<T, MithraError> {
        let mut results = Vec::new();
        for parser in &parsers {
            let result = parser(text)?;
            results.push(result);
        }
        Ok(selector(&mut results))
    });
    run_parser(parser)
}

pub fn sep_by<T1: 'static, T2: 'static>(
    parser: ClosureParser<T1>,
    sep_parser: ClosureParser<T2>,
) -> ClosureParser<Vec<T1>>
where
    T1: Debug,
{
    let parser = Box::new(move |text: &mut Text| -> Result<Vec<T1>, MithraError> {
        let mut results = Vec::new();
        let first = parser(text);
        if first.is_err() {
            return Ok(results);
        } else {
            results.push(first.unwrap());
        }
        loop {
            let sep = sep_parser(text);
            if sep.is_err() {
                break;
            }
            let next = parser(text).map_err(|err| match err {
                MithraError::ParseError(msg, line_num, inline_pos) => {
                    MithraError::ParseError(msg, line_num, inline_pos)
                }
                _ => unnecessary_separator_err(text.line_num(), text.inline_position()),
            })?;
            results.push(next);
        }
        Ok(results)
    });
    run_parser(parser)
}

pub fn many<T: 'static>(parser: ClosureParser<T>) -> ClosureParser<Vec<T>>
where
    T: Debug,
{
    let parser = Box::new(move |text: &mut Text| -> Result<Vec<T>, MithraError> {
        let mut results = Vec::new();
        loop {
            match parser(text) {
                Ok(result) => {
                    results.push(result);
                }
                Err(MithraError::ParseError(msg, line_num, inline_pos)) => {
                    return Err(MithraError::ParseError(msg, line_num, inline_pos));
                }
                Err(_) => {
                    return Ok(results);
                }
            }
        }
    });
    run_parser(parser)
}
