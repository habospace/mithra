use std::collections::BTreeMap;
use std::num::ParseFloatError;
use std::num::ParseIntError;

use crate::data::ClosureParser;
use crate::data::FnParser;
use crate::data::Function;
use crate::data::MithraError;
use crate::data::MithraVal;
use crate::data::Text;

use super::errors::indentation_err;
use super::errors::line_not_fully_parsed_err;
use super::errors::missing_assignment_expr_err;
use super::errors::missing_colon_after_else_err;
use super::errors::missing_colon_after_predicate_expr_err;
use super::errors::missing_mandatory_else_expr_err;
use super::errors::missing_mandatory_if_expr_err;
use super::errors::missing_predicate_expr_err;
use super::errors::missing_return_expr_err;
use super::errors::no_expr_in_function_err;
use super::errors::nothing_parsed_err;
use super::errors::unterminated_dictionary_err;
use super::errors::unterminated_func_call_err;
use super::errors::unterminated_list_err;

use super::errors::missing_close_parenthesis_before_func_args_err;
use super::errors::missing_colon_in_function_definition_line_err;
use super::errors::missing_function_args_err;
use super::errors::missing_function_name_err;
use super::errors::missing_open_parenthesis_before_func_args_err;

use super::primitive_parsers::any_string;
use super::primitive_parsers::char;
use super::primitive_parsers::numeric_chars;
use super::primitive_parsers::skip_many;
use super::primitive_parsers::string;
use super::primitive_parsers::word;

use super::parser_combinators::combine;
use super::parser_combinators::many;
use super::parser_combinators::run_parser;
use super::parser_combinators::sep_by;

type FnMithraParser = FnParser<MithraVal>;
type ClosureMithraParser = ClosureParser<MithraVal>;

fn parse_null(text: &mut Text) -> Result<MithraVal, MithraError> {
    string(format!("None").chars().collect())(text)?;
    Ok(MithraVal::Null)
}

pub fn parse_bool(text: &mut Text) -> Result<MithraVal, MithraError> {
    let true_chars = format!("True").chars().collect();
    let _true = string(true_chars)(text);
    if _true.is_ok() {
        return Ok(MithraVal::Bool(true));
    }
    let false_chars = format!("False").chars().collect();
    string(false_chars)(text)?;
    Ok(MithraVal::Bool(false))
}

fn parse_int(text: &mut Text) -> Result<MithraVal, MithraError> {
    fn chars_to_int(chars: Vec<char>) -> Result<i64, ParseIntError> {
        let string: String = chars.into_iter().collect();
        string.as_str().parse::<i64>()
    }

    let int_chars = numeric_chars(text)?;
    match chars_to_int(int_chars) {
        Ok(int) => Ok(MithraVal::Int(int)),
        Err(_) => Err(nothing_parsed_err(text.line_num(), text.inline_position())),
    }
}

fn parse_float(text: &mut Text) -> Result<MithraVal, MithraError> {
    fn chars_to_float(chars: Vec<char>) -> Result<f64, ParseFloatError> {
        let string: String = chars.into_iter().collect();
        string.as_str().parse::<f64>()
    }

    let mut integer_part = numeric_chars(text)?;
    parse_char('.')(text)?;
    integer_part.extend(vec!['.']);
    let fractional_part = numeric_chars(text)?;
    integer_part.extend(fractional_part);

    match chars_to_float(integer_part) {
        Ok(float) => Ok(MithraVal::Float(float)),
        Err(_) => Err(nothing_parsed_err(text.line_num(), text.inline_position())),
    }
}

fn parse_atom(text: &mut Text) -> Result<MithraVal, MithraError> {
    let atom = word(text)?;
    Ok(MithraVal::Atomic(atom))
}

fn parse_string(text: &mut Text) -> Result<MithraVal, MithraError> {
    let string = any_string(text)?;
    Ok(MithraVal::String(string))
}

fn parse_function_call(text: &mut Text) -> Result<MithraVal, MithraError> {
    let function_name = word(text)?;
    skip_spaces()(text)?;
    parse_char('(')(text)?;
    let args = comma_sep_exprs()(text)?;

    parse_char(')')(text).map_err(|_| {
        unterminated_func_call_err(&function_name, text.line_num(), text.inline_position())
    })?;

    Ok(MithraVal::List(vec![
        MithraVal::Atomic(function_name),
        MithraVal::List(args),
    ]))
}

fn parse_list(text: &mut Text) -> Result<MithraVal, MithraError> {
    parse_char('[')(text)?;
    let exprs = comma_sep_exprs()(text)?;
    parse_char(']')(text)
        .map_err(|_| unterminated_list_err(text.line_num(), text.inline_position()))?;
    Ok(MithraVal::List(exprs))
}

fn parse_dict(text: &mut Text) -> Result<MithraVal, MithraError> {
    fn parse_kv_pair(text: &mut Text) -> Result<(String, MithraVal), MithraError> {
        skip_spaces()(text)?;
        let key = any_string(text)?;
        skip_spaces()(text)?;
        parse_char(':')(text)?;
        skip_spaces()(text)?;
        let value = parse_expr(text)?;
        skip_spaces()(text)?;
        return Ok((key, value));
    }
    let parse_kv_pair = run_parser(Box::new(parse_kv_pair));
    parse_char('{')(text)?;
    let mut kv_pairs = sep_by(Box::new(parse_kv_pair), parse_char(','))(text)?;
    parse_char('}')(text)
        .map_err(|_| unterminated_dictionary_err(text.line_num(), text.inline_position()))?;
    let mut dictionary = BTreeMap::new();
    loop {
        match kv_pairs.pop() {
            Some(kv_pair) => {
                dictionary.insert(kv_pair.0, kv_pair.1);
            }
            None => break,
        }
    }
    Ok(MithraVal::Dict(dictionary))
}

fn parse_expr(text: &mut Text) -> Result<MithraVal, MithraError> {
    let parsers: [FnMithraParser; 9] = [
        parse_bool,
        parse_null,
        parse_function_call,
        parse_list,
        parse_dict,
        parse_float,
        parse_int,
        parse_string,
        parse_atom,
    ];
    for parser in &parsers {
        match run_parser(Box::new(*parser))(text) {
            Ok(value) => {
                return Ok(value);
            }
            Err(MithraError::ParseError(msg, line_num, inline_pos)) => {
                return Err(MithraError::ParseError(msg, line_num, inline_pos));
            }
            Err(_) => {}
        }
    }
    Err(nothing_parsed_err(text.line_num(), text.inline_position()))
}

fn parse_return_statement(text: &mut Text) -> Result<MithraVal, MithraError> {
    let return_chars = format!("return").chars().collect();
    let return_parsed = string(return_chars)(text)?;
    skip_spaces()(text)?;
    let expr = parse_expr(text).map_err(|err| match err {
        MithraError::ParseError(msg, line_num, inline_pos) => {
            MithraError::ParseError(msg, line_num, inline_pos)
        }
        _ => missing_return_expr_err(text.line_num(), text.inline_position()),
    })?;
    Ok(MithraVal::List(vec![
        MithraVal::String(return_parsed),
        expr,
    ]))
}

fn parse_assignment(text: &mut Text) -> Result<MithraVal, MithraError> {
    let varname = parse_atom(text)?;
    skip_spaces()(text)?;
    parse_char('=')(text)?;
    skip_spaces()(text)?;
    let expr = parse_expr(text).map_err(|err| match err {
        MithraError::ParseError(msg, line_num, inline_pos) => {
            MithraError::ParseError(msg, line_num, inline_pos)
        }
        _ => missing_assignment_expr_err(text.line_num(), text.inline_position()),
    })?;
    Ok(MithraVal::List(vec![
        varname,
        MithraVal::Atomic(format!("=")),
        expr,
    ]))
}

fn parse_empty_line(text: &mut Text) -> Result<MithraVal, MithraError> {
    skip_spaces()(text)?;
    parse_char('\n')(text)?;
    Ok(MithraVal::Null)
}

// Parser function factories
fn parse_empty_lines() -> ClosureParser<Vec<MithraVal>> {
    Box::new(
        move |text: &mut Text| -> Result<Vec<MithraVal>, MithraError> {
            many(run_parser(Box::new(parse_empty_line)))(text)
        },
    )
}

fn parse_char(c: char) -> ClosureMithraParser {
    let parser = Box::new(move |text: &mut Text| -> Result<MithraVal, MithraError> {
        Ok(MithraVal::Char(char(c)(text)?))
    });
    run_parser(parser)
}

fn skip_spaces() -> ClosureMithraParser {
    let parser = Box::new(move |text: &mut Text| -> Result<MithraVal, MithraError> {
        skip_many(vec![' '])(text)?;
        Ok(MithraVal::Null)
    });
    run_parser(parser)
}

fn inline_expr() -> ClosureParser<MithraVal> {
    fn first_non_null(items: &mut Vec<MithraVal>) -> MithraVal {
        loop {
            match items.pop() {
                Some(MithraVal::Null) => {}
                Some(non_null) => return non_null,
                None => break,
            }
        }
        MithraVal::Null
    }
    combine(
        vec![skip_spaces(), Box::new(parse_expr), skip_spaces()],
        first_non_null,
    )
}

fn comma_sep_exprs() -> ClosureParser<Vec<MithraVal>> {
    sep_by(inline_expr(), parse_char(','))
}

fn inline_word() -> ClosureParser<String> {
    fn first_non_empty_string(items: &mut Vec<String>) -> String {
        loop {
            match items.pop() {
                Some(word) => {
                    if !word.is_empty() {
                        return word;
                    }
                }
                None => break,
            }
        }
        format!("")
    }
    combine(
        vec![skip_many(vec![' ']), Box::new(word), skip_many(vec![' '])],
        first_non_empty_string,
    )
}

fn comma_sep_words() -> ClosureParser<Vec<String>> {
    sep_by(inline_word(), parse_char(','))
}

fn parse_indentation(indent: usize, error_on_failure: bool) -> ClosureMithraParser {
    let parser = Box::new(move |text: &mut Text| -> Result<MithraVal, MithraError> {
        let indent_chars = match indent {
            0 => Vec::new(),
            _ => {
                let spaces: String = std::iter::repeat(' ').take(indent * 4).collect();
                spaces.chars().collect()
            }
        };
        let indentation = string(indent_chars)(text);
        if indentation.is_err() && error_on_failure {
            Err(indentation_err(
                indent,
                text.line_num(),
                text.inline_position(),
            ))
        } else if indentation.is_err() {
            Err(nothing_parsed_err(text.line_num(), text.inline_position()))
        } else {
            Ok(MithraVal::Null)
        }
    });
    run_parser(parser)
}

fn parse_inline_expr(indent: usize, force_indentation: bool) -> ClosureMithraParser {
    let parser = Box::new(move |text: &mut Text| -> Result<MithraVal, MithraError> {
        // check if text has been consumed
        if text.get_next().is_none() {
            return Err(MithraError::TextConsumed);
        }
        // try parsing empty line first
        if run_parser(Box::new(parse_empty_line))(text).is_ok() {
            return Ok(MithraVal::Null);
        }
        // parse indentation next
        parse_indentation(indent, force_indentation)(text)?;
        // parse multi line expressions (start with more complex, order matters!)
        let closure_parsers: [ClosureMithraParser; 3] = [
            parse_if_else_block(indent),
            parse_if_block(indent),
            parse_function(indent),
        ];
        for parser in &closure_parsers {
            let result = parser(text);
            match result {
                Ok(mithra_val) => {
                    return Ok(mithra_val);
                }
                Err(MithraError::ParseError(msg, line_num, inline_pos)) => {
                    return Err(MithraError::ParseError(msg, line_num, inline_pos));
                }
                Err(_) => {}
            }
        }
        // parse single line expressions
        let fn_parsers: [FnMithraParser; 3] =
            [parse_return_statement, parse_assignment, parse_expr];
        for parser in &fn_parsers {
            match run_parser(Box::new(*parser))(text) {
                Ok(mithra_val) => {
                    skip_spaces()(text)?;
                    match parse_char('\n')(text) {
                        Ok(_) => {
                            return Ok(mithra_val);
                        }
                        Err(_) => {
                            return Err(line_not_fully_parsed_err(
                                text.line_num(),
                                text.inline_position(),
                            ))
                        }
                    }
                }
                Err(MithraError::ParseError(msg, line_num, inline_pos)) => {
                    return Err(MithraError::ParseError(msg, line_num, inline_pos));
                }
                Err(_) => {}
            }
        }
        Err(nothing_parsed_err(text.line_num(), text.inline_position()))
    });
    run_parser(parser)
}

pub fn parse_inline_exprs(indent: usize) -> ClosureParser<Vec<MithraVal>> {
    Box::new(
        move |text: &mut Text| -> Result<Vec<MithraVal>, MithraError> {
            many(parse_inline_expr(indent, false))(text)
        },
    )
}

pub fn parse_if_block(indent: usize) -> ClosureMithraParser {
    let parser = Box::new(move |text: &mut Text| -> Result<MithraVal, MithraError> {
        // parse 'if'
        let if_chars = format!("if").chars().collect();
        string(if_chars)(text)?;
        parse_char(' ')(text)?;
        skip_spaces()(text)?;
        // parse predicate expression
        let predicate = parse_expr(text).map_err(|err| match err {
            MithraError::ParseError(msg, line_num, inline_pos) => {
                MithraError::ParseError(msg, line_num, inline_pos)
            }
            _ => missing_predicate_expr_err(text.line_num(), text.inline_position()),
        })?;
        skip_spaces()(text)?;
        // parse ':' after predicate expression
        parse_char(':')(text).map_err(|_| {
            missing_colon_after_predicate_expr_err(text.line_num(), text.inline_position())
        })?;

        parse_empty_lines()(text)?;
        // parse mandatory first 'if' expression
        let first_if_expr = parse_inline_expr(indent + 1, true)(text).map_err(|err| match err {
            MithraError::ParseError(msg, line_num, inline_pos) => {
                MithraError::ParseError(msg, line_num, inline_pos)
            }
            _ => missing_mandatory_if_expr_err(text.line_num(), text.inline_position()),
        })?;
        // set up var for storing components of the 'if-else' block
        let mut if_else_block_parsed = vec![MithraVal::Atomic(format!("if")), predicate];
        // parse rest of expressions in 'if' block
        let tail_if_exprs = parse_inline_exprs(indent + 1)(text);
        match tail_if_exprs {
            Ok(mut exprs) => {
                exprs.insert(0, first_if_expr);
                if_else_block_parsed.push(MithraVal::List(exprs));
            }
            Err(MithraError::ParseError(msg, line_num, inline_pos)) => {
                return Err(MithraError::ParseError(msg, line_num, inline_pos));
            }
            Err(_) => {
                if_else_block_parsed.push(MithraVal::List(vec![first_if_expr]));
            }
        }
        Ok(MithraVal::List(if_else_block_parsed))
    });
    run_parser(parser)
}

pub fn parse_if_else_block(indent: usize) -> ClosureMithraParser {
    let parser = Box::new(move |text: &mut Text| -> Result<MithraVal, MithraError> {
        // parse 'if' block
        let if_block_parsed = parse_if_block(indent)(text)?;
        let mut if_else_block_parsed = match if_block_parsed {
            MithraVal::List(exprs) => exprs,
            _ => Vec::new(),
        };
        parse_indentation(indent, false)(text)?;
        // parse 'else:' and return everything we parsed if we fail
        let else_chars = format!("else").chars().collect();
        string(else_chars)(text)?;
        skip_spaces()(text)?;
        // parse ':' after 'else'
        parse_char(':')(text)
            .map_err(|_| missing_colon_after_else_err(text.line_num(), text.inline_position()))?;
        if_else_block_parsed.push(MithraVal::Atomic(format!("else")));
        parse_empty_lines()(text)?;
        // parse first mandatory 'else' expression
        let first_else_expr =
            parse_inline_expr(indent + 1, true)(text).map_err(|err| match err {
                MithraError::ParseError(msg, line_num, inline_pos) => {
                    MithraError::ParseError(msg, line_num, inline_pos)
                }
                _ => missing_mandatory_else_expr_err(text.line_num(), text.inline_position()),
            })?;
        // parse tail expressions in 'else' block
        let tail_else_exprs = parse_inline_exprs(indent + 1)(text);
        match tail_else_exprs {
            Ok(mut exprs) => {
                exprs.insert(0, first_else_expr);
                if_else_block_parsed.push(MithraVal::List(exprs));
            }
            Err(MithraError::ParseError(msg, line_num, inline_pos)) => {
                return Err(MithraError::ParseError(msg, line_num, inline_pos));
            }
            Err(_) => {
                if_else_block_parsed.push(MithraVal::List(vec![first_else_expr]));
            }
        }
        Ok(MithraVal::List(if_else_block_parsed))
    });
    run_parser(parser)
}

pub fn parse_function(indent: usize) -> ClosureMithraParser {
    let parser = Box::new(move |text: &mut Text| -> Result<MithraVal, MithraError> {
        // parse 'def'
        let def_chars = format!("def").chars().collect();
        string(def_chars)(text)?;
        // parse function name
        parse_char(' ')(text)?;
        skip_spaces()(text)?;
        let function_name = word(text)
            .map_err(|_| missing_function_name_err(text.line_num(), text.inline_position()))?;
        skip_spaces()(text)?;
        // parse '('
        parse_char('(')(text).map_err(|_| {
            missing_open_parenthesis_before_func_args_err(text.line_num(), text.inline_position())
        })?;
        // parse args
        let args = comma_sep_words()(text)
            .map_err(|_| missing_function_args_err(text.line_num(), text.inline_position()))?;
        // parse ')'
        parse_char(')')(text).map_err(|_| {
            missing_close_parenthesis_before_func_args_err(text.line_num(), text.inline_position())
        })?;
        // parse ':'
        parse_char(':')(text).map_err(|_| {
            missing_colon_in_function_definition_line_err(text.line_num(), text.inline_position())
        })?;
        parse_empty_lines()(text)?;
        // parse first mandatory function expression
        let first_expr = parse_inline_expr(indent + 1, true)(text).map_err(|err| match err {
            MithraError::ParseError(msg, line_num, inline_pos) => {
                MithraError::ParseError(msg, line_num, inline_pos)
            }
            _ => no_expr_in_function_err(&function_name, text.line_num(), text.inline_position()),
        })?;
        // create container for function expressions
        let mut func_exprs = vec![first_expr];
        // parse function tail expressions
        let tail_exprs = parse_inline_exprs(indent + 1)(text);
        match tail_exprs {
            Ok(exprs) => func_exprs.extend(exprs),
            Err(MithraError::ParseError(msg, line_num, inline_pos)) => {
                return Err(MithraError::ParseError(msg, line_num, inline_pos))
            }
            Err(_) => {}
        }
        let function = Function {
            name: function_name,
            params: args,
            body: func_exprs,
        };
        Ok(MithraVal::Function(function))
    });
    run_parser(parser)
}
