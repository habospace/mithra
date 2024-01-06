use mithra::data::MithraError;
use mithra::data::MithraError::ParseError;
use mithra::data::MithraVal;
use mithra::data::Text;
use mithra::parser::mithra_parsers;
use std::fs::File;
use std::io::{self, BufRead};

fn parse_mithra_program(file_path: String) -> Result<Vec<MithraVal>, MithraError> {
    let file = File::open(file_path).unwrap();
    let mut chars: Vec<char> = Vec::new();
    for line in io::BufReader::new(file).lines() {
        for c in line.unwrap().chars() {
            chars.push(c);
        }
        chars.push('\n');
    }
    let mut text = Text::new(chars);
    mithra_parsers::parse_inline_exprs(0)(&mut text)
}

#[test]
fn test_errors() {
    for (mithra_script, expected_err) in vec![
        (
            format!("reserved_word_err.mth"),
            ParseError(format!("can't use 'return' it's a reserved word"), 2, 11),
        ),
        (
            format!("unterminated_string_err.mth"),
            ParseError(format!("unterminated '\"'"), 14, 1),
        ),
        (
            format!("unnecessary_sep_char_err.mth"),
            ParseError(format!("unnecessary ','"), 5, 82),
        ),
        (
            format!("missing_function_def_colon_err.mth"),
            ParseError(format!("function args have to be followed by ':'"), 16, 22),
        ),
        (
            format!("missing_closing_parent_after_func_args_err.mth"),
            ParseError(
                format!("function args have to be closed by ')' eg. 'arg2, arg3)'"),
                21,
                19,
            ),
        ),
        (
            format!("missing_opening_parent_before_func_args_err.mth"),
            ParseError(
                format!("function name has to be followed by '(' eg. '(...'"),
                16,
                20,
            ),
        ),
        (
            format!("missing_function_name_err.mth"),
            ParseError(
                format!("'def' has to be followed by valid function name"),
                15,
                5,
            ),
        ),
        (
            format!("indentation_err_1.mth"),
            ParseError(format!("incorrect indentation, expected_level: 3"), 31, 1),
        ),
        (
            format!("missing_colon_after_else_err.mth"),
            ParseError(format!("'else' has to be followed by ':'"), 42, 9),
        ),
        (
            format!("indentation_err_2.mth"),
            ParseError(format!("incorrect indentation, expected_level: 2"), 25, 5),
        ),
        (
            format!("missing_colon_after_predicate_expr_err.mth"),
            ParseError(
                format!("predicate expression in 'if' block has to be followed by ':'"),
                40,
                26,
            ),
        ),
        (
            format!("missing_predicate_expr_err.mth"),
            ParseError(
                format!("'if' has to be followed by predicate expression"),
                54,
                8,
            ),
        ),
        (
            format!("missing_assignment_expr_err.mth"),
            ParseError(
                format!("'=' has to be followed by an expression in assignment"),
                8,
                12,
            ),
        ),
        (
            format!("missing_return_expr_err.mth"),
            ParseError(
                format!("'return' has to be followed by an expression"),
                37,
                12,
            ),
        ),
        (
            format!("unterminated_dict_err.mth"),
            ParseError(format!("unterminated dict, missing '}}'"), 5, 81),
        ),
        (
            format!("unterminated_list_err.mth"),
            ParseError(format!("unterminated list, missing ']'"), 17, 36),
        ),
        (
            format!("indentation_err_3.mth"),
            ParseError(format!("incorrect indentation, expected_level: 1"), 24, 1),
        ),
        (
            format!("line_not_fully_parsed_err.mth"),
            ParseError(format!("couldn't fully parse line"), 19, 66),
        ),
    ] {
        let file_path = format!("tests/test_programs/{}", mithra_script);
        let result = parse_mithra_program(file_path);
        assert_eq!(result.err().unwrap(), expected_err)
    }
}
