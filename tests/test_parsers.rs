use std::collections::BTreeMap;

use mithra::data::MithraError;
use mithra::data::MithraVal;
use mithra::data::Text;
use mithra::parser::mithra_parsers;
use mithra::parser::primitive_parsers;

#[test]
fn test_parse_null() {
    let chars = format!("None").chars().collect();
    let mut text = Text::new(chars);
    let result = mithra_parsers::parse_null(&mut text).unwrap();
    assert_eq!(MithraVal::Null, result);
}

#[test]
fn test_parse_bool() {
    for (raw, expected) in vec![
        (format!("False"), MithraVal::Bool(false)),
        (format!("True"), MithraVal::Bool(true)),
    ] {
        let chars = raw.chars().collect();
        let mut text = Text::new(chars);
        let parsed = mithra_parsers::parse_bool(&mut text).unwrap();
        assert_eq!(expected, parsed);
    }
}

#[test]
fn test_parse_int() {
    for (raw, expected) in vec![
        (format!("-25"), MithraVal::Int(-25)),
        (format!("-2"), MithraVal::Int(-2)),
        (format!("0"), MithraVal::Int(0)),
        (format!("4"), MithraVal::Int(4)),
        (format!("456"), MithraVal::Int(456)),
    ] {
        let chars = raw.chars().collect();
        let mut text = Text::new(chars);
        let parsed = mithra_parsers::parse_int(&mut text).unwrap();
        assert_eq!(expected, parsed);
    }
}

#[test]
fn test_parse_float() {
    for (raw, expected) in vec![
        (format!("-25.0"), MithraVal::Float(-25.0)),
        (format!("-2.1"), MithraVal::Float(-2.1)),
        (format!("0.0"), MithraVal::Float(0.0)),
        (format!("0.001"), MithraVal::Float(0.001)),
        (format!("4.3"), MithraVal::Float(4.3)),
        (format!("456.895"), MithraVal::Float(456.895)),
    ] {
        let chars = raw.chars().collect();
        let mut text = Text::new(chars);
        let parsed = mithra_parsers::parse_float(&mut text).unwrap();
        assert_eq!(expected, parsed);
    }
}

#[test]
fn test_parse_atom() {
    for (raw, expected) in vec![
        (
            format!("snake_case_word"),
            MithraVal::Atomic(format!("snake_case_word")),
        ),
        (
            format!("CamelCaseWord"),
            MithraVal::Atomic(format!("CamelCaseWord")),
        ),
        (format!("word"), MithraVal::Atomic(format!("word"))),
    ] {
        let chars = raw.chars().collect();
        let mut text = Text::new(chars);
        let parsed = mithra_parsers::parse_atom(&mut text).unwrap();
        assert_eq!(expected, parsed);
    }

    for raw in primitive_parsers::RESERVED_WORDS.iter() {
        let chars = raw.chars().collect();
        let mut text = Text::new(chars);
        let err = mithra_parsers::parse_atom(&mut text).err().unwrap();
        assert_eq!(
            err,
            MithraError::ParseError(
                format!("can't use '{}' it's a reserved word", raw,),
                text.line_num(),
                text.inline_position()
            )
        );
    }
}

#[test]
fn test_parse_string() {
    for (raw, expected) in vec![
        (format!("\"string\""), MithraVal::String(format!("string"))),
        (
            format!("\"something\""),
            MithraVal::String(format!("something")),
        ),
        (
            format!("\"nothing\""),
            MithraVal::String(format!("nothing")),
        ),
    ] {
        let chars = raw.chars().collect();
        let mut text = Text::new(chars);
        let parsed = mithra_parsers::parse_string(&mut text).unwrap();
        assert_eq!(expected, parsed);
    }
    for raw in vec![format!("\"unterminated"), format!("\"string")] {
        let chars = raw.chars().collect();
        let mut text = Text::new(chars);
        let err = mithra_parsers::parse_string(&mut text).err().unwrap();
        assert_eq!(
            err,
            MithraError::ParseError(
                format!("unterminated '\"'",),
                text.line_num(),
                text.inline_position()
            )
        );
    }
}

#[test]
pub fn test_parse_function_call() {
    for (raw, expected) in vec![
        (
            format!("add(1, 2, 3)"),
            MithraVal::List(vec![
                MithraVal::Atomic(format!("add")),
                MithraVal::List(vec![
                    MithraVal::Int(1),
                    MithraVal::Int(2),
                    MithraVal::Int(3),
                ]),
            ]),
        ),
        (
            format!("concat(\"google \", \"earth\")"),
            MithraVal::List(vec![
                MithraVal::Atomic(format!("concat")),
                MithraVal::List(vec![
                    MithraVal::String(format!("google ")),
                    MithraVal::String(format!("earth")),
                ]),
            ]),
        ),
        (
            format!("take(5, [1, 2, 3, 4])"),
            MithraVal::List(vec![
                MithraVal::Atomic(format!("take")),
                MithraVal::List(vec![
                    MithraVal::Int(5),
                    MithraVal::List(vec![
                        MithraVal::Int(1),
                        MithraVal::Int(2),
                        MithraVal::Int(3),
                        MithraVal::Int(4),
                    ]),
                ]),
            ]),
        ),
        (
            format!("append(my_list, {{\"1\": 1, \"2\": [1, 2, 3, 4]}})"),
            MithraVal::List(vec![
                MithraVal::Atomic(format!("append")),
                MithraVal::List(vec![
                    MithraVal::Atomic(format!("my_list")),
                    mithra::data::MithraVal::Dict(BTreeMap::from([
                        (format!("1"), MithraVal::Int(1)),
                        (
                            format!("2"),
                            MithraVal::List(vec![
                                MithraVal::Int(1),
                                MithraVal::Int(2),
                                MithraVal::Int(3),
                                MithraVal::Int(4),
                            ]),
                        ),
                    ])),
                ]),
            ]),
        ),
    ] {
        let chars = raw.chars().collect();
        let mut text = Text::new(chars);
        let parsed = mithra_parsers::parse_function_call(&mut text).unwrap();
        assert_eq!(expected, parsed);
    }
}
