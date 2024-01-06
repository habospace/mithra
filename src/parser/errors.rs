use crate::data::InlinePointer;
use crate::data::LineNum;
use crate::data::MithraError;

pub fn nothing_parsed_err(line_num: LineNum, inline_position: InlinePointer) -> MithraError {
    MithraError::NothingParsed(line_num, inline_position)
}

pub fn line_not_fully_parsed_err(line_num: LineNum, inline_position: InlinePointer) -> MithraError {
    MithraError::ParseError(
        format!("couldn't fully parse line"),
        line_num,
        inline_position,
    )
}

pub fn no_expr_in_function_err(
    func_name: &String,
    line_num: LineNum,
    inline_position: InlinePointer,
) -> MithraError {
    MithraError::ParseError(
        format!("function '{}' must have at least one expression", func_name),
        line_num,
        inline_position,
    )
}

pub fn unterminated_func_call_err(
    func_name: &String,
    line_num: LineNum,
    inline_position: InlinePointer,
) -> MithraError {
    MithraError::ParseError(
        format!("missing ')' for '{}' function call", func_name),
        line_num,
        inline_position,
    )
}

pub fn unterminated_list_err(line_num: LineNum, inline_position: InlinePointer) -> MithraError {
    MithraError::ParseError(
        format!("unterminated list, missing ']'",),
        line_num,
        inline_position,
    )
}

pub fn unterminated_dictionary_err(
    line_num: LineNum,
    inline_position: InlinePointer,
) -> MithraError {
    MithraError::ParseError(
        format!("unterminated dict, missing '}}'",),
        line_num,
        inline_position,
    )
}

pub fn missing_return_expr_err(line_num: LineNum, inline_position: InlinePointer) -> MithraError {
    MithraError::ParseError(
        format!("'return' has to be followed by an expression",),
        line_num,
        inline_position,
    )
}

pub fn missing_assignment_expr_err(
    line_num: LineNum,
    inline_position: InlinePointer,
) -> MithraError {
    MithraError::ParseError(
        format!("'=' has to be followed by an expression in assignment",),
        line_num,
        inline_position,
    )
}

pub fn indentation_err(
    indent: usize,
    line_num: LineNum,
    inline_position: InlinePointer,
) -> MithraError {
    MithraError::ParseError(
        format!("incorrect indentation, expected_level: {}", indent),
        line_num,
        inline_position,
    )
}

pub fn missing_predicate_expr_err(
    line_num: LineNum,
    inline_position: InlinePointer,
) -> MithraError {
    MithraError::ParseError(
        format!("'if' has to be followed by predicate expression",),
        line_num,
        inline_position,
    )
}

pub fn missing_colon_after_predicate_expr_err(
    line_num: LineNum,
    inline_position: InlinePointer,
) -> MithraError {
    MithraError::ParseError(
        format!("predicate expression in 'if' block has to be followed by ':'",),
        line_num,
        inline_position,
    )
}

pub fn missing_mandatory_if_expr_err(
    line_num: LineNum,
    inline_position: InlinePointer,
) -> MithraError {
    MithraError::ParseError(
        format!("'if' block must have at least one expression",),
        line_num,
        inline_position,
    )
}

pub fn missing_colon_after_else_err(
    line_num: LineNum,
    inline_position: InlinePointer,
) -> MithraError {
    MithraError::ParseError(
        format!("'else' has to be followed by ':'",),
        line_num,
        inline_position,
    )
}

pub fn missing_mandatory_else_expr_err(
    line_num: LineNum,
    inline_position: InlinePointer,
) -> MithraError {
    MithraError::ParseError(
        format!("'else' block must have at least one expression",),
        line_num,
        inline_position,
    )
}

pub fn missing_function_name_err(line_num: LineNum, inline_position: InlinePointer) -> MithraError {
    MithraError::ParseError(
        format!("'def' has to be followed by valid function name",),
        line_num,
        inline_position,
    )
}

pub fn missing_open_parenthesis_before_func_args_err(
    line_num: LineNum,
    inline_position: InlinePointer,
) -> MithraError {
    MithraError::ParseError(
        format!("function name has to be followed by '(' eg. '(...'",),
        line_num,
        inline_position,
    )
}

pub fn missing_function_args_err(line_num: LineNum, inline_position: InlinePointer) -> MithraError {
    MithraError::ParseError(
        format!("function name has to be followed by args eg. '(arg1, arg2, ...'",),
        line_num,
        inline_position,
    )
}

pub fn missing_close_parenthesis_after_func_args_err(
    line_num: LineNum,
    inline_position: InlinePointer,
) -> MithraError {
    MithraError::ParseError(
        format!("function args have to be closed by ')' eg. 'arg2, arg3)'",),
        line_num,
        inline_position,
    )
}

pub fn missing_colon_in_function_definition_line_err(
    line_num: LineNum,
    inline_position: InlinePointer,
) -> MithraError {
    MithraError::ParseError(
        format!("function args have to be followed by ':'",),
        line_num,
        inline_position,
    )
}

pub fn unnecessary_separator_err(line_num: LineNum, inline_position: InlinePointer) -> MithraError {
    MithraError::ParseError(format!("unnecessary ','",), line_num, inline_position)
}

pub fn reserved_word_err(
    word: &String,
    line_num: LineNum,
    inline_position: InlinePointer,
) -> MithraError {
    MithraError::ParseError(
        format!("can't use '{}' it's a reserved word", word,),
        line_num,
        inline_position,
    )
}

pub fn unterminated_string_err(line_num: LineNum, inline_position: InlinePointer) -> MithraError {
    MithraError::ParseError(format!("unterminated '\"'",), line_num, inline_position)
}
