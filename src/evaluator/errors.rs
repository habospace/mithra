use crate::data::LineNum;
use crate::data::MithraError;

pub fn undefined_variable_err(line_num: LineNum, var_name: &String) -> MithraError {
    MithraError::RuntimeError(format!("Variable '{}' is not defined", var_name), line_num)
}

pub fn cant_return_in_global_scope_err(line_num: LineNum) -> MithraError {
    MithraError::RuntimeError(format!("Can't return in global scope"), line_num)
}

pub fn variable_is_not_callable_err(line_num: LineNum, var_name: &String) -> MithraError {
    MithraError::RuntimeError(format!("'{}' is not callable", var_name), line_num)
}

pub fn incorrect_number_of_func_args_err(
    line_num: LineNum,
    func_name: &String,
    n_correct_args: usize,
    n_call_args: usize,
) -> MithraError {
    MithraError::RuntimeError(
        format!(
            "'{}' takes {} args (function called with {} args",
            func_name, n_correct_args, n_call_args
        ),
        line_num,
    )
}

pub fn type_err(line_num: LineNum, func_name: &String, correct_types: &String) -> MithraError {
    MithraError::RuntimeError(
        format!(
            "'{}' takes args of the following types: {}",
            func_name, correct_types
        ),
        line_num,
    )
}
