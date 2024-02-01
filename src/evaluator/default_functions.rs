use super::errors::incorrect_number_of_func_args_err;
use super::errors::type_err;
use crate::data::MithraVal;
use anyhow::anyhow;
use anyhow::Context;
use anyhow::Result;
use std::clone;
use std::vec;

pub fn validate_binary_operation_args(
    args: Vec<MithraVal>,
    func_name: &String,
    line_num: usize,
) -> Result<(MithraVal, MithraVal)> {
    if args.len() != 2 {
        return Err(anyhow!(incorrect_number_of_func_args_err(
            line_num,
            &func_name,
            2,
            args.len()
        )));
    }
    let first = args.get(0).unwrap();
    let second = args.get(1).unwrap();
    Ok((first.clone(), second.clone()))
}

pub fn plus_impl<T: std::ops::Add<Output = T> + Clone>(a: &T, b: &T) -> T {
    a.clone() + b.clone()
}

pub fn minus_impl<T: std::ops::Sub<Output = T> + Clone>(a: &T, b: &T) -> T {
    a.clone() - b.clone()
}

pub fn mul_impl<T: std::ops::Mul<Output = T> + Clone>(a: &T, b: &T) -> T {
    a.clone() * b.clone()
}

pub fn div_impl<T: std::ops::Div<Output = T> + Clone>(a: &T, b: &T) -> T {
    a.clone() / b.clone()
}

pub fn mod_impl(a: &i64, b: &i64) -> i64 {
    *a % *b
}

pub fn eqv_impl<T: std::cmp::PartialEq>(a: &T, b: &T) -> bool {
    *a == *b
}

pub fn lt_impl<T: std::cmp::PartialOrd>(a: &T, b: &T) -> bool {
    *a < *b
}

pub fn gt_impl<T: std::cmp::PartialOrd>(a: &T, b: &T) -> bool {
    *a > *b
}

pub fn neqv_impl<T: std::cmp::PartialOrd>(a: &T, b: &T) -> bool {
    a != b
}

pub fn gte_impl<T: std::cmp::PartialOrd>(a: &T, b: &T) -> bool {
    a >= b
}

pub fn lte_impl<T: std::cmp::PartialOrd>(a: &T, b: &T) -> bool {
    a <= b
}

pub fn int_numeric_binop(
    a: &MithraVal,
    b: &MithraVal,
    binop: fn(&i64, &i64) -> i64,
) -> Result<MithraVal> {
    match a {
        MithraVal::Int(a) => match b {
            MithraVal::Int(b) => {
                return Ok(MithraVal::Int(binop(a, b)));
            }
            _ => {}
        },
        _ => {}
    }
    Err(anyhow!(type_err(0, &format!(""), &format!("(int, int)"))))
}

pub fn float_numeric_binop(
    a: &MithraVal,
    b: &MithraVal,
    binop: fn(&f64, &f64) -> f64,
) -> Result<MithraVal> {
    match a {
        MithraVal::Float(a) => match b {
            MithraVal::Float(b) => {
                return Ok(MithraVal::Float(binop(&a, &b)));
            }
            _ => {}
        },
        _ => {}
    }
    Err(anyhow!(type_err(
        0,
        &format!(""),
        &format!("(float, float)")
    )))
}

pub fn int_bool_binop(
    a: &MithraVal,
    b: &MithraVal,
    binop: fn(&i64, &i64) -> bool,
) -> Result<MithraVal> {
    match a {
        MithraVal::Int(a) => match b {
            MithraVal::Int(b) => {
                return Ok(MithraVal::Bool(binop(a, b)));
            }
            _ => {}
        },
        _ => {}
    }
    Err(anyhow!(type_err(0, &format!(""), &format!("(int, int)"))))
}

pub fn float_bool_binop(
    a: &MithraVal,
    b: &MithraVal,
    binop: fn(&f64, &f64) -> bool,
) -> Result<MithraVal> {
    match a {
        MithraVal::Float(a) => match b {
            MithraVal::Float(b) => {
                return Ok(MithraVal::Bool(binop(a, b)));
            }
            _ => {}
        },
        _ => {}
    }
    Err(anyhow!(type_err(
        0,
        &format!(""),
        &format!("(float, float)")
    )))
}

pub fn string_bool_binop(
    a: &MithraVal,
    b: &MithraVal,
    binop: fn(&String, &String) -> bool,
) -> Result<MithraVal> {
    match a {
        MithraVal::String(a) => match b {
            MithraVal::String(b) => {
                return Ok(MithraVal::Bool(binop(a, b)));
            }
            _ => {}
        },
        _ => {}
    }
    Err(anyhow!(type_err(
        0,
        &format!(""),
        &format!("(String, String)")
    )))
}

pub fn plus(args: Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("plus");
    let (a, b) = validate_binary_operation_args(args, &function_name, line_num)?;
    let int_result = int_numeric_binop(&a, &b, plus_impl);
    if int_result.is_ok() {
        return int_result;
    }
    let result = float_numeric_binop(&a, &b, plus_impl).map_err(|_| {
        anyhow!(type_err(
            line_num,
            &function_name,
            &format!("(float, float) or (int int)")
        ))
    })?;
    Ok(result)
}

pub fn minus(args: Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("minus");
    let (a, b) = validate_binary_operation_args(args, &function_name, line_num)?;
    let int_result = int_numeric_binop(&a, &b, minus_impl);
    if int_result.is_ok() {
        return int_result;
    }
    let result = float_numeric_binop(&a, &b, minus_impl).map_err(|_| {
        anyhow!(type_err(
            line_num,
            &function_name,
            &format!("(float, float) or (int int)")
        ))
    })?;
    Ok(result)
}

pub fn mul(args: Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("mul");
    let (a, b) = validate_binary_operation_args(args, &function_name, line_num)?;
    let int_result = int_numeric_binop(&a, &b, mul_impl);
    if int_result.is_ok() {
        return int_result;
    }
    let result = float_numeric_binop(&a, &b, mul_impl).map_err(|_| {
        anyhow!(type_err(
            line_num,
            &function_name,
            &format!("(float, float) or (int int)")
        ))
    })?;
    Ok(result)
}

pub fn div(args: Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("div");
    let (a, b) = validate_binary_operation_args(args, &function_name, line_num)?;
    let int_result = int_numeric_binop(&a, &b, div_impl);
    if int_result.is_ok() {
        return int_result;
    }
    let result = float_numeric_binop(&a, &b, div_impl).map_err(|_| {
        anyhow!(type_err(
            line_num,
            &function_name,
            &format!("(float, float) or (int int)")
        ))
    })?;
    Ok(result)
}

pub fn mod_(args: Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("mod");
    let (a, b) = validate_binary_operation_args(args, &function_name, line_num)?;
    let result = int_numeric_binop(&a, &b, mod_impl)
        .map_err(|_| anyhow!(type_err(line_num, &function_name, &format!("(int int)"))))?;
    Ok(result)
}

pub fn eqv(args: Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("eqv");
    let (a, b) = validate_binary_operation_args(args, &function_name, line_num)?;
    let string_result = string_bool_binop(&a, &b, eqv_impl);
    if string_result.is_ok() {
        return string_result;
    }
    let int_result = int_bool_binop(&a, &b, eqv_impl);
    if int_result.is_ok() {
        return int_result;
    }
    let result = float_bool_binop(&a, &b, eqv_impl).map_err(|_| {
        anyhow!(type_err(
            line_num,
            &function_name,
            &format!("(float, float) or (int int)")
        ))
    })?;
    Ok(result)
}

pub fn lt(args: Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("lt");
    let (a, b) = validate_binary_operation_args(args, &function_name, line_num)?;
    let string_result = string_bool_binop(&a, &b, lt_impl);
    if string_result.is_ok() {
        return string_result;
    }
    let int_result = int_bool_binop(&a, &b, lt_impl);
    if int_result.is_ok() {
        return int_result;
    }
    let result = float_bool_binop(&a, &b, lt_impl).map_err(|_| {
        anyhow!(type_err(
            line_num,
            &function_name,
            &format!("(float, float) or (int int)")
        ))
    })?;
    Ok(result)
}

pub fn gt(args: Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("gt");
    let (a, b) = validate_binary_operation_args(args, &function_name, line_num)?;
    let string_result = string_bool_binop(&a, &b, gt_impl);
    if string_result.is_ok() {
        return string_result;
    }
    let int_result = int_bool_binop(&a, &b, gt_impl);
    if int_result.is_ok() {
        return int_result;
    }
    let result = float_bool_binop(&a, &b, gt_impl).map_err(|_| {
        anyhow!(type_err(
            line_num,
            &function_name,
            &format!("(float, float) or (int int)")
        ))
    })?;
    Ok(result)
}

pub fn neqv(args: Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("neqv");
    let (a, b) = validate_binary_operation_args(args, &function_name, line_num)?;
    let string_result = string_bool_binop(&a, &b, neqv_impl);
    if string_result.is_ok() {
        return string_result;
    }
    let int_result = int_bool_binop(&a, &b, neqv_impl);
    if int_result.is_ok() {
        return int_result;
    }
    let result = float_bool_binop(&a, &b, neqv_impl).map_err(|_| {
        anyhow!(type_err(
            line_num,
            &function_name,
            &format!("(float, float) or (int int)")
        ))
    })?;
    Ok(result)
}

pub fn gte(args: Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("gte");
    let (a, b) = validate_binary_operation_args(args, &function_name, line_num)?;
    let string_result = string_bool_binop(&a, &b, gte_impl);
    if string_result.is_ok() {
        return string_result;
    }
    let int_result = int_bool_binop(&a, &b, gte_impl);
    if int_result.is_ok() {
        return int_result;
    }
    let result = float_bool_binop(&a, &b, gte_impl).map_err(|_| {
        anyhow!(type_err(
            line_num,
            &function_name,
            &format!("(float, float) or (int int)")
        ))
    })?;
    Ok(result)
}

pub fn lte(args: Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("lte");
    let (a, b) = validate_binary_operation_args(args, &function_name, line_num)?;
    let string_result = string_bool_binop(&a, &b, lte_impl);
    if string_result.is_ok() {
        return string_result;
    }
    let int_result = int_bool_binop(&a, &b, lte_impl);
    if int_result.is_ok() {
        return int_result;
    }
    let result = float_bool_binop(&a, &b, lte_impl).map_err(|_| {
        anyhow!(type_err(
            line_num,
            &function_name,
            &format!("(float, float) or (int int)")
        ))
    })?;
    Ok(result)
}

//primitives :: [(String, [LillaVal] -> ThrowsLillaError LillaVal)]
//primitives = [
//        ("plus", numericBinop (+)),
//        ("minus", numericBinop (-)),
//        ("mul", numericBinop (*)),
//        ("div", numericBinop div),
//        ("mod", numericBinop mod),
//        ("eqv", numBoolBinop (==)),
//        ("lt", numBoolBinop (<)),
//        ("gt", numBoolBinop (>)),
//        ("ne", numBoolBinop (/=)),
//        ("gte", numBoolBinop (>=)),
//        ("lte", numBoolBinop (<=)),
//        ("_eqv", strBoolBinop (==)),
//        ("_lt", strBoolBinop (<)),
//        ("_gt", strBoolBinop (>)),
//        ("_ne", strBoolBinop (/=)),
//        ("_gte", strBoolBinop (>=)),
//        ("_lte", strBoolBinop (<=)),
//        ("and", boolBoolBinop (&&)),
//        ("or", boolBoolBinop (||)),
//        ("head", head'),
//        ("tail", tail'),
//        ("cons", cons),
//        ("concat", conc),
//        ("replicate", repl),
//        ("length", length'),
//        ("toString", toString),
//        ("toNumber", toNumber),
//        ("take", take'),
//        ("generateList", generateList),
//        ("sum", sum'),
//        ("max", max'),
//        ("min", min'),
//        ("split", split),
//        ("not", not')
//    ]
