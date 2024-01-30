use super::errors::incorrect_number_of_func_args_err;
use super::errors::type_err;
use crate::data::MithraVal;
use anyhow::anyhow;
use anyhow::Context;
use anyhow::Result;
use std::vec;

pub fn validate_num_binary_operation_args(
    args: Vec<MithraVal>,
    func_name: &String,
    line_num: usize,
) -> Result<(MithraVal, MithraVal)> {
    if args.len() != 2 {
        Err(anyhow!(incorrect_number_of_func_args_err(
            line_num,
            &func_name,
            2,
            args.len()
        )));
    }
    let first = *args.get(0)?;
    let second = *args.get(1)?;
    Ok((first, second))
}

pub fn plus_impl<T: std::ops::Add<Output = T>>(a: T, b: T) -> T {
    a + b
}

pub fn minus_impl<T: std::ops::Sub<Output = T>>(a: T, b: T) -> T {
    a - b
}

pub fn mul_impl<T: std::ops::Mul<Output = T>>(a: T, b: T) -> T {
    a * b
}

pub fn div_impl<T: std::ops::Div<Output = T>>(a: T, b: T) -> T {
    a / b
}

pub fn mod_impl(a: i64, b: i64) -> i64 {
    a % b
}

pub fn int_numeric_binop(
    a: MithraVal,
    b: MithraVal,
    binop: fn(i64, i64) -> i64,
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
    a: MithraVal,
    b: MithraVal,
    binop: fn(f64, f64) -> f64,
) -> Result<MithraVal> {
    match a {
        MithraVal::Float(a) => match b {
            MithraVal::Float(b) => {
                return Ok(MithraVal::Float(binop(a, b)));
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

pub fn plus(args: Vec<MithraVal>, line_num: usize, func_name: &String) -> Result<MithraVal> {
    let function_name = format!("plus");
    let (a, b) = validate_num_binary_operation_args(args, &function_name, line_num)?;
    let int_result = int_numeric_binop(a, b, plus_impl);
    if int_result.is_ok() {
        return int_result;
    }
    let result = float_numeric_binop(a, b, plus_impl).map_err(|_| {
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
    let (a, b) = validate_num_binary_operation_args(args, &function_name, line_num)?;
    let int_result = int_numeric_binop(a, b, minus_impl);
    if int_result.is_ok() {
        return int_result;
    }
    let result = float_numeric_binop(a, b, minus_impl).map_err(|_| {
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
    let (a, b) = validate_num_binary_operation_args(args, &function_name, line_num)?;
    let int_result = int_numeric_binop(a, b, mul_impl);
    if int_result.is_ok() {
        return int_result;
    }
    let result = float_numeric_binop(a, b, mul_impl).map_err(|_| {
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
    let (a, b) = validate_num_binary_operation_args(args, &function_name, line_num)?;
    let int_result = int_numeric_binop(a, b, div_impl);
    if int_result.is_ok() {
        return int_result;
    }
    let result = float_numeric_binop(a, b, div_impl).map_err(|_| {
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
    let (a, b) = validate_num_binary_operation_args(args, &function_name, line_num)?;
    let result = int_numeric_binop(a, b, mod_impl)
        .map_err(|_| anyhow!(type_err(line_num, &function_name, &format!("(int int)"))))?;
    Ok(result)
}


//primitives :: [(String, [LillaVal] -> ThrowsLillaError LillaVal)]
//primitives = [
///        ("plus", numericBinop (+)),
///        ("minus", numericBinop (-)),
///        ("mul", numericBinop (*)),
///        ("div", numericBinop div),
///        ("mod", numericBinop mod),
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
