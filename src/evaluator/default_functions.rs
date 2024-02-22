#![allow(dead_code)]
use super::errors::incorrect_number_of_func_args_err;
use super::errors::type_err;
use super::errors::value_err;
use crate::data::MithraVal;
use anyhow::anyhow;
use anyhow::Result;

fn plus_impl<T: std::ops::Add<Output = T>>(a: T, b: T) -> T {
    a + b
}

fn minus_impl<T: std::ops::Sub<Output = T>>(a: T, b: T) -> T {
    a - b
}

fn mul_impl<T: std::ops::Mul<Output = T>>(a: T, b: T) -> T {
    a * b
}

fn div_impl<T: std::ops::Div<Output = T>>(a: T, b: T) -> T {
    a / b
}

fn mod_impl(a: i64, b: i64) -> i64 {
    a % b
}

fn eqv_impl<T: std::cmp::PartialEq>(a: &T, b: &T) -> bool {
    *a == *b
}

fn lt_impl<T: std::cmp::PartialOrd>(a: &T, b: &T) -> bool {
    *a < *b
}

fn gt_impl<T: std::cmp::PartialOrd>(a: &T, b: &T) -> bool {
    *a > *b
}

fn neqv_impl<T: std::cmp::PartialOrd>(a: &T, b: &T) -> bool {
    *a != *b
}

fn gte_impl<T: std::cmp::PartialOrd>(a: &T, b: &T) -> bool {
    *a >= *b
}

fn lte_impl<T: std::cmp::PartialOrd>(a: &T, b: &T) -> bool {
    *a <= *b
}

fn and_impl(a: bool, b: bool) -> bool {
    a && b
}

fn or_impl(a: bool, b: bool) -> bool {
    a || b
}

fn validate_single_arg<'a>(
    args: &'a Vec<MithraVal>,
    func_name: &'a String,
    line_num: usize,
) -> Result<&'a MithraVal> {
    if args.len() != 1 {
        return Err(anyhow!(incorrect_number_of_func_args_err(
            line_num,
            &func_name,
            1,
            args.len()
        )));
    }
    let first = args.get(0).unwrap();
    Ok(first)
}

fn validate_two_args<'a>(
    args: &'a Vec<MithraVal>,
    func_name: &'a String,
    line_num: usize,
) -> Result<(&'a MithraVal, &'a MithraVal)> {
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
    Ok((first, second))
}

fn int_numeric_binop(
    a: &MithraVal,
    b: &MithraVal,
    binop: fn(i64, i64) -> i64,
) -> Result<MithraVal> {
    match a {
        MithraVal::Int(a) => match b {
            MithraVal::Int(b) => {
                return Ok(MithraVal::Int(binop(*a, *b)));
            }
            _ => {}
        },
        _ => {}
    }
    Err(anyhow!(type_err(0, &format!(""), &format!("(int, int)"))))
}

fn float_numeric_binop(
    a: &MithraVal,
    b: &MithraVal,
    binop: fn(f64, f64) -> f64,
) -> Result<MithraVal> {
    match a {
        MithraVal::Float(a) => match b {
            MithraVal::Float(b) => {
                return Ok(MithraVal::Float(binop(*a, *b)));
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

fn int_bool_binop(
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

fn float_bool_binop(
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

fn string_bool_binop(
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

fn bool_bool_binop(
    a: &MithraVal,
    b: &MithraVal,
    binop: fn(bool, bool) -> bool,
) -> Result<MithraVal> {
    match a {
        MithraVal::Bool(a) => match b {
            MithraVal::Bool(b) => {
                return Ok(MithraVal::Bool(binop(*a, *b)));
            }
            _ => {}
        },
        _ => {}
    }
    Err(anyhow!(type_err(0, &format!(""), &format!("(bool, bool)"))))
}

pub fn plus(args: &Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("plus");
    let (a, b) = validate_two_args(args, &function_name, line_num)?;
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

pub fn minus(args: &Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("minus");
    let (a, b) = validate_two_args(args, &function_name, line_num)?;
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

pub fn mul(args: &Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("mul");
    let (a, b) = validate_two_args(args, &function_name, line_num)?;
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

pub fn div(args: &Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("div");
    let (a, b) = validate_two_args(args, &function_name, line_num)?;
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

pub fn mod_(args: &Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("mod");
    let (a, b) = validate_two_args(args, &function_name, line_num)?;
    let result = int_numeric_binop(&a, &b, mod_impl)
        .map_err(|_| anyhow!(type_err(line_num, &function_name, &format!("(int int)"))))?;
    Ok(result)
}

pub fn eqv(args: &Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("eqv");
    let (a, b) = validate_two_args(args, &function_name, line_num)?;
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
            &format!("(float, float) or (int int) or (Strin, String)")
        ))
    })?;
    Ok(result)
}

pub fn lt(args: &Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("lt");
    let (a, b) = validate_two_args(args, &function_name, line_num)?;
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
            &format!("(float, float) or (int int) or (String, String)")
        ))
    })?;
    Ok(result)
}

pub fn gt(args: &Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("gt");
    let (a, b) = validate_two_args(args, &function_name, line_num)?;
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
            &format!("(float, float) or (int int) or (String, String)")
        ))
    })?;
    Ok(result)
}

pub fn neqv(args: &Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("neqv");
    let (a, b) = validate_two_args(args, &function_name, line_num)?;
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
            &format!("(float, float) or (int int) or (String, String)")
        ))
    })?;
    Ok(result)
}

pub fn gte(args: &Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("gte");
    let (a, b) = validate_two_args(args, &function_name, line_num)?;
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
            &format!("(float, float) or (int int) or (String, String)")
        ))
    })?;
    Ok(result)
}

pub fn lte(args: &Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("lte");
    let (a, b) = validate_two_args(args, &function_name, line_num)?;
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
            &format!("(float, float) or (int int) or (String, String)")
        ))
    })?;
    Ok(result)
}

pub fn or(args: &Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("or");
    let (a, b) = validate_two_args(args, &function_name, line_num)?;
    bool_bool_binop(&a, &b, or_impl)
}

pub fn and(args: &Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("or");
    let (a, b) = validate_two_args(args, &function_name, line_num)?;
    bool_bool_binop(&a, &b, and_impl)
}

pub fn not(args: &Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("or");
    let arg = validate_single_arg(args, &function_name, line_num)?;
    match arg {
        MithraVal::Bool(x) => Ok(MithraVal::Bool(!x)),
        _ => Err(anyhow!(type_err(
            line_num,
            &function_name,
            &format!("(bool)")
        ))),
    }
}

pub fn head(args: &Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("head");
    let arg = validate_single_arg(args, &function_name, line_num)?;
    match arg {
        MithraVal::List(_, list_vals) => match list_vals.get(0) {
            Some(val) => Ok(val.clone()),
            None => Err(anyhow!(value_err(
                line_num,
                format!("no head of empty list")
            ))),
        },
        _ => Err(anyhow!(type_err(
            line_num,
            &function_name,
            &format!("(List)")
        ))),
    }
}

pub fn tail(args: &Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("tail");
    let arg = validate_single_arg(args, &function_name, line_num)?;
    match arg {
        MithraVal::List(_, list_vals) => Ok(MithraVal::List(line_num, (&list_vals[1..]).to_vec())),
        _ => Err(anyhow!(type_err(
            line_num,
            &function_name,
            &format!("(List)")
        ))),
    }
}

pub fn concat(args: &Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("concat");
    let (list1, list2) = validate_two_args(args, &function_name, line_num)?;
    match list1 {
        MithraVal::List(_, list1_vals) => match list2 {
            MithraVal::List(_, list2_vals) => {
                let mut concatenated = list1_vals.clone();
                concatenated.extend(list2_vals.clone());
                return Ok(MithraVal::List(line_num, concatenated));
            }
            _ => {}
        },
        _ => {}
    }
    Err(anyhow!(type_err(
        line_num,
        &function_name,
        &format!("(List, List)")
    )))
}

pub fn length(args: &Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("length");
    let arg = validate_single_arg(args, &function_name, line_num)?;
    match arg {
        MithraVal::List(_, list_vals) => Ok(MithraVal::Int(list_vals.len() as i64)),
        _ => Err(anyhow!(type_err(
            line_num,
            &function_name,
            &format!("(List)")
        ))),
    }
}

pub fn to_string(args: &Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("to_string");
    let arg = validate_single_arg(args, &function_name, line_num)?;
    match arg {
        MithraVal::Int(val) => Ok(MithraVal::String(format!("{}", val))),
        MithraVal::Float(val) => Ok(MithraVal::String(format!("{}", val))),
        _ => Err(anyhow!(type_err(
            line_num,
            &function_name,
            &format!("(float) or (int)")
        ))),
    }
}

pub fn to_int(args: &Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("to_string");
    let arg = validate_single_arg(args, &function_name, line_num)?;
    match arg {
        MithraVal::String(string_val) => match string_val.parse::<i64>() {
            Ok(int_val) => Ok(MithraVal::Int(int_val)),
            Err(_) => Err(anyhow!(value_err(
                line_num,
                format!("can't convert {} to int", string_val)
            ))),
        },
        _ => Err(anyhow!(type_err(
            line_num,
            &function_name,
            &format!("(float) or (int)")
        ))),
    }
}

pub fn take(args: &Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("take");
    let (int, list) = validate_two_args(args, &function_name, line_num)?;
    match int {
        MithraVal::Int(n) => match list {
            MithraVal::List(_, list_vals) => {
                return Ok(MithraVal::List(
                    line_num,
                    (&list_vals[0..*n as usize]).to_vec(),
                ));
            }
            _ => {}
        },
        _ => {}
    }
    Err(anyhow!(type_err(
        line_num,
        &function_name,
        &format!("(int, list)")
    )))
}

pub fn range(args: &Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("range");
    let (start, end) = validate_two_args(args, &function_name, line_num)?;
    match start {
        MithraVal::Int(start_int) => match end {
            MithraVal::Int(end_int) => {
                return Ok(MithraVal::List(
                    line_num,
                    (*start_int..=*end_int).map(|x| MithraVal::Int(x)).collect(),
                ));
            }
            _ => {}
        },
        _ => {}
    }
    Err(anyhow!(type_err(
        line_num,
        &function_name,
        &format!("(int, int)")
    )))
}

pub fn split(args: &Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("split");
    let (string, split_string) = validate_two_args(args, &function_name, line_num)?;
    match string {
        MithraVal::String(string_val) => match split_string {
            MithraVal::String(split_string_val) => {
                return Ok(MithraVal::List(
                    line_num,
                    string_val
                        .split(split_string_val)
                        .map(|x| MithraVal::String(x.to_string()))
                        .collect(),
                ));
            }
            _ => {}
        },
        _ => {}
    }
    Err(anyhow!(type_err(
        line_num,
        &function_name,
        &format!("(int, int)")
    )))
}

pub fn cons(args: &Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
    let function_name = format!("cons");
    let (head, tail) = validate_two_args(args, &function_name, line_num)?;

    fn cons_impl(head: &MithraVal, tail: &Vec<MithraVal>, line_num: usize) -> Result<MithraVal> {
        let mut list_values = tail.clone();
        list_values.insert(0, head.clone());
        Ok(MithraVal::List(line_num, list_values))
    }

    match tail {
        MithraVal::List(_, tail_list) => match head {
            MithraVal::Null => {
                return cons_impl(head, tail_list, line_num);
            }
            MithraVal::Bool(_) => {
                return cons_impl(head, tail_list, line_num);
            }
            MithraVal::Int(_) => {
                return cons_impl(head, tail_list, line_num);
            }
            MithraVal::Float(_) => {
                return cons_impl(head, tail_list, line_num);
            }
            MithraVal::String(_) => {
                return cons_impl(head, tail_list, line_num);
            }
            MithraVal::List(_, _) => {
                return cons_impl(head, tail_list, line_num);
            }
            MithraVal::Dict(_, _) => {
                return cons_impl(head, tail_list, line_num);
            }
            _ => {}
        },
        _ => {}
    }
    Err(anyhow!(type_err(
        line_num,
        &function_name,
        &format!("(None | bool | int | float | String | List | Dict, List)")
    )))
}
