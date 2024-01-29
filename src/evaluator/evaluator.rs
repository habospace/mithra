use crate::data::Function;
use crate::data::FunctionName;
use crate::data::MithraError;
use crate::data::MithraVal;
use crate::data::Pointer;
use anyhow::anyhow;
use anyhow::Context;
use anyhow::Result;
use std::collections::BTreeMap;

use super::errors::cant_return_in_global_scope_err;
use super::errors::incorrect_number_of_func_args_err;
use super::errors::undefined_variable_err;
use super::errors::variable_is_not_callable_err;

type Memory = BTreeMap<String, MithraVal>;
type DefaultFunction = fn(Vec<MithraVal>) -> Result<MithraVal, MithraError>;

#[derive(Clone)]
enum Scope {
    Function,
    Global,
}

struct Program {
    exprs: Vec<MithraVal>,
    expr_pointer: Pointer,
    memory: Memory,
    default_funcs: BTreeMap<FunctionName, DefaultFunction>,
}

impl Program {
    pub fn new(exprs: Vec<MithraVal>) -> Program {
        Program {
            exprs: exprs,
            expr_pointer: 0,
            memory: BTreeMap::new(),
            default_funcs: BTreeMap::from([]),
        }
    }
    pub fn next_expr(&mut self) -> Option<MithraVal> {
        match self.exprs.get(self.expr_pointer) {
            Some(expr) => {
                self.expr_pointer += 1;
                Some(expr.clone())
            }
            None => None,
        }
    }
    pub fn set_memory(&mut self, key: String, value: MithraVal) {
        self.memory.insert(key, value);
    }
    pub fn get_memory(&self, key: &String) -> Option<MithraVal> {
        match self.memory.get(key) {
            Some(expr) => Some(expr.clone()),
            None => None,
        }
    }
    pub fn get_default_func(&mut self, func_name: &FunctionName) -> Option<&DefaultFunction> {
        self.default_funcs.get(func_name)
    }
    pub fn transfer_memory(&mut self, exprs: Vec<MithraVal>) -> Program {
        Program {
            exprs: exprs,
            expr_pointer: 0,
            memory: self.memory.clone(),
            default_funcs: self.default_funcs.clone(),
        }
    }
}

fn run(scope: Scope, program: &mut Program) -> Result<MithraVal> {
    while let Some(expr) = program.next_expr() {
        match scope {
            Scope::Function => match maybe_eval_return_expr(expr.clone(), program)? {
                Some(return_val) => return Ok(return_val),
                None => {
                    eval(expr, program, scope.clone())?;
                }
            },
            Scope::Global => {
                eval(expr, program, scope.clone())?;
            }
        }
    }
    Ok(MithraVal::Null)
}

fn maybe_eval_return_expr(expr: MithraVal, program: &mut Program) -> Result<Option<MithraVal>> {
    match expr {
        MithraVal::ReturnStatement(_, expr) => {
            Ok(Some(eval(*expr, program, Scope::Function)?))
        }
        _ => Ok(None),
    }
}

fn eval(expr: MithraVal, program: &mut Program, scope: Scope) -> Result<MithraVal> {
    match expr {
        MithraVal::Pass => Ok(MithraVal::Pass),
        MithraVal::Null => Ok(MithraVal::Null),
        MithraVal::Int(val) => Ok(MithraVal::Int(val)),
        MithraVal::Float(val) => Ok(MithraVal::Float(val)),
        MithraVal::Char(val) => Ok(MithraVal::Char(val)),
        MithraVal::String(val) => Ok(MithraVal::String(val)),
        MithraVal::Bool(val) => Ok(MithraVal::Bool(val)),
        MithraVal::List(line_num, list_exprs) => {
            let mut evaluated_exprs = Vec::new();
            for expr_ in list_exprs {
                evaluated_exprs.push(eval(expr_, program, scope.clone())?);
            }
            Ok(MithraVal::List(line_num, evaluated_exprs))
        }
        MithraVal::Dict(line_num, kv_pairs) => {
            let mut evaluated_kv_pairs = BTreeMap::new();
            for (k, v) in kv_pairs.iter() {
                evaluated_kv_pairs.insert(k.clone(), eval(v.clone(), program, scope.clone())?);
            }
            Ok(MithraVal::Dict(line_num, evaluated_kv_pairs))
        }
        MithraVal::Variable(line_num, var_name) => Ok(program
            .get_memory(&var_name)
            .ok_or(undefined_variable_err(line_num, &var_name))?),
        MithraVal::Assignment(_, var_name, expr) => {
            let evaluated = eval(*expr, program, scope)?;
            program.set_memory(var_name, evaluated.clone());
            Ok(evaluated)
        }
        MithraVal::ReturnStatement(line_num, expr) => match scope {
            Scope::Global => Err(anyhow!(cant_return_in_global_scope_err(line_num))),
            Scope::Function => Ok(eval(*expr, program, scope)?),
        },
        MithraVal::Function(line_num, Function { name, args, exprs }) => {
            program.set_memory(
                name.clone(),
                MithraVal::Function(line_num, Function { name, args, exprs }),
            );
            Ok(MithraVal::Pass)
        }
        MithraVal::IfBlock(_, predicate_expr, if_exprs) => {
            let evaluated_pred = eval(*predicate_expr, program, scope.clone())?;
            match evaluated_pred {
                MithraVal::Bool(true) => run(scope, &mut program.transfer_memory(if_exprs.clone())),
                _ => Ok(MithraVal::Pass),
            }
        }
        MithraVal::IfElseBlock(_, predicate_expr, if_exprs, else_exprs) => {
            let chosen_exprs = match eval(*predicate_expr, program, scope.clone())? {
                MithraVal::Bool(true) => if_exprs.clone(),
                _ => else_exprs.clone(),
            };
            run(scope, &mut program.transfer_memory(chosen_exprs))
        }
        MithraVal::FunctionCall(line_num, func_name, call_args) => {
            match program.get_default_func(&func_name) {
                Some(function) => Ok(function(call_args)?),
                None => {
                    let maybe_function = program
                        .get_memory(&func_name)
                        .ok_or(undefined_variable_err(line_num, &func_name))?;
                    match maybe_function {
                        MithraVal::Function(line_num, function) => {
                            let n_correct_args = function.args.len();
                            let n_call_args = call_args.len();
                            if n_correct_args != n_call_args {
                                Err(anyhow!(incorrect_number_of_func_args_err(
                                    line_num,
                                    &func_name,
                                    n_correct_args,
                                    n_call_args
                                )))
                            } else {
                                eval_user_defined_func(function, call_args, program)
                            }
                        }
                        _ => Err(anyhow!(variable_is_not_callable_err(line_num, &func_name))),
                    }
                }
            }
        }
    }
}

fn eval_user_defined_func(
    function: Function,
    call_args: Vec<MithraVal>,
    program: &mut Program,
) -> Result<MithraVal> {
    let mut function_execution = program.transfer_memory(function.exprs);
    for (var_name, var_value) in function.args.iter().zip(call_args.iter()) {
        function_execution.set_memory(var_name.to_string(), var_value.clone());
    }
    run(Scope::Function, &mut function_execution)
}
