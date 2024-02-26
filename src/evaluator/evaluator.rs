#![allow(dead_code)]
use crate::data::Function;
use crate::data::FunctionName;
use crate::data::LineNum;

use crate::data::MithraVal;

use anyhow::anyhow;

use anyhow::Result;
use std::collections::BTreeMap;
use std::collections::HashMap;

use super::errors::cant_return_in_global_scope_err;
use super::errors::incorrect_number_of_func_args_err;
use super::errors::undefined_variable_err;
use super::errors::variable_is_not_callable_err;

use super::default_functions::and;
use super::default_functions::concat;
use super::default_functions::cons;
use super::default_functions::div;
use super::default_functions::eqv;
use super::default_functions::gt;
use super::default_functions::gte;
use super::default_functions::head;
use super::default_functions::length;
use super::default_functions::lt;
use super::default_functions::lte;
use super::default_functions::minus;
use super::default_functions::mod_;
use super::default_functions::mul;
use super::default_functions::neqv;
use super::default_functions::not;
use super::default_functions::or;
use super::default_functions::plus;
use super::default_functions::range;
use super::default_functions::split;
use super::default_functions::tail;
use super::default_functions::to_int;
use super::default_functions::to_string;

type DefaultFunction = fn(&Vec<MithraVal>, LineNum) -> Result<MithraVal>;

#[derive(Clone, Debug)]
pub enum Scope {
    Function,
    Global,
}

// TODO: rename this as Interpreter
pub struct Program {
    scope: Scope,
    pub memory: HashMap<String, MithraVal>,
    default_funcs: HashMap<FunctionName, DefaultFunction>,
}

impl Program {
    pub fn new(scope: Scope) -> Program {
        Program {
            scope,
            memory: HashMap::new(),
            default_funcs: HashMap::from([
                (format!("plus"), plus as DefaultFunction),
                (format!("minus"), minus as DefaultFunction),
                (format!("mul"), mul as DefaultFunction),
                (format!("div"), div as DefaultFunction),
                (format!("mod"), mod_ as DefaultFunction),
                (format!("eqv"), eqv as DefaultFunction),
                (format!("neqv"), neqv as DefaultFunction),
                (format!("lt"), lt as DefaultFunction),
                (format!("gt"), gt as DefaultFunction),
                (format!("gte"), gte as DefaultFunction),
                (format!("lte"), lte as DefaultFunction),
                (format!("or"), or as DefaultFunction),
                (format!("and"), and as DefaultFunction),
                (format!("not"), not as DefaultFunction),
                (format!("head"), head as DefaultFunction),
                (format!("tail"), tail as DefaultFunction),
                (format!("concat"), concat as DefaultFunction),
                (format!("len"), length as DefaultFunction),
                (format!("to_string"), to_string as DefaultFunction),
                (format!("to_int"), to_int as DefaultFunction),
                (format!("range"), range as DefaultFunction),
                (format!("split"), split as DefaultFunction),
                (format!("cons"), cons as DefaultFunction),
            ]),
        }
    }
    fn set_memory(&mut self, key: String, value: MithraVal) {
        self.memory.insert(key, value);
    }
    fn get_memory(&self, key: &String) -> Option<MithraVal> {
        match self.memory.get(key) {
            Some(expr) => Some(expr.clone()),
            None => None,
        }
    }
    fn get_default_func(&mut self, func_name: &FunctionName) -> Option<&DefaultFunction> {
        self.default_funcs.get(func_name)
    }
    fn integrate_memory(&mut self, other_program: &Program) {
        for (var_name, var_value) in other_program.memory.iter() {
            self.memory.insert(var_name.to_string(), var_value.clone());
        }
    }
    pub fn run(&mut self, exprs: &Vec<MithraVal>) -> Result<MithraVal> {
        match self.scope {
            Scope::Function => self.run_impl(exprs, true),
            Scope::Global => self.run_impl(exprs, false),
        }
    }
    fn run_impl(&mut self, exprs: &Vec<MithraVal>, function_top_level: bool) -> Result<MithraVal> {
        let mut last_evaluated_expr: MithraVal = MithraVal::Null;
        for expr in exprs {
            let evaluated = self.eval(expr.clone())?;
            last_evaluated_expr = evaluated.clone();
            match self.scope {
                Scope::Function => match evaluated {
                    MithraVal::ReturnStatement(line_num, evaluated) => {
                        if function_top_level {
                            return Ok(*evaluated);
                        } else {
                            return Ok(MithraVal::ReturnStatement(line_num, evaluated));
                        }
                    }
                    _ => {}
                },
                Scope::Global => match evaluated {
                    MithraVal::ReturnStatement(line_num, _) => {
                        return Err(anyhow!(cant_return_in_global_scope_err(line_num)));
                    }
                    _ => {}
                },
            }
        }
        Ok(last_evaluated_expr)
    }
    fn eval(&mut self, expr: MithraVal) -> Result<MithraVal> {
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
                for list_item_expr in list_exprs {
                    evaluated_exprs.push(self.eval(list_item_expr)?);
                }
                Ok(MithraVal::List(line_num, evaluated_exprs))
            }
            MithraVal::Dict(line_num, kv_pairs) => {
                let mut evaluated_kv_pairs = BTreeMap::new();
                for (k, v) in kv_pairs.iter() {
                    evaluated_kv_pairs.insert(k.clone(), self.eval(v.clone())?);
                }
                Ok(MithraVal::Dict(line_num, evaluated_kv_pairs))
            }
            MithraVal::Variable(line_num, var_name) => Ok(self
                .get_memory(&var_name)
                .ok_or(undefined_variable_err(line_num, &var_name))?),
            MithraVal::Assignment(_, var_name, expr) => {
                let evaluated = self.eval(*expr)?;
                self.set_memory(var_name, evaluated.clone());
                Ok(MithraVal::Pass)
            }
            MithraVal::ReturnStatement(line_num, expr) => {
                let evaluated = self.eval(*expr)?;
                Ok(MithraVal::ReturnStatement(line_num, Box::new(evaluated)))
            }
            MithraVal::Function(line_num, Function { name, args, exprs }) => {
                self.set_memory(
                    name.clone(),
                    MithraVal::Function(line_num, Function { name, args, exprs }),
                );
                Ok(MithraVal::Pass)
            }
            MithraVal::IfBlock(_, predicate_expr, if_exprs) => {
                let evaluated_pred = self.eval(*predicate_expr)?;
                match evaluated_pred {
                    MithraVal::Bool(true) => {
                        let executed = self.run_impl(&if_exprs, false)?;
                        Ok(executed)
                    }

                    _ => Ok(MithraVal::Pass),
                }
            }
            MithraVal::IfElseBlock(_, predicate_expr, if_exprs, else_exprs) => {
                let chosen_exprs = match self.eval(*predicate_expr)? {
                    MithraVal::Bool(true) => if_exprs.clone(),
                    _ => else_exprs.clone(),
                };
                let executed = self.run_impl(&chosen_exprs, false)?;
                Ok(executed)
            }
            MithraVal::FunctionCall(line_num, func_name, call_args) => {
                let mut evaluated_call_args: Vec<MithraVal> = Vec::new();
                for arg in &call_args {
                    let evaluated = self.eval(arg.clone())?;
                    evaluated_call_args.push(evaluated);
                }
                match self.get_default_func(&func_name) {
                    Some(function) => Ok(function(&evaluated_call_args, line_num)?),
                    None => {
                        let maybe_function = self
                            .get_memory(&func_name)
                            .ok_or(undefined_variable_err(line_num, &func_name))?;
                        match maybe_function {
                            MithraVal::Function(line_num, function) => {
                                let n_correct_args = function.args.len();
                                let n_call_args = call_args.len();
                                if n_correct_args != n_call_args {
                                    // TODO: we should raise this error as early as possible
                                    Err(anyhow!(incorrect_number_of_func_args_err(
                                        line_num,
                                        &func_name,
                                        n_correct_args,
                                        n_call_args
                                    )))
                                } else {
                                    self.eval_user_defined_func(function, evaluated_call_args)
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
        &mut self,
        function: Function,
        call_args: Vec<MithraVal>,
    ) -> Result<MithraVal> {
        let mut function_program = Program::new(Scope::Function);
        function_program.integrate_memory(self);
        for (var_name, var_value) in function.args.iter().zip(call_args.iter()) {
            function_program.set_memory(var_name.to_string(), var_value.clone());
        }
        function_program.run(&function.exprs)
    }
}
