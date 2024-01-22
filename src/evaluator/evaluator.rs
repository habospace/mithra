use crate::data::Function;
use crate::data::MithraError;
use crate::data::MithraVal;
use crate::data::Pointer;
use std::collections::BTreeMap;

type Memory = BTreeMap<String, MithraVal>;

enum RunContext {
    Function,
    Global,
}

struct Program {
    exprs: Vec<MithraVal>,
    expr_pointer: Pointer,
    memory: Memory,
}

impl Program {
    pub fn get_next(&mut self) -> Option<MithraVal> {
        match self.exprs.get(self.expr_pointer) {
            Some(expr) => {
                self.expr_pointer += 1;
                Some(expr.clone())
            }
            None => None,
        }
    }
    pub fn set_env(&mut self, key: String, value: MithraVal) -> Option<MithraVal> {
        self.memory.insert(key, value)
    }
    pub fn get_env(&self, key: &String) -> Option<&MithraVal> {
        self.memory.get(key)
    }
}

fn run(run_context: RunContext, program: &mut Program) -> Result<MithraVal, MithraError> {
    while let Some(expr) = program.get_next() {
        match run_context {
            RunContext::Function => match maybe_eval_return_expr(&expr, program)? {
                Some(return_val) => return Ok(return_val),
                None => {
                    eval(expr, program)?;
                }
            },
            RunContext::Global => {
                eval(expr, program)?;
            }
        }
    }
    Ok(MithraVal::Null)
}

fn maybe_eval_return_expr(
    expr: &MithraVal,
    program: &mut Program,
) -> Result<Option<MithraVal>, MithraError> {
    Ok(None)
}

fn eval(expr: MithraVal, program: &mut Program) -> Result<(), MithraError> {
    Ok(())
}
