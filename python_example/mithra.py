from typing import TypeVar, Callable
from dataclasses import dataclass
from collections.abc import Iterable


@dataclass
class Text:
    chars: str
    pointer: int = 0

    def get_next(self) -> str | None:
        try:
            return self.chars[self.pointer]
        except IndexError:
            return None
        finally:
            self.pointer += 1

    def decr_pointer(self) -> None:
        self.pointer -= 1


T = TypeVar("T")


Parser = Callable[[Text], T | None]


def run_parser(parser_f: Parser[T]) -> Callable[[Text], T | None]:
    def wrapper(t: Text) -> T | None:
        before_pointer = t.pointer
        if (result := parser_f(t)) is None:
            t.pointer = before_pointer
        return result

    return wrapper


@dataclass
class Function:
    name: str
    args: list[str]
    exprs: list["MithraValue"]


@dataclass
class FunctionCall:
    name: str
    call_args: list["MithraValue"]


@dataclass
class Assignment:
    var_name: str
    expr: "MithraValue"


@dataclass
class Variable:
    name: str


@dataclass
class MithraValue:
    val: int \
       | str \
       | list[int | str] \
       | Function \
       | FunctionCall \
       | Assignment


@run_parser
def parse_int(t: Text) -> int | None:
    int_builder = ""
    while char := t.get_next():
        if not char.isdigit():
            t.decr_pointer()
            break
        int_builder += char
    return int(int_builder) if int_builder else None


@run_parser
def parse_string(t: Text) -> str | None:
    if t.get_next() != '"':
        return None
    string_builder = ""
    while char := t.get_next():
        if char == '"':
            break
        string_builder += char
    return string_builder


@run_parser
def word(t: Text) -> str | None:
    word_builder = ""
    while char := t.get_next():
        if not char.isalpha():
            t.decr_pointer()
            break
        word_builder += char
    return word_builder or None


def parse_variable(t: Text) -> Variable | None:
    if (var_name := word(t)) is None:
        return None
    return Variable(var_name)


@run_parser
def parse_expr(t: Text) -> MithraValue | None:
    for parser in [parse_int, parse_string, parse_function_call, parse_variable]:
        if result := parser(t):
            return MithraValue(val=result)
    return None


def create_string_parser(string: str) -> Parser[str]:
    @run_parser
    def parser(t: Text) -> str | None:
        for char in string:
            if char != t.get_next():
                return None
        return string

    return parser


T1 = TypeVar("T1")
T2 = TypeVar("T2")


def sep_by(main_parser: Parser[T1], sep_parser: Parser[T2]) -> Parser[list[T1]]:
    @run_parser
    def parser(t: Text) -> list[T1] | None:
        if (first := main_parser(t)) is None:
            return None
        results = [first]
        while True:
            if not sep_parser(t):
                break
            if next := main_parser(t):
                results.append(next)
        return results

    return parser


@run_parser
def parse_function_call(t: Text) -> FunctionCall | None:
    if (function_name := word(t)) is None:
        return None
    if t.get_next() != "(":
        return None
    comma_parser = create_string_parser(", ")
    if (
        call_args := sep_by(main_parser=parse_expr, sep_parser=comma_parser)(t)
    ) is None:
        return None
    if t.get_next() != ")":
        return None
    return FunctionCall(function_name, call_args)


@run_parser
def parse_assignment(t: Text) -> Assignment | None:
    if (var_name := word(t)) is None:
        return None
    assignment_parser = create_string_parser(" = ")
    if assignment_parser(t) is None:
        return None
    if (expr := parse_expr(t)) is None:
        return None
    return Assignment(var_name, expr)


code = """x = 5
y = add(add(x, 2), add(1, add(3, 4)))
z = add(y, 5)
"""

exprs = [parse_assignment(Text(line)) for line in code.splitlines()]


VarName = FunctionName = str
MithraFunction = Callable[[MithraValue, MithraValue], MithraValue]


class Program:

    memory: dict[VarName, MithraValue] = {}
    default_functions: dict[FunctionName, MithraFunction] = {
        "add": lambda x, y: MithraValue(x.val + y.val)
    }

    def run(self, exprs: Iterable[MithraValue]) -> MithraValue | None:
        evaluated: MithraValue | None = None
        for expr in exprs:
            evaluated = self.eval(expr)
        return evaluated

    def eval(self, expr: MithraValue) -> MithraValue:
        val = var = f_call = assignment = expr.val
        if isinstance(val, Variable):
            return self.eval(self.memory[var.name])
        elif isinstance(val, int):
            return expr
        elif isinstance(val, str):
            return expr
        elif isinstance(val, FunctionCall):
            evaluated_args = [self.eval(arg) for arg in f_call.call_args]
            function = self.default_functions[f_call.name]
            return function(*evaluated_args)
        else:  # Assignment
            evaluated_expr = self.eval(assignment.expr)
            self.memory[assignment.var_name] = evaluated_expr
            return evaluated_expr


# breakpoint()
program = Program()
program.run(map(MithraValue, exprs))
print(program.memory)
