from enum import Enum
from dicts import create_dict
from functions import execute_func, try_find_func
from globals import compiler_vars, local_vars, global_vars, functions, current_func_name
from typing import Callable, Tuple, cast
from error import raise_err
from numbers import parse_number
from interpreter import interpreter_eval
from lists import create_list
from strings import create_str
from tokeniser import special_tkns, keyword_tkns


class VarType(Enum):
    Expression = 0
    FunctionCall = 1 
    VarLoad = 2
    ListVal = 3
    DictVal = 4
    IntVal = 5
    StringVal = 6
    FloatVal = 7
    Operator = 8

type Variable = Tuple[ str | int | float | list[Variable] | dict[str, Variable], str ] # type: expression, list, dict, int, string, float, op
type VarList = dict[str, Variable]

# NOTE: still need to implement: ':h curly-brace-names'
# NOTE: still need to implement: ':h expr-lambda'

def try_find_variable(var_name : str) -> Variable | None:
    def fvar(vars : VarList) -> Variable | None:
        for item in vars.keys():
            if item == var_name:
                return compiler_vars[item]
        return None

    compiler_v = fvar(compiler_vars)
    if compiler_v == None:
        local_v = fvar(local_vars)
        if local_v == None:
            global_v = fvar(global_vars)
            if global_v == None:
                scope_v = fvar(functions[current_func_name].vars)
                if scope_v != None: 
                    return scope_v
                else:
                    return None
            else:
                return global_v
        else:
            return local_v
    else:
        return compiler_v


def typify_script(raw_script : list[str]) -> list[Variable]:
    result_script : list[Variable] = []
    for keyword in raw_script:
        if (keyword[0] == "\"" and keyword[-1] == "\"") or (keyword[0] == "'" and keyword[-1] == "'"):
            result_script.append(create_str(keyword))
        elif keyword[0] == '(' and keyword[-1] == ')':
            result_script.append(interpreter_eval([ keyword[1:-1] ]))
        elif keyword[0] == '[' and keyword[-1] == ']':
            result_script.append(create_list(keyword))
        elif keyword[0] == '{' and keyword[-1] == '}':
            result_script.append(create_dict(keyword))
        elif all(c.isalnum() or c == '_' for c in keyword) and not keyword[0].isnumeric():
            var_found = try_find_variable(keyword)
            if var_found != None:
                result_script.append(var_found)
            else:
                if keyword in keyword_tkns:
                    result_script.append((keyword, "keyword"))
                result_script.append(execute_func(keyword))
        elif keyword in special_tkns:
            result_script.append( (keyword, "op") )
        elif all(c.isnumeric() or c in [ '.', 'x', 'X', 'o', 'O', 'b', 'B' ] for c in keyword):
            result_script.append(parse_number(keyword))
        else:
            raise_err(f"Err - failed to identify type of token in script: {keyword}")

    return result_script


