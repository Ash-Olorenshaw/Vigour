from error import raise_err, raise_warn
from globals import Function, functions
from vars import Variable

def try_find_func(func_name : str) -> Function:
    # todo - lookup global/builtin funcs
    for func in functions:
        if func == func_name:
            return functions[func_name]
    raise_err(f"Err - failed to find function: '{func_name}'")
    return Function({}, [])

def execute_func(target_func_name : str) -> Variable:
    target_func = try_find_func(target_func_name)
    if len(target_func.lines) > 0: 
    else:
        raise_warn("function has no body.")

