from enum import Enum
from comparisons import difference, equality
from maths import add_op, div_op, minus_op, mod_op, mult_op
from strings import concat_str
from vars import Variable, typify_script
from globals import case_sensitive, func_vars, global_vars, local_vars
from error import raise_err
from tokeniser import generate_line_tokens

class VarScope(Enum):
    Local = 0
    Function = 1 
    Global = 2

def register_var(var_name : str, val : Variable, scope : VarScope):
    if scope == VarScope.Global:
        if var_name in global_vars:
            raise_err(f"Err - attempted to redefine already defined variable: '{var_name}'")
        else:
            global_vars[var_name] = val
    elif scope == VarScope.Function:
        if var_name in func_vars:
            raise_err(f"Err - attempted to redefine already defined variable: '{var_name}'")
        else:
            func_vars[var_name] = val
    else:
        if var_name in local_vars:
            raise_err(f"Err - attempted to redefine already defined variable: '{var_name}'")
        else:
            local_vars[var_name] = val

    return None

operator_precedence : dict[str, int] = { 
    "+" : 4, 
    "-" : 4, 
    "*" : 3, 
    "/" : 3, 
    "%" : 3, 
    "==" : 7, 
    "!=" : 7, 
    ">" : 6, 
    ">=" : 6, 
    "<" : 6, 
    "<=" : 6, 
    "==?" : 8, 
    "==#" : 8, 
    ".." : 4, 
    "." : 4,
    #"!" : 8 -- TODO : implement these and reassess their precedence level
    #"?" : 8
    #":" : 8
}

def interpreter_eval(raw_script : list[str]) -> Variable:
    script : list[Variable] = typify_script(raw_script)
    operators : list[int] = []
    for expression in script:
        expression_val, expression_type = expression
        if expression_type == "op" and expression_val in operator_precedence:
            prec = operator_precedence[expression_val]
            if prec not in operators:
                operators.append(prec)
        elif expression_type == "expression":
            if 1 not in operators:
                operators.append(1)

    operators.sort()

    for op_lvl in operators:
        new_script : list[Variable] = []
        for e_index in range(len(script)):
            expression, expression_type = script[e_index]
            if expression_type == "op" and expression in operator_precedence:
                if operator_precedence[expression] == op_lvl:
                    result : Variable = ( 0, "int" )
                    # TODO - implement things like '- - - 1 = 2'
                    if e_index > 0:
                        left_item = script[e_index - 1]
                    else:
                        raise_err(f"Err - operation '{expression}' does not have a left side item.")
                        exit(1)
                    if e_index < len(script) - 1:
                        right_item = script[e_index + 1]
                    else:
                        raise_err(f"Err - operation '{expression}' does not have a right side item.")
                        exit(1)
                    if expression == "+":
                        result = add_op(left_item, right_item)
                    elif expression == "-":
                        result = minus_op(left_item, right_item)
                    elif expression == "*":
                        result = mult_op(left_item, right_item)
                    elif expression == "/":
                        result = div_op(left_item, right_item)
                    elif expression == "%":
                        result = mod_op(left_item, right_item)
                    elif expression == "==":
                        result = equality(left_item, right_item, case_sensitive = case_sensitive)
                    elif expression == "!=":
                        result = equality(left_item, right_item, True, case_sensitive = case_sensitive)
                    elif expression in [ "<", ">", "<=", ">=" ]:
                        result = difference(left_item, right_item, expression)
                    elif expression == "==?":
                        result = equality(left_item, right_item, case_sensitive=False)
                    elif expression == "==#":
                        result = equality(left_item, right_item, case_sensitive=True)
                    elif expression == "." or expression == "..":
                        result = concat_str(left_item, right_item)

                    new_script.append(result)
                new_script.append(( expression, "op" ))
            elif expression_type == "expression" and type(expression) == str:
                evaled_exp = interpreter_eval(generate_line_tokens(expression))
                if evaled_exp is not None:
                    result = evaled_exp
        script = new_script

    if len(script) > 1:
        raise_err("Err - failed to evalute script.")
    return script[0]

