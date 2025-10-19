
from typing import cast
from error import raise_err
from strings import coerce_str
from vars import Variable


def equality(left: Variable, right: Variable, negate : bool = False, case_sensitive : bool = True) -> Variable:
    result : bool = False
    left_val, left_type = left
    right_val, right_type = right
    
    if left_type in ["string", "int", "float"] and right_type in ["string", "int", "float"] and (right_type != left_type and right_type == "string"):
        if left_type == "string": 
            left_val, left_type = coerce_str(left)
        if right_type == "string": 
            right_val, right_type = coerce_str(right)
        result = right_val == left_val
    elif left_type == "string" and right_type == "string":
        if case_sensitive:
            return (1 if right_val == left_val else 0, "int")
        else:
            return (1 if cast(str, right_val).lower() == cast(str, left_val).lower() else 0, "int")
    elif left_type == "list":
        if right_type != "list":
            raise_err(f"Err - unable to compare '{right_type}' and '{left_type}'")
        result = right_val == left_val
    elif left_type == "dict":
        if right_type != "dict":
            raise_err(f"Err - unable to compare '{right_type}' and '{left_type}'")
        result = right_val == left_val
    else:
        raise_err(f"Err - unable to understand comparison '{right_type}' == '{left_type}'")

    if negate:
        return (0 if result else 1, "int")
    else:
        return (1 if result else 0, "int")


def difference(left: Variable, right: Variable, op : str) -> Variable:
    result : bool = False
    left_val, left_type = left
    right_val, right_type = right
    
    if left_type in ["string", "int", "float"] and right_type in ["string", "int", "float"]:
        if left_type == "string": 
            left_val, left_type = coerce_str(left)
        if right_type == "string": 
            right_val, right_type = coerce_str(right)
        if op == ">": result = cast(int, right_val) > cast(int, left_val)
        elif op == "<": result = cast(int, right_val) < cast(int, left_val)
        elif op == ">=": result = cast(int, right_val) >= cast(int, left_val)
        elif op == "<=": result = cast(int, right_val) <= cast(int, left_val)
    else:
        raise_err(f"Err - unable to perform comparison: '{right_type}' {op} '{left_type}'")

    return (1 if result else 0, "int")
