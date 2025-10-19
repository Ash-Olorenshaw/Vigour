from typing import cast
from error import raise_err
from strings import coerce_str
from vars import Variable

def add_op(left: Variable, right: Variable) -> Variable:
    result = (0, "int")
    left_val, left_type = left
    right_val, right_type = right
    if left_type == "list":
        if right_type != "list":
            raise_err(f"Err - unable to perform operation '+' between types {left_type} and {right_type}")
        else:
            result = (cast(list, left_val) + cast(list, right_val), "list")
    elif left_type == "dict" or left_type == "expression":
        raise_err(f"Err - unable to perform operation '+' on {left_type}.")
    elif left_type == "int" or left_type == "float":
        if right_type == "string":
            right_val, right_type = coerce_str(right)
        elif right_type != "int" and right_type != "float": 
            raise_err(f"Err - unable to perform operation '+' between '{left_type}' and '{right_type}'")
        result_type = "float" if right_type == "float" or left_type == "float" else "int"
        result = (cast(int, left_val) + cast(int, right_val), result_type)
    elif left_type == "string":
        if right_type != "int" and right_type != "float": 
            raise_err(f"Err - unable to perform operation '+' between '{left_type}' and '{right_type}'")
        else:
            left_val, left_type = coerce_str(left)
        result = (cast(int, left_val) + cast(int, right_val), right_type)
    return result


def minus_op(left: Variable, right: Variable) -> Variable:
    result = (0, "int")
    left_val, left_type = left
    right_val, right_type = right
    if left_type == "list" or left_type == "dict" or left_type == "expression":
        raise_err(f"Err - unable to perform operation '-' on {left_type}.")
    elif left_type == "int" or left_type == "float":
        if right_type == "string":
            right_val, right_type = coerce_str(right)
        elif right_type != "int" and right_type != "float": 
            raise_err(f"Err - unable to perform operation '-' between '{left_type}' and '{right_type}'")
        result_type = "float" if right_type == "float" or left_type == "float" else "int"
        result = (cast(int, left_val) - cast(int, right_val), result_type)
    elif left_type == "string":
        if right_type != "int" and right_type != "float": 
            raise_err(f"Err - unable to perform operation '-' between '{left_type}' and '{right_type}'")
        else:
            left_val, left_type = coerce_str(left)
        result = (cast(int, left_val) - cast(int, right_val), right_type)
    return result


def mult_op(left: Variable, right: Variable) -> Variable:
    result = (0, "int")
    left_val, left_type = left
    right_val, right_type = right
    if left_type == "list" or left_type == "dict" or left_type == "expression":
        raise_err(f"Err - unable to perform operation '*' on {left_type}.")
    elif left_type == "int" or left_type == "float":
        if right_type == "string":
            right_val, right_type = coerce_str(right)
        elif right_type != "int" and right_type != "float": 
            raise_err(f"Err - unable to perform operation '*' between '{left_type}' and '{right_type}'")
        result_type = "float" if right_type == "float" or left_type == "float" else "int"
        result = (cast(int, left_val) * cast(int, right_val), result_type)
    elif left_type == "string":
        if right_type != "int" and right_type != "float": 
            raise_err(f"Err - unable to perform operation '*' between '{left_type}' and '{right_type}'")
        else:
            left_val, left_type = coerce_str(left)
        result = (cast(int, left_val) * cast(int, right_val), right_type)
    return result


def div_op(left: Variable, right: Variable) -> Variable:
    result : Variable = (0, "int")
    left_val, left_type = left
    right_val, right_type = right
    if left_type == "list" or left_type == "dict" or left_type == "expression":
        raise_err(f"Err - unable to perform operation '/' on {left_type}.")
    elif left_type == "int" or left_type == "float":
        if right_type == "string":
            right_val, right_type = coerce_str(right)
        elif right_type != "int" and right_type != "float": 
            raise_err(f"Err - unable to perform operation '/' between '{left_type}' and '{right_type}'")
        result_type = "float" if right_type == "float" or left_type == "float" else "int"
        if right_val == 0 and left_val == 0:
            result = (-9223372036854775807, "int")
        elif right_val == 0 and left_val != 0:
            result = (9223372036854775807, "int")
        else:
            result = (cast(int, left_val) / cast(int, right_val), result_type)
    elif left_type == "string":
        if right_type != "int" and right_type != "float": 
            raise_err(f"Err - unable to perform operation '/' between '{left_type}' and '{right_type}'")
        else:
            left_val, left_type = coerce_str(left)
        if right_val == 0 and left_val == 0:
            result = (-9223372036854775807, "int")
        elif right_val == 0 and left_val != 0:
            result = (9223372036854775807, "int")
        else:
            result = (cast(int, left_val) / cast(int, right_val), right_type)
    return result

def mod_op(left: Variable, right: Variable) -> Variable:
    result : Variable = (0, "int")
    left_val, left_type = left
    right_val, right_type = right
    if left_type == "list" or left_type == "dict" or left_type == "expression":
        raise_err(f"Err - unable to perform operation '%' on {left_type}.")
    elif left_type == "int" or left_type == "float":
        if right_type == "string":
            right_val, right_type = coerce_str(right)
        elif right_type != "int" and right_type != "float": 
            raise_err(f"Err - unable to perform operation '%' between '{left_type}' and '{right_type}'")
        result_type = "float" if right_type == "float" or left_type == "float" else "int"
        if right_val == 0:
            result = (0, "int")
        else:
            result = (cast(int, left_val) % cast(int, right_val), result_type)
    elif left_type == "string":
        if right_type != "int" and right_type != "float": 
            raise_err(f"Err - unable to perform operation '%' between '{left_type}' and '{right_type}'")
        else:
            left_val, left_type = coerce_str(left)
        if right_val == 0:
            result = (0, "int")
        else:
            result = (cast(int, left_val) % cast(int, right_val), right_type)
    return result
