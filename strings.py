


from typing import cast
from error import raise_err
from vars import Variable


def coerce_str(target : Variable) -> Variable:
    target_val, target_type = target
    if target_type != "string":
        raise_err(f"Err - attempted to coerce non-string '{target_val}' to int.")
        return ( 0, "int" )
    else:
        result_num_str = "0"
        for c in cast(str, target_val):
            if not c.isalpha():
                result_num_str += c
            else:
                break
        return (int(result_num_str), "int")

def concat_str(left: Variable, right: Variable) -> Variable:
    result = (0, "int")
    left_val, left_type = left
    right_val, right_type = right
    if left_type in ["string", "int", "float"] and right_type in ["string", "int", "float"]:
        if left_type != "string":
            left_val = str(left_val)
        if right_type != "string":
            right_val = str(right_val)
        result = (cast(str, left_val) + cast(str, right_val), "str")
    else:
        raise_err(f"Err - unable to perform string concat operation on '{left_type}' and '{right_type}'.")
    return result

def create_str(item : str) -> Variable:
    string_type = item[0]
    if string_type == "'" and item[-2] == "'" or string_type == "\"" and item[-2] == "\\":
        raise_err(f"Err - unclosed string: {item}")
    return (item[1:-1], "string")

