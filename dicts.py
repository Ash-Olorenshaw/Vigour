
from typing import Tuple, cast
from error import raise_err
from interpreter import interpreter_eval
from strings import create_str
from vars import Variable


def create_dict(target : str) -> Variable:
    dict_content = target[1:-1]
    dict_pairs = dict_content.split(",")
    result_dict : dict[str, Variable] = {}
    for item in dict_pairs:
        if not ':' in item or item.find(':') != 1:
            raise_err(f"Err - '{item}' is not a valid keypair for a dictionary.")

        parts = item.split(':')
        key : str = parts[0]
        val = parts[1]
        if (key[0] == "\"" and key[-1] == "\"") or (key[0] == "'" and key[-1] == "'"):
            key, _ = cast(Tuple[str, str], create_str(key))
        else:
            raise_err(f"Err - '{key}' is not a valid string.")

        new_val : Variable = interpreter_eval([ val ])
        result_dict[key] = new_val

    return result_dict, "dict"
