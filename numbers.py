
from vars import Variable


def parse_number(target : str) -> Variable:
    if (target.lower().startswith("0x") or target.lower().startswith("0b") or target.lower().startswith("0o") and len(target) > 2) or (target.startswith('0') and len(target) > 1):
        num_str_raw = target[:target.find('.')] if '.' in target else target
        if target[0:2].lower() == "0x":
            num_str = num_str_raw[2:]
            if target.find('.') > -1:
                return (str(int(num_str, 16)) + target[target.find('.') + 1:], "string")
            else: return int(num_str, 16), "int"
        elif target[0:2].lower() == "0b":
            num_str = num_str_raw[2:]
            if target.find('.') > -1:
                return (str(int(num_str, 2)) + target[target.find('.') + 1:], "string")
            else: return int(num_str, 2), "int"
        else: # octal - 0o66 or 066
            num_str = num_str_raw[2:]
            if target.find('.') > -1:
                return (str(int(num_str, 8)) + target[target.find('.') + 1:], "string")
            else: return int(num_str, 8), "int"
    else:
        if target.find('.') > -1:
            return float(target), "float"
        else: 
            return int(target), "int"


