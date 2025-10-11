from enum import Enum
from vars import Variable, global_vars, local_vars
from globals import scopes
from error import raise_err
from tokeniser import special_tkns

class VarScope(Enum):
    Local = 0
    Script = 1 
    Global = 2

def register_var(var_name : str, val : Variable, scope : VarScope):
    if scope == VarScope.Global:
        if var_name in global_vars:
            raise_err(f"Err - attempted to redefine already defined variable: '{var_name}'")
        else:
            global_vars[var_name] = val
    elif scope == VarScope.Script or (scope == VarScope.Local and len(scopes) == 0):
        if var_name in local_vars:
            raise_err(f"Err - attempted to redefine already defined variable: '{var_name}'")
        else:
            local_vars[var_name] = val
    else:
        scope_vars, _ = scopes[-1]
        if var_name in scope_vars:
            raise_err(f"Err - attempted to redefine already defined variable: '{var_name}'")
        else:
            scope_vars[var_name] = val

    return None

def eval(script : list[str]):
    for tkn in script:
        tkn = tkn.strip()
        if tkn == '-' or tkn == '+' or tkn[0].isalpha():
    # note to self. things like: + + + 1 and '---1' are possible, but not '*1', etc
