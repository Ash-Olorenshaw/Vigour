from vars import VarList
from dataclasses import dataclass

case_sensitive = True

# see: https://superuser.com/questions/732928/in-vim-what-are-settings-commands-that-begin-with-a-prefix-b-g

@dataclass
class Function:
    vars : VarList
    lines : list[list[str]]


compiler_vars : VarList = {} # v:
local_vars : VarList = {} # b:, w:, t:, s:
global_vars : VarList = {} # g:
func_vars : VarList = {} # a:, l:
functions : dict[str, Function] = {} # a:, l:

current_func_name = ""
