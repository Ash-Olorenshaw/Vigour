
type Variable = str | int | float | list[Variable] | dict[str, Variable]

compiler_vars : dict[str, Variable] = {} # v:
local_vars : dict[str, Variable] = {} # b:, w:, t:, s:
global_vars : dict[str, Variable] = {} # g:

# see: https://superuser.com/questions/732928/in-vim-what-are-settings-commands-that-begin-with-a-prefix-b-g
