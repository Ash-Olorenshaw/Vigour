# Vigour

Vigour is Fortran implementation of the VimScript specification as a VimScript -> C compiler fully separate from the `vim` application itself.


Currently, it is only a tokeniser with basic compilation capabilities (maths, `echo`, vars, if statements).

The only vimtypes supported are `Number` and `Float`

Todo: 

- [x] Fix currently broken equality testing funcs
- [x] Update `resolve_tkn_line` in `resolver_expressions` to not be a monstrous, unwieldy and likely very broken function
- [x] Change equality funcs to be macos??
