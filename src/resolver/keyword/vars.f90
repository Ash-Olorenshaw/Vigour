module resolver_keyword_vars
    use tokeniser_types, only: tkn_line_arr, tkn_line, EXPRESSION, &
        INTVAL, STRINGVAL, FLOATVAL, LISTVAL, DICTVAL, OPERATOR, &
        IDENTIFIER, FUNCTIONCALL, KEYWORD
    use utils_core, only: raise_err
    use writer, only: write_str
    use resolver_globals, only: var_exists, CURRENT_SCOPE, get_var_name, save_var, var
    use resolver_expressions, only: resolve_tkn_line
    implicit none
    private
    public :: define_var
contains
    subroutine define_var(tkns)
        type(tkn_line), intent(in) :: tkns
        type(tkn_line) :: resolved_tkns
        character(:), allocatable :: var_name, var_val
        logical :: s
        integer :: scope, i

        if (tkns%arr(2)%t == IDENTIFIER) then
            if (tkns%arr(3)%t == OPERATOR .and. tkns%arr(3)%val == "=") then
                resolved_tkns = resolve_tkn_line(tkns, 3)
                if (resolved_tkns%current == 2) then
                    if (resolved_tkns%arr(1)%internal) then
                        var_val = resolved_tkns%arr(1)%val
                    else
                        var_val = resolved_tkns%arr(1)%to_str()
                    end if
                else 
                    do i = 1, resolved_tkns%current
                        print *, resolved_tkns%arr(i)%val
                    end do
                    call raise_err("Passed too many values into var assignment statement")
                end if

                var_name = get_var_name(tkns%arr(2)%val, scope=scope)

                if (var_exists(var_name, scope)) then
                    s = write_str(var_name//"="//var_val//";")
                else
                    s = write_str("vim_var "//var_name//"="//var_val//";")
                    call save_var(var(tkns%arr(4)%t, var_name, scope))
                end if
            end if
        end if
    end subroutine
end module
