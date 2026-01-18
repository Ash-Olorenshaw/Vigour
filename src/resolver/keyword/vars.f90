module resolver_keyword_vars
    use tokeniser_types, only: tkn_line_arr, tkn_line, EXPRESSION, &
        INTVAL, STRINGVAL, FLOATVAL, LISTVAL, DICTVAL, OPERATOR, &
        IDENTIFIER, FUNCTIONCALL, KEYWORD
    use utils_core, only: raise_err
    use writer, only: write_to_main
    use resolver_globals, only: var_exists, CURRENT_SCOPE, get_var_name, save_var, var
    use resolver_expressions, only: resolve_tkn_line
    implicit none
    private
    public :: define_var
contains
    subroutine define_var(tkns)
        type(tkn_line), intent(in) :: tkns
        character(:), allocatable :: var_name
        logical :: s
        integer :: scope

        if (tkns%arr(2)%t == IDENTIFIER) then
            if (tkns%arr(3)%t == OPERATOR .and. tkns%arr(3)%val == "=") then
                ! TODO - use 'resolve_token_lines'
                var_name = get_var_name(tkns%arr(2)%val, scope=scope)
                if (var_exists(tkns%arr(2)%val, scope)) then
                    ! s = write_to_main(var_name//"=(vim_var)"//tkns%arr(4)%to_str()//";")
                    s = write_to_main(var_name//"="//resolve_tkn_line(tkns, 3)//";")
                else
                    ! s = write_to_main("vim_var "//var_name//"="//tkns%arr(4)%to_str()//";")
                    ! s = write_to_main("vim_var "//var_name//"="//tkns%arr(4)%to_str()//";")
                    s = write_to_main("vim_var "//var_name//"="//resolve_tkn_line(tkns, 3)//";")
                    call save_var(var(tkns%arr(4)%t, tkns%arr(2)%val, scope))
                end if
            end if
        end if
    end subroutine
end module
