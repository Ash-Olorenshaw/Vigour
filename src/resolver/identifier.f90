module resolver_identifier
    use tokeniser_types, only: tkn_line_arr, tkn_line, EXPRESSION, &
        INTVAL, STRINGVAL, FLOATVAL, LISTVAL, DICTVAL, OPERATOR, &
        IDENTIFIER, FUNCTIONCALL, KEYWORD
    use utils_core, only: raise_err
    use resolver_expressions, only: resolve_tkn_line
    use writer, only: write_to_main
    use resolver_globals, only: var_exists, CURRENT_SCOPE, get_var_name
    implicit none
contains
    subroutine resolve_identifier_line(tkns)
        type(tkn_line), intent(in) :: tkns
        type(tkn_line) :: resolved_tkns
        logical :: s

        select case(tkns%arr(1)%val)
            case("echo") 
                resolved_tkns = resolve_tkn_line(tkns, 1)
                call echo(resolved_tkns)
            case("echom")
                resolved_tkns = resolve_tkn_line(tkns, 1)
                call echo(resolved_tkns)
                s = write_to_main("printf(""\n"");")
        end select
    end subroutine

    subroutine echo(tkns)
        use stdlib_strings, only: to_string
        type(tkn_line), intent(in) :: tkns
        character(:), allocatable :: exec_str
        logical :: s
        integer :: i

        if (tkns%arr(1)%internal) then
            exec_str = tkns%arr(1)%val
        else
            exec_str = tkns%arr(1)%to_str()
        end if

        do i = 1, tkns%current - 1
            if (tkns%arr(1)%internal) then
                exec_str = exec_str//","//tkns%arr(i)%val
            else
                exec_str = exec_str//","//tkns%arr(i)%to_str()
            end if
        end do 
        s = write_to_main("vim_print("//to_string(tkns%current - 1)//", "//exec_str//");")
    end subroutine
end module
