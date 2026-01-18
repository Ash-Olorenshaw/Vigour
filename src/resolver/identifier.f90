module resolver_identifier
    use tokeniser_types, only: tkn_line_arr, tkn_line, EXPRESSION, &
        INTVAL, STRINGVAL, FLOATVAL, LISTVAL, DICTVAL, OPERATOR, &
        IDENTIFIER, FUNCTIONCALL, KEYWORD
    use utils_core, only: raise_err
    use writer, only: write_to_main
    use resolver_globals, only: var_exists, CURRENT_SCOPE, get_var_name
    implicit none
contains
    subroutine resolve_identifier_line(tkns)
        use tokeniser_types, only: t_to_str
        use stdlib_strings, only: to_string
        type(tkn_line), intent(in) :: tkns
        character(:), allocatable :: exec_str
        logical :: s
        integer :: i

        select case(tkns%arr(1)%val)
            case("echo") 
                exec_str = tkns%arr(2)%to_str()
                do i = 3, tkns%current - 1
                    exec_str = exec_str//","//tkns%arr(i)%to_str()
                end do 
                s = write_to_main("vim_print("//to_string(tkns%current - 2)//", "//exec_str//");")
            case("echom")
                exec_str = tkns%arr(2)%to_str()
                do i = 3, tkns%current - 1
                    exec_str = exec_str//","//tkns%arr(i)%to_str()
                end do 
                s = write_to_main("vim_print("//to_string(tkns%current - 2)//", "//exec_str//");")
                s = write_to_main("printf(""\n"");")
        end select
    end subroutine
end module
