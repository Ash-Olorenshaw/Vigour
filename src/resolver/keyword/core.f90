module resolver_keyword_core
    use resolver_keyword_vars, only: define_var
    use tokeniser_types, only: tkn_line
    implicit none
contains
    subroutine resolve_keyword_line(tkns)
        use tokeniser_types, only: t_to_str
        use stdlib_strings, only: to_string
        use resolver_expressions, only: resolve_tkn_line
        use writer, only: current_scope_pos, enter_scope, write_str, exit_scope
        type(tkn_line), intent(in) :: tkns
        character(:), allocatable :: resolved_line
        type(tkn_line) :: resolved_tkns
        logical :: s
        integer :: i

        select case(tkns%arr(1)%val)
            case("let")
                call define_var(tkns)
            case("if")
                resolved_tkns = resolve_tkn_line(tkns, 1)
                resolved_line = ""
                do i = 1, resolved_tkns%current - 1
                    if (resolved_tkns%arr(i)%internal) then
                        resolved_line = resolved_line//" "//resolved_tkns%arr(i)%val
                    else
                        resolved_line = resolved_line//" "//resolved_tkns%arr(i)%to_str()
                    end if
                end do 
                s = write_str("if (vim_if("//resolved_line//")) {")
                call enter_scope(current_scope_pos())
                ! TODO
            case("endif")
                call exit_scope()
                s = write_str("}")
        end select
    end subroutine
end module
