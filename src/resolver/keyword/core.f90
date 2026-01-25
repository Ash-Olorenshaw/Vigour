module resolver_keyword_core
    use resolver_keyword_vars, only: define_var
    use tokeniser_types, only: tkn_line
    implicit none
contains
    subroutine resolve_keyword_line(tkns)
        use tokeniser_types, only: t_to_str
        use stdlib_strings, only: to_string
        use resolver_expressions, only: resolve_tkn_line
        use writer, only: write_str
        type(tkn_line), intent(in) :: tkns
        logical :: s

        select case(tkns%arr(1)%val)
            case("let")
                call define_var(tkns)
            case("if")
                call gen_if_tkn(tkns, .false.)
            case("elseif")
                call gen_if_tkn(tkns, .true.)
            case("else")
                s = write_str("} else {")
            case("endif")
                s = write_str("}")
        end select
    end subroutine

    subroutine gen_if_tkn(tkns, else_if)
        use resolver_expressions, only: resolve_tkn_line
        use writer, only: write_str

        logical, intent(in) :: else_if
        type(tkn_line), intent(in) :: tkns
        type(tkn_line) :: resolved_tkns
        character(:), allocatable :: resolved_line
        logical :: s
        integer :: i

        resolved_tkns = resolve_tkn_line(tkns, 1)
        resolved_line = ""

        do i = 1, resolved_tkns%current - 1
            if (resolved_tkns%arr(i)%internal) then
                resolved_line = resolved_line//" "//resolved_tkns%arr(i)%val
            else
                resolved_line = resolved_line//" "//resolved_tkns%arr(i)%to_str()
            end if
        end do 
        if (else_if) then
            s = write_str("} else if (vim_if("//resolved_line//")) {")
        else
            s = write_str("if (vim_if("//resolved_line//")) {")
        end if
    end subroutine
end module
