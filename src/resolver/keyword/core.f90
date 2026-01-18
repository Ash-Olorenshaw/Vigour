module resolver_keyword_core
    use resolver_keyword_vars, only: define_var
    use tokeniser_types, only: tkn_line
    implicit none
contains
    subroutine resolve_keyword_line(tkns)
        use tokeniser_types, only: t_to_str
        use stdlib_strings, only: to_string
        type(tkn_line), intent(in) :: tkns
        character(:), allocatable :: exec_str
        logical :: s
        integer :: i

        select case(tkns%arr(1)%val)
            case("let")
                call define_var(tkns)
        end select
    end subroutine
end module
