module parser
    use tokeniser_types, only: tkn_line_arr, tkn_line, EXPRESSION, &
        INTVAL, STRINGVAL, FLOATVAL, LISTVAL, DICTVAL, OPERATOR, &
        IDENTIFIER, FUNCTIONCALL, KEYWORD
    use resolver_identifier, only: resolve_identifier_line
    use resolver_keyword_core, only: resolve_keyword_line
    use utils_core, only: raise_err
    implicit none

    public :: parse_tkns
contains
    subroutine parse_tkns(tkns)
        type(tkn_line_arr), intent(in) :: tkns
        type(tkn_line) :: tkns_line
        integer :: i

        do i = 1, tkns%size
            tkns_line = tkns%lines(i)
            select case(tkns_line%arr(1)%t)
                case (EXPRESSION)
                case (INTVAL)
                case (STRINGVAL)
                case (FLOATVAL)
                case (LISTVAL)
                case (DICTVAL)
                case (OPERATOR)
                case (IDENTIFIER)
                    call resolve_identifier_line(tkns_line)
                case (FUNCTIONCALL)
                case (KEYWORD)
                    call resolve_keyword_line(tkns_line)
                case default
                    print *, "Failed to compile token: '", tkns_line%arr(1)%val,"'"
            end select
        end do
    end subroutine
end module
