module tokeniser_gen
    use tokeniser_types, only: tkn_line, EXPRESSION, INTVAL, STRINGVAL, FLOATVAL, LISTVAL, DICTVAL, &
        OPERATOR, IDENTIFIER, FUNCTIONCALL, KEYWORD
    implicit none
    private

    public :: gen_tkns, print_tkns
contains
    subroutine print_tkns(tkns)
        use tokeniser_types, only: t_to_str
        type(tkn_line), intent(in) :: tkns
        integer :: i

        do i = 1, tkns%size
            if (tkns%arr(i)%t > -1) then
                write (*, '(A)', advance="no") t_to_str(tkns%arr(i)%t)
                write (*, '(A)', advance="no") " => "
                write (*, '(A)') tkns%arr(i)%val
            end if
        end do
    end subroutine

    subroutine gen_tkns(line, tkns)
        use stdlib_ascii, only: is_white
        use utils_strings, only: skip_whitespace, skip_to_whitespace, str_contains
        use tokeniser_raw_tokens, only: is_special, is_valid_var_id, is_number, skip_to_tkn, is_keyword
        character(*), intent(in) :: line
        type(tkn_line), intent(out) :: tkns
        character :: c
        integer :: i, new_i, start
        logical :: new_tkn
        
        i = 1
        start = 1
        tkns%size = 0
        new_tkn = .true.

        ! temp code vv
        allocate(tkns%arr(128))
        tkns%size = 128
        ! temp code ^^

        ! TODO - test for string concat with '.'

        do while(i <= len(line))
            c = line(i:i)

            if (is_white(c)) then
                call skip_whitespace(line, i)
                start = i
                cycle
            end if

            if (i < len(line)) then
                if (is_white(line(i+1:i+1))) then
                    new_tkn = .true.
                else if (is_special(line, i + 1, new_i)) then
                    new_tkn = .true.
                else
                    new_tkn = .false.
                end if
            else if (i == len(line)) then
                new_tkn = .true.
            else
                new_tkn = .false.
            end if

            if (c == '"' .or. c == "'") then
                if (i < len(line)) then
                    i = i + 1
                    start = i
                    call skip_to_tkn(line, i, c, esc=.true.)
                    call tkns%add(STRINGVAL, line(start:i-1))
                    start = i
                    cycle
                end if
            else if (c == '(') then
                start = i
                call skip_to_tkn(line, i, ')')
                call tkns%add(EXPRESSION, line(start+1:i-1))
                start = i
                cycle
            else if (c == '[') then
                start = i
                call skip_to_tkn(line, i, ']')
                call tkns%add(LISTVAL, line(start+1:i-1))
                start = i
                cycle
            else if (c == '{') then
                start = i
                call skip_to_tkn(line, i, '}')
                call tkns%add(DICTVAL, line(start+1:i-1))
                start = i
                cycle
            end if

            if (is_special(line, i, new_i)) then
                call tkns%add(OPERATOR, line(i:new_i))
                i = new_i
                start = i + 1
            else if (new_tkn .and. is_number(line(start:i))) then
                if (str_contains(line(start:i), '.') > 0) then
                    call tkns%add(FLOATVAL, line(start:i))
                else
                    call tkns%add(INTVAL, line(start:i))
                end if
                start = i
            else if (new_tkn .and. is_valid_var_id(line(start:i))) then
                if (is_keyword(line(start:i))) then
                    call tkns%add(KEYWORD, line(start:i))
                else
                    call tkns%add(IDENTIFIER, line(start:i))
                end if
                start = i
            end if 

            i = i + 1
        end do
    end subroutine
end module

