module tokeniser_raw_tokens
    implicit none
    private
    public :: is_special, is_valid_var_id, is_keyword, is_number, skip_to_tkn
contains
    function is_special(str, i, new_pos) result(res)
        use stdlib_strings, only: strip
        character(*), intent(in) :: str
        integer, intent(in) :: i
        integer, intent(out) :: new_pos
        integer :: tkn
        logical :: res
        character(:), allocatable :: stripped_tkn, comp_str
        character(3), dimension(19) :: special_tkns = [ "=  ", "+  ", "-  ", "*  ", "/  ", "%  ", "== ", "!= ", ">  ", ">= ", "<  ", "<= ", "==?", "==#", "!=?", "!=#" ,".. ", "!  ", "?  " ]

        res = .false.

        do tkn = 1, size(special_tkns)
            stripped_tkn = strip(special_tkns(tkn))
            if (i+len(stripped_tkn) - 1 <= len(str)) then
                comp_str = str(i:i + len(stripped_tkn) - 1)
                if (comp_str == stripped_tkn) then
                    res = .true.
                    new_pos = i + len(stripped_tkn) - 1
                    return
                end if
            end if
        end do
    end function

    function is_keyword(str) result(res)
        use stdlib_strings, only: strip
        character(*), intent(in) :: str
        logical :: res
        integer :: i
        character(11), dimension(24) :: keyword_tkns = [ "if         ", "elseif     ", "else       ", "endif      ", "while      ", "endwhile   ", &
            "for        ", "in         ", "endfor     ", "try        ", "catch      ", "finally    ", "endtry     ", "function   ", "return     ", "endfunction", &
            "call       ", "execute    ", "normal     ", "let        ", "unlet      ", "delfunction", "break      ", "continue   " ]
        
        res = .false.
        do i = 1, size(keyword_tkns)
            if (str == strip(keyword_tkns(i))) &
                res = .true.
        end do
    end function

    function is_valid_var_id(str) result(res)
        use stdlib_ascii, only: is_alphanum, is_digit
        character(*), intent(in) :: str
        logical :: res
        integer :: i

        res = .true.

        if (is_digit(str(1:1))) then
            res = .false.
        end if

        do i = 1, len(str)
            if (.not. is_alphanum(str(i:i)) .and. str(i:i) /= '_' .and. str(i:i) /= ':') res = .false.
        end do
    end function

    function is_number(str) result(res)
        use stdlib_ascii, only: is_alphanum, is_digit
        character(*), intent(in) :: str
        logical :: res
        integer :: i

        res = .true.

        do i = 1, len(str)
            if ( & 
                .not. is_digit(str(i:i)) & 
                .and. str(i:i) /= '.' & 
                .and. str(i:i) /= 'x' & 
                .and. str(i:i) /= 'X' & 
                .and. str(i:i) /= 'o' & 
                .and. str(i:i) /= 'O' & 
                .and. str(i:i) /= 'b' & 
                .and. str(i:i) /= 'B' & 
            ) then 
                res = .false.
                return
            end if
        end do
    end function

    subroutine skip_to_tkn(str, pos, tkn, esc)
        use utils_core, only: raise_err
        character(*), intent(in) :: str
        character, intent(in) :: tkn
        integer, intent(inout) :: pos
        logical, optional, intent(in) :: esc
        character :: c

        if (pos >= len(str)) &
            return

        do while(pos <= len(str))
            if (str(pos:pos) == tkn) then
                if (present(esc)) then
                    if (esc .and. pos > 1) then
                        if (str(pos - 1:pos - 1) == '\') then
                            pos = pos + 1
                            cycle
                        end if
                    end if
                end if
                return
            end if
            pos = pos + 1
        end do

        call raise_err("Failed to find token: '"//tkn//"'")
    end subroutine
end module
