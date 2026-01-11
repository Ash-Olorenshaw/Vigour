module tokeniser_raw_lines
    use utils_types, only: alloc_str_arr
    implicit none
    private

    public :: generate_lines
contains
    subroutine generate_lines(text, lines)
        use stdlib_ascii, only: LF
        use stdlib_strings, only: strip
        use utils_strings, only: is_whitespace
        character(*), intent(in) :: text
        type(alloc_str_arr), intent(out) :: lines
        character :: c, inside_string
        character(:), allocatable :: stripped_line
        integer :: str_s, i, lines_count

        str_s = 1
        lines_count = 1

        do i = 1, len(text) 
            c = text(i:i)
            if (c == LF .or. c == '|') &
                lines_count = lines_count + 1
        end do

        allocate(lines%arr(lines_count))
        lines_count = 1
        inside_string = ' '

        do i = 1, len(text)
            c = text(i:i)
            if (c == LF .or. c == '|' .and. inside_string == ' ') then
                if (i == len(text)) then
                    if (text(i-1:i-1) /= '\') then
                        ! TODO - separate out into func
                        stripped_line = strip(text(str_s:i-1))
                        if (.not. is_whitespace(stripped_line)) then
                            lines%arr(lines_count)%val = stripped_line
                            lines_count = lines_count + 1
                            str_s = i + 1
                        end if
                    end if
                else if (i == 1) then
                    if (text(i+1:i+1) /= '\') then
                        stripped_line = strip(text(str_s:i-1))
                        if (.not. is_whitespace(stripped_line)) then
                            lines%arr(lines_count)%val = stripped_line
                            lines_count = lines_count + 1
                            str_s = i + 1
                        end if
                    end if
                else
                    if (text(i+1:i+1) /= '\' .and. text(i-1:i-1) /= '\') then
                        stripped_line = strip(text(str_s:i-1))
                        if (.not. is_whitespace(stripped_line)) then
                            lines%arr(lines_count)%val = stripped_line
                            lines_count = lines_count + 1
                            str_s = i + 1
                        end if
                    end if
                end if
            else if (i == len(text)) then
                lines%arr(lines_count)%val = text(str_s:i)
                lines_count = lines_count + 1
            else if (c == '"' .or. c == "'") then
                if (inside_string == c) then
                    inside_string = ' '
                else if (inside_string == ' ') then
                    inside_string = c
                end if
            end if
        end do
            
        lines%size = lines_count
    end subroutine
end module


