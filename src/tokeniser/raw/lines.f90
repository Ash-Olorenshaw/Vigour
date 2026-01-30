module tokeniser_raw_lines
    use utils_types, only: alloc_str_arr
    implicit none
    private

    type :: raw_line
        character(:), allocatable :: val
        integer :: real_line_no, char_offset
    end type

    public :: generate_lines, raw_line
contains
    subroutine generate_lines(text, lines, count)
        use stdlib_ascii, only: LF
        use stdlib_strings, only: strip
        use utils_strings, only: is_whitespace
        character(*), intent(in) :: text
        type(raw_line), dimension(:), allocatable, intent(out) :: lines
        integer, intent(out) :: count
        character :: c, inside_string
        character(:), allocatable :: stripped_line
        integer :: str_s, i, lines_count, current_line, current_char

        str_s = 1
        lines_count = 1

        do i = 1, len(text) 
            c = text(i:i)
            if (c == LF .or. c == '|') &
                lines_count = lines_count + 1
        end do

        allocate(lines(lines_count))
        lines_count = 1
        inside_string = ' '
        current_line = 0
        current_char = 0

        do i = 1, len(text)
            c = text(i:i)
            if (c == LF) then
                current_line = current_line + 1
                current_char = 0
            else
                current_char = current_char + 1
            end if
            
            if (c == LF .or. c == '|' .and. inside_string == ' ') then
                if (i == len(text)) then
                    if (text(i-1:i-1) /= '\') then
                        ! TODO - separate out duplicated code
                        stripped_line = strip(text(str_s:i-1))
                        if (.not. is_whitespace(stripped_line)) then
                            lines(lines_count)%val = stripped_line
                            lines(lines_count)%real_line_no = current_line
                            lines(lines_count)%char_offset = current_char
                            lines_count = lines_count + 1
                            str_s = i + 1
                        end if
                    end if
                else if (i == 1) then
                    if (text(i+1:i+1) /= '\') then
                        stripped_line = strip(text(str_s:i-1))
                        if (.not. is_whitespace(stripped_line)) then
                            lines(lines_count)%val = stripped_line
                            lines(lines_count)%real_line_no = current_line
                            lines(lines_count)%char_offset = current_char
                            lines_count = lines_count + 1
                            str_s = i + 1
                        end if
                    end if
                else
                    if (text(i+1:i+1) /= '\' .and. text(i-1:i-1) /= '\') then
                        stripped_line = strip(text(str_s:i-1))
                        if (.not. is_whitespace(stripped_line)) then
                            lines(lines_count)%val = stripped_line
                            lines(lines_count)%real_line_no = current_line
                            lines(lines_count)%char_offset = current_char
                            lines_count = lines_count + 1
                            str_s = i + 1
                        end if
                    end if
                end if
            else if (i == len(text)) then
                lines(lines_count)%val = text(str_s:i)
                lines(lines_count)%real_line_no = current_line
                lines(lines_count)%char_offset = current_char
                lines_count = lines_count + 1
            else if (c == '"' .or. c == "'") then
                if (inside_string == c) then
                    inside_string = ' '
                else if (inside_string == ' ') then
                    inside_string = c
                end if
            end if
        end do
            
        count = lines_count
    end subroutine
end module


