module utils_strings
    implicit none
    private
    public :: is_whitespace, skip_whitespace, skip_to_whitespace, str_contains, replace_all_occurences
contains
    subroutine skip_whitespace(str, pos)
        use utils_core, only: raise_err
        use stdlib_ascii, only: is_white
        character(*), intent(in) :: str
        integer, intent(inout) :: pos
        character :: c

        do while(pos <= len(str))
            if (.not. is_white(str(pos:pos))) &
                exit
            pos = pos + 1
        end do
    end subroutine

    subroutine skip_to_whitespace(str, pos)
        use utils_core, only: raise_err
        use stdlib_ascii, only: is_white
        character(*), intent(in) :: str
        integer, intent(inout) :: pos
        character :: c

        do while(pos <= len(str))
            if (is_white(str(pos:pos))) then
                pos = pos - 1
                exit
            end if
            pos = pos + 1
        end do
    end subroutine

    pure function is_whitespace(str) result(res)
        use stdlib_ascii, only: is_white
        character(*), intent(in) :: str
        logical :: res
        integer :: i

        res = .true.
        do i = 1, len(str)
            if (.not. is_white(str(i:i))) res = .false.
        end do
    end function

    pure function str_contains(str, char) result(res)
        character(*), intent(in) :: str
        character, intent(in) :: char
        integer :: i, res

        res = 0

        do i = 1, len(str)
            if (str(i:i) == char) &
                res = res + 1
        end do
    end function

    function replace_all_occurences(str, char, replace) result(new_str)
        character(*), intent(in) :: str, replace
        character, intent(in) :: char
        character(:), allocatable :: new_str
        integer :: i, j, c, occurences

        occurences = str_contains(str, char)
        allocate(character(len(str) + occurences) :: new_str)

        i = 1
        j = 1
        do while(i <= len(str))
            if (str(i:i) == char) then
                do c = 1, len(replace)
                    new_str(j+c-1:j+c-1) = char
                end do
                j = j + len(replace) - 1
            else
                new_str(j:j) = str(i:i)
            end if
            i = i + 1
            j = j + 1
        end do
    end function
end module
