module utils_strings
    implicit none
    private
    public :: is_whitespace, skip_whitespace, skip_to_whitespace, str_contains
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

    function is_whitespace(str) result(res)
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
        logical :: res
        integer :: i

        res = .false.

        do i = 1, len(str)
            if (str(i:i) == char) &
                res = .true.
        end do
    end function
end module
