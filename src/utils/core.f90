module utils_core
    use stdlib_strings, only: to_string
    implicit none
    private

    public :: raise_err, raise_warn
contains
    subroutine raise_err(err, line, col)
        character(*), intent(in) :: err
        integer, optional, intent(in) :: line, col

        if (present(line) .and. present(col)) then
            print *, "ERR - "//err// " at position ("//to_string(line)//","//to_string(col)//")"
        else
            print *, "ERR - "//err
        end if

        print *, "STOP"
        stop 1
    end subroutine

    subroutine raise_warn(warn, line, col)
        character(*), intent(in) :: warn
        integer, optional, intent(in) :: line, col

        if (present(line) .and. present(col)) then
            print *, "WARN - "//warn// " at position ("//to_string(line)//","//to_string(col)//")"
        else
            print *, "WARN - "//warn
        end if
    end subroutine
end module


