module utils_core
    implicit none
    private

    public :: raise_err, raise_warn
contains
    subroutine raise_err(err)
        character(*), intent(in) :: err
        print *, "ERR - "//err
        print *, "STOP"
        stop 1
    end subroutine

    subroutine raise_warn(warn)
        character(*), intent(in) :: warn
        print *, "WARN - "//warn
    end subroutine
end module


