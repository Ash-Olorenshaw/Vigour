module utils_core
    implicit none
    private

    public :: raise_err
contains
    subroutine raise_err(err)
        character(*), intent(in) :: err
        print *, err
        stop 1
    end subroutine
end module


