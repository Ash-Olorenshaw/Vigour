module build_sys
    use writer, only: PROG_FILE
    implicit none
    private

    character(*), parameter, private :: lib_files = " ./VIMBUILD/lib/io.c ./VIMBUILD/lib/internal.c ./VIMBUILD/lib/maths/basic.c"
    public :: compile, run
contains
    function compile() result(success)
        integer :: exitstat
        logical :: success
        success = .true.
        call execute_command_line("gcc "//PROG_FILE//lib_files//" -Wall -Wextra -pedantic -o out", exitstat=exitstat)
        if (exitstat > 0) & 
            success = .false.
    end function

    subroutine run()
        call execute_command_line("./out")
    end subroutine
end module
