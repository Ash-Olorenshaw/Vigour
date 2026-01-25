module writer
    implicit none
    private

    public :: setup, write_str, write_to_line

    character(*), parameter, public :: PROG_FILE = "VIMBUILD/program.c"
    integer :: main_pos = 0
    integer :: global_var_pos = 5
    integer :: current_func_var_pos = -1
contains
    subroutine setup()
        use stdlib_ascii, only: LF
        logical :: s
        integer :: io
        s = write_contents( "&
            &#include <stdio.h>"//LF//"&
            &#include ""./lib/io.h"""//LF//"&
            &#include ""./lib/internal.h"""//LF//"&
            &#include ""./lib/equality.h"""//LF//"&
            &#include ""./lib/maths/basic.h"""//LF//"&
            &int main(int argc, char *argv[]) {"//LF//"&
            &return 0;"//LF//"&
            &}" &
        )
        main_pos = 6
    end subroutine
    
    function write_contents(str) result(success)
        character(*), intent(in) :: str
        integer :: io
        logical :: success
        open(newunit=io, file=PROG_FILE, status="replace", action="write")
        write(io, *) str
        close(io)
        success = .true.
    end function
    
    function write_str(str) result(success)
        use utils_strings, only: str_contains
        use stdlib_ascii, only: LF

        character(*), intent(in) :: str
        logical :: success
        integer :: newlines, i

        success = write_to_line(main_pos, str)
        newlines = str_contains(str, LF)
        main_pos = main_pos + newlines + 1
    end function

    function write_to_line(insert_after, str) result(success)
        character(*), intent(in) :: str
        integer, intent(in) :: insert_after
        logical :: success

        character(*), parameter :: tmp = 'VIMBUILD/data.tmp'
        character(1024) :: line
        integer :: io, io2, line_i, status
        
        success = .true.

        open(newunit=io , file=PROG_FILE, status='old', action='read')
        open(newunit=io2, file=tmp, status='replace', action='write')

        line_i = 0
        do
            read(io,'(a)',iostat=status) line

            if (status /= 0) &
                exit

            line_i = line_i + 1
            write(io2,'(a)') trim(line)

            if (line_i == insert_after) &
                write(io2,'(a)') str
        end do

        close(io)
        close(io2)

        status = 0
        call rename(tmp, PROG_FILE, status)
        if (status /= 0) &
            success = .false.
    end function
end module
