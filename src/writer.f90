module writer
    implicit none
    private

    public :: setup, write_str, write_to_line, exit_scope, enter_scope, current_scope_pos

    character(*), parameter, public :: PROG_FILE = "VIMBUILD/program.c"
    integer :: scopes(100)
    integer :: current_scope = 0

    integer :: global_var_pos = 4
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
            &#include ""./lib/maths/basic.h"""//LF//"&
            &int main(int argc, char *argv[]) {"//LF//"&
            &return 0;"//LF//"&
            &}" &
        )
        call enter_scope(5)
    end subroutine

    subroutine exit_scope()
        use utils_core, only: raise_err
        if (current_scope > 1) then
            current_scope = current_scope - 1
        else 
            call raise_err("Err - Failed to exit scope, already at a toplevel scope")
        end if
    end subroutine

    subroutine enter_scope(val)
        use utils_core, only: raise_err
        integer, intent(in) :: val
        if (current_scope < 100) then 
            current_scope = current_scope + 1
            scopes(current_scope) = val
        else 
            call raise_err("Err - Attempted to enter a scope beyone max depth of 100.")
        end if
    end subroutine

    function current_scope_pos() result(res)
        integer :: res
        res = scopes(current_scope) 
    end function
    
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

        success = write_to_line(scopes(current_scope), str)
        newlines = str_contains(str, LF)

        do i = 1, current_scope
            scopes(i) = scopes(i) + newlines + 1
        end do
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
