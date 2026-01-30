module vigour
    use tokeniser_raw_lines, only: generate_lines
    use utils_types, only: alloc_str_arr
    use utils_files, only: get_file_str
    use tokeniser_gen, only: gen_tkns, print_tkns
    use tokeniser_types, only: tkn_line, tkn_line_arr
    use writer, only: setup

    use build_sys, only: compile, run
    use parser, only: parse_tkns
    implicit none
    private

    public :: say_hello
contains
    subroutine say_hello
        use stdlib_ascii, only: LF
        use utils_core, only: raise_err
        use tokeniser_raw_lines, only: raw_line
        type(raw_line), dimension(:), allocatable :: lines
        type(tkn_line_arr) :: tkns
        integer :: i, lines_count
        logical :: s, comp_success

        call setup()

        call generate_lines(get_file_str("test.vim"), lines, lines_count)
        call tkns%alloc(lines_count)

        do i = 1, lines_count
            print *, ">> '", lines(i)%val, "'"
            call gen_tkns(lines(i), tkns%lines(i))
            call print_tkns(tkns%lines(i))
        end do

        call parse_tkns(tkns)

        comp_success = compile()
        if (comp_success) then
            call run()
        else
            call raise_err("Err - compilation failed")
        end if
    end subroutine
end module
