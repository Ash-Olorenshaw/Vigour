module vigour
    use tokeniser_raw_lines, only: generate_lines
    use utils_types, only: alloc_str_arr
    use utils_files, only: get_file_str
    use tokeniser_gen, only: gen_tkns, print_tkns
    use tokeniser_types, only: tkn_line, tkn_line_arr
    implicit none
    private

    public :: say_hello
contains
    subroutine say_hello
        type(alloc_str_arr) :: lines
        type(tkn_line_arr) :: tkns
        integer :: i

        call generate_lines(get_file_str("test.vim"), lines)
        call tkns%alloc(lines%size)

        do i = 1, lines%size
            print *, ">> '", lines%arr(i)%val, "'"
            call gen_tkns(lines%arr(i)%val, tkns%lines(i))
            call print_tkns(tkns%lines(i))
        end do
    end subroutine
end module
