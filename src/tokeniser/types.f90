module tokeniser_types
    implicit none
    private

    public :: tkn, tkn_line, tkn_line_arr, t_to_str

    integer, parameter, public :: EXPRESSION = 0 ! things in ()

    integer, parameter, public :: INTVAL = 1
    integer, parameter, public :: STRINGVAL = 2
    integer, parameter, public :: FLOATVAL = 3

    integer, parameter, public :: LISTVAL = 4
    integer, parameter, public :: DICTVAL = 5

    integer, parameter, public :: OPERATOR = 6

    integer, parameter, public :: IDENTIFIER = 7 ! also known as 'var'
    integer, parameter, public :: FUNCTIONCALL = 8 
    integer, parameter, public :: KEYWORD = 9
    ! TODO - lambda, function ptr, etc

    type :: tkn
        integer :: t = -1
        character(:), allocatable :: val
    end type

    type :: tkn_line_arr
        type(tkn_line), allocatable :: lines(:)
        integer :: size
    contains
        procedure :: alloc => tkn_line_arr_alloc
    end type

    type :: tkn_line
        type(tkn), allocatable :: arr(:)
        integer :: size, current = 1
    contains
        procedure :: add => add_tkn
    end type
contains
    subroutine add_tkn(tkns, t, val)
        use utils_core, only: raise_err
        class(tkn_line), intent(inout) :: tkns
        integer, intent(in) :: t
        character(*), intent(in) :: val

        if (tkns%size > tkns%current) then
            tkns%arr(tkns%current)%val = val
            tkns%arr(tkns%current)%t = t
            tkns%current = tkns%current + 1        
        else
            call raise_err("Failed to allocate token, token max exceeded.")
        end if
    end subroutine

    subroutine tkn_line_arr_alloc(tkn_lines, size)
        class(tkn_line_arr), intent(inout) :: tkn_lines
        integer, intent(in) :: size
        allocate(tkn_lines%lines(size))
        tkn_lines%size = size
    end subroutine

    function t_to_str(t) result(res)
        integer, intent(in) :: t
        character(:), allocatable :: res

        select case(t)
            case (0)
                res = "EXPRESSION"
            case (1)
                res = "INTEGER"
            case (2)
                res = "STRING"
            case (3)
                res = "FLOAT"
            case (4)
                res = "LIST"
            case (5)
                res = "DICT"
            case (6)
                res = "OPERATOR"
            case (7)
                res = "IDENTIFIER"
            case (8)
                res = "FUNCTIONCALL"
            case (9)
                res = "KEYWORD"
            case default
                res = "UNKNOWN"
        end select
    end function
end module
