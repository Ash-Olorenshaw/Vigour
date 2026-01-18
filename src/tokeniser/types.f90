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
        logical :: internal = .false.
        character(:), allocatable :: val
    contains
        procedure :: to_str => tkn_to_str
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
        procedure :: remove => remove_leading_tkns
        procedure :: reset => reset_tkns
    end type
contains
    function tkn_to_str(token) result(str)
        use resolver_globals, only: get_var_name, var_exists
        use utils_core, only: raise_err
        class(tkn), intent(in) :: token
        character(:), allocatable :: str, var_name
        integer :: scope
        
        if (token%t == IDENTIFIER) then
            var_name = get_var_name(token%val, scope=scope)
            if (var_exists(token%val, scope)) then
                str = get_var_name(token%val)
            else
                call raise_err("Err - Attempted to convert identifier to C value '"//token%val//"'")
            end if
        else
            str = "(vim_var){.val."//to_c_union_elem(token%t)//"="//token%val//","//".type="//to_c_type_str(token%t)//"}"
        end if
    end function

    subroutine reset_tkns(tkns) 
        class(tkn_line), intent(inout) :: tkns
        integer :: i
        do i = 1, tkns%current
            tkns%arr(i)%t = -1
            tkns%arr(i)%val = ""
            tkns%arr(i)%internal = .false.
        end do 
        tkns%current = 1
    end subroutine

    subroutine remove_leading_tkns(tkns, count) 
        class(tkn_line), intent(inout) :: tkns
        integer, intent(in) :: count
        tkns%arr = tkns%arr(count+1:)
        tkns%current = tkns%current - count
    end subroutine

    subroutine add_tkn(tkns, t, val, internal)
        use utils_core, only: raise_err
        class(tkn_line), intent(inout) :: tkns
        integer, intent(in) :: t
        character(*), intent(in) :: val
        logical, optional :: internal

        if (tkns%size > tkns%current) then
            tkns%arr(tkns%current)%val = val
            tkns%arr(tkns%current)%t = t
            if (present(internal)) then
                tkns%arr(tkns%current)%internal = internal
            else
                tkns%arr(tkns%current)%internal = .false.
            end if
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

    function to_c_union_elem(t) result(res)
        use utils_core, only: raise_err
        integer, intent(in) :: t
        character(:), allocatable :: res

        select case(t)
            case (EXPRESSION)
                call raise_err("Failed to convert expression token to c type")
            case (INTVAL)
                res = "Number"
            case (STRINGVAL)
                res = "String"
            case (FLOATVAL)
                res = "Float"
            case (LISTVAL)
                res = "Object"
            case (DICTVAL)
                res = "Object"
            case (OPERATOR)
                call raise_err("Failed to convert operator token to c type")
            case (IDENTIFIER)
                call raise_err("Failed to convert identifier token to c type")
            case (FUNCTIONCALL)
                res = "Object"
            case (KEYWORD)
                call raise_err("Failed to convert keyword token to c type")
            case default
                call raise_err("Failed to convert unknown token to c type")
        end select
    end function

    function to_c_type_str(t) result(res)
        use utils_core, only: raise_err
        integer, intent(in) :: t
        character(:), allocatable :: res

        select case(t)
            case (EXPRESSION)
                call raise_err("Failed to convert expression token to c type")
            case (INTVAL)
                res = "Number"
            case (STRINGVAL)
                res = "String"
            case (FLOATVAL)
                res = "Float"
            case (LISTVAL)
                res = "List"
            case (DICTVAL)
                res = "Dict"
            case (OPERATOR)
                call raise_err("Failed to convert operator token to c type")
            case (IDENTIFIER)
                call raise_err("Failed to convert identifier token to c type")
            case (FUNCTIONCALL)
                res = "Funcref"
            case (KEYWORD)
                call raise_err("Failed to convert keyword token to c type")
            case default
                call raise_err("Failed to convert unknown token to c type")
        end select
    end function

    function t_to_str(t) result(res)
        integer, intent(in) :: t
        character(:), allocatable :: res

        select case(t)
            case (EXPRESSION)
                res = "EXPRESSION"
            case (INTVAL)
                res = "INTEGER"
            case (STRINGVAL)
                res = "STRING"
            case (FLOATVAL)
                res = "FLOAT"
            case (LISTVAL)
                res = "LIST"
            case (DICTVAL)
                res = "DICT"
            case (OPERATOR)
                res = "OPERATOR"
            case (IDENTIFIER)
                res = "IDENTIFIER"
            case (FUNCTIONCALL)
                res = "FUNCTIONCALL"
            case (KEYWORD)
                res = "KEYWORD"
            case default
                res = "UNKNOWN"
        end select
    end function
end module
