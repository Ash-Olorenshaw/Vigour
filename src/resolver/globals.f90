module resolver_globals
    use utils_core, only: raise_warn
    implicit none
    private
    
    type :: var
        integer :: t = -1
        character(:), allocatable :: name
        integer :: scope
    end type

    type(var), public :: VARS(1024)
    type(var), allocatable, public :: FUNCTIONS(:)
    integer, public :: VARS_SIZE = 0
    integer, public :: FUNCTIONS_SIZE = 0

    ! integer, parameter, public :: SCOPE_COMPILER = 0 ! v:
    integer, parameter, public :: SCOPE_LOCAL = 1 ! b:, w:, t:, s:
    integer, parameter, public :: SCOPE_GLOBAL = 2 ! g:
    integer, parameter, public :: SCOPE_FUNC = 3 ! a:, l:
    integer, public :: CURRENT_SCOPE = SCOPE_LOCAL

    public :: save_var, var_exists, get_var_name, get_var_type, var
contains
    subroutine save_var(tosave)
        type(var), intent(in) :: tosave
        VARS_SIZE = VARS_SIZE + 1
        VARS(VARS_SIZE) = tosave
    end subroutine

    function get_var_type(name, scope) result(var_type)
        character(*), intent(in) :: name
        integer, intent(in) :: scope
        integer :: i, var_type

        var_type = -1

        if (VARS_SIZE > 0) then
            do i = 1, VARS_SIZE
                print *, "    TESTING: ", VARS(i)%name, " AGAINST: ", name
                if (VARS(i)%name == name) then
                    if (VARS(i)%scope == scope) then
                        var_type = VARS(i)%t
                        return
                    else
                        call raise_warn("Named variable exists in different scope.")
                    end if
                end if
            end do
            call raise_warn("Variable does not exist: '"//name//"'")
        end if
    end function

    function get_var_name(name, scope) result(var_name)
        use stdlib_strings, only: starts_with
        character(*), intent(in) :: name
        integer, optional, intent(out) :: scope
        integer :: s
        character(:), allocatable :: var_name

        if (starts_with(name, "g:")) then
            var_name = "global_var_"//name(3:)
            s = SCOPE_GLOBAL
        else if (starts_with(name, "a:") .or. starts_with(name, "l:")) then
            var_name = "scope_var_"//name(3:)
            s = SCOPE_FUNC
        else if (starts_with(name, "b:") .or. starts_with(name, "w:") .or. starts_with(name, "t:") .or. starts_with(name, "s:")) then
            var_name = "local_var_"//name(3:)
            s = SCOPE_LOCAL
        else
            s = CURRENT_SCOPE
            if (CURRENT_SCOPE == SCOPE_GLOBAL) then
                var_name = "global_var_"//name
            else if (CURRENT_SCOPE == SCOPE_FUNC) then
                var_name = "scope_var_"//name
            else
                var_name = "local_var_"//name
            end if
        end if

        if (present(scope)) &
            scope = s
    end function

    function var_exists(name, scope) result(exists)
        use utils_core, only: raise_err
        character(*), intent(in) :: name
        integer, intent(in) :: scope
        logical :: exists
        integer :: i
        exists = .false.

        if (VARS_SIZE > 0) then
            do i = 1, VARS_SIZE
                if (VARS(i)%name == name) then
                    if (VARS(i)%scope == scope) then
                        exists = .true.
                        return
                    ! else
                    !     call raise_warn("Err - named variable exists in different scope.")
                    end if
                end if
            end do
            ! call raise_err("Err - variable does not exist.")
        end if
    end function
end module
