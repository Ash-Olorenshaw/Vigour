module utils_types
    implicit none
    private

    public :: alloc_str, alloc_str_arr

    type :: alloc_str
        character(:), allocatable :: val
    end type

    type :: alloc_str_arr
        type(alloc_str), allocatable :: arr(:)
        integer :: size
    end type
end module
