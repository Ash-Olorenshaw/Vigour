module utils_files
    implicit none
    private
    public :: get_file_str
contains
    function get_file_str(file_target, success) result(file_content) 
        integer :: io, file_size, iostat
        character(*), intent(in) :: file_target
        logical, intent(out), optional :: success
        character(:), allocatable :: file_content
        logical :: exists

        inquire(file=file_target, SIZE=file_size, iostat=iostat, exist=exists)
        if (iostat /= 0 .or. .not. exists) then
            success = .false.
            file_content = ""
            return
        end if
        allocate(character(len=file_size)::file_content)

        open(newunit=io, file=file_target, access="stream", status="old", action="read", iostat=iostat)
            if (iostat /= 0) then
                success = .false.
                file_content = ""
                return
            end if
            read(io) file_content
        close(io)
    end function
end module
