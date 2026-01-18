module resolver_expressions
    use tokeniser_types, only: tkn_line, tkn
    implicit none
    private
    public :: resolve_tkn_line
contains

    subroutine consume_group(consumed, next_consumed, item1, item2, item3)
        integer, intent(inout) :: consumed(1024), next_consumed
        integer, intent(in) :: item1, item2, item3
        consumed(next_consumed) = item1
        consumed(next_consumed + 1) = item2
        consumed(next_consumed + 2) = item3
        next_consumed = next_consumed + 3
    end subroutine

    subroutine reset_consumed(consumed, next_consumed)
        integer, intent(inout) :: consumed(1024), next_consumed
        integer :: i

        do i = 1, next_consumed - 1
            consumed(i) = 0
            next_consumed = 1
        end do
    end subroutine

    function is_consumed(val, consumed, next_consumed) result(is) 
        integer, intent(in) :: consumed(1024), next_consumed, val
        integer :: i
        logical :: is
        is = .false.
        do i = 1, next_consumed - 1
            if (consumed(i) == val) then
                is = .true.
                return
            end if
        end do
    end function

    function resolve_tkn_line(tkns, start) result(val)
        use tokeniser_types, only: tkn_line, EXPRESSION, INTVAL, STRINGVAL, FLOATVAL, LISTVAL, DICTVAL, &
            OPERATOR, IDENTIFIER, FUNCTIONCALL, KEYWORD
        use utils_types, only: alloc_str_arr
        use utils_core, only: raise_err
        type(tkn_line), intent(in) :: tkns
        integer, intent(in) :: start
        type(tkn_line) :: new_tkns, last_tkns
        integer :: pos, lvl, ops, i, consumed(1024), next_consumed
        character(:), allocatable :: val, item1, item2
        type(tkn) :: next_tkn, prev_tkn

        ! 2 + 3 * 5 - 1
        ! >> vim_add(2, 3) * vim_add(5, 1)
        ! >> vim_mult(vim_add(2, 3), vim_sub(5, 1))

        ops = 0
        lvl = 7
        last_tkns = tkns 
        call last_tkns%remove(start)
        new_tkns%size = tkns%size
        ! new_tkns = last_tkns
        allocate(new_tkns%arr(tkns%size))
        pos = 1
        next_consumed = 1

        do while(lvl > 0)
            print *, "STARTING LOOP: ", pos, last_tkns%current - 1
            do while(pos < last_tkns%current)
                if (last_tkns%arr(pos)%t == OPERATOR) then
                    ! TODO - '+2' becomes '2', etc
                    if (pos > 1) then
                        if (pos < last_tkns%current - 1) then
                            prev_tkn = last_tkns%arr(pos - 1)
                            next_tkn = last_tkns%arr(pos + 1)
                            if (prev_tkn%internal) then
                                print *, "INTERNAL: ", prev_tkn%val
                                item1 = prev_tkn%val
                            else
                                print *, "NOT INTERNAL: ", prev_tkn%to_str()
                                item1 = prev_tkn%to_str()
                            end if
                            if (next_tkn%internal) then
                                print *, "INTERNAL: ", next_tkn%val
                                item2 = next_tkn%val
                            else
                                print *, "NOT INTERNAL: ", next_tkn%to_str()
                                item2 = next_tkn%to_str()
                            end if

                            ! print *, "GOT TKNS: ", prev_tkn%val, ", ", next_tkn%val

                            if (prev_tkn%t < 6 .and. prev_tkn%t > 0 .and. next_tkn%t < 6 .and. next_tkn%t > 0) then
                                if ((prev_tkn%t == INTVAL .or. prev_tkn%t == FLOATVAL) .and. (next_tkn%t == INTVAL .or. next_tkn%t == FLOATVAL)) then
                                    ! print *, "INT AND FLOAT"
                                    ! print *, "LVL :", lvl
                                    if (lvl == 6) then
                                        print *, "LVL 6: ", last_tkns%arr(pos)%val
                                        if (last_tkns%arr(pos)%val == "*") then
                                            ! print *, "ADDING: vim_mult("//item1//","//item2//")"
                                            call new_tkns%add(prev_tkn%t, "vim_mult("//item1//","//item2//")")
                                            print *, "NEW TOKEN IS INTERNAL: ", new_tkns%arr(new_tkns%current-1)%val
                                            new_tkns%arr(new_tkns%current-1)%internal = .true.
                                            call consume_group(consumed, next_consumed, pos-1, pos, pos+1)
                                            ops = ops + 1
                                            pos = pos + 1
                                        else if (last_tkns%arr(pos)%val == "/") then
                                            ! print *, "ADDING: vim_div("//item1//","//item2//")"
                                            call new_tkns%add(prev_tkn%t, "vim_div("//item1//","//item2//")")
                                            new_tkns%arr(new_tkns%current-1)%internal = .true.
                                            call consume_group(consumed, next_consumed, pos-1, pos, pos+1)
                                            ops = ops + 1
                                            pos = pos + 1
                                        end if
                                    else if (lvl == 5) then
                                        print *, "LVL 5: ", last_tkns%arr(pos)%val
                                        if (last_tkns%arr(pos)%val == "+") then
                                            ! print *, "ADDING: vim_add("//item1//","//item2//")"
                                            call new_tkns%add(prev_tkn%t, "vim_add("//item1//","//item2//")")
                                            print *, "NEW TOKEN IS INTERNAL: ", new_tkns%arr(new_tkns%current-1)%val
                                            new_tkns%arr(new_tkns%current-1)%internal = .true.
                                            call consume_group(consumed, next_consumed, pos-1, pos, pos+1)
                                            ops = ops + 1
                                            pos = pos + 1
                                        else if (last_tkns%arr(pos)%val == "-") then
                                            call new_tkns%add(prev_tkn%t, "vim_sub("//item1//","//item2//")")
                                            print *, "ADDING: ", new_tkns%arr(new_tkns%current-1)%val
                                            new_tkns%arr(new_tkns%current-1)%internal = .true.
                                            call consume_group(consumed, next_consumed, pos-1, pos, pos+1)
                                            ops = ops + 1
                                            pos = pos + 1
                                        end if
                                    end if
                                end if
                            end if
                        end if
                        if (.not. is_consumed(pos - 1, consumed, next_consumed)) then
                            call new_tkns%add(prev_tkn%t, prev_tkn%val, prev_tkn%internal)
                        end if
                        if (.not. is_consumed(pos, consumed, next_consumed)) then
                            call new_tkns%add(last_tkns%arr(pos)%t, last_tkns%arr(pos)%val, last_tkns%arr(pos)%internal)
                        end if
                    end if
                end if
                if (.not. is_consumed(pos, consumed, next_consumed) .and. pos == last_tkns%current - 1) then
                    print *, "ADDING TKN: ", last_tkns%arr(pos)%val
                    call new_tkns%add(last_tkns%arr(pos)%t, last_tkns%arr(pos)%val, last_tkns%arr(pos)%internal)
                end if
                pos = pos + 1
            end do
            print *, "RESOLVED TOKENS: "
            do i = 1, new_tkns%current
                print *, "    > ", new_tkns%arr(i)%val
            end do
            call reset_consumed(consumed, next_consumed)
            if (ops == 0) then
                lvl = lvl - 1
            end if
            last_tkns = new_tkns
            call new_tkns%reset()
            ops = 0
            pos = 1
            next_consumed = 1
        end do

        if (new_tkns%current == 1) then
            val = last_tkns%arr(1)%val
            print *, "RESULT: ", last_tkns%arr(1)%val
        else
            do i = 1, new_tkns%current
                print *, new_tkns%arr(i)%val
            end do
            call raise_err("FAILED")
        end if
    end function
end module
