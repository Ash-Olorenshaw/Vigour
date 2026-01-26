module resolver_expressions
    use tokeniser_types, only: tkn_line, tkn
    implicit none
    private
    public :: resolve_tkn_line
contains

    subroutine consume(consumed, next_consumed, item)
        integer, intent(inout) :: consumed(1024), next_consumed
        integer, intent(in) :: item
        consumed(next_consumed) = item
        next_consumed = next_consumed + 1
    end subroutine

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

    function identifier_matches(tkn1, tkn2, test) result(matches)
        use resolver_globals, only: var_exists, get_var_name, get_var_type
        use tokeniser_types, only: IDENTIFIER
        integer, intent(in) :: test(:)
        type(tkn), intent(in) :: tkn1, tkn2
        character(:), allocatable :: var_name
        integer :: i, scope
        logical :: matches, match1, match2
        matches = .false.

        do i = 1, size(test)
            if (tkn1%t == test(i)) then
                match1 = .true.
            else
                if (tkn1%t == IDENTIFIER) then
                    var_name = get_var_name(tkn1%val, scope=scope)
                    if (var_exists(var_name, scope) .and. get_var_type(var_name, scope) == test(i)) then
                        match1 = .true.
                    end if
                end if
            end if
            if (tkn2%t == test(i)) then
                match2 = .true.
            else
                if (tkn2%t == IDENTIFIER) then
                    var_name = get_var_name(tkn2%val, scope=scope)
                    if (var_exists(var_name, scope) .and. get_var_type(var_name, scope) == test(i)) then
                        match2 = .true.
                    end if
                end if
            end if
        end do

        matches = match1 .eqv. match2
    end function

    function resolve_tkn_val(tgt_tkn) result(val) 
        use resolver_globals, only: var_exists, get_var_name
        use tokeniser_types, only: IDENTIFIER, KEYWORD, OPERATOR
        use utils_core, only: raise_err
        type(tkn), intent(in) :: tgt_tkn
        character(:), allocatable :: val, var_name
        integer :: scope

        print *, "RESOLVING TKN VAL..."

        if (tgt_tkn%t == OPERATOR .or. tgt_tkn%t == KEYWORD) then
            call raise_err("Orphaned operator: '"//tgt_tkn%val//"'")
        else if (tgt_tkn%internal) then
            print *, "INTERNAL: ", tgt_tkn%val
            val = tgt_tkn%val
        else if (tgt_tkn%t == IDENTIFIER) then
            print *, "IDENTIFIER: ", tgt_tkn%val
            val = get_var_name(tgt_tkn%val, scope=scope)
            if (var_exists(val, scope)) then
                return
            else
                call raise_err("Could not find referenced var: '"//tgt_tkn%val//"'")
            end if
        else
            print *, "ITEM1 NOT INTERNAL: ", tgt_tkn%to_str()
            val = tgt_tkn%to_str()
        end if
    end function

    function resolve_tkn_line(tkns, start) result(resolved_tkns)
        use tokeniser_types, only: EXPRESSION, INTVAL, STRINGVAL, FLOATVAL, LISTVAL, DICTVAL, &
            OPERATOR, IDENTIFIER, FUNCTIONCALL, KEYWORD, t_to_str
        use resolver_globals, only: var_exists, get_var_name, get_var_type
        use utils_types, only: alloc_str_arr
        use utils_core, only: raise_err
        type(tkn_line), intent(in) :: tkns
        integer, intent(in) :: start
        type(tkn_line) :: new_tkns, last_tkns, resolved_tkns
        integer :: pos, lvl, var_scope, ops, consumed(1024), next_consumed, i
        character(:), allocatable :: item1, item2, var_name, new_tkn_val
        type(tkn) :: next_tkn, prev_tkn
        logical :: generated_new_tkn, new_tkn_internal

        ops = 0
        lvl = 7
        last_tkns = tkns 
        call last_tkns%remove(start)
        new_tkns%size = tkns%size
        allocate(new_tkns%arr(tkns%size))
        pos = 1
        next_consumed = 1
        generated_new_tkn = .false.

        do while(lvl > 0)
            print *, "STARTING LOOP: ", pos, last_tkns%current - 1
            do while(pos < last_tkns%current)
                if (last_tkns%arr(pos)%t == OPERATOR) then
                    if (pos > 1) then
                        if (pos < last_tkns%current - 1) then
                            prev_tkn = last_tkns%arr(pos - 1)
                            next_tkn = last_tkns%arr(pos + 1)
                            item1 = resolve_tkn_val(prev_tkn)
                            item2 = resolve_tkn_val(next_tkn)

                            if ((.not. is_consumed(pos, consumed, next_consumed)) .and. (.not. is_consumed(pos - 1, consumed, next_consumed)) &  
                                .and. (.not. is_consumed(pos + 1, consumed, next_consumed))) then
                                print *, "Not consumed"
                                if (lvl == 6) then
                                    print *, "LVL 6: ", last_tkns%arr(pos)%val
                                    if (last_tkns%arr(pos)%val == "*") then
                                        if (.not. identifier_matches(prev_tkn, next_tkn, [FLOATVAL, STRINGVAL, INTVAL])) &
                                            call raise_err("Incorrect types for '*' operation")
                                        new_tkn_val = "vim_mult("//item1//","//item2//")"
                                        generated_new_tkn = .true.
                                    else if (last_tkns%arr(pos)%val == "/") then
                                        if (.not. identifier_matches(prev_tkn, next_tkn, [FLOATVAL, STRINGVAL, INTVAL])) &
                                            call raise_err("Incorrect types for '/' operation")
                                        new_tkn_val = "vim_div("//item1//","//item2//")"
                                        generated_new_tkn = .true.
                                    end if
                                else if (lvl == 5) then
                                    print *, "LVL 5: ", last_tkns%arr(pos)%val
                                        if (last_tkns%arr(pos)%val == "+") then
                                            if (.not. identifier_matches(prev_tkn, next_tkn, [FLOATVAL, STRINGVAL, INTVAL])) &
                                                call raise_err("Incorrect types for '+' operation")
                                            new_tkn_val = "vim_add("//item1//","//item2//")"
                                            generated_new_tkn = .true.
                                        else if (last_tkns%arr(pos)%val == "-") then
                                            if (.not. identifier_matches(prev_tkn, next_tkn, [FLOATVAL, STRINGVAL, INTVAL])) &
                                                call raise_err("Incorrect types for '-' operation")
                                            new_tkn_val = "vim_sub("//item1//","//item2//")"
                                            generated_new_tkn = .true.
                                        end if
                                else if (lvl == 4) then
                                    print *, "LVL 4: ", last_tkns%arr(pos)%val//" "//t_to_str(prev_tkn%t)//" "//t_to_str(next_tkn%t)
                                    if (last_tkns%arr(pos)%val == "==") then
                                        new_tkn_val = "vim_eq("//item1//","//item2//",CASE_SENSITIVE)"
                                        generated_new_tkn = .true.
                                    else if (last_tkns%arr(pos)%val == "!=") then
                                        new_tkn_val = "vim_ne("//item1//","//item2//",CASE_SENSITIVE)"
                                        generated_new_tkn = .true.
                                    else if (identifier_matches(prev_tkn, next_tkn, [FLOATVAL, STRINGVAL, INTVAL])) then
                                        print *, "int vs int (float or string)"
                                        if (last_tkns%arr(pos)%val == "<") then
                                            if (.not. identifier_matches(prev_tkn, next_tkn, [FLOATVAL, STRINGVAL, INTVAL])) &
                                                call raise_err("Incorrect types for '<' comparison")
                                            new_tkn_val = "vim_lt("//item1//","//item2//",CASE_SENSITIVE)"
                                            generated_new_tkn = .true.
                                        else if (last_tkns%arr(pos)%val == ">") then
                                            if (.not. identifier_matches(prev_tkn, next_tkn, [FLOATVAL, STRINGVAL, INTVAL])) &
                                                call raise_err("Incorrect types for '>' comparison")
                                            new_tkn_val = "vim_gt("//item1//","//item2//",CASE_SENSITIVE)"
                                            generated_new_tkn = .true.
                                        else if (last_tkns%arr(pos)%val == ">=") then
                                            if (.not. identifier_matches(prev_tkn, next_tkn, [FLOATVAL, STRINGVAL, INTVAL])) &
                                                call raise_err("Incorrect types for '>=' comparison")
                                            new_tkn_val = "vim_ge("//item1//","//item2//",CASE_SENSITIVE)"
                                            generated_new_tkn = .true.
                                        else if (last_tkns%arr(pos)%val == "<=") then
                                            if (.not. identifier_matches(prev_tkn, next_tkn, [FLOATVAL, STRINGVAL, INTVAL])) &
                                                call raise_err("Incorrect types for '<=' comparison")
                                            new_tkn_val = "vim_le("//item1//","//item2//",CASE_SENSITIVE)"
                                            generated_new_tkn = .true.
                                        end if
                                    end if
                                end if
                            end if

                            if (generated_new_tkn) then
                                call new_tkns%add(prev_tkn%t, new_tkn_val)
                                new_tkns%arr(new_tkns%current-1)%internal = .true.
                                call consume_group(consumed, next_consumed, pos-1, pos, pos+1)
                                print *, "ADDING: ", new_tkns%arr(new_tkns%current-1)%val
                                ops = ops + 1
                                pos = pos + 1
                                generated_new_tkn = .false.
                            end if
                        end if
                        if (.not. is_consumed(pos - 1, consumed, next_consumed)) then
                            call new_tkns%add(prev_tkn%t, prev_tkn%val, prev_tkn%internal)
                        end if
                    end if
                end if

                if (.not. is_consumed(pos, consumed, next_consumed) .and. &
                        ((last_tkns%arr(pos)%t /= OPERATOR .and. last_tkns%arr(pos+1)%t /= OPERATOR) .or. &
                        last_tkns%arr(pos)%t == OPERATOR .or. &
                        pos == last_tkns%current - 1) &
                    ) then
                    new_tkn_val = last_tkns%arr(pos)%val
                    new_tkn_internal = last_tkns%arr(pos)%internal
                    if (last_tkns%arr(pos)%t == IDENTIFIER) then
                        print *, "IDENTIFIER"
                        var_name = get_var_name(new_tkn_val, scope=var_scope)
                        if (var_exists(var_name, var_scope)) then
                            new_tkn_val = var_name
                            new_tkn_internal = .true.
                        end if
                    end if
                    print *, "ADDING TKN: ", new_tkn_val
                    call new_tkns%add(last_tkns%arr(pos)%t, new_tkn_val, new_tkn_internal)
                    call consume(consumed, next_consumed, pos)
                end if

                print *, "INCREMENT POS"
                pos = pos + 1
            end do
            call reset_consumed(consumed, next_consumed)
            last_tkns = new_tkns
            call new_tkns%reset()
            if (ops == 0) &
                lvl = lvl - 1
            ops = 0
            pos = 1
            next_consumed = 1
        end do

        do i = 1, last_tkns%current
            print *, last_tkns%arr(i)%val
        end do

        resolved_tkns = last_tkns
    end function
end module
