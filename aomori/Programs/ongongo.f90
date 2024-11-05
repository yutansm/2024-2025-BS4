program test_index
    ! use always
    implicit none
    character(len=40) :: str
    integer :: pos_first, pos_last,n,i,input,j
    integer,dimension(5)::a = (/1,2,0,4,-1/)

    str = "Fortran, programmin, and traning"
    
    ! Find the first occurrence of "tran"
    pos_first = INDEX(str(12:), ",")
    print *, "First occurrence of 'tran' starts at position:", pos_first

    ! Find the last occurrence of "an" using BACK argument
    ! pos_last = INDEX(str, "an" ,back=.true.)
    ! print *, "Last occurrence of 'an' starts at position:", pos_last
    ! n = 1;j=1
    ! do 
    !     input = a(j)
    !     if(input == 0)then
    !         j = j+1; cycle
    !     elseif(input > 0) then
    !         n = n + 1
    !     elseif(input < 0) then
    !         exit
    !     end if
    !     j = j+1
    ! end do
    ! print*,n

    do
        end_pos = index(line(start_pos:), ',')
        if (end_pos > limit) then;print*,'end_pos > limit',line(start_pos:start_pos+end_pos-2)
            num_tokens = num_tokens + 1
            if (num_tokens > size(tokens)) then
                print *, '1 Error: num_tokens exceeds allocated size of tokens array'
                stop
            end if
            tokens(num_tokens) = adjustl(line(start_pos:start_pos+end_pos-2))
            start_pos = start_pos + end_pos
            ! print*,tokens(num_tokens)
        else if(end_pos == 2)then
            num_tokens = num_tokens + 1
            if (num_tokens > size(tokens)) then
                print *, '2 Error: num_tokens exceeds allocated size of tokens array'
                stop
            end if
            tokens(num_tokens) = adjustl(line(start_pos:start_pos))
            start_pos = start_pos + end_pos
        else if (end_pos == 1) then
            start_pos = start_pos + end_pos  ! skip the commas and 40s and 9s (40s and 9s are assigned to values with uncertain data)
        else if (end_pos == limit - 1) then;print*,'end_pos == limit - 1',line(start_pos:limit-1)
            if (line(start_pos:start_pos) == '9') then
                start_pos = start_pos + end_pos  ! skip the commas and 40s and 9s (40s and 9s are assigned to values with uncertain data)
            else
                num_tokens = num_tokens + 1
                if (num_tokens > size(tokens)) then
                    print *, '2 Error: num_tokens exceeds allocated size of tokens array'
                    stop
                end if
                tokens(num_tokens) = adjustl(line(start_pos:start_pos+end_pos-2))
                start_pos = start_pos + end_pos
            end if
        else if (end_pos == limit) then
            if (line(start_pos:start_pos+1) == '40') then
                start_pos = start_pos + end_pos  ! skip the commas and 40s and 9s (40s and 9s are assigned to values with uncertain data)
            else
                num_tokens = num_tokens + 1
                if (num_tokens > size(tokens)) then
                    print *, '3 Error: num_tokens exceeds allocated size of tokens array'
                    stop
                end if
                tokens(num_tokens) = adjustl(line(start_pos:start_pos+end_pos-2))
                start_pos = start_pos + end_pos
            end if
        else if (end_pos == 0) then! where the last token is reached
            num_tokens = num_tokens + 1
            if (num_tokens > size(tokens)) then
                print *, '4 Error: num_tokens exceeds allocated size of tokens array'
                stop
            end if
            tokens(num_tokens) = adjustl(line(start_pos:))
            exit
        end if
        ! print *, 'Token:', tokens(num_tokens)
    end do

    do
        end_pos = index(line(start_pos:), ',')
        print *, 'start_pos:', start_pos, 'end_pos:', end_pos, 'limit:', limit
    
        if (end_pos > limit) then
            print *, 'end_pos > limit', line(start_pos:start_pos+end_pos-2)
            num_tokens = num_tokens + 1
            if (num_tokens > size(tokens)) then
                print *, '1 Error: num_tokens exceeds allocated size of tokens array'
                stop
            end if
            tokens(num_tokens) = adjustl(line(start_pos:start_pos+end_pos-2))
            start_pos = start_pos + end_pos
        else if (end_pos == 2) then
            num_tokens = num_tokens + 1
            if (num_tokens > size(tokens)) then
                print *, '2 Error: num_tokens exceeds allocated size of tokens array'
                stop
            end if
            tokens(num_tokens) = adjustl(line(start_pos:start_pos))
            start_pos = start_pos + end_pos
        else if (end_pos == 1) then
            start_pos = start_pos + end_pos  ! skip the commas and 40s and 9s (40s and 9s are assigned to values with uncertain data)
        else if (end_pos == limit - 1) then
            print *, 'end_pos == limit - 1', line(start_pos:limit-1)
            if (line(start_pos:start_pos) == '9') then
                start_pos = start_pos + end_pos  ! skip the commas and 40s and 9s (40s and 9s are assigned to values with uncertain data)
            else
                num_tokens = num_tokens + 1
                if (num_tokens > size(tokens)) then
                    print *, '2 Error: num_tokens exceeds allocated size of tokens array'
                    stop
                end if
                tokens(num_tokens) = adjustl(line(start_pos:start_pos+end_pos-2))
                start_pos = start_pos + end_pos
            end if
        else if (end_pos == limit) then
            if (line(start_pos:start_pos+1) == '40') then
                start_pos = start_pos + end_pos  ! skip the commas and 40s and 9s (40s and 9s are assigned to values with uncertain data)
            else
                num_tokens = num_tokens + 1
                if (num_tokens > size(tokens)) then
                    print *, '3 Error: num_tokens exceeds allocated size of tokens array'
                    stop
                end if
                tokens(num_tokens) = adjustl(line(start_pos:start_pos+end_pos-2))
                start_pos = start_pos + end_pos
            end if
        else if (end_pos == 0) then  ! where the last token is reached
            num_tokens = num_tokens + 1
            if (num_tokens > size(tokens)) then
                print *, '4 Error: num_tokens exceeds allocated size of tokens array'
                stop
            end if
            tokens(num_tokens) = adjustl(line(start_pos:))
            exit
        else
            print *, 'Unhandled case: end_pos =', end_pos
            exit
        end if
        print *, 'Token:', tokens(num_tokens)
    end do
    

end program test_index