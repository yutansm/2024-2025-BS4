program test
    integer::n,int
    real::re
    character(len=20)::str
    ! str = '相馬※1' !is 10 characters
    ! str = '※' ! 226 128 187 is the code
    ! str = '－－－' ! － is 239 188 141
    str = '*' ! is 42 
    print*,iachar('a')
    print*,iachar('9')
    print*,iachar('.')
    n = 1
    do while(str(n:n)/=' ')
        if(iachar(str(n:n)) <=127)then
            print*,'number',iachar(str(n:n))
        else
            print*,'not number',iachar(str(n:n))
        end if
        n = n + 1
    end do
    print*,n


    str = '1200.1'
    read(str,'(i4)',iostat=ios) int
    if(ios/=0)then
        print*,'failed',int
    else
        print*,'success',int
    end if




end program