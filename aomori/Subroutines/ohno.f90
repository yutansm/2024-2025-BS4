program ohno
    implicit none
    integer,parameter::years = 15, months = 12, lines = 2, stations = 9, depth = 400
    integer,parameter::l = 1, st = 9 
    real,parameter::length = 26., height = 10.
    real,dimension(years,months)::SSH_f,SSH_t,semSSH_f,semSSH_t,devSSH_f,devSSH_t,array
    integer,dimension(years,months)::dataSSH_f,dataSSH_t
    real,dimension(years)::avSSH_f,avSSH_t,dot_y
    integer::y,n,count,m
    real::dx,summation,mean_excluding_zeros

    dx = length/(15.+1.)
    call plots(5.,5.,13,'test.ps')
    call create_box(length,height,3);call create_box(length/2.,height,3)
    

    call calibrated_fukauraSSH(SSH_f);call calibrated_tappiSSH(SSH_t)
    !getting mean of each year
    do n = 1,2 
        if (n==1) then
            array= SSH_f
        else; array= SSH_t
        end if
        do y = 1, years
            summation = 0.0
            count = 0
            do m = 1, months
                if (array(y, m) /= 0.0) then
                    summation = summation + array(y, m)
                    count = count + 1
                end if
            end do ! added all months of a year
            if (count > 0) then
                mean_excluding_zeros = summation/real(count)
            else
                mean_excluding_zeros = 0.0
            end if
            if(n==1) then
                avSSH_f(y) = mean_excluding_zeros
            else;avSSH_t(y) = mean_excluding_zeros
            end if
        end do
    end do
    do y = 1,15
        print*,avSSH_f(y),avSSH_t(y)
    end do
    !getting deviation from yearly mean
    do y = 1,years
        do m = 1, months
            if(SSH_f(y,m) == 0.0) then
                devSSH_f(y,m) = 0.0
            else if(SSH_t(y,m) == 0.0) then
                devSSH_t(y,m) = 0.0
            else
            devSSH_f(y,m) = SSH_f(y,m) - avSSH_f(y)
            devSSH_t(y,m) = SSH_t(y,m) - avSSH_t(y)
            end if
        end do
    end do
    ! print*,devSSH_f

    ! call plots(5.,5.,13,'test.ps')
    ! call create_box(length,height,3)
    call plote
end program
