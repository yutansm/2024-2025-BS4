program monthlymean_15years
    implicit none
    integer,parameter::years = 15, months = 12, lines = 2, stations = 9, depth = 400
    real,parameter::length = 24., height = 10.
    real,dimension(years,months)::SSH_f,SSH_t,devSSH_f=0.,devSSH_t,array
    real,dimension(years)::yearlyav_f,yearlyav_t
    integer,dimension(months)::dataSSH_f,dataSSH_t
    real,dimension(months)::monthlyav_f,monthlyav_t,semSSH_f,semSSH_t,dot_y
    integer::y,n,count,m
    real::dx,summation,mean_excluding_zeros,sem

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
                yearlyav_f(y) = mean_excluding_zeros
            else;yearlyav_t(y) = mean_excluding_zeros
            end if
        end do
    end do
    do y = 1,years
        ! print*,yearlyav_f(y),yearlyav_t(y)
    end do
    !getting deviation from yearly mean
    do y = 1,years
        do m = 1, months
            if(SSH_f(y,m) == 0.0) then
                devSSH_f(y,m) = 0.0
            else
                devSSH_f(y,m) = SSH_f(y,m) - yearlyav_f(y)
            end if
            if(SSH_t(y,m) == 0.0) then
                devSSH_t(y,m) = 0.0
            else
                devSSH_t(y,m) = SSH_t(y,m) - yearlyav_t(y)
            end if
        end do
    end do
    ! print*,devSSH_f(1:3,1:12)

! where math ends

    call plots(2.5,4.,13,'../Errorbar_plots/monthlymean_SSH_FandT_MAC.ps')
    call symbol(1.,13.,.8,('Monthly Time Series of SSH at Fukaura (15y mean)'),0.,len('Monthly time series of ssh at fukaura (15y mean)')) 
    call num_memori(-200.,200.,8,1,0.6,-1,height,-90,0,0)
    call create_box(length,height,4);call mod12_memori(13,1.,length,0.,0.)
    call plot(0.,height/2.,3);call plot(length,height/2.,2)
    call avsem_dataquan3(devSSH_f,monthlyav_f,semSSH_f,dataSSH_f)
    call symbolc(-1.5,height/2.,0.6,'diff from mean (mm)',90.,len('diff from mean (mm)'))
    ! print*,monthlyav_f,semSSH_f,dataSSH_f
    ! SAKUZU
    dx = length/14.
    call symbolc(0.,0.6+height,0.6,'n',0.,1)
    do n = 1,13
        if(n>12) then;m = mod(n,12);else;m = n
        end if
            dot_y(m) = (monthlyav_f(m)-(-200.))*height/400.
            sem = semSSH_f(m)*height/400.
            ! print*,real(n)
            call gmark(real(n)*dx,dot_y(m),0.1,1);call numberc(real(n)*dx,0.6+height,0.6,real(dataSSH_f(m)),0.,-1)
            call plot(real(n)*dx,dot_y(m)-sem,3);call plot(real(n)*dx,dot_y(m)+sem,2)
            if(n/=1 .and. n/=13)then
                call plot(real(n-1)*dx,dot_y(m-1),3);call plot(real(n)*dx,dot_y(m),2)
            else if (n==13) then
                call plot(real(n-1)*dx,dot_y(12),3);call plot(real(n)*dx,dot_y(1),2)
            end if
    end do
    
   
   

    call newpage
    call symbol(1.,13.,.8,('Monthly Time Series of SSH at Tappi (15y mean)'),0.,len('Monthly time series of ssh at tappi (15y mean)')) 
    call num_memori(-200.,200.,8,1,0.6,-1,height,-90,0,0)
    call create_box(length,height,4);call mod12_memori(13,1.,length,0.,0.)
    call plot(0.,height/2.,3);call plot(length,height/2.,2)
    call avsem_dataquan3(devSSH_t,monthlyav_t,semSSH_t,dataSSH_t)
    call symbolc(-1.5,height/2.,0.6,'diff from mean (mm)',90.,len('diff from mean (mm)'))
    ! print*,monthlyav_t,semSSH_t,dataSSH_t
    call symbolc(0.,0.6+height,0.6,'n',0.,1)
    do n = 1,13
        if(n>12) then;m = mod(n,12);else;m = n
        end if
            dot_y(m) = (monthlyav_t(m)-(-200.))*height/400.
            sem = semSSH_t(m)*height/400.
            ! print*,real(n)
            call gmark(real(n)*dx,dot_y(m),0.1,1);call numberc(real(n)*dx,0.6+height,0.6,real(dataSSH_t(m)),0.,-1)
            call plot(real(n)*dx,dot_y(m)-sem,3);call plot(real(n)*dx,dot_y(m)+sem,2)
            if(n/=1 .and. n/=13)then
                call plot(real(n-1)*dx,dot_y(m-1),3);call plot(real(n)*dx,dot_y(m),2)
            else if (n==13) then
                call plot(real(n-1)*dx,dot_y(12),3);call plot(real(n)*dx,dot_y(1),2)
            end if
    end do
    call plote
end program
