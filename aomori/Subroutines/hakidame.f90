program examining_2011
    implicit none
    integer,parameter::years = 15, months = 12, lines = 2, stations = 9, depth = 400
    integer,parameter::l = 1, st = 9 
    real,parameter::length = 26., height = 10.
    real,dimension(years,months)::SSH_f,SSH_t,dot_y,array,devSSH_f,devSSH_t
    real,dimension(years)::avSSH_f,avSSH_t
    integer::y,m,n,count=0
    real::dx,summmation=0.,mean_excluding_zeros,a,b,c

    dx = length/(12.*15.+1.)
    

    call calibrated_fukauraSSH(SSH_f);call calibrated_tappiSSH(SSH_t)
    !getting mean of each year
    do n = 1,2 
        if (n==1) then
            array= SSH_f
        else; array= SSH_t
        end if
        do y = 1, years
            summmation = 0.0
            count = 0
            do m = 1, months
                if (array(y, m) /= 0.0) then
                    summmation = summmation + array(y, m)
                    count = count + 1
                end if
            end do
            if (count > 0) then
                mean_excluding_zeros = summmation/real(count)
            else
                mean_excluding_zeros = 0.0
            end if
            if(n==1) then
                avSSH_f(y) = mean_excluding_zeros
            else;avSSH_t(y) = mean_excluding_zeros
            end if
        end do
    end do
    do y = 1,12
        ! write(*,*)avSSH_f(y),avSSH_t(y)
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
    ! print*, devSSH_f

    ! a =sum(SSH_f(2,1:months)); b=sum(SSH_f(2,1:months)) / 12.0; c=sum(SSH_f(2,1:months)) / 11.0   
    ! print *, a,b,c
    

    call plots(0.5,4.,13,'../Errorbar_plots/timeseries_SSH_FandT_MAC2.ps')
    call symbol(4.,14.,0.6,'Time Series of SSH at Fukaura (dev from yearly mean)',0.,len('Time Series of SSH at Fukaura (dev from yearly mean)'))
    call create_box(length,height,3);call mod12_memori(12*15,0.2,length,0.,0.);call num_memori(-200.,200.,40,5,0.4,-1,height,-90,0,0)

    ! n = 1
    ! do y = 1,years
    !     do m = 1, months
    !         dot_y(y,m) = (SSH_f(y,m)-1400.)*height/600.
    !         call gmark(real(n)*dx,dot_y(y,m),0.1,1)
    !         if (m == 1 .and.y /=1 .and. SSH_f(y,m)/=0. .and. SSH_f(y-1,12)/=0.) then
    !             call plot(real(n-1)*dx,dot_y(y-1,12),3);call plot(real(n)*dx,dot_y(y,1),2)
    !         else if(m /= 1 .and. SSH_f(y,m)/=0. .and. SSH_f(y,m-1)/=0.) then
    !             call plot(real(n-1)*dx,dot_y(y,m-1),3);call plot(real(n)*dx,dot_y(y,m),2)
    !         else;end if

    !         if(m ==6) then; call numberc(real(n)*dx,height+1.,0.4,real(y+2008),0.,-1)
    !     else; end if
    !         n = n+1
    !     end do
    ! end do

    ! call newpage
    ! call symbol(8.,14.,0.6,('Time Series of SSH at Tappi'),0.,len('time series of ssh at tappi'))
    ! call create_box(length,height,3);call mod12_memori(12*15,0.2,length,0.,0.);call num_memori(800.,1300.,10,2,0.4,-1,height,-90,0,0)

    ! n = 1
    ! do y = 1,years
    !     do m = 1, months
    !         dot_y(y,m) = (SSH_t(y,m)-800.)*height/500.
    !         call gmark(real(n)*dx,dot_y(y,m),0.1,1)
    !         if (m == 1 .and.y /=1 .and. SSH_t(y,m)/=0. .and. SSH_t(y-1,12)/=0.) then
    !             call plot(real(n-1)*dx,dot_y(y-1,12),3);call plot(real(n)*dx,dot_y(y,1),2)
    !         else if(m /= 1 .and. SSH_t(y,m)/=0. .and. SSH_t(y,m-1)/=0.) then
    !             call plot(real(n-1)*dx,dot_y(y,m-1),3);call plot(real(n)*dx,dot_y(y,m),2)
    !         else;end if

    !         if(m ==6) then; call numberc(real(n)*dx,height+1.,0.4,real(y+2008),0.,-1)
    !     else; end if
    !         n = n+1
    !     end do
    ! end do
  
    call plote
end program
