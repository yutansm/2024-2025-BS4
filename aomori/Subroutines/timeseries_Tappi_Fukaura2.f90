program monthlymean_15years
    implicit none
    integer,parameter::years = 15, months = 12, lines = 2, stations = 9, depth = 400
    integer,parameter::l = 1, st = 9 
    real,parameter::length = 26., height = 10.
    real,dimension(years,months)::SSH_f,SSH_t,devSSH_f,devSSH_t,array
    real,dimension(years)::yearlyav_f,yearlyav_t
    integer,dimension(months)::dataSSH_f,dataSSH_t
    real,dimension(months)::monthlyav_f,monthlyav_t,semSSH_f,semSSH_t,dot_y
    integer::y,n,count,m
    real::dx,summation,mean_excluding_zeros

    call plots(.5,4.,13,'../Errorbar_plots/yearlymean_SSH_FandT_MAC.ps')
    call symbol(6.,14.,0.6,('Time Series of SSH at Fukaura (yearly mean)'),0.,len('time series of ssh at fukaura (yearly mean)')) 
    ! call num_memori(-200.,200.,8,1,0.4,-1,height,-90,0.0)

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
    do y = 1,15
        print*,yearlyav_f(y),yearlyav_t(y)
    end do
    !getting deviation from yearly mean
    do y = 1,years
        do m = 1, months
            if(SSH_f(y,m) == 0.0) then
                devSSH_f(y,m) = 0.0
            else if(SSH_t(y,m) == 0.0) then
                devSSH_t(y,m) = 0.0
            else
            devSSH_f(y,m) = SSH_f(y,m) - yearlyav_f(y)
            devSSH_t(y,m) = SSH_t(y,m) - yearlyav_t(y)
            end if
        end do
    end do
    print*,devSSH_f(1,:)

! where math ends

    call create_box(length,height,3);call mod12_memori(12*15,0.2,length,0.,0.)
    call plot(0.,height/2.,3);call plot(length,height/2.,2)
    call avsem_dataquan3(devSSH_f,monthlyav_f,semSSH_f,dataSSH_f)
    ! print*, dataSSH_f

    ! do y = 1,years
    !         dot_y(y) = (devSSH_f(y,m)-400.)*height/400.
    !         call gmark(real(y)*dx,dot_y(y),0.1,1)
    !         call numberc(real(y)*dx,-0.5,0.4,real(y+2008),0.,-1)
    !         if(y/=1 .and.dot_y(y)/=0. .and. dot_y(y-1)/=0.) then
    !             call plot(real(y-1)*dx,dot_y(y-1),3);call plot(real(y)*dx,dot_y(y),2)
    !         end if
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
