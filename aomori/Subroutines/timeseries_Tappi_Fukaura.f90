program examining_2011
    implicit none
    integer,parameter::years = 15, months = 12, lines = 2, stations = 9, depth = 400
    integer,parameter::l = 1, st = 9
    real,parameter::length = 26., height = 10.
    real,dimension(years,months)::SSH_f,SSH_t,dot_y
    integer::y,m,n
    real::dx

    dx = length/(12.*15.+1.)

    call calibrated_fukauraSSH(SSH_f);call calibrated_tappiSSH(SSH_t)

    call plots(0.5,4.,13,'/LARGE0/gr10291/nishimori2/aomori/Errorbar_plots/timeseries_SSH_FandT.ps')
    call symbol(8.,14.,0.6,('Time Series of SSH at Fukaura'),0.,len('time series of ssh at fukaura'))
    call create_box(length,height,3);call mod12_memori(12*15,0.2,length,0.,0.);call num_memori(1400.,2000.,12,2,0.4,-1,height,-90,0,0)

    n = 1
    do y = 1,years
        do m = 1, months
            dot_y(y,m) = (SSH_f(y,m)-1400.)*height/600.
            call gmark(real(n)*dx,dot_y(y,m),0.1,1)
            if (m == 1 .and.y /=1 .and. SSH_f(y,m)/=0. .and. SSH_f(y-1,12)/=0.) then
                call plot(real(n-1)*dx,dot_y(y-1,12),3);call plot(real(n)*dx,dot_y(y,1),2)
            else if(m /= 1 .and. SSH_f(y,m)/=0. .and. SSH_f(y,m-1)/=0.) then
                call plot(real(n-1)*dx,dot_y(y,m-1),3);call plot(real(n)*dx,dot_y(y,m),2)
            else;end if

            if(m ==6) then; call numberc(real(n)*dx,height+1.,0.4,real(y+2008),0.,-1)
        else; end if
            n = n+1
        end do
    end do

    call newpage
    call symbol(8.,14.,0.6,('Time Series of SSH at Tappi'),0.,len('time series of ssh at tappi'))
    call create_box(length,height,3);call mod12_memori(12*15,0.2,length,0.,0.);call num_memori(800.,1300.,10,2,0.4,-1,height,-90,0,0)

    n = 1
    do y = 1,years
        do m = 1, months
            dot_y(y,m) = (SSH_t(y,m)-800.)*height/500.
            call gmark(real(n)*dx,dot_y(y,m),0.1,1)
            if (m == 1 .and.y /=1 .and. SSH_t(y,m)/=0. .and. SSH_t(y-1,12)/=0.) then
                call plot(real(n-1)*dx,dot_y(y-1,12),3);call plot(real(n)*dx,dot_y(y,1),2)
            else if(m /= 1 .and. SSH_t(y,m)/=0. .and. SSH_t(y,m-1)/=0.) then
                call plot(real(n-1)*dx,dot_y(y,m-1),3);call plot(real(n)*dx,dot_y(y,m),2)
            else;end if

            if(m ==6) then; call numberc(real(n)*dx,height+1.,0.4,real(y+2008),0.,-1)
        else; end if
            n = n+1
        end do
    end do
    call plote
end program