program examining_2011
    implicit none
    integer,parameter::years = 15, months = 12, lines = 2, stations = 9, depth = 400
    integer,parameter::l = 1, st = 9 
    real,parameter::length = 26., height = 10.
    real,dimension(years,months)::SSH_f,SSH_t,cSSH_f,cSSH_t,dot_y,plot_array
    integer::y,m,n,i
    real::dx,ymean

    dx = length/(12.*15.+1.)
    
    call fukauraSSH(SSH_f);call tappiSSH(SSH_t)
    call calibrated_fukauraSSH(cSSH_f);call calibrated_tappiSSH(cSSH_t)
    !データがある年の分の平均値をデータがないところへ入れる
    cSSH_f(2,11) = (sum(cSSH_f(1:15,11))/real(years-1))
    cSSH_t(11,2) = sum(cSSH_t(1:15,2))/real(years-2)

    ! calibratedの方から逆補正，SSPの影響を足す
    SSH_f(2,11) = cSSH_f(2,11) - (10163.-10130.)
    SSH_t(11,2) = cSSH_t(11,2) - (10177.-10130.)
    call plots(0.5,4.,13,'../Plots/SSH/timeseries_SSH_FandT_MAC2.ps')
    call symbol(8.,14.,0.6,('Time Series of SSH at Fukaura'),0.,len('time series of ssh at fukaura'))
    call create_box(length,height,3);call mod12_memori(12*15,0.2,length,0.,0.);call num_memori(1300.,1900.,12,2,0.4,-1,height,-90,0,0)

    do i = 1,2
        n = 1
        do y = 1,years
            do m = 1, months
                if(i==1)then;plot_array = SSH_f;else;plot_array = cSSH_f;call rgbk(1.,0.,0.);end if
                    dot_y(y,m) = (plot_array(y,m)-1300.)*height/600.
                call gmark(real(n)*dx,dot_y(y,m),0.1,1)
                if (m == 1 .and.y /=1 .and. plot_array(y,m)/=0. .and. plot_array(y-1,12)/=0.) then
                    call plot(real(n-1)*dx,dot_y(y-1,12),3);call plot(real(n)*dx,dot_y(y,1),2)
                else if(m /= 1 .and. plot_array(y,m)/=0. .and. plot_array(y,m-1)/=0.) then
                    call plot(real(n-1)*dx,dot_y(y,m-1),3);call plot(real(n)*dx,dot_y(y,m),2)
                else;end if

                if(i==1 .and. m ==6) then
                    call numberc(real(n)*dx,height+1.,0.4,real(y+2008),0.,-1)
                else; end if
                n = n+1
            end do
            ymean = sum(plot_array(y,1:12))/12.
            if(i==1) then; call newpen2(4);else;call newpen2(3);end if
            call plot(real(y-1)*12.*dx,(ymean-1300.)*height/600.,3);call plot(real(y)*12.*dx,(ymean-1300.)*height/600.,2)
            call newpen2(3)
            call rgbk(0.,0.,0.);call plot(real(y)*12.*dx,height,3);call plot(real(y)*12.*dx,0.,2)
        end do
    end do

    call newpage
    call symbol(8.,14.,0.6,('Time Series of SSH at Tappi'),0.,len('time series of ssh at tappi'))
    call create_box(length,height,3);call mod12_memori(12*15,0.2,length,0.,0.);call num_memori(800.,1300.,10,2,0.4,-1,height,-90,0,0)
    do i = 1,2
        n = 1
        do y = 1,years
            do m = 1, months
                if(i==1)then;plot_array = SSH_t;else;plot_array = cSSH_t;call rgbk(1.,0.,0.);end if
                    dot_y(y,m) = (plot_array(y,m)-800.)*height/500.
                call gmark(real(n)*dx,dot_y(y,m),0.1,1)
                if (m == 1 .and.y /=1 .and. plot_array(y,m)/=0. .and. plot_array(y-1,12)/=0.) then
                    call plot(real(n-1)*dx,dot_y(y-1,12),3);call plot(real(n)*dx,dot_y(y,1),2)
                else if(m /= 1 .and. plot_array(y,m)/=0. .and. plot_array(y,m-1)/=0.) then
                    call plot(real(n-1)*dx,dot_y(y,m-1),3);call plot(real(n)*dx,dot_y(y,m),2)
                else;end if

                if(i==1 .and. m ==6) then
                    call numberc(real(n)*dx,height+1.,0.4,real(y+2008),0.,-1)
                else; end if
                n = n+1
            end do
            ymean = sum(plot_array(y,1:12))/12.
            if(i==1) then; call newpen2(4);else;call newpen2(3);end if
            call plot(real(y-1)*12.*dx,(ymean-800.)*height/500.,3);call plot(real(y)*12.*dx,(ymean-800.)*height/500.,2)
            call newpen2(3)
            call rgbk(0.,0.,0.);call plot(real(y)*12.*dx,height,3);call plot(real(y)*12.*dx,0.,2)
        end do
    end do
    call plote
end program