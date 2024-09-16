program st1_mean_with_errorbars_by_depth
    implicit none
    integer,parameter::years = 15, months = 12, lines = 2, stations = 9, depth = 400
    integer,parameter::l = 1,st = 9     ! N Line Station 1(9 in program) ONLY!
    real,parameter::height = 6., width = 10.
    real,dimension(years,months,lines,stations,depth)::potemp_c5, sal_c5, sigma_c5,DH_array
    real,dimension(months,lines,stations,depth)::potemp_av, potemp_sd,potemp_sem, sal_av, sal_sd,sal_sem, sigma_av, sigma_sd,sigma_sem,DH_av,DH_sd,DH_sem
    real,dimension(years,months)::SSH_f,SSP_f,calibratedSSH_f,SSH_t,SSP_t,calibratedSSH_t
    real,dimension(months)::SSHav_f,SSHsem_f,SSHav_t,SSHsem_t
    integer,dimension(months)::SSHdata_f,SSHdata_t
    integer,dimension(months,lines,stations,depth)::potemp_data, sal_data, sigma_data, DH_data
    integer::m,d,reald,n,y
    real::plot_av,plot_sem,dx,r,g,b,plotavDH,plotsemDH,plotav_f,plotsem_f,plotav_t,plotsem_t,minDH,min_f,min_t

    dx = width/14.

    call calibrated_data51(potemp_c5,sal_c5)
    call create_sigma_array(potemp_c5,sal_c5,sigma_c5)
    call avsdsem_dataquan(potemp_c5,potemp_av,potemp_sd,potemp_sem,potemp_data)
    call avsdsem_dataquan(sal_c5,sal_av,sal_sd,sal_sem,sal_data)
    call avsdsem_dataquan(sigma_c5,sigma_av,sigma_sd,sigma_sem,sigma_data)
    call create_DH_array(sigma_c5,DH_array)
    call avsdsem_dataquan(DH_array,DH_av,DH_sd,DH_sem,DH_data)

    call fukauraSSH(SSH_f,SSP_f)
    call tappiSSH(SSH_t,SSP_t)
    ! do y = 1,years
    !     do m = 1,months
    !         call calibrate_SSH(SSH_f(y,m),SSP_f(y,m),calibratedSSH_f(y,m))
    !         call calibrate_SSH(SSH_t(y,m),SSP_t(y,m),calibratedSSH_t(y,m))
    !         print*,calibratedSSH_f(y,m),calibratedSSH_t(y,m)
    !     end do
    ! end do
    call avsem_dataquan3(calibratedSSH_f,SSHav_f,SSHsem_f,SSHdata_f)
    call avsem_dataquan3(calibratedSSH_t,SSHav_t,SSHsem_t,SSHdata_t)
    ! do m = 1, 12
    !     print*,'fukaura',SSHav_f(m),SSHsem_f(m),SSHdata_f(m)
    ! end do
    ! do m = 1, 12
    !     print*,'tappi',SSHav_t(m),SSHsem_t(m),SSHdata_t(m)
    ! end do

    ! call create_box(10.,6.,2);call plot(10.+3.,0.,-3)
    ! call create_box(10.,6.,2);call plot(-13.,-(6.+3.),-3)
    ! call create_box(10.,6.,2);call plot(10.+3.,0.,-3)
    ! call create_box(10.,6.,2) top left top right bottom left bottom right 

    call plots(2.5,16.,13,'/LARGE0/gr10291/nishimori2/aomori/Errorbar_plots/st1_TSsigmaDH.ps')
    call symbol(5.,2.5,0.8,'Mean with Error bars N-Line St.1',0.,len('Mean with Error bars N-Line St.1'))
    call plot(0.,-6.,-3)

! potemp
    ! call gmark(0.,0.,0.3,1)
    call create_box(width,height,3);call mod12_memori(13,0.5,width,0.,0.);call num_memori(0.,24.,24,5,0.45,1,height,-90,0,0)
    call symbolc(width/2.,height+0.6,0.6,'Potential Temp',0.,len('potential temp'))
        do n = 1, 13
            if(mod(n,12)/=0) then;m = mod(n,12)
        else;m = 12;end if
            do d = 1,4 !50,100,150,200 respectively
                reald = d*50
                plot_av = potemp_av(m,l,st,reald)*height/24.
                plot_sem = potemp_sem(m,l,st,reald)*height/24.
                if(d == 1) then;r = 1.;g = 0.;b = 0.
            else if(d == 2) then;r = 0.;g = 1.;b = 0.
            else if(d == 3) then;r = 0.;g = 0.;b = 1.
            else;r = 0.; g = 0.; b = 0.;end if
                call rgbk(r,g,b)
                call gmark(real(n)*dx-0.1+real(d-1)*0.2/3.,plot_av,0.1,1)
                call plot(real(n)*dx-0.1+real(d-1)*0.2/3.,plot_av-plot_sem,3);call plot(real(n)*dx-0.1+real(d-1)*0.2/3.,plot_av+plot_sem,2)
                
            end do
        end do

! sal
        call plot(10.+3.,0.,-3)
        ! call gmark(0.,0.,0.3,1)

        call create_box(width,height,3);call mod12_memori(13,0.5,width,0.,0.);call num_memori(34.4,33.7,35,5,0.45,2,height,-90,0,0)
        call symbolc(width/2.,height+0.6,0.6,'Salinity',0.,len('salinity'))
        do n = 1, 13
            if(mod(n,12)/=0) then;m = mod(n,12)
        else;m = 12;end if
            do d = 1,4 !50,100,150,200 respectively
                reald = d*50
                plot_av = height-(sal_av(m,l,st,reald)-33.7)*height/0.7
                plot_sem = (sal_sem(m,l,st,reald))*height/0.7
                if(d == 1) then;r = 1.;g = 0.;b = 0.
            else if(d == 2) then;r = 0.;g = 1.;b = 0.
            else if(d == 3) then;r = 0.;g = 0.;b = 1.
            else;r = 0.; g = 0.; b = 0.;end if
                call rgbk(r,g,b)
                call gmark(real(n)*dx-0.1+real(d-1)*0.2/3.,plot_av,0.1,1)
                call plot(real(n)*dx-0.1+real(d-1)*0.2/3.,plot_av-plot_sem,3);call plot(real(n)*dx-0.1+real(d-1)*0.2/3.,plot_av+plot_sem,2)
            end do
        end do

! sigma theta
        call plot(-13.,-(6.+3.),-3)
        ! call gmark(0.,0.,0.3,1)

        call create_box(width,height,3);call mod12_memori(13,0.5,width,0.,0.);call plot(0.,height,-3);call num_memori(24.0,27.2,32,5,0.45,1,-height,-90,0,0);call plot(0.,-height,-3)
        call symbolc(width/2.,height+0.6,0.6,'Density',0.,len('density'))
        do n = 1, 13
            if(mod(n,12)/=0) then;m = mod(n,12)
        else;m = 12;end if
            do d = 1,4 !50,100,150,200 respectively
                reald = d*50
                plot_av = height-(sigma_av(m,l,st,reald)-24.0)*height/3.2
                plot_sem = (sigma_sem(m,l,st,reald))*height/3.2
                if(d == 1) then;r = 1.;g = 0.;b = 0.
            else if(d == 2) then;r = 0.;g = 1.;b = 0.
            else if(d == 3) then;r = 0.;g = 0.;b = 1.
            else;r = 0.; g = 0.; b = 0.;end if
                call rgbk(r,g,b)
                call gmark(real(n)*dx-0.1+real(d-1)*0.2/3.,plot_av,0.1,1)
                call plot(real(n)*dx-0.1+real(d-1)*0.2/3.,plot_av-plot_sem,3);call plot(real(n)*dx-0.1+real(d-1)*0.2/3.,plot_av+plot_sem,2)
            end do
        end do

! Dynamic Height and fukaura and tappi
        ! do m = 1,12
        
        call plot(10.+3.,0.,-3)
        ! call gmark(0.,0.,0.3,1)

        call create_box(width,height,3);call mod12_memori(13,0.5,width,0.,0.);call num_memori(0.,30.,30,5,0.45,1,height,-90,0,0)
        call plot(width,0.,-3);call num_memori(0.,30.,30,5,0.45,1,height,90,0,0);call plot(-width,0.,-3)
        call symbolc(width/2.,height+0.6,0.5,'Dynamic Height (400db reference)',0.,len('dynamic height (400db reference)'))
        call symbolc(-1.3,height/2.,0.4,'(397.1[m]+[cm])',90.,len('(397.1[m]+[cm])'))
        call rgbk(1.,.4,0.)
        call symbolc(width+1.5,height*2./3.,0.3,'Fukaura',-90.,len('Fukaura'))
        call rgbk(0.,1.,1.)
        call symbolc(width+1.5,height*1./3.,0.3,'Tappi',-90.,len('Tappi'))
        minDH = minval(DH_av(1:12,1,9,400));min_f = minval(SSHav_f);min_t = minval(SSHav_t)
        print*,'minDH',minDH,'min_f',min_f,'min_t',min_t
        do n = 1, 13
            if(mod(n,12)/=0) then;m = mod(n,12)
        else;m = 12;end if
            ! do d = 1,4 !50,100,150,200 respectively
                reald = 400
                plotavDH = (DH_av(m,l,st,reald)-minDH)*height*100./30.
                plotsemDH = (DH_sem(m,l,st,reald))*height*100./30.
                plotav_f = ((SSHav_f(m)-min_f)/10.)*height/30.
                plotsem_f = (SSHsem_f(m)/10.)*height/30.
                plotav_t = ((SSHav_t(m)-min_t)/10.)*height/30.
                plotsem_t = (SSHsem_t(m)/10.)*height/30.
                print*,m,plotavDH
                print*,m,plotav_f
                print*,m,plotav_t
                call rgbk(.7,.3,1.)
                call gmark(real(n)*dx,plotavDH,0.1,1)
                call plot(real(n)*dx,plotavDH-plotsemDH,3);call plot(real(n)*dx,plotavDH+plotsemDH,2)
                call rgbk(1.,.4,0.)
                call gmark(real(n)*dx+0.05,plotav_f,0.1,1)
                call plot(real(n)*dx+0.05,plotav_f-plotsem_f,3);call plot(real(n)*dx+0.05,plotav_f+plotsem_f,2)
                call rgbk(0.,1.,1.)
                call gmark(real(n)*dx+0.1,plotav_t,0.1,1)
                call plot(real(n)*dx+0.1,plotav_t-plotsem_t,3);call plot(real(n)*dx+0.1,plotav_t+plotsem_t,2)
            ! end do
        end do



        
call plote 
end program


