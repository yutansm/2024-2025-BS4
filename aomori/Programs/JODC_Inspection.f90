program gomibako
    use always
    implicit none 
    type(JODC_TS)::temp,sal
    type(JODC_RHO)::den,dh,mdiff_dh
    character(len=100)::title
    real::ratio,height,width = 5.,width2 = 4.,min
    integer::ini_lat = 32,fin_lat = 46,ini_long = 126,fin_long = 142,dep,month
    real,dimension(:),allocatable::r1,g1,b1,r2,g2,b2,r3,g3,b3
    logical::stat
    
    allocate(mdiff_dh%mean(12,181,361,33))
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                    ! Data Obtainment
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        call JODC_data2(potemp = temp,sal = sal,den = den,dh = dh,info = .true.)
        print*,size(temp%mean)
        ! open(21,file = '../Data/JODC/JODC_DATA.bin',form = 'unformatted',access = 'direct',recl = 13*181*361*33*4,status = 'replace')
    
        !     write(21,rec = 1)temp%num_samples
        !     write(21,rec = 2)temp%mean
        !     write(21,rec = 3)temp%max
        !     write(21,rec = 4)temp%min
        !     write(21,rec = 5)temp%sd
        !     write(21,rec = 6)temp%sem

        !     write(21,rec = 7)sal%num_samples
        !     write(21,rec = 8)sal%mean
        !     write(21,rec = 9)sal%max
        !     write(21,rec = 10)sal%min
        !     write(21,rec = 11)sal%sd
        !     write(21,rec = 12)sal%sem

        !     ! write(21,rec = 13)den%num_samples
        !     write(21,rec = 13)den%mean
            
        !     write(21,rec = 14)dh%mean


        ! close(21)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                            ! removing data from areas other than the Japan Sea
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        sal%mean(:,141:142,ini_lat:44,:) = 0. 
        ! sal%mean(:,141:142,ini_lat:40,:) = 0. 
        ! sal%mean(1:,140,42,:) = 0. ! Tsugaru one grid
        sal%mean(:,140,ini_lat:39,:) = 0.
        sal%mean(:,139,ini_lat:36,:) = 0.
        sal%mean(:,138,ini_lat:36,:) = 0.
        sal%mean(:,131:137,ini_lat:34,:) = 0.
        sal%mean(:,130,ini_lat:32,:) = 0.
        sal%sem(:,141:142,ini_lat:44,:) = 0.
        ! sal%sem(:,141:142,ini_lat:40,:) = 0.
        ! sal%sem(1:,140,42,:) = 0.
        sal%sem(:,140,ini_lat:39,:) = 0.
        sal%sem(:,139,ini_lat:36,:) = 0.
        sal%sem(:,138,ini_lat:36,:) = 0.
        sal%sem(:,131:137,ini_lat:34,:) = 0.
        sal%sem(:,130,ini_lat:32,:) = 0.
        
        temp%mean(:,141:142,ini_lat:44,:) = 0. 
        ! temp%mean(:,141:142,ini_lat:40,:) = 0.
        ! temp%mean(1:,140,42,:) = 0.
        temp%mean(:,140,ini_lat:39,:) = 0.
        temp%mean(:,139,ini_lat:36,:) = 0.
        temp%mean(:,138,ini_lat:36,:) = 0.
        temp%mean(:,131:137,ini_lat:34,:) = 0.
        temp%mean(:,130,ini_lat:32,:) = 0.
        temp%sem(:,141:142,ini_lat:44,:) = 0.
        ! temp%sem(:,141:142,ini_lat:40,:) = 0.
        ! temp%sem(1:,140,42,:) = 0.
        temp%sem(:,140,ini_lat:39,:) = 0.
        temp%sem(:,139,ini_lat:36,:) = 0.
        temp%sem(:,138,ini_lat:36,:) = 0.
        temp%sem(:,131:137,ini_lat:34,:) = 0.
        temp%sem(:,130,ini_lat:32,:) = 0.

        den%mean(:,141:142,ini_lat:44,:) = 0.
        ! den%mean(:,141:142,ini_lat:40,:) = 0.
        ! den%mean(1:,140,42,:) = 0.
        den%mean(:,140,ini_lat:39,:) = 0.
        den%mean(:,139,ini_lat:36,:) = 0.
        den%mean(:,138,ini_lat:36,:) = 0.
        den%mean(:,131:137,ini_lat:34,:) = 0.
        den%mean(:,130,ini_lat:32,:) = 0.

        dh%mean(:,141:142,ini_lat:44,:) = 0.
        ! dh%mean(:,141:142,ini_lat:40,:) = 0.
        ! dh%mean(1:,140,42,:) = 0.
        dh%mean(:,140,ini_lat:39,:) = 0.
        dh%mean(:,139,ini_lat:36,:) = 0.
        dh%mean(:,138,ini_lat:36,:) = 0.
        dh%mean(:,131:137,ini_lat:34,:) = 0.
        dh%mean(:,130,ini_lat:32,:) = 0.

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        sal%mean(2,132,41,0) = 0.    ! removing weird data  02,41,132,0,5,27.88,34.03,3.4,13.68
        sal%mean(6,133,39,0) = 0.    ! removing weird data  06,39,133,0,60,31.08,34.58,3.39,9.32
        sal%mean(7,137,43,0) = 0.    ! removing weird data  07,43,137,0,6,32.09,33.97,22.92,4.49
        sal%mean(8,132,40,0) = 0.    ! removing weird data  08,40,132,0,19,30.62,34.14,3.34,9.61
        sal%mean(12,136,39,0) = 0.   ! removing weird data  12,39,136,0,15,32.88,34.23,26.34,2.61
        sal%mean(5,138,44,4) = 0.    ! removing weird data  05,44,138,50,5,33.56,34.18,31.48,1.17
        sal%sem(2,132,41,0) = 0.    ! removing weird data  02,41,132,0,5,27.88,34.03,3.4,13.68
        sal%sem(6,133,39,0) = 0.    ! removing weird data  06,39,133,0,60,31.08,34.58,3.39,9.32
        sal%sem(7,137,43,0) = 0.    ! removing weird data  07,43,137,0,6,32.09,33.97,22.92,4.49
        sal%sem(8,132,40,0) = 0.    ! removing weird data  08,40,132,0,19,30.62,34.14,3.34,9.61
        sal%sem(12,136,39,0) = 0.   ! removing weird data  12,39,136,0,15,32.88,34.23,26.34,2.61
        sal%sem(5,138,44,4) = 0.    ! removing weird data  05,44,138,50,5,33.56,34.18,31.48,1.17

        temp%mean(2,132,41,0) = 0.    ! removing weird data  02,41,132,0,5,27.88,34.03,3.4,13.68
        temp%mean(6,133,39,0) = 0.    ! removing weird data  06,39,133,0,60,31.08,34.58,3.39,9.32
        temp%mean(7,137,43,0) = 0.    ! removing weird data  07,43,137,0,6,32.09,33.97,22.92,4.49
        temp%mean(8,132,40,0) = 0.    ! removing weird data  08,40,132,0,19,30.62,34.14,3.34,9.61
        temp%mean(12,136,39,0) = 0.   ! removing weird data  12,39,136,0,15,32.88,34.23,26.34,2.61
        temp%mean(5,138,44,4) = 0.    ! removing weird data  05,44,138,50,5,33.56,34.18,31.48,1.17
        temp%sem(2,132,41,0) = 0.    ! removing weird data  02,41,132,0,5,27.88,34.03,3.4,13.68
        temp%sem(6,133,39,0) = 0.    ! removing weird data  06,39,133,0,60,31.08,34.58,3.39,9.32
        temp%sem(7,137,43,0) = 0.    ! removing weird data  07,43,137,0,6,32.09,33.97,22.92,4.49
        temp%sem(8,132,40,0) = 0.    ! removing weird data  08,40,132,0,19,30.62,34.14,3.34,9.61
        temp%sem(12,136,39,0) = 0.   ! removing weird data  12,39,136,0,15,32.88,34.23,26.34,2.61
        temp%sem(5,138,44,4) = 0.    ! removing weird data  05,44,138,50,5,33.56,34.18,31.48,1.17

        den%mean(2,132,41,0) = 0.    ! removing weird data  02,41,132,0,5,27.88,34.03,3.4,13.68
        den%mean(6,133,39,0) = 0.    ! removing weird data  06,39,133,0,60,31.08,34.58,3.39,9.32
        den%mean(7,137,43,0) = 0.    ! removing weird data  07,43,137,0,6,32.09,33.97,22.92,4.49
        den%mean(8,132,40,0) = 0.    ! removing weird data  08,40,132,0,19,30.62,34.14,3.34,9.61
        den%mean(12,136,39,0) = 0.   ! removing weird data  12,39,136,0,15,32.88,34.23,26.34,2.61
        den%mean(5,138,44,4) = 0.    ! removing weird data  05,44,138,50,5,33.56,34.18,31.48,1.17

        dh%mean(2,132,41,0) = 0.    ! removing weird data  02,41,132,0,5,27.88,34.03,3.4,13.68
        dh%mean(6,133,39,0) = 0.    ! removing weird data  06,39,133,0,60,31.08,34.58,3.39,9.32
        dh%mean(7,137,43,0) = 0.    ! removing weird data  07,43,137,0,6,32.09,33.97,22.92,4.49
        dh%mean(8,132,40,0) = 0.    ! removing weird data  08,40,132,0,19,30.62,34.14,3.34,9.61
        dh%mean(12,136,39,0) = 0.   ! removing weird data  12,39,136,0,15,32.88,34.23,26.34,2.61
        dh%mean(5,138,44,4) = 0.    ! removing weird data  05,44,138,50,5,33.56,34.18,31.48,1.17

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        min = minval(dh%mean(:,126:142,32:46,11),mask=(dh%mean(:,126:142,32:46,11)/=0.))
        print*,minval(dh%mean(:,126:142,32:46,11),mask=(dh%mean(:,126:142,32:46,11)/=0.)),maxval(dh%mean(:,126:142,32:46,11))
        do i = 1,12
            do j = 126,142
                do k = 32,46
                    if(dh%mean(i,j,k,11) /= 0..and.dh%mean(0,j,k,11)/=0.)then 
                        ! dh%mean(i,j,k,11) = dh%mean(i,j,k,11) - min ! dev from lowest point from each month
                        ! dh%mean(i,j,k,11) = dh%mean(i,j,k,11) - dh%mean(0,j,k,11) ! deviation from annual mean DO NOT USE IN CONJUNCTION WITH BELOW
                        if(i == 1)then
                            if(dh%mean(12,j,k,11) /= 0.)then 
                                mdiff_dh%mean(i,j,k,11) = dh%mean(i,j,k,11) - dh%mean(12,j,k,11) ! diff from Dec. to Jan.
                            else;mdiff_dh%mean(i,j,k,11) = 0.
                            end if
                        else
                            if(dh%mean(i-1,j,k,11) /= 0.)then
                                mdiff_dh%mean(i,j,k,11) = dh%mean(i,j,k,11) - dh%mean(i-1,j,k,11) ! diff from previous month
                            else;mdiff_dh%mean(i,j,k,11) = 0.
                            end if
                        end if
                    end if
                end do
            end do
        end do
        print*,minval(dh%mean(:,126:142,32:46,11),mask=(dh%mean(:,126:142,32:46,11)/=0.)),maxval(dh%mean(:,126:142,32:46,11))
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                            ! Plotting
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! call plots2(oopt = 'otops',mode = 'portrait',nnfile = 'JODC_Sal_0_50_100m_Japan_Sea',x = 1.,y = -5.)
    call plots2(oopt = 'otops',mode = 'landscape',nnfile = 'JODC_Seasonal_Means3_TSRHO',x = 1.7,y = -5.5,h = 'JODC Seasonal Means 100m')
    call plotsave('original')
    ratio = 6357./6378./cos((ini_lat+fin_lat)/2.*pi/180.)
    height = width2*ratio*real(fin_lat-ini_lat)/real(fin_long-ini_long)

    !!!!!!!!!!!!
    !  Monthly
    ! !!!!!!!!!!!!
        !!!!!!!!  Temp Mean !!!!!!!!

            ! do i = 1, 12
            !     call plotback('original')
            !     if(i==1.or.i==7)then 
            !         stat = .true.
            !     else
            !         stat = .false.
            !     end if
            !     call plot((width2+0.3)*real(mod(i-1,6)),0.,-3)
            !     call symbolc(width2/2.,height+0.4,0.8,monthnames(i),0.)
            !     if(i == 1.or. i==7)call symbolc(-1.8,height/2.,0.8,'0m')
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat)
            !     call butler_psk(temp%mean(i,ini_long:fin_long,ini_lat:fin_lat,0),width2,height,0.,0.,24.,2.,'b2r',12,bpt1 = 5,r = r1,g = g1,b = b1) ! 0m
            !     call butler_cont(temp%mean(i,ini_long:fin_long,ini_lat:fin_lat,0),width2,height,0.,-10.,1.,thicc = 5,maskn = .true.) ! 0m
            !     ! call butler_mask(temp%mean(i,ini_long:fin_long,ini_lat:fin_lat,0),width2,height,) ! 0m
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat,paintland = .false.)
            !     call plot(0.,-height-1.,-3)

            !     if(i == 1.or. i==7)call symbolc(-1.8,height/2.,0.8,'50m')
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat)
            !     call butler_psk(temp%mean(i,ini_long:fin_long,ini_lat:fin_lat,4),width2,height,0.,0.,24.,2.,'b2r',12,bpt1 = 5) ! 50m
            !     call butler_cont(temp%mean(i,ini_long:fin_long,ini_lat:fin_lat,4),width2,height,0.,-10.,1.,thicc = 5,maskn = .true.) ! 50m
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat,paintland = .false.)
            !     call plot(0.,-height-1.,-3)

            !     if(i == 1.or. i==7)call symbolc(-2.,height/2.,0.8,'100m')
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat)
            !     call butler_psk(temp%mean(i,ini_long:fin_long,ini_lat:fin_lat,6),width2,height,0.,0.,24.,2.,'b2r',12,bpt1 = 5) ! 100m
            !     call butler_cont(temp%mean(i,ini_long:fin_long,ini_lat:fin_lat,6),width2,height,0.,-10.,1.,thicc = 5,maskn = .true.) ! 100m
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat,paintland = .false.)
            !     call plot(0.,-height-1.,-3)
            !     if(i == 6)call newpage
            ! end do

            month = 2
            call plot(-0.5,0.,-3);call plotsave('seasonal')
            do i = 1, 5
                ! call plotback('original')
                if(i/=1)month = month +3
                if(i==5)month = 2
                if(month==2)then 
                    stat = .true.
                end if

                call symbolc(width2/2.,height+0.4,0.8,monthnames(month),0.)
                if(i == 1)then 
                    ! call symbolr(,height/2.,0.6,'100m',ang = 0.)
                    call symbolr(-1.,height/2.,0.6,'0m',ang = 0.)
                    stat = .true.
                else
                    stat = .false.
                end if
                call butler_psk(temp%mean(month,ini_long:fin_long,ini_lat:fin_lat,6),width2,height,0.,0.,24.,2.,'b2r',12,bpt1 = 6,r = r1, g = g1, b = b1) ! 100m
                call butler_cont(temp%mean(month,ini_long:fin_long,ini_lat:fin_lat,6),width2,height,0.,maskn = .true.,conti = 0.,continc = 1.,thicc = 5) ! 100m
                call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat,paintland = .true.)
                call butler_psk(sal%mean(month,ini_long:fin_long,ini_lat:fin_lat,6),width2,height,0.,33.7,34.3,0.05,'b2r',12,bpt1 = 6, y = -height-1.,r = r2, g = g2,b = b2) ! 100m
                call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat,paintland = .true.,y = -height-1.)

                call plot(width2 + 0.5,0.,-3)
            end do

            call newpage(y = -5.)
            call colorscale(12,r1,g1,b1,0.,24.,5,0.8,-1,10.,0.3,lt = 1, gt = 1,rangle = 90.)
            call plot(3.,0.,-3)
            call colorscale(12,r2,g2,b2,33.7,34.3,2,0.8,1,10.,0.3,lt = 1, gt = 1,rangle = 90.)


        !!!!!!!!! Temp sem !!!!!!!!

            ! call newpage(h = 'JODC Temperature SEM')
            ! do i = 1, 12
            !     call plotback('original')
            !     if(i==1.or.i==7)then 
            !         stat = .true.
            !     else
            !         stat = .false.
            !     end if
            !     call plot((width2+0.3)*real(mod(i-1,6)),0.,-3)
            !     call symbolc(width2/2.,height+0.4,0.8,monthnames(i),0.)
            !     if(i == 1.or. i==7)call symbolc(-1.8,height/2.,0.8,'0m')
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat)
            !     call butler_psk(temp%sem(i,ini_long:fin_long,ini_lat:fin_lat,0),width2,height,0.,0.,1.,0.1,'b2r',10,bpt1 = 5,r = r2,g = g2,b = b2) ! 0m
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat,paintland = .false.)
            !     call plot(0.,-height-1.,-3)

            !     if(i == 1.or. i==7)call symbolc(-1.8,height/2.,0.8,'50m')
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat)
            !     call butler_psk(temp%sem(i,ini_long:fin_long,ini_lat:fin_lat,4),width2,height,0.,0.,1.,0.1,'b2r',10,bpt1 = 5) ! 50m
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat,paintland = .false.)
            !     call plot(0.,-height-1.,-3)

            !     if(i == 1.or. i==7)call symbolc(-2.,height/2.,0.8,'100m')
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat)
            !     call butler_psk(temp%sem(i,ini_long:fin_long,ini_lat:fin_lat,6),width2,height,0.,0.,1.,0.1,'b2r',10,bpt1 = 5) ! 100m
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat,paintland = .false.)
            !     call plot(0.,-height-1.,-3)
            !     if(i == 6)call newpage
            ! end do

            ! call newpage
            ! call ocenter
            ! call colorscale(12,r1,g1,b1,0.,24.,5,0.8,-1,20.,0.3,lt = 1, gt = 1,rangle = 90.)
            ! call plot(3.,0.,-3)
            ! call colorscale(10,r2,g2,b2,0.,1.,1,0.8,1,20.,0.3,gt = 1,rangle = 90.)

        !!!!!!!!! Sal Mean !!!!!!!!

            ! call newpage(h = 'JODC Salinity Mean')
            ! do i = 1, 12
            !     call plotback('original')
            !     if(i==1.or.i==7)then 
            !         stat = .true.
            !     else
            !         stat = .false.
            !     end if
            !     call plot((width2+0.3)*real(mod(i-1,6)),0.,-3)
            !     call symbolc(width2/2.,height+0.4,0.8,monthnames(i),0.)
            !     if(i == 1.or. i==7)call symbolc(-1.8,height/2.,0.8,'0m')
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat)
            !     call butler_psk(sal%mean(i,ini_long:fin_long,ini_lat:fin_lat,0),width2,height,0.,33.,34.3,0.1,'b2r',13,bpt1 = 9,r = r1,g = g1,b = b1) ! 0m
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat,paintland = .false.)
            !     call plot(0.,-height-1.,-3)

            !     if(i == 1.or. i==7)call symbolc(-1.8,height/2.,0.8,'50m')
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat)
            !     call butler_psk(sal%mean(i,ini_long:fin_long,ini_lat:fin_lat,4),width2,height,0.,33.,34.3,0.1,'b2r',13,bpt1 = 9) ! 50m
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat,paintland = .false.)
            !     call plot(0.,-height-1.,-3)

            !     if(i == 1.or. i==7)call symbolc(-2.,height/2.,0.8,'100m')
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat)
            !     call butler_psk(sal%mean(i,ini_long:fin_long,ini_lat:fin_lat,6),width2,height,0.,33.,34.3,0.1,'b2r',13,bpt1 = 9) ! 100m
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat,paintland = .false.)
            !     call plot(0.,-height-1.,-3)
            !     if(i == 6)call newpage
            ! end do

        !!!!!!!!! sal sem !!!!!!!!

            ! call newpage(h = 'JODC Salinity SEM')
            ! do i = 1, 12
            !     call plotback('original')
            !     if(i==1.or.i==7)then 
            !         stat = .true.
            !     else
            !         stat = .false.
            !     end if
            !     call plot((width2+0.3)*real(mod(i-1,6)),0.,-3)
            !     call symbolc(width2/2.,height+0.4,0.8,monthnames(i),0.)
            !     if(i == 1.or. i==7)call symbolc(-1.8,height/2.,0.8,'0m')
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat)
            !     call butler_psk(sal%sem(i,ini_long:fin_long,ini_lat:fin_lat,0),width2,height,0.,0.,0.1,0.01,'b2r',10,bpt1 = 5,r = r2,g = g2,b = b2) ! 0m
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat,paintland = .false.)
            !     call plot(0.,-height-1.,-3)

            !     if(i == 1.or. i==7)call symbolc(-1.8,height/2.,0.8,'50m')
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat)
            !     call butler_psk(sal%sem(i,ini_long:fin_long,ini_lat:fin_lat,4),width2,height,0.,0.,0.1,0.01,'b2r',10,bpt1 = 5) ! 50m
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat,paintland = .false.)
            !     call plot(0.,-height-1.,-3)

            !     if(i == 1.or. i==7)call symbolc(-2.,height/2.,0.8,'100m')
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat)
            !     call butler_psk(sal%sem(i,ini_long:fin_long,ini_lat:fin_lat,6),width2,height,0.,0.,0.1,0.01,'b2r',10,bpt1 = 5) ! 100m
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat,paintland = .false.)
            !     call plot(0.,-height-1.,-3)
            !     if(i == 6)call newpage
            ! end do


            ! call newpage
            ! call ocenter
            ! call colorscale(13,r1,g1,b1,33.,34.3,1,0.8,1,20.,0.3,lt = 1, gt = 1,rangle = 90.)
            ! call plot(3.,0.,-3)
            ! call colorscale(10,r2,g2,b2,0.,0.1,1,0.8,2,20.,0.3,gt = 1,rangle = 90.)

        !!!!!!!!  Den Mean !!!!!!!!

            ! call newpage(h = 'JODC Density Mean')
            ! do i = 1, 12
            !     call plotback('original')
            !     if(i==1.or.i==7)then 
            !         stat = .true.
            !     else
            !         stat = .false.
            !     end if
            !     call plot((width2+0.3)*real(mod(i-1,6)),0.,-3)
            !     call symbolc(width2/2.,height+0.4,0.8,monthnames(i),0.)
            !     if(i == 1.or. i==7)call symbolc(-1.8,height/2.,0.8,'0m')
            !     ! call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat)
            !     call butler_psk(den%mean(i,ini_long:fin_long,ini_lat:fin_lat,0),width2,height,0.,24.,27.,0.2,'b2r',15,bpt1 = 8,r = r1,g = g1,b = b1,conti = 20.,continc = 0.2,thicc = 5) ! 0m
            !     ! call butler_cont(den%mean(i,ini_long:fin_long,ini_lat:fin_lat,0),width2,height,0.,-10.,1.,thicc = 5,maskn = .true.) ! 0m
            !     ! call butler_mask(temp%mean(i,ini_long:fin_long,ini_lat:fin_lat,0),width2,height,) ! 0m
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat,paintland = .false.)
            !     call plot(0.,-height-1.,-3)

            !     if(i == 1.or. i==7)call symbolc(-1.8,height/2.,0.8,'50m')
            !     ! call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat)
            !     call butler_psk(den%mean(i,ini_long:fin_long,ini_lat:fin_lat,4),width2,height,0.,24.,27.,0.2,'b2r',15,bpt1 = 8,conti = 20.,continc = 0.2,thicc = 5) ! 50m
            !     ! call butler_cont(den%mean(i,ini_long:fin_long,ini_lat:fin_lat,4),width2,height,0.,-10.,1.,thicc = 5,maskn = .true.) ! 50m
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat,paintland = .false.)
            !     call plot(0.,-height-1.,-3)

            !     if(i == 1.or. i==7)call symbolc(-2.,height/2.,0.8,'100m')
            !     ! call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat)
            !     call butler_psk(den%mean(i,ini_long:fin_long,ini_lat:fin_lat,6),width2,height,0.,24.,27.,0.2,'b2r',15,bpt1 = 8,conti = 20.,continc = 0.2,thicc = 5) ! 100m
            !     ! call butler_cont(den%mean(i,ini_long:fin_long,ini_lat:fin_lat,6),width2,height,0.,-10.,1.,thicc = 5,maskn = .true.) ! 100m
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat,paintland = .false.)
            !     call plot(0.,-height-1.,-3)
            !     if(i == 6)call newpage
            ! end do

            ! !!!!!!!! Den sem is impossible to draw !!!!!!!!

            ! call newpage
            ! call ocenter
            ! call colorscale(15,r1,g1,b1,24.,27.,5,0.8,1,20.,0.3,lt = 1, gt = 1,rangle = 90.)

        !!!!!!!! dh diff from annual mean !!!!!!!!
            ! call newpage(h = 'JODC Dynamic Height, dev from annual mean (300db ref.)')
            ! call plotback('original');call plot(-1.,1.,-3);call plotsave('three')
            !     do i = 1, 13
            !         if(i==1.or.i==7)call plotback('three')
            !         if(i==7)call plot(0.,-height-1.8,-3)
            !         if(i==1.or.i==7)then 
            !             stat = .true.
            !         else
            !             stat = .false.
            !         end if
            !         ! call plot((width2+0.3)*real(mod(i-1,6)),0.,-3)
            !         if(i/=13)call symbolc(width2/2.,height+0.3,0.8,monthnames(i),0.)
            !         if(i/=13)call butler_psk(dh%mean(i,ini_long:fin_long,ini_lat:fin_lat,JODC_dep_to_index(300)),width2,height,0.,-20.,20.,5.,'b2r',8,bpt1 = 4,r = r1,g = g1,b = b1,conti = 0.,continc = 1.,thicc = 5) 
            !         if(i/=13)call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat,paintland = .true.)
            !         call plot((width2+0.3),0.,-3)

            !         if(i == 13)then 
            !             call plotback('three');call plot(0.,2.*(-height-1.8),-3)
            !             call symbolc(width2/2.,height+0.3,0.8,'Annual')
            !             call butler_psk(dh%mean(0,ini_long:fin_long,ini_lat:fin_lat,JODC_dep_to_index(300)),width2,height,0.,29765.,29805.,5.,'b2r',8,bpt1 = 4,r = r1,g = g1,b = b1,conti = 29700.,continc = 2.,thicc = 5) ! annual
            !             call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = .true.,paintland = .true.)
            !         end if

            !     end do
            ! call newpage
            ! call ocenter
            ! call colorscale(8,r1,g1,b1,-20.,20.,2,0.8,-1,20.,0.3,lt = 1, gt = 1,rangle = 90.)
        !!!!!!!! dh diff between consecutive months !!!!!!!!
            ! call newpage(h = 'JODC Dynamic Height, dev from previous months (300db ref.)')
            ! call plotback('original');call plot(-1.,-1.,-3);call plotsave('three')
            !     do i = 1, 12
            !         if(i==1.or.i==7)call plotback('three')
            !         if(i==7)call plot(0.,-height-1.8,-3)
            !         if(i==1.or.i==7)then 
            !             stat = .true.
            !         else
            !             stat = .false.
            !         end if
            !         ! call plot((width2+0.3)*real(mod(i-1,6)),0.,-3)
            !         if(i/=1)call symbolc(width2/2.,height+0.3,0.8,monthnames(i)//'- '//monthnames(i-1),0.)
            !         if(i==1)call symbolc(width2/2.,height+0.3,0.8,monthnames(1)//'- '//monthnames(12),0.)
            !         call butler_psk(mdiff_dh%mean(i,ini_long:fin_long,ini_lat:fin_lat,JODC_dep_to_index(300)),width2,height,0.,-20.,20.,5.,'b2r',8,bpt1 = 4,r = r1,g = g1,b = b1,conti = 0.,continc = 1.,thicc = 5) 
            !         call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat,paintland = .true.)
            !         call plot((width2+0.3),0.,-3)

            !     end do
            ! call newpage
            ! call ocenter
            ! call colorscale(8,r1,g1,b1,-20.,20.,2,0.8,-1,20.,0.3,lt = 1, gt = 1,rangle = 90.)
    
    !!!!!!!!!!!!!!!!
      ! Annual
    !!!!!!!!!!!!!!!!
            ! ! call newpage(h = 'JODC Temperature Annual Means')
            ! call plotback('original')
            ! call plot(0.,1.,-3)
            ! call plotsave('two')
            ! do i = 1,3
            !     ! if(i==1)then 
            !     !     stat = .true.
            !     ! else
            !     !     stat = .false.
            !     ! end if
            !     if(i == 1)dep = JODC_dep_to_index(100)
            !     if(i == 2)dep = JODC_dep_to_index(200)
            !     if(i == 3)dep = JODC_dep_to_index(300)
            !     call symbolr(-1.,height/2.,0.8,int2str(JODC_index_to_dep(dep))//'m',0.)
            !     call butler_psk(temp%mean(0,ini_long:fin_long,ini_lat:fin_lat,dep),width2,height,0.,0.,10.,1.,'b2r',10,bpt1 = 3,r = r1,g = g1,b = b1) 
            !     call butler_cont(temp%mean(0,ini_long:fin_long,ini_lat:fin_lat,dep),width2,height,0.,-10.,1.,thicc = 5,maskn = .true.) ! 0m
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = .true.,paintland = .true.)
            !     if(i==3)then 
            !         call colorscale(10,r1,g1,b1,0.,10.,1,0.45,-1,width2,0.2,lt = 1,gt = 1,x = width2/2.,y = -1.)
            !     end if
            !     call plot((width2+2.),0.,-3)

            !     call butler_psk(sal%mean(0,ini_long:fin_long,ini_lat:fin_lat,dep),width2,height,0.,33.95,34.3,0.05,'b2r',7,bpt1 = 4,r = r2,g = g2,b = b2) 
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = .false.,paintland = .true.)
            !     if(i==3)then 
            !         call colorscale(7,r2,g2,b2,33.95,34.3,2,0.4,1,width2,0.2,lt = 1,gt = 1,x = width2/2.,y = -1.,symbol_start = 2)
            !     end if
            !     call plot((width2+2.),0.,-3)

            !     call butler_psk(den%mean(0,ini_long:fin_long,ini_lat:fin_lat,dep),width2,height,0.,26.,27.2,0.2,'b2r',6,bpt1 = 3,r = r3,g = g3,b = b3,conti = 20.,continc = 0.2,thicc = 5)
            !     call butler_cont(den%mean(0,ini_long:fin_long,ini_lat:fin_lat,dep),width2,height,0.,20.,.2,thicc = 5) ! 0m
            !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = .false.,paintland = .true.)
            !     if(i==3)then 
            !         call colorscale(6,r3,g3,b3,26.,27.2,2,0.45,1,width2,0.2,lt = 1,gt = 1,x = width2/2.,y = -1.)
            !     end if

                
            !     call plotback('two')
            !     if(i==1)then 
            !         call plot(0.,-height-1.,-3)
            !     else if(i==2)then
            !         call plot(0.,-2.*height-2.,-3)
            !     else if(i==3)then
            !         call plot(0.,-3.*height-3.,-3)
            !     end if
            ! end do
    call plote
end program 
