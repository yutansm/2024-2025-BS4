program gomibako
    use always
    implicit none 
    type(JODC_TS)::temp,sal
    type(JODC_RHO)::den,dh
    character(len=100)::title
    real::ratio,height,width = 5.,width2 = 4.,min
    integer::ini_lat = 32,fin_lat = 46,ini_long = 126,fin_long = 142
    real,dimension(:),allocatable::r1,g1,b1,r2,g2,b2
    logical::stat

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
        ! call openlog(omit=.true.)
        ! write(tolog,*)dh%mean(:,126:142,32:46,11)
        ! call closelog
        ! do i = 0,12
        !     do j = 126,142
        !         do k = 32,46
        !             if(dh%mean(i,j,k,11) /= 0..and.dh%mean(i,j,k,11)<30000.)then 
        !                 do l = 0,12
        !                     print*,i,j,k,JODC_index_to_dep(l),dh%mean(i,j,k,l)/100.
        !                 end do
        !             end if
        !         end do
        !     end do
        ! end do
        min = minval(dh%mean(:,126:142,32:46,11),mask=(dh%mean(:,126:142,32:46,11)/=0.))
        print*,minval(dh%mean(:,126:142,32:46,11),mask=(dh%mean(:,126:142,32:46,11)/=0.)),maxval(dh%mean(:,126:142,32:46,11))
        do i = 0,12
            do j = 126,142
                do k = 32,46
                    if(dh%mean(i,j,k,11) /= 0.)then 
                        dh%mean(i,j,k,11) = dh%mean(i,j,k,11) - min
                        ! dh%mean(i,j,k,11) = dh%mean(i,j,k,11) - dh%mean(0,j,k,11)
                    end if
                end do
            end do
        end do
        print*,minval(dh%mean(:,126:142,32:46,11),mask=(dh%mean(:,126:142,32:46,11)/=0.)),maxval(dh%mean(:,126:142,32:46,11))
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                            ! Plotting
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! call plots2(oopt = 'otops',mode = 'portrait',nnfile = 'JODC_Sal_0_50_100m_Japan_Sea',x = 1.,y = -5.)
    call plots2(oopt = 'otops',mode = 'landscape',nnfile = 'JODC_DH300_bet',x = 1.7,y = -5.5,h = 'JODC Temperature Mean')
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

        ! !!!!!!!! Temp sem !!!!!!!!

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

        ! !!!!!!!!  Sal Mean !!!!!!!!

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

        ! !!!!!!!! sal sem !!!!!!!!

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

        !     !!!!!!!  Den Mean !!!!!!!!

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

        !!!!!!!! dh mean !!!!!!!!
        call newpage(h = 'JODC Mean Dynamic Height at 300m depth')
        call plotback('original');call plot(-1.,1.,-3);call plotsave('three')
            do i = 1, 13
                if(i==1.or.i==7)call plotback('three')
                if(i==7)call plot(0.,-height-1.8,-3)
                if(i==1.or.i==7)then 
                    stat = .true.
                else
                    stat = .false.
                end if
                ! call plot((width2+0.3)*real(mod(i-1,6)),0.,-3)
                if(i/=13)call symbolc(width2/2.,height+0.3,0.8,monthnames(i),0.)
                if(i/=13)call butler_psbet(dh%mean(i,ini_long:fin_long,ini_lat:fin_lat,JODC_dep_to_index(300)),width2,height,0.,0.,40.,5.,'b2r',8,bpt1 = 4,r = r1,g = g1,b = b1,conti = 0.,continc = 2.,thicc = 5) 
                if(i/=13)call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat,paintland = .true.)
                call plot((width2+0.3),0.,-3)

                if(i == 13)then 
                    call plotback('three');call plot(0.,2.*(-height-1.8),-3)
                    call symbolc(width2/2.,height+0.3,0.8,'Annual')
                    call butler_psbet(dh%mean(0,ini_long:fin_long,ini_lat:fin_lat,JODC_dep_to_index(300)),width2,height,0.,0.,40.,5.,'b2r',8,bpt1 = 4,r = r1,g = g1,b = b1,conti = 0.,continc = 2.,thicc = 5) ! annual
                    call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = .true.,paintland = .true.)
                end if

            end do
        ! call openlog(omit = .true.)
        ! write(tolog,*)dh%mean(:,126:142,32:46,JODC_dep_to_index(300,info=.true.))
        ! call closelog

        call newpage
        call ocenter
        call colorscale(8,r1,g1,b1,0.,40.,2,0.8,-1,20.,0.3,lt = 1, gt = 1,rangle = 90.)

    ! !!!!!!!!!!!!!!!
      ! Annual
    ! !!!!!!!!!!!!!!!
    !     call newpage(h = 'JODC Temperature Annual Means')
    !     call plotback('original')
    !     call plot(0.,1.,-3)
    !     call plotsave('two')
    !     do i = 0, 17
    !         if(i==0)then 
    !             stat = .true.
    !         else
    !             stat = .false.
    !         end if
    !         call symbolc(width2/2.,height+0.2,0.8,int2str(JODC_index_to_dep(i))//'m',0.)
    !         call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat)
    !         call butler_psk(temp%mean(0,ini_long:fin_long,ini_lat:fin_lat,i),width2,height,0.,0.,24.,2.,'b2r',12,bpt1 = 5,r = r1,g = g1,b = b1) ! 0m
    !         call butler_cont(temp%mean(0,ini_long:fin_long,ini_lat:fin_lat,i),width2,height,0.,-10.,1.,thicc = 5,maskn = .true.) ! 0m
    !         call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat,paintland = .false.)
    !         ! call butler_mask(temp%mean(i,ini_long:fin_long,ini_lat:fin_lat,0),width2,height,) ! 0m
    !         call plot((width2+0.3),0.,-3)
    !         if(mod(i+1,6)==0)then 
    !             call plotback('two')
    !             if(i == 5)then 
    !                 call plot(0.,(-height-1.5)*(real(i+1)/6.),-3)
    !             else 
    !                 call plot(0.,(-height-1.5)*(real(i+1)/6.),-3)
    !             end if
    !         end if
    !     end do
    call plote
end program 
