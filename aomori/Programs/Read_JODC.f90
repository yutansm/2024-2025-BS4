program read_JODC_files
    use always
    implicit none 
    integer,dimension(:,:,:,:),allocatable::num_sample
    real,dimension(:,:,:,:),allocatable::mean_salt,max_salt,min_salt,sdv,sem
    character(len=100)::filename,firstrow,title
    integer::month,lat,lon,dep,ios
    integer,dimension(0:1000000)::dep_array
    real::ratio,height,width = 5.,mean,max,min,sd,width2 = 4.
    integer::ini_lat = 32,fin_lat = 46,ini_long = 126,fin_long = 142,num
    real,dimension(:),allocatable::r1,g1,b1,r2,g2,b2
    logical::stat

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                    ! Data Obtainment
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    allocate(num_sample(0:12,-180:180,-90:90,0:32))
    allocate(mean_salt(0:12,-180:180,-90:90,0:32))
    allocate(max_salt(0:12,-180:180,-90:90,0:32))
    allocate(min_salt(0:12,-180:180,-90:90,0:32))
    allocate(sdv(0:12,-180:180,-90:90,0:32))
    allocate(sem(0:12,-180:180,-90:90,0:32))
    num_sample = 0;mean_salt = 0.;max_salt = 0.;min_salt = 0.;sdv = 0.;sem = 0.

    do i = 1,13
        n = 0
        open(20,file = '../Data/JODC/Salinity/bss-'//int2str(i-1,form = '(i2.2)')//'.csv',status = 'old',action = 'read')
        read(20,'(A)')firstrow
        ! print*,firstrow
        do 
            read(20,*,iostat = ios)month,lat,lon,dep,num,mean,max,min,sd
            if(dep == 0)dep = 0
            if(dep == 10)dep = 1
            if(dep == 20)dep = 2
            if(dep == 30)dep = 3
            if(dep == 50)dep = 4
            if(dep == 75)dep = 5
            if(dep == 100)dep = 6
            if(dep == 125)dep = 7
            if(dep == 150)dep = 8
            if(dep == 200)dep = 9
            if(dep == 250)dep = 10
            if(dep == 300)dep = 11
            if(dep == 400)dep = 12
            if(dep == 500)dep = 13
            if(dep == 600)dep = 14
            if(dep == 700)dep = 15
            if(dep == 800)dep = 16
            if(dep == 900)dep = 17
            if(dep == 1000)dep = 18
            if(dep == 1100)dep = 19
            if(dep == 1200)dep = 20
            if(dep == 1300)dep = 21
            if(dep == 1400)dep = 22
            if(dep == 1500)dep = 23
            if(dep == 1750)dep = 24
            if(dep == 2000)dep = 25
            if(dep == 2500)dep = 26
            if(dep == 3000)dep = 27
            if(dep == 3500)dep = 28
            if(dep == 4000)dep = 29
            if(dep == 4500)dep = 30
            if(dep == 5000)dep = 31
            if(dep == 5500)dep = 32
            if(dep>32)then;print*,'out of range dep:',dep;stop;endif

            num_sample(month,lon,lat,dep) = num
            mean_salt(month,lon,lat,dep) = mean
            max_salt(month,lon,lat,dep) = max
            min_salt(month,lon,lat,dep) = min
            sdv(month,lon,lat,dep) = sd
            sem(month,lon,lat,dep) = sd/sqrt(real(num))

            ! if(dep == 32)print*,month,lat,lon

            if(ios==0)then 
                n = n + 1;dep_array(n) = dep
            else;print*,'number of rows is:',n
                exit
            end if
        end do
        print*,'maximum depth index is:',maxval(dep_array)
        ! print*,maxval(num_sample),maxval(mean_salt),maxval(max_salt),maxval(min_salt),maxval(sdv)
        close(20)
        print*,'-----------------------------------'
    end do

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                            ! removing data from areas other than the Japan Sea
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    mean_salt(:,141:142,ini_lat:44,:) = 0. 
    mean_salt(:,140,42,:) = 0.
    mean_salt(:,140,ini_lat:39,:) = 0.
    mean_salt(:,139,ini_lat:36,:) = 0.
    mean_salt(:,138,ini_lat:36,:) = 0.
    mean_salt(:,131:137,ini_lat:34,:) = 0.
    mean_salt(:,130,ini_lat:32,:) = 0.
    sem(:,141:142,ini_lat:44,:) = 0.
    sem(:,140,42,:) = 0.
    sem(:,140,ini_lat:39,:) = 0.
    sem(:,139,ini_lat:36,:) = 0.
    sem(:,138,ini_lat:36,:) = 0.
    sem(:,131:137,ini_lat:34,:) = 0.
    sem(:,130,ini_lat:32,:) = 0.
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    mean_salt(2,132,41,0) = 0.    ! removing weird data  02,41,132,0,5,27.88,34.03,3.4,13.68
    mean_salt(6,133,39,0) = 0.    ! removing weird data  06,39,133,0,60,31.08,34.58,3.39,9.32
    mean_salt(7,137,43,0) = 0.    ! removing weird data  07,43,137,0,6,32.09,33.97,22.92,4.49
    mean_salt(12,136,39,0) = 0.   ! removing weird data  12,39,136,0,15,32.88,34.23,26.34,2.61
    mean_salt(5,138,44,4) = 0.    ! removing weird data  05,44,138,50,5,33.56,34.18,31.48,1.17
    sem(2,132,41,0) = 0.    ! removing weird data  02,41,132,0,5,27.88,34.03,3.4,13.68
    sem(6,133,39,0) = 0.    ! removing weird data  06,39,133,0,60,31.08,34.58,3.39,9.32
    sem(7,137,43,0) = 0.    ! removing weird data  07,43,137,0,6,32.09,33.97,22.92,4.49
    sem(12,136,39,0) = 0.   ! removing weird data  12,39,136,0,15,32.88,34.23,26.34,2.61
    sem(5,138,44,4) = 0.    ! removing weird data  05,44,138,50,5,33.56,34.18,31.48,1.17

    mean_salt(8,132,40,0) = 0.    ! removing weird data  08,40,132,0,19,30.62,34.14,3.34,9.61
    sem(8,132,40,0) = 0.    ! removing weird data  08,40,132,0,19,30.62,34.14,3.34,9.61
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                            ! Plotting
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! call plots2(oopt = 'otops',mode = 'portrait',nnfile = 'JODC_Sal_0_50_100m_Japan_Sea',x = 1.,y = -5.)
    call plots2(oopt = 'otops',mode = 'landscape',nnfile = 'JODC_Sal_0_50_100m_Japan_Sea_psk_edited',x = 1.7,y = -5.5,h = 'JODC Salinity Mean')
    call plotsave('original')

    !!!!!!!! Mean !!!!!!!!

    ratio = 6357./6378./cos((ini_lat+fin_lat)/2.*pi/180.)
    height = width2*ratio*real(fin_lat-ini_lat)/real(fin_long-ini_long)
    do i = 1, 12
        call plotback('original')
        if(i==1.or.i==7)then 
            stat = .true.
        else
            stat = .false.
        end if
        call plot((width2+0.3)*real(mod(i-1,6)),0.,-3)
        call symbolc(width2/2.,height+0.4,0.8,monthnames(i),0.)
        if(i == 1.or. i==7)call symbolc(-1.8,height/2.,0.8,'0m')
        call butler_psk(mean_salt(i,ini_long:fin_long,ini_lat:fin_lat,0),width2,height,0.,33.,34.3,0.1,'b2r',13,bpt1 = 9,r = r1,g = g1,b = b1) ! 0m
        call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 20,symbols = stat)
        call plot(0.,-height-1.,-3)

        if(i == 1.or. i==7)call symbolc(-1.8,height/2.,0.8,'50m')
        call butler_psk(mean_salt(i,ini_long:fin_long,ini_lat:fin_lat,4),width2,height,0.,33.,34.3,0.1,'b2r',13,bpt1 = 9) ! 50m
        call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 20,symbols = stat)
        call plot(0.,-height-1.,-3)

        if(i == 1.or. i==7)call symbolc(-2.,height/2.,0.8,'100m')
        call butler_psk(mean_salt(i,ini_long:fin_long,ini_lat:fin_lat,6),width2,height,0.,33.,34.3,0.1,'b2r',13,bpt1 = 9) ! 100m
        call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 20,symbols = stat)
        call plot(0.,-height-1.,-3)
        if(i == 6)call newpage
    end do

    !!!!!!!! Sem !!!!!!!!

    print*,'max value of sem is:',maxval(sem)
    print*,'min value of sem is:',minval(sem)
    call newpage(h = 'JODC Salinity SEM')
    do i = 1, 12
        call plotback('original')
        if(i==1.or.i==7)then 
            stat = .true.
        else
            stat = .false.
        end if
        call plot((width2+0.3)*real(mod(i-1,6)),0.,-3)
        call symbolc(width2/2.,height+0.4,0.8,monthnames(i),0.)
        if(i == 1.or. i==7)call symbolc(-1.8,height/2.,0.8,'0m')
        call butler_psk(sem(i,ini_long:fin_long,ini_lat:fin_lat,0),width2,height,0.,0.,0.1,0.01,'b2r',10,bpt1 = 6,r = r2,g = g2,b = b2) ! 0m
        call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 20,symbols = stat)
        call plot(0.,-height-1.,-3)

        if(i == 1.or. i==7)call symbolc(-1.8,height/2.,0.8,'50m')
        call butler_psk(sem(i,ini_long:fin_long,ini_lat:fin_lat,4),width2,height,0.,0.,0.1,0.01,'b2r',10,bpt1 = 6) ! 50m
        call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 20,symbols = stat)
        call plot(0.,-height-1.,-3)

        if(i == 1.or. i==7)call symbolc(-2.,height/2.,0.8,'100m')
        call butler_psk(sem(i,ini_long:fin_long,ini_lat:fin_lat,6),width2,height,0.,0.,0.1,0.01,'b2r',10,bpt1 = 6) ! 100m
        call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 20,symbols = stat)
        call plot(0.,-height-1.,-3)
        if(i == 6)call newpage
    end do



    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                    ! FOR EXAMINATION OF DATA POINTS
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
    !     call butler_psbet(mean_salt(i,ini_long:fin_long,ini_lat:fin_lat,0),width2,height,0.,33.,34.3,0.1,'b2r',13,bpt1 = 9,thicc = 10,r = r1,g = g1,b = b1) ! 0m
    !     call plot(width2/real(fin_long - ini_long)/2.,height/real(fin_lat - ini_lat)/2.,-3)
    !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2-width2/real(fin_long - ini_long),symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 20,symbols = stat)
    !     call plot(-width2/real(fin_long - ini_long)/2.,-height/real(fin_lat - ini_lat)/2.,-3)
    !     call plot(0.,-height-1.,-3)

    !     if(i == 1.or. i==7)call symbolc(-1.8,height/2.,0.8,'50m')
    !     call butler_psbet(mean_salt(i,ini_long:fin_long,ini_lat:fin_lat,4),width2,height,0.,33.,34.3,0.1,'b2r',13,bpt1 = 9) ! 50m
    !     call plot(width2/real(fin_long - ini_long)/2.,height/real(fin_lat - ini_lat)/2.,-3)
    !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2-width2/real(fin_long - ini_long),symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 20,symbols = stat)
    !     call plot(-width2/real(fin_long - ini_long)/2.,-height/real(fin_lat - ini_lat)/2.,-3)
    !     call plot(0.,-height-1.,-3)

    !     if(i == 1.or. i==7)call symbolc(-2.,height/2.,0.8,'100m')
    !     call butler_psbet(mean_salt(i,ini_long:fin_long,ini_lat:fin_lat,6),width2,height,0.,33.,34.3,0.1,'b2r',13,bpt1 = 9) ! 100m
    !     call plot(width2/real(fin_long - ini_long)/2.,height/real(fin_lat - ini_lat)/2.,-3)
    !     call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2-width2/real(fin_long - ini_long),symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 20,symbols = stat)
    !     call plot(-width2/real(fin_long - ini_long)/2.,-height/real(fin_lat - ini_lat)/2.,-3)
    !     call plot(0.,-height-1.,-3)
    !     if(i == 6)call newpage
    ! end do

    call newpage
    call ocenter
    call colorscale(13,r1,g1,b1,33.,34.3,1,0.8,1,20.,0.3,lt = 1, gt = 1,rangle = 90.)
    call plot(3.,0.,-3)
    call colorscale(10,r2,g2,b2,0.,0.1,1,0.8,2,20.,0.3,gt = 1,rangle = 90.)

    ! call newpage
    ! call obottoms
    ! call map(24,46,122,148,15.)
    call plote
end program 
