program hakidame
    use always
    implicit none
    real,parameter::width = 25.,height=13.,width2 = 5.,height2 = 3.
    real,dimension(15,12,5)::Q_per_station
    real,dimension(:,:),allocatable::geovel2d
    real,dimension(15,12,5,400)::geovel
    real,dimension(15,12)::Q_1thru2,Q_1thru3,Q_1thru4,Q_1thru5,Q_1thru6    ! transport through stations 
    real,dimension(:),allocatable::av12,av23,av34,av45,av56,s12,s23,s34,s45,s56,sem12,sem23,sem34,sem45,sem56
    real,dimension(:),allocatable::avQ_1thru2,avQ_1thru3,avQ_1thru4,avQ_1thru5,avQ_1thru6,sQ_1thru2,sQ_1thru3,sQ_1thru4,sQ_1thru5,sQ_1thru6,semQ_1thru2,semQ_1thru3,semQ_1thru4,semQ_1thru5,semQ_1thru6
    real,dimension(14)::av12loop,av23loop,av34loop,av45loop,av56loop,s12loop,s23loop,s34loop,s45loop,s56loop,sem12loop,sem23loop,sem34loop,sem45loop,sem56loop,avarray,semarray
    real,dimension(14)::avQ_1thru2loop,avQ_1thru3loop,avQ_1thru4loop,avQ_1thru5loop,avQ_1thru6loop,sQ_1thru2loop,sQ_1thru3loop,sQ_1thru4loop,sQ_1thru5loop,sQ_1thru6loop,semQ_1thru2loop,semQ_1thru3loop,semQ_1thru4loop,semQ_1thru5loop,semQ_1thru6loop
    logical::memstat = .false.
    character(len=20)::tlabel
    real::r1,g1,b1
    integer::lthick

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Data obtainment and processing  !!!st12 = index(5),st23 = index(4),st34 = index(3),st45 = index(2),st56 = index(1)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! obtaining geostrophic velocities between station 1 and 6 for each month, each year
    call calibrated_data51(potemp_c5,sal_c5) ! 15*12*2*9*400
    do y = 1, 15
        do m = 1, 12
            call calc_geovel(geovel2d,delta_x,temp_2D = potemp_c5(y,m,1,4:9,:),sal_2D = sal_c5(y,m,1,4:9,:),lat = 41.)
            geovel(y,m,:,:) = geovel2d
            deallocate(geovel2d)
        end do
    end do
    ! calculating trasport per pairs of stations 1 through 6, Q_per_station. some transports are 0. Note geovel has 5 columns, not 6
    do y = 1, 15
        do m = 1, 12
            do i = 1, 5
                Q_per_station(y,m,i) = sum(geovel(y,m,i,:) * delta_x * 1 * 10.**(-6))  !v[m/s] * dx[m] * 1[m] * 10**(-6) = [Sv] per grid per station
                ! if(Q_per_station(y,m,i)==0..and.m/=1.and.m/=7)print*,'no data for station ',i,' in year ',y+2008,' month ',m
            end do
        end do
    end do
    ! calculating transport through stations 1-2, 1-3, 1-4, 1-5, 1-6
    Q_1thru2 = Q_per_station(:,:,5)        
    Q_1thru3 = Q_1thru2 + Q_per_station(:,:,4)
    Q_1thru4 = Q_1thru3 + Q_per_station(:,:,3)
    Q_1thru5 = Q_1thru4 + Q_per_station(:,:,2)  
    Q_1thru6 = Q_1thru5 + Q_per_station(:,:,1)   



    ! getting mean of every station paired transport
        ! st 1 and 2
        call avsemdata_2D(Q_per_station(:,:,5),'dim1',mean_1D = av12, s_1D = s12,sem_1D = sem12)
        ! st 2 and 3
        call avsemdata_2D(Q_per_station(:,:,4),'dim1',mean_1D = av23, s_1D = s23,sem_1D = sem23)
        ! st 3 and 4
        call avsemdata_2D(Q_per_station(:,:,3),'dim1',mean_1D = av34, s_1D = s34,sem_1D = sem34)
        ! st 4 and 5
        call avsemdata_2D(Q_per_station(:,:,2),'dim1',mean_1D = av45, s_1D = s45,sem_1D = sem45)
        ! st 5 and 6
        call avsemdata_2D(Q_per_station(:,:,1),'dim1',mean_1D = av56, s_1D = s56,sem_1D = sem56)

        ! filling in the gaps, linear interpolation
        av12(1) = (av12(12) + av12(2))/2. ; av12(7) = (av12(6) + av12(8))/2.
        av23(1) = (av23(12) + av23(2))/2. ; av23(7) = (av23(6) + av23(8))/2.
        av34(1) = (av34(12) + av34(2))/2. ; av34(7) = (av34(6) + av34(8))/2.
        av45(1) = (av45(12) + av45(2))/2. ; av45(7) = (av45(6) + av45(8))/2.
        av56(1) = (av56(12) + av56(2))/2. ; av56(7) = (av56(6) + av56(8))/2.


        ! looping
        av12loop(1:12) = av12 ; av12loop(13:14) = av12(1:2)
        av23loop(1:12) = av23 ; av23loop(13:14) = av23(1:2)
        av34loop(1:12) = av34 ; av34loop(13:14) = av34(1:2)
        av45loop(1:12) = av45 ; av45loop(13:14) = av45(1:2)
        av56loop(1:12) = av56 ; av56loop(13:14) = av56(1:2)
        s12loop(1:12) = s12 ; s12loop(13:14) = s12(1:2)
        s23loop(1:12) = s23 ; s23loop(13:14) = s23(1:2)
        s34loop(1:12) = s34 ; s34loop(13:14) = s34(1:2)
        s45loop(1:12) = s45 ; s45loop(13:14) = s45(1:2)
        s56loop(1:12) = s56 ; s56loop(13:14) = s56(1:2)
        sem12loop(1:12) = sem12 ; sem12loop(13:14) = sem12(1:2)
        sem23loop(1:12) = sem23 ; sem23loop(13:14) = sem23(1:2)
        sem34loop(1:12) = sem34 ; sem34loop(13:14) = sem34(1:2)
        sem45loop(1:12) = sem45 ; sem45loop(13:14) = sem45(1:2)
        sem56loop(1:12) = sem56 ; sem56loop(13:14) = sem56(1:2)

    ! getting mean of transports through stations 1-2, 1-3, 1-4, 1-5, 1-6
    call avsemdata_2D(Q_1thru2,'dim1',mean_1D = avQ_1thru2, s_1D = sQ_1thru2,sem_1D = semQ_1thru2)
    call avsemdata_2D(Q_1thru3,'dim1',mean_1D = avQ_1thru3, s_1D = sQ_1thru3,sem_1D = semQ_1thru3)
    call avsemdata_2D(Q_1thru4,'dim1',mean_1D = avQ_1thru4, s_1D = sQ_1thru4,sem_1D = semQ_1thru4)
    call avsemdata_2D(Q_1thru5,'dim1',mean_1D = avQ_1thru5, s_1D = sQ_1thru5,sem_1D = semQ_1thru5)
    call avsemdata_2D(Q_1thru6,'dim1',mean_1D = avQ_1thru6, s_1D = sQ_1thru6,sem_1D = semQ_1thru6)

    ! filling in the gaps, linear interpolation
    avQ_1thru2(1) = (avQ_1thru2(12) + avQ_1thru2(2))/2. ; avQ_1thru2(7) = (avQ_1thru2(6) + avQ_1thru2(8))/2.
    avQ_1thru3(1) = (avQ_1thru3(12) + avQ_1thru3(2))/2. ; avQ_1thru3(7) = (avQ_1thru3(6) + avQ_1thru3(8))/2.
    avQ_1thru4(1) = (avQ_1thru4(12) + avQ_1thru4(2))/2. ; avQ_1thru4(7) = (avQ_1thru4(6) + avQ_1thru4(8))/2.
    avQ_1thru5(1) = (avQ_1thru5(12) + avQ_1thru5(2))/2. ; avQ_1thru5(7) = (avQ_1thru5(6) + avQ_1thru5(8))/2.
    avQ_1thru6(1) = (avQ_1thru6(12) + avQ_1thru6(2))/2. ; avQ_1thru6(7) = (avQ_1thru6(6) + avQ_1thru6(8))/2.

    ! looping
    avQ_1thru2loop(1:12) = avQ_1thru2 ; avQ_1thru2loop(13:14) = avQ_1thru2(1:2)
    avQ_1thru3loop(1:12) = avQ_1thru3 ; avQ_1thru3loop(13:14) = avQ_1thru3(1:2)
    avQ_1thru4loop(1:12) = avQ_1thru4 ; avQ_1thru4loop(13:14) = avQ_1thru4(1:2)
    avQ_1thru5loop(1:12) = avQ_1thru5 ; avQ_1thru5loop(13:14) = avQ_1thru5(1:2)
    avQ_1thru6loop(1:12) = avQ_1thru6 ; avQ_1thru6loop(13:14) = avQ_1thru6(1:2)
    sQ_1thru2loop(1:12) = sQ_1thru2 ; sQ_1thru2loop(13:14) = sQ_1thru2(1:2)
    sQ_1thru3loop(1:12) = sQ_1thru3 ; sQ_1thru3loop(13:14) = sQ_1thru3(1:2)
    sQ_1thru4loop(1:12) = sQ_1thru4 ; sQ_1thru4loop(13:14) = sQ_1thru4(1:2)
    sQ_1thru5loop(1:12) = sQ_1thru5 ; sQ_1thru5loop(13:14) = sQ_1thru5(1:2)
    sQ_1thru6loop(1:12) = sQ_1thru6 ; sQ_1thru6loop(13:14) = sQ_1thru6(1:2)
    semQ_1thru2loop(1:12) = semQ_1thru2 ; semQ_1thru2loop(13:14) = semQ_1thru2(1:2)
    semQ_1thru3loop(1:12) = semQ_1thru3 ; semQ_1thru3loop(13:14) = semQ_1thru3(1:2)
    semQ_1thru4loop(1:12) = semQ_1thru4 ; semQ_1thru4loop(13:14) = semQ_1thru4(1:2)
    semQ_1thru5loop(1:12) = semQ_1thru5 ; semQ_1thru5loop(13:14) = semQ_1thru5(1:2)
    semQ_1thru6loop(1:12) = semQ_1thru6 ; semQ_1thru6loop(13:14) = semQ_1thru6(1:2)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                            ! Plotting
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! individual stations
    call plots2(oopt = 'otops',nnfile = 'geo_transport',y = -height2,h = 'Monthly Geostrophic Transports [Sv]')
    call plotsave('first')
    do i = 1, 5
        if(i == 1)then 
            avarray = av12loop ; semarray = sem12loop
            tlabel = 'Station 1-2'
        else if(i == 2)then
            avarray = av23loop ; semarray = sem23loop
            tlabel = 'Station 2-3'
        else if(i == 3)then
            avarray = av34loop ; semarray = sem34loop
            tlabel = 'Station 3-4'
        else if(i == 4)then
            avarray = av45loop ; semarray = sem45loop
            tlabel = 'Station 4-5'
        else if(i == 5)then
            avarray = av56loop ; semarray = sem56loop
            tlabel = 'Station 5-6'
        end if
        call mod12_memori(14,width2)
        if(i==1)memstat = .true.
        if(i/=1)memstat = .false.
        if(i/=1)then 
            call memori(3,0.05,1,height2,-90.,y = height2/2.,lthick = 3)
        end if
        call butler_linegraph(avarray,width2,height2,0.,1.,error_1D = semarray,mem = memstat,memiter = 3,tlabel = tlabel)
        call plot(width2+.5,0.,-3)
    end do

    ! all stations
    call plotback('first')
    call plot(1.,-height-1.,-3)

    do i = 1, 5
        if(i == 1)then 
            avarray = avQ_1thru2loop ; semarray = semQ_1thru2loop
            tlabel = 'Station 1-2'
            r1 = 1.;g1 = 0.;b1 = 0.
            lthick = 11
        else if(i == 2)then
            avarray = avQ_1thru3loop ; semarray = semQ_1thru3loop
            tlabel = 'Station 1-3'
            r1 = 0.;g1 = 1.;b1 = 0.
            lthick = 9
        else if(i == 3)then
            avarray = avQ_1thru4loop ; semarray = semQ_1thru4loop
            tlabel = 'Station 1-4'
            r1 = 0.;g1 = 0.;b1 = 1.
            lthick = 7
        else if(i == 4)then
            avarray = avQ_1thru5loop ; semarray = semQ_1thru5loop
            tlabel = 'Station 1-5'
            r1 = 1.;g1 = .5;b1 = 0.
            lthick = 5
        else if(i == 5)then
            avarray = avQ_1thru6loop ; semarray = semQ_1thru6loop
            tlabel = 'Station 1-6'
            r1 = 1.;g1 = 0.;b1 = 1.
            lthick = 3
        end if
        if(i==1)call mod12_memori(14,width,symbol_size = 0.6)
        ! if(i==1)memstat = .true.
        ! if(i/=1)memstat = .false.
        if(i == 1) call num_memori(0.,3.,4,1,1.,-1,height,-90)
        call butler_linegraph(avarray,width,height,0.,3.,error_1D = semarray,rl = r1,gl = g1,bl = b1,lthick = lthick)
    end do
    call plote
end program
