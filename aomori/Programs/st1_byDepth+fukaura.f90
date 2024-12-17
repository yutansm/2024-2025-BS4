program monthly_mean_of_st1_by_depth_plus_Fukaura
    use always
    implicit none
    real,parameter::width = 10.,height = 6.
    real,dimension(:,:,:,:),allocatable::avpotemp,avsal,avsigma,sempotemp,semsal,semsigma
    real,dimension(15,12)::SSH_f,DH
    real,dimension(:),allocatable::ymeanSSH_f,SSHav_f,SSHsem_f,dh1d,ymeanDH,avDH,semDH
    real,dimension(13,400)::avpotemp_loop,avsal_loop,avsigma_loop,sempotemp_loop,semsal_loop,semsigma_loop  
    real,dimension(13)::SSHav_f_loop,SSHsem_f_loop,avDH_loop,semDH_loop,plotarray,semarray
    real,dimension(180)::sshtimeseries
    logical::memstat
    integer::lthick
    real::r1,g1,b1
    character(len = 10)::tlabel,blabel

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                            ! Data Obtainment and Manipulation
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Data Obtainment
    call calibrated_data2(potemp_c5,sal_c5,sigma_c5,51)
    call SSH_data(SSH_f,slabel = '深浦',convert = .true.,calibrate = .true.) ! 15*12
! SSH and DH
    call avsemdata_2D(SSH_f,'dim2',mean_1D = ymeanSSH_f,rmask = -999.) ! yearly means SSH of Fukaura 15 elements
    print*,ymeanSSH_f
    do i = 1, 15
        do j = 1, 12
            if(SSH_f(i,j) == 0..or. SSH_f(i,j) == -999.)then ! skip if no data
            else
                SSH_f(i,j) = SSH_f(i,j) - ymeanSSH_f(i) ! SSH_f now contains deviations from yearly means
            end if
        end do
    end do
    call avsemdata_2D(SSH_f,'dim1',mean_1D = SSHav_f,sem_1D = SSHsem_f,rmask = -999.) ! monthly means of deviations from yearly means
    SSHav_f = SSHav_f/10. ; SSHsem_f = SSHsem_f/10. ! mm to cm

    do i = 1, 15
        call calc_dh(dh1d,den_2D = sigma_c5(i,:,1,1,:)) ! calculating DH for st1, Nline
        DH(i,:) = dh1d
    end do
    DH = DH*100. ! m to cm
    call avsemdata_2D(DH,'dim2',mean_1D = ymeanDH) ! yearly means DH of st1 15 elements
    do i = 1, 15
        do j = 1, 12
            if(DH(i,j) == 0.)then ! skip if no data
            else
                DH(i,j) = DH(i,j) - ymeanDH(i) ! DH now contains deviations from yearly means
            end if
        end do
    end do
    call avsemdata_2D(DH,'dim1',mean_1D = avDH,sem_1D = semDH) ! monthly means of deviations from yearly means

! PT, S, Rho

    call avsemdata_5D(potemp_c5,'dim1',mean_4D = avpotemp,sem_4D = sempotemp) ! P
    call avsemdata_5D(sal_c5,'dim1',mean_4D = avsal,sem_4D = semsal) ! S
    call avsemdata_5D(sigma_c5,'dim1',mean_4D = avsigma,sem_4D = semsigma) ! Rho
    call avsemdata_2D(DH,'dim1',mean_1D = avDH,sem_1D = semDH) ! DH

! loop for plotting
    avpotemp_loop (1:12,:) = avpotemp(:,1,1,:);avpotemp_loop(13,:) = avpotemp(1,1,1,:)
    avsal_loop (1:12,:) = avsal(:,1,1,:);avsal_loop(13,:) = avsal(1,1,1,:)
    avsigma_loop (1:12,:) = avsigma(:,1,1,:);avsigma_loop(13,:) = avsigma(1,1,1,:)
    sempotemp_loop (1:12,:) = sempotemp(:,1,1,:);sempotemp_loop(13,:) = sempotemp(1,1,1,:)
    semsal_loop (1:12,:) = semsal(:,1,1,:);semsal_loop(13,:) = semsal(1,1,1,:)
    semsigma_loop (1:12,:) = semsigma(:,1,1,:);semsigma_loop(13,:) = semsigma(1,1,1,:)
    SSHav_f_loop(1:12) = SSHav_f;SSHav_f_loop(13) = SSHav_f(1)
    SSHsem_f_loop(1:12) = SSHsem_f;SSHsem_f_loop(13) = SSHsem_f(1)
    avDH_loop(1:12) = avDH;avDH_loop(13) = avDH(1)
    semDH_loop(1:12) = semDH;semDH_loop(13) = semDH(1)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                            ! Plotting
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    

    call plots2('../Plots/Favorites/st1_byDepth+Fukaura.ps',h = 'Station 1 Examination',oopt = 'otops',x = 1.,y = -height-.5)
    call plotsave('PT')
! PT
    do i = 1,4
        if(i == 1)then 
            d = 50;r1 = 1.;g1 = 0.;b1 = 0.;memstat = .true.; lthick = 5
            call mod12_memori(13,width);blabel = 'Months';tlabel = 'PT'
        elseif(i == 2)then 
            d = 100;r1 = 0.;g1 = 1.;b1 = 0.;memstat = .false.; lthick = 4
                                        blabel = '';tlabel = ''
        elseif(i == 3)then 
            d = 150;r1 = 0.;g1 = 0.;b1 = 1.;memstat = .false.; lthick = 3
                                        blabel = '';tlabel = ''
        elseif(i == 4)then 
            d = 200;r1 = 0.;g1 = 0.;b1 = 0.;memstat = .false.; lthick = 4
                                        blabel = '';tlabel = ''
        end if
        call butler_linegraph(avpotemp_loop(:,d),width,height,0.,24.,mem = memstat,memsymfreq = 5,error_1D = sempotemp_loop(:,d),memsymsize = 0.8,memflqt = -1,blabel = blabel,tlabel = tlabel,lthick = lthick,rl = r1,gl = g1,bl = b1)
    end do

! S
    call plot(width+4.,0.,-3)
    do i = 1, 4
        if(i == 1)then 
            d = 50;r1 = 1.;g1 = 0.;b1 = 0.;memstat = .true.; lthick = 6
            call mod12_memori(13,width);blabel = 'Months';tlabel = 'S'
        elseif(i == 2)then
            d = 100;r1 = 0.;g1 = 1.;b1 = 0.;memstat = .false.; lthick = 5
                                        blabel = '';tlabel = ''
        elseif(i == 3)then
            d = 150;r1 = 0.;g1 = 0.;b1 = 1.;memstat = .false.; lthick = 4
                                        blabel = '';tlabel = ''
        elseif(i == 4)then
            d = 200;r1 = 0.;g1 = 0.;b1 = 0.;memstat = .false.; lthick = 3
                                        blabel = '';tlabel = ''
        end if
        call butler_linegraph(avsal_loop(:,d),width,height,34.4,33.7,mem = memstat,error_1D = semsal_loop(:,d),memsymsize = 0.8,memiter = 15,memsymfreq = 2,memflqt = 1,blabel = blabel,tlabel = tlabel,lthick = lthick,rl = r1,gl = g1,bl = b1)
    end do

! Rho
    call plotback('PT')
    call plot(0.,-height-3.5,-3)
    do i = 1,4
        if(i == 1)then 
            d = 50;r1 = 1.;g1 = 0.;b1 = 0.;memstat = .true.; lthick = 6
            call mod12_memori(13,width);blabel = 'Months';tlabel = 'Rho'
        elseif(i == 2)then
            d = 100;r1 = 0.;g1 = 1.;b1 = 0.;memstat = .false.; lthick = 5
                                        blabel = '';tlabel = ''
        elseif(i == 3)then
            d = 150;r1 = 0.;g1 = 0.;b1 = 1.;memstat = .false.; lthick = 4
                                        blabel = '';tlabel = ''
        elseif(i == 4)then
            d = 200;r1 = 0.;g1 = 0.;b1 = 0.;memstat = .false.; lthick = 3
                                        blabel = '';tlabel = ''
        end if
        call butler_linegraph(avsigma_loop(:,d),width,height,27.0,24.0,mem = memstat,memiter = 31,memsymfreq = 10,error_1D = semsigma_loop(:,d),memsymsize = 0.8,memflqt = 1,blabel = blabel,tlabel = tlabel,lthick = lthick,rl = r1,gl = g1,bl = b1)          
    end do

! DH and SSH
    call plot(width+4.,0.,-3)
    do i = 1, 2
        if(i == 1)then 
            plotarray = avDH_loop ; semarray = semDH_loop
            r1 = 0.;g1 = 0.;b1 = 0.;memstat = .true.; lthick = 6
            call mod12_memori(13,width);blabel = 'Months';tlabel = 'DH & SSH'
        elseif(i == 2)then
            plotarray = SSHav_f_loop ; semarray = SSHsem_f_loop
            r1 = 1.;g1 = .5;b1 = 0.;memstat = .false.; lthick = 4
                                        blabel = '';tlabel = ''
        end if
        call butler_linegraph(plotarray,width,height,-15.,15.,mem = memstat,memiter = 7,memsymsize = 0.8,memsymfreq = 1,memflqt = -1,blabel = blabel,tlabel = tlabel,error_1D = semarray,lthick = lthick,rl = r1,gl = g1,bl = b1)

    end do

    ! call newpage

    ! call obottoms
    ! sshtimeseries = reshape(SSH_f,[180])
    ! call butler_linegraph(sshtimeseries,20.,10.,0.,200.,mem = .true.)
    call plote
end program