program modelfiles
    use always 
    implicit none  
    real,dimension(:),allocatable::dh
    real,dimension(:,:,:,:),allocatable::avpotemp,avsal
    real,dimension(60,100)::bathy,iniEta
    real,dimension(15,20)::OBSTNline,OBSSNline
    real,dimension(60,20)::OBST,OBSS,OBSV
    real,dimension(:,:),allocatable::OBSV59
    real,dimension(60,100,20)::iniT,iniS,iniV
    real::dx,ploty,diff

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            ! make OBS files first
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! temp and sal
    call calibrated_data51(potemp_c5,sal_c5) ! 15*12*2*9*400
    call avsemdata_5D(potemp_c5,'dim1',mean_4D = avpotemp)
    call avsemdata_5D(sal_c5,'dim1',mean_4D = avsal)
    call DATA2OBJ(0,15,20,1,avpotemp(9,1,4:9,:),filenamewoex = "../MITgcm/verification/yuta's_first_model/input_f_bdnv_rough/OBS_T",OBJ_2D=OBSTNline)
    call DATA2OBJ(0,15,20,1,avsal(9,1,4:9,:),filenamewoex = "../MITgcm/verification/yuta's_first_model/input_f_bdnv_rough/OBS_S",OBJ_2D=OBSSNline)
    OBST(46:,:) = OBSTNline
    do i = 1, 45;OBST(i,:) = OBSTNline(1,:);end do ! copying st6 data westward
    OBSS(46:,:) = OBSSNline
    do i = 1, 45;OBSS(i,:) = OBSSNline(1,:);end do ! copying st6 data westward
! vel and eta
    call calc_dh(dh,temp_2D = OBST,sal_2D = OBSS,delta_zdb = 20.)
    diff = dh(1)
    do i = 1, 60
        dh(i) = dh(i) - diff
    end do
    print*,maxval(dh),minval(dh)
    iniEta = spread(dh,2,100)
    call calc_geovel(OBSV59,10000.,delta_zdb = 20.,temp_2D = OBST,sal_2D = OBSS,lat = 41.) ! this creates an array of 59x20
    OBSV(2:60,:) = OBSV59;OBSV(1,:) = OBSV59(1,:) ! filling gap in the array


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            ! make initial files from OBS files
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! temp and sal
    iniT = spread(OBST,2,100)
    iniS = spread(OBSS,2,100)
    iniV = spread(OBSV,2,100)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            ! make bathymetry file
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    bathy = -400.
    bathy(1,:) = 0.
    bathy(60,:) = 0.

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                ! creating binarys
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    open(22, file = "../MITgcm/verification/yuta's_first_model/input_f_bdnv_rough/OBS_T.bin", status='replace', action='write', form='unformatted', access='direct', recl=4*60*20, convert='big_endian')
    write(22,rec=1) OBST
    close(22)
    open(23, file = "../MITgcm/verification/yuta's_first_model/input_f_bdnv_rough/OBS_S.bin", status='replace', action='write', form='unformatted', access='direct', recl=4*60*20, convert='big_endian')
    write(23,rec=1) OBSS
    close(23)
    open(24, file = "../MITgcm/verification/yuta's_first_model/input_f_bdnv_rough/OBS_V.bin", status='replace', action='write', form='unformatted', access='direct', recl=4*60*20, convert='big_endian')
    write(24,rec=1) OBSV
    close(24)
    open(23, file="../MITgcm/verification/yuta's_first_model/input_f_bdnv_rough/initial_T.bin", status='replace', action='write', form='unformatted', access='direct', recl=4*60*100*20, convert='big_endian')
    write(23,rec=1) iniT
    close(23)
    open(24, file="../MITgcm/verification/yuta's_first_model/input_f_bdnv_rough/initial_S.bin", status='replace', action='write', form='unformatted', access='direct', recl=4*60*100*20, convert='big_endian')
    write(24,rec=1) iniS
    close(24)
    open(25, file="../MITgcm/verification/yuta's_first_model/input_f_bdnv_rough/initial_V.bin", status='replace', action='write', form='unformatted', access='direct', recl=4*60*100*20, convert='big_endian')
    write(25,rec=1) iniV
    close(25)
    open(26, file="../MITgcm/verification/yuta's_first_model/input_f_bdnv_rough/initial_Eta.bin", status='replace', action='write', form='unformatted', access='direct', recl=4*60*100, convert='big_endian')
    write(26,rec=1) iniEta
    close(26)
    open(27, file="../MITgcm/verification/yuta's_first_model/input_f_bdnv_rough/OBS_Eta.bin", status='replace', action='write', form='unformatted', access='direct', recl=4*60, convert='big_endian')
    write(27,rec=1) dh
    close(27)
    open(27, file="../MITgcm/verification/yuta's_first_model/input_f_bdnv_rough/bathy.bin", status='replace', action='write', form='unformatted', access='direct', recl=4*60*100, convert='big_endian')
    write(27,rec=1) bathy
    close(27)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            ! creating graphs for confirmation
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call plots2(psfile = "../MITgcm/verification/yuta's_first_model/input_f_bdnv_rough/inputs.ps",oopt = 'otops')
    call butler_psk(iniS(:,40,:),5.,-10.,0.,33.95,34.3,0.05,'b2w2r',7,bpt1=4,centralize = 4)
    call butler_cont(iniT(:,40,:),5.,-10.,0.,0.,1.,thicc = 5) ! lowres temp
    call plot(6.,0.,-3)
    call butler_cont(iniV(:,40,:),5.,-10.,1000.,-0.2,0.05,thicc = 2,maskn = .true.) ! lowres v
    print*,maxval(iniV(:,40,:)),minval(iniV(:,40,:))
    call plot(6.,0.,-3)
    call obottoms
    call obottoms;call plot(2.,0.,-3)
    dx = 20./61.
    call box(20.,5.,3)
    do i = 1, 60
        call num_memori(0.,1.,10,5,0.6,1,5.,-90)
        call gmark_ratio(iniEta(i,40),0.,1.,5.,ploty)
        call gmark(dx*real(i),ploty,0.1,1)
    end do
    call plote




end program