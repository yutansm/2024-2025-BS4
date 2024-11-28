program verify_OBS
    use always
    implicit none
    real,parameter::width = 7.,height=14.
    real,dimension(:,:,:,:),allocatable::avpotemp,avsal,avgeovel
    real,dimension(6,400)::psT,psS
    real, dimension(-1:22, 20) :: OBS_PT,OBS_S,OBS_V ! x and z dimensions

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !data obtainment and processing
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call calibrated_data51(potemp_c5,sal_c5) ! 15*12*2*9*400
    call geovel_array(51,geovel_5)

    call avsemdata_5D(potemp_c5,'dim1',mean_4D = avpotemp)
    call avsemdata_5D(sal_c5,'dim1',mean_4D = avsal)
    call avsemdata_5D(geovel_5,'dim1',mean_4D = avgeovel)

    psT = avpotemp(9,1,4:9,:);psS = avsal(9,1,4:9,:)

    open(unit=20, file="../MITgcm/verification/yuta's_first_model/input/OBS_T2.bin", status='old', action='read', form='unformatted', access='direct', recl=4*24*20, convert='big_endian')
    read(20,rec=1) OBS_PT
    close(20)
    open(unit=21, file="../MITgcm/verification/yuta's_first_model/input/OBS_S2.bin", status='old', action='read', form='unformatted', access='direct', recl=4*24*20, convert='big_endian')
    read(21,rec=1) OBS_S
    close(21)
    open(unit=22, file="../MITgcm/verification/yuta's_first_model/input/OBS_V2.bin", status='old', action='read', form='unformatted', access='direct', recl=4*24*20, convert='big_endian')
    read(22,rec=1) OBS_V
    close(22)

!!!!!!!!!!!!!!
    !plot
!!!!!!!!!!!!!!
call plots2(psfile = "../MITgcm/verification/Yuta's_first_model/input_OBS.ps",oopt = 'otops',h = 'Potemp and Sal')

    call symbolc(width/2.,0.8,0.6,'Model;1-OLx : Nx+OLx')
    call num_memori(-1.,22.,23,1,0.3,-1,width,0,y = -height)
    call num_memori(0.,20.,20,1,0.5,-1,-height,-90)
    call butler_psk(OBS_S,width,-height,0.,33.95,34.3,0.05,'b2w2r',7,bpt1=4,centralize = 4)
    call butler_cont(OBS_PT,width,-height,0.,0.,1.,thicc=5)
    
    call plot(width+2.,0.,-3)

    call symbolc(width/2.,0.8,0.6,'Model;1 : Nx')
    call num_memori(1.,20.,19,1,0.3,-1,width,0,y = -height)
    call num_memori(0.,20.,20,1,0.5,-1,-height,-90)
    call butler_psk(OBS_S(1:20,:),width,-height,0.,33.95,34.3,0.05,'b2w2r',7,bpt1=4,centralize = 4)
    call butler_cont(OBS_PT(1:20,:),width,-height,0.,0.,1.,thicc=5)

    call plot(width+2.,0.,-3)

    call symbolc(width/2.,0.5,0.6,'CTD Data')
    call num_memori(0.,400.,40,5,0.6,-1,-height,-90)
    call st_memori(1,6,width,1,0.6,y = -height)
    call butler_psk(psS,width,-height,0.,33.95,34.3,0.05,'b2w2r',7,bpt1=4,centralize = 4)
    call butler_cont(psT,width,-height,0.,0.,1.,thicc=5)

    call newpage(h = 'Geostrophic Velocity')
    print*,OBS_V(:,19),OBS_V(:,20)

    call symbolc(width/2.,0.8,0.6,'Model;1-OLx : Nx+OLx')
    call num_memori(-1.,22.,23,1,0.3,-1,width,0,y = -height)
    call num_memori(0.,20.,20,1,0.5,-1,-height,-90)
    call butler_cont(OBS_V,width,-height,100.,-20.,2.,thicc = 5,gap = 1)

    call plot(width+2.,0.,-3)

    call symbolc(width/2.,0.8,0.6,'Model;1 : Nx')
    call num_memori(1.,20.,19,1,0.3,-1,width,0,y = -height)
    call num_memori(0.,20.,20,1,0.5,-1,-height,-90)
    call butler_cont(OBS_V(1:20,:),width,-height,100.,-20.,2.,thicc = 5,gap = 1)

    call plot(width+2.,0.,-3)

    call symbolc(width/2.,0.5,0.6,'CTD Data')
    call num_memori(0.,400.,40,5,0.6,-1,-height,-90)
    call st_memori(1,6,width,1,0.6,y = -height)
    call butler_cont(avgeovel(9,1,5:9,:),width,-height,100.,-20.,2.,thicc = 5,gap = 1)


    call plote
end program