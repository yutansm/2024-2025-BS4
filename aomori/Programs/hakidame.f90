! program barotropic_model_inputs
!     use always
!     real,dimension(660,300)::bathy_noshelf,init_surf


!     bathy_noshelf = -1000. ! set the whole array to -1000m
!     bathy_noshelf(301:360,:) = 0. ! land region
!     bathy_noshelf(301:360,141:160) = -100. ! strait region depth is -100m

!     open(123,file = '../MITgcm/verification/yuta_baro/input1/bathy_noshelf.bin',form = 'unformatted',status = 'replace',action = 'write',access = 'direct',recl = 4*660*300,convert = 'big_endian')
!     write(123,rec = 1) bathy_noshelf
!     close(123)
!     call plots2(nnfile = 'bathy_noshelf',oopt = 'obottoms',x = 1.,y = 1.)
!     call butler_mask(bathy_noshelf,11.,5.,-1000.,-100.,b = 0.8)
!     call butler_mask(bathy_noshelf,11.,5.,-100.,0.,b = 1.,r = 0.4,g = 0.4)
!     call butler_mask(bathy_noshelf,11.,5.,0.,100.,g = 0.8)

!     init_surf = 0. ! set the whole array to 0m
!     init_surf(:300,:) = 0.02 ! 2cm higher for the left basin

!     open(123,file = '../MITgcm/verification/yuta_baro/input1/init_surf.bin',form = 'unformatted',status = 'replace',action = 'write',access = 'direct',recl = 4*660*300,convert = 'big_endian')
!     write(123,rec = 1) init_surf
!     close(123)

!     call plots2(nnfile = 'init_surf',oopt = 'obottoms',x = 1.,y = 1.)
!     call butler_mask(init_surf,11.,5.,0.01,0.02,b = 1.)

!     call plote
! end program

! program barotropic_model_inputs
!     use always
!     real,dimension(660,1)::OBSetaFile
!     real,dimension(660,300)::init_surf2
!     real,dimension(660,300)::bathy_noshelf_landperimiter
!     real,dimension(100,100)::bathy1

!     OBSetaFile = 0. ! set the whole array to 0m
!     do i = 1, 300
!         OBSetaFile(i,1) = 2./299.*(i-1) ! linear increase from 0 to 0.2m from left to right
!     end do
!     open(123,file = '../MITgcm/verification/yuta_baro/testrun_OB/OBS_eta.bin',form = 'unformatted',status = 'replace',action = 'write',access = 'direct',recl = 4*660*1,convert = 'big_endian')
!     write(123,rec = 1) OBSetaFile
!     close(123)

!     init_surf2 = 0.
!     init_surf2(:300,:5) = spread(OBSetaFile(:300,1),2,5) ! southernmost 5 grids have the same value as OBSetaFile
!     open(123,file = '../MITgcm/verification/yuta_baro/testrun_OB/init_surf2.bin',form = 'unformatted',status = 'replace',action = 'write',access = 'direct',recl = 4*660*300,convert = 'big_endian')
!     write(123,rec = 1) init_surf2
!     close(123)



!     ! call plots2(nnfile = 'init_surf',oopt = 'obottoms',x = 1.,y = 1.)
!     ! call butler_linegraph(OBSetaFile(:,1),15.,5.,0.,0.2,mem = .true.)

!     ! bathy_noshelf_landperimiter = -1000. ! set the whole array to -1000m
!     ! bathy_noshelf_landperimiter(301:360,:) = 0. ! land region
!     ! bathy_noshelf_landperimiter(301:360,141:160) = -100. ! strait region depth is -100m
!     ! bathy_noshelf_landperimiter(1,:) = 0. ! western boudary is land
!     ! bathy_noshelf_landperimiter(660,:) = 0. ! eastern boudary is land
!     ! bathy_noshelf_landperimiter(:,1) = 0. ! southern boudary is land
!     ! bathy_noshelf_landperimiter(:,300) = 0. ! northern boudary is land

!     ! call plots2(psfile = '../MITgcm/verification/yuta_baro/testrun_OB/bathy_noshelf_landperimiter.ps',oopt = 'obottoms',x = 1.,y = 1.)
!     ! call butler_mask(bathy_noshelf_landperimiter,11.,5.,-1000.,-100.,b = 0.8)
!     ! call butler_mask(bathy_noshelf_landperimiter,11.,5.,-100.,0.,b = 1.,r = 0.4,g = 0.4)
!     ! call butler_mask(bathy_noshelf_landperimiter,11.,5.,0.,100.,g = 0.8)



!     call plote
! end program

! program OB
!     use always

!     integer,dimension(660,300)::insideOBmaskFile


!     insideOBmaskFile = 1 ! set the whole array to 1 (outside OB) apparently, 1 means interior region
!     insideOBmaskFile(2:300,2) = 0 ! first wet cell row is inside OB
!     open(123,file = '../MITgcm/verification/yuta_baro/testrun_OB/insideOBmaskFile.bin',form = 'unformatted',status = 'replace',action = 'write',access = 'direct',recl = 4*660*300,convert = 'big_endian')
!     write(123,rec = 1) insideOBmaskFile
!     close(123)

!     call plots2(psfile = '../MITgcm/verification/yuta_baro/testrun_OB/insideOBmaskFile.ps',oopt = 'obottoms',x = 1.,y = 1.)
!     call butler_imask(insideOBmaskFile,11.,5.,1,g = 1.)

!     call plote
! end program

! program OBV
!     use always

!     real,dimension(660,1)::OBVFile

!     OBVFile = 0. ! set the whole array to 0m
!     OBVFile(:300,1) = 0.3 ! 0.3m/s northward velocity for the left basin

!     open(123,file = '../MITgcm/verification/yuta_baro/testrun_OB/OBVFile.bin',form = 'unformatted',status = 'replace',action = 'write',access = 'direct',recl = 4*660*1,convert = 'big_endian')
!     write(123,rec = 1) OBVFile
!     close(123)

! end program 

! program OBS_Stevens
!     use always
!     real,dimension(660,1)::OBS_Eta_Stevens

!     OBS_Eta_Stevens = 0. ! set the whole array to 0m
!     do i = 2, 300
!         OBS_Eta_Stevens(i,1) = 0.2/298.*(i-1) ! linear increase from 0 to 0.2m from left to right
!     end do
!     open(123,file = '../MITgcm/verification/yuta_baro/testrun_Stevens/OBS_Eta_Stevens.bin',form = 'unformatted',status = 'replace',action = 'write',access = 'direct',recl = 4*660*1,convert = 'big_endian')
!     write(123,rec = 1) OBS_Eta_Stevens
!     close(123)
    
! end program

! program small_OBV
!     use always

!     real,dimension(660,1)::OBVFile

!     OBVFile = 0. ! set the whole array to 0m
!     OBVFile(:300,1) = 3.3*10.**(-4) ! 0.00033m/s northward velocity for the left basin = 0.2sv for the whole basin

!     open(123,file = '../MITgcm/verification/yuta_baro/testrun_OB2_0.2sv/smallOBVFile.bin',form = 'unformatted',status = 'replace',action = 'write',access = 'direct',recl = 4*660*1,convert = 'big_endian')
!     write(123,rec = 1) OBVFile
!     close(123)

! end program 

!  program OBV_2sv
!      use always

!      real,dimension(660,1)::OBVFile

!      OBVFile = 0. ! set the whole array to 0m
!      OBVFile(:300,1) = 3.3*10.**(-3) ! 0.0033m/s northward velocity for the left basin = 2sv for the whole basin

!      open(123,file = '../MITgcm/verification/yuta_baro/testrun_OB2_0.2sv_rightopen/2svOBVFile.bin',form = 'unformatted',status = 'replace',action = 'write',access = 'direct',recl = 4*660*1,convert = 'big_endian')
!      write(123,rec = 1) OBVFile
!      close(123)

!  end program

! program bathy_withshelf
!     use always
!     real,dimension(660,300)::bathy_withshelf_landperimiter

!     bathy_withshelf_landperimiter = -1000. ! set the whole array to -1000m
!     bathy_withshelf_landperimiter(301:360,:) = 0. ! land region
!     bathy_withshelf_landperimiter(301:360,141:160) = -100. ! strait region depth is -100m
!     bathy_withshelf_landperimiter(1,:) = 0. ! western boudary is land
!     bathy_withshelf_landperimiter(660,:) = 0. ! eastern boudary is land
!     bathy_withshelf_landperimiter(:,1) = 0. ! southern boudary is land
!     bathy_withshelf_landperimiter(:,300) = 0. ! northern boudary is land
!     do i = 1 , 40
!         bathy_withshelf_landperimiter(261+i,:) = -1000. + real(i) / 40. * 900. ! shelf region depth linearly decreases from -1000m to -100m
!         bathy_withshelf_landperimiter(361+i,:) = -100. - real(i) / 40. * 900. ! east side shelf region depth linearly increases from -100m to -1000m
!     end do

!     call plots2(psfile = '../MITgcm/verification/yuta_baro/testrun_OB2_0.2sv_rightopen/with_shelf/bathy_withshelf_landperimiter.ps',oopt = 'obottoms',x = 1.,y = 1.)
!     call butler_psk(bathy_withshelf_landperimiter,11.,5.,0.,-1000.,0.,100.,'b2cy',10,6,conti = -1000.,continc = 100.)

!     open(123,file = '../MITgcm/verification/yuta_baro/testrun_OB2_0.2sv_rightopen/with_shelf/bathy_withshelf_landperimiter.bin',form = 'unformatted',status = 'replace',action = 'write',access = 'direct',recl = 4*660*300,convert = 'big_endian')
!     write(123,rec = 1) bathy_withshelf_landperimiter
!     close(123)
!     call plote

! end program

! program bathy_20layermodel
!     use always
!     real,dimension(100,100)::bathy_noshelf
!     real,dimension(:),allocatable::r,g,b

    
!     call plots2(psfile = '../MITgcm/verification/yuta_second_model/run3_outhv_noshelf/bathy_noshelf.ps',oopt = 'obottoms',x = 1., y = 1.)
!     open(123, file = '../MITgcm/verification/yuta_second_model/run3_outhv_noshelf/bathy1.bin',form = 'unformatted', status = 'old',access = 'direct',recl = 4 * 100 * 100, convert = 'big_endian')
!     read(123,rec = 1) bathy_noshelf
!     close(123)
!     bathy_noshelf(67:70,:) = -1000. ! no shelf


!     call butler_psbet(bathy_noshelf,11.,11.,0.,-1000.,0.,200.,'b2cy',5,3, r = r, g = g, b = b)
!     call colorscale(r,g,b,-1000.,0.,1,0.6,-1,10.,0.3,rangle = 90.,x = 11. + 2., y = 5.5)
!     call butler_mask(bathy_noshelf,11.,11.,-100.,-50.,g = 1.)
!     call butler_mask(bathy_noshelf,11.,11.,-50.,0.)

!     ! do i = 67,71
!     !     print*,bathy_noshelf(i,:)
!     ! end do
!     open(123,file = '../MITgcm/verification/yuta_second_model/run3_outhv_noshelf/bathy_noshelf.bin',form = 'unformatted',status = 'replace',action = 'write',access = 'direct',recl = 4*100*100,convert = 'big_endian')
!     write(123,rec = 1) bathy_noshelf
!     close(123)


!     call plote

! end program

program relaxmask_ps
    use always
    real,dimension(100,100)::relaxmask,relaxmask2

    call plots2(psfile = '../MITgcm/verification/yuta_second_model/run3_outhv_noshelf/relaxmask.ps',oopt = 'obottoms',x = 1., y = 1.)

    open(123, file = '../MITgcm/verification/yuta_second_model/run3_outhv_noshelf/relaxmask.bin',form = 'unformatted',status = 'old',access = 'direct',recl = 4 * 100 * 100, convert = 'big_endian')
    read(123,rec = 1) relaxmask
    close(123)
    open(123, file = '../MITgcm/verification/yuta_second_model/run3_outhv_noshelf/relaxmask_2.bin',form = 'unformatted',status = 'old',access = 'direct',recl = 4 * 100 * 100, convert = 'big_endian')
    read(123,rec = 1) relaxmask2
    close(123)

    call symbolc(5.,-1.,0.6,'relaxmask1')
    call butler_mask(relaxmask,10.,10.,1.,1.,r = 1.)
    call butler_mask(relaxmask,10.,10.,10.,10.,b = 1.)
    call plot(11.,0.,-3)
    call symbolc(5.,-1.,0.6,'relaxmask2')
    call butler_mask(relaxmask2,10.,10.,1.,1.,r = 1.)
    call butler_mask(relaxmask2,10.,10.,10.,10.,b = 1.)
    call header('red = 1, blue = 10')

    ! do i = 1, 100
    !     do j = 1, 100
    !         print*,i,j,relaxmask(i,j),relaxmask2(i,j)
    !     end do
    ! end do
    call plote

end program