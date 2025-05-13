program linear_transfromations
    use always
    implicit none 
    type(JODC_TS) :: potemp,sal
    type(JODC_RHO) :: den
    real:: width = 4.,height,sali,salf,sigma
    real,dimension(:,:,:),allocatable::temp, u, v, w
    ! real,dimension(:,:)
    integer::depi
    integer,dimension(6)::depr = [0,50,100,200,300,400]
    real,dimension(:),allocatable::r1,g1,b1

    ! call JODC_data2(potemp = potemp,sal = sal,den = den,calibrate1 = .true.)
    ! ! do j = 1, 6
    ! !     depi = JODC_dep2index(depr(j),info = .true.)
    ! !     call plots2(nnfile = 'GEBCO_JODC_TEMP_'//int2str(depr(j))//'m',oopt = 'otops',mode = 'land', y = -4.,h = 'GEBCO_JODC_TEMP_'//int2str(depr(j))//'m')

    ! !     if(j==1)call plotsave('first')
    ! !     do i = 1,12
    ! !         call GEBCOmap2(120,150,20,46,width,paintland = .true.,height = height,symbols = .true.,symbol_freq = 4,symbol_size = 0.3)
    ! !         call symbolc(width/2.,height+0.3,0.7,monthnames(i))
    ! !         call butler_psk(potemp%mean(i,120:150,20:46,depi),width,height,0.,0.,28.,2.,'b2r',14,8,conti = 0.,continc = 1.,thicc = 5)
    ! !         if(i/=6)then 
    ! !             call plot(width+0.5,0.,-3)
    ! !         else
    ! !             call plotback('first');call plot(0.,-height-2.,-3)
    ! !         end if
    ! !     end do
        
    ! !     call plotback('first');call plot(0.,2*(-height-2.),-3)
    ! !     call symbolc(width/2.,height+0.3,0.7,'Annual mean')
    ! !     call GEBCOmap2(120,150,20,46,width,paintland = .true.,height = height,symbols = .true.,symbol_freq = 4,symbol_size = 0.3)
    ! !     call butler_psk(potemp%mean(0,120:150,20:46,depi),width,height,0.,0.,28.,2.,'b2r',14,8,conti = 0.,continc = 1.,r = r1, g = g1, b = b1,thicc = 5) 
    ! !     call colorscale(r1,g1,b1,0.,28.,2,0.6,-1,width*3+1.,0.3,lt = 1, gt = 1, x = 3*(width+0.5),y = width/2.)
        
    ! ! end do
    ! do j = 1, 6 ! message for me, do the yearly means 4/7 for comparing salinity of depth >>okay
    !     depi = JODC_dep2index(depr(j),info = .true.)
    !     call plots2(nnfile = 'GEBCO_JODC_POTEMPSAL_'//int2str(depr(j))//'m',oopt = 'otops',mode = 'land', y = -4.,h = 'GEBCO JODC Annual Mean PT&S')
    !     ! if(i == 1)sali = 33.8;salf = 35.
    !     if(j==1)call plotsave('first')
    !     do i = 1,12
    !         call GEBCOmap2(120,150,20,46,width,paintland = .true.,height = height,symbols = .true.,symbol_freq = 4,symbol_size = 0.3)
    !         call symbolc(width/2.,height+0.3,0.7,monthnames(i))
    !         call butler_psk(sal%mean(i,120:150,20:46,depi),width,height,0.,33.5,34.8,.1,'b2r',13,7)
    !         call butler_cont(potemp%mean(i,120:150,20:46,depi),width,height,0.,-10.,1.,thicc = 5)
    !         if(i/=6)then 
    !             call plot(width+0.5,0.,-3)
    !         else
    !             call plotback('first');call plot(0.,-height-2.,-3)
    !         end if
    !     end do
        
    !     call plotback('first');call plot(0.,2*(-height-2.),-3)
    !     call symbolc(width/2.,height+0.3,0.7,'Annual mean')
    !     call GEBCOmap2(120,150,20,46,width,paintland = .true.,height = height,symbols = .true.,symbol_freq = 4,symbol_size = 0.3)
    !     call butler_psk(sal%mean(0,120:150,20:46,depi),width,height,0.,33.5,34.8,.1,'b2r',13,7,r = r1, g = g1, b = b1) 
    !     call butler_cont(potemp%mean(0,120:150,20:46,depi),width,height,0.,-10.,1.,thicc = 5)
    !     call colorscale(r1,g1,b1,33.5,34.8,5,0.6,1,width*3+1.,0.3,lt = 1, gt = 1, x = 3*(width+0.5),y = width/2.)
        
    ! end do
        
    ! call plote

    ! print*,sigma

    ! call state2mat("..//MITgcm/verification/Yuta's_first_model/run_bd100days/RESULTS.nc0001/state.0000000000.t001.nc",info = .true.)
    ! call inquire_netcdf("..//MITgcm/verification/Yuta's_first_model/run_bd100days/RESULTS.nc0001/state.0000000000.t001.nc")
    call ERA5_1000hPa(temp,u,v,w,info = .true.)
    call plots2(nnfile = 'ERA5_1000hPa',oopt = 'obottoms',mode = 'land', y = -4.,h = 'ERA5 1000hPa')
    call GEBCOmap2(120,150,20,50,15.,)
    call butler_vector(u(:,:,1000),v(:,:,1000),)

end program 