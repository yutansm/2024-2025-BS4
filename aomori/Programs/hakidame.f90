! program read_files
!     use always
!     real,dimension(40,80,20)::iniT,iniS,iniV
!     real,dimension(40,20)::OBJT,OBJS,OBJV
!     open(23,file = "../MITgcm/verification/yuta's_first_model/input_fplane/OBS_T.bin", form = 'unformatted', status = 'old', access = 'direct',recl = 4*40*20,convert = 'big_endian')
!     read(23,rec=1) OBJT
!     close(23)
!     open(24,file = "../MITgcm/verification/yuta's_first_model/input_fplane/OBS_S.bin", form = 'unformatted', status = 'old', access = 'direct',recl = 4*40*20,convert = 'big_endian')
!     read(24,rec=1) OBJS
!     close(24)
!     open(25,file = "../MITgcm/verification/yuta's_first_model/input_fplane/OBS_V.bin", form = 'unformatted', status = 'old', access = 'direct',recl = 4*40*20,convert = 'big_endian')
!     read(25,rec=1) OBJV
!     close(25)
!     do i = 1, 80
!         iniT(:,i,:) = OBJT;iniS(:,i,:) = OBJS;iniV(:,i,:) = OBJV
!     end do
!     open(23,file = "../MITgcm/verification/yuta's_first_model/input_fplane/initial_T.bin", form = 'unformatted', status = 'replace', access = 'direct',recl = 4*40*80*20,convert = 'big_endian')
!     write(23,rec=1) iniT
!     close(23)
!     open(24,file = "../MITgcm/verification/yuta's_first_model/input_fplane/initial_S.bin", form = 'unformatted', status = 'replace', access = 'direct',recl = 4*40*80*20,convert = 'big_endian')
!     write(24,rec=1) iniS
!     close(24)
!     open(25,file = "../MITgcm/verification/yuta's_first_model/input_fplane/initial_V.bin", form = 'unformatted', status = 'replace', access = 'direct',recl = 4*40*80*20,convert = 'big_endian')
!     write(25,rec=1) iniV
!     close(25)
!     ! call plots2(psfile = "../MITgcm/verification/yuta's_first_model/input_fplane/initial_T_y26.ps",oopt = 'otops',h = 'Potemp and Sal')
!     ! call butler_psk(iniS(:,26,:),4.,-8.,0.,33.95,34.3,0.05,'b2w2r',7,bpt1=4,centralize = 4)
!     ! call butler_cont(iniT(:,26,:),4.,-8.,0.,0.,1.,thicc=5)
!     ! call plot(4.+2.,0.,-3)
!     ! call butler_cont(iniV(:,26,:),4.,-8.,0.,-1.,0.02,thicc=5)
!     ! call plote
! end program

! program createfiles
!     use always
!     ! real,dimension(40,80,20)::iniT,iniS,iniV
!     real,dimension(40,20)::OBJT,OBJS,OBJV
!     real,dimension(120,200,20)::iniT_big,iniS_big,iniV_big
!     real,dimension(120,20)::OBJT_big,OBJS_big,OBJV_big  
!     open(23,file = "../MITgcm/verification/yuta's_first_model/input_fplane/OBS_T.bin", form = 'unformatted', status = 'old', access = 'direct',recl = 4*40*20,convert = 'big_endian')
!     read(23,rec=1) OBJT
!     close(23)
!     open(24,file = "../MITgcm/verification/yuta's_first_model/input_fplane/OBS_S.bin", form = 'unformatted', status = 'old', access = 'direct',recl = 4*40*20,convert = 'big_endian')
!     read(24,rec=1) OBJS
!     close(24)
!     open(25,file = "../MITgcm/verification/yuta's_first_model/input_fplane/OBS_V.bin", form = 'unformatted', status = 'old', access = 'direct',recl = 4*40*20,convert = 'big_endian')
!     read(25,rec=1) OBJV
!     close(25)
!     OBJT_big(1:80,:) = spread(OBJT(1,:),1,80);OBJT_big(81:,:) = OBJT
!     OBJS_big(1:80,:) = spread(OBJS(1,:),1,80);OBJS_big(81:,:) = OBJS
!     OBJV_big(1:80,:) = spread(OBJV(1,:),1,80);OBJV_big(81:,:) = OBJV
!     open(23,file = "../MITgcm/verification/yuta's_first_model/input_f_bigdomain/OBS_T.bin", form = 'unformatted', status = 'replace', access = 'direct',recl = 4*120*20,convert = 'big_endian')
!     write(23,rec=1) OBJT_big
!     close(23)
!     open(24,file = "../MITgcm/verification/yuta's_first_model/input_f_bigdomain/OBS_S.bin", form = 'unformatted', status = 'replace', access = 'direct',recl = 4*120*20,convert = 'big_endian')
!     write(24,rec=1) OBJS_big
!     close(24)
!     open(25,file = "../MITgcm/verification/yuta's_first_model/input_f_bigdomain/OBS_V.bin", form = 'unformatted', status = 'replace', access = 'direct',recl = 4*120*20,convert = 'big_endian')
!     write(25,rec=1) OBJV_big
!     close(25)
!     do i = 1, 200
!         iniT_big(:,i,:) = OBJT_big;iniS_big(:,i,:) = OBJS_big;iniV_big(:,i,:) = OBJV_big
!     end do
!     open(23,file = "../MITgcm/verification/yuta's_first_model/input_f_bigdomain/initial_T.bin", form = 'unformatted', status = 'replace', access = 'direct',recl = 4*120*200*20,convert = 'big_endian')
!     write(23,rec=1) iniT_big
!     close(23)
!     open(24,file = "../MITgcm/verification/yuta's_first_model/input_f_bigdomain/initial_S.bin", form = 'unformatted', status = 'replace', access = 'direct',recl = 4*120*200*20,convert = 'big_endian')
!     write(24,rec=1) iniS_big
!     close(24)
!     open(25,file = "../MITgcm/verification/yuta's_first_model/input_f_bigdomain/initial_V.bin", form = 'unformatted', status = 'replace', access = 'direct',recl = 4*120*200*20,convert = 'big_endian')
!     write(25,rec=1) iniV_big
!     close(25)
!     call plots2(psfile = "../MITgcm/verification/yuta's_first_model/input_f_bigdomain/initial_values.ps",oopt = 'otops',h = 'TS and V')
!     call butler_psk(iniS_big(:,26,:),4.,-8.,0.,33.95,34.3,0.05,'b2w2r',7,bpt1=4,centralize = 4)
!     call butler_cont(iniT_big(:,26,:),4.,-8.,0.,0.,1.,thicc=5)
!     call plot(4.+2.,0.,-3)
!     call butler_cont(iniV_big(:,26,:),4.,-8.,0.,-1.,0.02,thicc=5)
!     call plote
! end program
! program bathimetry
!     real,dimension(120,200)::bathy
!     bathy = -400.
!     bathy(1,:) = 0. ! west is land
!     bathy(120,:) = 0. ! east is land
!     open(23,file = "../MITgcm/verification/yuta's_first_model/input_f_bigdomain/bathy.bin", form = 'unformatted', status = 'replace', access = 'direct',recl = 4*120*200,convert = 'big_endian')
!     write(23,rec=1) bathy
!     close(23)

! end program
program first_few_grids_and_days
    use netcdf
    use always
    implicit none

    character(len=256) :: filename
    integer :: ncid, status
    integer :: nvars, ndims, ngatts, unlimdimid
    integer :: varid
    character(len=nf90_max_name) :: varname
    integer, allocatable :: dimids(:), dimlens(:)
    real,dimension(:,:,:,:),allocatable::U,V,W,temp,sal
    real,dimension(:,:,:),allocatable::eta
    real,parameter::width = 2.5,height=4.
    


    filename = "../MITgcm/verification/Yuta's_first_model/run_test_obs/results/state.nc"
! data obtainment
    ! Open the NetCDF file
    status = nf90_open(trim(filename), nf90_nowrite, ncid)
    if (status /= nf90_noerr) call handle_err(status)

    ! Get information about the file
    status = nf90_inquire(ncid, ndims, nvars, ngatts, unlimdimid)
    if (status /= nf90_noerr) call handle_err(status)

    print *, "Number of variables:", nvars

    ! Iterate through all variables to get basic idea of the file
    do varid = 1, nvars
        ! Get variable name and number of dimensions
        status = nf90_inquire_variable(ncid, varid, varname, ndims=ndims)
        if (status /= nf90_noerr) call handle_err(status)

        print *, "Variable ", trim(varname), " has ", ndims, " dimensions"

        ! Allocate arrays for dimension IDs and lengths
        allocate(dimids(ndims), dimlens(ndims))

        ! Get dimension IDs
        status = nf90_inquire_variable(ncid, varid, dimids=dimids)
        if (status /= nf90_noerr) call handle_err(status)

        ! Get dimension lengths
        do i = 1, ndims
            status = nf90_inquire_dimension(ncid, dimids(i), len=dimlens(i))
            if (status /= nf90_noerr) call handle_err(status)
            print *, "  Dimension ", i, " length: ", dimlens(i)
        end do

        ! Allocate arrays based on variable name and then read data
        select case(trim(varname))
        case('U')
            allocate(U(dimlens(1), dimlens(2), dimlens(3), dimlens(4)))
            status = nf90_get_var(ncid, varid, U)
            print*,'minimum U:',minval(U);print*,'maximum U:',maxval(U)
        case('V')
            allocate(V(dimlens(1), dimlens(2), dimlens(3), dimlens(4)))
            status = nf90_get_var(ncid, varid, V)
            print*,'minimum V:',minval(V);print*,'maximum V:',maxval(V)
        case('W')
            allocate(W(dimlens(1), dimlens(2), dimlens(3), dimlens(4)))
            status = nf90_get_var(ncid, varid, W)
            print*,'minimum W:',minval(W);print*,'maximum W:',maxval(W)
        case('Temp')
            allocate(temp(dimlens(1), dimlens(2), dimlens(3), dimlens(4)))
            status = nf90_get_var(ncid, varid, temp)
            print*,'minimum Temp:',minval(temp);print*,'maximum Temp:',maxval(temp)
        case('S')
            allocate(sal(dimlens(1), dimlens(2), dimlens(3), dimlens(4)))
            status = nf90_get_var(ncid, varid, sal)
            print*,'minimum S:',minval(sal);print*,'maximum S:',maxval(sal)
        case('Eta')
            allocate(eta(dimlens(1), dimlens(2), dimlens(3)))
            status = nf90_get_var(ncid, varid, eta)
            print*,'minimum Eta:',minval(eta);print*,'maximum Eta:',maxval(eta)
        end select
        deallocate(dimids, dimlens)



        print *, "------------------------"
    end do

    ! Close the file
    status = nf90_close(ncid)
    if (status /= nf90_noerr) call handle_err(status)

! data obtainment ends here
    call plots2(psfile="../MITgcm/verification/Yuta's_first_model/result_test_obs.ps", oopt='otops', h='x,y plane, Temp and Sal, day0-90')
    call plot(0.8,-height,-3)
    call plotsave('row1')
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     ! temp and sal xy plane, z = 1,3,6,11 (10m,50m,110m,210m)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! do i = 1,4
    !     call plotsave(int2str(i))
    !     if(i == 1)then;d = 1;call symbolr(-0.2,height/2.,0.6,'10m')
    !     else if(i == 2)then;d = 3;call symbolr(-0.2,height/2.,0.6,'50m')
    !     else if(i == 3)then;d = 6;call symbolr(-0.2,height/2.,0.6,'110m')
    !     else if(i == 4)then;d = 11;call symbolr(-0.2,height/2.,0.6,'210m')
    !     end if
    !     if(i==1)call symbolr(-0.1,height+0.5,0.6,'Grids;120*200;;d=')
    !     do j = 1, 10
    !         call memori(200,0.05,10,height,-90.,y = height/2.)
    !         call memori(120,0.05,10,width,0.,x = width/2.,y = 0.)
    !         if(i==1)call symbolc(width/2.,height+0.5,0.6,'day'//trim(int2str((j-1)*10)))
    !         call butler_psk(sal(:,:,d,(j-1)*10+1),width,height,0.,33.95,34.3,0.05,'b2w2r',7,bpt1=4,centralize = 4)
    !         call butler_cont(temp(:,:,d,(j-1)*10+1),width,height,0.,0.,1.,thicc = 5) 
    !         call plot(width+0.2,0.,-3)  
    !     end do
    !     call plotback(int2str(i))
    !     call plot(0.,-height-0.3,-3)
    ! end do
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     ! temp and sal xz plane, y = 10,30,50,70 (50km,150km,250km,350km)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call newpage(h = 'x,z plane, Temp and Sal, day0-90',x = 0.8,y = 0.)

    do i = 1,4
        if(i == 1)then;y = 1;call symbolr(-0.2,-height/2.,0.6,'1st;grid')
        else if(i == 2)then;y = 5;call symbolr(-0.2,-height/2.,0.6,'5nd ')
        else if(i == 3)then;y = 6;call symbolr(-0.2,-height/2.,0.6,'6rd')
        else if(i == 4)then;y = 7;call symbolr(-0.2,-height/2.,0.6,'7th')
        end if
        if(i==1)call symbolr(-0.1,0.5,0.6,'Grids;120*20;;y=')
        call plotsave(int2str(y))
        do j = 1, 10
            call memori(20,0.05,10,-height,-90.,y = -height/2.)
            call memori(120,0.05,10,width,0.,x = width/2.,y = -height)
            if(i==1)call symbolc(width/2.,0.5,0.6,'day'//trim(int2str(j-1)))
            call butler_psk(sal(:,y,:,(j-1)+1),width,-height,0.,33.95,34.3,0.05,'b2w2r',7,bpt1=4,centralize = 4)
            call butler_cont(temp(:,y,:,(j-1)+1),width,-height,0.,0.,1.,thicc = 5) 
            call plot(width+0.2,0.,-3)  
        end do
        call plotback(int2str(y))
        call plot(0.,-height-0.3,-3)
    end do

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                            ! vvel xy plane, z = 1,3,6,11 (10m,50m,110m,210m)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! call newpage(h = 'x,y plane, V, day0-90',x = 0.8,y = -height)
    ! do i = 1,4
    !     if(i == 1)then;d = 1;call symbolr(-0.2,height/2.,0.6,'10m')
    !     else if(i == 2)then;d = 3;call symbolr(-0.2,height/2.,0.6,'50m')
    !     else if(i == 3)then;d = 6;call symbolr(-0.2,height/2.,0.6,'110m')
    !     else if(i == 4)then;d = 11;call symbolr(-0.2,height/2.,0.6,'210m')
    !     end if
    !     if(i==1)call symbolr(-0.1,height+0.5,0.6,'Grids;120*200;;d=')
    !     call plotsave(int2str(i))
    !     do j = 1, 10
    !         call memori(200,0.05,10,height,-90.,y = height/2.)
    !         call memori(120,0.05,10,width,0.,x = width/2.,y = 0.)
    !         if(i==1)call symbolc(width/2.,height+0.5,0.6,'day'//trim(int2str((j-1)*10)))
    !         call butler_psmask(V(:,:,d,(j-1)*10+1),width,height,-1.,0.,r = 0.4 ,g = 0.4 ,b = 0.4)
    !         call butler_cont(V(:,:,d,(j-1)*10+1),width,height,0.,-0.2,0.05,thicc = 2) 
    !         call plot(width+0.2,0.,-3)
    !     end do
    !     call plotback(int2str(i))
    !     call plot(0.,-height-0.3,-3)
    ! end do
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                        ! vvel xz plane, y = 10,30,50,70 (50km,150km,250km,350km)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call newpage(h = 'x,z plane, V, day0-90',x = 0.8,y = 0.)
    do i = 1,4
        if(i == 1)then;y = 1;call symbolr(-0.2,-height/2.,0.6,'1st;grid')
        else if(i == 2)then;y = 5;call symbolr(-0.2,-height/2.,0.6,'5nd ')
        else if(i == 3)then;y = 6;call symbolr(-0.2,-height/2.,0.6,'6rd')
        else if(i == 4)then;y = 7;call symbolr(-0.2,-height/2.,0.6,'7th')
        end if
        if(i==1)call symbolr(-0.1,0.5,0.6,'Grids;120*20;;y=')
        call plotsave(int2str(y))
        do j = 1, 10
            call memori(20,0.05,10,-height,-90.,y = -height/2.)
            call memori(120,0.05,10,width,0.,x = width/2.,y = -height)
            if(i==1)call symbolc(width/2.,0.5,0.6,'day'//trim(int2str((j-1))))
            call butler_psmask(V(:,y,:,(j-1)+1),width,-height,-1.,0.,r = 0.4 ,g = 0.4 ,b = 0.4)
            call butler_cont(V(:,y,:,(j-1)+1),width,-height,0.,-0.2,0.05,thicc = 2) 
            call plot(width+0.2,0.,-3)
        end do
        call plotback(int2str(y))
        call plot(0.,-height-0.3,-3)
    end do

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    ! uvel xy plane, z = 1,3,6,11 (10m,50m,110m,210m)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! call newpage(h = 'x,y plane, U, day0-90',x = 0.8,y = -height)
    ! do i = 1,4
    !     if(i == 1)then;d = 1;call symbolr(-0.2,height/2.,0.6,'10m')
    !     else if(i == 2)then;d = 3;call symbolr(-0.2,height/2.,0.6,'50m')
    !     else if(i == 3)then;d = 6;call symbolr(-0.2,height/2.,0.6,'110m')
    !     else if(i == 4)then;d = 11;call symbolr(-0.2,height/2.,0.6,'210m')
    !     end if
    !     if(i==1)call symbolr(-0.1,height+0.5,0.6,'Grids;120*200;;d=')
    !     call plotsave(int2str(i))
    !     do j = 1, 10
    !         call memori(200,0.05,10,height,-90.,y = height/2.)
    !         call memori(120,0.05,10,width,0.,x = width/2.,y = 0.)
    !         if(i==1)call symbolc(width/2.,height+0.5,0.6,'day'//trim(int2str((j-1))))
    !         call butler_psmask(U(:,:,d,(j-1)+1),width,height,-1.,0.,r = 0.4 ,g = 0.4 ,b = 0.4)
    !         call butler_cont(U(:,:,d,(j-1)+1),width,height,0.,-0.2,0.05,thicc = 2) 
    !         call plot(width+0.2,0.,-3)
    !     end do
    !     call plotback(int2str(i))
    !     call plot(0.,-height-0.3,-3)
    ! end do
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    ! uvel xz plane, y = 10,30,50,70 (50km,150km,250km,350km)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! call newpage(h = 'x,z plane, U, day0-90',x = 0.8,y = 0.)
    ! do i = 1, 4
    !     if(i == 1)then;y = 10;call symbolr(-0.2,-height/2.,0.6,'50km')
    !     else if(i == 2)then;y = 30;call symbolr(-0.2,-height/2.,0.6,'150km')
    !     else if(i == 3)then;y = 50;call symbolr(-0.2,-height/2.,0.6,'250km')
    !     else if(i == 4)then;y = 70;call symbolr(-0.2,-height/2.,0.6,'350km')
    !     end if
    !     if(i==1)call symbolr(-0.1,0.5,0.6,'Grids;120*20;;y=')
    !     call plotsave(int2str(y))
    !     do j = 1, 10
    !         call memori(20,0.05,10,-height,-90.,y = -height/2.)
    !         call memori(120,0.05,10,width,0.,x = width/2.,y = -height)
    !         if(i==1)call symbolc(width/2.,0.5,0.6,'day'//trim(int2str((j-1))))
    !         call butler_psmask(U(:,y,:,(j-1)+1),width,-height,-1.,0.,r = 0.4 ,g = 0.4 ,b = 0.4)
    !         call butler_cont(U(:,y,:,(j-1)+1),width,-height,0.,-0.2,0.05,thicc = 2) 
    !         call plot(width+0.2,0.,-3)
    !     end do
    !     call plotback(int2str(y))
    !     call plot(0.,-height-0.3,-3)
    ! end do
    call plote

end program 

subroutine handle_err(status)
    use netcdf
    integer, intent(in) :: status
    if (status /= nf90_noerr) then
        print *, trim(nf90_strerror(status))
        stop "Stopped"
    end if
end subroutine handle_err