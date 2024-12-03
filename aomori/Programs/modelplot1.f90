program plot_model
    use netcdf
    use always
    implicit none

    character(len=256) :: ncfile,psfile
    integer :: ncid, status
    integer :: nvars, ndims, ngatts, unlimdimid
    integer :: varid
    character(len=nf90_max_name) :: varname
    integer, allocatable :: dimids(:), dimlens(:)
    real,dimension(:,:,:,:),allocatable::U,V,W,temp,sal
    real,dimension(:,:,:),allocatable::eta
    integer,dimension(:),allocatable::iterations
    real,parameter::width = 2.5,height=4.
    integer::initial_day,day_inc,number_of_graphs,d1,d2,d3,d4,y1,y2,y3,y4,pages,qnog,rnog
    ncfile = "../MITgcm/verification/Yuta's_first_model/run_bdbv40days/results/state.nc"
    psfile = "../MITgcm/verification/Yuta's_first_model/run_bdbv40days/state.ps"
    initial_day = 0
    day_inc = 4                     
    number_of_graphs = 10           
    d1 = 1                          !depths (in grids) to plot in xy planes
    d2 = 2
    d3 = 3
    d4 = 4
    y1 = 5                        !y values (in grids) to plot in xz planes
    y2 = 10
    y3 = 40
    y4 = 80
    
    if(number_of_graphs > 10)then
        if(mod(number_of_graphs,10) == 0)then
            pages = number_of_graphs/10
        else
            pages = int(number_of_graphs/10) + 1
        end if
    else
        pages = 1
    end if
    print*,'pages per variable:',pages
    allocate(iterations(pages))
    if(pages == 1)then 
        iterations(1) = number_of_graphs
    else
        qnog = int(number_of_graphs/10)
        iterations(1:qnog) = 10
        if(mod(number_of_graphs,10) /= 0)then
            rnog = mod(number_of_graphs,10)
            iterations(qnog+1) = rnog
        end if
    end if
        
! data obtainment
    ! Open the NetCDF file
    status = nf90_open(trim(ncfile), nf90_nowrite, ncid)
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
    call plots2(psfile=psfile, oopt='otops', h='x,y plane, Temp and Sal')
    call plot(0.8,-height,-3)
    call plotsave('row1')
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     ! temp and sal xy plane, z = 1,3,6,11 (10m,50m,110m,210m)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    do n = 1, pages 
        if(n>1.and.n/=pages)call newpage(h = 'x,y plane, Temp and Sal, continues',x = 0.8,y = -height)
        if(n>1.and.n == pages)call newpage(h = 'x,y plane, Temp and Sal, ends',x = 0.8,y = -height)
        do i = 1,4
            if(n==1)call plotsave(int2str(i))
            if(i == 1)then;d = d1;call symbolr(-0.2,height/2.,0.6,int2str((d-1)*20+10)//'m')
            else if(i == 2)then;d = d2;call symbolr(-0.2,height/2.,0.6,int2str((d-1)*20+10)//'m')
            else if(i == 3)then;d = d3;call symbolr(-0.2,height/2.,0.6,int2str((d-1)*20+10)//'m')
            else if(i == 4)then;d = d4;call symbolr(-0.2,height/2.,0.6,int2str((d-1)*20+10)//'m')
            end if
            if(i==1)call symbolr(-0.1,height+0.5,0.6,'Grids;120*200;;d=')
            do j = 1, iterations(n)
                call memori(200,0.05,10,height,-90.,y = height/2.)
                call memori(120,0.05,10,width,0.,x = width/2.,y = 0.)
                if(i==1)call symbolc(width/2.,height+0.5,0.6,'day'//trim(int2str((j-1)*day_inc+initial_day)))
                call butler_psk(sal(:,:,d,(j-1)*day_inc+initial_day+1),width,height,0.,33.95,34.3,0.05,'b2w2r',7,bpt1=4,centralize = 4)
                call butler_cont(temp(:,:,d,(j-1)*day_inc+initial_day+1),width,height,0.,0.,1.,thicc = 5) 
                call plot(width+0.2,0.,-3)  
            end do
            call plotback(int2str(i))
            call plot(0.,-height-0.3,-3)
        end do
    end do

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     ! temp and sal xz plane, y = 10,30,50,70 (50km,150km,250km,350km)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call newpage(h = 'x,z plane, Temp and Sal',x = 0.8,y = 0.)

    do n = 1, pages
        if(n>1.and.n/=pages)call newpage(h = 'x,z plane, Temp and Sal, continues',x = 0.8,y = 0.)
        if(n>1.and.n == pages)call newpage(h = 'x,z plane, Temp and Sal, ends',x = 0.8,y = 0.)
        do i = 1,4
            if(i == 1)then;y = y1;call symbolr(-0.2,-height/2.,0.6,real2str((y-1)*5+2.5)//';km')
            else if(i == 2)then;y = y2;call symbolr(-0.2,-height/2.,0.6,real2str((y-1)*5+2.5)//';km')
            else if(i == 3)then;y = y3;call symbolr(-0.2,-height/2.,0.6,real2str((y-1)*5+2.5)//';km')
            else if(i == 4)then;y = y4;call symbolr(-0.2,-height/2.,0.6,real2str((y-1)*5+2.5)//';km')
            end if
            if(i==1)call symbolr(-0.1,0.5,0.6,'Grids;120*20;;y=')
            if(n==1)call plotsave(int2str(y))
            do j = 1, iterations(n)
                call memori(20,0.05,10,-height,-90.,y = -height/2.)
                call memori(120,0.05,10,width,0.,x = width/2.,y = -height)
                if(i==1)call symbolc(width/2.,0.5,0.6,'day'//trim(int2str((j-1)*day_inc+initial_day)))
                call butler_psk(sal(:,y,:,(j-1)*day_inc+initial_day+1),width,-height,0.,33.95,34.3,0.05,'b2w2r',7,bpt1=4,centralize = 4)
                call butler_cont(temp(:,y,:,(j-1)*day_inc+initial_day+1),width,-height,0.,0.,1.,thicc = 5) 
                call plot(width+0.2,0.,-3)  
            end do
            call plotback(int2str(y))
            call plot(0.,-height-0.3,-3)
        end do
    end do

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                            ! vvel xy plane, z = 1,3,6,11 (10m,50m,110m,210m)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call newpage(h = 'x,y plane, V, day0-90',x = 0.8,y = -height)
    do n = 1, pages
        if(n>1.and.n/=pages)call newpage(h = 'x,y plane, V, continues',x = 0.8,y = -height)
        if(n>1.and.n == pages)call newpage(h = 'x,y plane, V, ends',x = 0.8,y = -height)
        do i = 1,4
            if(i == 1)then;d = d1;call symbolr(-0.2,height/2.,0.6,int2str((d-1)*20+10)//'m')
            else if(i == 2)then;d = d2;call symbolr(-0.2,height/2.,0.6,int2str((d-1)*20+10)//'m')
            else if(i == 3)then;d = d3;call symbolr(-0.2,height/2.,0.6,int2str((d-1)*20+10)//'m')
            else if(i == 4)then;d = d4;call symbolr(-0.2,height/2.,0.6,int2str((d-1)*20+10)//'m')
            end if
            if(i==1)call symbolr(-0.1,height+0.5,0.6,'Grids;120*200;;d=')
            ! if(n==1)call plotsave(int2str(i))
            do j = 1, iterations(n)
                call memori(200,0.05,10,height,-90.,y = height/2.)
                call memori(120,0.05,10,width,0.,x = width/2.,y = 0.)
                if(i==1)call symbolc(width/2.,height+0.5,0.6,'day'//trim(int2str((j-1)*day_inc+initial_day)))
                call butler_psmask(V(:,:,d,(j-1)*day_inc+initial_day+1),width,height,-1.,0.,r = 0.4 ,g = 0.4 ,b = 0.4)
                call butler_cont(V(:,:,d,(j-1)*day_inc+initial_day+1),width,height,0.,-0.2,0.05,thicc = 2) 
                call plot(width+0.2,0.,-3)
            end do
            call plotback(int2str(i))
            call plot(0.,-height-0.3,-3)
        end do
    end do
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                        ! vvel xz plane, y = 10,30,50,70 (50km,150km,250km,350km)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call newpage(h = 'x,z plane, V, day0-90',x = 0.8,y = 0.)
    do n = 1, pages
        if(n>1.and.n/=pages)call newpage(h = 'x,z plane, V, continues',x = 0.8,y = 0.)
        if(n>1.and.n == pages)call newpage(h = 'x,z plane, V, ends',x = 0.8,y = 0.)
        do i = 1,4
            if(i == 1)then;y = y1;call symbolr(-0.2,-height/2.,0.6,real2str((y-1)*5+2.5)//';km')
            else if(i == 2)then;y = y2;call symbolr(-0.2,-height/2.,0.6,real2str((y-1)*5+2.5)//';km')
            else if(i == 3)then;y = y3;call symbolr(-0.2,-height/2.,0.6,real2str((y-1)*5+2.5)//';km')
            else if(i == 4)then;y = y4;call symbolr(-0.2,-height/2.,0.6,real2str((y-1)*5+2.5)//';km')
            end if
            if(i==1)call symbolr(-0.1,0.5,0.6,'Grids;120*20;;y=')
            do j = 1, iterations(n)
                call memori(20,0.05,10,-height,-90.,y = -height/2.)
                call memori(120,0.05,10,width,0.,x = width/2.,y = -height)
                if(i==1)call symbolc(width/2.,0.5,0.6,'day'//trim(int2str((j-1)*day_inc+initial_day)))
                call butler_psmask(V(:,y,:,(j-1)*day_inc+initial_day+1),width,-height,-1.,0.,r = 0.4 ,g = 0.4 ,b = 0.4)
                call butler_cont(V(:,y,:,(j-1)*day_inc+initial_day+1),width,-height,0.,-0.2,0.05,thicc = 2) 
                call plot(width+0.2,0.,-3)
            end do
            call plotback(int2str(y))
            call plot(0.,-height-0.3,-3)
        end do
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
    !     do j = 1, 4
    !         call memori(200,0.05,10,height,-90.,y = height/2.)
    !         call memori(120,0.05,10,width,0.,x = width/2.,y = 0.)
    !         if(i==1)call symbolc(width/2.,height+0.5,0.6,'day'//trim(int2str((j-1)*10)))
    !         call butler_psmask(U(:,:,d,(j-1)*10+1),width,height,-1.,0.,r = 0.4 ,g = 0.4 ,b = 0.4)
    !         call butler_cont(U(:,:,d,(j-1)*10+1),width,height,0.,-0.2,0.05,thicc = 2) 
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
    !     do j = 1, 4
    !         call memori(20,0.05,10,-height,-90.,y = -height/2.)
    !         call memori(120,0.05,10,width,0.,x = width/2.,y = -height)
    !         if(i==1)call symbolc(width/2.,0.5,0.6,'day'//trim(int2str((j-1)*10)))
    !         call butler_psmask(U(:,y,:,(j-1)*10+1),width,-height,-1.,0.,r = 0.4 ,g = 0.4 ,b = 0.4)
    !         call butler_cont(U(:,y,:,(j-1)*10+1),width,-height,0.,-0.2,0.05,thicc = 2) 
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