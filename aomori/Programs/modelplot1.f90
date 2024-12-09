program plot_model
    use always
    implicit none
    character(len=256) :: ncfile,psfile
    real,dimension(:,:,:,:),allocatable::U,V,W,temp,sal
    real,dimension(:,:,:),allocatable::eta
    integer,dimension(:),allocatable::iterations
    real,parameter::width = 2.5,height=4.
    integer::initial_day,day_inc,number_of_graphs,d1,d2,d3,d4,y1,y2,y3,y4,pages,qnog,rnog,gridsx,gridsy,gridsz,gridsizex,gridsizey,gridsizez
    ncfile = "../MITgcm/verification/Yuta's_first_model/run_bdlvr/lv8x/results/state.nc"
    psfile = "../MITgcm/verification/Yuta's_first_model/run_bdlvr/lv8x100days.ps"
    gridsx = 60
    gridsy = 100
    gridsz = 20
    gridsizex = 10000
    gridsizey = 10000
    gridsizez = 20
    initial_day = 0
    day_inc = 10           
    number_of_graphs = 10           
    d1 = 1                          !depths (in grids) to plot in xy planes
    d2 = 2
    d3 = 3
    d4 = 4
    y1 = 5                       !y values (in grids) to plot in xz planes
    y2 = 10
    y3 = 40
    y4 = 60
    
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
        qnog = 1
    else
        qnog = int(number_of_graphs/10)
        iterations(1:qnog) = 10
        if(mod(number_of_graphs,10) /= 0)then
            rnog = mod(number_of_graphs,10)
            iterations(qnog+1) = rnog
        end if
    end if
        
    call state2mat(ncfile,Uc=U,Vc=V,T=temp,S=sal,Eta=eta,info = .true.)
    if((number_of_graphs-1)*day_inc+initial_day>size(temp,4))then 
        print*,'number of graphs exceeds the number of days in the model'
        stop
    end if

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
            if(i == 1)then;d = d1;call symbolr(-0.2,height/2.,0.6,int2str((d-1)*(gridsizez)+gridsizez/2)//'m')
            else if(i == 2)then;d = d2;call symbolr(-0.2,height/2.,0.6,int2str((d-1)*(gridsizez)+gridsizez/2)//'m')
            else if(i == 3)then;d = d3;call symbolr(-0.2,height/2.,0.6,int2str((d-1)*(gridsizez)+gridsizez/2)//'m')
            else if(i == 4)then;d = d4;call symbolr(-0.2,height/2.,0.6,int2str((d-1)*(gridsizez)+gridsizez/2)//'m')
            end if
            if(i==1)call symbolr(-0.1,height+0.5,0.6,'Grids;'//int2str(gridsx)//'*'//int2str(gridsy)//';;d=')
            do j = 1, iterations(n)
                call memori(gridsy,0.05,10,height,-90.,y = height/2.)
                call memori(gridsx,0.05,10,width,0.,x = width/2.,y = 0.)
                if(i==1)call symbolc(width/2.,height+0.5,0.6,'day'//trim(int2str((j-1)*day_inc+initial_day)))
                call butler_psk(sal(:,:,d,(j-1)*day_inc+initial_day+1),width,height,0.,33.95,34.3,0.05,'b2w2r',7,bpt1=4,centralize = 4)
                call butler_cont(temp(:,:,d,(j-1)*day_inc+initial_day+1),width,height,0.,0.,1.,thicc = 5) 
                call plot(width+0.2,0.,-3)  
            end do
            call plotback(int2str(i))
            call plot(0.,-height-0.3,-3)
        end do
    end do
    call plotomit
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     ! temp and sal xz plane, y = 10,30,50,70 (50km,150km,250km,350km)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call newpage(h = 'x,z plane, Temp and Sal',x = 0.8,y = 0.)

    do n = 1, pages
        if(n>1.and.n/=pages)call newpage(h = 'x,z plane, Temp and Sal, continues',x = 0.8,y = 0.)
        if(n>1.and.n == pages)call newpage(h = 'x,z plane, Temp and Sal, ends',x = 0.8,y = 0.)
        do i = 1,4
            if(i == 1)then;y = y1;call symbolr(-0.2,-height/2.,0.6,real2str(((y-1)*gridsizey+gridsizey/2.)/1000.)//';km')
            else if(i == 2)then;y = y2;call symbolr(-0.2,-height/2.,0.6,real2str(((y-1)*gridsizey+gridsizey/2.)/1000.)//';km')
            else if(i == 3)then;y = y3;call symbolr(-0.2,-height/2.,0.6,real2str(((y-1)*gridsizey+gridsizey/2.)/1000.)//';km')
            else if(i == 4)then;y = y4;call symbolr(-0.2,-height/2.,0.6,real2str(((y-1)*gridsizey+gridsizey/2.)/1000.)//';km')
            end if
            if(i == 1)call symbolr(-0.1,0.5,0.6,'Grids;'//int2str(gridsx)//'*'//int2str(gridsz)//';;y=')    
            if(n==1)call plotsave(int2str(y))
            do j = 1, iterations(n)
                call memori(gridsz,0.05,10,-height,-90.,y = -height/2.)
                call memori(gridsx,0.05,10,width,0.,x = width/2.,y = -height)
                if(i==1)call symbolc(width/2.,0.5,0.6,'day'//trim(int2str((j-1)*day_inc+initial_day)))
                call butler_psk(sal(:,y,:,(j-1)*day_inc+initial_day+1),width,-height,0.,33.95,34.3,0.05,'b2w2r',7,bpt1=4,centralize = 4)
                call butler_cont(temp(:,y,:,(j-1)*day_inc+initial_day+1),width,-height,0.,0.,1.,thicc = 5) 
                call plot(width+0.2,0.,-3)  
            end do
            call plotback(int2str(y))
            call plot(0.,-height-0.3,-3)
        end do
    end do
    call plotomit
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                            ! vvel xy plane, z = 1,3,6,11 (10m,50m,110m,210m)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call newpage(h = 'x,y plane, V, day0-90',x = 0.8,y = -height)
    do n = 1, pages
        if(n>1.and.n/=pages)call newpage(h = 'x,y plane, V, continues',x = 0.8,y = -height)
        if(n>1.and.n == pages)call newpage(h = 'x,y plane, V, ends',x = 0.8,y = -height)
        do i = 1,4
            if(i == 1)then;d = d1;call symbolr(-0.2,height/2.,0.6,int2str((d-1)*(gridsizez)+gridsizez/2)//'m')
            else if(i == 2)then;d = d2;call symbolr(-0.2,height/2.,0.6,int2str((d-1)*(gridsizez)+gridsizez/2)//'m')
            else if(i == 3)then;d = d3;call symbolr(-0.2,height/2.,0.6,int2str((d-1)*(gridsizez)+gridsizez/2)//'m')
            else if(i == 4)then;d = d4;call symbolr(-0.2,height/2.,0.6,int2str((d-1)*(gridsizez)+gridsizez/2)//'m')
            end if
            if(i==1)call symbolr(-0.1,height+0.5,0.6,'Grids;'//int2str(gridsx)//'*'//int2str(gridsy)//';;d=')
            if(n==1)call plotsave(int2str(i))
            do j = 1, iterations(n)
                call memori(gridsy,0.05,10,height,-90.,y = height/2.)
                call memori(gridsx,0.05,10,width,0.,x = width/2.,y = 0.)
                if(i==1)call symbolc(width/2.,height+0.5,0.6,'day'//trim(int2str((j-1)*day_inc+initial_day)))
                ! call butler_psmask(V(:,:,d,(j-1)*day_inc+initial_day+1),width,height,-1.,0.,r = 0.4 ,g = 0.4 ,b = 0.4)
                ! call butler_cont(V(:,:,d,(j-1)*day_inc+initial_day+1),width,height,0.,-0.2,0.02,thicc = 5,maskn = .true.) 
                ! print*,(j-1)*day_inc+initial_day+1
                ! print*,size(V(:,:,d,(j-1)*day_inc+initial_day+1))
                call butler_vector(U(:,:,d,(j-1)*day_inc+initial_day+1),V(:,:,d,(j-1)*day_inc+initial_day+1),width,height,scalef = 1.,line_thickness = 2,thinfx = 3,thinfy = 4)
                call plot(width+0.2,0.,-3)
            end do
            call plotback(int2str(i))
            call plot(0.,-height-0.3,-3)
        end do
    end do
    call plotomit
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                        ! vvel xz plane, y = 10,30,50,70 (50km,150km,250km,350km)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call newpage(h = 'x,z plane, V, day0-90',x = 0.8,y = 0.)
    do n = 1, pages
        if(n>1.and.n/=pages)call newpage(h = 'x,z plane, V, continues',x = 0.8,y = 0.)
        if(n>1.and.n == pages)call newpage(h = 'x,z plane, V, ends',x = 0.8,y = 0.)
        do i = 1,4
            if(i == 1)then;y = y1;call symbolr(-0.2,-height/2.,0.6,real2str(((y-1)*gridsizey+gridsizey/2.)/1000.)//';km')
            else if(i == 2)then;y = y2;call symbolr(-0.2,-height/2.,0.6,real2str(((y-1)*gridsizey+gridsizey/2.)/1000.)//';km')
            else if(i == 3)then;y = y3;call symbolr(-0.2,-height/2.,0.6,real2str(((y-1)*gridsizey+gridsizey/2.)/1000.)//';km')
            else if(i == 4)then;y = y4;call symbolr(-0.2,-height/2.,0.6,real2str(((y-1)*gridsizey+gridsizey/2.)/1000.)//';km')
            end if
            if(i == 1)call symbolr(-0.1,0.5,0.6,'Grids;'//int2str(gridsx)//'*'//int2str(gridsz)//';;y=')    
            if(n==1)call plotsave(int2str(y))
            do j = 1, iterations(n)
                call memori(gridsz,0.05,10,-height,-90.,y = -height/2.)
                call memori(gridsx,0.05,10,width,0.,x = width/2.,y = -height)
                if(i==1)call symbolc(width/2.,0.5,0.6,'day'//trim(int2str((j-1)*day_inc+initial_day)))
                call butler_psmask(V(:,y,:,(j-1)*day_inc+initial_day+1),width,-height,-1.,0.,r = 0.4 ,g = 0.4 ,b = 0.4)
                call butler_cont(V(:,y,:,(j-1)*day_inc+initial_day+1),width,-height,0.,-0.2,0.02,thicc = 5) 
                call plot(width+0.2,0.,-3)
            end do
            call plotback(int2str(y))
            call plot(0.,-height-0.3,-3)
        end do
    end do
    call plotomit
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                         ! Eta xy plane
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call newpage(h = 'x,y plane, Eta',x = 0.8,y = -height)

    do i = 1,qnog
        call plotsave(int2str(i))
        do j = 1, iterations(i)
            call memori(gridsy,0.05,10,height,-90.,y = height/2.)
            call memori(gridsx,0.05,10,width,0.,x = width/2.,y = 0.)
            if(i==1)call symbolc(width/2.,height+0.1,0.4,'day'//trim(int2str((j-1)*day_inc+initial_day)))
            ! call butler_psk(eta(:,:,(j-1)*day_inc+initial_day+1),width,height,-1.,-0.5,0.5,'b2w2r',7,bpt1=4,centralize = 4)
            call butler_cont(eta(:,:,(j-1)*day_inc+initial_day+1),width,height,0.,-1.,0.02,thicc = 5,maskn = .true.) 
            call plot(width+0.2,0.,-3)  
        end do
        call plotback(int2str(i))
        call plot(0.,-height-0.3,-3)
    end do
    call plotomit
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                         ! Eta xz plane
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! call newpage(h = 'x,z plane, Eta',x = 0.8,y = 0.)
    ! do n = 1, pages
    !     if(n>1.and.n/=pages)call newpage(h = 'x,z plane, Eta, continues',x = 0.8,y = 0.)
    !     if(n>1.and.n == pages)call newpage(h = 'x,z plane, Eta, ends',x = 0.8,y = 0.)
    !     do i = 1,4
    !         if(i == 1)then;y = y1;call symbolr(-0.2,-height/2.,0.6,real2str((y-1)*5+2.5)//';km')
    !         else if(i == 2)then;y = y2;call symbolr(-0.2,-height/2.,0.6,real2str((y-1)*5+2.5)//';km')
    !         else if(i == 3)then;y = y3;call symbolr(-0.2,-height/2.,0.6,real2str((y-1)*5+2.5)//';km')
    !         else if(i == 4)then;y = y4;call symbolr(-0.2,-height/2.,0.6,real2str((y-1)*5+2.5)//';km')
    !         end if
    !         if(i==1)call symbolr(-0.1,0.5,0.6,'Grids;120*20;;y=')
    !         if(n==1)call plotsave(int2str(y))
    !         do j = 1, iterations(n)
    !             call memori(20,0.05,10,-height,-90.,y = -height/2.)
    !             call memori(120,0.05,10,width,0.,x = width/2.,y = -height)
    !             if(i==1)call symbolc(width/2.,0.5,0.6,'day'//trim(int2str((j-1)*day_inc+initial_day)))
    !             call butler_cont(eta(:,:,(j-1)*day_inc+initial_day+1),width,-height,0.,-1.,0.02,thicc = 5,maskn = .true.) 
    !             call plot(width+0.2,0.,-3)
    !         end do
    !         call plotback(int2str(y))
    !         call plot(0.,-height-0.3,-3)
    !     end do
    ! end do

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