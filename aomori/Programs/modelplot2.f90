program plot_model ! for plotting domain with uneven depth layers
    use always
    ! made for Yuta's second model, which has 100x100 grids and 20 uneven depth layers
    ! dumpfreq is assumed to be 1 day
    character(len=256) :: ncfile,psfile,tinterval,psfile2,psfile2_title,tinterval2
    real,dimension(:,:,:,:),allocatable::U,V,W,temp,sal,UnotC
    real,dimension(:,:,:,:),allocatable::Uplot,Vplot,Wplot,Tplot,Splot ! uniform 10m layers just for plotting,100 layers
    real,dimension(:,:,:),allocatable::eta
    integer,dimension(:),allocatable::iterations
    real,dimension(:),allocatable::total_inflow,inflow_above400,outflow,delta_eta,r,g,b
    real,parameter::width = 2.5,height=4.
    logical::plotW,draw_2
    integer::initial_day,day_inc,number_of_graphs,d1,d2,d3,d4,y1,y2,y3,y4,pages,qnog,rnog,gridsx,gridsy,gridsz,gridsizex,gridsizey,gridsizez
    integer::initial_day2,day_inc2,number_of_graphs2,d5,y5


    ! ncfile = "../MITgcm/verification/yuta_second_model/run3_outhv/results/state.nc"
    ! psfile = "../MITgcm/verification/yuta_second_model/run3_outhv/results_run3_outhv.ps"
    ! psfile2 = "../MITgcm/verification/yuta_second_model/run3_outhv/results_run3_outhv2.ps"
    ncfile = "../MITgcm/verification/yuta_second_model/run3_outhv_noshelf/results/state.nc"
    psfile = "../MITgcm/verification/yuta_second_model/run3_outhv_noshelf/results_run3_outhv_noshelf.ps"
    psfile2 = "../MITgcm/verification/yuta_second_model/run3_outhv_noshelf/results_run3_outhv_noshelf2.ps"
    psfile2_title = 'no shelf'
    draw_2 = .true. ! to draw the 2nd part (inflow, outflow, delta eta...)


        gridsx = 100
        gridsy = 100
        gridsz = 20
        gridsizex = 10000
        gridsizey = 10000
        gridsizez = 20

    ! ps1 parameters 
        initial_day = 0
        day_inc = 5       
        ! tinterval = 'hour'
        tinterval = 'day' ! for 1 day interval
        inumber_of_recs = 101
        number_of_graphs = int((inumber_of_recs-initial_day)/day_inc)+1
        ! number_of_graphs = 20 ! 90 days
        plotW = .false.
        d1 = 1 !10m                         !depths (in grids) to plot in xy planes 10m intervals 
        d2 = 10 ! 100m
        d3 = 20 ! 200m
        d4 = 50 ! 500m
        y1 = 1 ! southern boundary                      !y values (in grids) to plot in xz planes
        y2 = 59 !590 strait
        y3 = 70 !700
        y4 = 100 !1000 boundary
    !ps2 paramters
        initial_day2 = 10
        day_inc2 = 10       
        tinterval2 = 'day' ! for 1 day interval
        number_of_graphs2 = int((inumber_of_recs-initial_day2)/day_inc2)+1 ! <=10
        d5 = 1 ! 10m ! surface 
        y5 = 70 ! 700m, strait
    
    ! calc for ps1
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
        print*,iterations
            
        call state2mat(ncfile,Uc=U,Vc=V,W=W,T=temp,S=sal,Eta=eta,info = .true.,U = UnotC)
        if((number_of_graphs-1)*day_inc+initial_day>size(temp,4))then 
            print*,'number of graphs exceeds the number of days in the model'
            stop
        end if
        allocate(Uplot(100,100,100,inumber_of_recs),Vplot(100,100,100,inumber_of_recs),Wplot(100,100,100,inumber_of_recs),Tplot(100,100,100,inumber_of_recs),Splot(100,100,100,inumber_of_recs)) 
    ! calc
    ! spread evenly to plot arrays
        do i = 1, 10 ! layers 1-10 20m each
            Uplot(:,:,2*(i-1)+1:2*i,:) = spread(U(:,:,i,:),3,2)
            Vplot(:,:,2*(i-1)+1:2*i,:) = spread(V(:,:,i,:),3,2)
            Wplot(:,:,2*(i-1)+1:2*i,:) = spread(W(:,:,i,:),3,2)
            Tplot(:,:,2*(i-1)+1:2*i,:) = spread(temp(:,:,i,:),3,2)
            Splot(:,:,2*(i-1)+1:2*i,:) = spread(sal(:,:,i,:),3,2)
            ! print*,i,2*(i-1)+1,2*i,'spread'
        end do

        do i = 11, 14 ! layers 11-14 50m each
            Uplot(:,:,5*(i-11)+21:5*(i-10)+20,:) = spread(U(:,:,i,:),3,5)
            Vplot(:,:,5*(i-11)+21:5*(i-10)+20,:) = spread(V(:,:,i,:),3,5)
            Wplot(:,:,5*(i-11)+21:5*(i-10)+20,:) = spread(W(:,:,i,:),3,5)
            Tplot(:,:,5*(i-11)+21:5*(i-10)+20,:) = spread(temp(:,:,i,:),3,5)
            Splot(:,:,5*(i-11)+21:5*(i-10)+20,:) = spread(sal(:,:,i,:),3,5)
            ! print*,i,5*(i-11)+21,5*(i-10)+20,'spread'
        end do

        do i = 15,20 ! layers 15-20 100m each
            Uplot(:,:,10*(i-15)+41:10*(i-14)+40,:) = spread(U(:,:,i,:),3,10)
            Vplot(:,:,10*(i-15)+41:10*(i-14)+40,:) = spread(V(:,:,i,:),3,10)
            Wplot(:,:,10*(i-15)+41:10*(i-14)+40,:) = spread(W(:,:,i,:),3,10)
            Tplot(:,:,10*(i-15)+41:10*(i-14)+40,:) = spread(temp(:,:,i,:),3,10)
            Splot(:,:,10*(i-15)+41:10*(i-14)+40,:) = spread(sal(:,:,i,:),3,10)
            ! print*,i,10*(i-15)+41,10*(i-14)+40,'spread'
        end do
    ! spreading evenly

    !debug
        ! print*,Vplot(55:70,50,:,25),'V at y = 50 cross section, 25th hour'
        ! print*,Vplot(55:70,50,:,25)*10E3*10E-6,'transport per grid'
        ! print*,sum(Vplot(55:70,50,:,25)*10E3*10.)*1.E-6,'transport at y = 50 cross section, 25th hour, in Sv'
        ! do i = 1, 25
        !     print*,i,'hour'
        !     print*,UnotC(1,57,1,i),UnotC(2,57,1,i),'<-W,E->',UnotC(99,57,1,i),UnotC(100,57,1,i),UnotC(101,57,1,i)
        !     print*,UnotC(1,57,2,i),UnotC(2,57,2,i),'<-W,E->',UnotC(99,57,2,i),UnotC(100,57,2,i),UnotC(101,57,2,i)
        !     print*,UnotC(1,57,3,i),UnotC(2,57,3,i),'<-W,E->',UnotC(99,57,3,i),UnotC(100,57,3,i),UnotC(101,57,3,i)
        !     print*,UnotC(1,57,4,i),UnotC(2,57,4,i),'<-W,E->',UnotC(99,57,4,i),UnotC(100,57,4,i),UnotC(101,57,4,i)
        !     print*,UnotC(1,57,5,i),UnotC(2,57,5,i),'<-W,E->',UnotC(99,57,5,i),UnotC(100,57,5,i),UnotC(101,57,5,i)
        ! end do
        ! do i = 1, 25
        !     print*,i,'hour, EAST'
        !     print*,UnotC(100,57:60,1,i),UnotC(101,57:60,1,i)
        !     print*,UnotC(100,57:60,2,i),UnotC(101,57:60,2,i)
        !     print*,UnotC(100,57:60,3,i),UnotC(101,57:60,3,i)
        !     print*,UnotC(100,57:60,4,i),UnotC(101,57:60,4,i)
        !     print*,UnotC(100,57:60,5,i),UnotC(101,57:60,5,i)
        ! end do
    !debug
    deallocate(U,V,W,temp,sal,UnotC)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! 1st part
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if(draw_2)goto 100

    call plots2(psfile=psfile, oopt='otops', h='x,y plane, Temp and Sal')
    call plot(0.8,-height,-3)
    call plotsave('row1')
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     ! temp and sal xy plane, z = 1,3,6,11
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        do n = 1, pages 
            if(n>1.and.n/=pages)call newpage(h = 'x,y plane, Temp and Sal, continues',x = 0.8,y = -height)
            if(n>1.and.n == pages)call newpage(h = 'x,y plane, Temp and Sal, ends',x = 0.8,y = -height)
            do i = 1,4
                if(n==1)call plotsave(int2str(i))
                if(i == 1)then;d = d1;call symbolr(-0.2,height/2.,0.6,int2str((d1-1)*10+5)//'m')
                else if(i == 2)then;d = d2;call symbolr(-0.2,height/2.,0.6,int2str((d2-1)*10+5)//'m')
                else if(i == 3)then;d = d3;call symbolr(-0.2,height/2.,0.6,int2str((d3-1)*10+5)//'m')
                else if(i == 4)then;d = d4;call symbolr(-0.2,height/2.,0.6,int2str((d4-1)*10+5)//'m')
                end if
                if(i==1)call symbolr(-0.1,height+0.5,0.6,'Grids;'//int2str(gridsx)//'*'//int2str(gridsy)//';;d=')
                do j = 1, iterations(n)
                    ! iplot = (n-1)*day_inc*(iterations(n))+initial_day + (j-1)*day_inc + 1 ! +1 is because day 0 is the first record
                    iplot = day_inc*(sum(iterations(1:n-1)))+initial_day + (j-1)*day_inc + 1
                    ! print*,iplot,n,i,j,'iplot'
                    call memori(gridsy+1,0.05,10,height,-90.,y = height/2.)
                    call memori(gridsx+1,0.05,10,width,0.,x = width/2.,y = 0.)
                    ! print*,n*(j-1)*day_inc+initial_day
                    if(i==1)call symbolc(width/2.,height+0.5,0.6,trim(tinterval)//trim(int2str(iplot-1)))
                    call butler_psk(Splot(:,:,d,iplot),width,height,0.,33.95,34.3,0.05,'b2w2r',7,bpt1=4,centralize = 4)
                    call butler_cont(Tplot(:,:,d,iplot),width,height,0.,0.,5.)! 5 degree interval 
                    call plot(width+0.2,0.,-3)  
                end do
                call plotback(int2str(i))
                call plot(0.,-height-0.3,-3)
            end do
        end do
        call plotomit
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     ! temp and sal xz plane, y = 10,30,50,70 
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
                    iplot = day_inc*(sum(iterations(1:n-1)))+initial_day + (j-1)*day_inc + 1
                    call memori(gridsz+1,0.05,10,-height,-90.,y = -height/2.)
                    call memori(gridsx+1,0.05,10,width,0.,x = width/2.,y = -height)
                    if(i==1)call symbolc(width/2.,0.5,0.6,trim(tinterval)//trim(int2str(iplot-1)))
                    call butler_psk(Splot(:,y,:,iplot),width,-height,0.,33.95,34.3,0.05,'b2w2r',7,bpt1=4,centralize = 4)
                    call butler_cont(Tplot(:,y,:,iplot),width,-height,0.,0.,5.)! 5 deg interval 
                    call plot(width+0.2,0.,-3)  
                end do
                call plotback(int2str(y))
                call plot(0.,-height-0.3,-3)
            end do
        end do
        call plotomit
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                            ! uvvel xy plane, z = 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        call newpage(h = 'x,y plane, V, day0-90',x = 0.8,y = -height)
        
        do n = 1, pages
            if(n>1.and.n/=pages)call newpage(h = 'x,y plane, V, continues',x = 0.8,y = -height)
            if(n>1.and.n == pages)call newpage(h = 'x,y plane, V, ends',x = 0.8,y = -height)
            do i = 1,4
                if(i == 1)then;d = d1;call symbolr(-0.2,height/2.,0.6,int2str((d1-1)*10+5)//'m')
                else if(i == 2)then;d = d2;call symbolr(-0.2,height/2.,0.6,int2str((d2-1)*10+5)//'m')
                else if(i == 3)then;d = d3;call symbolr(-0.2,height/2.,0.6,int2str((d3-1)*10+5)//'m')
                else if(i == 4)then;d = d4;call symbolr(-0.2,height/2.,0.6,int2str((d4-1)*10+5)//'m')
                end if
                if(i==1)call symbolr(-0.1,height+0.5,0.6,'Grids;'//int2str(gridsx)//'*'//int2str(gridsy)//';;d=')
                if(n==1)call plotsave(int2str(i))
                do j = 1, iterations(n)
                    if(n == 1.and.i == 1.and. j == 1)then 
                        call arrow(width/2.,height/2.,width/2.,height/2.+1.,0.2,3)
                    end if
                    iplot = day_inc*(sum(iterations(1:n-1)))+initial_day + (j-1)*day_inc + 1
                    call memori(gridsy+1,0.05,10,height,-90.,y = height/2.)
                    call memori(gridsx+1,0.05,10,width,0.,x = width/2.,y = 0.)
                    if(i==1)call symbolc(width/2.,height+0.5,0.6,trim(tinterval)//trim(int2str(iplot-1)))
                    ! call butler_psmask(V(:,:,d,(j-1)*day_inc+initial_day+1),width,height,-1.,0.,r = 0.4 ,g = 0.4 ,b = 0.4)
                    ! call butler_cont(V(:,:,d,(j-1)*day_inc+initial_day+1),width,height,0.,-0.2,0.02,thicc = 5,maskn = .true.) 
                    ! print*,(j-1)*day_inc+initial_day+1
                    ! print*,size(V(:,:,d,(j-1)*day_inc+initial_day+1))
                    call butler_vector(Uplot(:,:,d,iplot),Vplot(:,:,d,iplot),width,height,scalef = 1.,line_thickness = 2,thinfx = 3,thinfy = 4)
                    call plot(width+0.2,0.,-3)
                end do
                call plotback(int2str(i))
                call plot(0.,-height-0.3,-3)
            end do
        end do
        call plotomit
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                            ! Wvel xy plane, z = 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        if(plotW)then 
            call newpage(h = 'x,y plane, W, day0-90',x = 0.8,y = -height)
            
            do n = 1, pages
                if(n>1.and.n/=pages)call newpage(h = 'x,y plane, W, continues',x = 0.8,y = -height)
                if(n>1.and.n == pages)call newpage(h = 'x,y plane, W, ends',x = 0.8,y = -height)
                do i = 1,4
                    if(i == 1)then;d = d1;call symbolr(-0.2,height/2.,0.6,int2str((d1-1)*10+5)//'m')
                    else if(i == 2)then;d = d2;call symbolr(-0.2,height/2.,0.6,int2str((d2-1)*10+5)//'m')
                    else if(i == 3)then;d = d3;call symbolr(-0.2,height/2.,0.6,int2str((d3-1)*10+5)//'m')
                    else if(i == 4)then;d = d4;call symbolr(-0.2,height/2.,0.6,int2str((d4-1)*10+5)//'m')
                    end if
                    if(i==1)call symbolr(-0.1,height+0.5,0.6,'Grids;'//int2str(gridsx)//'*'//int2str(gridsy)//';;d=')
                    if(n==1)call plotsave(int2str(i))
                    do j = 1, iterations(n)
                        if(n == 1.and.i == 1.and. j == 1)then 
                            call arrow(width/2.,height/2.,width/2.,height/2.+1.,0.2,3)
                        end if
                        iplot = day_inc*(sum(iterations(1:n-1)))+initial_day + (j-1)*day_inc + 1
                        call memori(gridsy+1,0.05,10,height,-90.,y = height/2.)
                        call memori(gridsx+1,0.05,10,width,0.,x = width/2.,y = 0.)
                        if(i==1)call symbolc(width/2.,height+0.5,0.6,trim(tinterval)//trim(int2str(iplot-1)))
                        call butler_cont(Wplot(:,:,d,iplot),width,height,-999.,-1.,0.01,thicc = 5,maskn = .true.) ! 1cm interval
                        call plot(width+0.2,0.,-3)
                    end do
                    call plotback(int2str(i))
                    call plot(0.,-height-0.3,-3)
                end do
            end do
            call plotomit
        end if
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                            ! uvvel xz plane, y = 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        call newpage(h = 'x,z plane, V',x = 0.8,y = 0.)
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
                    iplot = day_inc*(sum(iterations(1:n-1)))+initial_day + (j-1)*day_inc + 1
                    call memori(gridsz+1,0.05,10,-height,-90.,y = -height/2.)
                    call memori(gridsx+1,0.05,10,width,0.,x = width/2.,y = -height)
                    if(i==1)call symbolc(width/2.,0.5,0.6,trim(tinterval)//trim(int2str(iplot-1)))
                    call butler_psmask(Vplot(:,y,:,iplot),width,-height,-1.,0.,r = 0.4 ,g = 0.4 ,b = 0.4)
                    call butler_cont(Vplot(:,y,:,iplot),width,-height,0.,-1.,0.1)!10cm interval 
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
        call plotsave('eta')
        do i = 1,number_of_graphs

                iplot = (i-1)*day_inc + initial_day + 1
                call memori(gridsy+1,0.05,10,height,-90.,y = height/2.)
                call memori(gridsx+1,0.05,10,width,0.,x = width/2.,y = 0.)
                call symbolc(width/2.,height+0.1,0.4,trim(tinterval)//trim(int2str(iplot-1)))
                ! call butler_psk(eta(:,:,(j-1)*day_inc+initial_day+1),width,height,-1.,-0.5,0.5,'b2w2r',7,bpt1=4,centralize = 4)
                call butler_cont(eta(:,:,iplot),width,height,0.,-10.,0.1,maskn = .true.) !10cm interval
            if(mod(i,10) == 0)then 
                call plotback('eta')
                call plot(0.,i/10*(-height-0.5),-3)
            else
                call plot(width+0.2,0.,-3)
            end if

        end do
        call plotomit

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! 2nd part
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

100     allocate(total_inflow(inumber_of_recs),inflow_above400(inumber_of_recs),outflow(inumber_of_recs),delta_eta(inumber_of_recs))
        do i = 1, 101 ! inflow
            total_inflow(i) = sum(Vplot(:70,5,:,i) * 10.*10.**(3.) * 10.)* 10.**(-6) ! total inflow at y = 50km, in Sv
            inflow_above400(i) = sum(Vplot(:70,5,:40,i) * 10.*10.**(3.) * 10.)* 10.**(-6) ! inflow above 400m at y = 50km, in Sv
        end do
        do i = 1, 101 ! outflow
            outflow(i) = sum(Uplot(99,57:60,:,i) * 10.*10.**(3.) * 10.)* 10.**(-6) ! outflow at eastern end of strait, in Sv
        end do
        do i = 1, 101 ! delta eta
            delta_eta(i) = (eta(73,57,i) - eta(73,60,i)) * 100. ! eta difference across the strait, in cm
        end do

        height2 = 5.;width2 = 15.
        call plots2(psfile = psfile2,oopt = 'otops', x = 1., y = -height2,mode = 'portrait',h = psfile2_title)
        !inflow
        call butler_linegraph(total_inflow,width2,height2,-4.,4.,0.,.true.,memsymfreq = 2,tlabel = 'inflow at y = 50km cross section')
        call butler_linegraph(inflow_above400,width2,height2,-4.,4.,0.,.false.,gl = 1.)
        call butler_linegraph(total_inflow-inflow_above400,width2,height2,-4.,4.,0.,.false.,bl = 1.)
        call symbol(width2+0.2,height2*2./3.,0.5,'gr = < 400m;blu = > 400m;black = total')
        call num_memori(0.,100.,101,10,0.6,-1,width2,gap = 2)
        !outflow
        call plot(0.,-height-3.,-3)
        call butler_linegraph(outflow,width2,height2,-2.,2.,0.,.true.,memsymfreq = 1,tlabel = 'outflow at east of strait')
        call num_memori(0.,100.,101,10,0.6,-1,width2,gap = 2)
        ! delta eta
        call plot(0.,-height2-3.,-3)
        call butler_linegraph(eta(73,57,:)*100.,width2,height2,0.,40.,0.,.false.,gl = 1.)
        call butler_linegraph(eta(73,60,:)*100.,width2,height2,0.,40.,0.,.false.,bl = 1.)
        call butler_linegraph(delta_eta,width2,height2,0.,40.,0.,.true.,memsymfreq = 5,tlabel = 'eta difference across the strait')
        call symbol(width2+0.2,height2*2./3.,0.5,'gr = south;blu = north')
        call num_memori(0.,100.,101,10,0.6,-1,width2,gap = 2)

        call newpage(mode = 'landscape')
        call otops(x = 0., y = -height+1.5)
        call plotsave('draw2')
        call header('Eta, 10cm interval',symbol_size = 0.6,y = 0.5)
        do i = 1,number_of_graphs2
            iplot = (i-1)*day_inc2 + initial_day2 + 1
            call memori(gridsy+1,0.05,10,height,-90.,y = height/2.)
            call memori(gridsx+1,0.05,10,width,0.,x = width/2.,y = 0.)
            call symbolc(width/2.,height+0.1,0.4,trim(tinterval2)//trim(int2str(iplot-1)))
            call butler_cont(eta(:,:,iplot),width,height,0.,-10.,0.1,maskn = .true.) !10cm interval
            if(i==1)then;call rgbk(1.,0.,0.);call plot(0.,height*0.7,3);call plot(width,height*0.7,2);call rgbk(0.,0.,0.);endif
            call plot(width+0.2,0.,-3)
        end do

        call plotback('draw2');call plot(0.,-height-1.0,-3)
        call header('velocity vectors',symbol_size = 0.6,y = -0.7-height)
        do i = 1,number_of_graphs2
            iplot = (i-1)*day_inc2 + initial_day2 + 1
                call memori(gridsy+1,0.05,10,height,-90.,y = height/2.)
                call memori(gridsx+1,0.05,10,width,0.,x = width/2.,y = 0.)
                call butler_vector(Uplot(:,:,d5,iplot),Vplot(:,:,d5,iplot),width,height,scalef = 2.,line_thickness = 3,thinfx = 6,thinfy = 4)
                call plot(width+0.2,0.,-3)
        end do
        call arrow(0.5,0.,0.5,.2);call symbolc(0.,0.4,0.4,'10cm/s')

        call plotback('draw2');call plot(0.,-height-1.0 -1.0,-3)
        call header('velocity profiles 100km north of strait,5cm int',symbol_size = 0.6,y = -2*height-1.7)

        do i = 1, number_of_graphs2
            iplot = (i-1)*day_inc2 + initial_day2 + 1
                call memori(gridsz+1,0.05,10,-height,-90.,y = -height/2.)
                call memori(gridsx+1,0.05,10,width,0.,x = width/2.,y = -height)
                call butler_psmask(Vplot(:,y5,:,iplot),width,-height,-1.,0.,r = 0.4 ,g = 0.4 ,b = 0.4)
                call butler_cont(Vplot(:,y5,:,iplot),width,-height,0.,-1.,0.05)!5cm interval 
                call plot(width+0.2,0.,-3)
        end do

        call plotback('draw2');call plot(0.,-2*height-1.0 -1.0 - 1.0,-3)
        call header('TS profiles 100km north of strait',symbol_size = 0.6,y = -3*height-2.7)

        do i = 1, number_of_graphs2
            iplot = (i-1)*day_inc2 + initial_day2 + 1
                call memori(gridsz+1,0.05,10,-height,-90.,y = -height/2.)
                call memori(gridsx+1,0.05,10,width,0.,x = width/2.,y = -height)
                call butler_psk(Splot(:,y5,:,iplot),width,-height,0.,33.95,34.3,0.05,'b2w2r',7,bpt1=4,centralize = 4,r = r, g = g, b = b)
                call butler_cont(Tplot(:,y5,:,iplot),width,-height,0.,0.,5.)! 5 deg interval 
                call plot(width+0.2,0.,-3)  
        end do
        call colorscale(r,g,b,33.95,34.3,2,0.3,1,height,0.2,lt = 1, gt = 1,rangle = 90.,symbol_start = 2,y = -height/2.)

        call newpage

        call plotback('draw2');call plot(0.,height-0.5,-3)
        call header('Cross Sectional View of Strait, TS, 5cm int')

        do i = 1, number_of_graphs2
            iplot = (i-1)*day_inc2 + initial_day2 + 1
                call memori(gridsz+1,0.05,10,-height,-90.,y = -height/2.)
                ! call memori(gridsx+1,0.05,10,width,0.,x = width/2.,y = -height)
                call symbolc(width/2.,0.1,0.4,trim(tinterval2)//trim(int2str(iplot-1)))
                call butler_psk(Splot(73,60:57:-1,:10,iplot),width,-height,0.,33.95,34.3,0.05,'b2w2r',7,bpt1=4,centralize = 4,r = r, g = g, b = b)
                call butler_cont(Tplot(73,60:57:-1,:10,iplot),width,-height,0.,0.,5.)! 5 deg interval 
                call plot(width+0.2,0.,-3)  
        end do

        call plotback('draw2');call plot(0.,-2.,-3)
        call header('Cross Sectional View of Strait, U, 5cm int',y = -height-1.5)

        do i = 1, number_of_graphs2
            iplot = (i-1)*day_inc2 + initial_day2 + 1
                call memori(gridsz+1,0.05,10,-height,-90.,y = -height/2.)
                ! call memori(gridsx+1,0.05,10,width,0.,x = width/2.,y = -height)
                call butler_psmask(Uplot(73,60:57:-1,:10,iplot),width,-height,-1.,0.,r = 0.4 ,g = 0.4 ,b = 0.4)
                call butler_psmask(Uplot(73,60:57:-1,:10,iplot),width,-height,0.,0.2,r = 1. ,g = 0.8 ,b = 0.8)
                call butler_psmask(Uplot(73,60:57:-1,:10,iplot),width,-height,0.2,0.4,r = 1. ,g = 0.4 ,b = 0.4)
                call butler_psmask(Uplot(73,60:57:-1,:10,iplot),width,-height,0.4,0.6,r = 1. ,g = 0. ,b = 0.)
                call butler_psmask(Uplot(73,60:57:-1,:10,iplot),width,-height,0.6,0.8,r = 0.8 ,g = 0. ,b = 0.)
                call butler_cont(Uplot(73,60:57:-1,:10,iplot),width,-height,0.,-1.,0.05)!5cm interval 
                call plot(width+0.2,0.,-3)
        end do

        call plote  

        
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


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