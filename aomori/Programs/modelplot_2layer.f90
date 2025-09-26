program plot_model ! for plotting domain with uneven depth layers
    use always
    ! made for Yuta's 2 layer model, which has 200x200 grids and 32 uneven depth layers
    !delx = dely = 4km
    character(len=256) :: ncfile,psfile,tunit,psfile2,psfile2_title,tunit2
    real,dimension(:,:,:,:),allocatable::U,V,W,temp,sal,UnotC
    real,dimension(:,:,:,:),allocatable::Uplot,Vplot,Wplot,Tplot,Splot,Denplot! uniform 10m layers just for plotting,95 layers
    real,dimension(:,:,:),allocatable::eta
    real,dimension(:,:),allocatable::Denplotslice
    integer,dimension(:),allocatable::iterations
    real,dimension(:),allocatable::total_inflow,inflow_above400,outflow,delta_eta,r,g,b
    real,parameter::width = 2.5,height=4.
    logical::plotW,draw_2,draw_1
    integer::initial_state,state_int,number_of_graphs,d1,d2,d3,d4,y1,y2,y3,y4,pages,qnog,rnog,gridsx,gridsy,gridsz1,gridsz2,gridsizex,gridsizey,gridsizez1,gridsizez2
    integer::initial_state2,state_int2,number_of_graphs2,d5,y5,plotgridsizez,plotgridsz,final_state,final_state2,gridsizez3,gridsz3,begin,end,copies,gridsz4,gridsizez4,gridsz5,gridsizez5


    ! ncfile = "../MITgcm/verification/yuta_second_model/run3_outhv/results/state.nc"
    ! psfile = "../MITgcm/verification/yuta_second_model/run3_outhv/results_run3_outhv.ps"
    ! psfile2 = "../MITgcm/verification/yuta_second_model/run3_outhv/results_run3_outhv2.ps"
    ncfile = "../MITgcm/verification/yuta_2layer/test_i2b2/20days/state.nc"
    psfile = "../MITgcm/verification/yuta_2layer/test_i2b2/20days/results.ps"
    psfile2 = "../MITgcm/verification/yuta_2layer/test_i2b2/20days/results2.ps"
    psfile2_title = 'no shelf'
    draw_1 = .true.
    draw_2 = .true. ! to draw the 2nd part (inflow, outflow, delta eta...)
        call state2mat(ncfile,Uc=U,Vc=V,W=W,T=temp,S=sal,Eta=eta,info = .true.,U = UnotC)
    ! model parameters
        gridsx = 200
        gridsizex = 4000 !m
        gridsy = 200
        gridsizey = 4000 !m
        gridsz1 = 26
        gridsizez1 = 10 !m
        gridsz2 = 2
        gridsizez2 = 20 !m
        gridsz3 = 2
        gridsizez3 = 50 !m
        gridsz4 = 2
        gridsizez4 = 100 !m
        gridsz5 = 2
        gridsizez5 = 200 !m
        plotgridsizez = 10 ! 10m evened out layers for plotting
        plotgridsz = int((gridsz1*gridsizez1 + gridsz2 * gridsizez2 + gridsz3 * gridsizez3 + gridsz4 * gridsizez4 + gridsz5 * gridsizez5)/plotgridsizez) ! total number of layers after even spreading

    ! ps1 parameters 
        initial_state = 1 ! first time snap shot to plot
        state_int = 1       
        tunit = ' days'
        final_state = size(U,4) ! final time snap shot to plot
        number_of_graphs = int((final_state-initial_state)/state_int) 
        plotW = .false.
        d1 = 10 !10m                         ! depths plot in xy planes 10m intervals 
        d2 = 100 ! 100m
        d3 = 200 ! 200m
        d4 = 500 ! 500m
        y1 = 2 ! 8km southern boundary                      !y values (in grids) to plot in xz planes
        y2 = 116 !460 south strait
        y3 = 125 !500
        y4 = 198 !992 boundary
    !ps2 paramters
        initial_state2 = 1
        state_int2 = 2  
        tunit2 = ' days' 
        final_state2 = size(U,4)
        number_of_graphs2 = int((final_state2-initial_state2)/state_int2) ! <=10
        d5 = 10 ! 10m ! surface 
        y5 = 150 ! 600km, 100km north of strait
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
            
    ! calc
    ! spread evenly to plot arrays
        allocate(Uplot(gridsx,gridsy,plotgridsz,int(final_state-initial_state+1)))
        allocate(Vplot(gridsx,gridsy,plotgridsz,int(final_state-initial_state+1)))
        allocate(Wplot(gridsx,gridsy,plotgridsz,int(final_state-initial_state+1)))
        allocate(Tplot(gridsx,gridsy,plotgridsz,int(final_state-initial_state+1)))
        allocate(Splot(gridsx,gridsy,plotgridsz,int(final_state-initial_state+1)))
        ! print*,int((gridsz1*gridsizez1 + gridsz2 * gridsizez2)/plotgridsizez),int(final_state-initial_state+1)
        do i = 1, gridsz1 ! 1st layer
            begin = gridsizez1/plotgridsizez*(i-1)+1
            end = gridsizez1/plotgridsizez*i
            copies = gridsizez1/plotgridsizez
            print*,'layer1:',i,begin,end,copies
            Uplot(:,:,begin:end,:) = spread(U(:,:,i,:),3,copies)
            Vplot(:,:,begin:end,:) = spread(V(:,:,i,:),3,copies)
            Wplot(:,:,begin:end,:) = spread(W(:,:,i,:),3,copies)
            Tplot(:,:,begin:end,:) = spread(temp(:,:,i,:),3,copies)
            Splot(:,:,begin:end,:) = spread(sal(:,:,i,:),3,copies)
        end do
        do i = 1, gridsz2 ! 2nd layer
            begin = end + 1
            end = begin + gridsizez2/plotgridsizez - 1
            copies = gridsizez2/plotgridsizez
            print*,'layer2:',i,begin,end,copies
            Uplot(:,:,begin:end,:) = spread(U(:,:,i+gridsz1,:),3,copies)
            Vplot(:,:,begin:end,:) = spread(V(:,:,i+gridsz1,:),3,copies)
            Wplot(:,:,begin:end,:) = spread(W(:,:,i+gridsz1,:),3,copies)
            Tplot(:,:,begin:end,:) = spread(temp(:,:,i+gridsz1,:),3,copies)
            Splot(:,:,begin:end,:) = spread(sal(:,:,i+gridsz1,:),3,copies)
        end do
        do i = 1, gridsz3
            begin = end + 1
            end = begin + gridsizez3/plotgridsizez - 1
            copies = gridsizez3/plotgridsizez
            print*,'layer3:',i,begin,end,copies
            Uplot(:,:,begin:end,:) = spread(U(:,:,i+gridsz1+gridsz2,:),3,copies)
            Vplot(:,:,begin:end,:) = spread(V(:,:,i+gridsz1+gridsz2,:),3,copies)
            Wplot(:,:,begin:end,:) = spread(W(:,:,i+gridsz1+gridsz2,:),3,copies)
            Tplot(:,:,begin:end,:) = spread(temp(:,:,i+gridsz1+gridsz2,:),3,copies)
            Splot(:,:,begin:end,:) = spread(sal(:,:,i+gridsz1+gridsz2,:),3,copies)
        end do
        do i = 1, gridsz4
            begin = end + 1
            end = begin + gridsizez4/plotgridsizez - 1
            copies = gridsizez4/plotgridsizez
            print*,'layer4:',i,begin,end,copies
            Uplot(:,:,begin:end,:) = spread(U(:,:,i+gridsz1+gridsz2+gridsz3,:),3,copies)
            Vplot(:,:,begin:end,:) = spread(V(:,:,i+gridsz1+gridsz2+gridsz3,:),3,copies)
            Wplot(:,:,begin:end,:) = spread(W(:,:,i+gridsz1+gridsz2+gridsz3,:),3,copies)
            Tplot(:,:,begin:end,:) = spread(temp(:,:,i+gridsz1+gridsz2+gridsz3,:),3,copies)
            Splot(:,:,begin:end,:) = spread(sal(:,:,i+gridsz1+gridsz2+gridsz3,:),3,copies)
        end do
        do i = 1, gridsz5
            begin = end + 1
            end = begin + gridsizez5/plotgridsizez - 1
            copies = gridsizez5/plotgridsizez
            print*,'layer5:',i,begin,end,copies
            Uplot(:,:,begin:end,:) = spread(U(:,:,i+gridsz1+gridsz2+gridsz3+gridsz4,:),3,copies)
            Vplot(:,:,begin:end,:) = spread(V(:,:,i+gridsz1+gridsz2+gridsz3+gridsz4,:),3,copies)
            Wplot(:,:,begin:end,:) = spread(W(:,:,i+gridsz1+gridsz2+gridsz3+gridsz4,:),3,copies)
            Tplot(:,:,begin:end,:) = spread(temp(:,:,i+gridsz1+gridsz2+gridsz3+gridsz4,:),3,copies)
            Splot(:,:,begin:end,:) = spread(sal(:,:,i+gridsz1+gridsz2+gridsz3+gridsz4,:),3,copies)
        end do

        ! allocate(Denplot(size(Tplot,1),size(Tplot,2),size(Tplot,3),size(Tplot,4)))
        ! allocate(Denplotslice(size(Tplot,1),size(Tplot,3)))
        ! do j = 1, size(Tplot,4)
        !     do i = 1, size(Tplot,2)
        !         call calc_density(Tplot(:,i,:,j),Splot(:,i,:,j),Denplotslice,depth = .true.,depth_int = plotgridsizez)
        !         Denplot(:,i,:,j) = Denplotslice
        !     end do
        ! end do

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
    if(draw_1)goto 100
    if(draw_2)goto 200
    if(draw_1 .eqv..false..and.draw_2 .eqv..false.)then
        print*,'neither draw_1 nor draw_2 is selected. exiting...'
        stop
    end if

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! 1st part
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
100 continue
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
                if(i == 1)then;d = d1/plotgridsizez;call symbolr(-0.2,height/2.,0.6,int2str(d1)//'m')
                else if(i == 2)then;d = d2/plotgridsizez;call symbolr(-0.2,height/2.,0.6,int2str(d2)//'m')
                else if(i == 3)then;d = d3/plotgridsizez;call symbolr(-0.2,height/2.,0.6,int2str(d3)//'m')
                else if(i == 4)then;d = d4/plotgridsizez;call symbolr(-0.2,height/2.,0.6,int2str(d4)//'m')
                end if
                ! if(i==1)call symbolr(-0.1,height+0.5,0.6,'Grids;'//int2str(gridsx)//'*'//int2str(gridsy)//';;d=')
                do j = 1, iterations(n)
                    ! iplot = (n-1)*state_int*(iterations(n))+initial_state + (j-1)*state_int + 1 ! +1 is because day 0 is the first record
                    iplot = state_int*(sum(iterations(1:n-1)))+initial_state + (j-1)*state_int + 1
                    ! print*,iplot,n,i,j,'iplot'
                    call memori(gridsy/10+1,0.05,5,height,-90.,y = height/2.) ! 40km memori
                    call memori(gridsx/10+1,0.05,5,width,0.,x = width/2.,y = 0.)
                    ! print*,n*(j-1)*state_int+initial_state
                    if(i==1)call symbolc(width/2.,height+0.5,0.6,trim(tunit)//trim(int2str(iplot-1)))
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
                if(i == 1)then;y = y1;call symbolr(-0.2,-height/2.,0.6,real2str((y*gridsizey)/1000.)//';km')
                else if(i == 2)then;y = y2;call symbolr(-0.2,-height/2.,0.6,real2str((y*gridsizey)/1000.)//';km')
                else if(i == 3)then;y = y3;call symbolr(-0.2,-height/2.,0.6,real2str((y*gridsizey)/1000.)//';km')
                else if(i == 4)then;y = y4;call symbolr(-0.2,-height/2.,0.6,real2str((y*gridsizey)/1000.)//';km')
                end if
                ! if(i == 1)call symbolr(-0.1,0.5,0.6,'Grids;'//int2str(gridsx)//'*'//int2str(gridsz)//';;y=')    
                if(n==1)call plotsave(int2str(y))
                do j = 1, iterations(n)
                    iplot = state_int*(sum(iterations(1:n-1)))+initial_state + (j-1)*state_int + 1
                    call memori(int(plotgridsz+1),0.05,10,-height,-90.,y = -height/2.)
                    call memori(gridsx/10+1,0.05,10,width,0.,x = width/2.,y = -height)
                    if(i==1)call symbolc(width/2.,0.5,0.6,trim(tunit)//trim(int2str(iplot-1)))
                    call butler_psk(Splot(:,y,:,iplot),width,-height,0.,33.95,34.3,0.05,'b2w2r',7,bpt1=4,centralize = 4)
                    call butler_cont(Tplot(:,y,:,iplot),width,-height,0.,0.,1.,thicc = 5)! 1 deg interval 
                    call plot(width+0.2,0.,-3)  
                end do
                call plotback(int2str(y))
                call plot(0.,-height-0.3,-3)
            end do
        end do
        call plotomit
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                            ! vvel xy plane, z = 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        call newpage(h = 'x,y plane, V, day0-90',x = 0.8,y = -height)
        
        do n = 1, pages
            if(n>1.and.n/=pages)call newpage(h = 'x,y plane, V, continues',x = 0.8,y = -height)
            if(n>1.and.n == pages)call newpage(h = 'x,y plane, V, ends',x = 0.8,y = -height)
            do i = 1,4
                if(i == 1)then;d = d1/plotgridsizez;call symbolr(-0.2,height/2.,0.6,int2str(d1)//'m')
                else if(i == 2)then;d = d2/plotgridsizez;call symbolr(-0.2,height/2.,0.6,int2str(d2)//'m')
                else if(i == 3)then;d = d3/plotgridsizez;call symbolr(-0.2,height/2.,0.6,int2str(d3)//'m')
                else if(i == 4)then;d = d4/plotgridsizez;call symbolr(-0.2,height/2.,0.6,int2str(d4)//'m')
                end if
                ! if(i==1)call symbolr(-0.1,height+0.5,0.6,'Grids;'//int2str(gridsx)//'*'//int2str(gridsy)//';;d=')
                if(n==1)call plotsave(int2str(i))
                do j = 1, iterations(n)
                    if(n == 1.and.i == 1.and. j == 1)then 
                        call arrow(-1.,height/4.,-1.,height/4.+.2,0.05,3)
                        call symbolc(-1.,height/4.+0.5,0.4,'10cm/s')
                    end if
                    iplot = state_int*(sum(iterations(1:n-1)))+initial_state + (j-1)*state_int + 1
                    call memori(gridsy/10+1,0.05,10,height,-90.,y = height/2.)
                    call memori(gridsx/10+1,0.05,10,width,0.,x = width/2.,y = 0.)
                    if(i==1)call symbolc(width/2.,height+0.5,0.6,trim(tunit)//trim(int2str(iplot-1)))
                    ! call butler_psmask(V(:,:,d,(j-1)*state_int+initial_state+1),width,height,-1.,0.,r = 0.4 ,g = 0.4 ,b = 0.4)
                    ! call butler_cont(V(:,:,d,(j-1)*state_int+initial_state+1),width,height,0.,-0.2,0.02,thicc = 5,maskn = .true.) 
                    ! print*,(j-1)*state_int+initial_state+1
                    ! print*,size(V(:,:,d,(j-1)*state_int+initial_state+1))
                    call butler_vector(Uplot(:,:,d,iplot),Vplot(:,:,d,iplot),width,height,scalef = 2.,line_thickness = 3,thinfx = 8,thinfy = 4)
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
                if(i == 1)then;d = d1/plotgridsizez;call symbolr(-0.2,height/2.,0.6,int2str(d1)//'m')
                else if(i == 2)then;d = d2/plotgridsizez;call symbolr(-0.2,height/2.,0.6,int2str(d2)//'m')
                else if(i == 3)then;d = d3/plotgridsizez;call symbolr(-0.2,height/2.,0.6,int2str(d3)//'m')
                else if(i == 4)then;d = d4/plotgridsizez;call symbolr(-0.2,height/2.,0.6,int2str(d4)//'m')
                end if
                    ! if(i==1)call symbolr(-0.1,height+0.5,0.6,'Grids;'//int2str(gridsx)//'*'//int2str(gridsy)//';;d=')
                    if(n==1)call plotsave(int2str(i))
                    do j = 1, iterations(n)
                        if(n == 1.and.i == 1.and. j == 1)then 
                            call arrow(width/2.,height/2.,width/2.,height/2.+1.,0.2,3)
                        end if
                        iplot = state_int*(sum(iterations(1:n-1)))+initial_state + (j-1)*state_int + 1
                        call memori(gridsy/10+1,0.05,10,height,-90.,y = height/2.)
                        call memori(gridsx/10+1,0.05,10,width,0.,x = width/2.,y = 0.)
                        if(i==1)call symbolc(width/2.,height+0.5,0.6,trim(tunit)//trim(int2str(iplot-1)))
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
                            ! vvel xz plane, y = 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        call newpage(h = 'x,z plane, V',x = 0.8,y = 0.)
        do n = 1, pages
            if(n>1.and.n/=pages)call newpage(h = 'x,z plane, V, continues',x = 0.8,y = 0.)
            if(n>1.and.n == pages)call newpage(h = 'x,z plane, V, ends',x = 0.8,y = 0.)
            do i = 1,4
                if(i == 1)then;y = y1;call symbolr(-0.2,-height/2.,0.6,real2str((y*gridsizey)/1000.)//';km')
                else if(i == 2)then;y = y2;call symbolr(-0.2,-height/2.,0.6,real2str((y*gridsizey)/1000.)//';km')
                else if(i == 3)then;y = y3;call symbolr(-0.2,-height/2.,0.6,real2str((y*gridsizey)/1000.)//';km')
                else if(i == 4)then;y = y4;call symbolr(-0.2,-height/2.,0.6,real2str((y*gridsizey)/1000.)//';km')
                end if
                ! if(i == 1)call symbolr(-0.1,0.5,0.6,'Grids;'//int2str(gridsx)//'*'//int2str(gridsz)//';;y=')    
                if(n==1)call plotsave(int2str(y))
                do j = 1, iterations(n)
                    iplot = state_int*(sum(iterations(1:n-1)))+initial_state + (j-1)*state_int + 1
                    call memori(plotgridsz+1,0.05,10,-height,-90.,y = -height/2.)
                    call memori(gridsx+1,0.05,10,width,0.,x = width/2.,y = -height)
                    if(i==1)call symbolc(width/2.,0.5,0.6,trim(tunit)//trim(int2str(iplot-1)))
                    call butler_psmask(Vplot(:,y,:,iplot),width,-height,-1.,0.,r = 0.6 ,g = 0.6 ,b = 0.6)
                    call butler_cont(Vplot(:,y,:,iplot),width,-height,0.,-1.,0.05)!5cm interval 
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

                iplot = (i-1)*state_int + initial_state + 1
                call memori(gridsy/10+1,0.05,10,height,-90.,y = height/2.)
                call memori(gridsx/10+1,0.05,10,width,0.,x = width/2.,y = 0.)
                call symbolc(width/2.,height+0.1,0.4,trim(tunit)//trim(int2str(iplot-1)))
                ! call butler_psk(eta(:,:,(j-1)*state_int+initial_state+1),width,height,-1.,-0.5,0.5,'b2w2r',7,bpt1=4,centralize = 4)
                call butler_cont(eta(:,:,iplot),width,height,0.,-10.,0.05,maskn = .true.) !5cm interval
            if(mod(i,10) == 0)then 
                call plotback('eta')
                call plot(0.,i/10*(-height-0.5),-3)
            else
                call plot(width+0.2,0.,-3)
            end if

        end do
        call plotomit

        call plote

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! 2nd part
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if(draw_2)goto 200
    if(draw_2 .eqv..false.)goto 300

200     allocate(total_inflow(final_state),inflow_above400(final_state),outflow(final_state),delta_eta(final_state))
        do i = 1, final_state2 ! inflow
            total_inflow(i) = sum(Vplot(:,:,:,i) * 4.*10.**(3.) * 10.)* 10.**(-6)/200.! total inflow average over the whole domain in sv
            inflow_above400(i) = sum(Vplot(:,:,:20,i) * 4.*10.**(3.) * 10.)* 10.**(-6)/200. ! average inflow above 200m in Sv
        end do
        print*,size(Vplot,1),size(Vplot,2),size(Vplot,3),size(Vplot,4)
        do i = 1, final_state2 ! outflow
            outflow(i) = sum(Uplot(126:200,116:125,:,i) * 4.*10.**(3.) * 10.)* 10.**(-6)/75. ! average eastern flow through strait, in Sv
        end do
        do i = 1, final_state2 ! delta eta
            delta_eta(i) = (eta(130,116,i) - eta(130,125,i)) * 100. ! eta difference across the strait, in cm
        end do

        height2 = 5.;width2 = 15.
        call plots2(psfile = psfile2,oopt = 'otops', x = 1., y = -height2,mode = 'portrait',h = psfile2_title)
        !inflow
            call butler_linegraph(total_inflow,width2,height2,-4.,4.,0.,.true.,memsymfreq = 2,tlabel = 'inflow at y = 50km cross section')
            call butler_linegraph(inflow_above400,width2,height2,-4.,4.,0.,.false.,gl = 1.)
            call butler_linegraph(total_inflow-inflow_above400,width2,height2,-4.,4.,0.,.false.,bl = 1.)
            call symbol(width2+0.2,height2*2./3.,0.5,'gr = < 400m;blu = > 400m;black = total')
            call num_memori(0.,40.,41,10,0.6,-1,width2,gap = 2)
        !outflow
            call plot(0.,-height-3.,-3)
            call butler_linegraph(outflow,width2,height2,-2.,2.,0.,.true.,memsymfreq = 1,tlabel = 'outflow at east of strait')
            call num_memori(0.,40.,41,10,0.6,-1,width2,gap = 2)
        ! delta eta
            call plot(0.,-height2-3.,-3)
            call butler_linegraph(eta(130,116,:)*100.,width2,height2,0.,40.,0.,.false.,gl = 1.)
            call butler_linegraph(eta(130,125,:)*100.,width2,height2,0.,40.,0.,.false.,bl = 1.)
            call butler_linegraph(delta_eta,width2,height2,0.,40.,0.,.true.,memsymfreq = 5,tlabel = 'eta difference across the strait')
            call symbol(width2+0.2,height2*2./3.,0.5,'gr = south;blu = north')
            call num_memori(0.,40.,41,10,0.6,-1,width2,gap = 2)

            call newpage(mode = 'landscape')
            call otops(x = 0., y = -height+1.5)
            call plotsave('draw2')
            call header('Eta, 2cm interval',symbol_size = 0.6,y = 0.5)
            do i = 1,number_of_graphs2
                iplot = (i-1)*state_int2 + initial_state2 
                call memori(gridsy/10+1,0.05,10,height,-90.,y = height/2.)
                call memori(gridsx/10+1,0.05,10,width,0.,x = width/2.,y = 0.)
                call symbolc(width/2.,height+0.1,0.4,trim(tunit2)//trim(int2str(iplot-1)))
                call butler_cont(eta(:,:,iplot),width,height,0.,-1.,0.02,maskn = .true.,thicc = 10) !2cm interval
                if(i==1)then;call rgbk(1.,0.,0.);call plot(0.,height*0.7,3);call plot(width,height*0.7,2);call rgbk(0.,0.,0.);endif
                call plot(width+0.2,0.,-3)
            end do
        ! velocity vectors
            call plotback('draw2');call plot(0.,-height-1.0,-3)
            call header('velocity vectors',symbol_size = 0.6,y = -0.7-height)
            do i = 1,number_of_graphs2
                d = d5/plotgridsizez
                iplot = (i-1)*state_int2 + initial_state2 
                    call memori(gridsy+1,0.05,10,height,-90.,y = height/2.)
                    call memori(gridsx+1,0.05,10,width,0.,x = width/2.,y = 0.)
                    call butler_vector(Uplot(:,:,d,iplot),Vplot(:,:,d,iplot),width,height,scalef = 2.,line_thickness = 3,thinfx = 8,thinfy = 4)
                    call plot(width+0.2,0.,-3)
            end do
            call arrow(0.5,0.,0.5,.2);call symbolc(0.,0.4,0.4,'10cm/s')

        ! vvel north of strait
            call plotback('draw2');call plot(0.,-height-1.0 -1.0,-3)
            call header('velocity profiles 100km north of strait,5cm int',symbol_size = 0.6,y = -2*height-1.7)

            do i = 1, number_of_graphs2
                iplot = (i-1)*state_int2 + initial_state2 
                    call memori(plotgridsz+1,0.05,10,-height,-90.,y = -height/2.)
                    call memori(gridsx+1,0.05,10,width,0.,x = width/2.,y = -height)
                    call butler_psmask(Vplot(:,y5,:,iplot),width,-height,-1.,0.,r = 0.6 ,g = 0.6 ,b = 0.6)
                    call butler_cont(Vplot(:,y5,:,iplot),width,-height,0.,-1.,0.05)!5cm interval 
                    call plot(width+0.2,0.,-3)
            end do

        ! density north of strait
        ! call plotback('draw2');call plot(0.,-2*height-1.0 -1.0 - 1.0,-3)
        ! call header('Density profiles 100km north of strait',symbol_size = 0.6,y = -3*height-2.7)

        ! do i = 1, number_of_graphs2
        !     iplot = (i-1)*state_int2 + initial_state2 
        !         call memori(plotgridsz+1,0.05,10,-height,-90.,y = -height/2.)
        !         call memori(gridsx+1,0.05,10,width,0.,x = width/2.,y = -height)
        !         call butler_psmask(Denplot(:,y5,:,iplot),width,-height,20.,26.5,r = 1. ,g = .9 ,b = .9)
        !         call butler_psmask(Denplot(:,y5,:,iplot),width,-height,26.5,30.,r = 1. ,g = .6 ,b = .6)
        !         call butler_cont(Denplot(:,y5,:,iplot),width,-height,0.,25.,0.2,thicc = 5)
        !         call plot(width+0.2,0.,-3)  
        ! end do
        ! call colorscale(r,g,b,33.95,34.3,2,0.3,1,height,0.2,lt = 1, gt = 1,rangle = 90.,symbol_start = 2,y = -height/2.)

        call newpage

        call plotback('draw2');call plot(0.,height-0.5,-3)
        call header('Cross Sectional View of Strait, TS, 5cm int')

        do i = 1, number_of_graphs2
            iplot = (i-1)*state_int2 + initial_state2 
                call memori(plotgridsz+1,0.05,10,-height,-90.,y = -height/2.)
                ! call memori(gridsx+1,0.05,10,width,0.,x = width/2.,y = -height)
                call symbolc(width/2.,0.1,0.4,trim(tunit2)//trim(int2str(iplot-1)))
                call butler_psk(Splot(195,125:116:-1,:20,iplot),width,-height,0.,33.95,34.3,0.05,'b2w2r',7,bpt1=4,centralize = 4,r = r, g = g, b = b)
                call butler_cont(Tplot(195,125:116:-1,:20,iplot),width,-height,0.,0.,5.)! 5 deg interval 
                call plot(width+0.2,0.,-3)  
        end do

        call plotback('draw2');call plot(0.,-2.,-3)
        call header('Cross Sectional View of Strait, U, 5cm int',y = -height-1.5)

        do i = 1, number_of_graphs2
            iplot = (i-1)*state_int2 + initial_state2 
                call memori(plotgridsz+1,0.05,10,-height,-90.,y = -height/2.)
                ! call memori(gridsx+1,0.05,10,width,0.,x = width/2.,y = -height)
                call butler_psmask(Uplot(195,125:116:-1,:20,iplot),width,-height,-1.,0.,r = 0.4 ,g = 0.4 ,b = 0.4)
                call butler_psmask(Uplot(195,125:116:-1,:20,iplot),width,-height,0.,0.2,r = 1. ,g = 0.8 ,b = 0.8)
                call butler_psmask(Uplot(195,125:116:-1,:20,iplot),width,-height,0.2,0.4,r = 1. ,g = 0.4 ,b = 0.4)
                call butler_psmask(Uplot(195,125:116:-1,:20,iplot),width,-height,0.4,0.6,r = 1. ,g = 0. ,b = 0.)
                call butler_psmask(Uplot(195,125:116:-1,:20,iplot),width,-height,0.6,0.8,r = 0.8 ,g = 0. ,b = 0.)
                call butler_cont(Uplot(195,125:116:-1,:20,iplot),width,-height,0.,-1.,0.05)!5cm interval 
                call plot(width+0.2,0.,-3)
        end do

        call plote  

        
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    call plote

300 continue
end program 

subroutine handle_err(status)
    use netcdf
    integer, intent(in) :: status
    if (status /= nf90_noerr) then
        print *, trim(nf90_strerror(status))
        stop "Stopped"
    end if
end subroutine handle_err
