program betcolor_st1_monthly
    implicit none
    integer,parameter::years = 15, months = 12, lines = 2, stations = 9, depth = 400
    integer,parameter::l = 1,st = 9     ! N Line Station 1(9 in program) ONLY!
    integer,parameter:: iterations_temp = 24
    real,parameter:: height = 3., width = 6.
    real,dimension(years,months,lines,stations,depth)::potemp_c5, sal_c5, sigma_5
    real,dimension(months,lines,stations,depth)::potemp_av, potemp_sd, sal_av, sal_sd, sigma_av, sigma_sd,potemp_sem,sal_sem,sigma_sem
    integer,dimension(months,lines,stations,depth)::potemp_data, sal_data, sigma_data
    real,dimension(0:iterations_temp+1)::r1,g1,b1
    real,dimension(0:100)::r2,g2,b2,r3,g3,b3,r4,g4,b4,r5,g5,b5,r6,g6,b6,rsem1,gsem1,bsem1,rsem2,gsem2,bsem2,rsem3,gsem3,bsem3
    real,dimension(5)::rdata,gdata,bdata
    real,dimension(depth):: bet_array
    integer,dimension(depth):: bet_arrayi
    integer,dimension(depth):: mask
    integer:: m,d,n,i
    real:: dx,dy

    call plots(3.,16.5,13,'../timeseries_betcolor_st1.ps')
    call symbol(2.,2.5,0.7,'Time Series of Monthly Mean and SD (Station 1,N-Line)',0.,len('Time Series of Monthly Mean and SD (Station 1,N-Line)'))
    dx = width/14.; dy = -height/real(depth)
    call calibrated_data51(potemp_c5,sal_c5)
    call create_sigma_array(potemp_c5,sal_c5,sigma_5)
    call avsdsem_dataquan(potemp_c5,potemp_av,potemp_sd,potemp_sem,potemp_data)
    call avsdsem_dataquan(sal_c5,sal_av,sal_sd,sal_sem,sal_data)
    call avsdsem_dataquan(sigma_5,sigma_av,sigma_sd,sigma_sem,sigma_data)

    ! print*,potemp_c5(8,8,1,1:stations,100:200)

! mean temp
    call create_box(width,-height,3);call mod12_memori(13,0.4,width,0.,-height);call num_memori(0.,400.,40,5,0.3,-1,-height,-90,0,0)
    call symbolc(width/2.,0.8,0.4,'Sample Mean',0.,len('sample mean'));call symbolc(-1.,-height/2.,0.4,'Temp',90.,len('temp'))
    call plot(dx/2.,0.,-3)
    call b2r_colorgrad(iterations_temp,7,r1,g1,b1)

        do i = 1,months+1
            if(mod(i,12)/=0) then;m = mod(i,12)
            else;m = 12;end if

            bet_array(1:depth) = potemp_av(m,l,st,1:depth)
            ! print*,m,bet_array(1:depth)
            do d = 1, depth
                if (bet_array(d)==0.) then
                    mask(d) = 0
                else;mask(d) = 1
                end if
            end do
            ! print*,mask(1:depth)
            call betcolork(0.,dx,dy,bet_array,depth,mask,-100.,0.,r1(0),g1(0),b1(0))
            call betcolork(0.,dx,dy,bet_array,depth,mask,24.,100.,r1(iterations_temp+1),g1(iterations_temp+1),b1(iterations_temp+1))
            do n = 1, iterations_temp
            call betcolork(0.,dx,dy,bet_array,depth,mask,real(n-1),real(n),r1(n),g1(n),b1(n))
            ! print*,n,r1(n),g1(n),b1(n)
            end do
            call plot(dx,0.,-3)
        end do
    call plot(-13.*dx,-height-1.5,-3);call colorscale_creator(24,r1,g1,b1,0.,24.,5,0.3,1,width-dx,dx,0,1,1);call plot(13.*dx,height+1.5,-3)

! temp sample standard deviation 
    call plot(2.,0.,-3)
    call create_box(width,-height,3);call mod12_memori(13,0.4,width,0.,-height);call num_memori(0.,400.,40,5,0.3,-1,-height,-90,0,0)
    call symbolc(width/2.,0.8,0.4,'Sample SD',0.,len('sample SD'))
    call plot(dx/2.,0.,-3)
    call b2r_colorgrad(25,12,r2,g2,b2)

        do i = 1,months+1
            if(mod(i,12)/=0) then;m = mod(i,12)
            else;m = 12;end if
            
            bet_array(1:depth) = potemp_sd(m,l,st,1:depth)
            ! print*,m,bet_array(1:depth)
            do d = 1, depth
                if(bet_array(d)==0.) then
                    mask(d) = 0
                else;mask(d) = 1
                end if
            end do
            call betcolork(0.,dx,dy,bet_array,depth,mask,-100.,0.,r2(0),g2(0),b2(0))
            call betcolork(0.,dx,dy,bet_array,depth,mask,2.5,100.,r2(26),g2(26),b2(26))
            do n = 1, 25
                call betcolork(0.,dx,dy,bet_array,depth,mask,real(n-1)/10.,real(n)/10.,r2(n),g2(n),b2(n))
            end do
            call plot(dx,0.,-3)
        end do
    call plot(-13.*dx,-height-1.5,-3);call colorscale_creator(25,r2,g2,b2,0.,2.5,5,0.3,1,width-dx,dx,0,1,1);call plot(13.*dx,height+1.5,-3)

! temp stadard error of the mean
    call plot(2.,0.,-3)
    call create_box(width,-height,3);call mod12_memori(13,0.4,width,0.,-height);call num_memori(0.,400.,40,5,0.3,-1,-height,-90,0,0)
    call symbolc(width/2.,0.8,0.4,'Standard Error',0.,len('Standard Error'))
    call plot(dx/2.,0.,-3)
    call b2r_colorgrad(28,14,rsem1,gsem1,bsem1)
    
    do i = 1,months+1
        if(mod(i,12)/=0) then;m = mod(i,12)
        else;m = 12;end if
        
        bet_array(1:depth) = potemp_sem(m,l,st,1:depth)
        ! print*,m,bet_array(1:depth)
        do d = 1, depth
            if(bet_array(d)==0.) then
                mask(d) = 0
            else;mask(d) = 1
            end if
        end do
        call betcolork(0.,dx,dy,bet_array,depth,mask,-100.,0.,rsem1(0),gsem1(0),bsem1(0))
        call betcolork(0.,dx,dy,bet_array,depth,mask,.7,100.,rsem1(29),gsem1(29),bsem1(29))
        do n = 1, 28
            call betcolork(0.,dx,dy,bet_array,depth,mask,real(n-1)/40.,real(n)/40.,rsem1(n),gsem1(n),bsem1(n))
        end do
        call plot(dx,0.,-3)
    end do
call plot(-13.*dx,-height-1.5,-3);call colorscale_creator(28,rsem1,gsem1,bsem1,0.,.7,4,0.3,1,width-dx,dx,0,1,1);call plot(13.*dx,height+1.5,-3)



! sal mean
    call plot(-1.*(13.5*dx*3.+4.),-1.*(height+1.5+1.),-3)
    ! call gmark(0.,0.,0.2,1)
    call create_box(width,-height,3);call mod12_memori(13,0.4,width,0.,-height);call num_memori(0.,400.,40,5,0.3,-1,-height,-90,0,0)
    call symbolc(-1.,-height/2.,0.4,'Sal',90.,len('sal'))
    call plot(dx/2.,0.,-3)
    call b2r_colorgrad(25,15,r3,g3,b3)

        do i = 1, months+1
            if(mod(i,12)/=0) then;m = mod(i,12)
            else;m = 12;end if

            bet_array(1:depth) = sal_av(m,l,st,1:depth)
            ! print*,m,bet_array(1:depth)
            do d = 1, depth
                if(bet_array(d)==0.) then
                    mask(d) = 0
                else;mask(d) = 1
                end if   
            end do
            call betcolork(0.,dx,dy,bet_array,depth,mask,0.,33.7,r3(0),g3(0),b3(0))
            call betcolork(0.,dx,dy,bet_array,depth,mask,34.2,100.,r3(26),g3(26),b3(26)) 
            do n = 1, 25
                call betcolork(0.,dx,dy,bet_array,depth,mask,33.7+real(n-1)/50.,33.7+real(n)/50.,r3(n),g3(n),b3(n))
            end do
            call plot(dx,0.,-3)
        end do
    call plot(-13.*dx,-height-1.5,-3);call colorscale_creator(25,r3,g3,b3,33.7,34.2,5,0.3,2,width-dx,dx,0,1,1);call plot(13.*dx,height+1.5,-3)

! sal sample SD
    call plot(2.,0.,-3)
    call create_box(width,-height,3);call mod12_memori(13,0.4,width,0.,-height);call num_memori(0.,400.,40,5,0.3,-1,-height,-90,0,0)
    call plot(dx/2.,0.,-3)
    call b2r_colorgrad(20,10,r4,g4,b4)

    do i = 1, months+1
        if(mod(i,12)/=0) then;m = mod(i,12)
        else;m = 12;end if

        bet_array(1:depth) = sal_sd(m,l,st,1:depth)
        ! print*,m,bet_array(1:depth)
        do d = 1, depth
            if(bet_array(d)==0.) then
                mask(d) = 0
            else;mask(d) = 1
            end if   
        end do
        call betcolork(0.,dx,dy,bet_array,depth,mask,-100.,0.,r4(0),g4(0),b4(0))
        call betcolork(0.,dx,dy,bet_array,depth,mask,0.4,100.,r4(21),g4(21),b4(21)) 
        do n = 1, 20
            call betcolork(0.,dx,dy,bet_array,depth,mask,real(n-1)/50.,real(n)/50.,r4(n),g4(n),b4(n))
        end do
        call plot(dx,0.,-3)
    end do
    call plot(-13.*dx,-height-1.5,-3);call colorscale_creator(20,r4,g4,b4,0.,0.4,5,0.3,2,width-dx,dx,0,1,1);call plot(13.*dx,height+1.5,-3)

! sal SEM
    call plot(2.,0.,-3)
    call create_box(width,-height,3);call mod12_memori(13,0.4,width,0.,-height);call num_memori(0.,400.,40,5,0.3,-1,-height,-90,0,0)
    call plot(dx/2.,0.,-3)
    call b2r_colorgrad(24,12,rsem2,gsem2,bsem2)
    
    do i = 1,months+1
        if(mod(i,12)/=0) then;m = mod(i,12)
        else;m = 12;end if
        
        bet_array(1:depth) = sal_sem(m,l,st,1:depth)
        ! print*,m,bet_array(1:depth)
        do d = 1, depth
            if(bet_array(d)==0.) then
                mask(d) = 0
            else;mask(d) = 1
            end if
        end do
        call betcolork(0.,dx,dy,bet_array,depth,mask,-100.,0.,rsem2(0),gsem2(0),bsem2(0))
        call betcolork(0.,dx,dy,bet_array,depth,mask,.12,100.,rsem2(25),gsem2(25),bsem2(25))
        do n = 1, 24
            call betcolork(0.,dx,dy,bet_array,depth,mask,real(n-1)/200.,real(n)/200.,rsem2(n),gsem2(n),bsem2(n))
        end do
        call plot(dx,0.,-3)
    end do
call plot(-13.*dx,-height-1.5,-3);call colorscale_creator(24,rsem2,gsem2,bsem2,0.,.12,4,0.25,2,width-dx,dx,0,1,1);call plot(13.*dx,height+1.5,-3)

! sigma mean
    call plot(-1.*(13.5*dx*3.+4.),-1.*(height+1.5+1.),-3)
    ! call gmark(0.,0.,0.2,1)

    call create_box(width,-height,3);call mod12_memori(13,0.4,width,0.,-height);call num_memori(0.,400.,40,5,0.3,-1,-height,-90,0,0)
    call symbolc(-1.,-height/2.,0.4,'Density',90.,len('density'))
    call plot(dx/2.,0.,-3)
    call b2r_colorgrad(32,15,r5,g5,b5)

    do i = 1, months+1
        if(mod(i,12)/=0) then;m = mod(i,12)
        else;m = 12;end if

        bet_array(1:depth) = sigma_av(m,l,st,1:depth)
        ! print*,m,bet_array(1:depth)
        do d = 1, depth
            if(bet_array(d)==0.) then
                mask(d) = 0
            else;mask(d) = 1
            end if   
        end do
        call betcolork(0.,dx,dy,bet_array,depth,mask,0.,24.0,r5(0),g5(0),b5(0))
        call betcolork(0.,dx,dy,bet_array,depth,mask,27.2,100.,r5(33),g5(33),b5(33)) 
        do n = 1, 32
            call betcolork(0.,dx,dy,bet_array,depth,mask,24.0+real(n-1)/10.,24.0+real(n)/10.,r5(n),g5(n),b5(n))
        end do
        call plot(dx,0.,-3)
    end do
    call plot(-13.*dx,-height-1.5,-3);call colorscale_creator(32,r5,g5,b5,24.0,27.2,5,0.3,1,width-dx,dx,0,1,1);call plot(13.*dx,height+1.5,-3)

! sigma sample SD
    call plot(2.,0.,-3) 
    call create_box(width,-height,3);call mod12_memori(13,0.4,width,0.,-height);call num_memori(0.,400.,40,5,0.3,-1,-height,-90,0,0)
    call plot(dx/2.,0.,-3)
    call b2r_colorgrad(28,14,r6,g6,b6)

    do i = 1, months+1
        if(mod(i,12)/=0) then;m = mod(i,12)
        else;m = 12;end if

        bet_array(1:depth) = sigma_sd(m,l,st,1:depth)
        ! print*,m,bet_array(1:depth)
        do d = 1, depth
            if(bet_array(d)==0.) then
                mask(d) = 0
            else;mask(d) = 1
            end if   
        end do
        call betcolork(0.,dx,dy,bet_array,depth,mask,-100.,0.,r6(0),g6(0),b6(0))
        call betcolork(0.,dx,dy,bet_array,depth,mask,0.7,100.,r6(29),g6(29),b6(29)) 
        do n = 1, 28
            call betcolork(0.,dx,dy,bet_array,depth,mask,0.+real(n-1)/40.,0.+real(n)/40.,r6(n),g6(n),b6(n))
        end do
        call plot(dx,0.,-3)
    end do
    call plot(-13.*dx,-height-1.5,-3);call colorscale_creator(28,r6,g6,b6,0.,0.7,4,0.3,1,width-dx,dx,0,1,1);call plot(13.*dx,height+1.5,-3)

! sigma SEM    
    call plot(2.,0.,-3)
    call create_box(width,-height,3);call mod12_memori(13,0.4,width,0.,-height);call num_memori(0.,400.,40,5,0.3,-1,-height,-90,0,0)
    call plot(dx/2.,0.,-3)
    call b2r_colorgrad(20,10,rsem3,gsem3,bsem3)
    
    do i = 1,months+1
        if(mod(i,12)/=0) then;m = mod(i,12)
        else;m = 12;end if
        
        bet_array(1:depth) = sigma_sem(m,l,st,1:depth)
        ! print*,m,bet_array(1:depth)
        do d = 1, depth
            if(bet_array(d)==0.) then
                mask(d) = 0
            else;mask(d) = 1
            end if
        end do
        call betcolork(0.,dx,dy,bet_array,depth,mask,-100.,0.,rsem3(0),gsem3(0),bsem3(0))
        call betcolork(0.,dx,dy,bet_array,depth,mask,.1,100.,rsem3(21),gsem3(21),bsem3(21))
        do n = 1, 20
            call betcolork(0.,dx,dy,bet_array,depth,mask,real(n-1)/100.,real(n)/100.,rsem3(n),gsem3(n),bsem3(n))
        end do
        call plot(dx,0.,-3)
    end do
call plot(-13.*dx,-height-1.5,-3);call colorscale_creator(20,rsem3,gsem3,bsem3,0.,.2,5,0.3,2,width-dx,dx,0,1,1);call plot(13.*dx,height+1.5,-3)

! DATA QUANTITTY
    ! dx = width/13.
    ! call plot(3.,height+1.5+1.,-3)
    ! call create_box(width,-height,3);call mod12_memori(12,0.4,width,0.,-height);call num_memori(0.,400.,40,5,0.3,-1,-height,-90,0,0)
    ! call symbolc(width/2.,0.8,0.4,'Data Quantity',0.,len('data quantity'))
    ! call plot(dx/2.,0.,-3)
    ! call r2g_colorgrad(5,3,rdata,gdata,bdata)

    ! do m = 1, months
    !     bet_arrayi(1:depth) = potemp_data(m,l,st,1:depth)
    !     ! print*,m,bet_arrayi(1:depth)
    !     do d = 1, depth
    !         if(bet_arrayi(d)==0) then
    !             mask(d) = 0
    !         else;mask(d) = 1
    !         end if   
    !     end do
    !     do n = 1,5
    !         call betcolorI(dx,dy,bet_arrayi,depth,mask,n+10,rdata(n),gdata(n),bdata(n))
    !     end do
    !     call plot(dx,0.,-3)
    ! end do
    ! call plot(-12.*dx,-height-1.5,-3);call colorscale_data(5,rdata,gdata,bdata,11,15,1,0.3,width-dx,dx,0)
    ! call plot(-dx/2.,-1.5,-3)
    ! call gmark(0.,0.,0.2,1)
! newpage for data quantitty just checking to confirm data quantitty
    call newpage
    dx = width/13.
    call plot(8.,1.,-3)
    call symbol(1.,1.5,0.5,'Data Quantity',0.,len('data quantity'))
! Temp data quantity
    call create_box(width,-height,3);call mod12_memori(12,0.4,width,0.,-height);call num_memori(0.,400.,40,5,0.3,-1,-height,-90,0,0)
    call symbolc(-1.,-height/2.,0.4,'Temp',90.,len('temp'))
    call plot(dx/2.,0.,-3)
    call r2g_colorgrad(5,3,rdata,gdata,bdata)

    do m = 1, months
        bet_arrayi(1:depth) = potemp_data(m,l,st,1:depth)
        ! print*,m,bet_arrayi(1:depth)
        do d = 1, depth
            if(bet_arrayi(d)==0) then
                mask(d) = 0
            else;mask(d) = 1
            end if   
        end do
        do n = 1,5
            call betcolorI(0.,dx,dy,bet_arrayi,depth,mask,n+10,rdata(n),gdata(n),bdata(n))
        end do
        call plot(dx,0.,-3)
    end do
    call plot(-12.*dx,-height-1.5,-3);call colorscale_data(5,rdata,gdata,bdata,11,15,1,0.3,width-dx,dx,0)
    call plot(-dx/2.,-1.5,-3)

! Sal Data quantity
    call create_box(width,-height,3);call mod12_memori(12,0.4,width,0.,-height);call num_memori(0.,400.,40,5,0.3,-1,-height,-90,0,0)
    call symbolc(-1.,-height/2.,0.4,'Sal',90.,len('sal'))
    call plot(dx/2.,0.,-3)
    call r2g_colorgrad(5,3,rdata,gdata,bdata)

    do m = 1, months
        bet_arrayi(1:depth) = sal_data(m,l,st,1:depth)
        ! print*,m,bet_arrayi(1:depth)
        do d = 1, depth
            if(bet_arrayi(d)==0) then
                mask(d) = 0
            else;mask(d) = 1
            end if   
        end do
        do n = 1,5
            call betcolorI(0.,dx,dy,bet_arrayi,depth,mask,n+10,rdata(n),gdata(n),bdata(n))
        end do
        call plot(dx,0.,-3)
    end do
    call plot(-12.*dx,-height-1.5,-3);call colorscale_data(5,rdata,gdata,bdata,11,15,1,0.3,width-dx,dx,0)
    call plot(-dx/2.,-1.5,-3)

! Sigma data quantity just for confirmation
    call create_box(width,-height,3);call mod12_memori(12,0.4,width,0.,-height);call num_memori(0.,400.,40,5,0.3,-1,-height,-90,0,0)
    call symbolc(-1.,-height/2.,0.4,'Density',90.,len('density'))
    call plot(dx/2.,0.,-3)
    call r2g_colorgrad(5,3,rdata,gdata,bdata)

    do m = 1, months
        bet_arrayi(1:depth) = sigma_data(m,l,st,1:depth)
        ! print*,m,bet_arrayi(1:depth)
        do d = 1, depth
            if(bet_arrayi(d)==0) then
                mask(d) = 0
            else;mask(d) = 1
            end if   
        end do
        do n = 1,5
            call betcolorI(0.,dx,dy,bet_arrayi,depth,mask,n+10,rdata(n),gdata(n),bdata(n))
        end do
        call plot(dx,0.,-3)
    end do
    call plot(-12.*dx,-height-1.5,-3);call colorscale_data(5,rdata,gdata,bdata,11,15,1,0.3,width-dx,dx,0)
    call plot(-dx/2.,-2.,-3)

    

    call plote

end program



    

