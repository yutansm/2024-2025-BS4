program welcht_for_station1
    implicit none
    integer,parameter::years = 15, months = 12, lines = 2, stations = 9, depth = 400
    integer,parameter::l = 1,st = 9     ! N Line Station 1(9 in program) ONLY!
    real,parameter::height = 6., width = 10.
    real,dimension(years,months,lines,stations,depth)::potemp_c5, sal_c5, sigma_c5,DH_array
    real,dimension(months,lines,stations,depth)::potemp_av,potemp_sd,sal_av,sal_sd,sigma_av,sigma_sd,DH_av,DH_sd,DH_sem
    integer,dimension(months,lines,stations,depth)::potemp_data, sal_data, sigma_data, DH_data
    real,dimension(400)::bet_av
    integer,dimension(400)::mask_av,mask_test
    real,dimension(0:100)::r1,g1,b1,r2,g2,b2,r3,g3,b3
    integer::m,d,i,n,result
    character::LorS*5
    real::dx,dy,plot_av,plot_sem

    dx = width/14.; dy = -height/real(depth)
    call calibrated_data51(potemp_c5,sal_c5)
    call create_sigma_array(potemp_c5,sal_c5,sigma_c5)
    call create_DH_array(sigma_c5,DH_array)
    call avsd_dataquan(potemp_c5,potemp_av,potemp_sd,potemp_data)
    call avsd_dataquan(sal_c5,sal_av,sal_sd,sal_data)
    call avsd_dataquan(sigma_c5,sigma_av,sigma_sd,sigma_data)
    call avsdsem_dataquan(DH_array,DH_av,DH_sd,DH_sem,DH_data)

    call plots(2.5,16.,13,'/LARGE0/gr10291/nishimori2/aomori/t_test/st1_TSsigmaDH.ps')
    call symbol(2.5,2.5,1.,"Welch's t Test for st1 (two sided, a=0.05)",0.,len("Welch's t Test for st1 (two sided, a=0.05)"))
    
! potemp
    call create_box(width,-height,3);call mod12_memori(13,0.6,width,0.,-height);call num_memori(0.,400.,40,5,0.4,-1,-height,-90,0,0)
    call symbolc(width/2.,0.6,0.6,'Potential Temp',0.,len('potential temp'))
    call b2r_colorgrad(24,7,r1,g1,b1)
    do i = 1,months+1
        if(mod(i,12)/=0) then;m = mod(i,12)
        else;m = 12;end if

        bet_av(1:depth) = potemp_av(m,l,st,1:depth)
        ! print*,m,bet_av(1:depth)
        do d = 1, depth
            if (bet_av(d)==0.) then
                mask_av(d) = 0
            else;mask_av(d) = 1
            end if
        end do
        call betcolork(dx/2.+real(i-1)*dx,dx,dy,bet_av,depth,mask_av,-100.,0.,r1(0),g1(0),b1(0))
        call betcolork(dx/2.+real(i-1)*dx,dx,dy,bet_av,depth,mask_av,24.,100.,r1(25),g1(25),b1(25))
        do n = 1, 24
        call betcolork(dx/2.+real(i-1)*dx,dx,dy,bet_av,depth,mask_av,real(n-1),real(n),r1(n),g1(n),b1(n))
        end do
        do d = 1,depth
            if(m == 1) then
                call welchttest(potemp_av(m,l,st,d),potemp_sd(m,l,st,d),potemp_data(m,l,st,d),potemp_av(12,l,st,d),potemp_sd(12,l,st,d),potemp_data(12,l,st,d),result,LorS)
            else;call welchttest(potemp_av(m,l,st,d),potemp_sd(m,l,st,d),potemp_data(m,l,st,d),potemp_av(m-1,l,st,d),potemp_sd(m-1,l,st,d),potemp_data(m-1,l,st,d),result,LorS)
            end if
            if(result == 0) then;mask_test(d) = 0
            else;mask_test(d) = 1
            end if
            ! print*,m,d,result,LorS
        end do
        call betcolorI(dx/2.+real(i-1)*dx,dx,dy,mask_test,depth,mask_test,1,0.,0.,0.)
    end do
    call newpen2(2)
        do n = 1,8
            call rgbk(1.,1.,1.)
            call plot(0.,-height/8.*real(n),3);call plot(width,-height/8.*real(n),2)
        end do
call plot(width+.5,-height,-3);call colorscale_creator(24,r1,g1,b1,0.,24.,5,0.4,1,height,0.3,90,1,1)
call plot(2.,height,-3)
! call gmark(0.,0.,0.2,1)

! sal
    call create_box(width,-height,3);call mod12_memori(13,0.6,width,0.,-height);call num_memori(0.,400.,40,5,0.4,-1,-height,-90,0,0)
    call symbolc(width/2.,0.6,0.6,'Salinity',0.,len('salinity'))
    call b2r_colorgrad(25,15,r2,g2,b2)
    do i = 1,months+1
        if(mod(i,12)/=0) then;m = mod(i,12)
        else;m = 12;end if

        bet_av(1:depth) = sal_av(m,l,st,1:depth)
        ! print*,m,bet_av(1:depth)
        do d = 1, depth
            if (bet_av(d)==0.) then
                mask_av(d) = 0
            else;mask_av(d) = 1
            end if
        end do
        call betcolork(dx/2.+real(i-1)*dx,dx,dy,bet_av,depth,mask_av,0.,33.7,r2(0),g2(0),b2(0))
        call betcolork(dx/2.+real(i-1)*dx,dx,dy,bet_av,depth,mask_av,34.2,100.,r2(26),g2(26),b2(26)) 
        do n = 1, 25
            call betcolork(dx/2.+real(i-1)*dx,dx,dy,bet_av,depth,mask_av,33.7+real(n-1)/50.,33.7+real(n)/50.,r2(n),g2(n),b2(n))
        end do
        do d = 1,depth
            if(m == 1) then
                call welchttest(sal_av(m,l,st,d),sal_sd(m,l,st,d),sal_data(m,l,st,d),sal_av(12,l,st,d),sal_sd(12,l,st,d),sal_data(12,l,st,d),result,LorS)
            else;call welchttest(sal_av(m,l,st,d),sal_sd(m,l,st,d),sal_data(m,l,st,d),sal_av(m-1,l,st,d),sal_sd(m-1,l,st,d),sal_data(m-1,l,st,d),result,LorS)
            end if
            if(result == 0) then;mask_test(d) = 0
            else;mask_test(d) = 1
            end if
            ! print*,m,d,result,LorS
        end do
        call betcolorI(dx/2.+real(i-1)*dx,dx,dy,mask_test,depth,mask_test,1,0.,0.,0.)
    end do
    call newpen2(2)
        do n = 1,8
            call rgbk(1.,1.,1.)
            call plot(0.,-height/8.*real(n),3);call plot(width,-height/8.*real(n),2)
        end do
call plot(width+.5,-height,-3);call colorscale_creator(25,r2,g2,b2,33.7,34.2,5,0.4,2,height,0.3,90,1,1)

call plot(-3.0-width*2.,-3.,-3)
! call gmark(0.,0.,0.2,1)

! Density
    call create_box(width,-height,3);call mod12_memori(13,0.6,width,0.,-height);call num_memori(0.,400.,40,5,0.4,-1,-height,-90,0,0)
    call symbolc(width/2.,0.6,0.6,'Density',0.,len('density'))
    call b2r_colorgrad(32,15,r3,g3,b3)
    do i = 1,months+1
        if(mod(i,12)/=0) then;m = mod(i,12)
        else;m = 12;end if

        bet_av(1:depth) = sigma_av(m,l,st,1:depth)
        ! print*,m,bet_av(1:depth)
        do d = 1, depth
            if (bet_av(d)==0.) then
                mask_av(d) = 0
            else;mask_av(d) = 1
            end if
        end do
        call betcolork(dx/2.+real(i-1)*dx,dx,dy,bet_av,depth,mask_av,0.,24.0,r3(0),g3(0),b3(0))
        call betcolork(dx/2.+real(i-1)*dx,dx,dy,bet_av,depth,mask_av,27.2,100.,r3(33),g3(33),b3(33)) 
        do n = 1, 32
            call betcolork(dx/2.+real(i-1)*dx,dx,dy,bet_av,depth,mask_av,24.0+real(n-1)/10.,24.0+real(n)/10.,r3(n),g3(n),b3(n))
        end do
        do d = 1,depth
            if(m == 1) then
                call welchttest(sigma_av(m,l,st,d),sigma_sd(m,l,st,d),sigma_data(m,l,st,d),sigma_av(12,l,st,d),sigma_sd(12,l,st,d),sigma_data(12,l,st,d),result,LorS)
            else;call welchttest(sigma_av(m,l,st,d),sigma_sd(m,l,st,d),sigma_data(m,l,st,d),sigma_av(m-1,l,st,d),sigma_sd(m-1,l,st,d),sigma_data(m-1,l,st,d),result,LorS)
            end if
            if(result == 0) then;mask_test(d) = 0
            else;mask_test(d) = 1
            end if
            ! print*,m,d,result,LorS
        end do
        call betcolorI(dx/2.+real(i-1)*dx,dx,dy,mask_test,depth,mask_test,1,0.,0.,0.)
    end do
    call newpen2(2)
        do n = 1,8
            call rgbk(1.,1.,1.)
            call plot(0.,-height/8.*real(n),3);call plot(width,-height/8.*real(n),2)
        end do
    call plot(width+.5,-height,-3);call colorscale_creator(32,r3,g3,b3,24.0,27.2,5,0.4,2,height,0.3,90,1,1)
    call plot(2.,0.,-3)

! Dynamic Height
    call create_box(width,height,3);call mod12_memori(13,0.6,width,0.,0.)
    call plot(width,0.,-3);call num_memori(0.5,3.5,30,5,0.45,1,height,90,0,0);call plot(-width,0.,-3)
    call symbolc(width/2.,height+0.6,0.6,'Dynamic Height (at 400db)',0.,len('dynamic height (at 400db)'))
    call symbolc(width+1.1,height/2.,0.4,'(3895.0+)',-90.,len('(3895.0+)'))
    do n = 1, 13
        if(mod(n,12)/=0) then;m = mod(n,12)
        else;m = 12;end if
        plot_av = (DH_av(m,l,st,400)-3895.5)*height/3.
        plot_sem = (DH_sem(m,l,st,400))*height/3.
        call rgbk(0.,0.,0.)
        call gmark(real(n)*dx,plot_av,0.1,1)
        call plot(real(n)*dx,plot_av-plot_sem,3);call plot(real(n)*dx,plot_av+plot_sem,2)
        if(m ==1) then
            call welchttest(DH_av(m,l,st,400),DH_sd(m,l,st,400),DH_data(m,l,st,400),DH_av(12,l,st,400),DH_sd(12,l,st,400),DH_data(12,l,st,400),result,LorS)
        else;call welchttest(DH_av(m,l,st,400),DH_sd(m,l,st,400),DH_data(m,l,st,400),DH_av(m-1,l,st,400),DH_sd(m-1,l,st,400),DH_data(m-1,l,st,400),result,LorS)
        end if
        if (LorS == 'LARGE') then;call rgbk(1.,0.,0.);call symbolc(dx*real(n),plot_av+1.,0.2,'UP',0.,2)
        else if(LorS == 'small') then;call rgbk(0.,0.,1.);call symbolc(dx*real(n),plot_av+1.,0.2,'DOWN',0.,4)
        else;call symbolc(dx*real(n),plot_av+1.,0.3,'-',0.,1)
        end if
    end do

call newpage
call symbol(6.,2.5,1.,"Increase or Decrease",0.,len("Increase or Decrease"))
! potemp up or down
    call create_box(width,-height,3);call mod12_memori(13,0.6,width,0.,-height);call num_memori(0.,400.,40,5,0.4,-1,-height,-90,0,0)
    call symbolc(width/2.,0.6,0.6,'Potential Temp',0.,len('potential temp'))
    ! call b2r_colorgrad(24,7,r1,g1,b1)
    do i = 1,months+1
        if(mod(i,12)/=0) then;m = mod(i,12)
        else;m = 12;end if
        do d = 1,depth
            if(m == 1) then
                call welchttest(potemp_av(m,l,st,d),potemp_sd(m,l,st,d),potemp_data(m,l,st,d),potemp_av(12,l,st,d),potemp_sd(12,l,st,d),potemp_data(12,l,st,d),result,LorS)
            else;call welchttest(potemp_av(m,l,st,d),potemp_sd(m,l,st,d),potemp_data(m,l,st,d),potemp_av(m-1,l,st,d),potemp_sd(m-1,l,st,d),potemp_data(m-1,l,st,d),result,LorS)
            end if
            if(result == 0 .and. LorS == 'LARGE') then;mask_test(d) = 1 !USE RED
            else if (result == 0 .and. LorS == 'small') then;mask_test(d) = 2 !USE BLUE
            else if (LorS == 'NODIF') then; mask_test(d) = 3 !USE BLACK
            end if
            print*,m,d,LorS
        end do
        call betcolorI(dx/2.+real(i-1)*dx,dx,dy,mask_test,depth,mask_test,1,1.,0.,0.)
        call betcolorI(dx/2.+real(i-1)*dx,dx,dy,mask_test,depth,mask_test,2,0.,0.,1.)
        call betcolorI(dx/2.+real(i-1)*dx,dx,dy,mask_test,depth,mask_test,3,0.,0.,0.)
    end do
    call newpen2(2)
        do n = 1,8
            call rgbk(1.,1.,1.)
            call plot(0.,-height/8.*real(n),3);call plot(width,-height/8.*real(n),2)
        end do
call plot(width+2.5,0.,-3)
! sal up or down
call rgbk(0.,0.,0.)
call create_box(width,-height,3);call mod12_memori(13,0.6,width,0.,-height);call num_memori(0.,400.,40,5,0.4,-1,-height,-90,0,0)
call symbolc(width/2.,0.6,0.6,'Salinity',0.,len('salinity'))
do i = 1,months+1
    if(mod(i,12)/=0) then;m = mod(i,12)
    else;m = 12;end if
    do d = 1,depth
        if(m == 1) then
            call welchttest(sal_av(m,l,st,d),sal_sd(m,l,st,d),sal_data(m,l,st,d),sal_av(12,l,st,d),sal_sd(12,l,st,d),sal_data(12,l,st,d),result,LorS)
        else;call welchttest(sal_av(m,l,st,d),sal_sd(m,l,st,d),sal_data(m,l,st,d),sal_av(m-1,l,st,d),sal_sd(m-1,l,st,d),sal_data(m-1,l,st,d),result,LorS)
        end if
        if(result == 0 .and. LorS == 'LARGE') then;mask_test(d) = 1 !USE RED
        else if (result == 0 .and. LorS == 'small') then;mask_test(d) = 2 !USE BLUE
        else if (LorS == 'NODIF') then; mask_test(d) = 3 !USE BLACK
        end if
        ! print*,m,d,LorS
    end do
    call betcolorI(dx/2.+real(i-1)*dx,dx,dy,mask_test,depth,mask_test,1,1.,0.,0.)
    call betcolorI(dx/2.+real(i-1)*dx,dx,dy,mask_test,depth,mask_test,2,0.,0.,1.)
    call betcolorI(dx/2.+real(i-1)*dx,dx,dy,mask_test,depth,mask_test,3,0.,0.,0.)
end do
call newpen2(2)
    do n = 1,8
        call rgbk(1.,1.,1.)
        call plot(0.,-height/8.*real(n),3);call plot(width,-height/8.*real(n),2)
    end do

    call rgbk(0.,0.,0.)

! density
    call plot(-width-2.5,-height-3.,-3)
    call create_box(width,-height,3);call mod12_memori(13,0.6,width,0.,-height);call num_memori(0.,400.,40,5,0.4,-1,-height,-90,0,0)
    call symbolc(width/2.,0.6,0.6,'Density',0.,len('density'))
    do i = 1,months+1
        if(mod(i,12)/=0) then;m = mod(i,12)
        else;m = 12;end if
        do d = 1,depth
            if(m == 1) then
                call welchttest(sigma_av(m,l,st,d),sigma_sd(m,l,st,d),sigma_data(m,l,st,d),sigma_av(12,l,st,d),sigma_sd(12,l,st,d),sigma_data(12,l,st,d),result,LorS)
            else;call welchttest(sigma_av(m,l,st,d),sigma_sd(m,l,st,d),sigma_data(m,l,st,d),sigma_av(m-1,l,st,d),sigma_sd(m-1,l,st,d),sigma_data(m-1,l,st,d),result,LorS)
            end if
            if(result == 0 .and. LorS == 'LARGE') then;mask_test(d) = 1 !USE RED
        else if (result == 0 .and. LorS == 'small') then;mask_test(d) = 2 !USE BLUE
        else if (LorS == 'NODIF') then; mask_test(d) = 3 !USE BLACK
        end if
            ! print*,m,d,result,LorS
        end do
        call betcolorI(dx/2.+real(i-1)*dx,dx,dy,mask_test,depth,mask_test,1,1.,0.,0.)
        call betcolorI(dx/2.+real(i-1)*dx,dx,dy,mask_test,depth,mask_test,2,0.,0.,1.)
        call betcolorI(dx/2.+real(i-1)*dx,dx,dy,mask_test,depth,mask_test,3,0.,0.,0.)
    end do
    call newpen2(2)
        do n = 1,8
            call rgbk(1.,1.,1.)
            call plot(0.,-height/8.*real(n),3);call plot(width,-height/8.*real(n),2)
        end do

call plote
end program
