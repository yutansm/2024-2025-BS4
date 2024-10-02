program tssigma201
    use constants
    use subroutines
    implicit none
    real,parameter::width = 2., height = 24.,width2 = 6.,height2 = 3.
    integer,parameter::ini_st = 4, fin_st = 9, obs_depth = 201
    real,dimension(fin_st-ini_st+1,months*years)::psarray2=0.
    integer,dimension(fin_st-ini_st+1,months*years)::mask2
    real,dimension(fin_st-ini_st+1,years)::psmean_y;  real,dimension(fin_st-ini_st+1,months)::psmean_m
    integer,dimension(fin_st-ini_st+1,years)::psdata_y; real,dimension(fin_st-ini_st+1,months)::psdata_m
    real,dimension(years)::testy;  real,dimension(months)::testm,semm;real,dimension(1)::totalol,slol
    integer,dimension(fin_st-ini_st+1,months)::datam
    real,dimension(0:180)::plotx=0.,ploty=0.
    character(len=5),dimension(180)::interpolated = 'false'
    integer::count = 0
    real::rnum,dxval,plotsem,sum1=0.,s,plot_s,totalmean
    integer,dimension(5)::two_peaks = (/3,6,9,13,15/)
    integer,dimension(4)::one_peak = (/5,7,12,14/)

    call plots(2.5,0.5,9,'../Plots/TempSalSigma/Timeseries/timeseries_potempc5_obs201R.ps')
    call symbol(0.,height+1.4,0.8,'time series of potemp',0.,len('time series of potemp'))
    call b2r_colorgrad(24,12,r1,g1,b1) !temp
    call b2cy2y2r_colorgrad(45,10,20,30,r2,g2,b2) !sigma
    call b2cy2y2r_colorgrad(48,12,24,36,r3,g3,b3) !sal
    call calibrated_data51(potemp_c5,sal_c5)
    call create_sigma_array(potemp_c5,sal_c5,sigma_c5)

    ! do j = 1,obs_depth,50
        j = obs_depth; l = 1
        call create_box(width,height,3)
        call numberr(width/2.,height+0.4,0.5,real(j),0.,-1);call symbol(width/2.,height+0.4,0.5,'m',0.,len('m'))
        dx = width/real(fin_st-ini_st+1);dy = height/(real(months*years))
        call mod12_memori3(0.,0.,months*years,0.15,-90,height,1,2)
        call st_memori(1,6,width,1,0.4,2)
        call floating_numbers(-1.5,dy*6.,2023.,-1.,15,0.6,0.,dy*12.,0.,-1)
        call symbolr(-0.3,-0.5,0.4,'St',0.,len('St'))
        call plot(0.,height,-3)

        do st = 1,fin_st-ini_st+1
            i = 1
            do y = 1, years
                do m = 1, months
                    !temp
                    psarray2(st,i) = potemp_c5(y,m,l,st+ini_st-1,j)
                    !sigma
                    ! psarray2(st,i) = sigma_c5(y,m,l,st+ini_st-1,j)
                    !sal
                    ! psarray2(st,i) = sal_c5(y,m,l,st+ini_st-1,j)

                        if(psarray2(st,i) == 0.) then;mask2(st,i) = 0;else;mask2(st,i) = 1;endif
                    i = i + 1
                end do 
            end do
        end do        
        ! print*,minval(psarray2),maxval(psarray2)
        !temp
            call betcolork2(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,months*years,fin_st-ini_st+1,months*years,-100.,0.,r1(0),g1(0),b1(0))
            call betcolork2(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,months*years,fin_st-ini_st+1,months*years,12.,100.,r1(25),g1(25),b1(25))
            do n = 1, 24
                call betcolork2(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,months*years,fin_st-ini_st+1,months*years,real(n-1)/2.,real(n)/2.,r1(n),g1(n),b1(n))
            end do
            do n = 1,7
                if(n<=4) then
                    call rgbk(0.,0.,0.7)
                else 
                    call rgbk(0.7,0.,0.)
                end if
                call pscont3(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,180,fin_st-ini_st+1,180,1,real((n-1)*2),0.)
            end do
        !sigma
            ! call betcolork2(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,months*years,fin_st-ini_st+1,months*years,0.,23.,r2(0),g2(0),b2(0))
            ! call betcolork2(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,months*years,fin_st-ini_st+1,months*years,27.5,100.,r2(46),g2(46),b2(46))
            ! do n = 1, 45
            !     call betcolork2(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,months*years,fin_st-ini_st+1,months*years,23.0+real(n-1)/10.,23.+real(n)/10.,r2(n),g2(n),b2(n))
            ! end do
            ! do n = 1,10
            !     if(n<=3) then
            !         call rgbk(0.,0.,0.7)
            !     else if(n<=5) then
            !         call rgbk(0.7,0.7,0.)
            !     else if (n<=7) then
            !         call rgbk(0.8,0.7,0.)
            !     else if (n<=10) then
            !         call rgbk(0.7,0.,0.)
            !     else
            !     end if
            !     call pscont3(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,180,fin_st-ini_st+1,180,1,23.+real((n-1)/2.),0.)
            ! end do
        !sal
            ! call betcolork2(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,months*years,fin_st-ini_st+1,months*years,0.,33.4,r3(0),g3(0),b3(0))
            ! call betcolork2(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,months*years,fin_st-ini_st+1,months*years,34.6,100.,r3(49),g3(49),b3(49))
            ! do n = 1, 48
            !     call betcolork2(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,months*years,fin_st-ini_st+1,months*years,33.4+real(n-1)/40.,33.4+real(n)/40.,r3(n),g3(n),b3(n))
            ! end do
            ! do n = 1,13
            !     if(n<=4) then
            !         call rgbk(0.7,0.,0.7)
            !     else if(n<=7) then
            !         call rgbk(0.,0.7,0.)
            !     else if (n<=10) then
            !         call rgbk(0.7,0.7,0.)
            !     else if (n<=13) then
            !         call rgbk(0.7,0.,0.)
            !     else
            !     end if
            !     call pscont3(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,180,fin_st-ini_st+1,180,1,33.4+real((n-1)/10.),0.)
            ! end do

    call rgbk(0.,0.,0.)
    call plot(dx*real(fin_st-ini_st+1)+2.*dx,-height,-3)

    !temp
    call colorscale_creator2(3.*width-1.5,0.,24,r1,g1,b1,0.,12.,1,0.3,1,10.,.5,90,1,1)
    !sigma
    ! call colorscale_creator(45,r2,g2,b2,23.,27.5,5,0.3,1,10.,.5,90,1,1)
    !sal
    ! call colorscale_creator(48,r3,g3,b3,33.4,34.6,4,0.3,2,10.,.5,90,1,1)

    ! Station 4 examination
    st = 6-3 !wakarizuraine trust me bro
    call plot(0.5,0.,-3)
    call create_box(width,height,3)
    call mod12_memori3(0.,0.,months*years,0.15,-90,height,1,2)
    call num_memori2(width,0.,0.,12.,12,5,0.4,1,-width,0,0,0)
    call symbolc(width/2.,height+0.4,0.5,'st4',0.,len('st4'))
    call plot(0.,height,-3)
    call floating_lines(-(width+2.*dx+0.5),-dy/2.,2.*width+2.*dx+0.5,0.,15,3,0.,-dy*12.)
    
    ! all data points
    ! inserting means
    ! psarray2(st,12+6) = avpro(6,1,6,201)
    ! psarray2(st,24+2) = avpro(2,1,6,201)
    ! psarray2(st,108+10) = avpro(10,1,6,201)
    ! psarray2(st,120+2) = avpro(2,1,6,201)
    ! psarray2(st,168+2) = avpro(2,1,6,201)
    ! print*,psarray2

    ! linear interpolation for years 2010 and onwards excluding 2012 of course
    do i = 12+1,180
        if(i>=12*3+1.and.i<=12*4+1)cycle
        if(psarray2(st,i)==0.) then
            interpolated(i) = 'TRUE'
            if(psarray2(st,i-1)/=0. .and. psarray2(st,i+1)/=0.) then
                psarray2(st,i) = (psarray2(st,i-1)+psarray2(st,i+1))/2.
            else if (psarray2(st,i-1)/=0. .and. psarray2(st,i+1)==0.) then
                psarray2(st,i) = psarray2(st,i-1)+(psarray2(st,i+2)-psarray2(st,i-1))/3.
            end if
        end if
    end do
        
    ! every single data point 
    do i = 1,180
        call gmark_ratio(psarray2(st,i),0.,12.,width,plotx(i))
        ! print*,psarray2(st,i),plotx(i)
        ploty(i) = -dy/2. - dy*real(i-1)
        if(interpolated(i)=='TRUE') then
            call rgbk(1.,0.,0.)
        else;end if
        call gmark(width-plotx(i),ploty(i),0.1,1)      
        if(i>1) then
            if(psarray2(st,i-1)/=0. .and. psarray2(st,i)/=0.) then
            call plot(width-plotx(i-1),ploty(i-1),3);call plot(width-plotx(i),ploty(i),2)
            end if
        else;end if
            call rgbk(0.,0.,0.)  
    end do
    ! total means
        call avsemloop_1D(psarray2(st,13:180),168,168,1,mean_1D=totalol,s_1D=slol)
        totalmean = totalol(1)
        print*,totalmean
        call rgbk(0.,0.,1.)
        call newpen2(4)
        call gmark_ratio(totalmean,0.,12.,width,rnum)
        call gmark_ratio(slol(1),0.,12.,width,plot_s)
        call plot(width-rnum,-dy/2.-12.*dy,3);call plot(width-rnum,-dy/2.-12.*dy*real(years),2)
        call rgbk(0.,0.,0.)
        call plot(width-rnum-plot_s,-12.5*dy/2.,3);call plot(width-rnum-plot_s,-dy/2.-12.*dy*real(years),2)
        call plot(width-rnum+plot_s,-12.5*dy/2.,3);call plot(width-rnum+plot_s,-dy/2.-12.*dy*real(years),2)
    !
    ! yearly means
        call avsemloop_1D(psarray2(st,1:180),180,12,15,mean_1D=testy)
        ! print*,testy,size(testy)
        do y = 1, years
            call gmark_ratio(testy(y),0.,12.,width,rnum)
            call plot(width-rnum,-dy/2.-12.*dy*real(y-1),3);call plot(width-rnum,-dy/2.-12.*dy*real(y),2)
        end do

    ! monthly means
    do m = 1, months
        count=0
        do y = 1, years
            i = m+(y-1)*months
            if(psarray2(st,i)/=0.) then;count = count +1;endif
        end do
        psdata_m(st,m) = count
        if(count == 0) then;psmean_m(st,m) = 0.;else;
        psmean_m(st,m) = sum(psarray2(st,m:months*years:months))/real(psdata_m(st,m))
        end if
        ! print*,psdata_m(st,m),psmean_m(st,m)
    end do
    
    ! call avsemdata_2D(psarray2(st,1:180),)

    call plot(width+4.,0.,-3)
    call create_box(width2,-height2,3)
    call mod12_memori3(0.,-height2,13,0.4,0,width2,0,2,dxval)
    call num_memori2(0.,-height2,0.,12.,12,5,0.4,1,height2,-90,0,0)
    ! do n = 1, 13
    !     if(n == 13) then;m = 1;else;m = n;endif
    !     call gmark_ratio(psmean_m(st,m),0.,12.,height2,rnum)
    !     call gmark_ratio(sempro(m,1,6,201),0.,12.,height2,plotsem)
    !     ! print*,plotsem
    !     call gmark(dxval/2.+dxval*real(n-1),-height2+rnum,0.1,1)
    !     call plot(dxval/2.+dxval*real(n-1),-height2+rnum-plotsem,3);call plot(dxval/2.+dxval*real(n-1),-height2+rnum+plotsem,2)
    !     if(m == 2 .or. m==6 .or. m==10) then
    !         call rgbk(1.,0.,0.)
    !     end if
    !     call numberc(dxval/2.+dxval*real(n-1),0.3,0.3,real(psdata_m(st,m)),0.,-1)
    !     call rgbk(0.,0.,0.)
    ! end do

    !examining 2011,2014,2017,2021,2023 appears to have two peaks
    call plot(0.,-2.*height2-2.,-3)
    call create_box(width2,height2,3)
    call mod12_memori3(0.,0.,13,0.4,0,width2,0,2)
    call num_memori2(0.,0.,0.,12.,12,5,0.4,1,height2,-90,0,0)
    call gmark_ratio(totalmean,0.,12.,height2,rnum)
    call gmark_ratio(s,0.,12.,height2,plot_s)
    call rgbk(0.,0.,1.)
    call plot(0.,rnum,3);call plot(width2,rnum,2)
    call rgbk(0.,0.,0.)
    call plot(0.,rnum-plot_s,3);call plot(width2,rnum-plot_s,2)
    call plot(0.,rnum+plot_s,3);call plot(width2,rnum+plot_s,2)
    ! call symbolc(width2/2.,height2+0.4,0.5,'2011,2014,2017,2021,2023',0.,len('2011,2014,2017,2021,2023'))
  
    do i = 1, size(two_peaks)
        if(i==1) then;call rgbk(1.,0.,0.);else
        call brightcolors(r,g,b);call rgbk(r(2*i),g(2*i),b(2*i))
        end if
        n = two_peaks(i)
        do j = 1, 13
            if(j == 13) then;m = 1;else;m = j;endif
            call gmark_ratio(potemp_c5(n,m,1,6,201),0.,12.,height2,ploty(m))
            call gmark(dxval/2.+dxval*real(j-1),ploty(m),0.1,1)
            if(m>1) then
                if(potemp_c5(n,m-1,1,6,201)/=0. .and. potemp_c5(n,m,1,6,201)/=0.) then
                call plot(dxval/2.+dxval*real(j-2),ploty(m-1),3);call plot(dxval/2.+dxval*real(m-1),ploty(m),2)
                end if
            else;end if
        end do
        call numberc(2.*dxval+2.*dxval*real(i-1),height2+0.3,0.3,real(n+2008),0.,-1)
    end do

    call rgbk(0.,0.,0.)
    call plot(0.,-height2-2.,-3)
    call create_box(width2,height2,3)
    call mod12_memori3(0.,0.,13,0.4,0,width2,0,2)
    call num_memori2(0.,0.,0.,12.,12,5,0.4,1,height2,-90,0,0)
    call rgbk(0.,0.,1.)
    call plot(0.,rnum,3);call plot(width2,rnum,2)
    call rgbk(0.,0.,0.)
    call plot(0.,rnum-plot_s,3);call plot(width2,rnum-plot_s,2)
    call plot(0.,rnum+plot_s,3);call plot(width2,rnum+plot_s,2)

  
    do i = 1, size(one_peak)
        if(i==1) then;call rgbk(1.,0.,0.);else
        call brightcolors(r,g,b);call rgbk(r(2*i-1),g(2*i),b(2*i+1))
        end if
        n = one_peak(i)
        do j = 1, 13
            if(j == 13) then;m = 1;else;m = j;endif
            call gmark_ratio(potemp_c5(n,m,1,6,201),0.,12.,height2,ploty(m))
            call gmark(dxval/2.+dxval*real(j-1),ploty(m),0.1,1)
            if(m>1) then
                if(potemp_c5(n,m-1,1,6,201)/=0. .and. potemp_c5(n,m,1,6,201)/=0.) then
                call plot(dxval/2.+dxval*real(j-2),ploty(m-1),3);call plot(dxval/2.+dxval*real(m-1),ploty(m),2)
                end if
            else;end if
        end do
        call numberc(2.*dxval+2.*dxval*real(i-1),height2+0.3,0.3,real(n+2008),0.,-1)
    end do
    call plot(width+0.5,-height2-2.,-3)
    call create_map(39,42,137,142,4,4,1,3.5,0.3)



    call plote
end program