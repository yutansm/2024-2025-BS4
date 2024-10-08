program tssigma201
    use constants
    use subroutines
    implicit none
    real,parameter::width = 2., height = 24.,width2 = 5.,height2 = 2.5,width3 = 2.,height3=3.8
    integer,parameter::ini_st = 4, fin_st = 9, obs_depth = 201
    real,dimension(fin_st-ini_st+1,months*years)::psarray2=0.
    integer,dimension(fin_st-ini_st+1,months*years)::mask2
    real,dimension(years)::testy;  real,dimension(months)::testm,semm;real,dimension(1)::totalol,slol
    integer,dimension(months)::datam
    real,dimension(0:180)::plotx=0.,ploty=0.
    character(len=5),dimension(180)::interpolated = 'false'
    real::rnum,dxval,plotsem,s,plot_s,totalmean
    integer::count1
    integer,dimension(6)::two_peaks = (/3,6,8,9,13,15/)
    integer,dimension(4)::one_peak = (/5,7,12,14/)
    integer,dimension(3)::within_sigma = (/2,10,11/)
    integer,dimension(10)::above_sigma = (/3,5,6,7,8,9,12,13,14,15/)
    integer,dimension(6)::array
    real,dimension(4,10,12,fin_st-ini_st+1,depth)::pstemp !4=two,one,within,above;6=number of years
    real,dimension(4,10,12,fin_st-ini_st+1,depth)::pssal
    real,dimension(4,10,12,fin_st-ini_st+1,depth)::psden
    real,dimension(4,10,12,fin_st-ini_st,depth)::psvel
    integer,dimension(12,12)::log=0
    real,dimension(:,:,:),allocatable::mmean
    real,dimension(:,:),allocatable::ymmean2,ymmean1,ymmeanwithin,ymmeanabove
    real,dimension(:,:),allocatable::ymsem2,ymsem1,ymsemwithin,ymsemabove
    integer,dimension(:,:),allocatable::ymdata2,ymdata1,ymdatawithin,ymdataabove
    real,dimension(:),allocatable::rtemp,gtemp,btemp,rsal,gsal,bsal,rsigma,gsigma,bsigma,rvel,gvel,bvel
    integer,dimension(6,depth)::intresult
    real,dimension(6,depth)::rresult



    call plots(2.5,0.5,9,'../Plots/TempSalSigma/Timeseries/timeseries_potempc5_obs201R.ps')
    call symbol(0.,height+1.4,0.8,'time series of potemp',0.,len('time series of potemp'))
    ! call b2r_colorgrad(24,12,r1,g1,b1) !temp
    ! call b2cy2y2r_colorgrad(45,10,20,30,r2,g2,b2) !sigma
    ! call b2cy2y2r_colorgrad(48,12,24,36,r3,g3,b3) !sal
    call calibrated_data51(potemp_c5,sal_c5)
    call create_sigma_array(potemp_c5,sal_c5,sigma_c5)
    call geovel_array(51,geovel_5)

    ! do j = 1,obs_depth,50
        j = obs_depth; l = 1
        call create_box(width,height,3)
        call numberr(width/2.,height+0.4,0.5,real(j),0.,-1);call symbol(width/2.,height+0.4,0.5,'m',0.,len('m'))
        dx = width/real(fin_st-ini_st+1);dy = height/(real(months*years))
        call mod12_memori(months*years,0.15,-90,height,1,2)
        call st_memori(1,6,width,1,0.4,2)
        call floating_numbers(2023.,-1.,15,0.6,0.,dy*12.,0.,-1,-1.5,dy*6.)
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
            call butler_psbet(psarray2,fin_st-ini_st+1,months*years,width,-height,0.,0.,12.,0.5,'b2r',24,bpt1=12,contquan = 7,conti = 0., continc = 2.,r= r1,g=g1,b=b1)
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
    call plot(dx*real(fin_st-ini_st+1),-height,-3)

    !temp
    call colorscale_creator2(3.,0.,24,r1,g1,b1,0.,12.,1,0.3,1,6.,.3,90,1,1)
    !sigma
    ! call colorscale_creator(45,r2,g2,b2,23.,27.5,5,0.3,1,10.,.5,90,1,1)
    !sal
    ! call colorscale_creator(48,r3,g3,b3,33.4,34.6,4,0.3,2,10.,.5,90,1,1)

    ! Station 4 examination
    st = 6-3 !wakarizuraine trust me bro
    call plot(0.5,0.,-3)
    call create_box(width,height,3)
    call mod12_memori(months*years,0.15,-90,height,1,2)
    call num_memori(0.,12.,12,5,0.4,1,-width,0,x=width)
    call symbolc(width/2.,height+0.4,0.5,'st4',0.,len('st4'))
    call plot(0.,height,-3)
    call floating_lines(2.*width+2.*dx+0.5,0.,15,3,0.,-dy*12.,-(width+2.*dx+0.5),-dy/2.)
    
    ! all data points
        ! inserting means
        ! psarray2(st,12+6) = avpro(6,1,6,201)
        ! psarray2(st,24+2) = avpro(2,1,6,201)
        ! psarray2(st,108+10) = avpro(10,1,6,201)
        ! psarray2(st,120+2) = avpro(2,1,6,201)
        ! psarray2(st,168+2) = avpro(2,1,6,201)
        ! print*,psarray2
    !
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
    ! total mean
        call avsemloop_1D(psarray2(st,13:180),168,168,1,mean_1D=totalol,s_1D=slol)
        totalmean = totalol(1)
        s = slol(1)
        ! print*,totalmean
        call rgbk(0.,0.,1.)
        call newpen2(4)
        call gmark_ratio(totalmean,0.,12.,width,rnum)
        call gmark_ratio(s,0.,12.,width,plot_s)
        call plot(width-rnum,-dy/2.-12.*dy,3);call plot(width-rnum,-dy/2.-12.*dy*real(years),2)
        call rgbk(0.4,0.4,0.4);call newpen2(3);call newpen2(-2)
        call plot(width-rnum-plot_s,-12.*dy-dy/2.,3);call plot(width-rnum-plot_s,-dy/2.-12.*dy*real(years),2)
        call plot(width-rnum+plot_s,-12.*dy-dy/2.,3);call plot(width-rnum+plot_s,-dy/2.-12.*dy*real(years),2)
        call rgbk(0.,0.,0.)
    !   
    ! yearly means
        call avsemloop_1D(psarray2(st,1:180),180,12,15,mean_1D=testy)
        call newpen2(3);call newpen2(-6)
        do y = 1, years
            call gmark_ratio(testy(y),0.,12.,width,rnum)
            call plot(width-rnum,-dy/2.-12.*dy*real(y-1),3);call plot(width-rnum,-dy/2.-12.*dy*real(y),2)
        end do

    ! ! monthly means
    !     call avsemjump_1D(psarray2(st,1:180),180,12,15,mean_1D=testm,sem_1D=semm,dataquan_1D=datam)
    !     call plot(width+4.,0.,-3)
    !     call create_box(width2,-height2,3)
    !     call mod12_memori(13,0.4,0,width2,0,2,y=-height2,dxval=dxval)
    !     call num_memori(0.,12.,12,5,0.4,1,height2,-90,y=-height2)
    !     do n = 1, 13
    !         if(n == 13) then;m = 1;else;m = n;endif
    !         call gmark_ratio(testm(m),0.,12.,height2,rnum)
    !         call gmark_ratio(semm(m),0.,12.,height2,plotsem)
    !         ! print*,plotsem
    !         call gmark(dxval/2.+dxval*real(n-1),-height2+rnum,0.1,1)
    !         call plot(dxval/2.+dxval*real(n-1),-height2+rnum-plotsem,3);call plot(dxval/2.+dxval*real(n-1),-height2+rnum+plotsem,2)
    !         if(m == 2 .or. m==6 .or. m==10) then
    !             call rgbk(1.,0.,0.)
    !         end if
    !         call numberc(dxval/2.+dxval*real(n-1),0.3,0.3,real(datam(m)),0.,-1)
    !         call rgbk(0.,0.,0.)
    !     end do
    ! !
    call plot(width+1.5,-height2,-3)

    !examining 2011,2014,2017,2021,2023 appears to have two peaks
    ! call plot(0.,-2.*height2-2.,-3)
    call create_box(width2,height2,3)
    call mod12_memori(13,0.4,0,width2,0,2,dxval=dxval)
    call num_memori(0.,12.,12,5,0.4,1,height2,-90)
    call gmark_ratio(totalmean,0.,12.,height2,rnum)
    call gmark_ratio(s,0.,12.,height2,plot_s)
    call rgbk(0.,0.,1.)
    call plot(0.,rnum,3);call plot(width2,rnum,2)
    call rgbk(0.,0.,0.)
    call plot(0.,rnum-plot_s,3);call plot(width2,rnum-plot_s,2)
    call plot(0.,rnum+plot_s,3);call plot(width2,rnum+plot_s,2)
    ! call symbolc(width2/2.,height2+0.4,0.5,'2011,2014,2017,2021,2023',0.,len('2011,2014,2017,2021,2023'))
  
    call dozencolors(r,g,b)

    do i = 1, size(two_peaks)
        call rgbk(r(2*i),g(2*i),b(2*i))
        n = two_peaks(i)
        do j = 1, 13
            if(j == 13) then;m = 1;else;m = j;endif
            call gmark_ratio(psarray2(st,(n-1)*12+m),0.,12.,height2,ploty(m))
            call gmark(dxval/2.+dxval*real(j-1),ploty(m),0.1,1)
            if(m>1) then
                if(psarray2(st,(n-1)*12+m-1)/=0. .and. psarray2(st,(n-1)*12+m)/=0.) then
                call plot(dxval/2.+dxval*real(j-2),ploty(m-1),3);call plot(dxval/2.+dxval*real(j-1),ploty(m),2)
                end if
            else if(j ==13) then;call plot(dxval/2.+dxval*real(j-2),ploty(12),3);call plot(dxval/2.+dxval*real(j-1),ploty(1),2)
            else;end if
        end do
        call numberc(2.*dxval+2.*dxval*real(i-1),height2+0.3,0.3,real(n+2008),0.,-1)
    end do

    call rgbk(0.,0.,0.)
    call plot(width2+1.5,0.,-3)
    call create_box(width2,height2,3)
    call mod12_memori(13,0.4,0,width2,0,2)
    call num_memori(0.,12.,12,5,0.4,1,height2,-90)
    call rgbk(0.,0.,1.)
    call plot(0.,rnum,3);call plot(width2,rnum,2)
    call rgbk(0.,0.,0.)
    call plot(0.,rnum-plot_s,3);call plot(width2,rnum-plot_s,2)
    call plot(0.,rnum+plot_s,3);call plot(width2,rnum+plot_s,2)

  
    do i = 1, size(one_peak)
        call rgbk(r(2*i-1),g(2*i-1),b(2*i-1))
        n = one_peak(i)
        do j = 1, 13
            if(j == 13) then;m = 1;else;m = j;endif
            call gmark_ratio(psarray2(st,(n-1)*12+m),0.,12.,height2,ploty(m))
            call gmark(dxval/2.+dxval*real(j-1),ploty(m),0.1,1)
            if(m>1) then
                if(psarray2(st,(n-1)*12+m-1)/=0. .and. psarray2(st,(n-1)*12+m)/=0.) then
                call plot(dxval/2.+dxval*real(j-2),ploty(m-1),3);call plot(dxval/2.+dxval*real(j-1),ploty(m),2)
                end if
            else;end if
            if(j ==13.and.ploty(12)/=0..and.ploty(1)/=0.) then;call plot(dxval/2.+dxval*real(j-2),ploty(12),3);call plot(dxval/2.+dxval*real(j-1),ploty(1),2);end if

        end do
        call numberc(2.*dxval+2.*dxval*real(i-1),height2+0.3,0.3,real(n+2008),0.,-1)
    end do

    call rgbk(0.,0.,0.)
    call plot(-(width2+1.5),-height2-1.5,-3)
    call create_box(width2,height2,3)
    call mod12_memori(13,0.4,0,width2,0,2)
    call num_memori(0.,12.,12,5,0.4,1,height2,-90)
    call rgbk(0.,0.,1.)
    call plot(0.,rnum,3);call plot(width2,rnum,2)
    call rgbk(0.,0.,0.)
    call plot(0.,rnum-plot_s,3);call plot(width2,rnum-plot_s,2)
    call plot(0.,rnum+plot_s,3);call plot(width2,rnum+plot_s,2)

    ! years within +_sigma 2010,2018,2019
    do i = 1, size(within_sigma)
        call rgbk(r(12-i),g(12-i),b(12-i))
        n = within_sigma(i)
        do j = 1, 13
            if(j == 13) then;m = 1;else;m = j;endif
            call gmark_ratio(psarray2(st,(n-1)*12+m),0.,12.,height2,ploty(m))
            call gmark(dxval/2.+dxval*real(j-1),ploty(m),0.1,1)
            if(m>1) then
                if(psarray2(st,(n-1)*12+m-1)/=0. .and. psarray2(st,(n-1)*12+m)/=0.) then
                call plot(dxval/2.+dxval*real(j-2),ploty(m-1),3);call plot(dxval/2.+dxval*real(j-1),ploty(m),2)
                end if
            else;end if
            if(j ==13.and.ploty(12)/=0..and.ploty(1)/=0.) then;call plot(dxval/2.+dxval*real(j-2),ploty(12),3);call plot(dxval/2.+dxval*real(j-1),ploty(1),2);end if

        end do
        call numberc(2.*dxval+2.*dxval*real(i-1),height2+0.3,0.3,real(n+2008),0.,-1)
    end do

    call rgbk(0.,0.,0.)
    call plot(width2+1.5,0.,-3)
    call plot(0.,height2/2.,3);call plot(width2,height2/2.,2);call plot(0.,0.,3);call plot(0.,height2,2)
    call num_memori(0.,12.,12,5,0.3,1,-height2/2.,-90,y=height2/2.);call num_memori(0.,12.,12,5,0.3,1,height2/2.,-90,y=height2/2.)
    call floating_numbers(1.,1.,12,0.3,dxval,0.,0.,-1,x=dxval,y = -0.4);call floating_numbers(1.,1.,12,0.3,dxval,0.,0.,-1,x=dxval,y = height2+0.2)
    call plot(0.,height2/2.,-3)

    do n = 1,2
        if(n==1) then;j = size(two_peaks);array=two_peaks;else if(n==2) then;j = size(one_peak);array(1:size(one_peak))=one_peak;else;j = size(within_sigma);array(1:size(within_sigma))=within_sigma;endif
        do i = 1,j
            y = array(i)
                do m = 1,12
                    if(psarray2(st,(y-1)*12+m)==0.)cycle
                    do z = 1,12
                        if(real(z-1)<=psarray2(st,(y-1)*12+m).and.psarray2(st,(y-1)*12+m)<real(z))then
                            dy = real(z-1)*height2/24.
                            count1=log(m,z)
                            dx = real(count1)*dxval/7.
                            ! print*,y+2008,m,(y-1)*12+m,h,count1
                            log(m,z)=log(m,z)+1
                            h = h+1;exit
                        else;end if
                    end do
                    if(n==1) then;call betsqk(dxval/2.+dxval*real(m-1)+dx,dy,dxval/2.+dxval*real(m-1)+dx+dxval/7.,dy+height2/24.,r(2*i),g(2*i),b(2*i));call gmark(dxval/2.+dxval*real(m-1)+dx,dy,0.01,2);call gmark(dxval/2.+dxval*real(m-1)+dx+dxval/7.,dy+height2/24.,0.01,2)
                    else if(n==2) then;call betsqk(dxval/2.+dxval*real(m-1)+dx,dy,dxval/2.+dxval*real(m-1)+dx+dxval/7.,dy+height2/24.,r(2*i-1),g(2*i-1),b(2*i-1));call gmark(dxval/2.+dxval*real(m-1)+dx,dy,0.01,2);call gmark(dxval/2.+dxval*real(m-1)+dx+dxval/7.,dy+height2/24.,0.01,2)
                    end if
                end do
        end do
    end do

    h = 1;log=0
        j = size(within_sigma);array(1:size(within_sigma))=within_sigma
        do i = 1,j
            y = array(i)
                do m = 1,12
                    if(psarray2(st,(y-1)*12+m)==0.)cycle
                    do z = 1,12
                        if(real(z-1)<=psarray2(st,(y-1)*12+m).and.psarray2(st,(y-1)*12+m)<real(z))then
                            dy = -1.*real(z-1)*height2/24.
                            count1=log(m,z)
                            dx = real(count1)*dxval/7.
                            ! print*,y+2008,m,(y-1)*12+m,h,count1
                            log(m,z)=log(m,z)+1
                            h = h+1;exit
                        else;end if
                    end do
                    call betsqk(dxval/2.+dxval*real(m-1)+dx,dy,dxval/2.+dxval*real(m-1)+dx+dxval/7.,dy-height2/24.,r(12-i),g(12-i),b(12-i));call gmark(dxval/2.+dxval*real(m-1)+dx,dy,0.01,2);call gmark(dxval/2.+dxval*real(m-1)+dx+dxval/7.,dy-height2/24.,0.01,2)
                end do
        end do


    call floating_lines(height2,90.,13,2,dxval,0.,x=dxval/2.,y=-height2/2.);call floating_lines(width2,0.,5,2,0.,height2/24.*5.,y=height2/24.*2-height2/2.)
                            
 ! composite psgraph
    do i = 1, size(two_peaks)
        n = two_peaks(i)
        pstemp(1,i,1:months,1:6,1:depth) = potemp_c5(n,1:months,1,4:9,1:depth)
        pssal(1,i,1:months,1:6,1:depth) = sal_c5(n,1:months,1,4:9,1:depth)
        psden(1,i,1:months,1:6,1:depth) = sigma_c5(n,1:months,1,4:9,1:depth)
        psvel(1,i,1:months,1:5,1:depth) = geovel_5(n,1:months,1,5:9,1:depth)
        ! print*,pstemp(1,i,1:months,st,201)
    end do
    do i = 1, size(one_peak)
        n = one_peak(i)
        pstemp(2,i,1:months,1:6,1:depth) = potemp_c5(n,1:months,1,4:9,1:depth)
        pssal(2,i,1:months,1:6,1:depth) = sal_c5(n,1:months,1,4:9,1:depth)
        psden(2,i,1:months,1:6,1:depth) = sigma_c5(n,1:months,1,4:9,1:depth)
        psvel(2,i,1:months,1:5,1:depth) = geovel_5(n,1:months,1,5:9,1:depth)
        ! print*,pstemp(2,i,1:months,st,201)
    end do
    do i = 1, size(within_sigma)
        n = within_sigma(i)
        pstemp(3,i,1:months,1:6,1:depth) = potemp_c5(n,1:months,1,4:9,1:depth)
        pssal(3,i,1:months,1:6,1:depth) = sal_c5(n,1:months,1,4:9,1:depth)
        psden(3,i,1:months,1:6,1:depth) = sigma_c5(n,1:months,1,4:9,1:depth)
        psvel(3,i,1:months,1:5,1:depth) = geovel_5(n,1:months,1,5:9,1:depth)
        ! print*,pstemp(3,i,1:months,st,201)
    end do
    do i = 1,size(above_sigma)
        n = above_sigma(i)
        pstemp(4,i,1:months,1:6,1:depth) = potemp_c5(n,1:months,1,4:9,1:depth)
        pssal(4,i,1:months,1:6,1:depth) = sal_c5(n,1:months,1,4:9,1:depth)
        psden(4,i,1:months,1:6,1:depth) = sigma_c5(n,1:months,1,4:9,1:depth)
        psvel(4,i,1:months,1:5,1:depth) = geovel_5(n,1:months,1,5:9,1:depth)
        ! print*,pstemp(4,i,1:months,st,201)
    end do


    !two peaks
        call plot(-(width2+.5),-2.5,-3)
        call num_memori(0.,400.,40,10,0.3,-1,-height3,-90)
        call symbolc(width3/2.,0.3,0.4,'Potemp',0.,len('Potemp'))
        call avsemdata_4D(pstemp(1,1:6,1:months,1:6,1:depth),6,months,6,depth,'dim2',mmean)
        call avsemdata_3D(mmean(1:6,1:6,1:depth),6,6,depth,'dim1',mean_2D=ymmean2,sem_2D=ymsem2,dataquan_2D=ymdata2)
        call butler_psk(ymmean2,6,depth,width3,-height3,0.,0.,15.,.5,'b2r',30,bpt1=14,contquan = 8,conti=0.,continc=2.,r=rtemp,g=gtemp,b=btemp)
        print*,maxval(ymmean2),minval(ymmean2)
        call plot(width3+.5,0.,-3)
        call symbolc(width3/2.,0.3,0.4,'Sal',0.,len('Sal'))        
        call avsemdata_4D(pssal(1,1:6,1:months,1:6,1:depth),6,months,6,depth,'dim2',mmean)
        call avsemdata_3D(mmean(1:6,1:6,1:depth),6,6,depth,'dim1',mean_2D=ymmean2,sem_2D=ymsem2,dataquan_2D=ymdata2)
        call butler_psk(ymmean2,6,depth,width3,-height3,0.,33.80,34.15,0.01,'b2r',35,bpt1=20,contquan = 8,conti=33.8,continc=0.05,r=rsal,g=gsal,b=bsal)
        print*,maxval(ymmean2),minval(ymmean2)
        call plot(width3+.5,0.,-3)
        call symbolc(width3/2.,0.3,0.4,'Density',0.,len('Density'))  
        call avsemdata_4D(psden(1,1:6,1:months,1:6,1:depth),6,months,6,depth,'dim2',mmean)
        call avsemdata_3D(mmean(1:6,1:6,1:depth),6,6,depth,'dim1',mean_2D=ymmean2,sem_2D=ymsem2,dataquan_2D=ymdata2)
        call butler_psk(ymmean2,6,depth,width3,-height3,0.,25.0,27.4,0.1,'b2r',24,bpt1=10,contquan = 8,conti=25.0,continc=0.2,r=rsigma,g=gsigma,b=bsigma)
        print*,maxval(ymmean2),minval(ymmean2)
        call plot(width3+.5,0.,-3)
        call symbolc(width3/2.,0.3,0.4,'Vel',0.,len('Vel'))  
        call avsemdata_4D(psvel(1,1:6,1:months,1:5,1:depth),6,months,5,depth,'dim2',mmean)
        call avsemdata_3D(mmean(1:6,1:5,1:depth),6,5,depth,'dim1',mean_2D=ymmean2,sem_2D=ymsem2,dataquan_2D=ymdata2)
        call butler_psk(ymmean2,5,depth,width3,-height3,0.,-2.,24.,1.,'b2r',26,bpt1=2,contquan = 12,conti=-2.,continc=2.,r=rvel,g=gvel,b=bvel)
        print*,maxval(ymmean2),minval(ymmean2)
    !
    !one peak
        call plot(-3.*(width3+.5),-height3-.3,-3)
        call num_memori(0.,400.,40,10,0.3,-1,-height3,-90)
        ! call st_memori(1,6,width3,1,0.4,0,y=-height3)
        call avsemdata_4D(pstemp(2,1:4,1:months,1:6,1:depth),4,months,6,depth,'dim2',mmean)
        call avsemdata_3D(mmean(1:4,1:6,1:depth),4,6,depth,'dim1',ymmean1)
        call butler_psk(ymmean1,6,depth,width3,-height3,0.,0.,15.,.5,'b2r',30,bpt1=14,contquan = 8,conti=0.,continc=2.)
        print*,maxval(ymmean1),minval(ymmean1)
        call plot(width3+.5,0.,-3)
        ! call st_memori(1,6,width3,1,0.4,0,y=-height3)
        call avsemdata_4D(pssal(2,1:4,1:months,1:6,1:depth),4,months,6,depth,'dim2',mmean)
        call avsemdata_3D(mmean(1:4,1:6,1:depth),4,6,depth,'dim1',ymmean1)
        call butler_psk(ymmean1,6,depth,width3,-height3,0.,33.80,34.15,0.01,'b2r',35,bpt1=20,contquan = 8,conti=33.8,continc=0.05)
        print*,maxval(ymmean1),minval(ymmean1)
        call plot(width3+.5,0.,-3)
        ! call st_memori(1,6,width3,1,0.4,0,y=-height3)
        call avsemdata_4D(psden(2,1:4,1:months,1:6,1:depth),4,months,6,depth,'dim2',mmean)
        call avsemdata_3D(mmean(1:4,1:6,1:depth),4,6,depth,'dim1',ymmean1)
        call butler_psk(ymmean1,6,depth,width3,-height3,0.,25.0,27.4,0.1,'b2r',24,bpt1=10,contquan = 8,conti=25.0,continc=0.2)
        print*,maxval(ymmean1),minval(ymmean1)
        call plot(width3+.5,0.,-3)
        ! call st_memori(1,6,width3,1,0.4,0,y=-height3)
        call avsemdata_4D(psvel(2,1:4,1:months,1:5,1:depth),4,months,5,depth,'dim2',mmean)
        call avsemdata_3D(mmean(1:4,1:5,1:depth),4,5,depth,'dim1',mean_2D=ymmean1,sem_2D=ymsem1,dataquan_2D=ymdata1)
        call butler_psk(ymmean1,5,depth,width3,-height3,0.,-2.,24.,1.,'b2r',26,bpt1=2,contquan = 12,conti=-2.,continc=2.)
        print*,maxval(ymmean1),minval(ymmean1)
    !
    !above sigma
        call plot(-3.*(width3+.5),-height3-.3,-3)
        call num_memori(0.,400.,40,10,0.3,-1,-height3,-90)

        call avsemdata_4D(pstemp(4,1:10,1:months,1:6,1:depth),10,months,6,depth,'dim2',mmean)
        call avsemdata_3D(mmean(1:10,1:6,1:depth),10,6,depth,'dim1',ymmeanabove)
        call butler_psk(ymmeanabove,6,depth,width3,-height3,0.,0.,15.,.5,'b2r',30,bpt1=14,contquan = 8,conti=0.,continc=2.)
        print*,maxval(ymmeanabove),minval(ymmeanabove)
        call plot(width3+.5,0.,-3)
        call memori(40,0.1,10,-height3,-90.,y=-height3)
        call avsemdata_4D(pssal(4,1:10,1:months,1:6,1:depth),10,months,6,depth,'dim2',mmean)
        call avsemdata_3D(mmean(1:10,1:6,1:depth),10,6,depth,'dim1',ymmeanabove)
        call butler_psk(ymmeanabove,6,depth,width3,-height3,0.,33.80,34.15,0.01,'b2r',35,bpt1=20,contquan = 8,conti=33.8,continc=0.05)
        print*,maxval(ymmeanabove),minval(ymmeanabove)
        call plot(width3+.5,0.,-3)
        ! call st_memori(1,6,width3,1,0.4,0,y=-height3)
        call avsemdata_4D(psden(4,1:10,1:months,1:6,1:depth),10,months,6,depth,'dim2',mmean)
        call avsemdata_3D(mmean(1:10,1:6,1:depth),10,6,depth,'dim1',ymmeanabove)
        call butler_psk(ymmeanabove,6,depth,width3,-height3,0.,25.0,27.4,0.1,'b2r',24,bpt1=10,contquan = 8,conti=25.0,continc=0.2)
        print*,maxval(ymmeanabove),minval(ymmeanabove)
        call plot(width3+.5,0.,-3)
        ! call st_memori(1,6,width3,1,0.4,0,y=-height3)
        call avsemdata_4D(psvel(4,1:10,1:months,1:5,1:depth),10,months,5,depth,'dim2',mmean)
        call avsemdata_3D(mmean(1:10,1:5,1:depth),10,5,depth,'dim1',mean_2D=ymmeanabove,sem_2D=ymsemabove,dataquan_2D=ymdataabove)
        call butler_psk(ymmeanabove,5,depth,width3,-height3,0.,-2.,24.,1.,'b2r',26,bpt1=2,contquan = 12,conti=-2.,continc=2.)
        print*,maxval(ymmeanabove),minval(ymmeanabove)
    !
    ! within sigma
        call plot(-3.*(width3+.5),-height3-.3,-3)
        call num_memori(0.,400.,40,10,0.3,-1,-height3,-90)
        call st_memori(1,6,width3,1,0.4,0,y=-height3)
        call avsemdata_4D(pstemp(3,1:3,1:months,1:6,1:depth),3,months,6,depth,'dim2',mmean)
        call avsemdata_3D(mmean(1:3,1:6,1:depth),3,6,depth,'dim1',ymmeanwithin)
        call butler_psk(ymmeanwithin,6,depth,width3,-height3,0.,0.,15.,.5,'b2r',30,bpt1=14,contquan = 8,conti=0.,continc=2.)
        print*,maxval(ymmeanwithin),minval(ymmeanwithin)
        call plot(width3+.5,0.,-3)
        call st_memori(1,6,width3,1,0.4,0,y=-height3)
        call avsemdata_4D(pssal(4,1:3,1:months,1:6,1:depth),3,months,6,depth,'dim2',mmean)
        call avsemdata_3D(mmean(1:3,1:6,1:depth),3,6,depth,'dim1',ymmeanwithin)
        call butler_psk(ymmeanwithin,6,depth,width3,-height3,0.,33.80,34.15,0.01,'b2r',35,bpt1=20,contquan = 8,conti=33.8,continc=0.05)
        print*,maxval(ymmeanwithin),minval(ymmeanwithin)
        call plot(width3+.5,0.,-3)
        call st_memori(1,6,width3,1,0.4,0,y=-height3)
        call avsemdata_4D(psden(4,1:3,1:months,1:6,1:depth),3,months,6,depth,'dim2',mmean)
        call avsemdata_3D(mmean(1:3,1:6,1:depth),3,6,depth,'dim1',ymmeanwithin)
        call butler_psk(ymmeanwithin,6,depth,width3,-height3,0.,25.0,27.4,0.1,'b2r',24,bpt1=10,contquan = 8,conti=25.0,continc=0.2)
        print*,maxval(ymmeanwithin),minval(ymmeanwithin)
        call plot(width3+.5,0.,-3)
        call st_memori(1,6,width3,1,0.4,0,y=-height3)
        call avsemdata_4D(psvel(4,1:3,1:months,1:5,1:depth),3,months,5,depth,'dim2',mmean)
        call avsemdata_3D(mmean(1:3,1:5,1:depth),3,5,depth,'dim1',mean_2D=ymmeanwithin,sem_2D=ymsemwithin,dataquan_2D=ymdatawithin)
        call butler_psk(ymmeanwithin,5,depth,width3,-height3,0.,-2.,24.,1.,'b2r',26,bpt1=2,contquan = 12,conti=-2.,continc=2.)
        print*,maxval(ymmeanwithin),minval(ymmeanwithin)

        ! !ymmean,sem,dataquan arrays have data of velocities in them as of now
        call newpage
        call plot(2.5,15.5,-3)
        call colorscale_creator2(0.,0.,30,rtemp,gtemp,btemp,0.,15.,1,0.3,1,2.*height3,0.4,-90,1,1)
        call plot(2.,0.,-3)
        call colorscale_creator2(0.,0.,35,rsal,gsal,bsal,33.8,34.15,5,0.3,2,2.*height3,0.4,-90,1,1)
        call plot(2.,0.,-3)
        call colorscale_creator2(0.,0.,24,rsigma,gsigma,bsigma,25.0,27.4,2,0.3,1,2.*height3,0.4,-90,1,1)
        call plot(2.,0.,-3)
        call colorscale_creator2(0.,0.,26,rvel,gvel,bvel,-2.,24.,2,0.3,-1,2.*height3,0.4,-90,1,1)

        ! !t test for velocity, comparison between within and above sigma
        ! do n = 1,5 !stations 5-9 in program words
        !     do i = 1,depth
        !         call welchttest(ymmeanabove(n,i),ymsemabove(n,i),ymdataabove(n,i),ymmeanwithin(n,i),ymsemwithin(n,i),ymdatawithin(n,i),intresult(n,i))
        !         rresult(n,i) = real(intresult(n,i))
        !         ! print*,rresult(n,i)
        !         if(intresult(n,i)/=0) print*,intresult(n,i),n,i,ymmeanabove(n,i),ymsemabove(n,i),ymdataabove(n,i),ymmeanwithin(n,i),ymsemwithin(n,i),ymdatawithin(n,i)
        !     end do
        ! end do
        ! do n = 1,5 !stations 5-9 in program words
        !     do i = 1,depth
        !         call welchttest(ymmean2(n,i),ymsem2(n,i),ymdata2(n,i),ymmean1(n,i),ymsem1(n,i),ymdata1(n,i),intresult(n,i))
        !         rresult(n,i) = real(intresult(n,i))
        !         ! print*,rresult(n,i)
        !         if(intresult(n,i)/=0) print*,intresult(n,i),n,i,ymmean2(n,i),ymsem2(n,i),ymdata2(n,i),ymmean1(n,i),ymsem1(n,i),ymdata1(n,i)
        !     end do
        ! end do
        ! call butler_mask(rresult,5,depth,width3,-height3,-0.01,0.,0.,0.,0.)

    call plote
end program