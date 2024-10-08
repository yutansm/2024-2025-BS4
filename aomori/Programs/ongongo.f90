program tssigma201
  use constants
  use subroutines
  implicit none
  real,parameter::width = 2., height = 24.,width2 = 8.,height2 = 6.,width3 = 2.,height3=3.8
  integer,parameter::ini_st = 4, fin_st = 9, obs_depth = 201
  real,dimension(fin_st-ini_st+1,months*years)::psarray2=0.
  integer,dimension(fin_st-ini_st+1,months*years)::mask2
  real,dimension(years)::testy;real,dimension(1)::totalol,slol
  real,dimension(0:180)::plotx=0.,ploty=0.
  character(len=5),dimension(180)::interpolated = 'false'
  real::rnum,dxval,plotsem,s,plot_s,totalmean; integer::count1
  integer,dimension(6)::array
  real,dimension(2,15,12,fin_st-ini_st+1,depth)::pstemp !2 = above below
  real,dimension(2,15,12,fin_st-ini_st+1,depth)::pssal
  real,dimension(2,15,12,fin_st-ini_st+1,depth)::psden
  real,dimension(2,15,12,fin_st-ini_st,depth)::psvel
  integer,dimension(12,12)::log=0
  real,dimension(:,:,:),allocatable::mmean
  real,dimension(:,:),allocatable::ymmeanabove,ymmeanbelow
  real,dimension(:,:),allocatable::ymsemabove,ymsembelow
  integer,dimension(:,:),allocatable::ymdataabove,ymdatabelow
  real,dimension(:),allocatable::rtemp,gtemp,btemp,rsal,gsal,bsal,rsigma,gsigma,bsigma,rvel,gvel,bvel
  integer,dimension(6,depth)::intresult
  real,dimension(6,depth)::rresult



  call plots(2.5,0.5,9,'../Plots/TempSalSigma/Timeseries/timeseries_potempc5_obs201Re.ps')
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
  call colorscale(24,r1,g1,b1,0.,12.,5,0.4,1,10.,.5,lessthan=1,morethan=1,rangle=90.,x=width+1.5)
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
  call plot(width+1.5,-height2,-3)

  dxval = width2/13.
    !above
  do i = 1, 15
    do m=1,12
      if(potemp_c5(i,m,1,6,201)>=totalmean+s)then
        pstemp(1,i,m,1:6,1:depth) = potemp_c5(i,m,1,4:9,1:depth)
        pssal(1,i,m,1:6,1:depth) = sal_c5(i,m,1,4:9,1:depth)
        psden(1,i,m,1:6,1:depth) = sigma_c5(i,m,1,4:9,1:depth)
        psvel(1,i,m,1:5,1:depth) = geovel_5(i,m,1,5:9,1:depth)
      else;pstemp(1,i,m,1:6,1:depth)=0.0
          pssal(1,i,m,1:6,1:depth)=0.0
          psden(1,i,m,1:6,1:depth)=0.0
          psvel(1,i,m,1:5,1:depth)=0.0
      end if
    end do
  end do
      
  !below 
  do i = 1,15
    do m = 1, 12
      if (totalmean-s<potemp_c5(i,m,1,6,201).and.potemp_c5(i,m,1,6,201)<totalmean)then
        pstemp(2,i,m,1:6,1:depth) = potemp_c5(i,m,1,4:9,1:depth)
        pssal(2,i,m,1:6,1:depth) = sal_c5(i,m,1,4:9,1:depth)
        psden(2,i,m,1:6,1:depth) = sigma_c5(i,m,1,4:9,1:depth)
        psvel(2,i,m,1:5,1:depth) = geovel_5(i,m,1,5:9,1:depth)
      else;pstemp(2,i,m,1:6,1:depth)=0.0
          pssal(2,i,m,1:6,1:depth)=0.0
          psden(2,i,m,1:6,1:depth)=0.0
          psvel(2,i,m,1:5,1:depth)=0.0
      end if     
    end do
      ! print*,pstemp(4,i,1:months,st,201)
  end do
  call plot(1.5,0.,-3)
  call rgbk(0.,0.,0.);call newpen2(3)
  call plot(0.,height2/2.,3);call plot(width2,height2/2.,2);call plot(0.,0.,3);call plot(0.,height2,2)
  call num_memori(0.,12.,12,5,0.3,1,-height2/2.,-90,y=height2/2.);call num_memori(0.,12.,12,5,0.3,1,height2/2.,-90,y=height2/2.)
  call floating_numbers(1.,1.,12,0.3,dxval,0.,0.,-1,x=dxval,y = -0.4);call floating_numbers(1.,1.,12,0.3,dxval,0.,0.,-1,x=dxval,y = height2+0.2)
  call plot(0.,height2/2.,-3);call gmark_ratio(totalmean,0.,12.,height2/2.,rnum);call gmark_ratio(s,0.,12.,height2/2.,plot_s)
  call newpen2(3);call rgbk(0.,0.,1.);call plot(0.,rnum,3);call plot(width2,rnum,2);call plot(0.,-rnum,3);call plot(width2,-rnum,2)
  call rgbk(0.,0.,0.);call newpen2(-2)
  call plot(0.,rnum-plot_s,3);call plot(width2,rnum-plot_s,2);call plot(0.,rnum+plot_s,3);call plot(width2,rnum+plot_s,2)
  call plot(0.,-rnum-plot_s,3);call plot(width2,-rnum-plot_s,2);call plot(0.,-rnum+plot_s,3);call plot(width2,-rnum+plot_s,2)
  !above
  h=0
    do i = 1,15
        do m = 1,12
                ! do m = 1,12
                    if(pstemp(1,i,m,3,201)==0.)cycle
                    do z = 1,12
                        if(real(z-1)<=pstemp(1,i,m,3,201).and.pstemp(1,i,m,3,201)<real(z))then
                            dy = real(z-1)*height2/24.
                            count1=log(m,z)
                            dx = real(count1)*dxval/10.
                            ! print*,y+2008,m,(y-1)*12+m,h,count1
                            log(m,z)=log(m,z)+1
                            h = h+1;exit
                        else;end if
                    end do
                    call betsqk(dxval/2.+dxval*real(m-1)+dx,dy,dxval/2.+dxval*real(m-1)+dx+dxval/10.,dy+height2/24.,0.,0.,0.);call gmark(dxval/2.+dxval*real(m-1)+dx,dy,0.01,2);call gmark(dxval/2.+dxval*real(m-1)+dx+dxval/10.,dy+height2/24.,0.01,2)
        end do
    end do

  !below
  h = 1;log=0
    do i = 1,15
            do m = 1,12
                ! do m = 1,12
                    if(pstemp(2,i,m,3,201)==0.)cycle
                    do z = 1,12
                        if(real(z-1)<=pstemp(2,i,m,3,201).and.pstemp(2,i,m,3,201)<real(z))then
                            dy = -1.*real(z-1)*height2/24.
                            count1=log(m,z)
                            dx = real(count1)*dxval/10.
                            ! print*,y+2008,m,(y-1)*12+m,h,count1
                            log(m,z)=log(m,z)+1
                            h = h+1;exit
                        else;end if
                    end do
                call betsqk(dxval/2.+dxval*real(m-1)+dx,dy,dxval/2.+dxval*real(m-1)+dx+dxval/10.,dy+height2/24.,0.,0.,0.);call gmark(dxval/2.+dxval*real(m-1)+dx,dy,0.01,2);call gmark(dxval/2.+dxval*real(m-1)+dx+dxval/10.,dy+height2/24.,0.01,2)
        end do
    end do


  call floating_lines(height2,90.,13,2,dxval,0.,x=dxval/2.,y=-height2/2.);call floating_lines(width2,0.,5,2,0.,height2/24.*5.,y=height2/24.*2-height2/2.)

  !above sigma
      call plot(0.,-height3-1.,-3)
      call symbolc(width3/2.,0.3,0.4,'Potemp',0.,len('Potemp'))
      call num_memori(0.,400.,40,10,0.3,-1,-height3,-90)
      call avsemdata_4D(pstemp(1,1:15,1:months,1:6,1:depth),15,months,6,depth,'dim2',mmean)
      call avsemdata_3D(mmean(1:15,1:6,1:depth),15,6,depth,'dim1',ymmeanabove)
      call butler_psk(ymmeanabove,6,depth,width3,-height3,0.,0.,16.,.5,'b2r',32,bpt1=16,contquan = 9,conti=0.,continc=2.,r=rtemp,g=gtemp,b=btemp)
      print*,maxval(ymmeanabove),minval(ymmeanabove)
      call plot(width3+.5,0.,-3)
      call symbolc(width3/2.,0.3,0.4,'Sal',0.,len('Sal'))
      call memori(40,0.1,10,-height3,-90.,y=-height3)
      call avsemdata_4D(pssal(1,1:15,1:months,1:6,1:depth),15,months,6,depth,'dim2',mmean)
      call avsemdata_3D(mmean(1:15,1:6,1:depth),15,6,depth,'dim1',ymmeanabove)
      call butler_psk(ymmeanabove,6,depth,width3,-height3,0.,33.80,34.20,0.01,'b2r',40,bpt1=20,contquan = 9,conti=33.8,continc=0.05,r=rsal,g=gsal,b=bsal)
      print*,maxval(ymmeanabove),minval(ymmeanabove)
      call plot(width3+.5,0.,-3)
      call symbolc(width3/2.,0.3,0.4,'Sigma',0.,len('Density'))
      call memori(40,0.1,10,-height3,-90.,y=-height3)
      call avsemdata_4D(psden(1,1:15,1:months,1:6,1:depth),15,months,6,depth,'dim2',mmean)
      call avsemdata_3D(mmean(1:15,1:6,1:depth),15,6,depth,'dim1',ymmeanabove)
      call butler_psk(ymmeanabove,6,depth,width3,-height3,0.,24.8,27.4,0.1,'b2r',26,bpt1=12,contquan = 14,conti=24.8,continc=0.2,r=rsigma,g=gsigma,b=bsigma)
      print*,maxval(ymmeanabove),minval(ymmeanabove)
      call plot(width3+.5,0.,-3)
      call symbolc(width3/2.,0.3,0.4,'Velocity',0.,len('Velocity'))
      call memori(40,0.1,10,-height3,-90.,y=-height3)
      call avsemdata_4D(psvel(1,1:15,1:months,1:5,1:depth),15,months,5,depth,'dim2',mmean)
      call avsemdata_3D(mmean(1:15,1:5,1:depth),15,5,depth,'dim1',mean_2D=ymmeanabove,sem_2D=ymsemabove,dataquan_2D=ymdataabove)
      call butler_psk(ymmeanabove,5,depth,width3,-height3,0.,-10.,40.,2.,'b2r',25,bpt1=5,contquan = 18,conti=-2.,continc=2.,r=rvel,g=gvel,b=bvel)
      print*,maxval(ymmeanabove),minval(ymmeanabove)
  !
  ! below sigma
      call plot(-3.*(width3+.5),-height3-.3,-3)
      call memori(40,0.1,10,-height3,-90.,y=-height3)
      call num_memori(0.,400.,40,10,0.3,-1,-height3,-90)
      call st_memori(1,6,width3,1,0.4,0,y=-height3)
      call avsemdata_4D(pstemp(2,1:15,1:months,1:6,1:depth),15,months,6,depth,'dim2',mmean)
      call avsemdata_3D(mmean(1:15,1:6,1:depth),15,6,depth,'dim1',ymmeanbelow)
      call butler_psk(ymmeanbelow,6,depth,width3,-height3,0.,0.,16.,.5,'b2r',32,bpt1=16,contquan = 9,conti=0.,continc=2.)
      print*,maxval(ymmeanbelow),minval(ymmeanbelow)
      call plot(width3+.5,0.,-3)
      call memori(40,0.1,10,-height3,-90.,y=-height3)
      call st_memori(1,6,width3,1,0.4,0,y=-height3)
      call avsemdata_4D(pssal(2,1:15,1:months,1:6,1:depth),15,months,6,depth,'dim2',mmean)
      call avsemdata_3D(mmean(1:15,1:6,1:depth),15,6,depth,'dim1',ymmeanbelow)
      call butler_psk(ymmeanbelow,6,depth,width3,-height3,0.,33.80,34.20,0.01,'b2r',40,bpt1=20,contquan = 9,conti=33.8,continc=0.05)
      print*,maxval(ymmeanbelow),minval(ymmeanbelow)
      call plot(width3+.5,0.,-3)
      call memori(40,0.1,10,-height3,-90.,y=-height3)
      call st_memori(1,6,width3,1,0.4,0,y=-height3)
      call avsemdata_4D(psden(2,1:15,1:months,1:6,1:depth),15,months,6,depth,'dim2',mmean)
      call avsemdata_3D(mmean(1:15,1:6,1:depth),15,6,depth,'dim1',ymmeanbelow)
      call butler_psk(ymmeanbelow,6,depth,width3,-height3,0.,24.8,27.4,0.1,'b2r',26,bpt1=12,contquan = 14,conti=24.8,continc=0.2)
      print*,maxval(ymmeanbelow),minval(ymmeanbelow)
      call plot(width3+.5,0.,-3)
      call memori(40,0.1,10,-height3,-90.,y=-height3)
      call st_memori(1,6,width3,1,0.4,0,y=-height3)
      call avsemdata_4D(psvel(2,1:15,1:months,1:5,1:depth),15,months,5,depth,'dim2',mmean)
      call avsemdata_3D(mmean(1:15,1:5,1:depth),15,5,depth,'dim1',mean_2D=ymmeanbelow,sem_2D=ymsembelow,dataquan_2D=ymdatabelow)
      call butler_psk(ymmeanbelow,5,depth,width3,-height3,0.,-10.,40.,2.,'b2r',25,bpt1=5,contquan = 18,conti=-2.,continc=2.)
      print*,maxval(ymmeanbelow),minval(ymmeanbelow)

      ! !ymmean,sem,dataquan arrays have data of velocities in them as of now
      ! call newpage
      call plot(-3.*(width3+0.5)+width3/2.,-2.*height3-height2+1.,-3)
      call colorscale(32,rtemp,gtemp,btemp,0.,16.,10,0.5,1,7.,0.3,lessthan=1,morethan=1,rangle=90.)
      call plot(width3+0.5,0.,-3)
      call colorscale(40,rsal,gsal,bsal,33.8,34.20,10,0.4,2,7.,0.3,lessthan=1,morethan=1,rangle=90.)
      call plot(width3+0.5,0.,-3)
      call colorscale(26,rsigma,gsigma,bsigma,24.8,27.4,5,0.5,1,7.,0.3,lessthan=1,morethan=1,rangle=90.,symbol_start=2)
      call plot(width3+0.5,0.,-3)
      call colorscale(25,rvel,gvel,bvel,-10.,40.,5,0.4,1,7.,0.3,lessthan=1,morethan=1,rangle=90.)

      ! !t test for velocity, comparison between below and above sigma
      ! do n = 1,5 !stations 5-9 in program words
      !     do i = 1,depth
      !         call welchttest(ymmeanabove(n,i),ymsemabove(n,i),ymdataabove(n,i),ymmeanbelow(n,i),ymsembelow(n,i),ymdatabelow(n,i),intresult(n,i))
      !         rresult(n,i) = real(intresult(n,i))
      !         ! print*,rresult(n,i)
      !         if(intresult(n,i)/=0) print*,intresult(n,i),n,i,ymmeanabove(n,i),ymsemabove(n,i),ymdataabove(n,i),ymmeanbelow(n,i),ymsembelow(n,i),ymdatabelow(n,i)
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