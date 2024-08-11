program geostrophic_velocity
    USE IFPORT
    implicit none
    intrinsic::sin,cos,tan,asin,acos
    real,parameter::omega = 7.2921*(10.**(-5.)),width = 3.,height = 6.
    integer,parameter::years = 15, months =12, lines = 2, stations = 9, depth = 400
    integer,parameter::iterations = 15, midpoint = 10
    real,dimension(years,months,lines,stations,depth)::potemp_c5,sal_c5,sigma_5
    ! real,dimension(months,lines,stations,depth)::sigma_sd,sigma_dataquan,sigma_av
    double precision::geo_av(months,lines,stations,depth);double precision::geo_sd(months,lines,stations,depth)
    double precision::integral_D(years,months,lines,stations,depth);double precision::hiyou(years,months,lines,stations,depth)
    double precision::delta_D(years,months,lines,stations,depth);double precision::geo_v(years,months,lines,stations,depth);double precision::geo_v400(years,months,lines,stations,depth)
    double precision::geomass_array(years,months,lines)
    double precision::minusone;double precision::diff;double precision::first_sum_p;double precision::first_sum_d;doubleprecision::geomass
    double precision::gravity;double precision::thousand;double precision::zero;double precision::one;double precision::numvault
    double precision::integral_P(months,lines,stations,depth);real,dimension(stations,depth)::ps_array
    real,dimension(iterations+2)::r,g,b
    integer,dimension(stations,depth)::mask
    integer,dimension(months,lines,stations,depth)::geo_dataquan
    real::delta_x,pi,f,dx,dy
    integer::y,m,l,st,d,n,realst,year
    character(len=4),dimension(12)::month_names
    character::filename*999,aaaa*9,mm*9,directory*999,line*9
    LOGICAL(4) result
    dx = width/5.; dy = -height/400.; minusone = -1.0d0; gravity = 9.8d0; thousand = 1000.0d0; zero = 0.0d0; one = 1.0d0
    ! call avsd_dataquan(sigma_5,sigma_av,sigma_sd,sigma_dataquan)
    ! stations are located 41N and 137.3333+(station_num -1)*1/3 E
    pi = 2.*asin(1.)
    delta_x = 2.*pi*6378.*1000.*cos(41.*pi/180.)/360.*1./3.  !dimension = m
    f = 2.*omega*sin(41.*pi/180.)
    print*, delta_x
    102 format(9(f9.4))

    ! call calibrated_data25(potemp_c5,sal_c5)                     !25db median filtered data
    call calibrated_data51(potemp_c5,sal_c5)
    call create_sigma_array(potemp_c5,sal_c5,sigma_5)

! do y =1,1
!     do m = 8,8
!         do l = 1,1
!             do st = 1,stations
!                 do d = 1,400
!                     if (sigma_5(y,m,l,st,d)/=0.) then
!                         hiyou(y,m,l,st,d) = 1./((thousand + sigma_5(y,m,l,st,d))) !m^3/kg
!                     else; hiyou = 0.
!                     end if
!                     ! print*, hiyou(y,m,l,st,d)
!                         integral_D(y,m,l,st,d) = first_sum_d + hiyou(y,m,l,st,d)*(10.**(4.))! d corresponds to roughly one meter worth of pressure(=1db = 10**4 pascals)
!                         first_sum_d = integral_D(y,m,l,st,d)
!                         ! print*,integral_P(y,m,l,st,d)-integral_P(y,m,l,st,d-1)
!                         ! print*,hiyou(y,m,l,st,d)*(integral_P(y,m,l,st,d)-integral_P(y,m,l,st,d-1))
!                         print*,'2009,8',st,d,integral_D(y,m,l,st,d)
!                 end do
!                 ! print*,'2023,8',st,d,integral_D(y,m,l,st,d)
!                 first_sum_p = zero
!                 first_sum_d = zero
!             end do
!         end do
!     end do
! end do


! do y = 5,5
!     write(aaaa,'(i4.4)')y+2008
!     do m = 5,5
!         write(mm,'(i2.2)')m
!         do l = 2,lines
!             if(l==1) then;line = 'N-Line';else; line='S-Line';end if
!             do st = 4,stations-1
!                 ! print*,st,st+1
!                 do d = 1,depth
!                     if (integral_D(y,m,l,st,d)/=0. .and. integral_D(y,m,l,st+1,d)/=0.) then
!                         delta_D(y,m,l,st+1,d) = (integral_D(y,m,l,st+1,d) - integral_D(y,m,l,st,d))   !st4,とst5のdelta_Dはdelta_D配列の5番目に入る
!                     else;delta_D(y,m,l,st+1,d) = 0.   !ある深度（等圧面と仮定している）におけるダイナミックデプスの差                                        
!                     end if
!                     ! print*,delta_D(y,m,l,st+1,d)
!                     geo_v(y,m,l,st+1,d) = -delta_D(y,m,l,st+1,d)/(f*delta_x)*100. !0m を無流面と仮定した場合のgeostrophic velocity cm/s presumably
!                 end do
!                 diff = geo_v(y,m,l,st+1,400)*minusone
!                 do d = 1,depth
!                     if(geo_v(y,m,l,st+1,d)/=zero) then
!                     geo_v400(y,m,l,st+1,d) = (geo_v(y,m,l,st+1,d) + diff)
!                     ! print*,diff
!                     else;geo_v400(y,m,l,st+1,d) = 0.
!                     end if
!                     print*,y+2008,m,st+1,geo_v400(y,m,l,st+1,d)
!                 end do !end of depth
!             end do !end of st
!             filename = '/LARGE0/gr10291/nishimori2/aomori/Geostrophy/'//trim(line)//'/'//trim(mm)//'/Velocity_25median'//trim(aaaa)//'.csv'
!             open(21,file = filename,status = 'replace')
!             do d = 1,depth
!                 write(21,102)0.,0.,0.,0.,(geo_v400(y,m,l,st,d),st = 5,9) !ややこしいけどあってる　ステーションの話
!             end do
!             close (21)
            
!         end do
!     end do
! end do



! do y = 5,5
!     write(aaaa,'(i4.4)')y+2008
!     do l = 2,lines
!         if(l==1) then;line = 'N-Line';else; line='S-Line';end if
!     do m = 5,5
!         write(mm,'(i2.2)')m
!         numvault = zero
!             do st = 5,stations !st+1 がややこしいのでこうした
!                 if(all(geo_v400(y,m,l,st,1:400)==0.)) then
!                     geomass = 0.;exit
!                 else
!                     do d = 1,depth                     
!                         geomass = numvault + geo_v400(y,m,l,st,d)*0.01*delta_x*(10.**(-6.))  !v(cm/s)*0.01(m/s)*1(m)*delta_x(m)*10^-6(sv)
!                         numvault = geomass
!                         ! print*,geomass
!                     end do
!                     ! print*,m,st,geomass
!                 end if
!             end do
!                 geomass_array(y,m,l) = geomass
!                 ! print*,y+2008,m,geomass
!                 filename = '/LARGE0/gr10291/nishimori2/aomori/Geostrophy/'//trim(line)//'/'//trim(mm)//'/Transport_25Median'//trim(aaaa)//'.csv'
!                 open(31,file = filename,status = 'replace')
            
!                     write(31,102)(geomass_array(y,m,l)) 
            
!                 close (31)
!         end do
!     end do
! end do








! geostrophic velocity of every single fucking year
! call geovel_array(51,geo_v400)
! call bgr_colorgrad(iterations,8,r,g,b)
! call month_str_array(month_names)
! l = 1
! do y = 15,years
!     year = y + 2008;write(aaaa,'(i4.4)') year
!     filename = '/LARGE0/gr10291/nishimori2/aomori/Geostrophy/graphs/'//'51_median/'//trim(aaaa)//'.ps'
!     call plots(2.,16.5,13,filename)
!     call symbol(3.,2.,0.8,'Geostrophic Velocity (NLine) '//trim(aaaa),0.,len('geostrophic velocity (nline) aaaa'))
!     do m = 1, months
!         do st = 4,stations-1
!             realst = st+1
!             do d = 1, depth
!                 ps_array(realst,d) = geo_v400(y,m,l,st+1,d)
!                 ! print*,geo_sd(m,l,realst,d)
!                 if(ps_array(realst,d) /= 0.) then
!                     mask(realst,d) = 1
!                 else;mask(realst,d) = 0
!                 end if
!             end do !end of d
!         end do !end of st
!         call pscolork(dx,dy,ps_array,mask,5,9,1,400,9,400,-100.,-14.,r(0),g(0),b(0))
!         call pscolork(dx,dy,ps_array,mask,5,9,1,400,9,400,36.,100.,r(iterations+1),g(iterations+1),b(iterations+1))
!             do n = 1, iterations
!                 call pscolork(dx,dy,ps_array,mask,5,9,1,400,9,400,-14.+2.*real(n-1),-14.+2.*real(n),r(n),g(n),b(n))
!             end do
!             call pscont3(dx,dy,ps_array,mask,5,9,1,400,9,400,15,-14.,4.)
!             call psframe(4,9,400,3.,6.,0.08)
!             call symbolc(1.5,0.4,0.5,month_names(m),0.,4)
!             if(mod(m,6) /= 0) then
!                 call plot(4.,0.,-3)
!             else;call plot(-4.*5.,-8.,-3)
!             end if
!     end do
!     call plot(4.,0.,-3)
!     call gmark(0.,1.,0.1,1);call gmark(0.,1.,0.3,4);call gmark(15.,1.,0.3,5);call gmark(15.,1.,0.3,4)
!     call colorscale_creator(iterations,r,g,b,-14.,36.,1,0.2,1,15.,0.5,0,1,1)
! end do

! call plots(2.,16.5,13,'/LARGE0/gr10291/nishimori2/aomori/Monthly_color/geostrophic_velocity_av_pt2.ps')
! call symbol(2.5,2.,0.8,'Monthly Averages of Geostrophic Velocity (NLine)',0.,len('monthly averages of geostrophic velocity (nline)'))
! call dp_avsd_dataquan(geo_v400,geo_av,geo_sd,geo_dataquan)
! call bgr_colorgrad(iterations,4,r,g,b)
! call month_str_array(month_names)
! l = 1
! do m = 1, months
!     print*, m
!     do st = 4,stations-1
!         print*, st,st+1
!         realst = st+1
!         do d = 1, depth
!             ps_array(realst,d) = geo_av(m,l,realst,d)
!             print*,geo_av(m,l,realst,d)
!             if(ps_array(realst,d) /= 0.) then
!                 mask(realst,d) = 1
!             else;mask(realst,d) = 0
!             end if
!         end do !end of d
!     end do !end of st
!     call pscolork(dx,dy,ps_array,mask,5,9,1,400,9,400,-100.,-6.,r(0),g(0),b(0))
!     call pscolork(dx,dy,ps_array,mask,5,9,1,400,9,400,24.,100.,r(iterations+1),g(iterations+1),b(iterations+1))
!         do n = 1, iterations
!             call pscolork(dx,dy,ps_array,mask,5,9,1,400,9,400,-6.+2.*real(n-1),-6.+2.*real(n),r(n),g(n),b(n))
!         end do
!         call pscont3(dx,dy,ps_array,mask,5,9,1,400,9,400,8,-6.,4.)
!         call psframe(4,9,400,3.,6.,0.08)
!         call symbolc(1.5,0.4,0.5,month_names(m),0.,4)
!         if(mod(m,6) /= 0) then
!             call plot(4.,0.,-3)
!         else;call plot(-4.*5.,-8.,-3)
!         end if
! end do

! call plot(4.,0.,-3)
! call gmark(0.,1.,0.1,1);call gmark(0.,1.,0.3,4);call gmark(15.,1.,0.3,5);call gmark(15.,1.,0.3,4)
! call colorscale_creator(iterations,r,g,b,-6.,24.,1,0.2,1,15.,0.5,0,1,1)

! ! SD
! call plots(2.,16.5,13,'/LARGE0/gr10291/nishimori2/aomori/Monthly_color/geostrophic_velocity_SD.ps')
! call symbol(2.5,2.,0.8,'Monthly Standard Deviations of Geostrophic Velocity (NLine)',0.,len('monthly standard deviations of geostrophic velocity (nline)'))
! call dp_avsd_dataquan(geo_v400,geo_av,geo_sd,geo_dataquan)
! call br_colorgrad(iterations,midpoint,r,g,b)
! call month_str_array(month_names)
! l = 1
! do m = 1, months
!     do st = 4,stations-1
!         realst = st+1
!         do d = 1, depth
!             ps_array(realst,d) = geo_sd(m,l,realst,d)
!             ! print*,geo_sd(m,l,realst,d)
!             if(ps_array(realst,d) /= 0.) then
!                 mask(realst,d) = 1
!             else;mask(realst,d) = 0
!             end if
!         end do !end of d
!     end do !end of st
!     call pscolork(dx,dy,ps_array,mask,5,9,1,400,9,400,-100.,-6.,r(0),g(0),b(0))
!     call pscolork(dx,dy,ps_array,mask,5,9,1,400,9,400,24.,100.,r(iterations+1),g(iterations+1),b(iterations+1))
!         do n = 1, iterations
!             call pscolork(dx,dy,ps_array,mask,5,9,1,400,9,400,-6.+2.*real(n-1),-6.+2.*real(n),r(n),g(n),b(n))
!         end do
!         call pscont3(dx,dy,ps_array,mask,5,9,1,400,9,400,10,-6.,2.)
!         call psframe(4,9,400,3.,6.,0.08)
!         call symbolc(1.5,0.4,0.5,month_names(m),0.,4)
!         if(mod(m,6) /= 0) then
!             call plot(4.,0.,-3)
!         else;call plot(-4.*5.,-8.,-3)
!         end if
! end do

! call plot(4.,0.,-3)
! call gmark(0.,1.,0.1,1);call gmark(0.,1.,0.3,4);call gmark(15.,1.,0.3,5);call gmark(15.,1.,0.3,4)
! call colorscale_creator(iterations,r,g,b,-6.,24.,2,0.2,1,15.,0.5,0,1,1)

call plote
end program