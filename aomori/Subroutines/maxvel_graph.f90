program maximum_velocity_graph
    implicit none
    integer,parameter::years = 15, months = 12, lines = 2,stations = 9,depth = 400
    real,parameter:: width1 =18., height1 = 12.,width2 = 4.,height2 = 3.
    real,dimension(years,months,lines,stations,depth)::geovel_5
    real,dimension(years,months,lines)::geovel_max
    real,dimension(months,lines)::av_array,sd_array
    integer,dimension(months,lines)::quan_array
    integer,dimension(2)::location
    integer::y,m,l,n1,n2
    real::dx1,dx2,av,sem,plot95,maximum_velocity,random,s1,s2,df
    real,dimension(30)::t95


    ! call plots(4.,3.,13,'/LARGE0/gr10291/nishimori2/aomori/Geostrophy/graphs/maximum_velocity_25.ps')
    ! call symbolc(9.0,height1+2.,.8,'Monthly Maximum velocities at NLine (25db Median Filtered)',0.,len('Monthly maximum velocities at NLine (mmdb Median Filtered)'))
    ! dx1 = width1/13.;dx2 = width2/13.
    l = 1                                           !N-Line ONLY!!!!
!     call geovel_array(25,geovel_5)
!     do y = 1,years
!         do m = 1, months
!             maximum_velocity = maxval(geovel_5(y,m,l,1:stations,1:depth))
!             location = maxloc(geovel_5(y,m,l,1:stations,1:depth))
!             geovel_max(y,m,l) = maximum_velocity
!             ! print*,y+2008,m,maximum_velocity,location(1),location(2)
!             print*,geovel_max(y,m,l)
!         end do 
!     end do

            
!     call avsd_dataquan2(geovel_max,av_array,sd_array,quan_array)
!     call t95_value(t95)
!     call newpen2(3)
!     call create_box(width1,height1,3)
!     call num_memori(0.,80.,80,5,0.25,1,height1,-90,0,0)
!     call month_memori(0.3,width1)
!     call symbol(0.,height1+0.5,0.4,'n',0.,1)
!     ! print*,t95(1:30)
!     do m = 1, months
!         call numberc(real(m)*dx1,height1+0.5,0.3,real(quan_array(m,l)),0.,-1)
!         av = av_array(m,l)*height1/80.
!         if (quan_array(m,l)/=0.) then
!         sem = sd_array(m,l)/sqrt(real(quan_array(m,l)))*height1/80.
!         else;sem = 0.;end if
!         plot95 = t95(quan_array(m,l)-1)*sem
!         ! print*,av,sem,plot95
!         call gmark(real(m)*dx1,av,0.1,1)
!         call plot(real(m)*dx1,av-sem,3);call plot(real(m)*dx1,av+sem,2)
!         call plot(real(m)*dx1+0.1,av-plot95,3);call plot(real(m)*dx1+0.1,av+plot95,2)
!         call numberc(real(m)*dx1,av+plot95+0.5,0.3,av_array(m,l),0.,2)
!     end do

!     call newpage
!     call plot(-3.,9.,-3)
!     call symbolc(13.2,height2+2.,1.,'Monthly Maximum Velocities at NLine (2009-2023)',0.,len('monthly mean of Maximum Velocities at NLine (2009-2023)'))
!     do y = 1, 15
!         call create_box(width2,height2,2)
!         call num_memori(0.,80.,80,5,0.2,1,height2,-90,0,0)
!         call month_memori(0.15,width2)
!         call numberc(width2/2.,height2+0.5,0.4,real(y+2008),0.,-1)

!         do  m = 1, months
!             call gmark(real(m)*dx2,real(geovel_max(y,m,l))*height2/80.,0.1,1)
!             call numberc(real(m)*dx2,real(geovel_max(y,m,l))*height2/80.+0.2,0.15,real(geovel_max(y,m,l)),0.,1)
!         end do
!         if (mod(y,5)/=0) then
!             call plot(width2+1.,0.,-3)
!         else
!             call plot(-4.*(width2+1.),-(height2+2.),-3)
!         end if
! end do

! call potemp_T_S_depth(random,0.,34.,400.)
! print*, random
! call potemp_T_S_depth(random,0.,41.,400.)
! print*, random


call plote
end program