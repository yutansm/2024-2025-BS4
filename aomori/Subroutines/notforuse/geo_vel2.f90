program geostrophic_velocity
    implicit none
    intrinsic::sin,cos,tan,asin,acos
    real,parameter::omega = 7.2921*(10.**(-5.)),width = 3.,height = 6.
    integer,parameter::years = 15, months =12, lines = 2, stations = 9, depth = 400
    integer,parameter::iterations = 15, midpoint = 4
    real,dimension(years,months,lines,stations,depth)::potemp_c5,sal_c5,sigma_5
    real,dimension(months,lines,stations,depth)::sigma_sd,sigma_dataquan,sigma_av
    ! double precision::sigma_5(years,months,lines,stations,depth)
    ! double presicion::sigma_av(months,lines,stations,depth)
    double precision::integral_D(months,lines,stations,depth);double precision::hiyou(months,lines,stations,depth)
    double precision::delta_D(months,lines,stations,depth);double precision::geo_v(months,lines,stations,depth);double precision::geo_v400(months,lines,stations,depth)
    double precision::a
    double precision::diff
    double precision::first_sum_p
    double precision::first_sum_d
    double precision::gravity
    double precision::thousand
    double precision::zero;double precision::one
    double precision::integral_P(months,lines,stations,depth);real,dimension(stations,depth)::ps_array
    real,dimension(iterations+2)::r,g,b
    integer,dimension(stations,depth)::mask
    real::delta_x,pi,f,dx,dy
    integer::y,m,l,st,d,n,realst
    character(len=4),dimension(12)::month_names
    dx = width/5.
    dy = -height/400.
    minusone = -1.0d0
    gravity = 9.8d0
    thousand = 1000.0d0
    zero = 0.0d0; one = 1.0d0
    call calibrated_data(potemp_c5,sal_c5)
    call create_sigma_array(potemp_c5,sal_c5,sigma_5)
    call avsd_dataquan(sigma_5,sigma_av,sigma_sd,sigma_dataquan)
    ! sigma_av(3,1,8,2:30) = sigma_av(3,1,8,31);sigma_av(3,1,8,1) = 0.
    ! sigma_av(3,1,9,2:30) = sigma_av(3,1,9,31);sigma_av(3,1,9,1) = 0.
    ! print*,sigma_av(3,1,8,1:400)
    ! stations are located 41N and 137.3333+(station_num -1)*1/3 E
    pi = 2.*asin(1.)
    delta_x = 2.*pi*6378.*1000.*cos(41.*pi/180.)/360.*1./3.  !dimension = m
    f = 2.*omega*sin(41.*pi/180.)
call plots(2.,16.5,13,'/LARGE0/gr10291/nishimori2/aomori/Monthly_color/geostrophic_velocity_fucked.ps')
call symbol(4.,2.,0.8,'Monthly Geostrophic Velocity (NLine)',0.,len('monthly geostrophic velocity (nline)'))

! print*,a
! print*,gravity
! print*,thousand
! print*,zero
! print*,one
print*,f,delta_x,f*delta_x
do m = 1, months
    do l = 1,1
        do st = 4,stations
            do d = 2,400
                ! if(sigma_av(m,l,st,d)/=0.) then
                ! integral_P(m,l,st,d) = first_sum_p + (thousand+sigma_av(m,l,st,d))*gravity*(10.**(-4.)) !10**-4 order dbars unit ~= meters
                ! first_sum_p = integral_P(m,l,st,d)
                ! else;end if
                ! print*,integral_P(m,l,st,d)
                ! print*,thousand+sigma_av(m,l,st,d)
                if (sigma_av(m,l,st,d)/=0.) then
                    hiyou(m,l,st,d) = 1./((thousand + sigma_av(m,l,st,d))) !m^3/kg^3
                else; hiyou = 0.
                end if
                ! print*, hiyou(m,l,st,d)
                    integral_D(m,l,st,d) = first_sum_d + hiyou(m,l,st,d)*(10.**(4.))! d corresponds to roughly one meter worth of pressure(=1db = 10**4 pascals)
                    first_sum_d = integral_D(m,l,st,d)
                    ! print*,integral_P(m,l,st,d)-integral_P(m,l,st,d-1)
                    ! print*,hiyou(m,l,st,d)*(integral_P(m,l,st,d)-integral_P(m,l,st,d-1))
                    ! print*,integral_D(m,l,st,d)
            end do
            ! print*,integral_D(m,l,st,d)
            first_sum_p = zero
            first_sum_d = zero
        end do
    end do
end do

do m = 1,months
    print*,m
    do l = 1,1
        do st = 4,stations-1
            print*,st+1
            do d = 1,depth
                if (integral_D(m,l,st,d)/=0. .and. integral_D(m,l,st+1,d)/=0.) then
                    delta_D(m,l,st+1,d) = (integral_D(m,l,st+1,d) - integral_D(m,l,st,d))   !st4,とst5のdelta_Dはdelta_D配列の5番目に入る
                else;delta_D(m,l,st+1,d) = 0.   !ある深度（等圧面と仮定している）におけるダイナミックデプスの差                                        
                end if
                ! print*,delta_D(m,l,st+1,d)
                geo_v(m,l,st+1,d) = -delta_D(m,l,st+1,d)/(f*delta_x)*100. !0m を無流面と仮定した場合のgeostrophic velocity cm/s presumably
            end do
            diff = geo_v(m,l,st+1,400)*minusone
            do d = 1,depth
                if(geo_v(m,l,st+1,d)/=zero) then
                geo_v400(m,l,st+1,d) = (geo_v(m,l,st+1,d) + diff)
                ! print*,diff
                else;geo_v400(m,l,st+1,d) = 0.
                end if
                print*,geo_v400(m,l,st+1,d)
            end do
        end do
    end do
end do
! print*,delta_D(3,1,4,30:400),m,l,st
! print*,delta_D(3,1,5,30:400),m,l,st
! print*,delta_D(3,1,7,30:400),m,l,st
! print*,geo_v400(3,1,6,1:400)

! start adding colors to geo_v400 array fucking finally 
call bgr_colorgrad(iterations,midpoint,r,g,b)
call month_str_array(month_names)
l = 1
do m = 1, months
    do st = 4,stations-1
        realst = st+1
        do d = 1, depth
            ps_array(realst,d) = geo_v400(m,l,realst,d)
            ! if(m == 3) then;print*,ps_array(6,1:400);else;end if
            if(ps_array(realst,d) /= 0.) then
                mask(realst,d) = 1
            else;mask(realst,d) = 0
            end if
        end do !end of d
    end do !end of st
    call pscolork(dx,dy,ps_array,mask,5,9,1,400,9,400,-100.,-6.,r(0),g(0),b(0))
    call pscolork(dx,dy,ps_array,mask,5,9,1,400,9,400,24.,100.,r(iterations+1),g(iterations+1),b(iterations+1))
        do n = 1, iterations
            call pscolork(dx,dy,ps_array,mask,5,9,1,400,9,400,-6.+2.*real(n-1),-6.+2.*real(n),r(n),g(n),b(n))
        end do
        call pscont3(dx,dy,ps_array,mask,5,9,1,400,9,400,14,-6.,4.)
        call psframe(4,9,400,3.,6.,0.08)
        call symbolc(1.5,0.4,0.5,month_names(m),0.,4)
        if(mod(m,6) /= 0) then
            call plot(4.,0.,-3)
        else;call plot(-4.*5.,-8.,-3)
        end if
end do

call plot(4.,0.,-3)
call gmark(0.,1.,0.1,1);call gmark(0.,1.,0.3,4);call gmark(15.,1.,0.3,5);call gmark(15.,1.,0.3,4)
call colorscale_creator(iterations,r,g,b,-6.,24.,1,0.2,1,15.,0.5,0,1,1)

end program