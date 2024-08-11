program geostrophic_velocity
    implicit none
    intrinsic::sin,cos,tan,asin,acos
    real,parameter::omega = 7.2921*(10.**(-5.)),width = 3.,height = 6.
    integer,parameter::years = 15, months =12, lines = 2, stations = 9, depth = 400, gravity = 9.8
    integer,parameter::iterations = 20, midpoint = 10
    real,dimension(years,months,lines,stations,depth)::potemp_c5,sal_c5
    real,dimension(months,lines,stations,depth)::sigma_sd,sigma_dataquan
    double precision::sigma_5(years,months,lines,stations,depth)
    double precision::sigma_av(months,lines,stations,depth)
    double precision::integral_D(months,lines,stations,depth);double precision::hiyou(months,lines,stations,depth)
    double precision::delta_D(months,lines,stations,depth);double precision::geo_v(months,lines,stations,depth);double precision::geo_v400(months,lines,stations,depth)
    double precision::a
    double precision::diff
    double precision::integral_P(months,lines,stations,depth);real,dimension(stations,depth)::ps_array
    real,dimension(iterations+2)::r,g,b
    integer,dimension(stations,depth)::mask
    real::delta_x,pi,first_sum_p,first_sum_d,f,dx,dy,singled,singlev
    integer::y,m,l,st,d,n,realst
    character(len=4),dimension(12)::month_names
    dx = width/5.
    dy = -height/400.
    a = -1.0d0

    call calibrated_data(potemp_c5,sal_c5)
    call create_sigma_array(potemp_c5,sal_c5,sigma_5)
    call avsd_dataquan(sigma_5,sigma_av,sigma_sd,sigma_dataquan)
    ! sigma_av(3,1,8,2:30) = sigma_av(3,1,8,31);sigma_av(3,1,8,1) = 0.
    ! sigma_av(3,1,9,2:30) = sigma_av(3,1,9,31);sigma_av(3,1,9,1) = 0.
    integral_P(1:months,1:lines,1:stations,0) = 0.
    ! print*,sigma_av(3,1,8,1:400)
    ! stations are located 41N and 137.3333+(station_num -1)*1/3 E
    pi = 2.*asin(1.)
    delta_x = 2.*pi*6378.*cos(41.*pi/180.)/180.*1./3.  !dimension = km
    print*,omega
    print*, delta_x ,'delta_x'
    f = 2.*omega*sin(41.*pi/180.)
    print*,f ,'f'
    print*,f*delta_x ,'f times delta_x'
call plots(1.,19.,13,'testd0_geov400.ps')
! call symbolr(0.,0.,0.4,'delta_x',0.,len('delta_x'));call number(0.1,0.,0.4,delta_x,0.,5)
! call symbolr(0.,-0.5,0.4,'f*delta_x',0.,len('f*delta_x'));call number(0.1,-0.5,0.4,f*delta_x,0.,5)


do m = 1, months
    do l = 1,1
        do st = 4,9
            do d = 2,400
                integral_P(m,l,st,d) = first_sum_p + (1000.+sigma_av(m,l,st,d))*gravity
                first_sum_p = integral_P(m,l,st,d)
                if (sigma_av(m,l,st,d)/=0.) then
                    hiyou(m,l,st,d) = 1./(sigma_av(m,l,st,d)+1000.)
                else; hiyou = 0.
                end if
                    integral_D(m,l,st,d) = first_sum_d + hiyou(m,l,st,d)*(integral_P(m,l,st,d)-integral_P(m,l,st,d-1)) !dp is pascals unit?
                    first_sum_d = integral_D(m,l,st,d)
            end do
            ! print*,integral_D(m,l,st,31:131)
            first_sum_p = 0.
            first_sum_d = 0.
        end do
    end do
end do



!delta_d の計算に入る
! print*, 'VELOCITY'
print*,'DELTA_D'
do m = 1, months
    do l = 1,1
        do st = 4,stations-1
            call symbolc(0.,-0.5,0.2,'d',0.,len('d'))
            call symbolc(.75,-0.5,0.2,'v',0.,len('v'))
            call rgbk(1.,0.,0.);call numberc(0.,0.,0.3,real(m),0.,-1);call rgbk(0.,0.,1.);call numberc(.75,0.,0.3,real(st+1),0.,-1);call rgbk(0.,0.,0.)
            do d = 1,depth
                if (integral_D(m,l,st,d)/=0. .and. integral_D(m,l,st+1,d)/=0.) then
                    delta_D(m,l,st+1,d) = (integral_D(m,l,st+1,d) - integral_D(m,l,st,d))   !st4,とst5のdelta_Dはdelta_D配列の5番目に入る
                else;delta_D(m,l,st+1,d) = 0.                                               
                end if
                geo_v(m,l,st+1,d) = delta_D(m,l,st+1,d)/(f*delta_x)*100.
            end do
            diff = geo_v(m,l,st+1,400)*a
            do d = 1,depth
                if(geo_v(m,l,st+1,d)/=0.) then
                geo_v400(m,l,st+1,d) = geo_v(m,l,st+1,d) + diff
                else;geo_v400(m,l,st+1,d) = 0.
                end if
                if(d ==1 .or. mod(d,20)==0) then
                    singled = delta_D(m,l,st+1,d);singlev = geo_v400(m,l,st+1,d)
                    call numberc(0.,-1.,0.15,delta_D(m,l,st+1,d),0.,5)
                    call numberc(.75,-1.,0.15,geo_v400(m,l,st+1,d),0.,5)
                    call plot(0.,-0.2,-3)
                    else;end if
            end do
            call plot(1.5,0.2*21.,-3)
            ! print*, delta_D(m,l,st+1,400),m,l,st+1
            ! print*,'geov below'
            ! print*,geo_v(m,l,st+1,400),m,l,st+1
        end do
    end do
    if(mod(m,3) == 0) then
        call plot(-22.5,-5.5,-3)
    else;end if
end do
print*,geo_v400(3,1,9,1:400)


end program