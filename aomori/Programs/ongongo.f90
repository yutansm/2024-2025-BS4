program ongongo
    implicit none
    real,dimension(0:61)::r,g,b
    integer,parameter::years = 15, months = 12, lines = 2, stations = 9, depth = 400,width = 4.,height=10.
    real,dimension(years,months,lines,stations,depth)::geovel
    real,dimension(stations,depth)::psarray=0.
    integer,dimension(stations,depth)::mask
    integer::st,d,n
    real::dx,dy

    call plots(3.,10.5,9,'../Plots/Geostrophy/colorscale.ps')
    ! call colorscale_creator(40,r,g,b,-20.,60.,10,0.5,1,10.,.5,90,1,1)
    call b2r_colorgrad(60,20,r,g,b)
    call geovel_array(51,geovel)
    psarray(1:stations,1:400)=geovel(1,6,1,1:stations,1:400)
    ! print*,psarray
    do st = 1, stations
        do d = 1, 400
            if(psarray(st,d) == 0.) then;mask(st,d) = 0;else;mask(st,d) = 1;endif
        end do
    end do
    dx = width/5.;dy = -1.*height/depth

    do n = 1, 60
        call betcolork2(dx,dy,psarray,mask,5,9,1,400,9,400,-40.+real(2*(n-1)),-40.+real(2*n),r(n),g(n),b(n))
    end do
    ! do n = 1, 60
    !     call pscolork(dx,dy,psarray,mask,1,5,1,400,9,400,-40.+real(2*(n-1)),-40.+real(2*n),r(n),g(n),b(n))
    ! end do
    call plote
end program

