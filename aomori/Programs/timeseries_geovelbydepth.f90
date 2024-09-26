program examining_2011
    implicit none
    integer,parameter::years = 15, months = 12, lines = 2, stations = 9, depth = 400
    real,parameter::width = 2., height = 24.
    integer,parameter::ini_st = 4, fin_st = 9, l = 1, obs_depth = 201
    real,dimension(years,months,lines,stations,depth)::geovel
    real,dimension(months*years)::psarray=0.
    real,dimension(fin_st-ini_st,months*years)::psarray2=0.
    integer,dimension(months*years)::mask
    integer,dimension(fin_st-ini_st,months*years)::mask2
    real,dimension(0:61)::r1=0.,g1=0.,b1=0.
    integer::y,m,n,i,st,j
    real::dx,dy,r,g,b

    call plots(3.,0.5,9,'../Plots/Geostrophy/timeseries_vel.ps')
    call symbol(0.,height+1.4,0.8,'time series of geostrophic velocity',0.,len('time series of geostrophic velocity'))
    call b2g2y2r_colorgrad(60,20,r1,g1,b1)
    call geovel_array(51,geovel)
    print*,maxval(geovel),minval(geovel)
    ! print*,geovel(1,9,1,9,1:50)

    do j = 1,obs_depth,50
        call create_box(width,height,3)
        call numberr(width/2.,height+0.4,0.5,real(j),0.,-1);call symbol(width/2.,height+0.4,0.5,'m',0.,len('m'))
        call mod12_memori2(12*15,0.15,-90,height,1)
        call num_memori(6.,1.,5,1,0.5,-1,width,0,0,0)
        call plot(0.,height,-3)
        dx = width/real(fin_st-ini_st);dy = -height/(15.*12.+1.)
        call plot(0.,dy/2.,-3)
        do y = 1, years
            if(j==1) then
                call newpen2(3)
            call number(-2.,dy*real(6+(y-1)*12),0.6,real(y+2008),0.,-1)
            call plot(0.,dy*real(12*y),3);call plot(5.*width+6.*dx,dy*real(12*y),2)
            end if
            ! call plot(0.,dy*real(12*y),3);call plot(width,dy*real(12*y),2)
        end do

        do st = 1,fin_st-ini_st
            i = 1
            do y = 1, years
                do m = 1, months
                    psarray(i) = geovel(y,m,l,st+ini_st,j)
                    psarray2(st,i) = geovel(y,m,l,st+ini_st,j)
                    if(psarray(i) == 0.) then;mask(i) = 0;else;mask(i) = 1;endif
                        if(psarray2(st,i) == 0.) then;mask2(st,i) = 0;else;mask2(st,i) = 1;endif
                    i = i + 1
                    ! print*,y,m,st,psarray(i)
                end do 
            end do
            ! print*,r1
                    call betcolork(0.,dx,dy,psarray,months*years,mask,-100.,-40.,r1(0),g1(0),b1(0))
                    call betcolork(0.,dx,dy,psarray,months*years,mask,80.,100.,r1(61),g1(61),b1(61))
                    ! print*,psarray
                    do n = 1, 60
                    !     r = r1(n);g = g1(n);b = b1(n)
                    ! call betcolork(-dx,dx,dy,psarray,months*years,mask,-20.+real(2*(n-1)),-20.+real(2*n),r,g,b)
                    call betcolork(0.,dx,dy,psarray,months*years,mask,-40.+real(2*(n-1)),-40.+real(2*n),r1(n),g1(n),b1(n))
                    end do

                call plot(dx,0.,-3)
                ! call gmark(0.,0.,0.5,1)
        end do
   
        call plot(-dx*real(fin_st-ini_st),0.,-3)
        do n = 1,12
            if(n<=2) then
                call rgbk(0.,0.,0.7)
            else if(n<=4) then
                call rgbk(0.,0.7,0.)
            else if (n<=8) then
                call rgbk(0.8,0.7,0.)
            else if (n<=12) then
                call rgbk(0.7,0.,0.)
            else
            end if
            call pscont3(dx,dy,psarray2,mask2,1,fin_st-ini_st,1,180,fin_st-ini_st,180,1,-40.+real(n)*10.,0.)
        end do
        call rgbk(0.,0.,0.)
        call plot(dx*real(fin_st-ini_st),0.,-3)

        call plot(1.5*dx,-(height+dy/2.),-3)

    end do
    ! call gmark(0.,0.,0.5,1)
    call plot(1.,0.,-3)
    call colorscale_creator(60,r1,g1,b1,-40.,80.,5,0.5,1,10.,.5,90,1,1)
    call plot(0.,14.,-3)
    call create_map(39,42,137,142,1,6,1,3.,0.3)

    call plote
end program