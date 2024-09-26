program examining_2011
    implicit none
    integer,parameter::years = 15, months = 12, lines = 2, stations = 9, depth = 400
    real,parameter::width = 2., height = 24.
    integer,parameter::ini_st = 4, fin_st = 9, l = 1, obs_depth = 201
    real,dimension(years,months,lines,stations,depth)::potemp_c5,sal_c5,sigma_c5
    real,dimension(fin_st-ini_st+1,months*years)::psarray2=0.
    integer,dimension(fin_st-ini_st+1,months*years)::mask2
    real,dimension(0:101)::r1=0.,g1=0.,b1=0.,r2,g2,b2,r3,g3,b3
    integer::y,m,n,i,st,j
    real::dx,dy

    call plots(3.,0.5,9,'../Plots/Geostrophy/timeseries_salc5.ps')
    call symbol(0.,height+1.4,0.8,'time series of salinity',0.,len('time series of salinity'))
    call b2g2y2r_colorgrad2(26,5,r1,g1,b1) !temp
    call b2g2y2r_colorgrad(45,20,r2,g2,b2) !sigma
    call b2g2y2r_colorgrad(48,24,r3,g3,b3) !sal
    call calibrated_data51(potemp_c5,sal_c5)
    call create_sigma_array(potemp_c5,sal_c5,sigma_c5)
    ! print*,potemp_c5(1,1:12,1,5:9,1)
    ! print*,geovel(1,9,1,9,1:50)

    do j = 1,obs_depth,50
        call create_box(width,height,3)
        call numberr(width/2.,height+0.4,0.5,real(j),0.,-1);call symbol(width/2.,height+0.4,0.5,'m',0.,len('m'))
        dx = width/real(fin_st-ini_st+1);dy = height/(real(months*years))
        call mod12_memori3(0.,0.,months*years,0.15,-90,height,1,2)
        call st_memori(1,6,width,1,0.4,2)
        if(j==1) then
            call floating_numbers(-2.,dy*6.,2023.,-1.,15,0.6,0.,dy*12.,0.,-1)
        end if
        call plot(0.,height,-3)

        do st = 1,fin_st-ini_st+1
            i = 1
            do y = 1, years
                do m = 1, months
                    !temp
                    ! psarray2(st,i) = potemp_c5(y,m,l,st+ini_st-1,j)
                    !sigma
                    ! psarray2(st,i) = sigma_c5(y,m,l,st+ini_st-1,j)
                    psarray2(st,i) = sal_c5(y,m,l,st+ini_st-1,j)
                        if(psarray2(st,i) == 0.) then;psarray2(st,i)=1.;mask2(st,i) = 0;else;mask2(st,i) = 1;endif
                    i = i + 1
                end do 
            end do
        end do        
        print*,minval(psarray2),maxval(psarray2)
        !temp
            ! call betcolork2(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,months*years,fin_st-ini_st+1,months*years,-100.,0.,r1(0),g1(0),b1(0))
            ! call betcolork2(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,months*years,fin_st-ini_st+1,months*years,26.,100.,r1(27),g1(27),b1(27))
            ! do n = 1, 26
            !     call betcolork2(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,months*years,fin_st-ini_st+1,months*years,real(n-1),real(n),r1(n),g1(n),b1(n))
            ! end do
            ! do n = 1,14
            !     if(n<=3) then
            !         call rgbk(0.,0.,0.7)
            !     else if(n<=7) then
            !         call rgbk(0.,0.7,0.)
            !     else if (n<=10) then
            !         call rgbk(0.8,0.7,0.)
            !     else if (n<=14) then
            !         call rgbk(0.7,0.,0.)
            !     else
            !     end if
            !     call pscont3(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,180,fin_st-ini_st+1,180,1,real((n-1)*2),0.)
            ! end do
        !sigma
            ! call betcolork2(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,months*years,fin_st-ini_st+1,months*years,0.,24.,r2(0),g2(0),b2(0))
            ! call betcolork2(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,months*years,fin_st-ini_st+1,months*years,27.2,100.,r2(46),g2(46),b2(46))
            ! do n = 1, 45
            !     call betcolork2(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,months*years,fin_st-ini_st+1,months*years,23.0+real(n-1)/10.,23.+real(n)/10.,r2(n),g2(n),b2(n))
            ! end do
            ! do n = 1,10
            !     if(n<=3) then
            !         call rgbk(0.,0.,0.7)
            !     else if(n<=5) then
            !         call rgbk(0.,0.7,0.)
            !     else if (n<=7) then
            !         call rgbk(0.8,0.7,0.)
            !     else if (n<=10) then
            !         call rgbk(0.7,0.,0.)
            !     else
            !     end if
            !     call pscont3(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,180,fin_st-ini_st+1,180,1,23.+real((n-1)/2.),0.)
            ! end do
        !sal
            call betcolork2(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,months*years,fin_st-ini_st+1,months*years,0.,33.4,r3(0),g3(0),b3(0))
            call betcolork2(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,months*years,fin_st-ini_st+1,months*years,34.6,100.,r3(49),g3(49),b3(49))
            do n = 1, 48
                call betcolork2(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,months*years,fin_st-ini_st+1,months*years,33.4+real(n-1)/40.,33.4+real(n)/40.,r3(n),g3(n),b3(n))
            end do
            do n = 1,13
                if(n<=4) then
                    call rgbk(0.,0.,0.7)
                else if(n<=7) then
                    call rgbk(0.,0.7,0.)
                else if (n<=10) then
                    call rgbk(0.7,0.7,0.)
                else if (n<=13) then
                    call rgbk(0.7,0.,0.)
                else
                end if
                call pscont3(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,180,fin_st-ini_st+1,180,1,33.4+real((n-1)/10.),0.)
            end do

        call rgbk(0.,0.,0.)
        call plot(dx*real(fin_st-ini_st+1)+1.5*dx,-height,-3)
        ! call plot(1.5*dx,-(height+dy/2.),-3)

    end do
    ! call gmark(0.,0.,0.5,1)
    call plot(1.,0.,-3)
    !temp
    ! call colorscale_creator(26,r1,g1,b1,0.,26.,5,0.3,1,10.,.5,90,1,1)
    !sigma
    ! call colorscale_creator(45,r2,g2,b2,23.,27.5,5,0.3,1,10.,.5,90,1,1)
    !sal
    call colorscale_creator(48,r3,g3,b3,33.4,34.6,4,0.3,2,10.,.5,90,1,1)

    call plot(0.,14.,-3)
    call create_map(39,42,137,142,1,6,1,3.,0.3)


    call plote
end program