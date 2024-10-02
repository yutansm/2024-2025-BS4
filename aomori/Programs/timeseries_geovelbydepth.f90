program examining_2011
    use constants
    implicit none
    ! integer,parameter::years = 15, months = 12, lines = 2, stations = 9, depth = 400
    real,parameter::width = 2., height = 24.
    integer,parameter::ini_st = 5, fin_st = 9,obs_depth = 201
    ! real,dimension(years,months,lines,stations,depth)::geovel_5
    real,dimension(fin_st-ini_st+1,months*years)::psarray2=0.
    integer,dimension(fin_st-ini_st+1,months*years)::mask2
    ! real,dimension(0:101)::r4,g4,b4
    ! integer::y,m,n,i,st,j
    ! real::dx,dy
    l=1

    call geovel_array(51,geovel_5)
    ! print*,geovel_5(1,6,1,9,1:400)
    call plots(3.,0.5,9,'../Plots/Geostrophy/timeseries_vel3.ps')
    call symbol(0.,height+1.4,0.8,'time series of geostrophic velocity',0.,len('time series of geostrophic velocity'))
    call b2r_colorgrad(24,8,r4,g4,b4)

    do j = 1,obs_depth,50
        call create_box(width,height,3)
        call numberr(width/2.,height+0.4,0.5,real(j),0.,-1);call symbol(width/2.,height+0.4,0.5,'m',0.,len('m'))
        dx = width/real(fin_st-ini_st+1);dy = height/(real(months*years))
        call mod12_memori3(0.,0.,months*years,0.15,-90,height,1,2)
        call st_memori(1,6,width,1,0.4,0)
        if(j==1) then
            call floating_numbers(-2.,dy*6.,2023.,-1.,15,0.6,0.,dy*12.,0.,-1)
        end if
        call plot(0.,height,-3)

        do st = 1,fin_st-ini_st+1
            i = 1
            do y = 1, years
                do m = 1, months
                    !geovel
                    psarray2(st,i) = geovel_5(y,m,l,st+ini_st-1,j)
                        if(psarray2(st,i) == 0.) then;psarray2(st,i)=1.;mask2(st,i) = 0;else;mask2(st,i) = 1;endif
                    i = i + 1
                end do 
            end do
        end do        
        print*,minval(psarray2),maxval(psarray2)
        !geovel
            call betcolork2(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,months*years,fin_st-ini_st+1,months*years,-100.,-40.,r4(0),g4(0),b4(0))
            call betcolork2(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,months*years,fin_st-ini_st+1,months*years,80.,100.,r4(25),g4(25),b4(25))
            do n = 1, 24
                call betcolork2(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,months*years,fin_st-ini_st+1,months*years,-40.+real(n-1)*5.,-40.+real(n)*5.,r4(n),g4(n),b4(n))
            end do
            do n = 1,13
                if(n<=4) then
                    call rgbk(0.,0.,0.7)
                else if(n<=5) then
                    call rgbk(0.,0.,0.)
                else if (n<=13) then
                    call rgbk(0.7,0.,0.)
                end if
                if(-40.+real((n-1)*10.) == 0.) then
                    call rgbk(0.,0.,0.)
                end if
                call pscont3(dx,-dy,psarray2,mask2,1,fin_st-ini_st+1,1,180,fin_st-ini_st+1,180,1,-40.+real((n-1)*10.),0.)
            end do

        call rgbk(0.,0.,0.)
        call plot(dx*real(fin_st-ini_st+1)+1.5*dx,-height,-3)
        ! call plot(1.5*dx,-(height+dy/2.),-3)

    end do
    ! call gmark(0.,0.,0.5,1)
    call plot(1.,0.,-3)
    !geovel
    call colorscale_creator(24,r4,g4,b4,-40.,80.,2,0.3,1,10.,.5,90,1,1)

    call plot(0.,14.,-3)
    call create_map(39,42,137,142,1,6,1,3.,0.3)


    call plote
end program