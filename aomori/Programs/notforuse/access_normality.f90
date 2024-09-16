program standard_normality_curve
    implicit none
    intrinsic sin,cos,tan,asin,acos
    integer::n,y,m,l,st,d,datanum,i
    integer,parameter::years = 15, months = 12, lines = 2, stations = 9, depth = 400
    real::yplot,xplot,pi,sd,mean,total,minisum,varsum,variance,z_score,probability,dx,x_normdis,y_normdis
    real,parameter::length = 6.
    ! real,parameter::sigma = 1., mu = 0.
    real,dimension(years,months,lines,stations,depth)::potemp_c5,sal_c5,sigma_5
    real,dimension(720000)::alldata_array
    pi = 2.*asin(1.)
    call plots(10.,5.,13,'/LARGE0/gr10291/nishimori2/aomori/Subroutines/normal_distribution_curve.ps')
    ! call standard_normal_distribution(6.)

    call calibrated_data(potemp_c5,sal_c5)
    call create_sigma_array(potemp_c5,sal_c5,sigma_5)
    l = 1
    minisum = 0.
    datanum = 0
    do y = 1,years
        do m = 1,months
            do st = 4,9
                do d = 1, depth
                    if (sigma_5(y,m,l,st,d)/=0.) then
                        total = minisum + sigma_5(y,m,l,st,d)
                        minisum = total
                        datanum = datanum+1
                    else;end if
                end do
            end do
        end do
    end do
    mean = total/real(datanum)
    print*,'datanum for calculating mean',datanum
    print*,'mean',mean
    minisum = 0.
    datanum = 0
    do y = 1, years
        do m = 1, months
            do st = 4,9
                do d = 1,depth
                    if (sigma_5(y,m,l,st,d)/=0.) then
                    varsum = minisum + (sigma_5(y,m,l,st,d) - mean)**2
                    minisum = varsum
                    datanum = datanum +1
                    else;end if
                end do
            end do
        end do
    end do
    print*,'datanum for calculating varsum',datanum
    variance = varsum/real(datanum-1)
    sd = sqrt(variance)
    print*,'sd',sd

    !mean = 26.92033,sd = 0.5991395,datanum = 301160
    ! do 1,40
    !     if ()



    !median below
    n = 0
    do y = 1,years
        do m = 1,months
            do st = 4,9
                do d = 1, depth                   
                    if(sigma_5(y,m,l,st,d)/=0.) then
                        n = n+1
                        alldata_array(n) = sigma_5(y,m,l,st,d)
                    else;end if
                end do
            end do
        end do
    end do 
    print*,n
    ! n = 301160 nice
    ! print*,alldata_array(1:1000)
    dx = length/40.
    datanum = 0
    do i = 1,40
        do n = 1, 301160
            if(alldata_array(n)>=25.+real(i-1)/10. .and. alldata_array(n)<=25.+real(i)/10.) then
                datanum = datanum+1
            else;end if
        end do
        print*,datanum
        probability = real(datanum)/301160.
        yplot = length*probability
        call betsqk(real(i-1)*dx,0.,real(i)*dx,yplot,0.,0.,0.)
        datanum = 0
    end do
    call newpen2(3)
    call num_memori(25.,29.,40,2,0.1,1,length,0,0,0)
    !total datapoints plotted is 295604
    call plot(0.,0.,3);call plot(length,0.,2);call plot(length,length,2);call plot(0.,length,2);call plot(0.,0.,2)
    call plot(length,0.,-3);call num_memori(0.,1.,10,1,0.2,1,length,90,0,0)
    call plot(-length/2.,0.,-3)
    do n = -100,100
        x_normdis = length/2.*real(n)/100.
        y_normdis = length*1./(sd*sqrt(2.*pi))*exp(-0.5*((x_normdis-mean)/sd)**2.)
        call gmark(x_normdis,y_normdis,0.02,1)
        print*,x_normdis,y_normdis
    end do
    print*,mean,sd

end program