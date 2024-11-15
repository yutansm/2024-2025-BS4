program SSHdiff
    use always
    implicit none
    real,parameter::width = 7.,height = 6.,width2 = 26.,height2 = 4.,width3 = 26.,height3 = 8.
    real,dimension(15,12)::saka,tobi,plotarray,ncalsaka=-999.,ncaltobi=-999.,rawsaka,rawtobi,saka_tobi=0.
    real,dimension(12)::avarray,semarray
    integer,dimension(12)::dataarray
    real,dimension(180)::ploty=0.,ploty2=0.,saka_tobi1d
    real,dimension(:),allocatable::avsaka,avtobi,semsaka,semtobi,sdsaka,sdtobi,ymeansaka,ymeantobi,ymeanrawsaka,ymeanrawtobi,ymeansaka_tobi,avsaka_tobi
    integer,dimension(:),allocatable::sakadata,tobidata
    real::dx,min,max,num1,num2,num3,num4,ssakatobi1d
    integer::month1,month2,result
    integer::k2
    character(len=30)::string

    !! saka == from 北海道開発局, tobi == from 国土地理院 not sure
    ! saka = 4703, tobi = 1702
    call SSH_data(saka, ilabel = 4602, convert = 1, calibrate = 1)
    call SSH_data(tobi, ilabel = 1611, convert = 1, calibrate = 1)
    call SSH_data(rawsaka, ilabel = 4602, convert = 1)
    call SSH_data(rawtobi, ilabel = 1611, convert = 1)
    saka(1,2)=-999.;saka(1,1)=-999.
    ! print*,saka
    ! print*,ncalsaka

    call avsemdata_2D(saka, 'dim2', mean_1D = ymeansaka, rmask = -999.) ! 15 yearly means
    call avsemdata_2D(tobi, 'dim2', mean_1D = ymeantobi, rmask = -999.) ! 15 yearly means
    call avsemdata_2D(rawsaka, 'dim2', mean_1D = ymeanrawsaka, rmask = -999.) ! 15 yearly means
    call avsemdata_2D(rawtobi, 'dim2', mean_1D = ymeanrawtobi, rmask = -999.) ! 15 yearly means
    ! print*,ymeansaka,'saka'
    ! print*,ymeantobi,'tobi'
    do n = 1, 15
        do m = 1,12
            if(saka(n,m) /= 0 .and. saka(n,m) /= -999.) then
                saka(n,m) = saka(n,m) - ymeansaka(n) ! saka array now contains deviations from yearly means
            end if
            if(tobi(n,m) /= 0 .and. tobi(n,m) /= -999.) then
                tobi(n,m) = tobi(n,m) - ymeantobi(n) ! tobi array now contains deviations from yearly means
            end if
            if(rawsaka(n,m) /= 0 .and. rawsaka(n,m) /= -999.) then
                ncalsaka(n,m) = rawsaka(n,m) - ymeanrawsaka(n) ! ncalsaka array now contains deviations from yearly means
            end if
            if(rawtobi(n,m) /= 0 .and. rawtobi(n,m) /= -999.) then
                ncaltobi(n,m) = rawtobi(n,m) - ymeanrawtobi(n) ! ncaltobi array now contains deviations from yearly means
            end if
        end do
    end do

    !saka - tobi 差の時系列
    do n = 1, 15
        do m = 1,12
            if(saka(n,m) /= 0 .and. saka(n,m) /= -999. .and. tobi(n,m) /= 0 .and. tobi(n,m) /= -999.) then
                saka_tobi(n,m) = saka(n,m) - tobi(n,m) ! saka_tobi contains differences between saka and tobi from their respective yearly means
            end if
        end do
    end do

    call avsemdata_2D(saka, 'dim1', mean_1D = avsaka, sem_1D = semsaka, s_1D = sdsaka, rmask = -999., dataquan_1D = sakadata) ! 12 monthly means
    call avsemdata_2D(tobi, 'dim1', mean_1D = avtobi, sem_1D = semtobi, s_1D = sdtobi, rmask = -999., dataquan_1D = tobidata) ! 12 monthly means

    call avsemdata_2D(saka_tobi, 'dim2', mean_1D = ymeansaka_tobi, rmask=-999.) ! 差の時系列の年平均

    do n = 1, 15
        do i = 1,12
            if(saka(n,i) /= -999. .and. saka(n,i) /= 0 .and. tobi(n,i) /= -999. .and. tobi(n,i) /= 0) then
                saka_tobi(n,i) = saka_tobi(n,i) - ymeansaka_tobi(n) ! 差の時系列の年平均からの差
            end if
        end do
    end do

    saka_tobi1d = reshape(saka_tobi, [180])

    call avsemdata_1D(saka_tobi1d, mean = num3, rmask = -999., s = ssakatobi1d) ! 差の時系列の年平均からの差の和＝0 平均＝0 になるはず。
    print*, num3, ssakatobi1d
    call avsemdata_2D(saka_tobi, 'dim1', mean_1D = avsaka_tobi) ! 12 monthly means

    call plots2('../Plots/SSH/monthly_coast-offshore_sakatobi.ps', h = 'Coastal and Offshore Comparisons of Sea Surface Height', oopt= 'otops')
    call plot(1.5,0., -3)
    call plotsave('first')

! monthly means and diffs
    ! saka and tobi
    do l = 1, 3
        call newpen2(3)
        if(l == 1) then
            min = 150.; max = -150.; avarray = avsaka; semarray = semsaka; dataarray = sakadata
            call symbolc(width/2., .6, 0.6, 'Saka', 0.)
        else if(l == 2) then
            min = 150.; max = -150.; avarray = avtobi; semarray = semtobi; dataarray = tobidata
            call symbolc(width/2., .6, 0.6, 'Tobi', 0.)
        else if(l == 3) then
            min = 2.; max = -2.; avarray = avsaka_tobi
            do i = 1, size(avarray)
                semarray(i) = sqrt((sdsaka(i)**2.0 / real(sakadata(i))) + (sdtobi(i)**2.0 / real(tobidata(i))))
            end do
            call symbolc(width/2., 0.9, 0.6, 'Saka;-Tobi', 0.)
            call rgbk(0.4, 0.4, 0.4)
            ! call floating_lines(width,0.,5,3,y_inc=-height/6.,y=-height/6.,dashy = -4)
            call rgbk(0., 0., 0.)
        end if
        call box(width, -height, 3)
        call mod12_memori(13, 0.5, 0, width, gap = 2, num_freq = 1, y = -height, dxval = dx)
        if(l /= 3) call num_memori(min/10., max/10., 20, 5, 0.6, -1, -height, -90)
        if(l == 3) call num_memori(min, max, 4, 1, 0.6, -1, -height, -90)
        call rgbk(0.4, 0.4, 0.4)
        call newpen2(3)
        call newpen2(-4)
        call plot(0., -height/2., 3)
        call plot(width, -height/2., 2)
        call rgbk(0., 0., 0.)
        do n = 1, 13
            if(n == 13) then
                m = 1
            else
                m = n
            end if
            if(l /= 3) then
                call gmark_ratio(avarray(m), min, max, -height, ploty(m)) ! individual data points
                ploty2(m) = semarray(m)*height/(min - max)
            end if
            if(l == 3) then
                if(m == 1) then
                    month1 = 1
                else
                    month1 = m
                end if
                month2 = m - 1
                call gmark_ratio(avarray(m)/ssakatobi1d, min, max, -height, ploty(m)) ! diff from yearly mean of the diffs normalized 
                ! ploty2(m) = sqrt((sdsaka(m)**2.0 / sakadata(m)) + (sdtobi(m)**2.0 / tobidata(m)))/avsaka_tobi(m)*height/(min - max)
            else 
                ! Placeholder for additional actions if needed
            end if
            call newpen2(3)
            call gmark(dx/2. + dx*real(n-1), ploty(m), 0.2, 1)
            if(n > 1) then
                if(n /= 13) then
                    call plot(dx/2. + dx*real(n-2), ploty(m-1), 3)
                    call plot(dx/2. + dx*real(n-1), ploty(m), 2)
                else
                    call plot(dx/2. + dx*real(n-2), ploty(12), 3)
                    call plot(dx/2. + dx*real(n-1), ploty(1), 2)
                end if
            end if
            if(l /= 3) then
                call plot(dx/2. + dx*real(n-1), ploty(m) - ploty2(m), 3) ! sem or 95 CI lines
                call plot(dx/2. + dx*real(n-1), ploty(m) + ploty2(m), 2)
            end if
        end do
        call plot(width + 2., 0., -3)
    end do

    ! call newpage('SSH timeseries', x = 1.5, y = 0.5)
    call plotback('first');call plot(0.,-height-3.,-3)
    call header('SSH timeseries',symbol_size = 0.8,y = -height-3.5)
    call rgbk(0.6, 0.6, 0.6)
    call floating_lines(-21., 90., 15, 4, x_inc = width3/15., x = width3/15./12./2., dashy = -3)
    call rgbk(0., 0., 0.)
    call floating_numbers(2009., 1., 15, 0.4, width3/15., 0., 0., -1, x = width3/15./12.*6.5, y = 0.2)

    ! time series of saka and tobi together
    do l = 1,2
        min = 200.; max = -200.
        call rgbk(0., 0., 0.)
        ! if(l == 2) call plot(0., -height3 - 0.7, -3)
        if(l==1)then
            call box(width3, -height3, 3)
            call mod12_memori(15*12, 0.3, 0, width3, gap = 2, num_freq = 6, num_st = 1, y = -height3, dxval = dx)
            call num_memori(min, max, 20, 5, 0.5, 1, -height3, -90)
            call rgbk(0.4, 0.4, 0.4)
            call newpen2(3)
            call newpen2(-4)
            call plot(0., -height3/2., 3)
            call plot(width3, -height3/2., 2)
            call rgbk(0., 0., 0.)
        end if


        if(l == 1) then
            plotarray = saka
            call symbolc(-2., -height3/2., 0.5, 'Saka&Tobi;Tobi=Grey', 90.)
        else if(l == 2) then
            plotarray = tobi
        end if
        j = 1
        do n = 1, 15
            do i = 1,12
                if(plotarray(n,i) == 0 .or. plotarray(n,i) == -999.) then
                    if(j == 180) then
                        ploty(j) = 0.
                        exit
                    else
                        ploty(j) = 0.
                        j = j + 1
                        cycle
                    end if
                else
                    call gmark_ratio(plotarray(n,i), min, max, -height3, ploty(j))
                end if
                if(mod(l,2) == 0) call rgbk(0.5, 0.5, 0.5)
                call gmark(dx/2. + dx*real(j-1), ploty(j), 0.15, 1)
                if(j > 1) then
                    if(l==1.and.j<4)then
                    else
                        do k = 1, 5
                            if(ploty(j-k) /= 0 .and. ploty(j-k) /= -999.) then
                                k2 = k
                                exit
                            end if
                        end do
                        if(k2 > 1) then
                            call newpen2(3)
                            call newpen2(-6)
                        else if(k2 == 1) then
                            call newpen2(3)
                        end if
                        call plot(dx/2. + dx*real(j - k2 - 1), ploty(j - k2), 3)
                        call plot(dx/2. + dx*real(j-1), ploty(j), 2)
                    end if
                end if
                j = j + 1
            end do
        end do
    end do

    ! time series of diffs between pairs of stations
    call newpage('Timeseries of SSH difference', x = 1.5,y = 0.7)
    call rgbk(0.6, 0.6, 0.6)
    call floating_lines(-21., 90., 15, 4, x_inc = width3/15., x = width3/15./12./2., dashy = -3)
    call rgbk(0., 0., 0.)
    call floating_numbers(2009., 1., 15, 0.4, width3/15., 0., 0., -1, x = width3/15./12.*6.5, y = 0.2)

    do l = 1,1
        call rgbk(0., 0., 0.)
        if(l == 2) call plot(0., -height3 - 0.7, -3)
        min = 100.; max = -100.
        call box(width3, -height3, 3)
        call mod12_memori(15*12, 0.3, 0, width3, gap = 2, num_freq = 6, num_st = 1, y = -height3, dxval = dx)
        call num_memori(min, max, 20, 5, 0.5, 1, -height3, -90)
        call rgbk(0.4, 0.4, 0.4)
        call newpen2(3)
        call newpen2(-4)
        call plot(0., -height3/2., 3)
        call plot(width3, -height3/2., 2)
        call rgbk(0., 0., 0.)

        plotarray = saka_tobi
        call symbolc(-2., -height3/2., 0.5, 'Saka-Tobi', 90.)

        j = 1
        do n = 1, 15
            do i = 1,12
                if(plotarray(n,i) == 0 .or. plotarray(n,i) == -999.) then
                    if(j == 180) then
                        ploty(j) = 0.
                        exit
                    else
                        ploty(j) = 0.
                        j = j + 1
                        cycle
                    end if
                else
                    call gmark_ratio(plotarray(n,i), min, max, -height3, ploty(j))
                    call gmark(dx/2. + dx*real(j-1), ploty(j), 0.15, 1)
                    if(j > 3) then
                        do k = 1, 5
                            if(ploty(j-k) /= 0 .and. ploty(j-k) /= -999.) then
                                k2 = k
                                exit
                            end if
                        end do
                        if(k2 > 1) then
                            call newpen2(3)
                            call newpen2(-6)
                        else if(k2 == 1) then
                            call newpen2(3)
                        end if
                        call plot(dx/2. + dx*real(j - k2 - 1), ploty(j - k2), 3)
                        call plot(dx/2. + dx*real(j-1), ploty(j), 2)
                    end if
                end if
                j = j + 1
            end do
        end do
    end do

    ! call newpage('SSH individual timeseries', x = 1.5, y = 0.5)
    call plotback('first');call plot(0.,-height3-1.,-3);call plotsave('second')
    call rgbk(0.6, 0.6, 0.6)
    call floating_lines(-21., 90., 15, 4, x_inc = width2/15., x = width2/15./12./2., dashy = -3)
    call rgbk(0., 0., 0.)
    call floating_numbers(2009., 1., 15, 0.4, width2/15., 0., 0., -1, x = width2/15./12.*6.5, y = 0.2)
    ! time series of individual stations

    ! call newpage('SSH individual timeseries', x = 1.5, y = 0.5)
    do l = 1,6
        if(l == 3) call plotback('second')
        if(l==5) call newpage('Raw Data', x = 1.5, y = 0.5)
        ! if(l == 5) call plotback('first');call plot(0.,-height2-3.,-3)
        min = 200.; max = -200.
        if(l == 1) then
            plotarray = saka
            call symbolc(-2., -height2/2., 0.7, 'Saka', 90.)
        else if(l == 2) then
            plotarray = tobi
            call symbolc(-2., -height2/2., 0.7, 'Tobi', 90.)
        else if(l == 3) then
            plotarray = ncalsaka
        else if(l == 4) then
            plotarray = ncaltobi
        else if(l == 5) then
            plotarray = rawsaka
            call symbolc(-2., -height2/2., 0.7, 'Saka', 90.)
        else if(l == 6) then
            plotarray = rawtobi
            call symbolc(-2., -height2/2., 0.7, 'Tobi', 90.)
        end if
        if(l > 4) then
            min = 2500.; max = 0.
        end if
        if(l < 3 .or. l > 4) then
            call box(width2, -height2, 3)
            call mod12_memori(15*12, 0.3, 0, width2, gap = 2, num_freq = 6, num_st = 1, y = -height2, dxval = dx)
            call num_memori(min, max, 25, 5, 0.5, 1, -height2, -90)
        end if
        j = 1
        do n = 1, 15
            do i = 1,12
                if(l > 2) call rgbk(1., 0., 0.)
                if(l > 4) call rgbk(0., 0., 0.)
                if(plotarray(n,i) == 0 .or. plotarray(n,i) == -999.) then
                    if(j == 180) then
                        ploty(j) = 0.
                        exit
                    else
                        ploty(j) = 0.
                        j = j + 1
                        cycle
                    end if
                else
                    call gmark_ratio(plotarray(n,i), min, max, -height2, ploty(j))
                end if
                call gmark(dx/2. + dx*real(j-1), ploty(j), 0.15, 1)
                if(j > 1) then
                    if(mod(l,2)==1.and.j<4.and.l/=5)then
                    else
                        do k = 1, 5
                            if(ploty(j-k) /= 0 .and. ploty(j-k) /= -999.) then
                                k2 = k
                                exit
                            end if
                        end do
                        if(k2 > 1) then
                            call newpen2(3)
                            call newpen2(-6)
                        else if(k2 == 1) then
                            call newpen2(3)
                        end if
                        call plot(dx/2. + dx*real(j - k2 - 1), ploty(j - k2), 3)
                        call plot(dx/2. + dx*real(j-1), ploty(j), 2)
                    end if
                end if
                j = j + 1
                call rgbk(0., 0., 0.)
            end do
        end do
        call plot(0., -height2 - 0.7, -3)
    end do

    call plote

end program