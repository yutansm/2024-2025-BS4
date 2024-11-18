program SSH_final
    use always
    implicit none
    real,parameter::width = 7.,height = 6.,width2 = 26.,height2 = 4.,width3 = 26.,height3 = 8.
    real,dimension(15,12)::esa,oku,sak,tob,plotarray,esa_oku=0.,sak_tob=0.
    real,dimension(12)::avarray,semarray
    integer,dimension(12)::dataarray
    real,dimension(180)::ploty=0.,ploty2=0.
    real,dimension(:),allocatable::avesa,avoku,avsak,avtob,semesa,semoku,semsak,semtob,sdesa,sdoku,sdsak,sdtob,ymeane,ymeano,ymeans,ymeant,ymeanesa_oku,ymeansak_tob,avesa_oku,avsak_tob
    real::dx,min,max
    integer::k2
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    ! data obtainment and processing
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! obtain SSAP calibrated data
    call SSH_data(esa,ilabel = 4703,convert = 1, calibrate = 1)
    call SSH_data(oku,ilabel = 1702,convert = 1, calibrate = 1)
    call SSH_data(sak, ilabel = 4602, convert = 1, calibrate = 1)
    call SSH_data(tob, ilabel = 1611, convert = 1, calibrate = 1)
    esa(2,2)=-999.;esa(4,2)=-999.
    sak(1,2)=-999.;sak(1,1)=-999.
    ! obtain yearly means
    call avsemdata_2D(esa,'dim2',mean_1D = ymeane,rmask = -999.) ! 15 yearly means
    call avsemdata_2D(oku,'dim2',mean_1D = ymeano,rmask = -999.)
    call avsemdata_2D(sak, 'dim2', mean_1D = ymeans, rmask = -999.) 
    call avsemdata_2D(tob, 'dim2', mean_1D = ymeant, rmask = -999.) 

    ! calculate diff from yearly means for every month
    do n = 1, 15
        do m = 1,12
                if(esa(n,m) == 0..or. esa(n,m) == -999.)then
            else;esa(n,m) = esa(n,m) - ymeane(n) ! esa array now contains deviations from yearly means      2023 has no data
            end if
                if(oku(n,m) == 0..or. oku(n,m) == -999.)then
            else;oku(n,m) = oku(n,m) - ymeano(n) ! oku array now contains deviations from yearly means
            end if
                if(sak(n,m) == 0..or. sak(n,m) == -999.)then
            else;sak(n,m) = sak(n,m) - ymeans(n) ! sak array now contains deviations from yearly means
            end if
                if(tob(n,m) == 0..or. tob(n,m) == -999.)then
            else;tob(n,m) = tob(n,m) - ymeant(n) ! tob array now contains deviations from yearly means
            end if
        end do
    end do
    ! calculate monthly means of above arrays
    call avsemdata_2D(esa,'dim1',mean_1D = avesa,sem_1D = semesa,s_1D = sdesa,rmask = -999.) ! 12 monthly means
    call avsemdata_2D(oku,'dim1',mean_1D = avoku,sem_1D = semoku,s_1D = sdoku,rmask = -999.) ! 12 monthly means
    call avsemdata_2D(sak,'dim1',mean_1D = avsak,sem_1D = semsak,s_1D = sdsak,rmask = -999.) ! 12 monthly means
    call avsemdata_2D(tob,'dim1',mean_1D = avtob,sem_1D = semtob,s_1D = sdtob,rmask = -999.) ! 12 monthly means   -> arrays to be used for the first graph

    !esa - oku 差の時系列(それぞれ年平均の偏差)
    do n = 1, 15
        do m = 1,12
            if(esa(n,m) == 0..or. esa(n,m) == -999..or. oku(n,m) == 0..or. oku(n,m) == -999.)then
            else;esa_oku(n,m) = esa(n,m) - oku(n,m) ! esa_oku contains differences between esashi and okushiri from their respective yearly means
            end if
        end do
    end do

    !sak - tob
    do n = 1, 15
        do m = 1,12
            if(sak(n,m) == 0..or. sak(n,m) == -999..or. tob(n,m) == 0..or. tob(n,m) == -999.)then
            else;sak_tob(n,m) = sak(n,m) - tob(n,m) ! 
            end if
        end do
    end do

    ! 差の時系列の年平均
    call avsemdata_2D(esa_oku,'dim2',mean_1D = ymeanesa_oku,rmask=-999.) 
    call avsemdata_2D(sak_tob,'dim2',mean_1D = ymeansak_tob,rmask=-999.) 

    ! 差の時系列の年平均からの偏差
    do n = 1, 15
        do i = 1,12
            if(esa(n,i)==-999..or.esa(n,i)==0..or.oku(n,i)==-999..or.oku(n,i)==0)then
            else;esa_oku(n,i) = esa_oku(n,i) - ymeanesa_oku(n) 
            end if
        end do
    end do
    do n = 1, 15
        do i = 1,12
            if(sak(n,i)==-999..or.sak(n,i)==0..or.tob(n,i)==-999..or.tob(n,i)==0)then
            else;sak_tob(n,i) = sak_tob(n,i) - ymeansak_tob(n)
            end if
        end do
    end do

    call avsemdata_2D(esa_oku,'dim1',mean_1D = avesa_oku) ! 12 monthly means
    call avsemdata_2D(sak_tob,'dim1',mean_1D = avsak_tob) ! 12 monthly means
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                        ! end of data obtainment and processing
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                ! plot the data
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

call plots2('../Plots/Favorites/monthly_coast-offshore.ps',h = 'Coastal and Offshore Comparisons of Sea Surface Height',oopt= 'otops')
call plot(0.5,-1.,-3);call plotsave('first')
    ! monthly means and diffs
    ! esashi and okushiri
    do l = 1, 3
        if(l==3)then;min = 30.;max=-30.
        else;min = 100.;max = -100.
        end if
        ! creating box
        if(l/=2)then
            call box(width,-height,4);call mod12_memori(13,0.5,0,width,gap = 2,num_freq = 1,y = -height,dxval = dx)
        end if
        if(l==1)call num_memori(min/10.,max/10.,20,5,0.6,-1,-height,-90) ! -> converting to cm
        if(l==3)call num_memori(min/10.,max/10.,6,1,0.6,-1,-height,-90)
        call rgbk(0.4,0.4,0.4);call newpen2(3);call plot(0.,-height/2.,3);call plot(width,-height/2.,2);call rgbk(0.,0.,0.)
        ! created box
        if(l == 2)then
            avarray = avesa;semarray = semesa
            call symbolc(width/2.,1.,0.6,'Esa&Oku',0.);call newpen2(4)
        else if(l == 1)then
            avarray = avoku;semarray = semoku
            call rgbk(0.6,0.6,0.6);call newpen2(6)
        else if(l == 3)then
            avarray = avesa_oku
            call symbolc(width/2.,0.9,0.6,'Esa-Oku',0.)
            call rgbk(0.,0.,0.);call newpen2(4)
        end if
        do n = 1, 13
            if(n==13)then;m = 1;else;m = n
            end if
            if(l/=3)then
                call gmark_ratio(avarray(m),min,max,-height,ploty(m)) !individual data points
                ploty2(m) = semarray(m)*height/(min-max)
            end if
            if(l == 3)then
                call gmark_ratio(avarray(m),min,max,-height,ploty(m))
            endif
            if(n>1)then
                if(n/=13)then
                    call plot(dx/2.+dx*real(n-2),ploty(m-1),3);call plot(dx/2.+dx*real(n-1),ploty(m),2)
                else;call plot(dx/2.+dx*real(n-2),ploty(12),3);call plot(dx/2.+dx*real(n-1),ploty(1),2)
                end if
            end if
            if(l/=3)then
                call plot(dx/2.+dx*real(n-1),ploty(m)-ploty2(m),3);call plot(dx/2.+dx*real(n-1),ploty(m)+ploty2(m),2) ! sem lines
            end if
        end do
        if(l==2)call plot(width+2.,0.,-3)
        call rgbk(0.,0.,0.)
    end do
    
    call plotback('first')
    call plot(0.,-height-2.5,-3)

    ! SAK and TOB
    do l = 1, 3
        if(l==3)then;min = 30.;max=-30.
        else;min = 150.;max = -150.
        end if
        ! creating box
        if(l/=2)then
            call box(width,-height,4);call mod12_memori(13,0.5,0,width,gap = 2,num_freq = 1,y = -height,dxval = dx)
        end if
        if(l==1)call num_memori(min/10.,max/10.,30,5,0.6,-1,-height,-90) ! -> converting to cm
        if(l==3)call num_memori(min/10.,max/10.,6,1,0.6,-1,-height,-90)
        call rgbk(0.4,0.4,0.4);call newpen2(3);call plot(0.,-height/2.,3);call plot(width,-height/2.,2);call rgbk(0.,0.,0.)
        ! created box
        if(l == 2) then
            avarray = avsak; semarray = semsak
            call symbolc(width/2., .6, 0.6, 'Sak&Tob', 0.);call newpen2(4)
        else if(l == 1) then
            avarray = avtob; semarray = semtob
            call rgbk(0.6, 0.6, 0.6);call newpen2(6)
        else if(l == 3) then
            avarray = avsak_tob
            call symbolc(width/2., 0.9, 0.6, 'Sak;-Tob', 0.);call newpen2(4)
        end if
        do n = 1, 13
            if(n == 13) then;m = 1
            else;m = n
            end if
            if(l /= 3) then
                call gmark_ratio(avarray(m), min, max, -height, ploty(m)) ! individual data points
                ploty2(m) = semarray(m)*height/(min - max)
            end if
            if(l == 3) then
                call gmark_ratio(avarray(m), min, max, -height, ploty(m)) ! diff from yearly mean of the diffs normalized 
            end if
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
        if(l==2)call plot(width + 2., 0., -3);call rgbk(0., 0., 0.)
    end do

! SAK and TOB
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                            ! end of plotting
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                            ! creating map
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call plot(width+2.,-height,-3)
    call create_map(38,43,137,142,1,6,3,9.,0.6)


call plote
end program