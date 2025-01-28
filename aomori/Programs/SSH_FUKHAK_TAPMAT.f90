program SSH_FukHak_TapMat
    use always
    implicit none
    real,parameter::width = 7.,height = 6.,width2 = 26.,height2 = 4.,width3 = 26.,height3 = 8.
    real,dimension(15,12)::esa,oku,sak,tob,esa_oku=0.,sak_tob=0.
    real,dimension(180)::timeseries
    real,dimension(13)::avarray,semarray
    real,dimension(:),allocatable::avesa,avoku,avsak,avtob,semesa,semoku,semsak,semtob,sdesa,sdoku,sdsak,sdtob,ymeane,ymeano,ymeans,ymeant,ymeanesa_oku,ymeansak_tob,avesa_oku,avsak_tob,semesa_oku,semsak_tob
    real::min,max
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    ! data obtainment and processing
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! obtain SSAP calibrated data
    call SSH_data(esa,ilabel = 2605,convert = .true., calibrate = .true.) !!!!!! is Fukaura !!!!!!
    ! call SSH_data(oku,ilabel = 2103,convert = .true., calibrate = .true.) !!!!!! is Hakodate !!!!!!
    call SSH_data(oku,ilabel = 4701,convert = .true., calibrate = .true.) !!!!!! is Matsumae !!!!!!
    call SSH_data(sak, ilabel = 3603, convert = .true., calibrate = .true.) !!!!!! is Tappi !!!!!!
    call SSH_data(tob, ilabel = 4701, convert = .true., calibrate = .true.) !!!!!! is Matsumae !!!!!!


    ! esa(2,2)=-999.;esa(4,2)=-999.  ! delete shady data, the program below handles cases where one station has data and the other doesn't so do not worry about oku(2,2) or oku(4,2) when taking the diff

    sak(14,8) = -999.;tob(14,8) = -999. ! delete shady data
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
            if(esa(n,i)==-999..or.esa(n,i)==0..or.oku(n,i)==-999..or.oku(n,i)==0.)then
            else;esa_oku(n,i) = esa_oku(n,i) - ymeanesa_oku(n) 
            end if
        end do
    end do
    do n = 1, 15
        do i = 1,12
            if(sak(n,i)==-999..or.sak(n,i)==0..or.tob(n,i)==-999..or.tob(n,i)==0.)then
            else;sak_tob(n,i) = sak_tob(n,i) - ymeansak_tob(n)
            end if
        end do
    end do

    call avsemdata_2D(esa_oku,'dim1',mean_1D = avesa_oku,sem_1D = semesa_oku) ! 12 monthly means
    call avsemdata_2D(sak_tob,'dim1',mean_1D = avsak_tob,sem_1D = semsak_tob) ! 12 monthly means
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    ! end of data obtainment and processing
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                ! plot the data
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call plots2('../Plots/Favorites/SSH_FukMat_TapMat.ps',h = 'Title',oopt= 'otops')
    call plot(0.7,-.5-height,-3);call plotsave('first')
        ! monthly means and diffs
        ! esashi and okushiri
        do l = 1, 3
            if(l==3)then;min = -60.;max=60.
            else;min = -150.;max = 150.
            end if
            if(l/=2)then
                call mod12_memori(13,width,gap = 2,num_freq = 1)
            end if
            if(l == 1)then
                avarray(1:12) = avoku;avarray(13) = avoku(1)
                semarray(1:12) = semoku;semarray(13) = semoku(1)
                ! call symbolc(width/2.,height+.6,0.6,'Esa&Oku',0.)
            else if(l == 2)then
                avarray(1:12) = avesa;avarray(13) = avesa(1)
                semarray(1:12) = semesa;semarray(13) = semesa(1)
            else if(l == 3)then
                avarray(1:12) = avesa_oku;avarray(13) = avesa_oku(1)
                semarray(1:12) = semesa_oku;semarray(13) = semesa_oku(1)
                ! call symbolc(width/2.,height+.6,0.6,'Esa-Oku',0.)
            end if
            if(l==1)call butler_linegraph(avarray,width,height,min,max,mem=.true.,memscale = 0.1,memiter = 7,memsymsize = 0.6,memsymfreq = 1,memflqt = -1,blabel = 'Months',error_1D = semarray,rl = 0.6,gl=0.6,bl=0.6,lthick = 7)
            if(l==2)call butler_linegraph(avarray,width,height,min,max,error_1D = semarray,lthick = 4)
            if(l==3)call butler_linegraph(avarray,width,height,min,max,mem=.true.,memscale = 0.1,memiter = 13,memsymsize = 0.6,memsymfreq = 2,memflqt = -1,blabel = 'Months',error_1D = semarray,maskbyc = .true.,lthick = 4)
            if(l==2)call plot(width+2.,0.,-3)
        end do
        
        call plotback('first')
        call plot(0.,-height-3.,-3)

        !SAK and TOB
        do l = 1, 3
            if(l==3)then;min = -60.;max=60.
            else;min = -150.;max = 150.
            end if
            ! creating box
            if(l/=2)then
                call mod12_memori(13,width,gap = 2,num_freq = 1)
            end if
            if(l == 1) then
                avarray(1:12) = avtob; semarray(1:12) = semtob
                avarray(13) = avtob(1); semarray(13) = semtob(1)
                ! call symbolc(width/2., height+.6, 0.6, 'Sak&Tob', 0.)
            else if(l == 2) then
                avarray(1:12) = avsak; semarray(1:12) = semsak
                avarray(13) = avsak(1); semarray(13) = semsak(1)
            else if(l == 3) then
                avarray(1:12) = avsak_tob; semarray(1:12) = semsak_tob
                avarray(13) = avsak_tob(1); semarray(13) = semsak_tob(1)
                ! call symbolc(width/2., height+0.6, 0.6, 'Sak-Tob', 0.);call newpen2(4)
            end if
            if(l==1)call butler_linegraph(avarray,width,height,min,max,mem=.true.,memscale = 0.1,memiter = 7,memsymsize = 0.6,memsymfreq = 1,memflqt = -1,blabel = 'Months',error_1D = semarray,rl = 0.6,gl=0.6,bl=0.6,lthick = 7)
            if(l==2)call butler_linegraph(avarray,width,height,min,max,error_1D = semarray,lthick = 4)
            if(l==3)call butler_linegraph(avarray,width,height,min,max,mem=.true.,memscale = 0.1,memiter = 13,memsymsize = 0.6,memsymfreq = 2,memflqt = -1,blabel = 'Months',error_1D = semarray,maskbyc = .true.,lthick = 4)
            
            if(l==2)call plot(width + 2., 0., -3)
        end do

! SAK and TOB
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                            ! end of plotting
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                            ! creating map
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call plot(width+2.7,2.,-3)
    call map(38,43,137,142,8.,line_opt = 1)

    call newpage
    call obottoms(2.)
    timeseries = reshape(transpose(esa),[180])
    call butler_linegraph(timeseries,25.,17.,-150.,150.,-999.,mem=.true.,memscale = 0.1,memiter = 7,memsymsize = 1.2,tlabel = 'Fukaura')
    call mod12_memori(180,25.,symbol_size = 0.3,num_freq = 6)

    call newpage
    call obottoms(2.)
    timeseries = reshape(transpose(oku),[180])
    call butler_linegraph(timeseries,25.,17.,-150.,150.,-999.,mem=.true.,memscale = 0.1,memiter = 7,memsymsize = 1.2,tlabel = 'Hakodate')
    call mod12_memori(180,25.,symbol_size = 0.3,num_freq = 6)

    call newpage
    call obottoms(2.)
    timeseries = reshape(transpose(esa_oku),[180])
    call butler_linegraph(timeseries,25.,17.,-150.,150.,-999.,mem=.true.,memscale = 0.1,memiter = 7,memsymsize = 1.2,tlabel = 'Fukaura-Hakodate')
    call mod12_memori(180,25.,symbol_size = 0.3,num_freq = 6)

    call newpage 
    call obottoms(2.)
    timeseries = reshape(transpose(sak),[180])
    call butler_linegraph(timeseries,25.,17.,-150.,150.,-999.,mem=.true.,memscale = 0.1,memiter = 7,memsymsize = 1.2,tlabel = 'Tappi')
    call mod12_memori(180,25.,symbol_size = 0.3,num_freq = 6)

    call newpage
    call obottoms(2.)
    timeseries = reshape(transpose(tob),[180])
    call butler_linegraph(timeseries,25.,17.,-150.,150.,-999.,mem=.true.,memscale = 0.1,memiter = 7,memsymsize = 1.2,tlabel = 'Matsumae')
    call mod12_memori(180,25.,symbol_size = 0.3,num_freq = 6)

    call newpage
    call obottoms(2.)
    timeseries = reshape(transpose(sak_tob),[180])
    call butler_linegraph(timeseries,25.,17.,-150.,150.,0.,mem=.true.,memscale = 0.1,memiter = 7,memsymsize = 1.2,tlabel = 'Tappi-Matsumae')
    call mod12_memori(180,25.,symbol_size = 0.3,num_freq = 6)

    


call plote
end program