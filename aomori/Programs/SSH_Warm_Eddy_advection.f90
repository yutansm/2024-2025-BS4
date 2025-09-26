program warm_eddy_advection
    use always
    implicit none 
    real,parameter::width = 23.,height = 2.5
    real,dimension(15,12)::array
    real,dimension(180)::esa,fuk,oga,sak,nez,array2,esa2,fuk2,oga2,sak2,nez2,esa3,fuk3,oga3,sak3,nez3  ! esa2 == esa - mean of 5 loc,esa3 == esa - mean of 4 loc
    real,dimension(:),allocatable::meanof5,semof5,meanof4,semof4
    real,dimension(2,180)::tempseries
    real,dimension(5,180)::efosn
    character(len=10)::tlabel

    call SSH_data(array,slabel = '江差港',convert = .true.,calibrate = .true.,diff_from_yearly_mean = .true.,unit = 'cm');esa = reshape(transpose(array),[180])
    call SSH_data(array,slabel = '深浦',convert = .true.,calibrate = .true.,diff_from_yearly_mean = .true.,unit = 'cm');fuk = reshape(transpose(array),[180])
    call SSH_data(array,slabel = '男鹿',convert = .true.,calibrate = .true.,diff_from_yearly_mean = .true.,unit = 'cm');oga = reshape(transpose(array),[180])
    call SSH_data(array,slabel = '酒田港',convert = .true.,calibrate = .true.,diff_from_yearly_mean = .true.,unit = 'cm');sak = reshape(transpose(array),[180]);sak(1:2) = 0.
    call SSH_data(array,slabel = '鼠ヶ関',convert = .true.,calibrate = .true.,diff_from_yearly_mean = .true.,unit = 'cm');nez = reshape(transpose(array),[180])
    call calibrated_data2(potemp_c5 = potemp_c5,sal_c5 = sal_c5,match_station_labels_and_array_indices = .true.)
    tempseries(1,:) = reshape(transpose(potemp_c5(:,:,1,4,200)),[180]);print*,'N line'
    tempseries(2,:) = reshape(transpose(potemp_c5(:,:,2,4,200)),[180]);print*,'S line'
    call plots2(nnfile = 'SSH_Warm_Eddy_Advection',oopt = 'otops',x = 2.,y = -2.5,h = 'SSH Warm Eddy Advection')
    call plotsave('first')

! RAW DATA
    do i = 1, 5 ! raw data
        if(i == 1)then 
            array2 = esa;tlabel = 'ESA';efosn(1,:) = esa
        else if(i == 2)then 
            array2 = fuk;tlabel = 'FUK';efosn(2,:) = fuk
        elseif(i == 3)then 
            array2 = oga;tlabel = 'OGA';efosn(3,:) = oga
        elseif(i == 4)then 
            array2 = sak;tlabel = 'SAK';efosn(4,:) = sak
        elseif(i == 5)then 
            array2 = nez;tlabel = 'NEZ';efosn(5,:) = nez
        end if
        call butler_linegraph(array2,width,height,-20.,20.,0.,.true.,memsymfreq = 10,LI = .true.,maskbelow = 0.,tlabel = tlabel)
        call mod12_memori(180,width,0.3,num_freq = 6,gap = 2)

        if(i/=5)call plot(0.,-3.5,-3)
    end do
! RAW DATA

! MEAN SSH timeseries of 5 stations
    call avsemdata_2D(efosn,'dim1',mean_1D = meanof5,sem_1D = semof5)
    call newpage(h = 'mean SSH timeseries of 5 stations');call plotback('first')

    call butler_linegraph(meanof5,width,height,-20.,20.,0.,.true.,memsymfreq = 10,LI = .true.,maskbelow = 0.,tlabel = 'Mean of 5 stations',error_1D = semof5)
    call mod12_memori(180,width,0.3,num_freq = 6,gap = 2)
! MEAN SSH timeseries of 5 stations
! MEAN SSH timeseries of 4 stations,excluding esa
    call avsemdata_2D(efosn(2:5,:),'dim1',mean_1D = meanof4,sem_1D = semof4)
    call plot(0.,-3.5,-3)

    call butler_linegraph(meanof4,width,height,-20.,20.,0.,.true.,memsymfreq = 10,LI = .true.,maskbelow = 0.,tlabel = 'Mean of 4 stations (excluding ESA)',error_1D = semof4)
    call mod12_memori(180,width,0.3,num_freq = 6,gap = 2)
! MEAN SSH timeseries of 4 stations,excluding esa
! Temperature timeseries
    call plot(0.,-3.5,-3)
    
    call butler_linegraph(tempseries(1,:),width,height,0.,10.,0.,.true.,memsymfreq = 2,LI = .true.,tlabel = 'Temperature of St1 at 200m, N-Line : black, S-Line : red')
    call butler_linegraph(tempseries(2,:),width,height,0.,10.,0.,.false.,LI = .true.,rl = 1.)
    call mod12_memori(180,width,0.3,num_freq = 6,gap = 2)
! Temperature timeseries
! subtracting 5 loc mean
    esa2 = 0.;fuk2 = 0.;oga2 = 0.;sak2 = 0.;nez2 = 0.    
    do i = 1, 180 ! subtracting 5 loc mean 
        if(esa(i)/=0.)then 
            esa2(i) = esa(i) - meanof5(i)
        end if
        if(fuk(i)/=0.)then 
            fuk2(i) = fuk(i) - meanof5(i)
        end if
        if(oga(i)/=0.)then
            oga2(i) = oga(i) - meanof5(i)
        end if
        if(sak(i)/=0.)then
            sak2(i) = sak(i) - meanof5(i)
        endif
        if(nez(i)/=0.)then
            nez2(i) = nez(i) - meanof5(i)
        end if
    end do

    call newpage(h = 'Differences from the mean SSH timeseries of 5 stations');call plotback('first')

    
    do i = 1, 5 ! raw data - mean of 5 stations
        if(i == 1)then 
            array2 = esa2;tlabel = 'ESA'
        else if(i == 2)then 
            array2 = fuk2;tlabel = 'FUK'
        elseif(i == 3)then 
            array2 = oga2;tlabel = 'OGA'
        elseif(i == 4)then 
            array2 = sak2;tlabel = 'SAK'
        elseif(i == 5)then 
            array2 = nez2;tlabel = 'NEZ'
        end if
        call butler_linegraph(array2,width,height,-5.,5.,0.,.true.,memsymfreq = 5,LI = .true.,maskbelow = 0.,tlabel = tlabel)
        call mod12_memori(180,width,0.3,num_freq = 6,gap = 2)

        if(i/=5)call plot(0.,-3.5,-3)
    end do

! subtracting 5 loc mean -> 鼠ヶ関から深浦までは情報が伝播しているように思う。江差は季節変動が他4地点よりも小さくて，また時系列の形が違う。

! subtracting 4 loc mean
    esa3 = 0.;fuk3 = 0.;oga3 = 0.;sak3 = 0.;nez3 = 0.    
    do i = 1, 180 ! subtracting 5 loc mean 
        if(esa(i)/=0.)then 
            esa3(i) = esa(i) - meanof4(i)
        end if
        if(fuk(i)/=0.)then 
            fuk3(i) = fuk(i) - meanof4(i)
        end if
        if(oga(i)/=0.)then
            oga3(i) = oga(i) - meanof4(i)
        end if
        if(sak(i)/=0.)then
            sak3(i) = sak(i) - meanof4(i)
        endif
        if(nez(i)/=0.)then
            nez3(i) = nez(i) - meanof4(i)
        end if
    end do

    call newpage(h = 'Differences from the mean SSH timeseries of 4 stations');call plotback('first')

    
    do i = 1, 5 ! raw data - mean of 4 stations
        if(i == 1)then 
            array2 = esa3;tlabel = 'ESA'
        else if(i == 2)then 
            array2 = fuk3;tlabel = 'FUK'
        elseif(i == 3)then 
            array2 = oga3;tlabel = 'OGA'
        elseif(i == 4)then 
            array2 = sak3;tlabel = 'SAK'
        elseif(i == 5)then 
            array2 = nez3;tlabel = 'NEZ'
        end if
        call butler_linegraph(array2,width,height,-5.,5.,0.,.true.,memsymfreq = 5,LI = .true.,maskbelow = 0.,tlabel = tlabel)
        call mod12_memori(180,width,0.3,num_freq = 6,gap = 2)

        if(i/=5)call plot(0.,-3.5,-3)
    end do

! subtracting 4 loc mean 






!     １） 北から、江差、深浦、男鹿、秋田、ネズカ関

! の計5地点の月別経年変化を気圧補正する。 check

! ２）5地点とも平均からの偏差値にする。 check
! ３）5地点平均の経年変化時系列を作成 check
! ４）5地点ともその5地点平均からの偏差時系列を作成 check
! ５）5地点の偏差時系列を縦に並べる。
! ６）渦流擾乱が負の偏差として、南の観測点から
! 　　　北の観測点へ移動する（移動すればいいなと思う）
    call plote
end program 