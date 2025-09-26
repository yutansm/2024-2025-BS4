program SSH_again
    use always
    real,dimension(15,12)::esa,oku,sak,tob,esa_oku,sak_tob,esa_oku_sak_tob
    real,dimension(:),allocatable::e_omean,e_osd,s_tmean,s_tsd,eo_stmean,eo_stsd
    real,dimension(13)::e_omeanloop,e_osdloop,s_tmeanloop,s_tsdloop,eo_stmeanloop,eo_stsdloop

    width = 8.;height = 5.;width2 =25.;height2 = 5.
    call SSH_data(esa,slabel = '江差港',calibrate = .true.,diff_from_yearly_mean = .true.,unit = 'cm')
    call SSH_data(oku,slabel = '奥尻',calibrate = .true.,diff_from_yearly_mean = .true.,unit = 'cm')
    call SSH_data(sak,slabel = '酒田港',calibrate = .true.,diff_from_yearly_mean = .true.,unit = 'cm')
    call SSH_data(tob,slabel = '飛島',calibrate = .true.,diff_from_yearly_mean = .true.,unit = 'cm')
    esa(2,2)=0.;esa(4,2)=0.  ! delete shady data, the program below handles cases where one station has data and the other doesn't so do not worry about oku(2,2) or oku(4,2) when taking the diff
    sak(1,2)=0.;sak(1,1)=0.
    do i = 1, 15
        do j = 1, 12
            if(esa(i,j)/=0..and.oku(i,j)/=0.)then 
                esa_oku(i,j) = esa(i,j) - oku(i,j) ! esa_oku contains differences between esashi and okushiri from their respective yearly means
            else 
                esa_oku(i,j) = 0.
            end if
            if(sak(i,j)/=0..and.tob(i,j)/=0.)then 
                sak_tob(i,j) = sak(i,j) - tob(i,j) ! sak_tob contains differences between sakata and tobi from their respective yearly means
            else 
                sak_tob(i,j) = 0.
            end if
            if(esa_oku(i,j)/=0..and.sak_tob(i,j)/=0.)then 
                esa_oku_sak_tob(i,j) = esa_oku(i,j) - sak_tob(i,j) ! esa_oku_sak_tob contains differences between esashi-oku and sakata-tobi from their respective yearly means
            else 
                esa_oku_sak_tob(i,j) = 0.
            end if
        end do
    end do
    call avsemdata_2D(esa_oku,'dim1',mean_1D = e_omean,sem_1D = e_osd)
    call avsemdata_2D(sak_tob,'dim1',mean_1D = s_tmean,sem_1D = s_tsd)
    call avsemdata_2D(esa_oku_sak_tob,'dim1',mean_1D = eo_stmean,sem_1D = eo_stsd)

    e_omeanloop(1:12) = e_omean ; e_omeanloop(13) = e_omean(1)
    s_tmeanloop(1:12) = s_tmean ; s_tmeanloop(13) = s_tmean(1)
    e_osdloop(1:12) = e_osd ; e_osdloop(13) = e_osd(1)
    s_tsdloop(1:12) = s_tsd ; s_tsdloop(13) = s_tsd(1)
    eo_stmeanloop(1:12) = eo_stmean ; eo_stmeanloop(13) = eo_stmean(1)
    eo_stsdloop(1:12) = eo_stsd ; eo_stsdloop(13) = eo_stsd(1)

    call plots2(nnfile = 'comparison_of_esa-oku_and_sak-tob',h = 'comparison of esa-oku and sak-tob',oopt = 'otops',x = 0.5,y = -height-2.)
    call plotsave('first')

    call butler_linegraph(e_omeanloop,width,height,-3.,3.,0.,.true.,error_1D = e_osdloop,tlabel = 'esa-oku',maskbelow = 0.)
    call mod12_memori(13,width,gap = 2)
    
    call plot(width+1.5,0.,-3)

    call butler_linegraph(s_tmeanloop,width,height,-3.,3.,0.,.true.,error_1D = s_tsdloop,tlabel = 'sak-tob',maskbelow = 0.)
    call mod12_memori(13,width,gap = 2)

    call plot(width+1.5,0.,-3)

    call butler_linegraph(eo_stmean,width,height,-3.,3.,0.,.true.,tlabel = 'esa-oku - sak-tob',maskbelow = 0.,error_1D = eo_stsd)
    call mod12_memori(13,width,gap = 2)


    call newpage('first')
    call butler_linegraph(reshape(transpose(esa_oku),[15*12]),width2,height2,-10.,10.,0.,.true.,tlabel = 'esa-oku',memsymfreq = 5,maskbelow = 0.)

    call plot(0.,-height2-1.,-3)
    call butler_linegraph(reshape(transpose(sak_tob),[15*12]),width2,height2,-10.,10.,0.,.true.,tlabel = 'sak-tob',memsymfreq = 5,maskbelow = 0.)

    call plote
    
end program  
