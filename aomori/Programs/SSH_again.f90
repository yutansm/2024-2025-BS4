program SSH
    use always
    real,dimension(15,12)::tap,mat,mutsuoga,hak,mat_hak,tap_mutsu,tap_hak,tap_mat,asamushi,asa_hak,aomori,aomori_hak,fuk_mat,fuk
    real,dimension(:),allocatable::mat_hak_mean,tap_mutsu_mean,tap_hak_mean,tap_hak_sem,tap_mutsu_sem,mat_hak_sem,tap_mat_mean,tap_mat_sem,asa_hak_mean,asa_hak_sem,aomori_hak_mean,aomori_hak_sem
    real,dimension(:),allocatable::fuk_mat_mean,fuk_mat_sem,upmean2,downmean2,upsem2,downsem2
    real,dimension(4,15,12)::upstream,downstream ! upstream: fuk tap mat esa, downstream: tomakomai muroran mutsuoga hachinohe
    real,dimension(:,:),allocatable::upmean,downmean ! 15*12
    real,dimension(:),allocatable::upminusdown_mean,upminusdown_sem
    ! relative values from yearly means
        call SSH_data(tap,slabel = '竜飛',calibrate = .true.,diff_from_yearly_mean = .true.)
        call SSH_data(mat,slabel = '松前港',calibrate = .true.,diff_from_yearly_mean = .true.)
        call SSH_data(mutsuoga,slabel = 'むつ小川原港',calibrate = .true.,diff_from_yearly_mean = .true.)
        call SSH_data(hak,slabel = '函館',calibrate = .true.,diff_from_yearly_mean = .true.)

        width = 25.; height = 4.;width2 = 10.
        call plots2(nnfile = 'SSH_again',oopt = 'otops',x = 1.,y = -height+1.5,h = 'differences from yearly means')

        call butler_linegraph(reshape(transpose(tap),[15*12]),width,height,-15.,15.,mem = .true.,memsymfreq = 5,memlabel = 'TAP',maskbelow = 0.,memflqt = -1,LI = .true.)
        call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)
        call plot(0.,-height-1.,-3)
        call butler_linegraph(reshape(transpose(mat),[15*12]),width,height,-15.,15.,mem = .true.,memsymfreq = 5,memlabel = 'MAT',maskbelow = 0.,memflqt = -1,LI = .true.)
        call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)
        call plot(0.,-height-1.,-3)
        call butler_linegraph(reshape(transpose(hak),[15*12]),width,height,-15.,15.,mem = .true.,memsymfreq = 5,memlabel = 'HAK',maskbelow = 0.,memflqt = -1,LI = .true.)
        call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)
        call plot(0.,-height-1.,-3)
        call butler_linegraph(reshape(transpose(mutsuoga),[15*12]),width,height,-15.,15.,mem = .true.,memsymfreq = 5,memlabel = 'MUTSUOGA',maskbelow = 0.,memflqt = -1,LI = .true.)
        call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)

    ! relative differences of stations
        call newpage(h = 'differences of stations')
        mat(14,8) = 0. ! removing the outlier at MAT
        mat_hak = 0.; tap_mutsu = 0.; tap_hak = 0.; tap_mat = 0.
        do i = 1, 15
            do j = 1, 12
                if(mat(i,j)/=0. .and. hak(i,j)/=0.)then 
                    mat_hak(i,j) = mat(i,j) - hak(i,j)
                end if
                if(tap(i,j)/=0. .and. mutsuoga(i,j)/=0.)then 
                    tap_mutsu(i,j) = tap(i,j) - mutsuoga(i,j)
                end if
                if(tap(i,j)/=0. .and. hak(i,j)/=0.)then 
                    tap_hak(i,j) = tap(i,j) - hak(i,j)
                end if
                if(tap(i,j)/=0. .and. mat(i,j)/=0.)then 
                    tap_mat(i,j) = tap(i,j) - mat(i,j)
                end if
            end do
        end do

        call butler_linegraph(reshape(transpose(mat_hak),[15*12]),width,height,-6.,6.,mem = .true.,memsymfreq = 2,memlabel = 'MAT-HAK',maskbelow = 0.,memflqt = -1,LI = .true.)   
        call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)
        call plot(0.,-height-1.,-3)
        call butler_linegraph(reshape(transpose(tap_mutsu),[15*12]),width,height,-6.,6.,mem = .true.,memsymfreq = 2,memlabel = 'TAP-MUTSUOGA',maskbelow = 0.,memflqt = -1,LI = .true.)
        call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)
        call plot(0.,-height-1.,-3)
        call butler_linegraph(reshape(transpose(tap_hak),[15*12]),width,height,-6.,6.,mem = .true.,memsymfreq = 2,memlabel = 'TAP-HAK',maskbelow = 0.,memflqt = -1,LI = .true.)
        call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)
        call plot(0.,-height-1.,-3)
        call butler_linegraph(reshape(transpose(tap_mat),[15*12]),width,height,-6.,6.,mem = .true.,memsymfreq = 2,memlabel = 'TAP-MAT',maskbelow = 0.,memflqt = -1,LI = .true.)
        call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)

    ! upstream downstream
        !upstream
        call SSH_data(upstream(1,:,:),slabel = '深浦',calibrate = .true.,diff_from_yearly_mean = .true.)
        call SSH_data(upstream(2,:,:),slabel = '竜飛',calibrate = .true.,diff_from_yearly_mean = .true.)
        call SSH_data(upstream(3,:,:),slabel = '松前港',calibrate = .true.,diff_from_yearly_mean = .true.)
        call SSH_data(upstream(4,:,:),slabel = '江差港',calibrate = .true.,diff_from_yearly_mean = .true.)

        call SSH_data(downstream(1,:,:),slabel = '苫小牧西港',calibrate = .true.,diff_from_yearly_mean = .true.)
        call SSH_data(downstream(2,:,:),slabel = '室蘭港',calibrate = .true.,diff_from_yearly_mean = .true.)
        call SSH_data(downstream(3,:,:),slabel = 'むつ小川原港',calibrate = .true.,diff_from_yearly_mean = .true.)
        call SSH_data(downstream(4,:,:),slabel = '久慈港',calibrate = .true.,diff_from_yearly_mean = .true.)


        call newpage(h = 'upstream')
        call butler_linegraph(reshape(transpose(upstream(1,:,:)),[15*12]),width,height,-15.,15.,mem = .true.,memsymfreq = 5,memlabel = 'fuk',maskbelow = 0.,memflqt = -1,LI = .true.)
        call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)
        call plot(0.,-height-1.,-3)
        call butler_linegraph(reshape(transpose(upstream(2,:,:)),[15*12]),width,height,-15.,15.,mem = .true.,memsymfreq = 5,memlabel = 'tap',maskbelow = 0.,memflqt = -1,LI = .true.)
        call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)
        call plot(0.,-height-1.,-3)
        call butler_linegraph(reshape(transpose(upstream(3,:,:)),[15*12]),width,height,-15.,15.,mem = .true.,memsymfreq = 5,memlabel = 'mat',maskbelow = 0.,memflqt = -1,LI = .true.)
        call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)
        call plot(0.,-height-1.,-3)
        call butler_linegraph(reshape(transpose(upstream(4,:,:)),[15*12]),width,height,-15.,15.,mem = .true.,memsymfreq = 5,memlabel = 'esa',maskbelow = 0.,memflqt = -1,LI = .true.)
        call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)
        
        call newpage(h = 'downstream')
        call butler_linegraph(reshape(transpose(downstream(1,:,:)),[15*12]),width,height,-15.,15.,mem = .true.,memsymfreq = 5,memlabel = 'tomakomai',maskbelow = 0.,memflqt = -1,LI = .true.)
        call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)
        call plot(0.,-height-1.,-3)
        call butler_linegraph(reshape(transpose(downstream(2,:,:)),[15*12]),width,height,-15.,15.,mem = .true.,memsymfreq = 5,memlabel = 'muroran',maskbelow = 0.,memflqt = -1,LI = .true.)
        call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)
        call plot(0.,-height-1.,-3)
        call butler_linegraph(reshape(transpose(downstream(3,:,:)),[15*12]),width,height,-15.,15.,mem = .true.,memsymfreq = 5,memlabel = 'mutsuoga',maskbelow = 0.,memflqt = -1,LI = .true.)
        call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)
        call plot(0.,-height-1.,-3)
        call butler_linegraph(reshape(transpose(downstream(4,:,:)),[15*12]),width,height,-15.,15.,mem = .true.,memsymfreq = 5,memlabel = 'hachinohe',maskbelow = 0.,memflqt = -1,LI = .true.)
        call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)

    ! upstream mean - downstream mean
        call newpage(h = 'upstream and downstream')
        call avsemdata_3D(upstream,'dim1',mean_2D = upmean) ! regional means
        call avsemdata_3D(downstream,'dim1',mean_2D = downmean)
        call avsemdata_2D(upmean-downmean,'dim1',mean_1D = upminusdown_mean,sem_1D = upminusdown_sem)
        
        call butler_linegraph(reshape(transpose(upmean),[15*12]),width,height,-15.,15.,mem = .true.,memsymfreq = 5,memlabel = 'upstream mean',maskbelow = 0.,memflqt = -1,LI = .true.)
        call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)
        call plot(0.,-height-1.,-3)
        call butler_linegraph(reshape(transpose(downmean),[15*12]),width,height,-15.,15.,mem = .true.,memsymfreq = 5,memlabel = 'downstream mean',maskbelow = 0.,memflqt = -1,LI = .true.)
        call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)
        call plot(0.,-height-1.,-3)
        call butler_linegraph(reshape(transpose(upmean-downmean),[15*12]),width,height,-6.,6.,mem = .true.,memsymfreq = 2,memlabel = 'upstream - downstream',maskbelow = 0.,memflqt = -1,LI = .true.)
        call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)
        call plot(0.,-height-1.,-3)
        call newpage(h = 'upstream - downstream mean')
        call plot(0.,-height-1.,-3)
        call butler_linegraph([upminusdown_mean,upminusdown_mean(1)],width2*0.7,height,-6.,6.,mem = .true.,memsymfreq = 2,tlabel = 'upstream - downstream',maskbelow = 0.,memflqt = -1,LI = .true.,error_1D = [upminusdown_sem,upminusdown_sem(1)])
        call mod12_memori(13,width2*0.7,gap = 2,symbol_size = 0.5)
        call plot(width2*0.7+1.,0.,-3)

        call avsemdata_2D(upmean,'dim1',mean_1D = upmean2,sem_1D = upsem2)! monthly means of upstream
        call avsemdata_2D(downmean,'dim1',mean_1D = downmean2,sem_1D = downsem2)! monthly means of downstream

        call butler_linegraph([upmean2,upmean2(1)],width2*0.7,height,-15.,15.,mem = .true.,memsymfreq = 5,tlabel = 'upstream mean',maskbelow = 0.,memflqt = -1,LI = .true.,error_1D = [upsem2,upsem2(1)])
        call mod12_memori(13,width2*0.7,gap = 2,symbol_size = 0.5)
        call plot(width2*0.7+1.,0.,-3)
        call butler_linegraph([downmean2,downmean2(1)],width2*0.7,height,-15.,15.,mem = .true.,memsymfreq = 5,tlabel = 'downstream mean',maskbelow = 0.,memflqt = -1,LI = .true.,error_1D = [downsem2,downsem2(1)])
        call mod12_memori(13,width2*0.7,gap = 2,symbol_size = 0.5)


    ! yearly means of differences
        call avsemdata_2D(mat_hak,'dim1',mean_1D = mat_hak_mean,sem_1D = mat_hak_sem)
        call avsemdata_2D(tap_mutsu,'dim1',mean_1D = tap_mutsu_mean,sem_1D = tap_mutsu_sem)
        call avsemdata_2D(tap_hak,'dim1',mean_1D = tap_hak_mean,sem_1D = tap_hak_sem)
        call avsemdata_2D(tap_mat,'dim1',mean_1D = tap_mat_mean,sem_1D = tap_mat_sem)
        call newpage(h = 'yearly means of differences')
        call butler_linegraph([mat_hak_mean,mat_hak_mean(1)],width2,height,-6.,6.,mem = .true.,memsymfreq = 2,memlabel = 'MAT-HAK',maskbelow = 0.,memflqt = -1,LI = .true.,error_1D = [mat_hak_sem,mat_hak_sem(1)])
        call mod12_memori(13,width2,gap = 2,symbol_size = 0.8)
        call plot(0.,-height-1.,-3)
        call butler_linegraph([tap_mutsu_mean,tap_mutsu_mean(1)],width2,height,-6.,6.,mem = .true.,memsymfreq = 2,memlabel = 'TAP-MUTSUOGA',maskbelow = 0.,memflqt = -1,LI = .true.,error_1D = [tap_mutsu_sem,tap_mutsu_sem(1)])
        call mod12_memori(13,width2,gap = 2,symbol_size = 0.8)
        call plot(0.,-height-1.,-3)
        call butler_linegraph([tap_hak_mean,tap_hak_mean(1)],width2,height,-6.,6.,mem = .true.,memsymfreq = 2,memlabel = 'TAP-HAK',maskbelow = 0.,memflqt = -1,LI = .true.,error_1D = [tap_hak_sem,tap_hak_sem(1)])
        call mod12_memori(13,width2,gap = 2,symbol_size = 0.8)
        call plot(0.,-height-1.,-3)
        call butler_linegraph([tap_mat_mean,tap_mat_mean(1)],width2,height,-6.,6.,mem = .true.,memsymfreq = 2,memlabel = 'TAP-MAT',maskbelow = 0.,memflqt = -1,LI = .true.,error_1D = [tap_mat_sem,tap_mat_sem(1)])
        call mod12_memori(13,width2,gap = 2,symbol_size = 0.8)
        call plot(0.,-height-1.,-3)

        call SSH_data(fuk,slabel = '深浦',calibrate = .true.,diff_from_yearly_mean = .true.)

        call plot(xorigin + width2+ 2.,yorigin,-3)
        do i = 1, 15
            do j = 1, 12
                if(fuk(i,j)/=0..and.mat(i,j)/=0.)then 
                    fuk_mat(i,j) = fuk(i,j) - mat(i,j)
                else
                    fuk_mat(i,j) = 0.
                end if
            end do
        end do
        call avsemdata_2D(fuk_mat,'dim1',mean_1D = fuk_mat_mean,sem_1D = fuk_mat_sem)
        call butler_linegraph([fuk_mat_mean,fuk_mat_mean(1)],width2,height,-6.,6.,mem = .true.,memsymfreq = 2,memlabel = 'FUK-MAT',maskbelow = 0.,memflqt = -1,LI = .true.,error_1D = [fuk_mat_sem,fuk_mat_sem(1)])
        call mod12_memori(13,width2,gap = 2,symbol_size = 0.8)



    !! absolute values


        ! call newpage(h = 'absolute values')

        ! call SSH_data(tap,slabel = '竜飛',calibrate = .true.,diff_from_yearly_mean = .false.)
        ! call SSH_data(mat,slabel = '松前港',calibrate = .true.,diff_from_yearly_mean = .false.)
        ! call SSH_data(mutsuoga,slabel = 'むつ小川原港',calibrate = .true.,diff_from_yearly_mean = .false.)
        ! call SSH_data(hak,slabel = '函館',calibrate = .true.,diff_from_yearly_mean = .false.)

        ! call butler_linegraph(reshape(transpose(tap),[15*12]),width,height,0.,200.,mem = .true.,memsymfreq = 50,memlabel = 'TAP',memflqt = -1,LI = .true.)
        ! call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)
        ! call plot(0.,-height-1.,-3)
        ! call butler_linegraph(reshape(transpose(mat),[15*12]),width,height,0.,200.,mem = .true.,memsymfreq = 50,memlabel = 'MAT',memflqt = -1,LI = .true.)
        ! call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)
        ! call plot(0.,-height-1.,-3)
        ! call butler_linegraph(reshape(transpose(hak),[15*12]),width,height,0.,200.,mem = .true.,memsymfreq = 50,memlabel = 'HAK',memflqt = -1,LI = .true.)
        ! call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)
        ! call plot(0.,-height-1.,-3)
        ! call butler_linegraph(reshape(transpose(mutsuoga),[15*12]),width,height,0.,200.,mem = .true.,memsymfreq = 50,memlabel = 'MUTSUOGA',memflqt = -1,LI = .true.)
        ! call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)

    ! asamushi and hakodate
        call SSH_data(asamushi,slabel = '浅虫',calibrate = .true.,diff_from_yearly_mean = .true.)

        call newpage(h = 'asamushi and hakodate')
        call butler_linegraph(reshape(transpose(asamushi),[15*12]),width,height,-15.,15.,mem = .true.,memsymfreq = 5,memlabel = 'Asamushi',memflqt = -1,LI = .true.)
        call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)
        call plot(0.,-height-1.,-3)
        call butler_linegraph(reshape(transpose(hak),[15*12]),width,height,-15.,15.,mem = .true.,memsymfreq = 5,memlabel = 'Hakodate',memflqt = -1,LI = .true.)
        call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)

        do i = 1, 15
            do j = 1, 12
                if(asamushi(i,j)/=0. .and. hak(i,j)/=0.)then 
                    asa_hak(i,j) = asamushi(i,j) - hak(i,j)
                else
                    asa_hak(i,j) = 0.
                end if
            end do
        end do

        call plot(0.,-height-1.,-3)
        call butler_linegraph(reshape(transpose(asa_hak),[15*12]),width,height,-6.,6.,mem = .true.,memsymfreq = 2,memlabel = 'Asamushi-Hakodate',maskbelow = 0.,memflqt = -1,LI = .true.)
        call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)
        call plot(0.,-height-1.,-3)

        ! asamushi - hakodate mean
        call avsemdata_2D(asa_hak,'dim1',mean_1D = asa_hak_mean,sem_1D = asa_hak_sem)
        call butler_linegraph([asa_hak_mean,asa_hak_mean(1)],width2*0.7,height,-6.,6.,mem = .true.,memsymfreq = 2,memlabel = 'Asamushi-Hakodate',maskbelow = 0.,memflqt = -1,LI = .true.,error_1D = [asa_hak_sem,asa_hak_sem(1)])
        call mod12_memori(13,width2*0.7,gap = 2,symbol_size = 0.5)

    ! aomori - hakodate
        call SSH_data(aomori,slabel = '青森港',calibrate = .true.,diff_from_yearly_mean = .true.)
        call newpage(h = 'aomori and hakodate')
        call butler_linegraph(reshape(transpose(aomori),[15*12]),width,height,-15.,15.,mem = .true.,memsymfreq = 5,memlabel = 'Aomori',memflqt = -1,LI = .true.)
        call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)
        call plot(0.,-height-1.,-3)
        call butler_linegraph(reshape(transpose(hak),[15*12]),width,height,-15.,15.,mem = .true.,memsymfreq = 5,memlabel = 'Hakodate',memflqt = -1,LI = .true.)
        call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)

        do i = 1, 15
            do j = 1, 12
                if(aomori(i,j)/=0. .and. hak(i,j)/=0.)then 
                    aomori_hak(i,j) = aomori(i,j) - hak(i,j)
                else
                    aomori_hak(i,j) = 0.
                end if
            end do
        end do

        call plot(0.,-height-1.,-3)
        call butler_linegraph(reshape(transpose(aomori_hak),[15*12]),width,height,-6.,6.,mem = .true.,memsymfreq = 2,memlabel = 'Aomori-Hakodate',maskbelow = 0.,memflqt = -1,LI = .true.)
        call mod12_memori(180,width,gap = 2,symbol_size = 0.4,num_freq = 6)
        call plot(0.,-height-1.,-3)

        ! aomori - hakodate mean
        call avsemdata_2D(aomori_hak,'dim1',mean_1D = aomori_hak_mean,sem_1D = aomori_hak_sem)
        call butler_linegraph([aomori_hak_mean,aomori_hak_mean(1)],width2*0.7,height,-6.,6.,mem = .true.,memsymfreq = 2,memlabel = 'Aomori-Hakodate',maskbelow = 0.,memflqt = -1,LI = .true.,error_1D = [aomori_hak_sem,aomori_hak_sem(1)])
        call mod12_memori(13,width2*0.7,gap = 2,symbol_size = 0.5)
        call plot(0.,-height-1.,-3)
        

    call plote
end program
