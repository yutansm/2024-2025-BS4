program unknown
    use always
    real,dimension(15,12)::tap,mat,tap_mat
    real,dimension(:),allocatable::tap_mat_mean,tap_mat_sem,tap_mean,mat_mean,tap_sem,mat_sem


    width = 25.
    height = 4.
    call SSH_data(tap,slabel = '竜飛',calibrate = .true.,diff_from_yearly_mean = .true.,unit = 'cm')
    call SSH_data(mat,slabel = '松前港',calibrate = .true.,diff_from_yearly_mean = .true.,unit = 'cm')

    call plots2(nnfile = 'TAP-MAT_final',oopt = 'otops',x = 1.5,y = -height+1.)
    call plotsave('first')

    call symbolc(width/2.,height+0.8,0.8,'TAP')
    call butler_linegraph(reshape(transpose(tap),[15*12]),width,height,-15.,15.,memsymfreq = 5,maskbelow = 0.,lthick = 4,mem = .true.,memlabel = '[cm]')
    call mod12_memori(180,width,0.4,gap = 2,num_freq = 6,dxval = dx)
    call floating_numbers(2009.,1.,15,0.6,12*dx,0.,0.,-1,x = 6.5*dx,y = height+0.1)
    call floating_lines(height,90.,15,3,12*dx,0.,x = .5*dx,dashy = -6,r = 0.4,g = 0.4,b = 0.4)
    
    call plot(0.,-height-2.5,-3)

    call symbolc(width/2.,height+0.8,0.8,'MAT')
    call butler_linegraph(reshape(transpose(mat),[15*12]),width,height,-15.,15.,memsymfreq = 5,maskbelow = 0.,lthick = 4,mem = .true.)
    call mod12_memori(180,width,0.4,gap = 2,num_freq = 6)
    call floating_numbers(2009.,1.,15,0.6,12*dx,0.,0.,-1,x = 6.5*dx,y = height+0.1)
    call floating_lines(height,90.,15,3,12*dx,0.,x = .5*dx,dashy = -6,r = 0.4,g = 0.4,b = 0.4)
    do i = 1, 15
        do j = 1, 12
            if(tap(i,j) == 0. .or. mat(i,j) == 0.) then
                tap_mat(i,j) = 0.
            else
                tap_mat(i,j) = tap(i,j) - mat(i,j)
            end if
        end do
    end do

    tap_mat(14,8) = 0.

    call plot(0.,(-height-2.5),-3)

    call symbolc(width/2.,height+0.8,0.8,'TAP-MAT')
    call butler_linegraph(reshape(transpose(tap_mat),[15*12]),width,height,-6.,6.,memsymfreq = 2,maskbelow = 0.,lthick = 4,mem = .true.)
    call mod12_memori(180,width,0.4,gap = 2,num_freq = 6)
    call floating_numbers(2009.,1.,15,0.6,12*dx,0.,0.,-1,x = 6.5*dx,y = height+0.1)
    call floating_lines(height,90.,15,3,12*dx,0.,x = .5*dx,dashy = -6,r = 0.4,g = 0.4,b = 0.4)

    call avsemdata_2D(tap_mat,'dim1',mean_1D = tap_mat_mean,sem_1D = tap_mat_sem)

    open(123,file = '../Data/TAP_MAT_mean.bin',form = 'unformatted',status = 'replace',access = 'direct',recl = 13*4,convert = 'big_endian')
    write(123,rec = 1) [tap_mat_mean,tap_mat_mean(1)]
    write(123,rec = 2) [tap_mat_sem,tap_mat_sem(1)]
    close(123)

    call newpage('first', y = -5.)
    
    call avsemdata_2D(tap,'dim1',mean_1D = tap_mean,sem_1D = tap_sem)
    call avsemdata_2D(mat,'dim1',mean_1D = mat_mean,sem_1D = mat_sem)

    call butler_linegraph([mat_mean,mat_mean(1)],width/3.,height*2,-15.,15.,memsymfreq = 1,tlabel = 'TAP,MAT',mem = .true.,error_1D = [mat_sem,mat_sem(1)],lthick = 7,rl = 0.6,gl = 0.6,bl = 0.6,memfreq = 5.)
    call butler_linegraph([tap_mean,tap_mean(1)],width/3.,height*2,-15.,15.,memsymfreq = 1,lthick = 4,mem = .true.,error_1D = [tap_sem,tap_sem(1)],memfreq = 5.)
    call mod12_memori(13,width/3.,0.6,gap = 2,num_freq = 1)

    call plot(width/3.+3.,0.,-3)
    call butler_linegraph([tap_mat_mean,tap_mat_mean(1)],width/3.,height*2,-6.,6.,memsymfreq = 2,maskbelow = 0.,lthick = 4,tlabel = 'TAP-MAT mean',mem = .true.,error_1D = [tap_mat_sem,tap_mat_sem(1)])
    call mod12_memori(13,width/3.,0.6,gap = 2,num_freq = 1)

    call plote
end program  