program sshagain
    use always
    implicit none
    real,dimension(15,12)::fuk
    real,dimension(12*15)::fukseries,fuk_fukmeanseries
    real,dimension(:),allocatable::fukmean,fuksem
    

    call ssh_data(fuk,slabel = '深浦',convert = .true.,calibrate = .true.,diff_from_yearly_mean = .true.)
    fuk = fuk/10.
    call avsemdata_2D(fuk,'dim1',mean_1D = fukmean,sem_1D = fuksem)
    ! print*,fuksem
    fukseries = reshape(transpose(fuk),[12*15]) 

    do i = 1, 180
        if(mod(i,12) == 0)then
            fuk_fukmeanseries(i) = fukseries(i) - fukmean(12)
        else
            fuk_fukmeanseries(i) = fukseries(i) - fukmean(mod(i,12))
        end if
    end do

    call plots2(nnfile = 'fukaura',oopt = 'otops',x = 2.,y = -5.5,h = 'Fukaura Tides')

    call symbolc(23./2.,5.8,0.8,'(Pressure Corrected SSH) - (Monthly Means)')
    call butler_linegraph(fuk_fukmeanseries,23.,5.,-10.,10.,0.,.true.,memsymfreq = 5,LI = .true.,maskbelow = 0.)
    call mod12_memori(180,23.,0.3,num_freq = 6,gap = 2)
    call floating_numbers(2009.,1.,15,0.5,23./15.,0.,0.,-1,x = 23./30.,y = 5.1)
    call plot(0.,-7.,-3)

    call symbolc(23./2.,5.8,0.8,'Pressure Corrected SSH')
    call butler_linegraph(fukseries,23.,5.,-20.,20.,0.,.true.,memsymfreq = 5,LI = .true.,maskbelow = 0.)
    call mod12_memori(180,23.,0.3,num_freq = 6,gap = 2)
    call floating_numbers(2009.,1.,15,0.5,23./15.,0.,0.,-1,x = 23./30.,y = 5.1)

    call plot(0.,-5.,-3)
    call butler_linegraph(fukmean,10.,4.,-20.,20.,0.,.true.,memsymfreq = 5,maskbelow = 0.,error_1D = fuksem)
    call mod12_memori(12,10.,0.3,num_freq = 1,gap = 2)
    call symbol(11.,2.,0.8,'<- Monthly Means')

    call plote

end program 