program testing_butler_psbet
    use always 
    implicit none 
    real,dimension(:),allocatable::r1,g1,b1


    call plots2(oopt = 'ocenter',x = 2.)
    ! call calibrated_data2(sal_c5 = sal_c5)

    call b2r_colorgrad(10,6,r1,g1,b1)
    call colorscale(r1,g1,b1,0.,5.,1,0.5,1,10.,0.2,lt = 1, gt = 1)
    call ocenter(y = -4.)
    call butler_linegraph(r1,6.,8.,0.,1.,mem=.true.,memlabel = 'wow',blabel = 'bottom',tlabel = 'top',memloc = 'left',memsymfreq = 1,memiter = 6,rotation = -90.,memflqt = 1)
    call ocenter()
    call num_memori(0.,10.,11,5,rangle = -90.,length = 4.,rotatenumbers = .true.,float_quantity = 1)
    ! call simple_map(132,142,42,46,5.,symbols = .true.)

    call plote
    

end program 
