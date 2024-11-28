program brunt_vaisala_freq
    use always
    implicit none  
    real,dimension(:,:,:,:),allocatable::avpotemp,avsal
    real,dimension(6,400)::sigma2d
    real,dimension(6,400)::Nsq=0.0
    real,dimension(:),allocatable::Nsqdepth
    real::Nsqtotalmean

    call calibrated_data51(potemp_c5,sal_c5) ! 15*12*2*9*400
    call avsemdata_5D(potemp_c5,'dim1',mean_4D = avpotemp)
    call avsemdata_5D(sal_c5,'dim1',mean_4D = avsal)
    call calc_density(avpotemp(9,1,4:9,:),avsal(9,1,4:9,:),sigma2d)

    do i = 1, 6
        do j = 1,400
            if(j == 400)cycle
            Nsq(i,j) = -g*((1000.+sigma2d(i,j))-(1000.+sigma2d(i,j+1)))/(1000.+sigma2d(i,j))
        end do
    end do
    call openlog(basename = 'brunt_vaisala_freq')
    do j = 1,400
        write(tolog,*)Nsq(:,j)
    end do
    call avsemdata_2D(Nsq,'dim1',mean_1D = Nsqdepth)
    call avsemdata_1D(Nsqdepth,mean = Nsqtotalmean)
    write(tolog,*)Nsqdepth,'Nsqdepth'
    write(tolog,*)Nsqtotalmean,'Nsqtotalmean'
    
    call closelog

end program

