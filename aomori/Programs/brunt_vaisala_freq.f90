program brunt_vaisala_freq
    use always
    implicit none  
    real,dimension(:,:,:,:),allocatable::avpotemp,avsal
    real,dimension(6,400)::sigma2d
    real,dimension(6,400)::Nsq=0.0
    real,dimension(:),allocatable::Nsqdepth,sigmastmean
    real :: Nsqtotalmean,sigmatotmean
    real :: BruntVaisalaFreq

    call calibrated_data51(potemp_c5,sal_c5) ! 15*12*2*9*400
    call avsemdata_5D(potemp_c5,'dim1',mean_4D = avpotemp)
    call avsemdata_5D(sal_c5,'dim1',mean_4D = avsal)
    call calc_density(avpotemp(9,1,4:9,:),avsal(9,1,4:9,:),sigma2d)
    call avsemdata_2D(sigma2d,'dim1',mean_1D = sigmastmean) ! average density over 6 stations
    call avsemdata_1D(sigmastmean,mean = sigmatotmean) ! average density over 6 stations and 400m depth
    print*,'Average density (kg/m^3) = ',sigmatotmean ! average density over 6 stations and 400m depth

    do i = 1, 6
        do j = 1,400
            if(j == 400)cycle
            Nsq(i,j) = -9.81/sigmatotmean*((1000.+sigma2d(i,j))-(1000.+sigma2d(i,j+1))) ! N^2 = -g/rho0*(rho(z)-rho(z+1)/dz
            ! print*,i,j,Nsq(i,j),'Nsq(i,j)'
        end do
    end do

    call avsemdata_2D(Nsq,'dim1',mean_1D = Nsqdepth)
    ! print*,Nsqdepth,'Nsqdepth'
    call avsemdata_1D(Nsqdepth,mean = Nsqtotalmean)
    print*,Nsqtotalmean,'Nsqtotalmean'
    BruntVaisalaFreq = sqrt(Nsqtotalmean)
    print*,'Brunt-Vaisala frequency (s^-1) = ',BruntVaisalaFreq ! N^2 = -g*(rho2-rho1)/dz, N = sqrt(N^2)

    print*,'NH/f (m) = ',BruntVaisalaFreq*400./(2.*7.2921E-5*sin(41.*pi/180.)) ! NH/f  where H is the depth of the water column (400m), f is the coriolis parameter at 41N
    print*,'Nh/f (km) = ',BruntVaisalaFreq*400./(2.*7.2921E-5*sin(41.*pi/180.))*1.E-3 ! NH/f  where H is the depth of the water column (400m), f is the coriolis parameter at 41N


end program

