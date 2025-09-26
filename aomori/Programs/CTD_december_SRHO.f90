program unko
    use always
    real,dimension(:,:),allocatable::meanPTdec,meanSdec
    real,dimension(6,400)::uniformS,meanSigma_uniformS,Sigma_diff,meanSigmadec

    call calibrated_data2(potemp_c5,sal_c5,sigma_c5)

    ! call avsemdata_3D(sigma_c5(:,12,1,4:9,:),'dim1',mean_2D = meanSigmadec) ! 6 stations, 400m depth for december Nline
    call avsemdata_3D(potemp_c5(:,12,1,4:9,:),'dim1',mean_2D = meanPTdec) ! 6 stations, 400m depth for december Nline
    call avsemdata_3D(sal_c5(:,12,1,4:9,:),'dim1',mean_2D = meanSdec) ! 6 stations, 400m depth for december Nline
    ! uniformS(:,:110) = 34.1
    ! uniformS(:,111:) = meanSdec(:,111:) ! uniform salinity profile with 34.1 for the first 110 layers
    do i = 1,6
        do j = 1, 400
            if(meanSdec(i,j)<34.07) then 
                cycle
            else
                uniformS(i,:j-1) = 34.07
                uniformS(i,j:) = meanSdec(i,j:)
                exit
            end if
        end do
    end do
    ! uniformS(1,:) = meanSdec(1,:) ! salinity is < 34.1 for all depth in station 6(i = 1)
        
    call calc_density(meanPTdec,uniformS,meanSigma_uniformS,depth = .true.) ! density profile when salinity is uniform
    call calc_density(meanPTdec,meanSdec,meanSigmadec,depth = .true.) ! density profile with real salinity

    call plots2(nnfile = 'CTD_december',oopt = 'otops')
    call plotsave('first')

    width = 3.; height = 6.
    ! with real salinity
    call butler_psk(meanSdec,width,-height,0.,33.95,34.3,0.05,'b2w2r',7,4)
    call butler_cont(meanPTdec,width,-height,-999.,0.,1.,thicc = 5)
    call plot(width+2.,0.,-3)

    ! with uniform salinity
    call butler_psk(uniformS,width,-height,0.,33.95,34.3,0.05,'b2w2r',7,4)
    call butler_cont(meanPTdec,width,-height,-999.,0.,1.,thicc = 5)
    call plot(width+2.,0.,-3)

    ! density profile
    call plotback('first');call plot(0.,-height-2.,-3)
    
    call butler_psmask(meanSigmadec,width,-height,25.,26.,r = 1.,g = 0.9, b = 0.9)
    call butler_psmask(meanSigmadec,width,-height,26.,27.,r = 1., g = .5, b = 0.5)
    call butler_psmask(meanSigmadec,width,-height,27.,28.,r = 1., g = 0.2, b = 0.2)
    call butler_cont(meanSigmadec,width,-height,0.,20.,0.2,thicc = 5)

    call plot(width+2.,0.,-3)

    call butler_psmask(meanSigma_uniformS,width,-height,25.,26.,r = 1.,g = 0.9, b = 0.9)
    call butler_psmask(meanSigma_uniformS,width,-height,26.,27.,r = 1., g = .5, b = 0.5)
    call butler_psmask(meanSigma_uniformS,width,-height,27.,28.,r = 1., g = 0.2, b = 0.2)
    call butler_cont(meanSigma_uniformS,width,-height,0.,20.,0.2,thicc = 5)

    ! difference between the two density profiles
    Sigma_diff = meanSigmadec - meanSigma_uniformS
    ! sigma_diff(:,111:) = 0. ! set the last 290 layers to zero
    call plot(width+2.,0.,-3)

    ! call butler_psmask(Sigma_diff,width,-height,-0.3,-0.2,r = .2,g = 0.2,b = 1.)
    ! call butler_psmask(Sigma_diff,width,-height,-0.2,-0.1,r = .5,g = 0.5,b = 1.)
    ! call butler_psmask(Sigma_diff,width,-height,-0.1,0.,r = 0.8,g = 0.8,b = .8)
    ! call butler_psmask(Sigma_diff,width,-height,0.,0.1,r = 1.,g = 1.,b = .9)
    call butler_cont(Sigma_diff,width,-height,-999.,-1.,0.1,thicc = 5,maskn = .true.)

    ! do i = 1,400
    !     print*, Sigma_diff(1,i),Sigma_diff(2,i),Sigma_diff(3,i),Sigma_diff(4,i),Sigma_diff(5,i),Sigma_diff(6,i)
    ! end do
    do i = 1, 400
        print*,uniformS(6,i), meanSdec(6,i),meanSigma_uniformS(6,i),meanSigmadec(6,i),Sigma_diff(6,i)
    end do

    call plote
end program
