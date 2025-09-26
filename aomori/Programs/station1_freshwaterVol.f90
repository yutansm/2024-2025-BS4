program Shita 
    use always
    real,dimension(:,:,:),allocatable::meanPT,meanS,meanSigma ! month * station * depth
    real,dimension(:,:),allocatable::meanPTst14,meanSst14,meanSigmast14 ! month * station 1-4
    real,dimension(:),allocatable::meanSst14_200mean,meanSst1_200mean
    real,dimension(13)::freshwatervolume,freshwatervolumest1
    real,dimension(13,9,400)::meanPTloop,meanSloop,meanSigmaloop ! months * station * depth
    real,dimension(:),allocatable::r,g,b
    real::lon1deg,lat1deg,gridvolume
    call calibrated_data2(potemp_c5 = potemp_c5, sal_c5 = sal_c5, sigma_c5 = sigma_c5) ! flipped indices
    width = 8.;height = 5.

    ! temp mean for each station
        call avsemdata_4D(potemp_c5(:,:,1,:,:),'dim1',mean_3D = meanPT)
        meanPTloop(1:12,:,:) = meanPT;meanPTloop(13,:,:) = meanPT(1,:,:) ! 13th month is the same as the first month
    ! sal mean for each station
        call avsemdata_4D(sal_c5(:,:,1,:,:),'dim1',mean_3D = meanS)
        meanSloop(1:12,:,:) = meanS;meanSloop(13,:,:) = meanS(1,:,:) ! 13th month is the same as the first month
    ! sigma mean for each station
        call avsemdata_4D(sigma_c5(:,:,1,:,:),'dim1',mean_3D = meanSigma)
        meanSigmaloop(1:12,:,:) = meanSigma;meanSigmaloop(13,:,:) = meanSigma(1,:,:) ! 13th month is the same as the first month

    call plots2(nnfile = 'meanPTSRHOperstation',oopt = 'otops',x = 1.,h = 'Mean PT, S, Sigma at each station')
    call plotsave('first')

    do i = 1, 9
        call symbolc(width/2.,0.2,0.7,'station'//trim(adjustl(int2str(10-i))))
        if(i == 1.or.i == 4.or.i == 7)call num_memori2(0.,400.,-height,100.,-90.,float_quantity = -1)
        if(i >=7)call mod12_memori(13,width,0.7,y = -height)
        call butler_psk(meanSloop(:,i,:),width,-height,0.,33.95,34.3,0.05,'b2r',7,bpt1 = 4,centralize = 4)
        ! call butler_cont(meanPTloop(:,i,:),width,-height,0.,0.,1.,thicc = 5)
        call butler_cont(meanSigmaloop(:,i,:),width,-height,0.,20.,1.,thicc = 1, r = 1.,g = 1.,b= 1.)
        call butler_cont(meanSigmaloop(:,i,:),width,-height,0.,27.,0.1,thicc = 5)
        call plot(width + 0.5,0.,-3)

        if(i == 3.or.i == 6)then 
            call plotback('first')
            call plot(0.,-(height+1.)*i/3,-3)
        endif
    end do

    ! mean PT and S for stations 1-4
        call avsemdata_3D(meanPTloop(:,6:9,:),'dim2',mean_2D = meanPTst14)
        meanPTst14(1,:) = (meanPTst14(12,:)+meanPTst14(2,:))/2. ! linear interpolation
        meanPTst14(7,:) = (meanPTst14(6,:)+meanPTst14(8,:))/2.
        meanPTst14(13,:) = meanPTst14(1,:)
    ! mean S for stations 1-4
        call avsemdata_3D(meanSloop(:,6:9,:),'dim2',mean_2D = meanSst14)
        meanSst14(1,:) = (meanSst14(12,:)+meanSst14(2,:))/2.
        meanSst14(7,:) = (meanSst14(6,:)+meanSst14(8,:))/2.
        meanSst14(13,:) = meanSst14(1,:)
    ! mean Sigma for stations 1-4
        call avsemdata_3D(meanSigmaloop(:,6:9,:),'dim2',mean_2D = meansigmast14)
        meansigmast14(1,:) = (meansigmast14(12,:)+meansigmast14(2,:))/2.
        meansigmast14(7,:) = (meansigmast14(6,:)+meansigmast14(8,:))/2.
        meansigmast14(13,:) = meansigmast14(1,:)

    call newpage('first')

    call num_memori2(0.,300.,-height,100.,-90.,float_quantity = -1)
    call butler_psk(meanSst14(:,:300),width,-height,0.,33.95,34.3,0.05,'b2r',7,bpt1 = 4,centralize = 4,r = r,g = g, b = b)
    ! call butler_cont(meanPTst14(:,:300),width,-height,0.,0.,1.,thicc = 5)
    call butler_cont(meansigmast14(:,:300),width,-height,0.,20.,1.,thicc = 1, r = 1.,g = 1.,b= 1.)
    call butler_cont(meansigmast14(:,:300),width,-height,0.,27.,0.1,thicc = 5)
    call colorscale(r,g,b,33.95,34.3,2,0.7,1,width,0.3,lt = 1, gt = 1,x = width/2.,y = -height-2.,symbol_start = 2)
    call mod12_memori(13,width,0.7,y = -height)
    call plot(width + 3.,0.,-3)

    call butler_psk(meanSst14(:,:300),width,-height,0.,33.8,34.4,0.1,'b2r',6,bpt1 = 4,r = r, g = g, b = b)
    call butler_cont(meansigmast14(:,:300),width,-height,0.,20.,1.,thicc = 1, r = 1.,g = 1.,b= 1.)
    call butler_cont(meansigmast14(:,:300),width,-height,0.,27.,0.1,thicc = 5)
    call colorscale(r,g,b,33.8,34.4,2,0.7,1,width,0.3,lt = 1, gt = 1,x = width/2.,y = -height-2.)
    call mod12_memori(13,width,0.7,y = -height)


    watercolumnheight = 200.

    lon1deg = 6400.*cos(39.*pi/180.)*2.*pi/360. * 10.**(3) ! in m   ! same reference lat as JODC *by the way the lon1deg is supposed to be lat1deg
    lat1deg = 2.*pi*6400./360. * 10**(3) ! in m
    gridvolume = lon1deg * lat1deg * real(watercolumnheight)
    print*,lon1deg,lat1deg,gridvolume
    ! lon1deg = 6400.*cos(41.*pi/180.)*2.*pi/360. * 10.**(3) ! in m
    ! lat1deg = 2.*pi*6400./360. * 10**(3) ! in m
    ! gridvolume = lon1deg * lat1deg * real(watercolumnheight)
    ! print*,lon1deg,lat1deg,gridvolume
    call avsemdata_2D(meanSst14(:,:200),'dim2',mean_1D = meanSst14_200mean)

    print*,meanSst14_200mean

    do i = 1, 13
        freshwatervolume(i) = gridvolume * (34.7-meanSst14_200mean(i))/34.7
        freshwatervolume(i) = freshwatervolume(i)/lon1deg/lat1deg ! in m^3/m^2
        print*,i,freshwatervolume(i)
    end do

    call plotback('first')
    call plot(0.,-height*3.-1.,-3)

    call butler_linegraph(freshwatervolume,width,height,1.,6.,0.,.true.)
    call mod12_memori(13,width,0.7)


    call avsemdata_2D(meanSloop(:,9,:200),'dim2',mean_1D = meanSst1_200mean)
    print*,meanSst1_200mean
    do i = 1, 13
        freshwatervolumest1(i) = gridvolume * (34.7-meanSst1_200mean(i))/34.7
        freshwatervolumest1(i) = freshwatervolumest1(i)/lon1deg/lat1deg ! in m^3/m^2
        print*,i,freshwatervolumest1(i)
    end do

    call plot(width+2.,0.,-3)
    call butler_linegraph(freshwatervolumest1,width,height,1.,6.,0.,.true.)
    call mod12_memori(13,width,0.7)


    

    call plote

end program


        





