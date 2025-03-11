program hakidame
    use always 
    implicit none 
    real,parameter:: width = 25.,height = 5.,width2 = 8.,height2 = 7.
    real,dimension(15,12,2,9,400)::potemp_c5d,sal_c5d,sigma_c5d
    real,dimension(15,12,2,9)::dh_c4d = 0.
    real,dimension(4,12,2,9,400)::potemp_ms5,sal_ms5,sigma_ms5
    real,dimension(4,12,2,9)::dh_ms4
    real,dimension(180,400)::salinityseries,temperatureseries,sigmaseries,dsalinityseries,dtemperatureseries,dsigmaseries
    ! real,dimension(:),allocatable::dhseries
    real,dimension(-6:6,400)::sal_tides_correlation,temp_tides_correlation,sigma_tides_correlation
    real,dimension(-6:6,1)::dh_tides_correlation
    real,dimension(15,12)::mat,tap
    real,dimension(:),allocatable::avtap,avmat
    real,dimension(180)::tap_mat,dhseries,diffdhseries
    real,dimension(:),allocatable::r1,g1,b1,r2,g2,b2,r0,g0,b0,r3,g3,b3,r4,g4,b4
    real,dimension(-6:6,2)::c95,c99


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                        ! Obtaining data
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call calibrated_data2(potemp_c5 = potemp_c5,sal_c5 = sal_c5,sigma_c5 = sigma_c5,dh_c4 = dh_c4,match_station_labels_and_array_indices = .true.,potemp_ms5 = potemp_ms5,sal_ms5 = sal_ms5,sigma_ms5 = sigma_ms5,dh_ms4 = dh_ms4)
    ! print*,count(dh_c4(:,:,1,:)/=0.)
    ! print*,count(dh_c4(:,:,1,:)==0.)
    call SSH_data(mat,ilabel = 4701,calibrate = .true.,diff_from_yearly_mean = .true.) ! matsumae
    call SSH_data(tap,slabel = '竜飛',calibrate = .true.,diff_from_yearly_mean = .true.)
    call avsemdata_2D(tap,'dim1',mean_1D = avtap)
    call avsemdata_2D(mat,'dim1',mean_1D = avmat)
    print*,tap,mat
    ! do i = 1, 15
    !     do j = 1, 12
    !     if(tap(i,j)/=0.)tap(i,j) = tap(i,j) - avtap(j)
    !     if(mat(i,j)/=0.)mat(i,j) = mat(i,j) - avmat(j)
    !     end do
    ! end do
    ! do i = 1, 15
    !     tap(i,:) = avtap(:)
    !     mat(i,:) = avmat(:)
    ! end do
    
    
    do i = 1, 15
        if(i == 4)cycle
        do m = 1,12
            if(all(potemp_c5(i,m,1,1,:)==0.))cycle
            potemp_c5d(i,m,:,:,:) = potemp_c5(i,m,:,:,:) - potemp_ms5(1,m,:,:,:) ! removing the mean
            sal_c5d(i,m,:,:,:) = sal_c5(i,m,:,:,:) - sal_ms5(1,m,:,:,:) ! removing the mean
            sigma_c5d(i,m,:,:,:) = sigma_c5(i,m,:,:,:) - sigma_ms5(1,m,:,:,:) ! removing the mean
            dh_c4d(i,m,:,:) = dh_c4(i,m,:,:)-dh_ms4(1,m,:,:) ! removing the mean
        end do
    end do
    do i = 1, 400
        salinityseries(:,i) = reshape(transpose(sal_c5(:,:,1,1,i)),[180])
        temperatureseries(:,i) = reshape(transpose(potemp_c5(:,:,1,1,i)),[180])
        sigmaseries(:,i) = reshape(transpose(sigma_c5(:,:,1,1,i)),[180])
        dsalinityseries(:,i) = reshape(transpose(sal_c5d(:,:,1,1,i)),[180])
        dtemperatureseries(:,i) = reshape(transpose(potemp_c5d(:,:,1,1,i)),[180])
        dsigmaseries(:,i) = reshape(transpose(sigma_c5d(:,:,1,1,i)),[180])
    end do
    dhseries = reshape(transpose(dh_c4(:,:,1,1)),[180])
    diffdhseries = reshape(transpose(dh_c4d(:,:,1,1)),[180])
    ! print*,dhseries
    ! print*,diffdhseries
    tap_mat = reshape(transpose((tap - mat)),[180])
    do i = 1,180
        if(tap_mat(i)<-100..or.tap_mat(i)>100.)then 
            tap_mat(i) = 0.
            ! print*,i
        end if
    end do
    tap_mat(12*3+1:12*4) = 0. ! no data for 2012
                                                    !                                            lag
    do i = -6,6 ! positive value means salinity leads tides   sal timeseries ; ------------------ <-> 
                                                    !       tides timeseries ;    -------------------   
        c95(i,:) = f_rcritical95(180-abs(i))
        c99(i,:) = f_rcritical99(180-abs(i))
        do j  = 1, 400
            if(i>=0)then 
                sal_tides_correlation(i,j) = fcorrecoeff(dsalinityseries(1+i:180,j),tap_mat(1:180-i)) ! correlation between salinity and tides per depth
                temp_tides_correlation(i,j) = fcorrecoeff(dtemperatureseries(1+i:180,j),tap_mat(1:180-i)) ! correlation between temperature and tides per depth
                sigma_tides_correlation(i,j) = fcorrecoeff(dsigmaseries(1+i:180,j),tap_mat(1:180-i)) ! correlation between sigma and tides per depth
            else if (i<0)then 
                sal_tides_correlation(i,j) = fcorrecoeff(dsalinityseries(1:180+i,j),tap_mat(1-i:180)) ! correlation between salinity and tides per depth
                temp_tides_correlation(i,j) = fcorrecoeff(dtemperatureseries(1:180+i,j),tap_mat(1-i:180)) ! correlation between temperature and tides per depth
                sigma_tides_correlation(i,j) = fcorrecoeff(dsigmaseries(1:180+i,j),tap_mat(1-i:180)) ! correlation between sigma and tides per depth
            end if
        end do
        if(i>=0)then 
            dh_tides_correlation(i,1) = fcorrecoeff(diffdhseries(1+i:180),tap_mat(1:180-i)) ! correlation between dh and tides per depth
        else if (i<0)then 
            dh_tides_correlation(i,1) = fcorrecoeff(diffdhseries(1:180+i),tap_mat(1-i:180)) ! correlation between dh and tides per depth
        end if
    end do
    
    print*,'temp_tides_correlation'
    print*,maxval(temp_tides_correlation),minval(temp_tides_correlation)

    print*,'sal_tides_correlation'
    print*,maxval(sal_tides_correlation),minval(sal_tides_correlation)

    print*,'sigma_tides_correlation'
    print*,maxval(sigma_tides_correlation),minval(sigma_tides_correlation)

    print*,'dh_tides_correlation'
    print*,maxval(dh_tides_correlation),minval(dh_tides_correlation)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                        ! Plotting data
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    call plots2(nnfile = 'MAT-TAP_PTSRHO_Correlations',oopt = 'otops', x = .8,y = 0.8,h = 'Temperature Timeseries')
    call plotsave('first')
    call num_memori(0.,400.,41,10,0.7,-1,-height,angle = -90)
    ! call butler_psk(temperatureseries,width,-height,0.,0.,20.,2.5,'b2r',8,bpt1 = 5, r = r0, g = g0, b = b0,conti = 0.,continc = 5.)
    ! call colorscale(r0,g0,b0,-10.,10.,2,0.6,1,height,0.25,lt = 1, gt = 1,rangle = 90.,x = width + .5,y = -height/2.)
    call butler_psk(dtemperatureseries,width,-height,0.,-3.,3.,1.,'b2r',6,bpt1 = 4, r = r0, g = g0, b = b0,conti = -20.,continc = 5.)
    call colorscale(r0,g0,b0,-3.,3.,1,0.6,1,height,0.25,lt = 1, gt = 1,rangle = 90.,x = width + .5,y = -height/2.)
    call mod12_memori(180,width,symbol_size = 0.4,num_freq = 6,y = -height)
    call floating_lines(-height,90.,15,4,width/15.,0.,x = width/180./2.,dashy = -6,r = 0.4,g = 0.4,b = 0.4)

    call plotback('first');call plot(0.,-height-1.5,-3)
    call symbolc(width/2.,0.2,1.,'Salinity Timeseries')
    call num_memori(0.,400.,41,10,0.7,-1,-height,angle = -90)
    ! call butler_psk(salinityseries,width,-height,0.,33.9,34.3,0.05,'b2r',8,bpt1 = 5,conti = 32.3,continc = 0.4, r = r1, g = g1, b = b1)
    ! call colorscale(r1,g1,b1,33.9,34.3,2,0.6,1,height,0.25,lt = 1, gt = 1,rangle = 90.,x = width + .5,y = -height/2.)
    call butler_psk(dsalinityseries,width,-height,0.,-0.2,0.2,0.05,'b2r',8,bpt1 = 5,conti = -1.2,continc = 0.4, r = r1, g = g1, b = b1)
    call colorscale(r1,g1,b1,-0.2,0.2,2,0.6,1,height,0.25,lt = 1, gt = 1,rangle = 90.,x = width + .5,y = -height/2.)
    call mod12_memori(180,width,symbol_size = 0.4,num_freq = 6,y = -height)
    call floating_lines(-height,90.,15,4,width/15.,0.,x = width/180./2.,dashy = -6,r = 0.4,g = 0.4,b = 0.4)

    call plotback('first');call plot(0.,-3*height-3.,-3)
    call symbolc(width/2.,height + 0.2,1.,'TAP-MAT')
    call butler_linegraph(tap_mat,width,height,-100.,100.,memiter = 5,maskbelow = 0.,memscale = 0.1,mem = .true.,lthick = 4,memsymsize = 0.7)
    call floating_lines(height,90.,15,4,width/15.,0.,x = width/180./2.,dashy = -6,r = 0.4,g = 0.4,b = 0.4)
    call mod12_memori(180,width,symbol_size = 0.4,num_freq = 6,y = 0.)
    
    call newpage(x = .8,y = 0.8,h = 'Density (RHO) Timeseries')
    call plotback('first')
    call num_memori(0.,400.,41,10,0.7,-1,-height,angle = -90)
    ! call butler_psk(sigmaseries,width,-height,0.,25.,27.,0.2,'b2r',10,bpt1 = 6,conti = 20.,continc = 1.,r = r3, g = g3, b = b3)
    ! call colorscale(r3,g3,b3,25.,27.,5,0.6,1,height,0.25,lt = 1, gt = 1,rangle = 90.,x = width + .5,y = -height/2.)
    call butler_psk(dsigmaseries,width,-height,0.,-.6,.6,0.2,'b2r',6,bpt1 = 4,conti = -2.,continc = 1.,r = r3, g = g3, b = b3)
    call colorscale(r3,g3,b3,-.6,.6,1,0.6,1,height,0.25,lt = 1, gt = 1,rangle = 90.,x = width + .5,y = -height/2.)
    call mod12_memori(180,width,symbol_size = 0.4,num_freq = 6,y = -height)
    call floating_lines(-height,90.,15,4,width/15.,0.,x = width/180./2.,dashy = -6,r = 0.4,g = 0.4,b = 0.4)

    call plotback('first');call plot(0.,-height*2-2.,-3)
    call symbolc(width/2.,height+0.3,1.,'Dynamic Height Timeseries')
    call butler_linegraph(dhseries,width,height,39700.,39750.,mem = .true.,memiter = 6,memscale = 0.01)
    ! call butler_linegraph(diffdhseries,width,height,-15.,15.,mem = .true.,memiter = 7,memscale = 1.)
    call mod12_memori(180,width,symbol_size = 0.4,num_freq = 6,y = 0.)
    call floating_lines(height,90.,15,4,width/15.,0.,x = width/180./2.,dashy = -6,r = 0.4,g = 0.4,b = 0.4)
    ! call colorscale(r1,g1,b1,33.9,34.3,2,0.6,1,height,0.25,lt = 1, gt = 1,rangle = 90.,x = width + .5,y = -height/2.)

    call newpage(h = 'PT,S,RHO and Tides Correlation',x = 1.5,y = -1.)
!temp
    call plotsave('second')
    call symbolc(width2/2.,1.2,0.8,'T')
    call symbolc(width2/2.,0.5,0.55,'Tides lead T <-> T leads Tides')
    call num_memori(0.,400.,41,10,0.7,-1,-height2,angle = -90)
    ! call butler_psbet(temp_tides_correlation,width2,-height2,0.,-0.5,0.5,0.1,'b2r',10,bpt1 = 6,r = r2, g = g2, b = b2)
    call butler_psbet2(temp_tides_correlation,width2,-height2,c95(1,2),c95(1,1),-0.5,0.5,colorscheme='b2r',iterations=6,bpt1 = 4,maskcolor = 'light grey',r = r2, g = g2, b = b2)
    call num_memori(-6.,6.,13,1,0.6,-1,width2,gap = 2,y = -height2)

    call plot(width2+0.3 ,0.,-3)
!sal
    ! call num_memori(0.,400.,41,10,0.7,-1,-height2,angle = -90)
    call symbolc(width2/2.,1.2,0.8,'S')
    call symbolc(width2/2.,0.5,0.55,'Tides lead S <-> S leads Tides')
    call memori(41,0.125,5,height2,-90.,y = -height2/2.)
    call butler_psbet2(sal_tides_correlation,width2,-height2,c95(1,2),c95(1,1),-0.45,0.45,colorscheme='b2r',iterations=6,bpt1 = 4,maskcolor = 'light grey',r = r2, g = g2, b = b2)
    call num_memori(-6.,6.,13,1,0.6,-1,width2,gap = 2,y = -height2)
    call colorscale2(r2,g2,b2,-0.45,0.45,4,c95(1,2),c95(1,1),1,0.6,2,20.,0.25,lt = 1, gt = 1,x = width2/2.,y = -5.-height2)
    call plotsave('sal')
    call plot(width2+0.3,0.,-3)
!sigma
    ! call num_memori(0.,400.,41,10,0.7,-1,-height2,angle = -90)
    call symbolc(width2/2.,1.2,0.8,'RHO')
    call symbolc(width2/2.,0.5,0.5,'Tides lead RHO <-> RHO leads Tides')
    call memori(41,0.125,5,height2,-90.,y = -height2/2.)
    call butler_psbet2(sigma_tides_correlation,width2,-height2,c95(1,2),c95(1,1),-0.5,0.5,colorscheme='b2r',iterations=6,bpt1 = 4,maskcolor = 'light grey',r = r2, g = g2, b = b2)
    call num_memori(-6.,6.,13,1,0.6,-1,width2,gap = 2,y = -height2)

    call ocenter(y = -8.);call plotsave('center-8')
    call symbolc(0.,0.,0.7,'Critical Values for 95% Confidence for a = 0.05, df ~ 180 are')
    call symbolc(0.,-1.,0.7,real2str(c95(1,1),3)//' and '//real2str(c95(1,2),3))

!dh
    call plotback('sal');call plot(0.,-height2-3.,-3)
    call symbolc(width2/2.,1.2,0.8,'DH')
    call symbolc(width2/2.,0.5,0.5,'Tides lead DH <-> DH leads Tides')
    call butler_psbet2(dh_tides_correlation(:,1:1),width2,-1.,c95(1,2),c95(1,1),-0.5,0.5,colorscheme='b2r',iterations=6,bpt1 = 4,maskcolor = 'light grey',r = r2, g = g2, b = b2)


    
    call plote
end program