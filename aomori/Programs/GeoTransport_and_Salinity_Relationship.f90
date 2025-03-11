program hakidame
    use always
    implicit none 
    real,parameter:: width = 25.,height = 7.,width2 = 4.5,height2 = 2.,width3 = 3.5
    real,dimension(15,12,5)::Q_per_station
    real,dimension(15,12)::Q1to2,Q1to6,salinitymean,salinitysem,FUK
    real,dimension(15*12)::salinityperdepth ! this array exists because of my incapabilities 
    real,dimension(15*12,50)::salinityseries
    real,dimension(:),allocatable::salinitymeanseries,salinitysemseries,FUKymean
    real,dimension(2)::c95array
    real::dx,c95
    logical::mem

    call SSH_data(FUK,slabel = '深浦',calibrate = .true.)
    ! print*,FUK
    call avsemdata_2D(FUK,'dim2',mean_1D = FUKymean,rmask = -999.)
    ! print*,FUKymean 
    do i = 1,15
        do j = 1, 12
            if(FUK(i,j)/=-999.)then 
                FUK(i,j) = FUK(i,j) - FUKymean(i)
            end if
        end do
    end do ! FUK now has anomalies from each year
    print*,minval(FUK,mask = (FUK/=-999.)),maxval(FUK)
    call calibrated_data2(potemp_c5 = potemp_c5,sal_c5 = sal_c5,geovel_c5 = geovel_c5,match_station_labels_and_array_indices = .true.)
        ! calculating trasport per pairs of stations 1 through 6. some transports are 0. Note geovel has 5 columns, not 6
    do y = 1, 15
        do m = 1, 12
            do i = 1, 5
                Q_per_station(y,m,i) = sum(geovel_c5(y,m,1,i,:) * delta_x * 1 * 10.**(-6))  !v[m/s] * dx[m] * 1[m] * 10**(-6) = [Sv] per grid per station
                ! if(Q_per_station(y,m,i)==0..and.m/=1.and.m/=7)print*,'no data for station ',i,' in year ',y+2008,' month ',m
            end do
        end do
    end do
    
    ! calculating trasport per pairs of stations 1 through 2 for every month of every year 
    Q1to2 = Q_per_station(:,:,1) + Q_per_station(:,:,2);print*,maxval(Q1to2),minval(Q1to2)
    ! calculating trasport per pairs of stations 1 through 6 for every month of every year
    Q1to6 = sum(Q_per_station(:,:,1:5), dim=3);print*,maxval(Q1to6),minval(Q1to6)

    ! salinity, station 1 0~50m salinity data for every month of every year
    do i = 1, 50
        salinityseries(:,i) = reshape(transpose(sal_c5(:,:,1,1,i)),[15*12])
    end do
    ! getting mean of this surface salinity data
    call avsemdata_2D(salinityseries,'dim2',mean_1D = salinitymeanseries,sem_1D = salinitysemseries)
    salinitymean = transpose(reshape(salinitymeanseries,[12,15]))
    salinitysem = transpose(reshape(salinitysemseries,[12,15]))
    ! print*,salinitymeanseries,size(salinitymeanseries),shape(salinitymeanseries)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                            ! Plotting
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    ! Salinity and Transport Correlation
        call plots2(nnfile = 'Transport_and_Salinity_Correlation',oopt = 'obottoms',x = .8,y = 10.,h = 'Transport and Salinity Correlation')
        call mod12_memori(180,width,symbol_size = 0.3,num_freq = 6,dxval = dx)
        call floating_lines(height,90.,15,3,dx*12.,x = dx*3./2.,dashy = -4) ! feb
        call floating_lines(height,90.,15,3,dx*12.,x = dx*19./2.,dashy = -4) ! oct
        call butler_linegraph(reshape(transpose(Q1to6),[15*12]),width,height,-2.,5.,mem = .true.,LI=.true.,maskbelow=0.,lthick = 7)
        call butler_linegraph(reshape(transpose(Q1to2),[15*12]),width,height,-2.,5.,mem = .true.,LI=.true.,maskbelow=0.,lthick = 6,rl = .4,bl=0.4,gl=0.4)
        call butler_linegraph(salinitymeanseries,width,height,34.6,32.6,mem = .true.,LI=.true.,memiter = 6,memloc = 'right',lthick = 5,bl = 1.)

        call plot(0.,-height2-2.,-3)
        call plotsave('monthly')
        c95array = f_rcritical95(15)
        c95 = c95array(1)
        call numberr(-.5,-1.5,0.5,c95,0.,2)
        call numberr(-.5,-2.,0.5,-c95,0.,2)
        do i = 1, 12
            if(i==1.or.i==7)then 
                cycle
            else if(i==2.or.i==8)then
                mem = .true.
            else
                mem = .false.
            endif
            call symbolc(width2/2.,height2+0.2,0.5,monthnames(i))
            call num_memori(1.,15.,15,1,0.25,-1,width2,gap = 2)
            call butler_linegraph(Q1to2(:,i),width2,height2,-2.,5.,LI=.true.,maskbelow=0.,rl = .4,bl=0.4,gl=0.4)
            call butler_linegraph(Q1to6(:,i),width2,height2,-2.,5.,mem = mem,LI=.true.,maskbelow=0.)
            call butler_linegraph(salinitymean(:,i),width2,height2,34.6,32.6,mem = mem,LI=.true.,bl=1.,rl=0.,gl=0.,memiter = 6,memloc = 'right')
            call symbolr(width2/2.-0.2,-1.,0.3,'r,S-Qst1~2');call number(width2/2.+0.2,-1.,0.5,fcorrecoeff(salinitymean(:,i),Q1to2(:,i)),0.,2)
            call symbolr(width2/2.-0.2,-1.5,0.3,'r,S-Qst1~6');call number(width2/2.+0.2,-1.5,0.5,fcorrecoeff(salinitymean(:,i),Q1to6(:,i)),0.,2)
            ! print*,f_rcritical95(15)
            if(i/=6)then 
                call plot(width2+0.8,0.,-3)
            else 
                call plotback('monthly');call plot(0.,-height2-3.,-3)
            endif
        end do
    
    ! Salinity and Tides Correlation
        call newpage('Salinity and FUK Tides Correlation',x = .8,y = 10.)
        call mod12_memori(180,width,symbol_size = 0.3,num_freq = 6,dxval = dx)
        call floating_lines(height,90.,15,3,dx*12.,x = dx*3./2.,dashy = -4) ! feb
        call floating_lines(height,90.,15,3,dx*12.,x = dx*19./2.,dashy = -4) ! oct
        call butler_linegraph(reshape(transpose(FUK),[15*12]),width,height,-200.,200.,mem = .true.,LI=.true.,rl=1.,memiter = 9,memloc = 'right',rmask = -999.,lthick = 4,memscale = 0.1,memsymsize = 0.7,maskbelow =0.)
        call butler_linegraph(salinitymeanseries,width,height,34.6,32.6,mem = .true.,LI=.true.,memiter = 6,lthick = 5,bl = 1.,memsymsize = 0.7)        
        call plotback('monthly')
        call numberr(-.5,-1.5,0.5,c95,0.,2)
        call numberr(-.5,-2.,0.5,-c95,0.,2)
        do i = 1, 12
            if(i==1.or.i==7)then
                mem = .true.
            else
                mem = .false.
            endif
            call symbolc(width3/2.,height2+0.2,0.5,monthnames(i))
            call num_memori(1.,15.,15,1,0.25,-1,width3,gap = 2)
            call butler_linegraph(FUK(:,i),width3,height2,-200.,200.,mem = mem,rl = 1.,rmask = -999.,LI=.true.,maskbelow=0.,memscale = 0.1,memiter = 9)
            call butler_linegraph(salinitymean(:,i),width3,height2,34.6,32.6,mem = mem,LI=.true.,bl=1.,rl=0.,gl=0.,memiter = 6,memloc = 'right')
            call symbolr(width3/2.-0.2,-1.,0.3,'r,S-FUK');call number(width3/2.+0.2,-1.,0.5,fcorrecoeff(salinitymean(:,i),Q1to2(:,i)),0.,2)
            if(i/=6)then 
                call plot(width3+0.8,0.,-3)
            else 
                call plotback('monthly');call plot(0.,-height2-3.,-3)
            endif
        end do

    ! Transport and Tides Correlation
        call newpage('Transport and FUK Tides Correlation',x = .8,y = 10.)
        call mod12_memori(180,width,symbol_size = 0.3,num_freq = 6,dxval = dx)
        call floating_lines(height,90.,15,3,dx*12.,x = dx*3./2.,dashy = -4) ! feb
        call floating_lines(height,90.,15,3,dx*12.,x = dx*19./2.,dashy = -4) ! oct
        call butler_linegraph(reshape(transpose(FUK),[15*12]),width,height,-200.,200.,mem = .true.,LI=.true.,rl=1.,memiter = 9,memloc = 'right',rmask = -999.,lthick = 4,memscale = 0.1,memsymsize = 0.7)
        call butler_linegraph(reshape(transpose(Q1to2),[15*12]),width,height,-2.,5.,mem = .true.,LI=.true.,maskbelow=0.,lthick = 6,rl = .4,bl=0.4,gl=0.4)
        call butler_linegraph(reshape(transpose(Q1to6),[15*12]),width,height,-2.,5.,mem = .true.,LI=.true.,maskbelow=0.,lthick = 7)
        call plotback('monthly')
        call numberr(-.5,-1.5,0.5,c95,0.,2)
        call numberr(-.5,-2.,0.5,-c95,0.,2)
        do i = 1, 12
            if(i==1.or.i==7)then
                mem = .true.
            else
                mem = .false.
            endif
            call symbolc(width3/2.,height2+0.2,0.5,monthnames(i))
            call num_memori(1.,15.,15,1,0.25,-1,width3*15/16.,x = width3*1/32.)
            call butler_linegraph(FUK(:,i),width3,height2,-200.,200.,mem = mem,rmask = -999.,LI=.true.,memscale = 0.1,memiter = 9,rl = 1.)
            call butler_linegraph(Q1to2(:,i),width3,height2,-2.,5.,LI=.true.,maskbelow=0.,rl = .4,bl=0.4,gl=0.4)
            call butler_linegraph(Q1to6(:,i),width3,height2,-2.,5.,mem = mem,LI=.true.,maskbelow=0.,memloc = 'right')
            call symbolr(width3/2.-0.2,-1.,0.3,'r,FUK-Qst1~2');call number(width3/2.+0.2,-1.,0.5,fcorrecoeff(FUK(:,i),Q1to2(:,i)),0.,2)
            call symbolr(width3/2.-0.2,-1.5,0.3,'r,FUK-Qst1~6');call number(width3/2.+0.2,-1.5,0.5,fcorrecoeff(FUK(:,i),Q1to6(:,i)),0.,2)
            if(i/=6)then 
                call plot(width3+0.8,0.,-3)
            else 
                call plotback('monthly');call plot(0.,-height2-3.,-3)
            endif
        end do

    call plote
end program