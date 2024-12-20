program hakidame
    use always
    implicit none 
    real,dimension(:,:,:,:),allocatable::geovel_c4
    ! real,dimension(years,months,lines,stations)::depth_200  ! depth 200 of station 4
    real,dimension(months,years,lines,stations,depth)::PTswapped,Sswapped,Dswapped,Vgswapped
    real,dimension(years*months,lines,stations,depth)::PTtimeseries,Stimeseries,Dtimeseries,Vgtimeseries
    real,dimension(:),allocatable::st4tsmean,st4tssd ! dimension is 2. N line and S line
    real,dimension(years*months,stations,depth)::PToswarmN,PToscoldN,SoswarmN,SoscoldN,DoswarmN,DoscoldN,VgoswarmN,VgoscoldN   ! oswarm = offshore warm, oscold = offshore cold
    ! real,dimension(years*months,stations,depth)::diffPT,diffS,diffD,diffVg
    real,dimension(:,:),allocatable::meanPToswarmN,sdPToswarmN,meanSoswarmN,sdSoswarmN,meanDoswarmN,sdDoswarmN,meanVgoswarmN,sdVgoswarmN
    real,dimension(:,:),allocatable::meanPToscoldN,sdPToscoldN,meanSoscoldN,sdSoscoldN,meanDoscoldN,sdDoscoldN,meanVgoscoldN,sdVgoscoldN
    real,dimension(stations,depth)::meanPTdiff,meanSdiff,meanDdiff,meanVgdiff
    integer,dimension(stations,depth)::resultPT,resultS,resultD,resultVg
    real,dimension(:),allocatable::r1,g1,b1
    real,parameter::width = 3.5,height = 7.


    call calibrated_data2(potemp_c5,sal_c5,sigma_c5,geovel_c5,.true.,51)
    ! call avsemdata_5D(geovel_c5,'dim1',mean_4D = geovel_c4)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                    ! Station 4 Examination, the array indices match here
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! getting total mean and standard error of station 4 depth 201

    do i = 1,2
        do n = 1, 9
            do l = 1, 400
                PTswapped(:,:,i,n,l) = transpose(potemp_c5(:,:,i,n,l))  ! flipping the array dimensions
                Sswapped(:,:,i,n,l) = transpose(sal_c5(:,:,i,n,l))
                Dswapped(:,:,i,n,l) = transpose(sigma_c5(:,:,i,n,l))
                if(n/=9)Vgswapped(:,:,i,n,l) = transpose(geovel_c5(:,:,i,n,l))
            end do
        end do
    end do
    PTtimeseries = reshape(PTswapped,(/years*months,lines,stations,depth/)) ! reshaping the array to be in chronological order
    Stimeseries = reshape(Sswapped,(/years*months,lines,stations,depth/))
    Dtimeseries = reshape(Dswapped,(/years*months,lines,stations,depth/))
    Vgtimeseries = reshape(Vgswapped,(/years*months,lines,stations,depth/))
    call avsemdata_2D(PTtimeseries(:,:,4,201),'dim1',mean_1D = st4tsmean,s_1D = st4tssd) ! mean and sd of station 4 depth 200 both lines
    print*,st4tsmean,st4tssd !   N mean = 2.87311602   S mean = 3.38972688      Nsd =  2.21267033     Ssd =   2.30837226   
    ! call plots2(oopt = 'obottoms',x = 3.)
    ! call mod12_memori(180,20.,num_freq = 6,symbol_size = 0.5)
    ! call butler_linegraph(PTtimeseries(:,1,4),20.,10.,0.,12.,mem = .true.,dots = .true.) 
    ! call butler_linegraph(PTtimeseries(:,2,4),20.,10.,0.,12.,mem = .true.,dots = .true.,rl = 1.,gl = 0.,bl = 0.)
    ! call plote

    ! copying values of entire Nline for when warm water is offshore
    ! warm
        do i = 1, years*months
            if(PTtimeseries(i,1,4,201) /= 0. .and. PTtimeseries(i,1,4,201) > st4tsmean(1)+st4tssd(1))then
                PToswarmN(i,:,:) = PTtimeseries(i,1,:,:)
                SoswarmN(i,:,:) = Stimeseries(i,1,:,:)
                DoswarmN(i,:,:) = Dtimeseries(i,1,:,:)
                VgoswarmN(i,:,:) = Vgtimeseries(i,1,:,:)
            else
                PToswarmN(i,:,:) = 0.0
                SoswarmN(i,:,:) = 0.0
                DoswarmN(i,:,:) = 0.0
                VgoswarmN(i,:,:) = 0.0
            end if
        end do

    ! cold
        do i = 1, years*months
            if(PTtimeseries(i,1,4,201) /= 0. .and. PTtimeseries(i,1,4,201) < st4tsmean(1))then
                PToscoldN(i,:,:) = PTtimeseries(i,1,:,:)
                SoscoldN(i,:,:) = Stimeseries(i,1,:,:)
                DoscoldN(i,:,:) = Dtimeseries(i,1,:,:)
                VgoscoldN(i,:,:) = Vgtimeseries(i,1,:,:)
            else
                PToscoldN(i,:,:) = 0.0
                SoscoldN(i,:,:) = 0.0
                DoscoldN(i,:,:) = 0.0
                VgoscoldN(i,:,:) = 0.0
            end if
        end do

        print*,'warm',count(PToswarmN(:,4,201) /=0. ),'cold',count(PToscoldN(:,4,201) /=0.)

        ! taking means of times with and without warm water offshore
        call avsemdata_3D(PToswarmN,'dim1',mean_2D = meanPToswarmN,s_2D = sdPToswarmN) 
        call avsemdata_3D(PToscoldN,'dim1',mean_2D = meanPToscoldN,s_2D = sdPToscoldN)
        call avsemdata_3D(SoswarmN,'dim1',mean_2D = meanSoswarmN,s_2D = sdSoswarmN)
        call avsemdata_3D(SoscoldN,'dim1',mean_2D = meanSoscoldN,s_2D = sdSoscoldN)
        call avsemdata_3D(DoswarmN,'dim1',mean_2D = meanDoswarmN,s_2D = sdDoswarmN)
        call avsemdata_3D(DoscoldN,'dim1',mean_2D = meanDoscoldN,s_2D = sdDoscoldN)
        call avsemdata_3D(VgoswarmN,'dim1',mean_2D = meanVgoswarmN,s_2D = sdVgoswarmN)
        call avsemdata_3D(VgoscoldN,'dim1',mean_2D = meanVgoscoldN,s_2D = sdVgoscoldN)

        ! taking diffs of times with and without warm water offshore
        meanPTdiff = meanPToswarmN - meanPToscoldN
        meanSdiff = meanSoswarmN - meanSoscoldN
        meanDdiff = meanDoswarmN - meanDoscoldN
        meanVgdiff = meanVgoswarmN - meanVgoscoldN

        do st = 1, 9
            do d = 1, 400
                resultPT(st,d) = fwelcht_onetailed(meanPToswarmN(st,d),sdPToswarmN(st,d),count(PToswarmN(:,st,d) /= 0.),meanPToscoldN(st,d),sdPToscoldN(st,d),count(PToscoldN(:,st,d) /= 0.))
                resultS(st,d) = fwelcht_onetailed(meanSoswarmN(st,d),sdSoswarmN(st,d),count(SoswarmN(:,st,d) /= 0.),meanSoscoldN(st,d),sdSoscoldN(st,d),count(SoscoldN(:,st,d) /= 0.))
                resultD(st,d) = fwelcht_onetailed(meanDoswarmN(st,d),sdDoswarmN(st,d),count(DoswarmN(:,st,d) /= 0.),meanDoscoldN(st,d),sdDoscoldN(st,d),count(DoscoldN(:,st,d) /= 0.))
                if(l/=9)resultVg(st,d) = fwelcht_onetailed(meanVgoswarmN(st,d),sdVgoswarmN(st,d),count(VgoswarmN(:,st,d) /= 0.),meanVgoscoldN(st,d),sdVgoscoldN(st,d),count(VgoscoldN(:,st,d) /= 0.))
            end do
        end do
            
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                            ! Plotting
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        call plots2('../Plots/Favorites/Composite_Graphs.ps',h = 'Comparison of Times with and without Offshore Warm Water',oopt = 'otops',mode = 'land',hsize = 1.,x = 1.)
        call plotsave('first')
        call num_memori(0.,400.,41,10,0.7,-1,-height,-90)
        call st_memori(1,6,width,1,0.7,y = -height)
        call butler_psk(meanSoswarmN(6:1:-1,:),width,-height,0.,33.95,34.3,0.05,'b2w2r',7,bpt1=4,centralize = 4)
        call butler_cont(meanPToswarmN(6:1:-1,:),width,-height,0.,0.,1.,thicc = 5)
        call plot(width+0.5,0.,-3)
        call memori(40,0.1,10,height,rangle = -90.,y = -height/2.);call memori(6,0.2,0,width,x = width/2.,y = -height)
        call butler_cont(meanDoswarmN(6:1:-1,:),width,-height,0.,20.,0.2,thicc = 5,maskn = .true.)
        call plot(width+0.5,0.,-3)
        call memori(40,0.1,10,height,rangle = -90.,y = -height/2.);call memori(6,0.2,0,width,x = width/2.,y = -height)
        call butler_cont(meanVgoswarmN(6:1:-1,:),width,-height,1000.,-0.2,0.02,thicc = 5,maskn = .true.,gap = 1)

        call plotback('first');call plot(0.,-height-1.5,-3)

        call num_memori(0.,400.,41,10,0.7,-1,-height,-90)
        call st_memori(1,6,width,1,0.7,y = -height)
        call butler_psk(meanSoscoldN(6:1:-1,:),width,-height,0.,33.95,34.3,0.05,'b2w2r',7,bpt1=4,centralize = 4,r = r1,g = g1,b = b1)
        call butler_cont(meanPToscoldN(6:1:-1,:),width,-height,0.,0.,1.,thicc = 5)
        call plot(width+0.5,0.,-3)
        call memori(40,0.1,10,height,rangle = -90.,y = -height/2.);call memori(6,0.2,0,width,x = width/2.,y = -height)
        call butler_cont(meanDoscoldN(6:1:-1,:),width,-height,0.,20.,0.2,thicc = 5,maskn = .true.)
        call plot(width+0.5,0.,-3)
        call memori(40,0.1,10,height,rangle = -90.,y = -height/2.);call memori(6,0.2,0,width,x = width/2.,y = -height)
        call butler_cont(meanVgoscoldN(5:1:-1,:),width,-height,1000.,-0.2,0.02,thicc = 5,maskn = .true.,gap = 1)

        call colorscale(7,r1,g1,b1,33.95,34.3,2,0.6,1,5.,0.3,lt=1,gt=1,rangle=90.,symbol_start = 2,x = width+1.,y = 0.75)
        ! call plotback('first');call plot(0.,2.*(-height-1.),-3)
        call plotback('first');call plot(3.*(width+0.5)+5.,0.,-3);call plotsave('second')

        call num_memori(0.,400.,41,10,0.7,-1,-height,-90)
        call st_memori(1,6,width,1,0.7,y = -height)
        call butler_cont(meanDdiff(6:1:-1,:),width,-height,0.,-2.,0.2,thicc = 5,maskn = .true.)
        call plot(width+0.5,0.,-3)
        call memori(40,0.1,10,height,rangle = -90.,y = -height/2.);call memori(6,0.2,0,width,x = width/2.,y = -height)
        call butler_cont(meanVgdiff(5:1:-1,:),width,-height,1000.,-0.4,0.02,thicc = 5,maskn = .true.,gap = 1)

        ! call plotback('first');call plot(0.,3.*(-height-1.),-3)  
        call plotback('second');call plot(0.,-height-1.5,-3)

        call num_memori(0.,400.,41,10,0.7,-1,-height,-90)
        call st_memori(1,6,width,1,0.7,y = -height)
        call butler_imask(resultD(6:1:-1,:),width,-height,0,r=0.,g=0.,b=0.)
        call butler_imask(resultD(6:1:-1,:),width,-height,1,r=1.,g=0.6,b=0.6)
        call butler_imask(resultD(6:1:-1,:),width,-height,-1,r=0.6,g=0.6,b=1.)
        call plot(width+0.5,0.,-3)
        call memori(40,0.1,10,height,rangle = -90.,y = -height/2.);call memori(6,0.2,0,width,x = width/2.,y = -height)
        call butler_imask(resultVg(5:1:-1,:),width,-height,0,r=0.,g=0.,b=0.,gap = 1)
        call butler_imask(resultVg(5:1:-1,:),width,-height,1,r=1.,g=0.6,b=0.6,gap = 1)
        call butler_imask(resultVg(5:1:-1,:),width,-height,-1,r=0.6,g=0.6,b=1.,gap = 1)


    
        call plote


end program