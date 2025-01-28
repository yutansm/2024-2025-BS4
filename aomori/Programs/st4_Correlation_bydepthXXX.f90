program gomibako
    use always
    implicit none 
    real,dimension(years*months)::Nst4_201,Sst4_201
    real,dimension(:),allocatable::Ncontinuous,Scontinuous,NNcoeffs,SScoeffs,NScoeffs
    real,dimension(:,:),allocatable::autocritical,crosscritical
    real,parameter::width = 25.,height = 5.,width2 = 11.,height2 = 10.
    real::maxvalue,Ntotalmean,Ntotalsd
    real,dimension(:),allocatable::Ntmeanarray,Ntsdarray
    integer::initial,gap,obs_depth

    call calibrated_data2(potemp_c5,sal_c5,match_station_labels_and_array_indices=.true.)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                    ! Station 4 Examination, the array indices match here
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    do d = 1, 7
        if(d==1)obs_depth = 50
        if(d==2)obs_depth = 100
        if(d==3)obs_depth = 150
        if(d==4)obs_depth = 200
        if(d==5)obs_depth = 250
        if(d==6)obs_depth = 300
        if(d==7)obs_depth = 350
        Nst4_201 = reshape(transpose(potemp_c5(:,:,1,4,obs_depth)),[years*months])
        Sst4_201 = reshape(transpose(potemp_c5(:,:,2,4,obs_depth)),[years*months])

        print*,maxval(Nst4_201),maxval(Sst4_201)
        maxvalue = max(maxval(Nst4_201),maxval(Sst4_201))

        ! do i = 1, 180
        !     if(Nst4_201(i)/=0..and.Sst4_201(i)/=0.)then 
        !         cycle
        !     else
        !         if(Nst4_201(i)==0..and.Sst4_201(i)==0.)then
        !             cycle
        !         else
        !             print*,i,Nst4_201(i),Sst4_201(i)
        !         end if
        !     end if
        ! end do

        ! linear interpolation of missing data 
        do i = 1, 180
            if(Nst4_201(i)/=0..and.Sst4_201(i)/=0.)then
                initial = i
                exit
            end if
        end do
        print*,'initial data point is',initial
        !first data point is initial = 6
        do i = initial,180
            if(Nst4_201(i)==0.)then ! for Nline
                if(i>=37.and.i<=48)cycle  ! 2012 is unusable
                if(i>1.and.Nst4_201(i-1)==0.)cycle 
                do n = 1, 10
                    if(Nst4_201(i+n)/=0.)then  ! find the next data point
                        gap = n
                        exit
                    end if
                end do
                do n = 1, gap
                    Nst4_201(i+n-1) = Nst4_201(i+n-2) + (Nst4_201(i+gap) - Nst4_201(i-1))/real(gap+1)
                end do
            end if

            if(Sst4_201(i)==0.)then ! for Sline
                if(i>=37.and.i<=48)cycle  ! 2012 is unusable
                if(i>1.and.Sst4_201(i-1)==0.)cycle 
                do n = 1, 10
                    if(Sst4_201(i+n)/=0.)then  ! find the next data point
                        gap = n
                        exit
                    end if
                end do
                do n = 1, gap
                    Sst4_201(i+n-1) = Sst4_201(i+n-2) + (Sst4_201(i+gap) - Sst4_201(i-1))/real(gap+1)
                end do
            end if
        end do

        ! now create a continuous time series array
        allocate(Ncontinuous(180-initial+1),Scontinuous(180-initial+1))
        print*,size(Ncontinuous),size(Scontinuous)

        n = 1;m = 1
        do i = initial, 180
            ! if(Nst4_201(i)/=0.)then
                Ncontinuous(n) = Nst4_201(i)
                n = n + 1
            ! end if
            ! if(Sst4_201(i)/=0.)then
                Scontinuous(m) = Sst4_201(i)
                m = m + 1
            ! end if
        end do

        call avsemdata_1D(Ncontinuous,mean = Ntotalmean,s = Ntotalsd)
        print*,'N mean and sem are = ',Ntotalmean,Ntotalsd
        allocate(Ntmeanarray(size(Ncontinuous)),Ntsdarray(size(Ncontinuous)))
        Ntmeanarray = Ntotalmean;Ntsdarray = Ntotalsd

        ! calculate the correlation coefficients for the 2 timeseries, first separately, then cross-correlation

        ! allocate(NNcoeffs(0:size(Ncontinuous)),SScoeffs(0:size(Scontinuous)))  full size
        ! allocate(autocritical(2,0:size(Ncontinuous)))
        ! do i = 0, size(Ncontinuous)
        !     NNcoeffs(i) = fcorrecoeff(Ncontinuous(1+i:size(Ncontinuous)),Ncontinuous(1:size(Ncontinuous)-i))
        !     SScoeffs(i) = fcorrecoeff(Scontinuous(1+i:size(Scontinuous)),Scontinuous(1:size(Scontinuous)-i))
        !     autocritical(:,i) = f_rcritical95(size(Ncontinuous)-i)
        ! end do
        allocate(NNcoeffs(0:24),SScoeffs(0:24))   ! up to 24 months of lag  
        allocate(autocritical(2,0:24))
        do i = 0, 24
            NNcoeffs(i) = fcorrecoeff(Ncontinuous(1+i:size(Ncontinuous)),Ncontinuous(1:size(Ncontinuous)-i))
            SScoeffs(i) = fcorrecoeff(Scontinuous(1+i:size(Scontinuous)),Scontinuous(1:size(Scontinuous)-i))
            autocritical(:,i) = f_rcritical95(size(Ncontinuous)-i)
        end do

        ! positive values mean Sline leads Nline
        allocate (NScoeffs(-6:6)) ! 13 values for no lag and upto +-6 months of lag
        allocate(crosscritical(2,-6:6))
        do i = 6,-6,-1
            if(i>=0)then 
                NScoeffs(i) = fcorrecoeff(Ncontinuous(i+1:size(Ncontinuous)),Scontinuous(1:size(Scontinuous)-i))
                crosscritical(:,i) = f_rcritical95(size(Ncontinuous)-i)
            else
                NScoeffs(i) = fcorrecoeff(Ncontinuous(1:size(Ncontinuous)+i),Scontinuous(1-i:size(Scontinuous)))
                crosscritical(:,i) = f_rcritical95(size(Ncontinuous)+i)
            end if
        end do

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                                ! Plotting
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        
        call plots2(psfile='../Plots/Favorites/Correlations/Nst4_'//int2str(obs_depth)//'_CorrelationsXXX.ps',x = 1.,y = -height+1.,oopt = 'otops', h = 'Auto and Cross Correlations for Station 4 at ' // int2str(obs_depth) // 'm')
        ! call num_memori(0.,162.,82,6,0.6,-1,width-width/163.,0,x = width/163./2.)
        call num_memori(0.,real(180-initial+1),180-initial+1,symbol_freq = 12,symbol_size = 0.6,float_quantity = -1,length = width,gap = 2,num_fac = 12.)
        call butler_linegraph(Ncontinuous,width,height,0.,int(maxvalue)+1.,mem = .true.,memsymfreq = 5,lidots = .true.)
        call butler_linegraph(Scontinuous,width,height,0.,int(maxvalue)+1.,mem = .false.,rl = 1.,gl = 0.,bl = 0.,lidots=.true.)
        call plot(0.,height*Ntotalmean/real(int(maxvalue)+1),3);call plot(width,height*Ntotalmean/real(int(maxvalue)+1),2)
        call newpen2(-6)
        call plot(0.,height*Ntotalmean/real(int(maxvalue)+1)+height*Ntotalsd/real(int(maxvalue)+1),3);call plot(width,height*Ntotalmean/real(int(maxvalue)+1)+height*Ntotalsd/real(int(maxvalue)+1),2)
        call plot(0.,height*Ntotalmean/real(int(maxvalue)+1)-height*Ntotalsd/real(int(maxvalue)+1),3);call plot(width,height*Ntotalmean/real(int(maxvalue)+1)-height*Ntotalsd/real(int(maxvalue)+1),2)

        call obottoms(x = 1.5)
        call num_memori(0.,24.,25,6,0.8,-1,width2,gap = 2)
        call butler_linegraph(autocritical(1,:),width2,height2,-1.,1.,rl = 0.,gl = 0.,bl = 1.,lthick = 4)
        call butler_linegraph(autocritical(2,:),width2,height2,-1.,1.,rl = 0.,gl = 0.,bl = 1.,lthick = 4)
        call butler_linegraph(NNcoeffs,width2,height2,-1.,1.,mem = .true.,lthick = 7,memiter = 5,memsymsize = 1.)
        call butler_linegraph(SScoeffs,width2,height2,-1.,1.,rl = 1.,gl = 0.,bl = 0.,lthick = 5)

        call plot(width2+2.5,0.,-3)

        call num_memori(-6.,6.,13,1,0.7,-1,width2-width2/14.,0,x = width2/14./2.)
        call butler_linegraph(crosscritical(1,:),width2,height2,-1.,1.,rl=0.,gl=0.,bl=1.)
        call butler_linegraph(crosscritical(2,:),width2,height2,-1.,1.,rl=0.,gl=0.,bl=1.)
        call butler_linegraph(NScoeffs,width2,height2,-1.,1.,mem = .true.,memiter = 5,memsymsize = 1.)

        call plote

        deallocate(Ncontinuous,Scontinuous,NNcoeffs,SScoeffs,NScoeffs,autocritical,crosscritical,Ntmeanarray,Ntsdarray)

    end do
end program