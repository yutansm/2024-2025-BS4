program testing_butler_psbet
    use always 
    implicit none 
    integer,parameter::obsdepth = 400, watercolumnheight = 300
    integer::depthindex,depdiff,dep(0:12)
    real,dimension(:),allocatable::r1,g1,b1,x1d,y1d,Jx,Jy
    logical::stat
    real::x,red,green,blue,dotsize,meansalinity(0:12,123:142,24:46),sref,sobs,freshwatervolume(0:12,123:142,24:46),gridvolume,height,width = 4.
    real::lon1deg,lat1deg,freshwaterratio(0:12,123:142,24:46),somejodcarray(0:12,123:142,24:46)
    type(JODC_TS)::sal
    real,dimension(0:12,123:142,24:46,0:400)::Jsal400 ! linear interpolation for the first 12 layers 

    lon1deg = 6400.*cos(39.*pi/180.)*2.*pi/360. * 10.**(3) ! in m
    lat1deg = 2*pi*6400./360. * 10**(3) ! in m
    gridvolume = lon1deg * lat1deg * real(watercolumnheight)
    print*,gridvolume
    call calibrated_data2(sal_c5 = sal_c5,potemp_c5 = potemp_c5,match_station_labels_and_array_indices = .true.)
    call JODC_data2(sal = sal,ilat = 24,flat = 46,ilon = 123,flon = 142,calibrate1 = .true.,info = .true.)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Initial Inspection
    ! call plots2(oopt = 'otops',x = 2., y = -2.,h = 'Salinity Profiles',nnfile = 'JODC_CTD_Salinity_Comparison_wKuroshioRegion')
    ! Station 1 
        ! call num_memori(0.,400.,41,5,0.7,-1,-15.,rangle = -90.)
        ! call symbolc(3.5,2.,0.8,'St1')
        ! do y = 1, 15
        !     do m = 1, 12
        !         ! if(y==1.and.m==1)then;stat = .true.;else;stat = .false.;end if
        !         ! call butler_linegraph(sal_c5(y,m,1,1,:),15.,7.,24.5,35.,mem=stat,memloc = 'left',memsymfreq = 5,memfreq = 0.1,rotation = -90.,memflqt = 1,lthick = 1,memsymsize = 0.6)
        !         call helper_linegraph(sal_c5(y,m,1,1,:),15.,7.,32.5,35.,mem = .false.,rotation = -90.,lthick = 1)
        !         if(y==1.and.m==1)call num_memori2(32.5,35.,7.,0.1,-0.,5,0.7)
        !     end do
        ! end do
    ! Station 1

    ! JODC Raw
        depthindex = JODC_dep2index(obsdepth,info = .true.)
        ! allocate(x1d(0:depthindex),y1d(0:depthindex))
        ! call plot(8.,0.,-3)
        ! do i = 0,depthindex
        !     y1d(i) = real(JODC_index2dep(i))
        ! end do

        ! call newpen2(6);call newpen2(-6);call rgbk(0.4,1.,0.4)
        ! call plot(7./2.,0.,3);call plot(7./2.,-15.,2)
        ! call rgbk(0.,0.,0.)
        ! do i = 12,0,-1
        !     do j = 123,142
        !         do k = 24,46
        !             if(i == 0)then 
        !                 red = 1.;green = 0.4;blue = 0.4; dotsize = 0.1
        !             else ;red = 0.4;green= 0.4;blue = 0.4 ; dotsize = 0.15
        !             end if
        !             x1d = sal%mean(i,j,k,0:depthindex)
        !             call helper_scatter(x1d,y1d,7.,-15.,xi = 30.,xf = 35.,yi = 0.,yf = 400.,r = red,g = green,b = blue,dotsize = dotsize)
        !         end do
        !     end do
        ! end do
        ! call rgbk(0.,0.,0.)
        ! call memori(41,0.1,5,-15.,rangle = -90.,y = -15./2.)
        ! call num_memori(35.,30.,51,10,0.6,-1,7.,rangle = 180.,x = 7.,y = 0.1)
        ! call symbolc(3.5,2.,0.8,'JODC, Raw')

    ! JODC Raw

    ! Linear Interpolation of JODC Salinity   !     IS NECESSARY FOR LATER PROGRAMS
        do i = 12,0,-1
            do j = 123,142
                do k = 24,46
                    do l = 0, depthindex ! is 12 
                        dep(l) = JODC_index2dep(l)
                        Jsal400(i,j,k,dep(l)) = sal%mean(i,j,k,l)
                        if(l==0)cycle
                        ! depdiff = dep(l) - dep(l-1)
                        
                        if(sal%mean(i,j,k,l)<20..or.sal%mean(i,j,k,l)==0.)then ! cycle if the salinity of a certain layer is suspicious
                            sal%mean(i,j,k,l) = 0.;cycle
                        end if
                        ! sal%mean(i,j,k,l) /= 0. is satisfied, next looking at the layer above
                        if(sal%mean(i,j,k,l-1)<20..or.sal%mean(i,j,k,l-1)==0.)then ! if the layer above has no data
                            if(l == 1)then 
                                Jsal400(i,j,k,0:dep(1)) = sal%mean(i,j,k,1);cycle
                            end if
                            if(sal%mean(i,j,k,l-2)<20..or.sal%mean(i,j,k,l-2)==0.)then
                                ! print*,'no data for 2 layers above at',i,j,k,l
                            else
                                depdiff = dep(l) - dep(l-2)
                                do n = 1, depdiff-1 ! linear interpolation
                                    Jsal400(i,j,k,dep(l-2)+n) = sal%mean(i,j,k,l-2) + (sal%mean(i,j,k,l) - sal%mean(i,j,k,l-2))/depdiff*n
                                end do 
                                ! print*,'Interpolated 2 layers above at',i,j,k,l
                            end if
                        else; ! if the layer above has data
                            depdiff = dep(l) - dep(l-1)
                            do n = 1, depdiff-1 ! linear interpolation
                                Jsal400(i,j,k,dep(l-1)+n) = sal%mean(i,j,k,l-1) + (sal%mean(i,j,k,l) - sal%mean(i,j,k,l-1))/depdiff*n
                            end do
                            ! print*,'Interpolated 1 layer above at',i,j,k,l
                        end if


                        ! print*,dep(l),depdiff
                    end do
                end do
            end do
        end do
    ! Linear Interpolation of JODC Salinity

    ! call plot(8.,0.,-3)
    ! call newpen2(6);call newpen2(-6);call rgbk(0.4,1.,0.4)
    ! call plot(7./2.,0.,3);call plot(7./2.,-15.,2)
    ! call rgbk(0.,0.,0.)

    ! JODC Interpolated
        ! allocate(Jx(0:obsdepth),Jy(0:obsdepth))
        ! Jy = [(i,i = 0,obsdepth)]
        ! do i = 12,0,-1
        !     do j = 123,142
        !         do k = 24,46
        !             if(i == 0)then 
        !                 red = 1.;green = 0.4;blue = 0.4; dotsize = 0.02
        !             else ;red = 0.4;green= 0.4;blue = 0.4 ; dotsize = 0.05
        !             end if
        !             Jx = Jsal400(i,j,k,:)
        !             call helper_scatter(Jx,Jy,7.,-15.,xi = 30.,xf = 35.,yi = 0.,yf = 400.,r = red,g = green,b = blue,dotsize = dotsize)
        !         end do
        !     end do
        ! end do
        ! call memori(41,0.1,5,-15.,rangle = -90.,y = -15./2.)
        ! call num_memori(35.,30.,51,10,0.6,-1,7.,rangle = 180.,x = 7.,y = 0.1)
        ! call symbolc(3.5,2.,0.8,'JODC, Interpolated')
    ! JODC Interpolated

    call plote 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! End of Initial Inspection

call plots2(oopt = 'otops',x = -0.5, y = -5.,h = 'Freshwater Volume, 1°*1°*'//trim(int2str(watercolumnheight))//'m',nnfile = 'FreshWaterVolumeKuroshioRef_'//trim(int2str(watercolumnheight))//'m')

    meansalinity = 0.
    ! Calculation of Mean Salinity of water column with depth of watercolumnheightm
        do i = 12,0,-1
            do j = 123,142
                do k = 24,46
                    ! print*,count(Jsal400(i,j,k,0:watercolumnheight)>20.)
                    if(count(Jsal400(i,j,k,0:watercolumnheight)>20.)==0)then 
                        ! print*,'No data of array(0:watercolumnheight) at',i,j,k
                    elseif(count(Jsal400(i,j,k,0:watercolumnheight)>20.)<watercolumnheight)then 
                        ! print*,'Data quantity less than watercolumnheight of array(0:watercolumnheight) at',i,j,k,count(Jsal400(i,j,k,0:watercolumnheight)>20.)
                    ! elseif(all(Jsal400(i,j,k,101:watercolumnheight)==0.))then 
                        ! print*,'No data deeper than 100m at',i,j,k
                    else ! sufficient data quantity and depth
                        meansalinity(i,j,k) = sum(Jsal400(i,j,k,0:watercolumnheight))/real(watercolumnheight+1)
                        ! print*,'Mean Salinity of water column with depth of watercolumnheightm at',i,j,k,'is',meansalinity
                    end if
                end do
            end do
        end do
    ! log
        ! call openlog(basename = 'JODC_meansal_uptowatercolumnheightm')
        ! write(tolog,*)'Maximum and Minimum of Mean Salinity of water column with depth of watercolumnheightm'
        ! write(tolog,*)'the array indices are 0:12(seasons), 123:142(longitude), 24:46(latitude)'
        ! write(tolog,*) maxval(meansalinity),minval(meansalinity,mask = meansalinity/=0.)
        ! write(tolog,*) maxloc(meansalinity),minloc(meansalinity,mask = meansalinity/=0.)

        ! write(tolog,*) 'max and min for every month'
        ! do i = 0,12
        !     write(tolog,*) maxval(meansalinity(i,:,:)),minval(meansalinity(i,:,:),mask = meansalinity(i,:,:)/=0.)
        !     write(tolog,*) maxloc(meansalinity(i,:,:)),minloc(meansalinity(i,:,:),mask = meansalinity(i,:,:)/=0.)
        ! end do
        ! call closelog
    !

        sref = maxval(meansalinity(:,:,:)) ! sref is the max sal of yearly mean
        print*,sref
        ! sref  = 35.0


        do i = 0,12
            do j = 123,142
                do k = 24,46
                    sobs = meansalinity(i,j,k)
                    if(sobs == 0.)then 
                        freshwatervolume(i,j,k) = 0.;cycle
                    end if
                    freshwatervolume(i,j,k) = gridvolume * ((sref - sobs)/sref)
                    ! print*,sobs,((sref - sobs)/sref),freshwatervolume(i,j,k)
                end do
            end do
        end do
        ! freshwatervolume = freshwatervolume/10.**(9) ! convert to km^3
        freshwatervolume = freshwatervolume / lon1deg / lat1deg ! volume / columnheight * 1m^2
        freshwaterratio = freshwatervolume / watercolumnheight * 100. ! percentage of freshwater volume in the water column
        print*,minval(freshwatervolume,mask = freshwatervolume/=0.),maxval(freshwatervolume)
        print*,minval(freshwaterratio,mask = freshwaterratio/=0.),maxval(freshwaterratio)

        ! print*,freshwatervolume

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! monthly
        call plotsave('first')
        do i = 1,12
            call simple_map(126,142,32,46,width,symbols = .true.,height = height,symbol_freq = 4)
            call symbolc(width/2.,height+0.2,0.8,monthnames(i))
            call butler_psk(freshwatervolume(i,126:142,32:46),width,height,0.,2.5,5.5,.5,'blue',6,conti = 0.,continc = .2,r = r1,g = g1,b = b1)
            call plot(width+0.8,0.,-3)
            if(i == 6)then 
                call plotback('first');call plot(0.,-height-2.,-3)
            end if
        end do

        call ocenter(y = -6.)
        call colorscale(r1,g1,b1,2.5,5.5,2,0.7,1,10.,0.3,lt = 1, gt = 1)
        call symbolc(0.,-2.,0.8,'H*1*1[m^3]')
    ! monthly
    ! monthly ratio
        call newpage
        call plotback('first')
        do i = 1,12
            call simple_map(126,142,32,46,width,symbols = .true.,height = height,symbol_freq = 4)
            call symbolc(width/2.,height+0.2,0.8,monthnames(i))
            call butler_psk(freshwaterratio(i,126:142,32:46),width,height,0.,2.,4.,.5,'blue',4,conti = 0.,continc = .1,r = r1,g = g1,b = b1)
            call plot(width+0.8,0.,-3)
            if(i == 6)then 
                call plotback('first');call plot(0.,-height-2.,-3)
            end if
        end do

        call ocenter(y = -6.)
        call colorscale(r1,g1,b1,2.,4.,2,0.7,1,10.,0.3,lt = 1, gt = 1)
        call symbolc(0.,-2.,0.8,'[%]')
    ! monthly ratio
    ! monthly diff
        call newpage
        call plotback('first')
        do i = 1,12
            if(i == 1)then 
                do j = 123,142
                    do k = 24,46
                        if(freshwatervolume(i,j,k)/=0. .and. freshwatervolume(12,j,k)/=0.)then 
                            somejodcarray(i,j,k) = freshwatervolume(i,j,k) - freshwatervolume(12,j,k)
                        else
                            somejodcarray(i,j,k) = 0.
                        end if  
                    end do
                end do
            else
                do j = 123,142
                    do k = 24,46
                        if(freshwatervolume(i,j,k)/=0. .and. freshwatervolume(i-1,j,k)/=0.)then 
                            somejodcarray(i,j,k) = freshwatervolume(i,j,k) - freshwatervolume(i-1,j,k)
                        else
                            somejodcarray(i,j,k) = 0.
                        end if  
                    end do
                end do
            end if
            call simple_map(126,142,32,46,width,symbols = .true.,height = height,symbol_freq = 4)
            if(i==1)call symbolc(width/2.,height+0.2,0.8,monthnames(i)//' - '//monthnames(12))
            if(i/=1)call symbolc(width/2.,height+0.2,0.8,monthnames(i)//' - '//monthnames(i-1))
            call butler_psk(somejodcarray(i,126:142,32:46),width,height,0.,-1.,1.,.2,'r2b',10,6,conti = -100.,continc = .2,r = r1,g = g1,b = b1)
            call plot(width+0.8,0.,-3)
            if(i == 6)then 
                call plotback('first');call plot(0.,-height-2.,-3)
            end if
        end do
        print*,minval(somejodcarray,mask = somejodcarray/=0.),maxval(somejodcarray)

        call ocenter(y = -6.)
        call colorscale(r1,g1,b1,-1.,1.,5,0.7,1,10.,0.3,lt = 1, gt = 1)
        call symbolc(0.,-2.,0.8,'H*1*1[m^3]')
    ! monthly diff
    ! monthly diff ratio
        call newpage
        call plotback('first')
        somejodcarray = somejodcarray / watercolumnheight * 100.
        do i = 1,12
            call simple_map(126,142,32,46,width,symbols = .true.,height = height,symbol_freq = 4)
            if(i==1)call symbolc(width/2.,height+0.2,0.8,monthnames(i)//' - '//monthnames(12))
            if(i/=1)call symbolc(width/2.,height+0.2,0.8,monthnames(i)//' - '//monthnames(i-1))
            call butler_psk(somejodcarray(i,126:142,32:46),width,height,0.,-0.5,0.5,.1,'r2b',10,6,conti = -100.,continc = .1,r = r1,g = g1,b = b1)
            call plot(width+0.8,0.,-3)
            if(i == 6)then 
                call plotback('first');call plot(0.,-height-2.,-3)
            end if
        end do
        print*,minval(somejodcarray,mask = somejodcarray/=0.),maxval(somejodcarray)

        call ocenter(y = -6.)
        call colorscale(r1,g1,b1,-0.5,0.5,5,0.7,1,10.,0.3,lt = 1, gt = 1)
        call symbolc(0.,-2.,0.8,'[%]')
    ! monthly diff ratio


    call plote
    

end program 
