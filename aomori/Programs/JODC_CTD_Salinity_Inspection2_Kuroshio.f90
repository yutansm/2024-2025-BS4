program testing_butler_psbet
    use always 
    integer,parameter::obsdepth = 400, watercolumnheight = 200
    integer::depthindex,depdiff,dep(0:12),meanmaxlocation(3),maximumlocation(4)
    real,dimension(:),allocatable::r1,g1,b1,x1d,y1d,Jx,Jy,r2,g2,b2,r3,g3,b3
    logical::stat
    real::x,red,green,blue,dotsize,meansalinity(0:12,121:142,22:46),kuroshiomeansalinity(0:12,121:142,22:46),sref,sobs,freshwatervolume(0:12,121:142,22:46),gridvolume,height,width = 4.,height2
    real::lon1deg,lat1deg,freshwaterratio(0:12,121:142,22:46),somejodcarray(0:12,121:142,22:46),maximumsalinity
    type(JODC_TS)::sal,temp
    type(JODC_RHO)::rho
    real::Jsal400(0:12,121:142,22:46,0:400),Jsal2(13,0:400),Jrho400(0:12,121:142,22:46,0:400),Jrho2(13,0:400),Jtemp400(0:12,121:142,22:46,0:400),Jtemp2(13,0:400)
    ! linear interpolation for the first 12 layers 

    lon1deg = 6400.*cos(39.*pi/180.)*2.*pi/360. * 10.**(3) ! in m
    lat1deg = 2*pi*6400./360. * 10**(3) ! in m
    gridvolume = lon1deg * lat1deg * real(watercolumnheight)
    print*,gridvolume
    call calibrated_data2(sal_c5 = sal_c5,potemp_c5 = potemp_c5,match_station_labels_and_array_indices = .true.)
    call JODC_data2(potemp = temp,sal = sal,den = rho,ilat = 22,flat = 46,ilon = 121,flon = 142,calibrate1 = .true.,info = .true.)
    ! sal%mean(:,121:131,22:24,:) = 0.! removing out of range kuroshio region
    ! sal%mean(:,126:131,22:25,:) = 0.
    ! sal%mean(:,128:131,25:27,:) = 0.
    ! sal%mean(:,130:131,27:28,:) = 0.


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Initial Inspection
    ! call plots2(oopt = 'otops',x = 2., y = -2.,h = 'Salinity Profiles',nnfile = 'JODC_SAL_126-142E_22-46N')
    ! ! Station 1 
    !     call num_memori(0.,400.,41,5,0.7,-1,-15.,rangle = -90.)
    !     call symbolc(3.5,2.,0.8,'St1')
    !     do y = 1, 15
    !         do m = 1, 12
    !             ! if(y==1.and.m==1)then;stat = .true.;else;stat = .false.;end if
    !             ! call butler_linegraph(sal_c5(y,m,1,1,:),15.,7.,22.5,35.,mem=stat,memloc = 'left',memsymfreq = 5,memfreq = 0.1,rotation = -90.,memflqt = 1,lthick = 1,memsymsize = 0.6)
    !             call helper_linegraph(sal_c5(y,m,1,1,:),15.,7.,32.5,35.,mem = .false.,rotation = -90.,lthick = 1)
    !             if(y==1.and.m==1)call num_memori2(32.5,35.,7.,0.1,-0.,5,0.7)
    !         end do
    !     end do
    ! ! Station 1

    ! ! JODC Raw
    !     allocate(x1d(0:depthindex),y1d(0:depthindex))
    !     call plot(8.,0.,-3)
    !     do i = 0,depthindex
    !         y1d(i) = real(JODC_index2dep(i))
    !     end do

    !     call newpen2(6);call newpen2(-6);call rgbk(0.4,1.,0.4)
    !     call plot(7./2.,0.,3);call plot(7./2.,-15.,2)
    !     call rgbk(0.,0.,0.)
    !     do i = 12,0,-1
    !         do j = 121,142
    !             do k = 22,46
    !                 if(i == 0)then 
    !                     red = 1.;green = 0.4;blue = 0.4; dotsize = 0.1
    !                 else ;red = 0.4;green= 0.4;blue = 0.4 ; dotsize = 0.15
    !                 end if
    !                 x1d = sal%mean(i,j,k,0:depthindex)
    !                 call helper_scatter(x1d,y1d,7.,-15.,xi = 30.,xf = 35.,yi = 0.,yf = 400.,r = red,g = green,b = blue,dotsize = dotsize)
    !             end do
    !         end do
    !     end do
    !     call rgbk(0.,0.,0.)
    !     call memori(41,0.1,5,-15.,rangle = -90.,y = -15./2.)
    !     call num_memori(35.,30.,51,10,0.6,-1,7.,rangle = 180.,x = 7.,y = 0.1)
    !     call symbolc(3.5,2.,0.8,'JODC, Raw')

    ! ! JODC Raw

    ! Linear Interpolation of JODC Salinity   !     IS NECESSARY FOR LATER PROGRAMS
        depthindex = JODC_dep2index(obsdepth,info = .true.)
        do i = 12,0,-1
            do j = 121,142
                do k = 22,46
                    do l = 0, depthindex ! is 12 
                        dep(l) = JODC_index2dep(l)
                        Jsal400(i,j,k,dep(l)) = sal%mean(i,j,k,l)
                        Jtemp400(i,j,k,dep(l)) = temp%mean(i,j,k,l)
                        call sigma_T_S(Jrho400(i,j,k,dep(l)),Jtemp400(i,j,k,dep(l)),Jsal400(i,j,k,dep(l)))
                        if(l==0)cycle
                        ! depdiff = dep(l) - dep(l-1)
                        
                        if(sal%mean(i,j,k,l)<20..or.sal%mean(i,j,k,l)==0.)then ! cycle if the salinity of a certain layer is suspicious
                            sal%mean(i,j,k,l) = 0.;temp%mean(i,j,k,l) = 0.;cycle
                        end if
                        ! sal%mean(i,j,k,l) /= 0. is satisfied, next looking at the layer above
                        if(sal%mean(i,j,k,l-1)<20..or.sal%mean(i,j,k,l-1)==0.)then ! if the layer above has no data
                            if(l == 1)then 
                                Jsal400(i,j,k,0:dep(1)) = sal%mean(i,j,k,1);Jtemp400(i,j,k,0:dep(1)) = temp%mean(i,j,k,1)
                                cycle
                            end if
                            if(sal%mean(i,j,k,l-2)<20..or.sal%mean(i,j,k,l-2)==0.)then
                                ! print*,'no data for 2 layers above at',i,j,k,l
                            else
                                depdiff = dep(l) - dep(l-2)
                                do n = 1, depdiff-1 ! linear interpolation
                                    Jsal400(i,j,k,dep(l-2)+n) = sal%mean(i,j,k,l-2) + (sal%mean(i,j,k,l) - sal%mean(i,j,k,l-2))/depdiff*n
                                    Jtemp400(i,j,k,dep(l-2)+n) = temp%mean(i,j,k,l-2) + (temp%mean(i,j,k,l) - temp%mean(i,j,k,l-2))/depdiff*n
                                    call sigma_T_S(Jrho400(i,j,k,dep(l-2)+n),Jtemp400(i,j,k,dep(l-2)+n),Jsal400(i,j,k,dep(l-2)+n))
                                end do 
                                print*,'Interpolated 2 layers above at',i,j,k,l
                            end if
                        else; ! if the layer above has data
                            depdiff = dep(l) - dep(l-1)
                            do n = 1, depdiff-1 ! linear interpolation
                                Jsal400(i,j,k,dep(l-1)+n) = sal%mean(i,j,k,l-1) + (sal%mean(i,j,k,l) - sal%mean(i,j,k,l-1))/depdiff*n
                                Jtemp400(i,j,k,dep(l-1)+n) = temp%mean(i,j,k,l-1) + (temp%mean(i,j,k,l) - temp%mean(i,j,k,l-1))/depdiff*n
                                call sigma_T_S(Jrho400(i,j,k,dep(l-1)+n),Jtemp400(i,j,k,dep(l-1)+n),Jsal400(i,j,k,dep(l-1)+n))
                            end do
                            ! print*,'Interpolated 1 layer above at',i,j,k,l
                        end if
                        ! density

                    end do
                end do
            end do
        end do
        allocate(Jx(0:obsdepth),Jy(0:obsdepth))
        Jy = [(i,i = 0,obsdepth)]
    ! Linear Interpolation of JODC Salinity

    ! call plot(8.,0.,-3)
    ! call newpen2(6);call newpen2(-6);call rgbk(0.4,1.,0.4)
    ! call plot(7./2.,0.,3);call plot(7./2.,-15.,2)
    ! call rgbk(0.,0.,0.)

    ! !JODC Interpolated
    !     do i = 12,0,-1
    !         do j = 126,142 ! nihonkai
    !             do k = 32,46
    !                 if(i == 0)then 
    !                     red = 1.;green = 0.4;blue = 0.4; dotsize = 0.02
    !                 else ;red = 0.4;green= 0.4;blue = 0.4 ; dotsize = 0.05
    !                 end if
    !                 Jx = Jsal400(i,j,k,:)
    !                 call helper_scatter(Jx,Jy,7.,-15.,xi = 30.,xf = 35.,yi = 0.,yf = 400.,r = red,g = green,b = blue,dotsize = dotsize)
    !             end do
    !         end do
    !     end do
    !     call memori(41,0.1,5,-15.,rangle = -90.,y = -15./2.)
    !     call num_memori(35.,30.,51,10,0.6,-1,7.,rangle = 180.,x = 7.,y = 0.1)
    !     call symbolc(3.5,2.,0.8,'JODC, Interpolated'//';0m : '//trim(int2str(obsdepth))//'m;'//'126-142E,32-46N')
    ! !JODC Interpolated

    call plote 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! End of Initial Inspection
    meansalinity = 0.;kuroshiomeansalinity = 0.
    ! Calculation of Mean Salinity of water column with depth of watercolumnheightm > good (only in the kuroshio region)
        do i = 12,0,-1
            do j = 121,142
                do k = 22,46
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
                    if(j<=129.and.k<=34)then ! kuroshio high sal region
                        if(temp%mean(i,j,k,JODC_dep2index(200))>=13..and.temp%mean(i,j,k,JODC_dep2index(200))<=17.)then 
                            kuroshiomeansalinity(i,j,k) = meansalinity(i,j,k)
                            ! print*,'kuroshio region??',i,j,k,temp%mean(i,j,k,JODC_dep2index(200)),meansalinity(i,j,k)
                        end if
                    end if
                end do
            end do
        end do

        ! ! botsu
        !     meanmaxlocation = maxloc(kuroshiomeansalinity) ! max no
        !     print*,'meanmaxlocation',meanmaxlocation
        !     kuroshiomeansalinity(meanmaxlocation(1)-1,meanmaxlocation(2)+120,meanmaxlocation(3)+21) = 0.

        !     meanmaxlocation = maxloc(kuroshiomeansalinity) ! 2nd max no
        !     print*,'meanmaxlocation',meanmaxlocation
        !     kuroshiomeansalinity(meanmaxlocation(1)-1,meanmaxlocation(2)+120,meanmaxlocation(3)+21) = 0.

        !     meanmaxlocation = maxloc(kuroshiomeansalinity) ! 3rd max no
        !     print*,'meanmaxlocation',meanmaxlocation
        !     kuroshiomeansalinity(meanmaxlocation(1)-1,meanmaxlocation(2)+120,meanmaxlocation(3)+21) = 0.

        !     meanmaxlocation = maxloc(kuroshiomeansalinity) ! 4th max no
        !     print*,'meanmaxlocation',meanmaxlocation
        !     kuroshiomeansalinity(meanmaxlocation(1)-1,meanmaxlocation(2)+120,meanmaxlocation(3)+21) = 0.

        !     meanmaxlocation = maxloc(kuroshiomeansalinity) ! 5th max no
        !     print*,'meanmaxlocation',meanmaxlocation
        !     kuroshiomeansalinity(meanmaxlocation(1)-1,meanmaxlocation(2)+120,meanmaxlocation(3)+21) = 0.

        !     meanmaxlocation = maxloc(kuroshiomeansalinity) ! 6th max
        !     print*,'meanmaxlocation',meanmaxlocation
        !     kuroshiomeansalinity(meanmaxlocation(1)-1,meanmaxlocation(2)+120,meanmaxlocation(3)+21) = 0.

        !     meanmaxlocation = maxloc(kuroshiomeansalinity) ! 7th max
        !     print*,'meanmaxlocation',meanmaxlocation
        !     kuroshiomeansalinity(meanmaxlocation(1)-1,meanmaxlocation(2)+120,meanmaxlocation(3)+21) = 0.

        !     meanmaxlocation = maxloc(kuroshiomeansalinity) ! 8th max
        !     print*,'meanmaxlocation',meanmaxlocation
        !     kuroshiomeansalinity(meanmaxlocation(1)-1,meanmaxlocation(2)+120,meanmaxlocation(3)+21) = 0.

        !     meanmaxlocation = maxloc(kuroshiomeansalinity) ! 9th max
        !     print*,'meanmaxlocation',meanmaxlocation
        ! ! botsu

        ! meanmaxlocation(1) = meanmaxlocation(1) -1
        ! meanmaxlocation(2) = meanmaxlocation(2) + 120
        ! meanmaxlocation(3) = meanmaxlocation(3) + 21


        ! sref  = kuroshiomeansalinity(meanmaxlocation(1),meanmaxlocation(2),meanmaxlocation(3))
        ! print*,sref,meanmaxlocation,'meanmaxlocation,Temp at meanmaxlocation',temp%mean(meanmaxlocation(1),meanmaxlocation(2),meanmaxlocation(3),JODC_dep2index(200)),count(kuroshiomeansalinity/=0.)
    ! ends

        sref = 34.7

    call plots2(oopt = 'otops',x = -0.5, y = -6.,nnfile = 'aaa'//trim(int2str(watercolumnheight))//'m')
    call plotsave('first')
    ! JODC Interpolated
        call plot(3.,4.,-3)
        Jy = [(i,i = 0,obsdepth)]
        do i = 12,0,-1
            do j = 121,129
                do k = 22,34
                    if(kuroshiomeansalinity(i,j,k)==0.)cycle ! kuroshio region??

                    if(i == 0)then 
                        ! red = 1.;green = 0.4;blue = 0.4; dotsize = 0.02
                        cycle
                    else if(i>=1 .and. i<=3)then 
                        red = 0.4;green = 0.4;blue = 1.; dotsize = 0.04 ! jan - mar is blue
                    else if(i>=4 .and. i<=6)then 
                        red = 0.4;green = 1.;blue = .4; dotsize = 0.09 ! apr - jul is green
                    else if(i>=7 .and. i<=9)then 
                        red = 1.;green = 0.4;blue = 0.4; dotsize = 0.13 ! aug - oct is red
                    else if(i>=10 .and. i<=12)then
                        red = 1.;green = 1.;blue = 0.4; dotsize = 0.17 ! nov - dec is yellow
                    else 
                        ! red = 0.4;green= 0.4;blue = 0.4 ; dotsize = 0.05
                    end if
                    Jx = Jsal400(i,j,k,:)
                    call helper_scatter(Jx,Jy,7.,-15.,xi = 33.,xf = 35.,yi = 0.,yf = 400.,r = red,g = green,b = blue,dotsize = dotsize)
                end do
            end do
        end do
        call num_memori2(0.,400.,-15.,10.,-90.,5,0.8,-1)
        call num_memori2(35.,33.,7.,.5,180.,1,0.7,1,x = 7.,y = 0.1)
        call symbolc(3.5,3.,0.8,'JODC, Interpolated;0m : '//trim(int2str(obsdepth))//'m;'//'121-129N,22-34E')
        ! Jx = Jsal400(meanmaxlocation(1),meanmaxlocation(2),meanmaxlocation(3),:)
        ! call helper_scatter(Jx,Jy,7.,-15.,xi = 33.,xf = 35.,yi = 0.,yf = 400.,r = 0.,g = 0.,b = 0.,dotsize = 0.20)
        ! call helper_scatter(Jx,Jy,7.,-15.,xi = 33.,xf = 35.,yi = 0.,yf = 400.,r = 0.4,g = 1.,b = 0.4,dotsize = 0.06)

        Jx = 0.
        call plot(9.,0.,-3)
        call symbolc(3.5,3.,0.8,'JODC, Interpolated;0m : '//trim(int2str(obsdepth))//'m;'//'121-129N,22-34E')
        do i = 12,0,-1
            do j = 121,129
                do k = 22,34
                    if(kuroshiomeansalinity(i,j,k)==0.)cycle ! kuroshio region??
                    if(i == 0)then 
                        ! red = 1.;green = 0.4;blue = 0.4; dotsize = 0.02
                        cycle
                    else if(i>=1 .and. i<=3)then 
                        red = 0.4;green = 0.4;blue = 1.; dotsize = 0.07 ! jan - mar is blue
                    else if(i>=4 .and. i<=6)then 
                        red = 0.4;green = 1.;blue = .4; dotsize = 0.11 ! apr - jul is green
                    else if(i>=7 .and. i<=9)then 
                        red = 1.;green = 0.4;blue = 0.4; dotsize = 0.16 ! aug - oct is red
                    else if(i>=10 .and. i<=12)then
                        red = 1.;green = 1.;blue = 0.4; dotsize = 0.20 ! nov - dec is yellow
                    else 
                        ! red = 0.4;green= 0.4;blue = 0.4 ; dotsize = 0.05
                    end if
                    Jx = Jsal400(i,j,k,:)
                    call helper_scatter(Jx(:obsdepth),Jy(:obsdepth),7.,-15.,xi = 34.2,xf = 34.8,yi = 0.,yf = 400.,r = red,g = green,b = blue,dotsize = dotsize,maskxi = -999.,maskxf = 34.2)
                end do
            end do
        end do
        ! Jx = Jsal400(meanmaxlocation(1),meanmaxlocation(2),meanmaxlocation(3),:)
        ! call helper_scatter(Jx(:obsdepth),Jy(:obsdepth),7.,-15.,xi = 34.2,xf = 34.8,yi = 0.,yf = 400.,r = 0.,g = 0.,b = 0.,dotsize = 0.25,maskxi = -999.,maskxf = 34.2)
        ! call helper_scatter(Jx(:obsdepth),Jy(:obsdepth),7.,-15.,xi = 34.2,xf = 34.8,yi = 0.,yf = 400.,r = 0.4,g = 1.,b = 0.4,dotsize = 0.09,maskxi = -999.,maskxf = 34.2)
        ! call newpen2(3);call newpen2(-4)
        ! call plot(7.*1.1/1.2,0.,3);call plot(7.*1.1/1.2,-15.,2)
        call newpen2(3)
        call num_memori2(0.,400.,-15.,10.,-90.,5,0.8,-1)
        call num_memori2(34.8,34.2,7.,0.1,180.,2,0.7,1,x = 7., y = 0.1)
        call plot(9.,0.,-3)
        
        call plotsave('first2')
        ! call symbolc(width+2.,0.,0.5,';location of;maximum;watercolumn;mean Sal;'//monthnames(meanmaxlocation(1))//';lon = N'//trim(int2str(meanmaxlocation(3)))//';lat = E'//trim(int2str(meanmaxlocation(2))))
        height = mapheight(width,121,135,22,34)
        do i = 0, 4
            if(i==3)cycle
            call butler_psbet(Jsal400(0,121:135,22:34,i*100),width,height,0.,34.,34.8,0.1,'b2r',8,5,r = r2,g = g2,b = b2)
            call GEBCOmap2(121,135,22,34,width,symbols = .true.,height = height,symbol_freqx = 2,symbol_freqy = 4,paintland = .true.,bathy = .false.)
            ! call box(width/15.,height/13.,5,x = width*(meanmaxlocation(2)-121)/15.,y = height*(meanmaxlocation(3)-22)/13.,g = 1.)
            call butler_cont(temp%mean(meanmaxlocation(1),121:135,22:34,JODC_dep2index(i*100)),width,height,0.,0.,1.,thicc = 5)
            call symbolc(width/2.,height+0.1,0.6,int2str(i*100)//' m')
            if(i/=4)call plot(0.,-height-1.1,-3)
        end do
        ! print*,meansalinity(meanmaxlocation(1),meanmaxlocation(2),meanmaxlocation(3)),'check meanmaxlocation '
        call colorscale(r2,g2,b2,34.,34.8,5,0.7,1,width,0.2,lt = 1, gt = 1,x = width+0.2,y = height/2.,rangle = 90.)
        ! print*,width*(meanmaxlocation(2)-121)/11.,height*(meanmaxlocation(3)-22)/10.
    ! JODC Interpolated

        do i = 0,12
            do j = 121,142
                do k = 22,46
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
        print*,minval(freshwatervolume,mask = freshwatervolume/=0.),maxval(freshwatervolume),'freshwater volume'
        print*,minval(freshwaterratio,mask = freshwaterratio/=0.),maxval(freshwaterratio),'freshwater ratio'

        ! print*,freshwatervolume

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! ! monthly not shown
    !     call newpage(h = 'Freshwater Volume, 1°*1°*'//trim(int2str(watercolumnheight))//'m')
    !     call plotback('first')
    !     do i = 1,12
    !         call GEBCOmap2(126,142,32,46,width,symbols = .true.,height = height,symbol_freqx = 2,paintland = .true.)
    !         call symbolc(width/2.,height+0.2,0.8,monthnames(i))
    !         ! call butler_psk(freshwatervolume(i,121:131,22:31),width,height,0.,2.5,5.5,.5,'blue',6,conti = 0.,continc = .2,r = r1,g = g1,b = b1)
    !         call butler_psk(freshwatervolume(i,126:142,32:46),width,height,0.,2.5,4.5,.5,'cy2b',4,bpt1 = 3,conti = 0.,continc = .2,r = r1,g = g1,b = b1)
    !         ! call box(width*10./21.,height*9./24.,3)
    !         call plot(width+0.8,0.,-3)
    !         if(i == 6)then 
    !             call plotback('first');call plot(0.,-height-2.,-3)
    !         end if
    !     end do

    !     call ocenter(y = -7.)
    !     call colorscale(r1,g1,b1,2.5,4.5,1,0.7,1,10.,0.3,lt = 1, gt = 1)
    !     call symbolc(0.,-2.,0.8,'H*1*1[m^3]')
    ! ! monthly
    ! Annual
        call newpage(h = 'Freshwater Volume, 1°*1°*'//trim(int2str(watercolumnheight))//'m;Annual')
        call plotback('first');call plot(.7,-5.,-3);call plotsave('second')

            call GEBCOmap2(126,142,32,46,width*2,symbols = .true.,height = height,symbol_freqx = 2,symbol_freqy = 4,paintland = .true.)
            call symbolc(width,height+0.2,0.8,'Annual')
            freshwatervolume(0,140,42) = 0. ! tsugaru weird
            freshwatervolume(0,141,40) = 0. ! on land grid 
            freshwatervolume(0,134:142,32:34) = 0. ! pacific
            freshwatervolume(0,142,35:42) = 0. ! east of tsugaru
            freshwatervolume(0,141,35:38) = 0.
            freshwatervolume(0,137:140,35) = 0.
            PRINT*,freshwatervolume(0,137:140,35),'here'

            call butler_psbet(freshwatervolume(0,126:142,32:46),width*2,height,0.,2.5,4.5,.5,'cy2b',4,bpt1 = 3,conti = 0.,continc = .2,r = r1,g = g1,b = b1)
            
            call plot(width*2+1.5,0.,-3)

            call GEBCOmap2(126,142,32,46,width*2,symbols = .true.,height = height,symbol_freqx = 2,symbol_freqy = 4,paintland = .true.)
            call symbolc(width,height+0.2,0.8,'Annual')
            call butler_psbet(freshwatervolume(0,126:142,32:46),width*2,height,0.,2.5,4.5,.5,'cy2b',4,bpt1 = 3,conti = 0.,continc = .2,r = r1,g = g1,b = b1)
            ! call butler_gridcoords(freshwatervolume(0,126:142,32:46),width*2,height)

            ! RUS grids
            ! call box(width*2/17.,height/15.,7,x = 2*width*2/17.,y = 7*height/15.,r = .8) ! (3,8)
            call box(width*2/17.,height/15.,7,x = 3*width*2/17.,y = 7*height/15.,r = 1.) ! (4,8)
            call gmark(3.5*width*2/17.,7.5*height/15.,0.5,1);call rgbk(1.,1.,1.);call numberc(3.5*width*2/17.,7.2*height/15.,0.5,1.,0.,-1) ! (4,8)
            call box(width*2/17.,height/15.,7,x = 4*width*2/17.,y = 8*height/15.,g = 1.) ! (5,9)
            call gmark(4.5*width*2/17.,8.5*height/15.,0.5,1);call rgbk(1.,1.,1.);call numberc(4.5*width*2/17.,8.2*height/15.,0.5,2.,0.,-1) ! (5,9)
            call box(width*2/17.,height/15.,7,x = 4*width*2/17.,y = 9*height/15.,b = 1.) ! (5,10)
            call gmark(4.5*width*2/17.,9.5*height/15.,0.5,1);call rgbk(1.,1.,1.);call numberc(4.5*width*2/17.,9.2*height/15.,0.5,3.,0.,-1) ! (5,10)
            ! NJP grids
            ! call box(width*2/17.,height/15.,7,x = 11*width*2/17.,y = 4*height/15.,r = .8) ! (12,5)
            ! call box(width*2/17.,height/15.,7,x = 12*width*2/17.,y = 6*height/15.,r = 1.) ! (13,7)
            ! call box(width*2/17.,height/15.,7,x = 13*width*2/17.,y = 6*height/15.,g = 1.) ! (14,7)
            ! call box(width*2/17.,height/15.,7,x = 13*width*2/17.,y = 7*height/15.,b = 1.) ! (14,8)
            call box(width*2/17.,height/15.,7,x = 13*width*2/17.,y = 7*height/15.,r = 1.) ! (14,8)
            call gmark(13.5*width*2/17.,7.5*height/15.,0.5,1);call rgbk(1.,1.,1.);call numberc(13.5*width*2/17.,7.2*height/15.,0.5,1.,0.,-1) ! (14,8)
            call box(width*2/17.,height/15.,7,x = 13*width*2/17.,y = 8*height/15.,g = 1.) ! (14,9)
            call gmark(13.5*width*2/17.,8.5*height/15.,0.5,1);call rgbk(1.,1.,1.);call numberc(13.5*width*2/17.,8.2*height/15.,0.5,2.,0.,-1) ! (14,9)
            call box(width*2/17.,height/15.,7,x = 14*width*2/17.,y = 9*height/15.,b = 1.) ! (15,10)
            call gmark(14.5*width*2/17.,9.5*height/15.,0.5,1);call rgbk(1.,1.,1.);call numberc(14.5*width*2/17.,9.2*height/15.,0.5,3.,0.,-1) ! (15,10)
            ! ECS grids
            call box(width*2/17.,height/15.,7,x = 4*width*2/17.,y = 3*height/15.,r = 1.) ! (5,4)
            call gmark(4.5*width*2/17.,3.5*height/15.,0.5,1);call rgbk(1.,1.,1.);call numberc(4.5*width*2/17.,3.2*height/15.,0.5,1.,0.,-1) ! (5,4)
            call box(width*2/17.,height/15.,7,x = 5*width*2/17.,y = 3*height/15.,g = 1.) ! (6,4)
            call gmark(5.5*width*2/17.,3.5*height/15.,0.5,1);call rgbk(1.,1.,1.);call numberc(5.5*width*2/17.,3.2*height/15.,0.5,2.,0.,-1) ! (6,4)
            call box(width*2/17.,height/15.,7,x = 6*width*2/17.,y = 3*height/15.,b = 1.) ! (7,4)
            call gmark(6.5*width*2/17.,3.5*height/15.,0.5,1);call rgbk(1.,1.,1.);call numberc(6.5*width*2/17.,3.2*height/15.,0.5,3.,0.,-1) ! (7,4)
            ! call box(width*2/17.,height/15.,7,x = 8*width*2/17.,y = 3*height/15.) ! (9,4)
            ! call box(width*2/17.,height/15.,7,x = 5*width*2/17.,y = 3*height/15.,r = 1.) ! (6,4)
            ! call box(width*2/17.,height/15.,7,x = 6*width*2/17.,y = 3*height/15.,g = 1.) ! (7,4)
            ! call box(width*2/17.,height/15.,7,x = 7*width*2/17.,y = 4*height/15.,b = 1.) ! (8,5)

            call plot(width*2+1.5,0.,-3)

            !  FOR INSPECTION PURPOSES, DATANUM
            ! call GEBCOmap2(126,142,32,46,width*2,symbols = .true.,height = height,symbol_freqx = 2,symbol_freqy = 4,paintland = .true.)
            ! call butler_datanum(freshwatervolume(:,126:142,32:46),width*2,height,1)
            ! call symbolc(width,height+0.2,0.8,'Data num')

            call ocenter(y = -7.)
            call colorscale(r1,g1,b1,2.5,4.5,1,0.7,1,10.,0.3,lt = 1, gt = 1)
            call symbolc(0.,-2.,0.8,'H*1*1[m^3]')
    ! Annual

    ! Regional analysis
        height2 = height/2.
        call newpage(h = 'Regional Analysis')
        call plotback('first');call plot(1.,.5,-3);call plotsave('third')
            ! legend
            call box(width*2/17.,height/15.,7,x = 4*width*2/17.,y = 3*height/15.,r = 1.) ! (5,4)
            call gmark(4.5*width*2/17.,3.5*height/15.,0.5,1);call rgbk(1.,1.,1.);call numberc(4.5*width*2/17.,3.2*height/15.,0.5,1.,0.,-1) ! (5,4)
            call box(width*2/17.,height/15.,7,x = 5.3*width*2/17.,y = 3*height/15.,g = 1.) ! (6,4)
            call gmark(5.8*width*2/17.,3.5*height/15.,0.5,1);call rgbk(1.,1.,1.);call numberc(5.8*width*2/17.,3.2*height/15.,0.5,2.,0.,-1) ! (6,4)
            call box(width*2/17.,height/15.,7,x = 6.6*width*2/17.,y = 3*height/15.,b = 1.) ! (7,4)
            call gmark(7.1*width*2/17.,3.5*height/15.,0.5,1);call rgbk(1.,1.,1.);call numberc(7.1*width*2/17.,3.2*height/15.,0.5,3.,0.,-1) ! (7,4)

        ! ECS
        ! 130,35 red (5,4)
        call butler_linegraph([freshwatervolume(1:,125+5,31+4),freshwatervolume(1,125+5,31+4)],8.,height2,1.,6.,0.,.true.,blabel = 'Months',rl = 1.,tlabel = 'ECS',lthick = 6)
        ! 131,35 green(6,4)
        call butler_linegraph([freshwatervolume(1:,125+6,31+4),freshwatervolume(1,125+6,31+4)],8.,height2,1.,6.,0.,.false.,gl = 1.,lthick = 6)
        ! 132,35 blue(7,4)
        call butler_linegraph([freshwatervolume(1:,125+7,31+4),freshwatervolume(1,125+7,31+4)],8.,height2,1.,6.,0.,.false.,bl = 1.,lthick = 5)
        call mod12_memori(13,8.,0.6,gap = 2)
        call newpen2d(4,-4)
        call plot(0.,(sum(freshwatervolume(0,125+5:125+7,31+4))/3.-1)/5.*height2,3);call plot(8.,(sum(freshwatervolume(0,125+5:125+7,31+4))/3.-1)/5.*height2,2)
        ! ! 134,35
        ! call butler_linegraph([freshwatervolume(1:,125+9,31+4),freshwatervolume(1,125+9,31+4)],8.,height2,1.,6.,0.,.false.,lthick = 5)
        ! call mod12_memori(13,8.,0.6,gap = 2) 

        ! ! red (6,4) ! just an inspection diff ref botsu
        ! call butler_linegraph([freshwatervolume(1:,125+6,31+4),freshwatervolume(1,125+6,31+4)],8.,height2,1.,6.,0.,.true.,blabel = 'Months',rl = 1.,tlabel = 'ECS',lthick = 6)
        ! call mod12_memori(13,8.,0.6,gap = 2)
        ! ! green(7,4)
        ! call butler_linegraph([freshwatervolume(1:,125+7,31+4),freshwatervolume(1,125+7,31+4)],8.,height2,1.,6.,0.,.false.,gl = 1.,lthick = 6)
        ! call mod12_memori(13,8.,0.6,gap = 2)
        ! ! blue(8,5)
        ! call butler_linegraph([freshwatervolume(1:,125+8,31+5),freshwatervolume(1,125+8,31+5)],8.,height2,1.,6.,0.,.false.,bl = 1.,lthick = 5)
        ! call mod12_memori(13,8.,0.6,gap = 2)

        call header('Monthly Salinity',y = -9.4)
        call plot(0.,-4.,-3)
        call num_memori2(0.,300.,-height2,50.,-90.,2,0.7,-1)
        call mod12_memori(13,8.,gap = 2,y = -height2)
        Jsal2(1:12,:) = Jsal400(1:12,125+6,31+4,:);Jsal2(13,:) = Jsal2(1,:) ! using 6,4
        Jrho2(1:12,:) = Jrho400(1:12,125+6,31+4,:);Jrho2(13,:) = Jrho2(1,:) ! using 6,4
        ! Jsal2(1:12,:) = Jsal400(1:12,125+8,31+5,:);Jsal2(13,:) = Jsal2(1,:) ! using 8,5
        ! Jrho2(1:12,:) = Jrho400(1:12,125+8,31+5,:);Jrho2(13,:) = Jrho2(1,:) ! using 8,5
        ! print*,Jrho2(1,:)
        call butler_psk(Jsal2(:,:300), 8., -height2, 0., 33.8, 34.4, 0.1, 'b2r', 6, bpt1 = 4)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,20.,1.,thicc = 1,r = 1.,g = 1.,b = 1.)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,27.,1.,thicc = 1,contq = 1)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,27.1,0.1)
        call symbolc(4.,0.2,1.,'ECS-2')

        call box(0.5,0.5,8,x = 3.75,y = -height2-1.5,g = 1.)
        call gmark(4.,-height2-1.25,0.5,1);call rgbk(1.,1.,1.);call numberc(4.,-height2-1.4,0.5,2.,0.,-1)

        ! NJP
        call plotback('third');call plot(8.+1.5,0.,-3)
        ! 137,36
        ! call butler_linegraph([freshwatervolume(1:,125+12,31+5),freshwatervolume(1,125+12,31+5)],8.,height2,1.,6.,0.,.true.,blabel = 'Months',rl = .8,tlabel = 'NJP',lthick = 6)
        ! call mod12_memori(13,8.,0.6,gap = 2)
        ! ! 138,37
        ! call butler_linegraph([freshwatervolume(1:,125+13,31+6),freshwatervolume(1,125+13,31+6)],8.,height2,1.,6.,0.,.false.,rl = 1.,lthick = 6,blabel = 'Months',tlabel = 'NJP')
        ! call mod12_memori(13,8.,0.6,gap = 2)
        ! ! 139,38
        ! call butler_linegraph([freshwatervolume(1:,125+14,31+7),freshwatervolume(1,125+14,31+7)],8.,height2,1.,6.,0.,.false.,gl = 1.,lthick = 5)
        ! call mod12_memori(13,8.,0.6,gap = 2)
        ! ! 139,39
        ! call butler_linegraph([freshwatervolume(1:,125+14,31+8),freshwatervolume(1,125+14,31+8)],8.,height2,1.,6.,0.,.false.,bl = 1.,lthick = 5)
        ! call mod12_memori(13,8.,0.6,gap = 2)

        ! 14,8 ! diff ref
        call butler_linegraph([freshwatervolume(1:,125+14,31+8),freshwatervolume(1,125+13,31+6)],8.,height2,1.,6.,0.,.false.,rl = 1.,lthick = 6,blabel = 'Months',tlabel = 'NJP')
        call mod12_memori(13,8.,0.6,gap = 2)
        ! 14,9
        call butler_linegraph([freshwatervolume(1:,125+14,31+9),freshwatervolume(1,125+14,31+7)],8.,height2,1.,6.,0.,.false.,gl = 1.,lthick = 5)
        call mod12_memori(13,8.,0.6,gap = 2)
        ! 15,10
        call butler_linegraph([freshwatervolume(1:,125+15,31+10),freshwatervolume(1,125+14,31+8)],8.,height2,1.,6.,0.,.false.,bl = 1.,lthick = 5)
        call mod12_memori(13,8.,0.6,gap = 2)
        call newpen2d(4,-4)
        total = (freshwatervolume(0,125+14,31+8) + freshwatervolume(0,125+14,31+9) + freshwatervolume(0,125+15,31+10))/3.
        call plot(0.,(total-1)/5.*height2,3);call plot(8.,(total-1)/5.*height2,2)


        call plot(0.,-4.,-3)
        call memori(7,0.15,2,-height2,rangle = -90.,y = -height2/2.)
        call mod12_memori(13,8.,gap = 2,y = -height2)
        Jsal2(1:12,:) = Jsal400(1:12,125+14,31+9,:);Jsal2(13,:) = Jsal2(1,:)
        ! Jrho(1:12,0:12) = rho%mean(1:12,125+14,31+8,0:12);Jrho(13,:) = Jrho(1,:)
        Jrho2(1:12,:) = Jrho400(1:12,125+14,31+9,:);Jrho2(13,:) = Jrho2(1,:)
        call butler_psk(Jsal2(:,:300), 8., -height2, 0., 33.8, 34.4, 0.1, 'b2r', 6, bpt1 = 4,r = r3,g = g3,b = b3)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,20.,1.,thicc = 1,r = 1.,g = 1.,b = 1.)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,27.,1.,thicc = 1,contq = 1)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,27.1,0.1)
        call symbolc(4.,0.2,1.,'NJP-2')

        call box(0.5,0.5,8,x = 3.75,y = -height2-1.5,b = 1.)
        call gmark(4.,-height2-1.25,0.5,1);call rgbk(1.,1.,1.);call numberc(4.,-height2-1.4,0.5,2.,0.,-1)
        call colorscale(r3,g3,b3,33.8,34.4,2,0.7,1,10.,0.3,lt = 1, gt = 1,y = -height2-2.25,x = 4.)



        ! RUS
        call plotback('third');call plot(2*(8.+1.5),0.,-3)
        ! 128,39
        ! call butler_linegraph([freshwatervolume(1:,125+3,31+8),freshwatervolume(1,125+3,31+8)],8.,height2,1.,6.,0.,.true.,rl = .8,blabel = 'Months',tlabel = 'RUS',lthick = 6)
        ! call mod12_memori(13,8.,0.6,gap = 2)
        ! 129,39
        call butler_linegraph([freshwatervolume(1:,125+4,31+8),freshwatervolume(1,125+4,31+8)],8.,height2,1.,6.,0.,.false.,rl = 1.,lthick = 6,tlabel = 'RUS',blabel = 'Months')
        call mod12_memori(13,8.,0.6,gap = 2)
        ! 130,40
        call butler_linegraph([freshwatervolume(1:,125+5,31+9),freshwatervolume(1,125+5,31+9)],8.,height2,1.,6.,0.,.false.,gl = 1.,lthick = 5)
        call mod12_memori(13,8.,0.6,gap = 2)
        ! 130,41
        call butler_linegraph([freshwatervolume(1:,125+5,31+10),freshwatervolume(1,125+5,31+10)],8.,height2,1.,6.,0.,.false.,bl = 1.,lthick = 5)
        call mod12_memori(13,8.,0.6,gap = 2)
        call newpen2d(4,-4)
        total = (freshwatervolume(0,125+4,31+8) + freshwatervolume(0,125+5,31+9) + freshwatervolume(0,125+5,31+10))/3.
        call plot(0.,(total-1)/5.*height2,3);call plot(8.,(total-1)/5.*height2,2)

        call plot(0.,-4.,-3)
        call memori(7,0.15,2,-height2,rangle = -90.,y = -height2/2.)
        call mod12_memori(13,8.,gap = 2,y = -height2)
        Jsal2(1:12,:) = Jsal400(1:12,125+5,31+10,:);Jsal2(13,:) = Jsal2(1,:)
        ! Jrho(1:12,0:12) = rho%mean(1:12,125+5,31+10,0:12);Jrho(13,:) = Jrho(1,:)
        Jrho2(1:12,:) = Jrho400(1:12,125+5,31+10,:);Jrho2(13,:) = Jrho2(1,:)
        call butler_psk(Jsal2(:,:300), 8., -height2, 0., 33.8, 34.4, 0.1, 'b2r', 6, bpt1 = 4)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,20.,1.,thicc = 1,r = 1.,g = 1.,b = 1.)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,27.,1.,thicc = 1,contq = 1)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,27.1,0.1)
        call symbolc(4.,0.2,1.,'RUS-3')
        call box(0.5,0.5,8,x = 3.75,y = -height2-1.5,b = 1.)
        call gmark(4.,-height2-1.25,0.5,1);call rgbk(1.,1.,1.);call numberc(4.,-height2-1.4,0.5,3.,0.,-1)

        

    ! Regional analysis

    ! Regional analysis appendix

        call newpage(h = 'RUS')
        call plotback('third')
        ! RUS
        ! 128,39
        ! call butler_linegraph([freshwatervolume(1:,125+3,31+8),freshwatervolume(1,125+3,31+8)],8.,height2,1.,6.,0.,.true.,rl = .8,blabel = 'Months',tlabel = 'RUS',lthick = 6)
        ! call mod12_memori(13,8.,0.6,gap = 2)
        ! 129,39
        call butler_linegraph([freshwatervolume(1:,125+4,31+8),freshwatervolume(1,125+4,31+8)],8.,height2,1.,6.,0.,.false.,rl = 1.,lthick = 6,tlabel = 'RUS',blabel = 'Months')
        call mod12_memori(13,8.,0.6,gap = 2)
        ! 130,40
        call butler_linegraph([freshwatervolume(1:,125+5,31+9),freshwatervolume(1,125+5,31+9)],8.,height2,1.,6.,0.,.false.,gl = 1.,lthick = 5)
        call mod12_memori(13,8.,0.6,gap = 2)
        ! 130,41
        call butler_linegraph([freshwatervolume(1:,125+5,31+10),freshwatervolume(1,125+5,31+10)],8.,height2,1.,6.,0.,.false.,bl = 1.,lthick = 5)
        call mod12_memori(13,8.,0.6,gap = 2)

        ! Salinity Profiles of each grid
        call plotback('third');call plot(0.,-2.,-3)
        call num_memori2(0.,300.,-height2,50.,-90.,2,0.7,-1)
        ! call mod12_memori(13,6.,gap = 2,y = -height2)
        ! Jsal2(1:12,:) = Jsal400(1:12,125+3,31+8,:);Jsal2(13,:) = Jsal2(1,:)
        ! call butler_psk(Jsal2(:,:300), 6., -height2, 0., 33.8, 34.4, 0.1, 'b2r', 6, bpt1 = 4)
        ! call box(0.5,0.5,8,x = 2.75,y = -height2-1.5,r = .8)
        ! call plot(7.,0.,-3)
        ! call memori(7,0.15,2,-height2,rangle = -90.,y = -height2/2.)
        call mod12_memori(13,8.,gap = 2,y = -height2)
        Jsal2(1:12,:) = Jsal400(1:12,125+4,31+8,:);Jsal2(13,:) = Jsal2(1,:)
        ! Jrho(1:12,0:12) = rho%mean(1:12,125+4,31+8,0:12);Jrho(13,:) = Jrho(1,:)
        Jrho2(1:12,:) = Jrho400(1:12,125+4,31+8,:);Jrho2(13,:) = Jrho2(1,:)
        Jtemp2(1:12,:) = Jtemp400(1:12,125+4,31+8,:);Jtemp2(13,:) = Jtemp2(1,:)
        call butler_psk(Jsal2(:,:300), 8., -height2, 0., 33.8, 34.4, 0.1, 'b2r', 6, bpt1 = 4)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,20.,1.,thicc = 1,r = 1.,g = 1.,b = 1.)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,27.,1.,thicc = 1,contq = 1)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,27.1,0.1)
        call butler_cont(Jtemp2(:,:300),8.,-height2,0.,0.,5.,g = 1.)
        call box(0.5,0.5,8,x = 3.75,y = -height2-1.5,r = 1.)
        call plot(9.5,0.,-3)
        call memori(7,0.15,2,-height2,rangle = -90.,y = -height2/2.)
        call mod12_memori(13,8.,gap = 2,y = -height2)
        Jsal2(1:12,:) = Jsal400(1:12,125+5,31+9,:);Jsal2(13,:) = Jsal2(1,:)
        ! Jrho(1:12,0:12) = rho%mean(1:12,125+5,31+9,0:12);Jrho(13,:) = Jrho(1,:)
        Jrho2(1:12,:) = Jrho400(1:12,125+5,31+9,:);Jrho2(13,:) = Jrho2(1,:)
        Jtemp2(1:12,:) = Jtemp400(1:12,125+5,31+9,:);Jtemp2(13,:) = Jtemp2(1,:)
        call butler_psk(Jsal2(:,:300), 8., -height2, 0., 33.8, 34.4, 0.1, 'b2r', 6, bpt1 = 4)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,20.,1.,thicc = 1,r = 1.,g = 1.,b = 1.)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,27.,1.,thicc = 1,contq = 1)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,27.1,0.1)
        call butler_cont(Jtemp2(:,:300),8.,-height2,0.,0.,5.,g = 1.)
        call box(0.5,0.5,8,x = 3.75,y = -height2-1.5,g = 1.)
        call colorscale(r3,g3,b3,33.8,34.4,2,0.7,1,10.,0.3,lt = 1, gt = 1,y = -height2-3.,x = 4.)
        call plot(9.5,0.,-3)
        call memori(7,0.15,2,-height2,rangle = -90.,y = -height2/2.)
        call mod12_memori(13,8.,gap = 2,y = -height2)
        Jsal2(1:12,:) = Jsal400(1:12,125+5,31+10,:);Jsal2(13,:) = Jsal2(1,:)
        ! Jrho(1:12,0:12) = rho%mean(1:12,125+5,31+10,0:12);Jrho(13,:) = Jrho(1,:)
        Jrho2(1:12,:) = Jrho400(1:12,125+5,31+10,:);Jrho2(13,:) = Jrho2(1,:)
        Jtemp2(1:12,:) = Jtemp400(1:12,125+5,31+10,:);Jtemp2(13,:) = Jtemp2(1,:)
        call butler_psk(Jsal2(:,:300), 8., -height2, 0., 33.8, 34.4, 0.1, 'b2r', 6, bpt1 = 4)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,20.,1.,thicc = 1,r = 1.,g = 1.,b = 1.)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,27.,1.,thicc = 1,contq = 1)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,27.1,0.1)
        call butler_cont(Jtemp2(:,:300),8.,-height2,0.,0.,5.,g = 1.)
        call box(0.5,0.5,8,x = 3.75,y = -height2-1.5,b = 1.)

        ! NJP
        call newpage(h = 'NJP')
        call plotback('third')
        ! call butler_linegraph([freshwatervolume(1:,125+12,31+5),freshwatervolume(1,125+12,31+5)],8.,height2,1.,6.,0.,.true.,blabel = 'Months',rl = .8,tlabel = 'NJP',lthick = 6)
        ! call mod12_memori(13,8.,0.6,gap = 2)
        ! ! 138,37
        ! call butler_linegraph([freshwatervolume(1:,125+13,31+6),freshwatervolume(1,125+13,31+6)],8.,height2,1.,6.,0.,.false.,rl = 1.,lthick = 6,tlabel = 'NJP',blabel = 'Months')
        ! call mod12_memori(13,8.,0.6,gap = 2)
        ! ! 139,38
        ! call butler_linegraph([freshwatervolume(1:,125+14,31+7),freshwatervolume(1,125+14,31+7)],8.,height2,1.,6.,0.,.false.,gl = 1.,lthick = 5)
        ! call mod12_memori(13,8.,0.6,gap = 2)
        ! ! 139,39
        ! call butler_linegraph([freshwatervolume(1:,125+14,31+8),freshwatervolume(1,125+14,31+8)],8.,height2,1.,6.,0.,.false.,bl = 1.,lthick = 5)
        ! call mod12_memori(13,8.,0.6,gap = 2)
        ! 14,8 ! diff ref
        call butler_linegraph([freshwatervolume(1:,125+14,31+8),freshwatervolume(1,125+13,31+6)],8.,height2,1.,6.,0.,.false.,rl = 1.,lthick = 6,tlabel = 'NJP',blabel = 'Months')
        call mod12_memori(13,8.,0.6,gap = 2)
        ! 14,9
        call butler_linegraph([freshwatervolume(1:,125+14,31+9),freshwatervolume(1,125+14,31+7)],8.,height2,1.,6.,0.,.false.,gl = 1.,lthick = 5)
        call mod12_memori(13,8.,0.6,gap = 2)
        ! 15,9
        call butler_linegraph([freshwatervolume(1:,125+15,31+9),freshwatervolume(1,125+14,31+8)],8.,height2,1.,6.,0.,.false.,bl = 1.,lthick = 5)
        call mod12_memori(13,8.,0.6,gap = 2)
        call symbolc(4.,0.2,0.7,'NJP-15,9')
        print*,freshwatervolume(1:,125+15,31+9),'freshwatervolume(1:,125+15,31+9)'

        ! Salinity Profiles of each grid
        call plotback('third');call plot(0.,-2.,-3)
        call num_memori2(0.,300.,-height2,50.,-90.,2,0.7,-1)
        call mod12_memori(13,8.,gap = 2,y = -height2)
        ! prior references
            ! ! call mod12_memori(13,6.,gap = 2,y = -height2)
            ! ! Jsal2(1:12,:) = Jsal400(1:12,125+12,31+5,:);Jsal2(13,:) = Jsal2(1,:)
            ! ! call butler_psk(Jsal2(:,:300), 6., -height2, 0., 33.8, 34.4, 0.1, 'b2r', 6, bpt1 = 4)
            ! ! call box(0.5,0.5,8,x = 2.75,y = -height2-1.5,r = .8)
            ! ! call plot(7.,0.,-3)
            ! ! call memori(7,0.15,2,-height2,rangle = -90.,y = -height2/2.)
            ! call mod12_memori(13,8.,gap = 2,y = -height2)
            ! Jsal2(1:12,:) = Jsal400(1:12,125+13,31+6,:);Jsal2(13,:) = Jsal2(1,:)
            ! Jrho2(1:12,:) = Jrho400(1:12,125+13,31+6,:);Jrho2(13,:) = Jrho2(1,:)
            ! call butler_psk(Jsal2(:,:300), 8., -height2, 0., 33.8, 34.4, 0.1, 'b2r', 6, bpt1 = 4)
            ! call butler_cont(Jrho2(:,:300),8.,-height2,0.,20.,0.2,5)
            ! call box(0.5,0.5,8,x = 3.75,y = -height2-1.5,r = 1.)
            ! call plot(9.5,0.,-3)
            ! call memori(7,0.15,2,-height2,rangle = -90.,y = -height2/2.)
            ! call mod12_memori(13,8.,gap = 2,y = -height2)
            ! Jsal2(1:12,:) = Jsal400(1:12,125+14,31+7,:);Jsal2(13,:) = Jsal2(1,:)
            ! ! Jrho(1:12,0:12) = rho%mean(1:12,125+14,31+7,0:12);Jrho(13,:) = Jrho(1,:)
            ! Jrho2(1:12,:) = Jrho400(1:12,125+14,31+7,:);Jrho2(13,:) = Jrho2(1,:)
            ! call butler_psk(Jsal2(:,:300), 8., -height2, 0., 33.8, 34.4, 0.1, 'b2r', 6, bpt1 = 4)
            ! call butler_cont(Jrho2(:,:300),8.,-height2,0.,20.,0.2,5)
            ! call box(0.5,0.5,8,x = 3.75,y = -height2-1.5,g = 1.)
            ! call colorscale(r3,g3,b3,33.8,34.4,2,0.7,1,10.,0.3,lt = 1, gt = 1,y = -height2-3.,x = 4.)
            ! call plot(9.5,0.,-3)
            ! call memori(7,0.15,2,-height2,rangle = -90.,y = -height2/2.)
            ! call mod12_memori(13,8.,gap = 2,y = -height2)
            ! Jsal2(1:12,:) = Jsal400(1:12,125+14,31+8,:);Jsal2(13,:) = Jsal2(1,:)
            ! ! Jrho(1:12,0:12) = rho%mean(1:12,125+14,31+8,0:12);Jrho(13,:) = Jrho(1,:)
            ! Jrho2(1:12,:) = Jrho400(1:12,125+14,31+8,:);Jrho2(13,:) = Jrho2(1,:)
            ! call butler_psk(Jsal2(:,:300), 8., -height2, 0., 33.8, 34.4, 0.1, 'b2r', 6, bpt1 = 4)
            ! call butler_cont(Jrho2(:,:300),8.,-height2,0.,20.,0.2,5)
            ! call box(0.5,0.5,8,x = 3.75,y = -height2-1.5,b = 1.)
        ! diff ref below
        Jsal2(1:12,:) = Jsal400(1:12,125+14,31+8,:);Jsal2(13,:) = Jsal2(1,:)
        Jrho2(1:12,:) = Jrho400(1:12,125+14,31+8,:);Jrho2(13,:) = Jrho2(1,:)
        Jtemp2(1:12,:) = Jtemp400(1:12,125+14,31+8,:);Jtemp2(13,:) = Jtemp2(1,:)
        call butler_psk(Jsal2(:,:300), 8., -height2, 0., 33.8, 34.4, 0.1, 'b2r', 6, bpt1 = 4)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,20.,1.,thicc = 1,r = 1.,g = 1.,b = 1.)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,27.,1.,thicc = 1,contq = 1)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,27.1,0.1)
        call butler_cont(Jtemp2(:,:300),8.,-height2,0.,0.,5.,g = 1.)
        call box(0.5,0.5,8,x = 3.75,y = -height2-1.5,r = 1.)
        call plot(9.5,0.,-3)
        call memori(7,0.15,2,-height2,rangle = -90.,y = -height2/2.)
        call mod12_memori(13,8.,gap = 2,y = -height2)
        Jsal2(1:12,:) = Jsal400(1:12,125+14,31+9,:);Jsal2(13,:) = Jsal2(1,:)
        ! Jrho(1:12,0:12) = rho%mean(1:12,125+14,31+7,0:12);Jrho(13,:) = Jrho(1,:)
        Jrho2(1:12,:) = Jrho400(1:12,125+14,31+9,:);Jrho2(13,:) = Jrho2(1,:)
        Jtemp2(1:12,:) = Jtemp400(1:12,125+14,31+9,:);Jtemp2(13,:) = Jtemp2(1,:)
        call butler_psk(Jsal2(:,:300), 8., -height2, 0., 33.8, 34.4, 0.1, 'b2r', 6, bpt1 = 4)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,20.,1.,thicc = 1,r = 1.,g = 1.,b = 1.)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,27.,1.,thicc = 1,contq = 1)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,27.1,0.1)
        call butler_cont(Jtemp2(:,:300),8.,-height2,0.,0.,5.,g = 1.)
        call box(0.5,0.5,8,x = 3.75,y = -height2-1.5,g = 1.)
        call colorscale(r3,g3,b3,33.8,34.4,2,0.7,1,10.,0.3,lt = 1, gt = 1,y = -height2-3.,x = 4.)
        call plot(9.5,0.,-3)
        call memori(7,0.15,2,-height2,rangle = -90.,y = -height2/2.)
        call mod12_memori(13,8.,gap = 2,y = -height2)
        Jsal2(1:13,:) = Jsal400(0:12,125+15,31+9,:)
        ! ;Jsal2(13,:) = Jsal2(1,:) 15,9 の青は年平均は計算できるけど，季節変化は無理
        Jrho2(1:12,:) = Jrho400(1:12,125+15,31+9,:);Jrho2(13,:) = Jrho2(1,:)
        Jtemp2(1:12,:) = Jtemp400(1:12,125+15,31+9,:);Jtemp2(13,:) = Jtemp2(1,:)
        call butler_psbet(Jsal2(:,:300), 8., -height2, 0., 33.8, 34.4, 0.1, 'b2r', 6, bpt1 = 4)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,20.,1.,thicc = 1,r = 1.,g = 1.,b = 1.)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,27.,1.,thicc = 1,contq = 1)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,27.1,0.1)
        call butler_cont(Jtemp2(:,:300),8.,-height2,0.,0.,5.,g = 1.)
        call box(0.5,0.5,8,x = 3.75,y = -height2-1.5,b = 1.)
        call symbolc(4.,0.2,0.7,'NJP-15,9')
        ! ECS
        call newpage(h = 'ECS')
        call plotback('third')
        ! 130,35
        call butler_linegraph([freshwatervolume(1:,125+5,31+4),freshwatervolume(1,125+5,31+4)],8.,height2,1.,6.,0.,.true.,blabel = 'Months',rl = .8,tlabel = 'ECS',lthick = 6)
        call mod12_memori(13,8.,0.6,gap = 2)
        ! 131,35
        call butler_linegraph([freshwatervolume(1:,125+6,31+4),freshwatervolume(1,125+6,31+4)],8.,height2,1.,6.,0.,.false.,gl = 1.,lthick = 6)
        call mod12_memori(13,8.,0.6,gap = 2)
        ! 132,35
        call butler_linegraph([freshwatervolume(1:,125+7,31+4),freshwatervolume(1,125+7,31+4)],8.,height2,1.,6.,0.,.false.,bl = 1.,lthick = 5)
        call mod12_memori(13,8.,0.6,gap = 2)
        ! 134,35
        ! call butler_linegraph([freshwatervolume(1:,125+9,31+4),freshwatervolume(1,125+9,31+4)],8.,height2,1.,6.,0.,.false.,lthick = 5)
        ! call mod12_memori(13,8.,0.6,gap = 2)

        ! Salinity Profiles of each grid
        call plotback('third');call plot(0.,-2.,-3)
        call num_memori2(0.,300.,-height2,50.,-90.,2,0.7,-1)
        ! call mod12_memori(13,6.,gap = 2,y = -height2)
        ! Jsal2(1:12,:) = Jsal400(1:12,125+5,31+4,:);Jsal2(13,:) = Jsal2(1,:)
        ! call butler_psk(Jsal2(:,:300), 6., -height2, 0., 33.8, 34.4, 0.1, 'b2r', 6, bpt1 = 4)
        ! call box(0.5,0.5,8,x = 2.75,y = -height2-1.5,r = .8)
        ! call plot(7.,0.,-3)
        ! call memori(7,0.15,2,-height2,rangle = -90.,y = -height2/2.)
        call mod12_memori(13,8.,gap = 2,y = -height2)
        Jsal2(1:12,:) = Jsal400(1:12,125+6,31+4,:);Jsal2(13,:) = Jsal2(1,:)
        ! Jrho(1:12,0:12) = rho%mean(1:12,125+6,31+4,0:12);Jrho(13,:) = Jrho(1,:)
        Jrho2(1:12,:) = Jrho400(1:12,125+6,31+4,:);Jrho2(13,:) = Jrho2(1,:)
        Jtemp2(1:12,:) = Jtemp400(1:12,125+6,31+4,:);Jtemp2(13,:) = Jtemp2(1,:)
        call butler_psk(Jsal2(:,:300), 8., -height2, 0., 33.8, 34.4, 0.1, 'b2r', 6, bpt1 = 4)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,20.,1.,thicc = 1,r = 1.,g = 1.,b = 1.)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,27.,1.,thicc = 1,contq = 1)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,27.1,0.1)
        call butler_cont(Jtemp2(:,:300),8.,-height2,0.,0.,5.,g = 1.)
        call box(0.5,0.5,8,x = 3.75,y = -height2-1.5,r = 1.)
        call plot(9.5,0.,-3)
        call memori(7,0.15,2,-height2,rangle = -90.,y = -height2/2.)
        call mod12_memori(13,8.,gap = 2,y = -height2)
        Jsal2(1:12,:) = Jsal400(1:12,125+7,31+4,:);Jsal2(13,:) = Jsal2(1,:)
        ! Jrho(1:12,0:12) = rho%mean(1:12,125+7,31+4,0:12);Jrho(13,:) = Jrho(1,:)
        Jrho2(1:12,:) = Jrho400(1:12,125+7,31+4,:);Jrho2(13,:) = Jrho2(1,:)
        Jtemp2(1:12,:) = Jtemp400(1:12,125+7,31+4,:);Jtemp2(13,:) = Jtemp2(1,:)
        call butler_psk(Jsal2(:,:300), 8., -height2, 0., 33.8, 34.4, 0.1, 'b2r', 6, bpt1 = 4)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,20.,1.,thicc = 1,r = 1.,g = 1.,b = 1.)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,27.,1.,thicc = 1,contq = 1)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,27.1,0.1)
        call butler_cont(Jtemp2(:,:300),8.,-height2,0.,0.,5.,g = 1.)
        call box(0.5,0.5,8,x = 3.75,y = -height2-1.5,g = 1.)
        call colorscale(r3,g3,b3,33.8,34.4,2,0.7,1,10.,0.3,lt = 1, gt = 1,y = -height2-3.,x = 4.)
        call plot(9.5,0.,-3)
        call memori(7,0.15,2,-height2,rangle = -90.,y = -height2/2.)
        call mod12_memori(13,8.,gap = 2,y = -height2)
        Jsal2(1:12,:) = Jsal400(1:12,125+9,31+4,:);Jsal2(13,:) = Jsal2(1,:)
        ! Jrho(1:12,0:12) = rho%mean(1:12,125+9,31+4,0:12);Jrho(13,:) = Jrho(1,:)
        Jrho2(1:12,:) = Jrho400(1:12,125+9,31+4,:);Jrho2(13,:) = Jrho2(1,:)
        Jtemp2(1:12,:) = Jtemp400(1:12,125+9,31+4,:);Jtemp2(13,:) = Jtemp2(1,:)
        call butler_psk(Jsal2(:,:300), 8., -height2, 0., 33.8, 34.4, 0.1, 'b2r', 6, bpt1 = 4)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,20.,1.,thicc = 1,r = 1.,g = 1.,b = 1.)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,27.,1.,thicc = 1,contq = 1)
        call butler_cont(Jrho2(:,:300),8.,-height2,0.,27.1,0.1)
        call butler_cont(Jtemp2(:,:300),8.,-height2,0.,0.,5.,g = 1.)
        call box(0.5,0.5,8,x = 3.75,y = -height2-1.5,b = 1.)

    ! Regional analysis appendix

    ! monthly ratio not shown
    !     call newpage
    !     call plotback('first')
    !     do i = 1,12
    !         call GEBCOmap2(121,142,22,46,width,symbols = .true.,height = height,symbol_freqx = 2,paintland = .false.)
    !         call symbolc(width/2.,height+0.2,0.8,monthnames(i))
    !         call butler_psk(freshwaterratio(i,:,:),width,height,0.,2.,4.,.5,'blue',4,conti = 0.,continc = .1,r = r1,g = g1,b = b1)
    !         call plot(width+0.8,0.,-3)
    !         if(i == 6)then 
    !             call plotback('first');call plot(0.,-height-2.,-3)
    !         end if
    !     end do

    !     call ocenter(y = -7.)
    !     call colorscale(r1,g1,b1,2.,4.,2,0.7,1,10.,0.3,lt = 1, gt = 1)
    !     call symbolc(0.,-2.,0.8,'[%]')
    ! monthly ratio
    ! ! monthly diff not shown
    !     call newpage(h = 'Difference in Freshwater Volume, 1°*1°*'//trim(int2str(watercolumnheight))//'m')
    !     call plotback('first')
    !     do i = 1,12
    !         if(i == 1)then 
    !             do j = 121,142
    !                 do k = 22,46
    !                     if(freshwatervolume(i,j,k)/=0. .and. freshwatervolume(12,j,k)/=0.)then 
    !                         somejodcarray(i,j,k) = freshwatervolume(i,j,k) - freshwatervolume(12,j,k)
    !                     else
    !                         somejodcarray(i,j,k) = 0.
    !                     end if  
    !                 end do
    !             end do
    !         else
    !             do j = 121,142
    !                 do k = 22,46
    !                     if(freshwatervolume(i,j,k)/=0. .and. freshwatervolume(i-1,j,k)/=0.)then 
    !                         somejodcarray(i,j,k) = freshwatervolume(i,j,k) - freshwatervolume(i-1,j,k)
    !                     else
    !                         somejodcarray(i,j,k) = 0.
    !                     end if  
    !                 end do
    !             end do
    !         end if
    !         call GEBCOmap2(126,142,32,46,width,symbols = .true.,height = height,symbol_freqx = 2,paintland = .false.)
    !         if(i==1)call symbolc(width/2.,height+0.2,0.8,monthnames(i)//' - '//monthnames(12))
    !         if(i/=1)call symbolc(width/2.,height+0.2,0.8,monthnames(i)//' - '//monthnames(i-1))
    !         call butler_psk(somejodcarray(i,126:142,32:46),width,height,0.,-.8,.8,.4,'r2b',4,3,conti = -100.,continc = .2,r = r1,g = g1,b = b1)
    !         ! call box(width*10./21.,height*9./24.,3)
    !         call plot(width+0.8,0.,-3)
    !         if(i == 6)then 
    !             call plotback('first');call plot(0.,-height-2.,-3)
    !         end if
    !     end do
    !     print*,minval(somejodcarray,mask = somejodcarray/=0.),maxval(somejodcarray)

    !     call ocenter(y = -7.)
    !     call colorscale(r1,g1,b1,-.8,.8,1,0.7,1,10.,0.3,lt = 1, gt = 1)
    !     call symbolc(0.,-2.,0.8,'H*1*1[m^3]')
    ! ! monthly diff
    ! monthly diff ratio not shown
        ! call newpage
        ! call plotback('first')
        ! somejodcarray = somejodcarray / watercolumnheight * 100.
        ! do i = 1,12
        !     call GEBCOmap2(121,142,22,46,width,symbols = .true.,height = height,symbol_freqx = 2,paintland = .false.)
        !     if(i==1)call symbolc(width/2.,height+0.2,0.8,monthnames(i)//' - '//monthnames(12))
        !     if(i/=1)call symbolc(width/2.,height+0.2,0.8,monthnames(i)//' - '//monthnames(i-1))
        !     call butler_psk(somejodcarray(i,:,:),width,height,0.,-0.5,0.5,.1,'r2b',10,6,conti = -100.,continc = .1,r = r1,g = g1,b = b1)
        !     call plot(width+0.8,0.,-3)
        !     if(i == 6)then 
        !         call plotback('first');call plot(0.,-height-2.,-3)
        !     end if
        ! end do
        ! print*,minval(somejodcarray,mask = somejodcarray/=0.),maxval(somejodcarray)

        ! call ocenter(y = -7.)
        ! call colorscale(r1,g1,b1,-0.5,0.5,5,0.7,1,10.,0.3,lt = 1, gt = 1)
        ! call symbolc(0.,-2.,0.8,'[%]')
    ! monthly diff ratio


    call plote
    

end program 
