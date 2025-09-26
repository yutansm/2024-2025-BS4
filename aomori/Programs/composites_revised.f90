program composites_warmcold_by_months
    ! this has monthly composites of warm and cold waters at st4 250m depth
    use always
    real,dimension(12*15,2,9,400)::Pseries,Sseries,Vseries
    real,dimension(15,12,9,400)::Pwarm2 = 0.,Swarm2 = 0.,Dwarm2 = 0.
    real,dimension(15,12,8,400)::Vwarm2 = 0. ! quantity in the month,months, st, depth
    real,dimension(:,:),allocatable::Pmeanwarm,Pmeancold,Smeanwarm,Smeancold,Dmeanwarm,Dmeancold,Vmeanwarm,Vmeancold
    real,dimension(15,12,9,400)::Pcold2 = 0.,Scold2 = 0.,Dcold2 = 0.
    real,dimension(15,12,8,400)::Vcold2 = 0. !  quantity in the month,months st, depth
    real,dimension(:),allocatable::r,g,b
    ! real::logger(12,15)


    call calibrated_data2(potemp_c5,sal_c5,sigma_c5,geovel_c5 = geovel_c5,match_station_labels_and_array_indices = .true.)
    ! print*,geovel_c5
    ! stop
    do i = 1, 2
        do j = 1, 9
            do k = 1, 400
                Pseries(:,i,j,k) = reshape(transpose(potemp_c5(:,:,i,j,k)),[12*15])
                Sseries(:,i,j,k) = reshape(transpose(sal_c5(:,:,i,j,k)),[12*15])
                if(j/=9)Vseries(:,i,j,k) = reshape(transpose(geovel_c5(:,:,i,j,k)),[12*15])

            end do
        end do
    end do
    ! Pseries, Sseries, Vseries are now 12*15 x 2 x 9 x 400 arrays
    ! looking at temp of st4 depth 250 at n line
    call avsemdata_1D(Pseries(:,1,4,250),mean = Pmean,s = Psd)
    print*, 'Pmean', Pmean
    print*, 'Psem', Psd

    icountwarm = 0
    icountcold = 0

    width = 2.;height=4.

    do i = 1, 15
        do j = 1, 12
            if(potemp_c5(i,j,1,4,250)==0.)cycle ! skip if no data
            if(potemp_c5(i,j,1,4,250)>Pmean+Psd)then
                icountwarm = icountwarm + 1
                Pwarm2(i,j,:,:) = potemp_c5(i,j,1,:,:)
                Swarm2(i,j,:,:) = sal_c5(i,j,1,:,:)
                Dwarm2(i,j,:,:) = sigma_c5(i,j,1,:,:)
                Vwarm2(i,j,:,:) = geovel_c5(i,j,1,:,:)
            else if(potemp_c5(i,j,1,4,250)<Pmean)then
                icountcold = icountcold + 1
                Pcold2(i,j,:,:) = potemp_c5(i,j,1,:,:)
                Scold2(i,j,:,:) = sal_c5(i,j,1,:,:)
                Dcold2(i,j,:,:) = sigma_c5(i,j,1,:,:)
                Vcold2(i,j,:,:) = geovel_c5(i,j,1,:,:)
            end if
        end do
    end do

    print*, 'countwarm=', icountwarm
    print*, 'countcold=', icountcold
    call plots2(nnfile = 'garbage')
    ! call plots2(nnfile = 'Warm_ref_st4250',h = '(PT of st4 250m) > mean + SD',oopt = 'otops',y = -1.)
    ! call plots2(nnfile = 'Cold_ref_st4250',h = '(PT of st4 250m) < mean',oopt = 'otops',y = -1.)
    ! call header('mean PT='//trim(adjustl(real2str(Pmean,3)))//'  SD='//trim(adjustl(real2str(Psd,3)))//' at st4 250m',0.7,y = -1.)
    ! call plotsave('first')

    ! ! WARM
    ! iplotcount = 0
    ! do i = 1, 12
    !     do j = 1, 15
    !         if(all(Pwarm2(j,i,:,:)==0.) .eqv. .true.)cycle
    !         iplotcount = iplotcount + 1
    !         call symbolc(width/2.,0.8,0.6,monthnames(i)//';'//int2str(j+2008))
    !         if(iplotcount == 1.or.iplotcount == 12)then 
    !             call num_memori2(0.,400.,-height,50.,-90.,2,0.5,-1)
    !             call st_memori(1,6,width,1,0.5,2,y = -height)
    !         else 
    !             call memori(9,0.15,2,-height,-90.,y = -height/2.)
    !         end if
    !         call butler_psk(Swarm2(j,i,6:1:-1,:),width,-height,0.,33.95,34.3,0.05,'b2r',7,4,r = r ,g = g, b = b)
    !         call butler_cont(Pwarm2(j,i,6:1:-1,:),width,-height,-999.,0.,1.,thicc = 5)
    !         if(mod(iplotcount,11)==0)then 
    !             call plotback('first');call plot(0.,(-height-2.5),-3)
    !         else 
    !             call plot(width + 0.5,0.,-3)
    !         end if
    !         if(iplotcount == 16)then 
    !             call avsemdata_3D(reshape(Pwarm2,[15*12,9,400]),'dim1',mean_2D = Pmeanwarm)
    !             call avsemdata_3D(reshape(Swarm2,[15*12,9,400]),'dim1',mean_2D = Smeanwarm)
    !             call avsemdata_3D(reshape(Dwarm2,[15*12,9,400]),'dim1',mean_2D = Dmeanwarm)
    !             call avsemdata_3D(reshape(Vwarm2,[15*12,8,400]),'dim1',mean_2D = Vmeanwarm)
    !             open(123,file = '../Data/Composites_Binaries/PT_totmean_bifur.bin',form = 'unformatted',status = 'replace',access = 'direct',recl = 4*6*400,convert = 'big_endian')
    !             write(123,rec = 1) Pmeanwarm(:6,:)
    !             close(123)
    !             open(123,file = '../Data/Composites_Binaries/S_totmean_bifur.bin',form = 'unformatted',status = 'replace',access = 'direct',recl = 4*6*400,convert = 'big_endian')
    !             write(123,rec = 1) Smeanwarm(:6,:)
    !             close(123)
    !             open(123,file = '../Data/Composites_Binaries/D_totmean_bifur.bin',form = 'unformatted',status = 'replace',access = 'direct',recl = 4*6*400,convert = 'big_endian')
    !             write(123,rec = 1) Dmeanwarm(:6,:)
    !             close(123)
    !             open(123,file = '../Data/Composites_Binaries/V_totmean_bifur.bin',form = 'unformatted',status = 'replace',access = 'direct',recl = 4*5*400,convert = 'big_endian')
    !             write(123,rec = 1) Vmeanwarm(:5,:)
    !             close(123)
    !             call symbolc(width/2.,0.2,0.7,'tot mean')
    !             call memori(9,0.15,2,-height,-90.,y = -height/2.)
    !             call butler_psk(Smeanwarm(6:1:-1,:),width,-height,0.,33.95,34.3,0.05,'b2r',7,4,centralize = 4)
    !             call butler_cont(Pmeanwarm(6:1:-1,:),width,-height,-999.,0.,1.,thicc = 5)
    !             call colorscale(r,g,b,33.95,34.3,2,0.6,1,7.,0.3,lt = 1, gt = 1,symbol_start = 2,x = 7.5,y = -height/2.)
    !         end if

    !     end do
    ! end do
    ! ! WARM

    ! !  COLD 
    ! iplotcount = 0
    ! do i = 1, 12
    !     do j = 1, 15
    !         if(all(Pcold2(j,i,:,:)==0.) .eqv. .true.)cycle
    !         iplotcount = iplotcount + 1
    !         call symbolc(width/2.,0.8,0.6,monthnames(i)//';'//int2str(j+2008))
    !         if(mod(iplotcount-1,11)==0)then 
    !             call num_memori2(0.,400.,-height,50.,-90.,2,0.5,-1)
    !             call st_memori(1,6,width,1,0.5,2,y = -height)
    !         else 
    !             call memori(9,0.15,2,-height,-90.,y = -height/2.)
    !         end if
    !         call butler_psk(Scold2(j,i,6:1:-1,:),width,-height,0.,33.95,34.3,0.05,'b2r',7,4,r = r ,g = g, b = b)
    !         call butler_cont(Pcold2(j,i,6:1:-1,:),width,-height,-999.,0.,1.,thicc = 5)
    !         if(mod(iplotcount,33)==0)then 
    !             call newpage('first')
    !         else if(iplotcount == 22.or.iplotcount == 55.or.iplotcount == 88)then 
    !             call plotback('first');call plot(0.,2*(-height-2.),-3)
    !         elseif(iplotcount == 11 .or. iplotcount == 44 .or. iplotcount == 77)then 
    !             call plotback('first');call plot(0.,-height-2.,-3)
    !         else 
    !             call plot(width + 0.5,0.,-3)
    !         end if
    !         if(iplotcount == 94)then 
    !             call newpage('first',h = 'monthly means')
    !             do k = 1, 13
    !                 if(k /= 13)then 
    !                     call symbolc(width/2.,0.2,0.7,monthnames(k))
    !                     call avsemdata_3D(Pcold2(:,k,:,:),'dim1',mean_2D = Pmeancold) ! averageing by years
    !                     call avsemdata_3D(Scold2(:,k,:,:),'dim1',mean_2D = Smeancold)
    !                     call avsemdata_3D(Dcold2(:,k,:,:),'dim1',mean_2D = Dmeancold)
    !                     call avsemdata_3D(Vcold2(:,k,:,:),'dim1',mean_2D = Vmeancold)
    !                     ! print*,Vmeancold
    !                     call newpage('first',h = 'monthly means')
    !                     call butler_cont(Vmeancold(5:1:-1,:),width,-height,-999.,0.,2.,thicc = 5)
    !                     call plot(width + 1.,0.,-3)
    !                     call butler_cont(Dmeancold(6:1:-1,:),width,-height,-999.,20.,0.2,thicc = 5)
    !                     open(123,file = '../Data/Composites_Binaries/PT_'//monthnames2(k)//'_trapped.bin',form = 'unformatted',status = 'replace',access = 'direct',recl = 4*6*400,convert = 'big_endian')
    !                     write(123,rec = 1) Pmeancold(:6,:)
    !                     close(123)
    !                     open(123,file = '../Data/Composites_Binaries/S_'//monthnames2(k)//'_trapped.bin',form = 'unformatted',status = 'replace',access = 'direct',recl = 4*6*400,convert = 'big_endian')
    !                     write(123,rec = 1) Smeancold(:6,:)
    !                     close(123)
    !                     open(123,file = '../Data/Composites_Binaries/D_'//monthnames2(k)//'_trapped.bin',form = 'unformatted',status = 'replace',access = 'direct',recl = 4*6*400,convert = 'big_endian')
    !                     write(123,rec = 1) Dmeancold(:6,:)
    !                     close(123)
    !                     open(123,file = '../Data/Composites_Binaries/V_'//monthnames2(k)//'_trapped.bin',form = 'unformatted',status = 'replace',access = 'direct',recl = 4*5*400,convert = 'big_endian')
    !                     write(123,rec = 1) Vmeancold(:5,:)
    !                     close(123)
    !                 else
    !                     call symbolc(width/2.,0.2,0.7,'tot mean')
    !                     call avsemdata_3D(reshape(Pcold2,[15*12,9,400]),'dim1',mean_2D = Pmeancold)
    !                     call avsemdata_3D(reshape(Scold2,[15*12,9,400]),'dim1',mean_2D = Smeancold)
    !                     call avsemdata_3D(reshape(Dcold2,[15*12,9,400]),'dim1',mean_2D = Dmeancold)
    !                     call avsemdata_3D(reshape(Vcold2,[15*12,8,400]),'dim1',mean_2D = Vmeancold)
    !                     open(123,file = '../Data/Composites_Binaries/PT_totmean_trapped.bin',form = 'unformatted',status = 'replace',access = 'direct',recl = 4*6*400,convert = 'big_endian')
    !                     write(123,rec = 1) Pmeancold(:6,:)
    !                     close(123)
    !                     open(123,file = '../Data/Composites_Binaries/S_totmean_trapped.bin',form = 'unformatted',status = 'replace',access = 'direct',recl = 4*6*400,convert = 'big_endian')
    !                     write(123,rec = 1) Smeancold(:6,:)
    !                     close(123)
    !                     open(123,file = '../Data/Composites_Binaries/D_totmean_trapped.bin',form = 'unformatted',status = 'replace',access = 'direct',recl = 4*6*400,convert = 'big_endian')
    !                     write(123,rec = 1) Dmeancold(:6,:)
    !                     close(123)
    !                     open(123,file = '../Data/Composites_Binaries/V_totmean_trapped.bin',form = 'unformatted',status = 'replace',access = 'direct',recl = 4*5*400,convert = 'big_endian')
    !                     write(123,rec = 1) Vmeancold(:5,:)
    !                     close(123)
    !                 end if
    !                 if(i == 1.or.i == 11)call num_memori2(0.,400.,-height,50.,-90.,2,0.5,-1)
    !                 call memori(9,0.15,2,-height,-90.,y = -height/2.)
    !                 call butler_psk(Smeancold(6:1:-1,:),width,-height,0.,33.95,34.3,0.05,'b2r',7,4,centralize = 4)
    !                 call butler_cont(Pmeancold(6:1:-1,:),width,-height,-999.,0.,1.,thicc = 5)
    !                 if(k==13)call colorscale(r,g,b,33.95,34.3,2,0.6,1,7.,0.3,lt = 1, gt = 1,symbol_start = 2,x = 7.5,y = -height/2.)
    !                 if(k == 11)then 
    !                     call plotback('first');call plot(0.,(-height-2.),-3)
    !                 else 
    !                     call plot(width + 0.5,0.,-3)
    !                 end if
    !             end do
    !         end if
    !     end do
    ! end do
    ! ! COLD


    call plote

end program

! program test
!     use always
!     real, dimension(6, 400) :: pt,s,den
!     real, dimension(5, 400) :: v

!     call plots2(psfile = '../Data/Composites_Binaries/conf_bifur.ps', oopt = 'otops', y = -1.)
!     call plotsave('first')
!     width = 2.; height = 4.
!     do i = 1, 12
!         open(123,file = '../Data/Composites_Binaries/PT_totmean_bifur.bin', &
!             form = 'unformatted', status = 'old', access = 'direct', recl = 4*6*400, convert = 'big_endian')
!         read(123, rec = 1) pt
!         close(123)

!         open(123,file = '../Data/Composites_Binaries/S_totmean_bifur.bin', &
!             form = 'unformatted', status = 'old', access = 'direct', recl = 4*6*400, convert = 'big_endian')
!         read(123, rec = 1) s
!         close(123)

        
!         call symbolc(width/2.,0.2,0.6,monthnames(i))
!         call butler_psk(s(6:1:-1,:),width,-height,0.,33.95,34.3,0.05,'b2r',7,4)
!         call butler_cont(pt(6:1:-1,:),width,-height,-999.,0.,1.,thicc = 5)
!         call plot(width+0.3,0.,-3)
!     end do 
        
!     call plotback('first');call plot(0.,-height-1.,-3)
!         do i = 1, 12
!         open(123,file = '../Data/Composites_Binaries/D_totmean_bifur.bin', &
!             form = 'unformatted', status = 'old', access = 'direct', recl = 4*6*400, convert = 'big_endian')
!         read(123, rec = 1) den
!         close(123)

!         open(123,file = '../Data/Composites_Binaries/V_totmean_bifur.bin', &
!             form = 'unformatted', status = 'old', access = 'direct', recl = 4*5*400, convert = 'big_endian')
!         read(123, rec = 1) v
!         close(123)

        
!         call symbolc(width/2.,0.2,0.6,monthnames(i))
!         call butler_cont(den(6:1:-1,:),width,-height,0.,20.,0.2,thicc = 5)
!         call plot(0.,-height-1.,-3)
!         call butler_cont(v(5:1:-1,:),width,-height,-999.,0.,0.02,thicc = 5)
!         call plot(0.,height+1.,-3)
!         call plot(width+0.3,0.,-3)
!     end do 

!     call plote
    
! end program
