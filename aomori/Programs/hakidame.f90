! program create_files
!     use always
!     implicit none
!     real,dimension(:,:,:,:),allocatable::avpotemp,avsal
!     real, dimension(-1:22, 20) :: OBS_PT=0.,OBS_S=0.;OBS_V=0.            ! x and z dimensions
!     integer ::  unit_number, ios, recl,x,z,gap,iterations,position
!     real::incT(20),incS(20)

!     call calibrated_data51(potemp_c5,sal_c5) ! 15*12*2*9*400
!     call geovel_array(51,geovel_5)
!     call avsemdata_5D(potemp_c5,'dim1',mean_4D = avpotemp)
!     call avsemdata_5D(sal_c5,'dim1',mean_4D = avsal)
!     call avsemdata_5D(geovel_5,'dim1',mean_4D = avgeovel)
!     print*,avsal(9,1,4:9,400)

!     do i = 1,6 ! x and j are for model array, st and z are for CTD array
!         do j = 1, 20
!             if(i/=6)then
!                 x = (i-1)*4+1
!             else; x = 20
!             end if
!             z = 20*j
!             st = i + 3 
!             OBS_PT(x,j) = avpotemp(9,1,st,z)
!             OBS_S(x,j) = avsal(9,1,st,z)
!         end do
!     end do

    
    
!     ! put the CTD data intermittently into the model array


!     ! put the st6 and st1 data into the overlapping grid points of the model array
!     OBS_PT(-1,1:20) = OBS_PT(1,1:20);OBS_PT(0,1:20) = OBS_PT(1,1:20)
!     OBS_PT(21,1:20) = OBS_PT(20,1:20);OBS_PT(22,1:20) = OBS_PT(20,1:20)
!     OBS_S(-1,1:20) = OBS_S(1,1:20);OBS_S(0,1:20) = OBS_S(1,1:20)
!     OBS_S(21,1:20) = OBS_S(20,1:20);OBS_S(22,1:20) = OBS_S(20,1:20)
!     ! linearly interpolate the data between the CTD data points
!     position = 1
!     do i = 1,5
!         if(i/=5)then 
!             gap = 4;iterations = 3
!         else;gap = 3;iterations = 2
!         end if

!         do j = 1, iterations
!             incT = (OBS_PT(position+gap,1:20) - OBS_PT(position,1:20))/real(gap) ! tis an array of diffs
!             incS = (OBS_S(position+gap,1:20) - OBS_S(position,1:20))/real(gap) ! tis an array of diffs
!             print*,incS(20)
!             OBS_PT(position+j,1:20) = OBS_PT(position,1:20) + incT*j
!             OBS_S(position+j,1:20) = OBS_S(position,1:20) + incS*j
!         end do
!         position = position + gap
!     end do

!     ! Create the CSV file for viewing
!     open(unit=20, file="../MITgcm/verification/yuta's_first_model/input/OBS_T2.csv", status='replace')
!     do j = 1, 20
!         write(20, '(24(f9.4))') (OBS_PT(i, j), i=-1, 22)
!     end do
!     close(20)
!     open(unit=21, file="../MITgcm/verification/yuta's_first_model/input/OBS_S2.csv", status='replace')
!     do j = 1, 20
!         write(21, '(24(f9.4))') (OBS_S(i, j), i=-1, 22)
!     end do
!     close(21)

!     ! Create the binary file
!     open(unit_number, file="../MITgcm/verification/yuta's_first_model/input/OBS_T2.bin", form='unformatted', status='replace', access='direct', recl=4*24*20, convert='big_endian')
!     write(unit_number,rec=1) OBS_PT
!     close(unit_number)

!     open(unit_number, file="../MITgcm/verification/yuta's_first_model/input/OBS_S2.bin", form='unformatted', status='replace', access='direct', recl=4*24*20, convert='big_endian')
!     write(unit_number,rec=1) OBS_S
!     close(unit_number)

! end program create_files
! Program OBS_Vel
!     use always
!     implicit none 
!     real, dimension(-1:22, 20) :: OBS_V=0.            ! x and z dimensions
!     real, dimension(:,:,:,:),allocatable::avgeovel
!     integer ::  unit_number, ios, recl,x,z,gap,iterations,position
!     real::incV(20)

!     call geovel_array(51,geovel_5)
!     call avsemdata_5D(geovel_5,'dim1',mean_4D = avgeovel)

!     do i = 1,5 ! x and j are for model array, st and z are for CTD array
!         do j = 1, 20
!             if(i/=5)then
!                 x = (i-1)*5+1
!             else; x = 20
!             end if
!             z = 20*j
!             st = i + 4
!             OBS_V(x,j) = avgeovel(9,1,st,z)
!         end do
!     end do
!     ! put the CTD data intermittently into the model array

!     ! put the st6 and st1 data into the overlapping grid points of the model array
!     OBS_V(-1,1:20) = OBS_V(1,1:20);OBS_V(0,1:20) = OBS_V(1,1:20)
!     OBS_V(21,1:20) = OBS_V(20,1:20);OBS_V(22,1:20) = OBS_V(20,1:20)
!     ! linearly interpolate the data between the CTD data points
!     position = 1
!     do i = 1,4
!         if(i/=5)then 
!             gap = 5;iterations = 4
!         else;gap = 4;iterations = 3
!         end if

!         do j = 1, iterations
!             incV = (OBS_V(position+gap,1:20) - OBS_V(position,1:20))/real(gap) ! tis an array of diffs
!             ! print*,incV(20)
!             OBS_V(position+j,1:20) = OBS_V(position,1:20) + incV*j
!         end do
!         position = position + gap
!     end do

!     ! Create the CSV file for viewing
!     open(unit=20, file="../MITgcm/verification/yuta's_first_model/input/OBS_V2.csv", status='replace')
!     do j = 1, 20
!         write(20, '(24(f9.4))') (OBS_V(i, j), i=-1, 22)
!     end do
!     close(20)
!     ! Create the binary file
!     unit_number = 22
!     open(unit_number, file="../MITgcm/verification/yuta's_first_model/input/OBS_V2.bin", form='unformatted', status='replace', access='direct', recl=4*24*20, convert='big_endian')
!     write(unit_number,rec=1) OBS_V
!     close(unit_number)


! end program
! program create_files
!     implicit none
!     real,dimension(40,80,20) :: iniT,iniS ! x and z dimensions 
!     integer :: i, j, unit_number, ios

!     ! Initialize the array
!     iniT = 0.
!     iniS = 34.

!     ! Create the binary file
!     unit_number = 10
!     open(unit_number, file="../MITgcm/verification/yuta's_first_model/input3/initial_T.bin", form='unformatted', status='replace', access='direct', recl=4*40*80*20, convert='big_endian')
!     write(unit_number,rec=1) iniT
!     close(unit_number)
!     open(unit_number, file="../MITgcm/verification/yuta's_first_model/input3/initial_S.bin", form='unformatted', status='replace', access='direct', recl=4*40*80*20, convert='big_endian')
!     write(unit_number,rec=1) iniS
!     close(unit_number)

!     ! Create the CSV file
!     ! open(unit=20, file="../MITgcm/verification/yuta's_first_model/input/initial_T.csv", status='replace')
!     !     do j = 1,20
!     !         write(20,'(22(f6.1))') (data_array(i,j), i=0,21)
!     !     end do
!     ! close(20)

!     ! print *, 'Binary file and CSV file created successfully.'

! end program create_files
! program bath
!     real, dimension(40,80) :: bathy

!     bathy = -400.
!     bathy(40,:) = 0.
!     open(23,file = "../MITgcm/verification/yuta's_first_model/input3/bathy.bin", form = 'unformatted', status = 'replace', access = 'direct',recl = 4*40*80,convert = 'big_endian')
!     write(23,rec=1) bathy
!     close(23)


! end program
! program read_files
!     use always
!     real,dimension(62,62)::data_array

!     open(23,file = "../MITgcm/verification/yuta's_first_model/input/bathy.bin", form = 'unformatted', status = 'old', access = 'direct',recl = 4*62*62,convert = 'big_endian')
!     read(23,rec=1) data_array
!     close(23)
!     call openlog(basename='bathy.log')
!     write(tolog,*)data_array
!     call closelog
! end program
program read_files
    use always
    dimension::data_array(40,80,20)
    open(23,file = "../MITgcm/verification/yuta's_first_model/input3/initial_T.bin", form = 'unformatted', status = 'old', access = 'direct',recl = 4*40*80*20,convert = 'big_endian')
    read(23,rec=1) data_array
    close(23)
    ! call openlog(basename='initial_T.log')
    ! write(tolog,*)data_array
    ! call closelog

    print*,size(data_array)
end program


! INTEGER i, rank, ierr

! real,dimension(1-OLx:Nx+OLx,Nr)::OBS_T

! include 'mpif.h'

! IF (useOrlanskiSouth) THEN

!     CALL ORLANSKI_SOUTH(
!  &          bi, bj, futureTime,
!  &          uVel, vVel, wVel, theta, salt,
!  &          myThid )

!   ELSE
!         open(59,file = '../input/OBS_T.bin',form='unformatted',
!  &           status='old',access = 'direct',recl=4*24*20)
!         read(59,rec=1) OBS_T
!         call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
!         if(rank == 0)then
!               OBSt(1-OLx:sNx+OLx,1:Nr,bi,bj) = OBS_T
!         else
!         DO k=1,Nr
!         DO i=1-OLx,sNx+OLx
!               IF ( OB_Js(i,bi,bj).NE.OB_indexNone ) THEN
!               OBSu(i,k,bi,bj)=0.
!               OBSv(i,k,bi,bj)=0.
!               OBSt(i,k,bi,bj)=tRef(k)
!               OBSs(i,k,bi,bj)=sRef(k)

!               OBSw(i,k,bi,bj)=0.

!               ENDIF
!         ENDDO
!         ENDDO
!         end if
!   END IF
