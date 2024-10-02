program moduletest 
    use constants
    use subroutines
    implicit none
    real,dimension(months,lines,stations,depth)::av,sem
    integer,dimension(months,lines,stations,depth)::quan
    real,dimension(:,:,:,:),allocatable::mean
    integer,dimension(:,:,:,:),allocatable::dat
    call calibrated_data51(potemp_c5,sal_c5)
    call plots(5.,15.,9,'test2.ps')
    call gmark(0.,0.,0.1,1)
    ! call avsem_dataquan(potemp_c5,av,sem,quan)
    ! call avsemdata_2D(potemp_c5(1:years,1:months,1,6,201),years,months,years,mean_1D=mean,s_1D=s,sem_1D=sem,dataquan_1D=dat)
    ! call avsemdata_3D(potemp_c5(1:years,1:months,1,9,1:100),years,months,100,years,mean_2D=mean)
    ! call avsemdata_4D(potemp_c5(1:years,1:months,1,1:9,1:100),years,months,stations,100,years,mean_3D=mean)
    call avsemdata_5D(potemp_c5,years,months,lines,stations,depth,years,mean_4D=mean)
    !   print*, mean,sem,s,dat
    ! do d = 1,100
    !     print*,mean(8,1,9,d),av(8,1,9,d)
    ! end do
    call create_box(4.,-10.,3)
    call butler_psbet(mean(10,1,4:9,1:400),6,400,4.,-10.,0.,0.,26.,1.,'b2r',26,8,contquan = 10,conti = 0.,continc = 1.)
    call st_memori2(0.,-10.,1,6,4.,1,0.3,0)
    call plote
    ! deallocate(mean,s,sem,dat)
  end program
  
  
