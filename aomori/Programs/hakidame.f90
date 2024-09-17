! program ps2pdfcommandbro
!     implicit none
!     integer,parameter::years = 15, months = 12, lines = 2, stations = 9, depth = 400
!     real,parameter::length=5.,height=10.
!     real,dimension(years,months,lines,stations,1000)::potemp_5=0.,sal_5=0.
!     real,dimension(years,months,lines,stations,depth)::cpotemp_5=0.,csal_5=0.,geovel
    
!     ! call potempsal_51(potemp_5,sal_5)
!     call calibrated_data51(cpotemp_5,csal_5)
!     ! call geovel_array(51,geovel)
!     call plots(5.,5.,13,'../test1.ps')
!     call create_box(5.,10.,3)
!     call betsqk(0.,0.,length,height,0.5,1.,0.5)
    
    
!     ! print*,sal_5(1,12,2,5,1:depth)
!     ! call calibrated_data51(cpotemp_5,csal_5)
!     ! print*,csal_5(8,8,1,1:stations,100:200)
!     ! print*,geovel(1,8,1,7,1:depth)

!     call plote
! end program

program ps2pdfcommandbro
    implicit none
    integer,parameter::years = 15, months = 12, lines = 2, stations = 9, depth = 400
    real,parameter::length=5.,height=10.
    ! real,dimension(years,months,lines,stations,1000)::potemp_5=0.,sal_5=0.
    real,dimension(years,months,lines,stations,depth)::cpotemp_5,csal_5,geovel
    call calibrated_data51(cpotemp_5,csal_5)

    call plots(5.,5.,13,'../test1.ps')
    ! call create_box(5.,10.,3)
    ! call betsqk(0.,0.,length,height,0.5,1.,0.5)
    ! call calibrated_data51(cpotemp_5,csal_5)
    call plote
end program