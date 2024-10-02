program ongongo
  implicit none
!   call plots(5.,5.,13,'../Plots/TempSalSigma/asdf.ps')
!   call floating_lines(0.,0.,5.,45.,5,3,1.,0.)
!   call symbolr(0.,0.,0.5,'St',0.,len('St'))
  integer::values(8)
  call date_and_time(values=values)
    print*,values  
end program

