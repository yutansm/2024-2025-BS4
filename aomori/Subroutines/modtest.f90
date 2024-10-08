program moduletest 
  use constants
  use subroutines
    implicit none
    call plots(4.,13.,9,"test.ps")
    call b2r_colorgrad(32,16,r,g,b)
    call colorscale(32,r,g,b,0.,16.,10,0.5,2,7.,0.3,lessthan=1,morethan=1,rangle=90.,symbol_TorB='T')

    call plote
  end program
  
  

