program vertical
    implicit none
integer::i,k
integer,parameter::imax=5,kmax=1500
character::filename*100
real,dimension(imax,kmax)::sal,potemp

        102 format(15(f9.4))
!観測データsal,tem
    filename='       51potemp40.50.csv'
    open(31,file=filename,status='old',action='read')
    do k=1,kmax
        read(31,102) (potemp(i,k),i=1,imax)
    end do
    close(31)
    filename='       51sal40.50.csv'
    open(31,file=filename,status='old',action='read')
    do k=1,kmax
        read(31,102) (sal(i,k),i=1,imax)
    end do
    close(31)

!断面図を作りましょう

end program
