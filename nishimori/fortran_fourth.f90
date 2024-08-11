program fourth 
    implicit none
integer::i,k,n
real::r,g,b
integer,parameter::imax=5,kmax=1500
character::filename*100
integer,parameter::is=1, ks=1
real,dimension(imax,kmax)::sal,potemp
integer,dimension(imax,kmax)::mask
real,parameter::xlength=5.,ylength=15.
real,parameter::dx=xlength/real(imax),dy=-ylength/real(kmax)



        102 format(15(f9.4))
!観測データsal,tem
    filename='51potemp40.50.csv'
    open(31,file=filename,status='old',action='read')
    do k=1,kmax
        read(31,102) (potemp(i,k),i=1,imax)
    end do
    close(31)
    filename='51sal40.50.csv'
    open(31,file=filename,status='old',action='read')
    do k=1,kmax
        read(31,102) (sal(i,k),i=1,imax)
    end do
    close(31)

!断面図を作りましょう

call plots(10.,10.,13,'fourth_2.ps')    
! call factor(1.25)
call newpen2(3)

do i=1,imax
    do k=1,kmax
        if(sal(i,k)<30.) then
            mask(i,k)=0
        else
            mask(i,k)=1
        end if 
    end do
end do

do n = 3370,3430,1

    r = real(n-3370)/60.
    g = 0.
    b= 1.-real(n-3370)/60.

    call pscolorK(dx,dy,sal,mask,is,3,ks,1000,imax,kmax,real(n)/100.,real(n+1)/100.,r,g,b)
end do


call newpen2(4)
call rgbk(0.,0.,0.)
call pscont3(dx,dy,sal,mask,is,imax,ks,kmax,imax,kmax,7,33.7,1./6.)


call plote




end program