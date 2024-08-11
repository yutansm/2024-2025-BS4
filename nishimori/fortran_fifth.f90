program pin_hakodate
    implicit none
    intrinsic sin,cos,tan,asin,acos
    integer::i,j,is,ie,js,je,initial_lat, final_lat, initial_long, final_long
    real::dx,dy,xlength,ylength,ratio,pi
    integer,parameter::imax=2080, jmax=2640
    real,dimension(imax,jmax)::dep
    integer,dimension(imax,jmax)::dep_m
    ! 24-46N,122-148E 
    ! 80 data per longitude(i), 120 data per latitude(j)
    ! equatorial radius of the Earth is 6378km Re
    ! polar radius of the Earth is 6357km  Rp
    ! Rp:Re=6378sin/6357cos
    ! integer::initial_lat, final_lat, initial_long, final_long
    ! integer,parameter::is=(int(initial_lat)-122)*80+1, ie=(int(final_lat)-122)*80, js=(int(initial_long)-40)*120+1, je=(int(final_long)-40)*120
    ! real,parameter::pi=2.*asin(1.)
    ! real,parameter::ratio=6357./6378.*tan(int(initial_lat)+(int(final_lat)-int(initial_lat))/2.)*pi/180.
    ! real,parameter::xlength=22./2., ylength=26./2.*int(ratio)
    ! real,parameter::dx=xlength/real(ie-is), dy=ylength/real(je-js)



    
open(21,file='japan1km122-148_24-46.bin',form='unformatted',status='old')
do j=jmax,1,-1
    read(21)(dep(i,j),i=1,imax)
    dep(i,j)=-dep(i,j)    
end do
close(21)

call plots(5.,5.,13,'Map_of_Japan.ps')

do i=1,imax
    do j=1,jmax
        dep_m(i,j)=1
    end do
end do

write(*,*)'Enter initial latitude (24 to 46):'
read(*,*)initial_lat
write(*,*)'Enter final latitude (24 to 46):'
read(*,*)final_lat
write(*,*)'Enter initial longitude (122 to 148):'
read(*,*)initial_long
write(*,*)'Enter final longitude (122 to 148):'
read(*,*)final_long


is=(int(initial_long)-122)*80+1
ie=(int(final_long-122))*80
js=(int(initial_lat)-24)*120+1
je=(int(final_lat)-24)*120
pi=2.*asin(1.)
ratio=6357./6378.*1./cos((initial_lat + final_lat)/2.*pi/180.)*22./26.
xlength=10.
ylength=real(xlength)*real(ratio)
dx=xlength/real(ie-is);dy=ylength/real(je-js)

write(*,*)is,ie,js,je,dx,dy


call newpen2(4)
call rgbk(0.,0.,0.)
call pscont3(dx,dy,dep,dep_m,is,ie,js,je,imax,jmax,1,0.,10)

if (final_lat<41 .or. final_long<140) then
    ! write(*,*)final_lat, final_long
    call symbol(4.0,10.0,1.0,'Hakodate is out of sight',0.0,20)
else
    call symbolc(xlength*(140.7288-real(initial_long))/(real(final_long)-real(initial_long)),ylength*(41.7687-real(initial_lat))/(real(final_lat)-real(initial_lat)),1.,'Here!',0.,5)
end if





call plote

end program
