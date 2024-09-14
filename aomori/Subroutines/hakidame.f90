program whyisntplotsworking
    implicit none
    integer,parameter::years = 15, months = 12
    real,parameter::length = 26., height = 10.
    ! real,dimension(years,months)::f,t,devf,devt,array
    ! real,dimension(years)::ylymeanf,ylymeant
    ! integer,dimension(months)::dataf,datat
    ! real,dimension(months)::mlymeanf,mlymeant,semf,semt,dot_y
    ! integer::y,n,count,m
    ! real::dx,summation,mean_excluding_zeros
    ! call calibrated_fukauraSSH(f);call calibrated_tappiSSH(t)

    call plots(.5,4.,13,'hakidamebromanimcrying.ps')
    call symbol(6.,14.,0.6,('Time Series of SSH at Fukaura (yearly mean)'),0.,len('time series of ssh at fukaura (yearly mean)'))
    call num_memori(-200.,200.,8,1,0.4,-1,height,-90,0,0)
    call month_memori(0.5,length)
    ! call calibrated_fukauraSSH(f);call calibrated_tappiSSH(t)
    ! ! print*,f
    ! !getting mean of each year
    ! do n = 1,2
    !     if (n==1) then
    !         array= f
    !     else; array= t
    !     end if
    !     do y = 1, years
    !         summation = 0.0 
    !         count = 0
    !         do m = 1, months
    !             if (array(y, m) /= 0.0) then
    !                 summation = summation + array(y, m)
    !                 count = count + 1
    !             end if
    !         end do ! added all months of a year
    !         if (count > 0) then
    !             mean_excluding_zeros = summation/real(count)
    !         else
    !             mean_excluding_zeros = 0.0
    !         end if
    !         if(n==1) then
    !             ylymeanf(y) = mean_excluding_zeros
    !         else;ylymeant(y) = mean_excluding_zeros
    !         end if
    !     end do
    ! end do
    ! print*,ylymeanf
    call plote

end program