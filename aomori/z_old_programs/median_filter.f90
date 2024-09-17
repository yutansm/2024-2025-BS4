program create_median_filter
    implicit none
    integer,parameter::n = 11 !total number of numbers
    real,dimension(n)::r
    ! double precision::d
    integer:: i,j,x1,x2,initial_num,x
    real::numvault,sum,med
    real,dimension(5)::a


    !まずはランダムないくつかの数字を小さいものから順に並べなおすプログラムを作成

call random_number(r)
write(*,*) 'The numbers are',r 

do i = 1,n-1
    do j = i+1,n 
        if(r(i) > r(j)) then
            numvault = r(i)  !numvault に一旦数字をいれる
            r(i) = r(j)  !r(i)の場所のr(j)をいれる
            r(j) = numvault  !numvaultに入った数字をr(j)に入れる,これでr(i)とr(j)はいれかわった
        else
        end if
    end do
end do
    !ランダムなｎ個の数字を小さいものから順に並べるプログラム作成完了
write(*,*) 'The rearranged numbers are',r

if (mod(n,2)==0) then 
    x1 = n/2
    x2 = n/2+1
    sum = r(x1)+r(x2)
    write(*,*)'The median number is',sum/2.
else
    x1 = (n+1)/2
    sum = r(x1)
    write(*,*)'The median number is',sum
end if                               
    !並び変えて中央値も得ることが出来た。

a(1) = 0.
a(2) = 0.
a(3) = 0.
a(4) = 3.
a(5) = 4.

do x = 1,5
    if (a(x)/=0.) then
        initial_num = x
        exit
    else
    end if
end do

print*, initial_num

!上のやつできてるよかった

                 

end program
