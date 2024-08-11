program sample
    implicit none
    integer,parameter::imax=3,jmax=10
    integer,parameter::is=1,ie=imax,js=1,je=jmax
    integer::i,j,n
    real,dimension(imax,jmax)::box
    integer,dimension(imax,jmax)::mask
    real,parameter::dx=1.,dy=0.5
    real,parameter::xl=dx*imax,yl=dy*jmax


!output.txtに出力される
print *, 'abc'

call plots(5.,5.,13,'sample.ps')
!絵を描くときに全体を拡大縮小
call factor(1.25)

do n=1,2

    !線の太さ
    call newpen2(2)

    do i=1,2
        !線を引く  3が始点それ以降2
        call plot(0.,0.,3);call plot(2.,2.,2);call plot(4.,0.,2)

        !原点をずらす　-3
        call plot(0.,4.,-3)
    end do
    !またずらして
    call plot(5.,-8.,-3)
    !配列に適当に値を入れてみる
    do j=1,jmax
        box(1,j)=real(j)
        box(2,j)=real(j)*3.
        box(3,j)=real(j)*2.
    end do

    !マスク 隠すところを0,色を付けるところを1に設定す
    do i=1,imax
        do j=1,jmax
        if (j==3) then
            mask(i,j)=0
        else
            mask(i,j)=1
        end if
        end do
    end do

    !カラースケール例           
    call pscolorK(dx,dy,box,mask,is,ie,js,je,imax,jmax,1.,10.,1.,0.5,0.5)
    call pscolorK(dx,dy,box,mask,is,ie,js,je,imax,jmax,10.,20.,1.,0.25,0.25)
    call pscolorK(dx,dy,box,mask,is,ie,js,je,imax,jmax,20.,30.,1.,0.,0.)
    !カラースケール例
    call newpen2(2);call rgbk(0.,0.,0.)
    call pscont3(dx,dy,box,mask,is,ie,js,je,imax,jmax,15,2.,2.)
    call newpen2(3);call rgbk(0.75,0.75,0.75)
    call pscont3(dx,dy,box,mask,is,ie,js,je,imax,jmax,3,10.,10.)

    call newpen2(3)
    call rgbk(0.,0.,0.)
    call plot(0.,0.,3);call plot(xl,0.,2);call plot(xl,yl,2);call plot(0.,yl,2);call plot(0.,0.,2)

    !文字を書く
    call symbol(xl/2.,-1.,0.5,'xl',0.,2)
    !数字を書く
    do j=1,jmax
        call number(xl+0.5,yl*(real(j)/real(jmax)),0.5,real(j),0.,-1)
    end do

    ! call gmark(0.,0.,1.,1);call gmark(xl,0.,0.5,4);call gmark(xl,yl,0.75,8);call gmark(0.,yl,1.5,3)

    call plot(xl+1.,0.,-3)

    call betsqk(0.,0.,1.,1.,0.,1.,0.)

    call newpage

end do ! n　

call plote

end program