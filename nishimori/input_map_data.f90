program japan
    implicit none
    !変数,配列指定,(注意点:実数,整数,配列個数)
    integer::i,j
    integer,parameter::imax=2080 , jmax= 2640
    real,dimension(imax,jmax)::dep
    integer,dimension(imax,jmax)::dep_m

!データ入力 binファイルは開けるとき忠実に真似←binを作成したときと同じ形式でなければ開けれない。
open(21,file='japan1km122-148_24-46.bin',form='unformatted',status='old')
do j=jmax, 1,-1
    read(21) (dep(i,j),i=1,imax)
    dep(i,j)=-dep(i,j)  !深度がプラスの値
end do
close(21)

!経度は122度から148度まで，緯度は24度から46度までの海底地形のデータ
!経度は1度あたり80個のデータ、緯度は1度あたり120個のデータがあります。







end program
