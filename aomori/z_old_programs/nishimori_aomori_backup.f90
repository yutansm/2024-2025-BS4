program aomori_data
    ! implicit none

    integer,parameter::month=12,year_start=2008,year_max=15,station_y=2,station_x=9,depth_max=1000 !station_xは深層見れない時7
    !line_num=緯度別の観測、st_x=経度別の観測列、年、月、深さ、塩分または水温
    real,dimension(year_max,month,station_y,station_x,depth_max)::potemp_5,sal_5
    ! real,dimension(station_y,station_x,depth_max)::initial_tempsum
    real,dimension(year_max,month,station_y,station_x)::lon
    character::aa*9,filename*999,line_name*20,moon*9
    character(len=*), parameter::pass_CTD = 'edited_data'
    integer::i,j,k,y,m,line_num,a,b,c,year,mon

    real,dimension(month,station_y,station_x,depth_max)::av_array
    real,dimension(station_x,depth_max)::useful_array
    integer,dimension(station_x,depth_max)::mask
    real,parameter::xlength=1.5, ylength=3.
    real,parameter::dx = xlength/real(station_x), dy = -ylength/real(depth_max)
    integer::is=1,js=1,n,q,x,memori,num = 0
    real::calc_sum,first_sum,calc_av,g,r,blue,xpoints,ypoints,rosso,verde,azul
    


!情報解析演習にも載ってる、出力する値の書かれ方
!この場合横に9列、〇〇〇〇.〇〇〇〇
!            全体で9マスうち4桁小数を書く
102 format(9(f9.4))
!観測data
    ! write(meter,'(i4.4)') depth_max
    do line_num=1,station_y
        if (line_num==1) then
            line_name='S-line'
        else if (line_num==2) then
            line_name='N-line'
        else;end if  !参照するｃｓｖファイルを決定するために名前を付ける
    do m=1,month
        !↓下の分の操作　　例)m=5のintegerをmoon='5'の文字列に変換する
        write(moon,'(i2.2)') m  !moonに月の数字を代入していく
    do y=1,year_max
        !↓下の分の操作　　例)year=2020のintegerをaa='2020'の文字列に変換する
        year=y+year_start;write(aa,'(i4.4)') year
        !//は文字列の結合を意味する。  ここでintegerの変数のまま結合できないので上の行で文字列化させている。
        filename=trim(pass_CTD)//'/'//trim(line_name)//'/'//trim(moon)//'/01'//'potemp'//trim(aa)//'.csv'  !01でなく51ではないかkaiketsu
        open(31,file=filename,status='old',action='read')
        do k=1,depth_max
            read(31,102) (potemp_5(y,m,line_num,i,k),i=1,station_x)!??
        end do
        close(31)
        filename=trim(pass_CTD)//'/'//trim(line_name)//'/'//trim(moon)//'/01'//'sal'//trim(aa)//'.csv'
        open(31,file=filename,status='old',action='read')
        do k=1,depth_max
            read(31,102) (sal_5(y,m,line_num,i,k),i=1,station_x)
        end do
        close(31)
        filename=trim(pass_CTD)//'/'//trim(line_name)//'/lon.csv'
        open(31,file=filename,status='old',action='read')
            read(31,102) (lon(y,m,line_num,i),i=1,station_x) 
        close(31)

    end do
    end do;end do
    !potemp5とsal5は，ある年月の，北か南のラインでの９測定位置分のデータが入った膨大な５次元配列
    !各月平均を知りたい。全ての年の同じ月におけるデータの平均を取って新たな配列avpotemp_5にでもいれてみる。
    call plots(0.,5.,13,'MonthlyAverage_Temp.ps')
    call newpen2(3)

    do m = 1,month
        do line_num = 1,2
            do x = 1, station_x
                do  k = 1, depth_max
                    do y = 1, year_max
                        if (potemp_5(y,m,line_num,x,k) /= 0.) then
                            calc_sum = first_sum + potemp_5(y,m,line_num,x,k) !→ 1 datam
                            first_sum = calc_sum
                            num = num+1
                        else 
                            first_sum = first_sum
                            num = num
                        end if
                        ! calc_av = calc_sum/real(num)
                    end do !End of y years
                    if (num /= 0) then
                        calc_av = calc_sum/real(num)  !real number yearly av !データが無い部分に注意
                        av_array(m,line_num,x,k) = calc_av
                        useful_array(x,k) = av_array(m,line_num,x,k)  
                        mask(x,k) = 1
                                    
                    else
                        useful_array(x,k) = 0.
                        mask(x,k) = 0
                    end if 
                    if(k==100) then
                        call numberc(real(x)*dx,0.05,.1,real(num),0.,-1) !何年分のデータがあるかをdepth=100を基準に判断した
                    else
                    end if                 
                    num = 0
                    first_sum = 0  !原点ずらしてデータの個数をｘ毎に書く
                end do !End of k depth
              end do !End of x stations
              
            do n=1,31,1  !最初は，１月,S_line の鉛直図の作成 
                if (n<=11) then
                    r = (real(n)-1)/10.
                    g = (real(n)-1)/10.
                    blue = 1.
                    call pscolorK(dx,dy,useful_array,mask,is,station_x,js,depth_max,station_x,depth_max,real(n),real(n+1),r,g,blue)
                else
                    r = 1.
                    g = 1.-(real(n)-11.)/20.
                    blue = 1.-(real(n)-11.)/20.
                    call pscolorK(dx,dy,useful_array,mask,is,station_x,js,depth_max,station_x,depth_max,real(n),real(n+1),r,g,blue)
                end if
                call plot(0.,0.,3)  !枠作り
                call plot(0.,0.-ylength,2)
                call plot(dx*9.,0.-ylength,2)
                call plot(dx*9.,0.,2)
                call plot(0.,0.,2)
                
                do xpoints = 1,9 !目盛
                    call plot(dx*(real(xpoints)),-ylength,3) !-1で原点スタート
                    call plot(dx*(real(xpoints)),-ylength-0.05,2) !-1で原点スタート
                    call numberc(dx*(real(xpoints)),-ylength-0.2,0.15,real(xpoints),0.,-1)
                end do

                do ypoints = 0,10  !目盛
                    call plot(0.,dy*100.*(real(ypoints)),3)
                    call plot(-0.05,dy*100*(real(ypoints)),2)
                    call numberc(-0.15,dy*100.*real((ypoints)),0.1,real((ypoints)*100.),0.,-1)
                end do                   
            end do  !鉛直図完成
            call numberc(xlength/2.,0.3,0.4,real(m),0.,-1) !数字を月に対応させることを試みる
            if(line_num==1 .and. m==6) then
                call symbolc(dx*12,1.,0.6,'S-line',0.,6)
                call plot (0.,8.,-3)
            else if (line_num==2 .and. m==6) then
                call symbolc(dx*12,1.,0.6,'N-line',0.,6)     
                call plot(dx*12.,-8.,-3)
            else if(line_num==1) then
                call plot(0.,8.,-3)
            else
                call plot(dx*12.,-8.,-3)
            end if
        end do    ! End of line_num                          

    end do  !End of month

    call plot(-7.*dx*12,3.,-3)

    call newpen2(2)

    do n=1,31
        if (n<=11) then
            rosso = (real(n)-1)/10.
            verde = (real(n)-1)/10.
            azul = 1.
            call betsqk(0.,0.,0.2,0.3,rosso,verde,azul)
            call rgbk(0.,0.,0.)
            call plot(0.,0.,3)
            call plot(0.2,0.,2)
            call plot(0.,0.,3)
            call plot(0.,-0.05,2)
            call numberc(0.,-0.15,0.1,real(n-1),0.,-1)
        else
            rosso = 1.
            verde = 1.-(real(n)-11.)/20.
            azul = 1.-(real(n)-11.)/20.
            call betsqk(0.,0.,0.2,0.3,rosso,verde,azul)
            call rgbk(0.,0.,0.)
            call plot(0.,0.,3)
            call plot(0.2,0.,2)
            call plot(0.,0.,3)
            call plot(0.,-0.05,2)
            call plot(0.2,0.,3)
            call plot(0.2,-0.05,2)
            call numberc(0.,-0.15,0.1,real(n-1),0.,-1)
        end if
        call plot(0.2,0.,-3)
    end do

    call numberc(0.,-0.15,0.1,31.,0.,0)
    call symbolc()




    !等高線用
    ! call newpen2(3)
    ! call rgbk(0.,0.,0.)
    ! call pscont3(dx,dy,sal,mask,is,imax,ks,kmax,imax,kmax,5,0.,6.)
   



 



    




call plote


end program

