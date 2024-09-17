program aomori_data
    ! implicit none

    integer,parameter::month=12,seasons=4,year_start=2008,year_max=15,station_y=2,station_x=9,depth_max=400 !station_xは深層見れない時7
    !line_num=緯度別の観測、st_x=経度別の観測列、年、月、深さ、塩分または水温
    real,dimension(year_max,month,station_y,station_x,depth_max)::potemp_5,sal_5
    ! real,dimension(station_y,station_x,depth_max)::initial_tempsum
    ! real,dimension(year_max,month,station_y,station_x)::lon
    character::aa*9,filename*999,line_name*20,moon*9
    character(len=*), parameter::pass_CTD = 'Yuta_edit_median_potemp'   !高槻さんのデータ引用　後で変える
    character(len=*), parameter::pass_CTD_01 = 'edited_data'
    
    ! real,dimension(month,station_y,station_x,depth_max)::av_array
    real,dimension(station_x,depth_max)::average_array,sd_array
    real,dimension(year_max,station_x,depth_max)::trimonth_av_array
    integer,dimension(station_x,depth_max)::maskav,masksd
    real,parameter::xlength=1.5, ylength=3.
    real,parameter::dx = xlength/real(station_x), dy = -ylength/real(depth_max)
    integer::is=1,js=1,n,q,x,memori,num,numy,numtotal,i,j,k,y,m,line_num,year,mon,stations,gain,yagain
    real::calc_sum,first_sum,calc_av,first_dev,dev_sum,SD,var,g,r,b,blue,xpoints,ypoints,rosso,verde,azul,xco,yco
    real::tri_first,tri_sum
    !map用
    intrinsic sin,cos,tan,asin,acos

    integer,parameter::imax=2080, jmax=2640, istart=(137-122)*80+1, iend=(142-122)*80, jstart=(40-24)*120+1, jend=(42-24)*120
    real,parameter::pi=2.*asin(1.),ratio=6357./6378./cos(41.*pi/180.),mapylength = ylength, mapxlength = mapylength*5./2./ratio
    real,parameter::dx2=mapxlength/real(iend-istart+1.),dy2=mapylength/real(jend-jstart+1.)
    real,dimension(imax,jmax)::dep
    real,dimension(station_y,station_x)::lon
    integer,dimension(imax,jmax)::dep_m
    real::SlineYco = dy2*(40.6-40.)*120, NlineYco = dy2*(41.-40.)*120
    


!情報解析演習にも載ってる、出力する値の書かれ方
!この場合横に9列、〇〇〇〇.〇〇〇〇
!            全体で9マスうち4桁小数を書く
102 format(9(f9.4))
!観測data
    ! write(meter,'(i4.4)') depth_max
    do line_num=1,station_y
        if (line_num==1) then
            line_name='S-Line'
        else if (line_num==2) then
            line_name='N-Line'
        else;end if  !参照するｃｓｖファイルを決定するために名前を付ける
    do m=1,month
        !↓下の分の操作　　例)m=5のintegerをmoon='5'の文字列に変換する
        write(moon,'(i2.2)') m  !moonに月の数字を代入していく
    do y=1,year_max
        !↓下の分の操作　　例)year=2020のintegerをaa='2020'の文字列に変換する
        year=y+year_start;write(aa,'(i4.4)') year
        !//は文字列の結合を意味する。  ここでintegerの変数のまま結合できないので上の行で文字列化させている。
        filename=trim(pass_CTD)//'/'//trim(line_name)//'/'//trim(moon)//'/'//'potemp_tem_sal51mdn'//trim(aa)//'.csv'  !01でなく51ではないかkaiketsu
        open(31,file=filename,status='old',action='read')
        do k=1,depth_max
            read(31,102) (potemp_5(y,m,line_num,i,k),i=1,station_x)!??
        end do
        close(31)
        ! filename=trim(pass_CTD_01)//'/'//trim(line_name)//'/'//trim(moon)//'/'//'sal'//trim(aa)//'.csv'
        ! open(32,file=filename,status='old',action='read')
        ! do k=1,depth_max
        !     read(32,102) (sal_5(y,m,line_num,i,k),i=1,station_x)
        ! end do
        ! close(32)
        ! filename=trim(pass_CTD)//'/'//trim(line_name)//'/lon.csv'
        ! open(33,file=filename,status='old',action='read')
        !     read(33,102) (lon(y,m,line_num,i),i=1,station_x) 
        ! close(33)

    end do
    end do;end do
    !potemp5とsal5は，ある年月の，北か南のラインでの９測定位置分のデータが入った膨大な５次元配列
    !各月平均を知りたい。全ての年の同じ月におけるデータの平均を取って新たな配列avpotemp_5にでもいれてみる。
    call plots(1.2,5.,13,'Seasonal_Data_Temp_Yuta_medpot.ps')
    call newpen2(3)
    call symbolc(13.,14.,1.,'Seasonal Data of Water Temperature',0.,len('Seasonal Data of Water Temperature'))
    ! call symbol(13.,14.,.2,'Data represents the median temp.',0.,len('Seasonal Data of Water Temperature'))

    do m = 1,month,3          
        do line_num = 1,2
            if (line_num == 1) then
                stations = station_x-1
            else
                stations = station_x
            end if
            do x = 1, stations
                do  k = 1, depth_max
                    do y = 1, year_max
                            calc_sum = first_sum + potemp_5(y,m,line_num,x,k)+potemp_5(y,m+1,line_num,x,k)+potemp_5(y,m+2,line_num,x,k) !→ 1 datam
                            first_sum = calc_sum   !三か月分のデータを足し合わせた。
                            tri_sum = potemp_5(y,m,line_num,x,k)+potemp_5(y,m+1,line_num,x,k)+potemp_5(y,m+2,line_num,x,k)
                                do gain = 0,2
                                    if (potemp_5(y,m+gain,line_num,x,k) /= 0.) then
                                        num = num+1
                                        numtotal = numtotal+1
                                    else 
                                        ! num = num
                                        ! numtotal = numtotal
                                    end if
                                end do
                            if (num /= 0) then
                                trimonth_av_array(y,x,k) = tri_sum/real(num)
                            else 
                                trimonth_av_array(y,x,k) = 0.
                            end if
                            num = 0                              !tri_sumは一年分，calc_sumは十五年分
                      end do !End of y years　三か月分のデータを１５年分足し合わせた。
                    ! if(k==100) then
                    !     call numberc(real(x)*dx-dx/2.,0.05,.13,real(numtotal),0.,-1) !何年分のデータがあるかをdepth=100を基準に判断した
                    !     ! call numberc(real(x)*dx-dx,0.2,.13,calc_sum,0.,-1)
                    ! else
                    ! end if              
                    if (numtotal /= 0) then
                        calc_av = calc_sum/real(numtotal)  !15年間における三か月分の平均
                        do yagain = 1,year_max
                            if(trimonth_av_array(yagain,x,k) /=0.) then
                                dev_sum = first_dev + (trimonth_av_array(yagain,x,k)-calc_av)**2.
                                first_dev = dev_sum
                                numy = numy+1
                            else
                                first_dev = first_dev
                                numy = numy
                            end if
                        end do   !１５年分の偏差の二乗の和ゲット
                        average_array(x,k) = calc_av !m=1,4,7,10 で入るといいな　入った。
                        maskav(x,k) = 1
                        var = dev_sum/real(numy)
                        SD = sqrt(var)
                        sd_array(x,k) = SD
                        masksd(x,k) = 1
                                    
                    else !numtotal = 0　の時
                        average_array(x,k) = 0.
                        maskav(x,k) = 0
                        sd_array(x,k) = 0.
                        masksd(x,k) = 0
                    end if 
                    ! datanum_array(x,k) = num  !三か月分のデータ数が貯蓄される
                    if (real(numtotal)==0.) then
                        r = 1.
                        g = 1.
                        b = 1.
                    else if (real(numtotal)<=20. .and. real(numtotal)/=0.) then
                        r = 1.
                        g = (real(numtotal)/20.)
                        b = 0.
                    else if(real(numtotal)>=21. .and. real(numtotal)<=40.) then
                        r = 1.-(real(numtotal)-20.)/20.
                        g = 1.
                        b = 0.
                    else
                        r = 0.
                        g =1.
                        b =0.
                    end if
                    xco = dx*real(x-1)
                    yco = dy*real(k-1)
                    call betsqk(xco,yco,xco+dx,yco+dy,r,g,b)
                    if(k==100) then
                        ! call numberc(real(x)*dx-dx/2.,0.3,.05,real(SD),0.,1)
                        ! call numberc(real(x)*dx-dx/2.,0.4,.1,real(numy),0.,-1)
                        call numberc(real(x)*dx-dx/2.,0.3,.1,real(numtotal),0.,-1)
                    else
                    end if

                    numtotal = 0
                    numy = 0
                    first_sum = 0.  
                    first_dev = 0.

                end do !End of k depth     x=1でのデータ数鉛直棒が出来た 
              end do !End of x stations    データ鉛直図が出来た
                call newpen2(3)
                call plot(0.,0.,3)  !枠作り
                call plot(0.,0.-ylength,2)
                call plot(dx*9.,0.-ylength,2)
                call plot(dx*9.,0.,2)
                call plot(0.,0.,2)
                call symbolc(9.*dx/2.,0.05,0.15,'Data Quantity',0.,len('Data Quantity'))
                do xpoints = 1,9 !目盛
                    call plot(dx*(real(xpoints))-dx/2.,-ylength,3) !-1で原点スタート
                    call plot(dx*(real(xpoints))-dx/2.,-ylength-0.05,2) !-1で原点スタート
                    call numberc(dx*(real(xpoints))-dx/2.,-ylength-0.2,0.15,real(xpoints),0.,-1)
                end do
                do ypoints = 0,8  !目盛
                    call plot(0.,dy*50.*(real(ypoints)),3)
                    call plot(-0.05,dy*50*(real(ypoints)),2)
                    call numberc(-0.15,dy*50.*real((ypoints)),0.1,real((ypoints)*50.),0.,-1)
                end do 

                call plot(dx*12.,0.,-3) !原点をｘ右方向にずらす 　　av鉛直図をデータ数の鉛直図の隣に書く
                call symbolc(9.*dx/2.,0.05,0.15,'Average Temp.(deg.C)',0.,len('Average Temp.(deg.C)'))

                call pscolork(dx,dy,average_array,maskav,is,stations,js,depth_max,station_x,depth_max,-100.,0.,0.,0.,1.)
                call pscolork(dx,dy,average_array,maskav,is,stations,js,depth_max,station_x,depth_max,26.,100.,1.,0.,0.) !外れ値マスク
                do n=1,26,1  !最初は，１月,S_line の鉛直図の作成 
                    if (n<=11) then
                        r = (real(n)-1)/10.
                        g = (real(n)-1)/10.
                        blue = 1.
                        call pscolorK(dx,dy,average_array,maskav,is,stations,js,depth_max,station_x,depth_max,real(n-1),real(n),r,g,blue)
                    else 
                        r = 1.
                        g = 1.-(real(n)-11.)/15.
                        blue = 1.-(real(n)-11.)/15.
                    call pscolorK(dx,dy,average_array,maskav,is,stations,js,depth_max,station_x,depth_max,real(n-1),real(n),r,g,blue)
                    end if
                end do
                call newpen2(3)
                call plot(0.,0.,3)  !枠作り
                call plot(0.,0.-ylength,2)
                call plot(dx*9.,0.-ylength,2)
                call plot(dx*9.,0.,2)
                call plot(0.,0.,2)
                
                do xpoints = 1,9 !目盛
                    call plot(dx*(real(xpoints))-dx/2.,-ylength,3) !-1で原点スタート
                    call plot(dx*(real(xpoints))-dx/2.,-ylength-0.05,2) !-1で原点スタート
                    call numberc(dx*(real(xpoints))-dx/2.,-ylength-0.2,0.15,real(xpoints),0.,-1)
                end do

                do ypoints = 0,8  !目盛
                    call plot(0.,dy*50.*(real(ypoints)),3)
                    call plot(-0.05,dy*50*(real(ypoints)),2)
                    call numberc(-0.15,dy*50.*real((ypoints)),0.1,real((ypoints)*50.),0.,-1)
                end do                                                          !av鉛直図完成

                call plot(dx*12.,0.,-3)              !原点をｘ右方向にずらす 　　sd鉛直図をav鉛直図の隣に書く
                call symbolc(9.*dx/2.,0.05,0.15,'SD of Temp.(deg.C)',0.,len('SD of Temp.(deg.C)'))    
                call pscolork(dx,dy,sd_array,masksd,is,stations,js,depth_max,station_x,depth_max,5.,100.,1.,0.,0.) 
                do n=1,20,1  !最初は，１月,S_line の鉛直図の作成 
                    if (n<=10) then
                        r = (real(n)-1)/10.
                        g = (real(n)-1)/10.
                        blue = 1.
                        call pscolorK(dx,dy,sd_array,masksd,is,stations,js,depth_max,station_x,depth_max,real(n-1)/4.,real(n)/4.,r,g,blue)
                    else 
                        r = 1.
                        g = 1.-(real(n)-11.)/9.
                        blue = 1.-(real(n)-11.)/9.
                    call pscolorK(dx,dy,sd_array,masksd,is,stations,js,depth_max,station_x,depth_max,real(n-1)/4.,real(n)/4.,r,g,blue)
                    end if
                end do
                call newpen2(3)
                call plot(0.,0.,3)  !枠作り
                call plot(0.,0.-ylength,2)
                call plot(dx*9.,0.-ylength,2)
                call plot(dx*9.,0.,2)
                call plot(0.,0.,2)
                
                call newpen2(4)
                call plot(12*dx,1.,3)  !しきり作り
                call plot(12*dx,-ylength-1.,2)
                
                do xpoints = 1,9 !目盛
                    call plot(dx*(real(xpoints))-dx/2.,-ylength,3) !-1で原点スタート
                    call plot(dx*(real(xpoints))-dx/2.,-ylength-0.05,2) !-1で原点スタート
                    call numberc(dx*(real(xpoints))-dx/2.,-ylength-0.2,0.15,real(xpoints),0.,-1)
                end do

                do ypoints = 0,8  !目盛
                    call plot(0.,dy*50.*(real(ypoints)),3)
                    call plot(-0.05,dy*50*(real(ypoints)),2)
                    call numberc(-0.15,dy*50.*real((ypoints)),0.1,real((ypoints)*50.),0.,-1)
                end do 
                


                if (m==1) then
                    call symbolc(-7.5*dx,0.6,0.4,'Jan,Feb,Mar',0.,len('Jan,Feb,Mar'))
                else if(m==4) then
                    call symbolc(-7.5*dx,0.6,0.4,'Apr,May,June',0.,len('Apr,May,June'))
                else if(m==7) then
                    call symbolc(-7.5*dx,0.6,0.4,'July,Aug,Sep',0.,len('July,Aug,Sep'))
                else if(m==10) then
                    call symbolc(-7.5*dx,0.6,0.4,'Oct,Nov,Dec',0.,len('Oct,Nov,Dec'))
                end if

                if(line_num==1 .and. m==4) then
                    call symbolc(12*dx,1.2,0.6,'S-line',0.,6)
                    call plot (-12.*2.*dx,10.,-3)
                else if (line_num==2 .and. m==4) then
                    call symbolc(12*dx,1.2,0.6,'N-line',0.,6)     
                    call plot(dx*15.,-10.,-3)
                else if(line_num==1) then
                    call plot(-12*2*dx,10.,-3)
                else
                    call plot(dx*15.,-10.,-3)
                end if
                                    
        end do    ! End of line_num                          

    end do  !End of month

    call newpen2(2)
    call plot(-152.*dx,5.,-3)  !データ数バーの原点になる

    call symbol(0.,0.4,0.3,'Data Quantity',0.,len('Data Quantity'))
    do n = 0,44
        if(n==0) then
            r = 1.
            g = 1.
            b = 1.
        else if (n<=20 .and. n/=0) then
            r = 1.
            g = (real(n)/20.)
            b = 0.
        else if(n>=21 .and. n<=40) then
            r = 1.-(real(n)-20.)/20.
            g = 1.
            b = 0.
        else 
            r = 0.
            g =1.
            b =0.
        end if
        call betsqk(0.,0.,0.2,0.3,r,g,b)
        call rgbk(0.,0.,0.)
        call plot(0.,0.,3)
        call plot(0.2,0.,2)
        call plot(0.,0.,3)
        call plot(0.,-0.05,2)
            if (mod(n,5)==0) then
                call numberc(0.,-0.2,0.15,real(n),0.,-1)
            else
        end if
        call plot(0.2,0.,-3)
    end do
    call plot(0.,0.,3)
    call plot(0.,-0.05,2)
    call numberc(0.,-0.2,0.15,45.,0.,-1)
    ! call arohd(-14.*0.2,-0.15,-14.*0.2,-0.4,0.1,0.05,6)
    ! call symbolc(-14*0.2,-0.6,0.2,'Max',0.,3)
    call arohd(-45.*0.2,-0.15,-45.*0.2,-0.4,0.1,0.05,6)
    call symbolc(-45*0.2,-0.6,0.2,'Boundary Issue',0.,len('Boundary Issue'))

    call plot(-45*0.2,-1.5,-3)  !温度バーの原点
    call symbol(0.,0.4,0.3,'Temperature (deg.C)',0.,len('Temperature (deg.C)'))
    do n=0,25
        if (n<=10) then
            rosso = real(n)/10.
            verde = real(n)/10.
            azul = 1.
        else 
            rosso = 1.
            verde = 1.-(real(n)-10.)/15.
            azul = 1.-(real(n)-10)/15.
        end if
            call betsqk(0.,0.,9./26.,0.3,rosso,verde,azul)
            call rgbk(0.,0.,0.)
            call plot(0.,0.,3)
            call plot(9./26.,0.,2)
            call plot(0.,0.,3)
            call plot(0.,-0.05,2)
            call numberc(0.,-0.2,0.15,real(n),0.,-1)
        call plot(9./26.,0.,-3)
    end do
    call plot(0.,0.,3)
    call plot(0.,-0.05,2)
    call numberc(0.,-0.2,0.15,26.,0.,-1) !温度バー最後の目盛

    call plot(-45*0.2,-1.5,-3)  !sdバーの原点
    call symbol(0.,0.4,0.3,'Standard Deviation (deg.C)',0.,len('Standard Deviation (deg.C)'))
    do n=0,19
        if (n<=10) then
            rosso = real(n)/10.
            verde = real(n)/10.
            azul = 1.
        else 
            rosso = 1.
            verde = 1.-(real(n)-10.)/9.
            azul = 1.-(real(n)-10)/9.
        end if
            call betsqk(0.,0.,9./20.,0.3,rosso,verde,azul)
            call rgbk(0.,0.,0.)
            call plot(0.,0.,3)
            call plot(9./20.,0.,2)
            call plot(0.,0.,3)
            call plot(0.,-0.05,2)
            call numberc(0.,-0.2,0.15,real(n)/4.,0.,2)
        call plot(9./20.,0.,-3)
    end do
    call newpen2(3)
    call plot(0.,0.,3)
    call plot(0.,-0.05,2)
    call numberc(0.,-0.2,0.15,5.,0.,2) !温度バー最後の目盛

    call plot(5.5,ylength,-3) ! 原点を温度バーの隣に置く
    call plot(0.,0.,3)  !凡例枠
    call plot(0.,0.-ylength,2)
    call plot(dx*9.,0.-ylength,2)
    call plot(dx*9.,0.,2)
    call plot(0.,0.,2)
    
    do xpoints = 1,9 !目盛
        call plot(dx*(real(xpoints)),-ylength,3) !-1で原点スタート
        call plot(dx*(real(xpoints)),-ylength-0.05,2) !-1で原点スタート
        call numberc(dx*(real(xpoints)),-ylength-0.2,0.15,real(xpoints),0.,-1)
    end do

    do ypoints = 0,8  !目盛
        call plot(0.,dy*50.*(real(ypoints)),3)
        call plot(-0.05,dy*50*(real(ypoints)),2)
        call numberc(-0.15,dy*50.*real((ypoints)),0.1,real((ypoints)*50.),0.,-1)
    end do

    call arohd(0.,0.,0.,-ylength,0.1,0.05,6) !depth用の矢印
    call symbolc(-0.4,-ylength/2.,0.2,'Depth(m)',90.,8)
    call arohd(0.,-ylength,dx*9.,-ylength,0.1,0.05,6)
    call symbolc(dx*9./2.,-ylength-0.5,0.2,'Stations(No.)',0.,13)
    call symbolc(dx*9./2.,0.4,0.4,'Months',0.,6)
    call symbolc(dx*9./2.,0.05,0.2,'Data Type',0.,9)

    call plot(2.+xlength,-ylength,-3) ! 原点を凡例図の隣に置く
    !凡例マップの作製
    do line_num = 1,2    !参照ファイルの決定
        if(line_num==1) then
            line_name ='S-line'
        else 
            line_name ='N-line'
        end if
        filename = trim(pass_CTD_01)//'/'//trim(line_name)//'/lon.csv'
        open(34,file=filename,status='old',action='read')
        read(34,102) (lon(line_num,i),i=1,station_x)  !経度lonを配列に入れる
        close(34)
    end do

    open(21,file='japan1km122-148_24-46.bin',form='unformatted',status='old') !配列にデータを入れる
    do j=jmax,1,-1
        read(21)(dep(i,j),i=1,imax)
        dep(i,j)=-dep(i,j)    
    end do
    close(21)
    
    do i=1,imax !マスキング
        do j=1,jmax
            dep_m(i,j)=1
        end do
    end do    

    call newpen2(3)
    call rgbk(0.,0.,0.)
    call pscont3(dx2,dy2,dep,dep_m,istart,iend,jstart,jend,imax,jmax,1,0.,1) !地図作成

    call plot(0.,0.,3)  !枠作り
    call plot(mapxlength,0.,2)
    call plot(mapxlength,mapylength,2)
    call plot(0.,mapylength,2)
    call plot(0.,0.,2)

    do x=1,6  !目盛
        call plot(dx2*80*real(x-1),0,3)
        call plot(dx2*80*real(x-1),-0.1,2)
        call numberc(dx2*80*real(x-1),-0.3,0.2,real(x+136),0.,-1)
    end do

    do y=1,3  !目盛　　　　　後でもっと細かくする
        call plot(0.,dy2*120*real(y-1),3)
        call plot(-0.1,dy2*120*real(y-1),2)
        call numberc(-0.2,dy2*120*real(y-1),0.2,real(y+39),0.,-1)
    end do
! call numberc(5.,5.,1.,lon(1,1),0.,3)

    do line_num = 1,2
        if (line_num == 1) then
            stations = station_x-1
        else
            stations = station_x
        end if
        do x = 1,stations
            Xco = dx2*(lon(line_num,x)-137.)*80.    !x_stationそれぞれのｘ座標
            if (line_num == 1) then   ! 1 = S-line, 2 = N-line
                call gmark(Xco,SlineYco,0.1,1)
                call numberc(Xco,SlineYco+0.1,0.18,real(x),0.,-1)
            else if(line_num == 2) then
                call gmark(Xco,NlineYco,0.1,1)
                call numberc(Xco,NlineYco+0.1,0.2,real(x),0.,-1)
            else
            end if
        end do    
    end do

    call symbol(1.,SlineYco-0.4,0.3,'S-Line',0.,6)
    call symbol(1.,NlineYco+0.4,0.3,'N-Line',0.,6)
    call symbolc(-0.5,dy2*120.,0.2,'Lat.(N)',90.,8)
    call symbolc(dx2*80.*2.5,-0.6,0.2,'Long.(E)',0.,9)
    call symbolc(mapxlength/2.,mapylength+0.4,0.4,'Station Coordinates',0.,len('Station Coordinates'))




    !等高線用
    ! call newpen2(3)
    ! call rgbk(0.,0.,0.)
    ! call pscont3(dx,dy,sal,maskav,is,imax,ks,kmax,imax,kmax,5,0.,6.)
   



call plote


end program

   
