program aomori_data_sd_temp
    integer,parameter::month=12,year_start=2008,year_max=15,station_y=2,station_x=9,depth_max=1000 !station_xは深層見れない時7
    !line_num=緯度別の観測、st_x=経度別の観測列、年、月、深さ、塩分または水温
    real,dimension(year_max,month,station_y,station_x,depth_max)::potemp_5,sal_5
    ! real,dimension(station_y,station_x,depth_max)::initial_tempsum
    ! real,dimension(year_max,month,station_y,station_x)::lon
    character::aa*9,filename*999,line_name*20,moon*9
    character(len=*), parameter::pass_CTD = 'edited_data'
    integer::i,j,k,y,m,line_num,a,b,c,year,mon,stations,yagain

    real,dimension(month,station_y,station_x,depth_max)::av_array
    real,dimension(station_x,depth_max)::useful_array
    integer,dimension(station_x,depth_max)::mask
    real,parameter::xlength=1.5, ylength=3.
    real,parameter::dx = xlength/real(station_x), dy = -ylength/real(depth_max)
    integer::is=1,js=1,n,q,x,memori,num = 0
    real::calc_sum,first_sum,calc_av,dev_sum,first_dev,var,g,r,blue,xpoints,ypoints,rosso,verde,azul

    !map用
    intrinsic sin,cos,tan,asin,acos
    integer,parameter::imax=2080, jmax=2640, istart=(137-122)*80+1, iend=(142-122)*80, jstart=(40-24)*120+1, jend=(42-24)*120
    real,parameter::pi=2.*asin(1.),ratio=6357./6378./cos(41.*pi/180.), mapxlength=4.,mapylength=mapxlength*ratio*2./5.
    real,parameter::dx2=mapxlength/real(iend-istart+1.),dy2=mapylength/real(jend-jstart+1.)
    real,dimension(imax,jmax)::dep
    real,dimension(station_y,station_x)::lon
    integer,dimension(imax,jmax)::dep_m
    real::SlineYco = dy2*(40.6-40.)*120, NlineYco = dy2*(41.-40.)*120

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
    end do
end do
end do

call plots(2.,5.,13,'Monthly_sd_Temp.ps')
    call newpen2(3)
    call symbolc(10.5,13.,1.,'Monthly Standard Deviation of Water Temp.',0.,len('Monthly Standard Deviation of Water Temp.'))

    do m = 1,month
        do line_num = 1,2
            if (line_num == 1) then
                stations = station_x-1
            else
                stations = station_x
            end if
            do x = 1, stations
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
                        ! av_array(m,line_num,x,k) = calc_av
                        do yagain = 1,year_max
                            if (potemp_5(yagain,m,line_num,x,k) /= 0.) then
                            dev_sum = first_dev + (potemp_5(yagain,m,line_num,x,k)-calc_av)**2
                            first_dev = dev_sum  !偏差の二乗の和
                            else
                                first_dev = first_dev
                            end if
                        end do
                        var = dev_sum/real(num) !偏差の二乗の和の平均　つまり分散
                        SD = sqrt(var)  !分散のルート sd  
                        useful_array(x,k) = SD
                        mask(x,k) = 1
                    else
                        useful_array(x,k) = 0.
                        mask(x,k) = 0
                    end if 
                    if(k==100) then
                        call numberc(real(x)*dx-dx/2.,0.05,.13,real(num),0.,-1) !何年分のデータがあるかをdepth=100を基準に判断した
                    else
                    end if                 
                    num = 0
                    first_sum = 0.  
                    first_dev = 0.
                end do !End of k depth
              end do !End of x stations
                call pscolork(dx,dy,useful_array,mask,is,stations,js,depth_max,station_x,depth_max,5.,100.,1.,0.,0.)
                ! call pscolork(dx,dy,useful_array,mask,is,stations,js,depth_max,station_x,depth_max,26.,100.,1.,0.,0.)
                do n=1,21,1  !最初は，１月,S_line の鉛直図の作成 
                    if (n<=11) then
                        r = (real(n)-1)/10.
                        g = (real(n)-1)/10.
                        blue = 1.
                        call pscolorK(dx,dy,useful_array,mask,is,stations,js,depth_max,station_x,depth_max,real(n-1)/4.,real(n)/4.,r,g,blue)
                    else 
                        r = 1.
                        g = 1.-(real(n)-11.)/10.
                        blue = 1.-(real(n)-11.)/10.
                    call pscolorK(dx,dy,useful_array,mask,is,stations,js,depth_max,station_x,depth_max,real(n-1)/4.,real(n)/4.,r,g,blue)
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

                do ypoints = 0,10  !目盛
                    call plot(0.,dy*100.*(real(ypoints)),3)
                    call plot(-0.05,dy*100*(real(ypoints)),2)
                    call numberc(-0.15,dy*100.*real((ypoints)),0.1,real((ypoints)*100.),0.,-1)
                end do  !鉛直図完成
            call numberc(xlength/2.,0.4,0.4,real(m),0.,-1) !数字を月に対応させることを試みる
            if(line_num==1 .and. m==1) then
                call symbol(0.,1.,0.6,'S-line',0.,6)
                call plot (0.,10.,-3)
            else if (line_num==2 .and. m==1) then
                call symbol(0.,1.,0.6,'N-line',0.,6)     
                call plot(dx*12.,-10.,-3)
            else if(line_num==1) then
                call plot(0.,10.,-3)
            else
                call plot(dx*12.,-10.,-3)
            end if
        end do    ! End of line_num                          

    end do  !End of month

    call plot(-10.*dx*12,3.,-3)  !目盛バーの原点になる
    call newpen2(2)
    call symbolc(2.0,0.5,0.3,'Standard Deviation(deg.)',0.,len('Standard Deviation(deg.)'))

    do n=1,21
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
            if(mod(n,2)/=0) then
                call numberc(0.,-0.15,0.1,real(n-1)/4.,0.,2)
                else 
                end if
        else
            rosso = 1.
            verde = 1.-(real(n)-11.)/10.
            azul = 1.-(real(n)-11.)/10.
            call betsqk(0.,0.,0.2,0.3,rosso,verde,azul)
            call rgbk(0.,0.,0.)
            call plot(0.,0.,3)
            call plot(0.2,0.,2)
            call plot(0.,0.,3)
            call plot(0.,-0.05,2)
            call plot(0.2,0.,3)
            call plot(0.2,-0.05,2)
            if(mod(n,2)/=0) then
                call numberc(0.,-0.15,0.1,real(n-1)/4.,0.,2)
                else 
                end if
        end if
        call plot(0.2,0.,-3)
    end do
    ! call numberc(0.,-0.15,0.1,5.50,0.,2) !温度バー最後の目盛

    call plot(1.5,2.,-3) ! 原点を温度バーの隣に置く
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

    do ypoints = 0,10  !目盛
        call plot(0.,dy*100.*(real(ypoints)),3)
        call plot(-0.05,dy*100*(real(ypoints)),2)
        call numberc(-0.15,dy*100.*real((ypoints)),0.1,real((ypoints)*100.),0.,-1)
    end do

    call arohd(0.,0.,0.,-ylength,0.1,0.05,6) !depth用の矢印
    call symbolc(-0.4,-ylength/2.,0.2,'Depth(m)',90.,8)
    call arohd(0.,-ylength,dx*9.,-ylength,0.1,0.05,6)
    call symbolc(dx*9./2.,-ylength-0.5,0.2,'Stations(No.)',0.,13)
    call symbolc(dx*9./2.,0.4,0.4,'Month',0.,5)
    call symbolc(dx*9./2.,0.05,0.13,'Numbers of years with Data',0.,26)

    call plot(3.,dy*1000.,-3) ! 原点を凡例図の隣に置く
    !凡例マップの作製
    do line_num = 1,2    !参照ファイルの決定
        if(line_num==1) then
            line_name ='S-line'
        else 
            line_name ='N-line'
        end if
        filename = trim(pass_CTD)//'/'//trim(line_name)//'/lon.csv'
        open(31,file=filename,status='old',action='read')
        read(31,102) (lon(line_num,i),i=1,station_x)  !経度lonを配列に入れる
        close(31)
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

    call symbol(1.,SlineYco-0.4,0.3,'S-line',0.,6)
    call symbol(1.,NlineYco+0.4,0.3,'N-line',0.,6)
    call symbolc(-0.5,dy2*120.,0.2,'Lat.(N)',90.,8)
    call symbolc(dx2*80.*2.5,-0.6,0.2,'Long.(E)',0.,9)

    call plote
end program