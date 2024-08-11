program aomori_coastal_data
    implicit none

    integer,parameter::month=12,seasons=4,year_start=2008,year_max=15,station_y=2,station_x=9,depth_max=400 !station_xは深層見れない時7
    !line_num=緯度別の観測、st_x=経度別の観測列、年、月、深さ、塩分または水温
    real,dimension(year_max,month,station_y,station_x,depth_max)::potemp_5,sal_5
    character::aa*9,filename*999,line_name*20,moon*9,from*9,to*9,salmin*9,salmax*9,tempmin*9,tempmax*9   !*数字は文字数を表す。trimすると空白の部分が抜かれる。　　　　　　　　　!要実験!!!!!!!!!!!!!!!!!!!!
    character(len=*), parameter::pass_CTD = 'Yuta_edit_median_potemp'   !高槻さんのデータ引用　後で変える → done
    ! character(len=*), parameter::pass_CTD_01 = 'edited_data'
    character(len=3),dimension(12)::month_names
    integer,dimension(month,station_x,depth_max)::temp_data_quantity,sal_data_quantity
    real,dimension(station_x,depth_max)::temp_avarray,temp_SD_array,sal_avarray,sal_SD_array
    integer,parameter::axis_temp_min = 0, axis_temp_max = 24, station_num = 9                           !温度目盛りの範囲とステーションを指定できる
    real,parameter:: valid_sal_min = 30.00, valid_sal_max = 34.65, sal_range = valid_sal_max - valid_sal_min  !ふさわしいデータとして扱う塩分範囲を指定できる
    ! integer,parameter::axis_temp_range = axis_temp_max-axis_temp_min,temp_range = axis_temp_max-axis_temp_min, axis_sal_range = 40 
    integer,dimension(station_x,depth_max)::temp_mask,sal_mask

    real,parameter::xlength=1.5, ylength=3.
    real,parameter::dx = xlength/2., temp_dy = -ylength/real(depth_max), sal_dy = -ylength/real(depth_max),dy = -ylength/real(depth_max)
    integer::is=station_num-1,js=1,n,q,x,memori,tempnum,salnum,temp_numtotal,sal_numtotal,i,j,k,y,m,line_num,year,mon,stations,gain,yagain,xpoints,ypoints,magain
    real::temp_sum,first_temp_sum,temp_av,first_temp_devsq,temp_devsq_sum,temp_SD,temp_var,g,r,b,blue,rosso,verde,azul,xco,yco,d
    real::sal_sum,first_sal_sum,sal_av,first_sal_devsq,sal_devsq_sum,sal_SD,sal_var,temp_av_yplot,sal_av_yplot,xplot,plot_temp_SD,plot_sal_SD
    !map用
    intrinsic sin,cos,tan,asin,acos

    integer,parameter::imax=2080, jmax=2640, istart=(137-122)*80+1, iend=(142-122)*80, jstart=(40-24)*120+1, jend=(42-24)*120
    real,parameter::pi=2.*asin(1.),ratio=6357./6378./cos(41.*pi/180.),mapylength = ylength*2./3., mapxlength = mapylength*5./2./ratio
    real,parameter::dx2=mapxlength/real(iend-istart+1.),dy2=mapylength/real(jend-jstart+1.)
    real,dimension(imax,jmax)::dep
    real,dimension(station_y,station_x)::lon
    integer,dimension(imax,jmax)::dep_m
    real::SlineYco = dy2*(40.6-40.)*120, NlineYco = dy2*(41.-40.)*120

    month_names=(/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'/)

102 format(9(f9.4))
!観測data
    do line_num=1,station_y
        if (line_num==1) then
            line_name='N-Line'
        else if (line_num==2) then
            line_name='S-Line'   !NとSを今までと逆にしたことに注意→元からこちらの方が自然な気がする。
        else;end if  !参照するｃｓｖファイルを決定するために名前を付ける
        do m=1,month
        write(moon,'(i2.2)') m  !moonに月の数字を代入していく
        do y=1,year_max
        year=y+year_start;write(aa,'(i4.4)') year
        filename=trim(pass_CTD)//'/'//trim(line_name)//'/'//trim(moon)//'/'//'potemp_tem_sal51mdn'//trim(aa)//'.csv'  !01でなく51ではないか→kaiketsu
        open(31,file=filename,status='old',action='read')
        do k=1,depth_max
            read(31,102) (potemp_5(y,m,line_num,i,k),i=1,station_x)!??
        end do
        close(31)
        filename=trim(pass_CTD)//'/'//trim(line_name)//'/'//trim(moon)//'/sal_51mdn'//trim(aa)//'.csv'
        open(32,file=filename,status='old',action='read')
        do k=1,depth_max
            read(32,102) (sal_5(y,m,line_num,i,k),i=1,station_x)
        end do
        close(32)
        filename=trim(pass_CTD)//'/'//trim(line_name)//'/lon.csv'
        open(33,file=filename,status='old',action='read')
            read(33,102) (lon(line_num,i),i =1, station_x) 
        close(33)                                                                                !読み込みの段階ではstation_x全て配列にぶち込んである   使うのはx = 9 のみ

    end do
    end do;end do

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
        write(from,'(i1)') station_num-1
        write(to,'(i1)') station_num
        write(salmin,'(f5.2)') valid_sal_min
        write(salmax,'(f5.2)') valid_sal_max
        write(tempmin,'(f4.1)') real(axis_temp_min)
        write(tempmax,'(f4.1)') real(axis_temp_max)

        call plots(2.,15.5,13,'Monthly_av_and_sd/Monthly_Data_of_stations'//trim(from)//'to'//trim(to)//'with_contour_2017excluded.ps')
        call newpen2(3)
        call symbol(0.,3.,1.,'Monthly Data of stations '//trim(from)//' to '//trim(to),0.,len('Monthly Data of stations '//trim(from)//'  to'//trim(to)))
        call symbol(0.,2.,0.3,'using '//trim(salmin)//'<= sal <='//trim(salmax)//' as valid data',0.,len('using '//trim(salmin)//'<= sal <='//trim(salmax)//' as valid data'))
        call symbol(0.,1.3,0.3,'data of year 2017 excluded',0.,len('data of year 2017 excluded'))
        ! call symbol(12.,4.5,0.3,trim(tempmin)//'<= temp_axis <='//trim(tempmax),0.,len(trim(tempmin)//'<= temp_axis <='//trim(tempmax)))
        
        call rgbk(0.,0.,0.)
        call plot(16.,4.-mapylength,-3)      !地図用の原点
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
        do line_num = 1,1
        do x = station_num-1,station_num,1    !沿岸域だけ
            Xco = dx2*(lon(line_num,x)-137.)*80.    !x_stationそれぞれのｘ座標
            if (line_num == 2) then   ! 1 = N-line, 2 = S-line  途中で変えたのでややこしくなっている
                call gmark(Xco,SlineYco,0.1,1)
                call numberc(Xco,SlineYco+0.1,0.18,real(x),0.,-1)
            else if(line_num == 1) then
                call gmark(Xco,NlineYco,0.1,1)
                call numberc(Xco,NlineYco+0.1,0.2,real(x),0.,-1)
            else
            end if
        end do    
        end do

        ! call symbol(1.,SlineYco-0.4,0.3,'S-Line',0.,6)
        call symbol(1.,NlineYco+0.4,0.3,'N-Line',0.,6)
        call symbolc(-0.5,dy2*120.,0.2,'Lat.(N)',90.,8)
        call symbolc(dx2*80.*2.5,-0.6,0.2,'Long.(E)',0.,9)

    call plot(-16.,-4.+mapylength,-3)

    do line_num = 1,1
        do m = 1,month
            do x = station_num-1,station_num
                do k = 1,depth_max
                    do y = 1, year_max
                        if (y == 9) cycle

                    if (potemp_5(y,m,line_num,x,k) /= 0.) then   !水温に関して
                        temp_sum = first_temp_sum + potemp_5(y,m,line_num,x,k)
                        first_temp_sum = temp_sum
                        tempnum = tempnum +1
                        else
                        ! first_temp_sum = first_temp_sum
                        ! tempnum = tempnum                 not necessary
                        end if
                    
                        if (sal_5(y,m,line_num,x,k) >= valid_sal_min .and. sal_5(y,m,line_num,x,k)<=valid_sal_max) then
                        sal_sum = first_sal_sum + sal_5(y,m,line_num,x,k)
                        first_sal_sum = sal_sum
                        salnum = salnum +1                                            !１５年分の和を得る
                        else 
                        end if
                

                    end do                              !end of y years 

                temp_data_quantity(m,x,k) = tempnum
                sal_data_quantity(m,x,k) = salnum

                if(tempnum /= 0) then
                    temp_av = temp_sum/real(tempnum)
                    do yagain = 1, year_max         !もう一度yのループを行い，それぞれの年のデータと，先ほどのループで得られた平均値との差を比較する　→　temp_SDのために

                        if (yagain==9) cycle

                        if (potemp_5(yagain,m,line_num,x,k)/=0.) then
                            temp_devsq_sum = first_temp_devsq + (potemp_5(yagain,m,line_num,x,k) - temp_av)**2     !temp_devsq_sumは各データにおける平均との偏差の二乗
                            first_temp_devsq = temp_devsq_sum
                        else
                        end if 
                        end do !end of yagain
                        temp_var = temp_devsq_sum/real(tempnum-1)
                        temp_SD = sqrt(temp_var)
                        temp_SD_array(x,k) = temp_SD
                        temp_avarray(x,k) = temp_av    
                        temp_mask(x,k) = 1                                                                                                             !図で使うaverageとtemp_SD　ゲット
                        else
                        temp_av = 0.
                        temp_SD = 0.
                        temp_avarray(x,k) = 0.
                        temp_SD_array(x,k) = 0.
                        temp_mask(x,k) = 0
                    end if                                                                                                                                      !ある月の15年分のavとtemp_SDをそれぞれ配列に入れた
                
                if (salnum /= 0) then
                    sal_av = sal_sum/real(salnum)
                    do yagain = 1, year_max         !もう一度yのループを行い，それぞれの年のデータと，先ほどのループで得られた平均値との差を比較する　→　temp_SDのために

                        if (yagain==9) cycle

                        if (sal_5(yagain,m,line_num,x,k) >= valid_sal_min .and. sal_5(yagain,m,line_num,x,k)<=valid_sal_max) then
                            sal_devsq_sum = first_sal_devsq + (sal_5(yagain,m,line_num,x,k) - sal_av)**2     !temp_devsq_sumは各データにおける平均との偏差の二乗
                            first_sal_devsq = sal_devsq_sum
                        else
                        end if 
                        end do !end of yagain 
                        sal_var = sal_devsq_sum/real(salnum-1)
                        sal_SD = sqrt(sal_var)
                        sal_SD_array(x,k) = sal_SD
                        sal_avarray(x,k) = sal_av        
                        sal_mask(x,k) = 1                                                                                                         !図で使うaverageとtemp_SD　ゲット
                    else
                    sal_av = 0.
                    sal_SD = 0.
                    sal_avarray(x,k) = 0.
                    sal_SD_array(x,k) = 0.
                    sal_mask(x,k) = 0
                end if       
                        
                tempnum = 0
                first_temp_devsq = 0.
                first_temp_sum = 0.
                salnum = 0
                first_sal_devsq = 0.
                first_sal_sum = 0.

                if (m ==1 .or. m == 7 .and. x == 9) then            !temp av for jan and jul
                    do n = 1,24
                        if (n<=10) then
                            r = (real(n))/10.*.9; g = (real(n))/10.*.9; b = 1.
                        else
                            r = 1.; g = .9-(real(n)-10.)/14.*.9 ;b = .9-(real(n)-10.)/14.*.9  
                        end if
                        if (real(n-1)< temp_av .and. temp_av<=real(n)) then
                            call betsqk(dx,real(k-1)*dy,2*dx,real(k)*dy,r,g,b)
                        else if(temp_av<0.) then
                            call betsqk(dx,real(k-1)*dy,2*dx,real(k)*dy,0.,0.,1.)
                        else if(temp_av==0.) then
                            call betsqk(dx,real(k-1)*dy,2*dx,real(k)*dy,1.,1.,1.)
                        else if(temp_av>24.) then
                            call betsqk(dx,real(k-1)*dy,2*dx,real(k)*dy,1.,0.,0.)
                        end if
                    end do
                    
                    call plot(0.,-ylength*(1.3),-3)
                    do n = 1,20                                     !temp sd for jan and jul
                        if (n<=10) then
                            r = (real(n))/10.*.9; g = (real(n))/10.*.9; b = 1.
                        else 
                            r = 1.; g = .9-(real(n)-10.)/10.*.9; b =.9-(real(n)-10.)/10.*.9
                        end if
                        if (real(n-1)/4.<temp_SD .and. temp_SD<=real(n)/4.) then
                            call betsqk(dx,real(k-1)*dy,2*dx,real(k)*dy,r,g,b)
                        else if(temp_SD == 0.) then
                            call betsqk(dx,real(k-1)*dy,2*dx,real(k)*dy,1.,1.,1.)
                        else if (temp_SD>5.) then
                            call betsqk(dx,real(k-1)*dy,2*dx,real(k)*dy,1.,0.,0.)
                        end if
                    end do
                    
                    call plot(0.,-ylength*(1.3),-3)
                    do n=1,25                                       !sal av for jan and jul
                        if (n<=10) then
                            r = (real(n))/10.*.9; g = (real(n))/10.*.9; b = 1.
                        else
                            r = 1.; g = .9-(real(n)-10.)/15.*0.9; b = .9-(real(n)-10.)/15.*0.9
                        end if
                        if (33.8+real(n-1)/50.<sal_av .and. sal_av<=33.8+real(n)/50.) then
                            call betsqk(dx,real(k-1)*dy,2*dx,real(k)*dy,r,g,b)
                        else if(sal_av<33.8 .and. sal_av/=0.) then
                            call betsqk(dx,real(k-1)*dy,2*dx,real(k)*dy,0.,0.,1.)
                        else if(sal_av>34.3) then
                            call betsqk(dx,real(k-1)*dy,2*dx,real(k)*dy,1.,0.,0.)
                        else if(sal_av == 0.) then
                            call betsqk(dx,real(k-1)*dy,2*dx,real(k)*dy,1.,1.,1.)
                        end if
                    end do
                    
                    call plot(0.,-ylength*(1.3),-3)
                    do n=1,40,1                                     !sal sd for jan and jul
                        if (n<=20) then
                            r = (real(n))/20.*.9; g = (real(n))/20.*.9; b = 1.
                        else 
                            r = 1.; g = .9-(real(n)-20.)/20.*.9; b = .9-(real(n)-20.)/20.*.9
                        end if
                        if (real(n-1)/100.<sal_SD .and. sal_SD<=real(n)/100.) then
                            call betsqk(dx,real(k-1)*dy,2*dx,real(k)*dy,r,g,b)
                        else if(sal_SD == 0.) then
                            call betsqk(dx,real(k-1)*dy,2*dx,real(k)*dy,1.,1.,1.)
                        else if(sal_SD > 0.50) then
                            call betsqk(dx,real(k-1)*dy,2*dx,real(k)*dy,1.,0.,0.)
                        end if
                    end do
                            
                    call plot(0.,3.*ylength*(1.3),-3)
                end if                                 !end of betsqk for jan and jul

            end do                                  !end of k depth                    図面毎の計算終了      
        end do                                                  !end of x                  

        call symbolc(xlength/2.,.5,.4,month_names(m),0.,len(month_names(m)))
        do n = 1,4                          !枠作り
            call newpen2(3)
            call plot(0.,0.,3)  !枠作り
            call plot(0.,0.-ylength,2)
            call plot(xlength,0.-ylength,2)
            call plot(xlength,0.,2)
            call plot(0.,0.,2)
            do xpoints = 1,2 !目盛
            call plot(dx*(real(xpoints))-dx/2.,-ylength,3) !-1で原点スタート
            call plot(dx*(real(xpoints))-dx/2.,-ylength-0.05,2) !-1で原点スタート
            call numberc(dx*(real(xpoints))-dx/2.,-ylength-0.2,0.15,real(station_num-2 + xpoints),0.,-1)
            end do
            do ypoints = 0,int(depth_max)/50  !目盛
            call plot(0.,temp_dy*50.*(real(ypoints)),3)
            call plot(-0.05,temp_dy*50*(real(ypoints)),2)
            call numberc(-0.15,temp_dy*50.*real((ypoints)),0.1,real((ypoints)*50.),0.,-1)
            end do                                                                                      
            call plot(0.,-ylength*(1.3),-3) 
        end do!枠作り終わり
            call plot(0.,4.*ylength*(1.3),-3) !原点戻し

        !↓温度平均図
        if(m == 1) then
            call symbolc(-.7,-ylength/2.,0.4,'Temp Av',90.,len('temp av'))
            else
            end if
            
            call pscolork(dx,dy,temp_avarray,temp_mask,is,station_num,js,depth_max,station_x,depth_max,-100.,0.,0.,0.,1.)
            call pscolork(dx,dy,temp_avarray,temp_mask,is,station_num,js,depth_max,station_x,depth_max,24.,100.,1.,0.,0.)
        
            do n=1,24,1  !最初は，１月,S_line の鉛直図の作成 
            if (n<=10) then
                r = (real(n))/10.*.9
                g = (real(n))/10.*.9
                blue = 1.     
            else 
                r = 1.
                g = .9-(real(n)-10.)/14.*.9
                blue = .9-(real(n)-10.)/14.*.9
            end if
            call pscolorK(dx,dy,temp_avarray,temp_mask,is,station_num,js,depth_max,station_x,depth_max,real(n-1),real(n),r,g,blue)
            end do   
        call newpen2(2)
        call rgbk(0.,0.,0.)
        call pscont3(dx,dy,temp_avarray,temp_mask,is,station_num,js,depth_max,station_x,depth_max,8,0.,3.)                                                                                       !水温の平均鉛直図一個完成


        !↓温度ｓｄ図      
        call plot(0.,-ylength*(1.3),-3) 
        if(m == 1) then
            call symbolc(-.7,-ylength/2.,0.4,'Temp sd',90.,len('temp sd'))
            else
            end if
            call pscolork(dx,dy,temp_SD_array,temp_mask,is,station_num,js,depth_max,station_x,depth_max,-100.,0.,0.,0.,1.)
            call pscolork(dx,dy,temp_SD_array,temp_mask,is,station_num,js,depth_max,station_x,depth_max,5.,100.,1.,0.,0.)
            do n=1,20,1  !最初は，１月,S_line の鉛直図の作成 
            if (n<=10) then
                r = (real(n))/10.*.9
                g = (real(n))/10.*.9
                blue = 1.
                else 
                r = 1.
                g = .9-(real(n)-10.)/10.*.9
                blue = .9-(real(n)-10.)/10.*.9
            end if
            call pscolorK(dx,dy,temp_SD_array,temp_mask,is,station_num,js,depth_max,station_x,depth_max,real(n-1)/4.,real(n)/4.,r,g,blue)
        end do

        !↓塩分av図
        call plot(0.,-ylength*(1.3),-3)
        if(m == 1) then
            call symbolc(-.7,-ylength/2.,0.4,'Sal Av',90.,len('sal av'))
            else
            end if
            call pscolorK(dx,dy,sal_avarray,sal_mask,is,station_num,js,depth_max,station_x,depth_max,-100.,33.8,0.,0.,1.)
            call pscolorK(dx,dy,sal_avarray,sal_mask,is,station_num,js,depth_max,station_x,depth_max,34.3,100.,1.,0.,0.)

            do n=1,25  !最初は，１月,S_line の鉛直図の作成 
            if (n<=10) then
                r = (real(n))/10.*.9
                g = (real(n))/10.*.9
                blue = 1.
            else
                r = 1.
                g = .9-(real(n)-10.)/15.*0.9
                blue = .9-(real(n)-10.)/15.*0.9
            end if
            call pscolorK(dx,dy,sal_avarray,sal_mask,is,station_num,js,depth_max,station_x,depth_max,33.8+real(n-1)/50.,33.8+real(n)/50.,r,g,blue)
            end do
        ! print*, sal_avarray(8,200)
            call newpen2(1)
            call rgbk(0.,0.,0.)
            call pscont3(dx,dy,sal_avarray,sal_mask,is,station_num,js,depth_max,station_x,depth_max,6,33.8,0.1)

        !↓塩分sd図
        call plot(0.,-ylength*(1.3),-3)
        if(m == 1) then
            call symbolc(-.7,-ylength/2.,0.4,'Sal sd',90.,len('sal sd'))
            else
            end if
            call pscolork(dx,dy,sal_SD_array,sal_mask,is,station_num,js,depth_max,station_x,depth_max,0.4,100.,1.,0.,0.)
                do n=1,40,1  !最初は，１月,S_line の鉛直図の作成 
                    if (n<=20) then
                        r = (real(n))/20.*.9
                        g = (real(n))/20.*.9
                        blue = 1.
                    else 
                        r = 1.
                        g = .9-(real(n)-20.)/20.*.9
                        blue = .9-(real(n)-20.)/20.*.9
                    end if
                    call pscolorK(dx,dy,sal_SD_array,sal_mask,is,station_num,js,depth_max,station_x,depth_max,real(n-1)/100.,real(n)/100.,r,g,blue)
                end do

        call plot(2.*dx+0.4,3.*ylength*(1.3),-3)                                                            !次の月へ原点移し

    end do                                          !end of m months
      
end do                                    !end of line_num if there is such a thing lol


call newpage

call newpen2(3)
call plot(0.,2.,-3)
call symbol(0.,0.5,0.3,'Temp Av (deg.C)',0.,len('Temp Av (deg.c)'))
    do n=0,25                       !temp av　凡例
        if(n==0) then
            rosso = 0.; verde = 0. ; azul =1.
        else if (1<=n .and. n<=10) then
            rosso = (real(n))/10.*.9 
            verde = (real(n))/10.*.9
            azul = 1.
        else if (11<=n .and. n<=24) then
            rosso = 1.
            verde = .9-(real(n)-10.)/14.*.9
            azul = .9-(real(n)-10.)/14.*.9
        else ! n==25
            rosso = 1.;verde = 0. ; azul = 0.
        end if
            call betsqk(0.,0.,0.6,0.4,rosso,verde,azul)
            call rgbk(0.,0.,0.)
            call plot(0.,0.,3)
            call plot(0.,-0.05,2)
            call plot(0.,0.,3)
            call plot(0.6,0.,2)
            call plot(0.6,0.,3)
            call plot(0.6,-0.05,2)
            if (1<=n .and. n<=24) then
                call numberc(0.6,-0.3,0.2,real(n),0.,1)
            else if (n ==0) then
                call symbolr(0.,-0.3,0.2,'<0.0',0.,4)
                call symbolc(0.6,-0.3,0.2,'0.0',0.,3)
            else if (n == 25) then
                call symbol(0.6,-0.3,0.2,'24.0<',0.,5)
            end if           
        call plot(0.6,0.,-3)
    end do

    call plot(-26.*0.6,-2.,-3)
    call symbol(0.,0.5,0.3,'Temp sd (deg.C)',0.,len('Temp sd (deg.c)'))
    do n=0,21                       !temp sd 凡例
        if(n==0) then
            rosso = 0.; verde = 0. ; azul =1.
        else if (1<=n .and. n<=10) then
            rosso = (real(n))/10.*.9 
            verde = (real(n))/10.*.9
            azul = 1.
        else if (11<=n .and. n<=20) then
            rosso = 1.
            verde = .9-(real(n)-10.)/10.*.9
            azul = .9-(real(n)-10.)/10.*.9
        else ! n==21
            rosso = 1.;verde = 0. ; azul = 0.
        end if
        call betsqk(0.,0.,15.6/22.,0.4,rosso,verde,azul)
            call rgbk(0.,0.,0.)
            call plot(0.,0.,3)
            call plot(0.,-0.05,2)
            call plot(0.,0.,3)
            call plot(15.6/22.,0.,2)
            call plot(15.6/22.,0.,3)
            call plot(15.6/22.,-0.05,2)
            if (1<=n .and. n<=20) then
                call numberc(15.6/22.,-0.3,0.2,real(n)/4.,0.,2)
            else if (n ==0) then
                ! call symbolc(0.,-0.3,0.2,'<0.0',0.,4)
                call symbolc(15.6/22.,-0.3,0.2,'0.0',0.,3)
            else if (n == 21) then
                call symbol(0.6,-0.3,0.2,'5.00<',0.,5)
            end if           
        call plot(15.6/22.,0.,-3)
    end do

    call plot(-15.6,-2.,-3)
    call symbol(0.,0.5,0.3,'Sal av (per mille)',0.,len('Sal av (per mille)'))
    do n=0,26                       !sal av 凡例
        if (n ==0) then
            rosso = 0.; verde = 0.; azul =1.
        else if (1<=n .and. n<=10) then
            rosso = (real(n))/10.*.9
            verde = (real(n))/10.*.9
            azul = 1.
        else if (11<=n .and. n<=25) then
            rosso = 1.
            verde = .9-(real(n)-10.)/15.*.9
            azul = .9-(real(n)-10.)/15.*.9
        else ! n =26
            rosso=1.;verde=0.;azul=0.
        end if
            call betsqk(0.,0.,15.6/27.,0.4,rosso,verde,azul)
            call rgbk(0.,0.,0.)
            call plot(0.,0.,3)
            call plot(0.,-0.05,2)
            call plot(0.,0.,3)
            call plot(15.6/27.,0.,2)
            call plot(15.6/27.,0.,3)
            call plot(15.6/27.,-0.05,2)
            if (1<=n .and. n<=25 .and. mod(n,2)/=0) then
                call numberc(15.6/27.,-0.3,0.2,33.8+real(n)/50.,0.,2)
            else if (n ==0) then
                call symbolr(0.,-0.3,0.2,'<33.80',0.,6)
                call symbolc(15.6/27.,-0.3,0.2,'33.80',0.,5)
            else if (n == 26) then
                call symbol(15.6/27.,-0.3,0.2,'34.30<',0.,6)
            end if    
            call plot(15.6/27.,0.,-3)       
        end do
    call plot(-15.6,-2.,-3)

        call symbol(0.,0.5,0.3,'Sal sd (per mille)',0.,len('Sal sd (per mille)'))
    do n=0,41                       !sal sd 凡例
        if (n ==0) then
            rosso = 0.; verde = 0.; azul =1.
        else if (1<=n .and. n<=20) then
            rosso = (real(n))/20.*.9
            verde = (real(n))/20.*.9
            azul = 1.
        else if (21<=n .and. n<=40) then
            rosso = 1.
            verde = .9-(real(n)-20.)/20.*.9
            azul = .9-(real(n)-20.)/20.*.9
        else ! n =41
            rosso=1.;verde=0.;azul=0.
        end if
            call betsqk(0.,0.,15.6/42.,0.4,rosso,verde,azul)
            call rgbk(0.,0.,0.)
            call plot(0.,0.,3)
            call plot(0.,-0.05,2)
            call plot(0.,0.,3)
            call plot(15.6/42.,0.,2)
            call plot(15.6/42.,0.,3)
            call plot(15.6/42.,-0.05,2)
            if (1<=n .and. n<=40 .and. mod(n,2)==0) then
                call numberc(15.6/42.,-0.3,0.2,real(n)/100.,0.,2)
            else if (n ==0) then
                ! call symbolc(0.,-0.3,0.2,'<33.80',0.,6)
                call symbolc(15.6/42.,-0.3,0.2,'0.00',0.,4)
            else if (n == 41) then
                call symbol(15.6/42.,-0.3,0.2,'0.40<',0.,5)
            end if    
            call plot(15.6/42.,0.,-3)       
    end do
        call plot(-15.6,-2.5,-3)

        !データ数の表を作成する
        call symbol(0.,ylength/3.,0.5,'Data Quantity',0.,len('data quantity'))
        call symbolc(-.7,-ylength/2.,0.4,'Temp',90.,len('temp'))
        call newpen2(3)
        do m = 1, month                     !temp data quatity
            do x = station_num-1, station_num
                do k = 1, depth_max
                    if (temp_data_quantity(m,x,k) == 0) then
                        r = 1.
                        g = 1.
                        b = 1.
                    else if (temp_data_quantity(m,x,k) <= 10) then
                        r = 1.
                        g = (real(temp_data_quantity(m,x,k)))/10.
                        b = 0.
                    else 
                        r = 1.-(real(temp_data_quantity(m,x,k))-10.)/5.
                        g = 1.
                        b = 0.
                    end if
                    xco = dx*real(x-(station_num-1))
                    yco = dy*real(k-1)
                    call betsqk(xco,yco,xco+dx,yco+dy,r,g,b)
                                                  
                end do !end of k
            end do !end of x 一個の図が完成
            call symbolc(xlength/2.,.5,.4,month_names(m),0.,len(month_names(m)))
            call plot(0.,0.,3)  !枠作り
            call plot(0.,0.-ylength,2)
            call plot(xlength,0.-ylength,2)
            call plot(xlength,0.,2)
            call plot(0.,0.,2)
            do xpoints = 1,2 !目盛
            call plot(dx*(real(xpoints))-dx/2.,-ylength,3) !-1で原点スタート
            call plot(dx*(real(xpoints))-dx/2.,-ylength-0.05,2) !-1で原点スタート
            call numberc(dx*(real(xpoints))-dx/2.,-ylength-0.2,0.15,real(station_num-2 + xpoints),0.,-1)
            end do
            do ypoints = 0,int(depth_max)/50  !目盛
            call plot(0.,temp_dy*50.*(real(ypoints)),3)
            call plot(-0.05,temp_dy*50*(real(ypoints)),2)
            call numberc(-0.15,temp_dy*50.*real((ypoints)),0.1,real((ypoints)*50.),0.,-1)
            end do
            call plot(2.*dx+0.4,0.,-3)
            
        end do !end of m
        call plot(-12.*(2.*dx+.4),-ylength-1.,-3)

        call symbolc(-.7,-ylength/2.,0.4,'Sal',90.,len('sal'))
        do m = 1, month                     !sal data quantity
            do x = station_num-1, station_num
                do k = 1, depth_max
                    if (sal_data_quantity(m,x,k) == 0) then
                        r = 1.
                        g = 1.
                        b = 1.
                    else if (sal_data_quantity(m,x,k) <= 10) then
                        r = 1.
                        g = (real(sal_data_quantity(m,x,k)))/10.
                        b = 0.
                    else 
                        r = 1.-(real(sal_data_quantity(m,x,k))-10.)/5.
                        g = 1.
                        b = 0.
                    end if
                    xco = dx*real(x-(station_num-1))
                    yco = dy*real(k-1)
                    call betsqk(xco,yco,xco+dx,yco+dy,r,g,b)
                                                  
                end do !end of k
            end do !end of x 一個の図が完成
            call plot(0.,0.,3)  !枠作り
            call plot(0.,0.-ylength,2)
            call plot(xlength,0.-ylength,2)
            call plot(xlength,0.,2)
            call plot(0.,0.,2)
            do xpoints = 1,2 !目盛
            call plot(dx*(real(xpoints))-dx/2.,-ylength,3) !-1で原点スタート
            call plot(dx*(real(xpoints))-dx/2.,-ylength-0.05,2) !-1で原点スタート
            call numberc(dx*(real(xpoints))-dx/2.,-ylength-0.2,0.15,real(station_num-2 + xpoints),0.,-1)
            end do
            do ypoints = 0,int(depth_max)/50  !目盛
            call plot(0.,temp_dy*50.*(real(ypoints)),3)
            call plot(-0.05,temp_dy*50*(real(ypoints)),2)
            call numberc(-0.15,temp_dy*50.*real((ypoints)),0.1,real((ypoints)*50.),0.,-1)
            end do
            call plot(2.*dx+0.4,0.,-3)
            
        end do !end of m

        call plot(-12.*(2.*dx+.4),-ylength-1.5,-3)
        call symbol(0.,0.5,0.3,'Data Quantity',0.,len('Data Quantity'))
    do n = 0,15                             !data quantity legend
        if(n==0) then
            r = 1.
            g = 1.
            b = 1.
        else if (n<=10) then
            r = 1.
            g = (real(n)/10.)
            b = 0.
        else
            r = 1.-(real(n)-10.)/5.
            g = 1.
            b = 0.
        end if
        call betsqk(0.,0.,14.4/15.,0.4,r,g,b)
        call rgbk(0.,0.,0.)
        call plot(0.,0.,3)
        call plot(14.4/15.,0.,2)
        call plot(14.4/15./2.,0.,3)
        call plot(14.4/15./2.,-0.05,2)
        call numberc(14.4/15./2.,-0.3,0.2,real(n),0.,-1)
        
        call plot(14.4/15.,0.,-3)
    end do
        





    call plote

end program

                


