program aomori_coastal_data
    implicit none

    integer,parameter::month=12,seasons=4,year_start=2008,year_max=15,station_y=2,station_x=9,depth_max=200 !station_xは深層見れない時7
    !line_num=緯度別の観測、st_x=経度別の観測列、年、月、深さ、塩分または水温
    real,dimension(year_max,month,station_y,station_x,depth_max)::potemp_5,sal_5
    character::aa*9,filename*999,line_name*20,moon*9,station*9,salmin*9,salmax*9,tempmin*9,tempmax*9   !*数字は文字数を表す。trimすると空白の部分が抜かれる。　　　　　　　　　!要実験!!!!!!!!!!!!!!!!!!!!
    character(len=*), parameter::pass_CTD = 'Yuta_edit_median_potemp'   !高槻さんのデータ引用　後で変える → done
    ! character(len=*), parameter::pass_CTD_01 = 'edited_data'
    character(len=3),dimension(12)::month_names
        
    real,dimension(month,station_x,depth_max)::temp_avarray,temp_SD_array,sal_avarray,sal_SD_array
    integer,parameter::axis_temp_min = 0, axis_temp_max = 24, station_num = 8                           !温度目盛りの範囲とステーションを指定できる
    real,parameter:: valid_sal_min = 33.80, valid_sal_max = 34.40, sal_range = valid_sal_max - valid_sal_min  !ふさわしいデータとして扱う塩分範囲を指定できる
    integer,parameter::axis_temp_range = axis_temp_max-axis_temp_min,temp_range = axis_temp_max-axis_temp_min, axis_sal_range = 40 

    real,parameter::xlength=5., ylength=2.5
    real,parameter::dx = xlength/real(month), temp_dy = ylength/real(axis_temp_range), sal_dy = ylength/real(axis_sal_range)
    integer::is=1,js=1,n,q,x,memori,tempnum,salnum,temp_numtotal,sal_numtotal,i,j,k,y,m,line_num,year,mon,stations,gain,yagain,xpoints,ypoints,magain
    real::temp_sum,first_temp_sum,temp_av,first_temp_devsq,temp_devsq_sum,temp_SD,temp_var,g,r,b,blue,rosso,verde,azul,xco,yco,d
    real::sal_sum,first_sal_sum,sal_av,first_sal_devsq,sal_devsq_sum,sal_SD,sal_var,temp_av_yplot,sal_av_yplot,xplot,plot_temp_SD,plot_sal_SD
    real::tri_first,tri_sum
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
    write(station,'(i1)') station_num
    write(salmin,'(f5.2)') valid_sal_min
    write(salmax,'(f5.2)') valid_sal_max
    write(tempmin,'(f4.1)') real(axis_temp_min)
    write(tempmax,'(f4.1)') real(axis_temp_max)

    call plots(2.,13.8,13,'Monthly_av_and_sd/Monthly_Data_station'//trim(station)//'.ps')
    call newpen2(3)
    call symbol(0.,5.,1.,'Monthly Data of station'//trim(station)//'.ps',0.,len('Monthly Data of station x'))
    call symbol(12.,4.,0.3,'using '//trim(salmin)//'<= sal <='//trim(salmax)//' as valid data',0.,len('using '//trim(salmin)//'<= sal <='//trim(salmax)//' as valid data'))
    call symbol(12.,4.5,0.3,trim(tempmin)//'<= temp_axis <='//trim(tempmax),0.,len(trim(tempmin)//'<= temp_axis <='//trim(tempmax)))
    
    call rgbk(0.,0.,0.)
    call plot(3.*(xlength+1.5),4.25,-3)      !地図用の原点
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
    do line_num = 1,2
        if (line_num == 1) then
            stations = station_x
        else
            stations = station_x
        end if
        do x = station_num,station_num,1    !沿岸域だけ
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

    call symbol(1.,SlineYco-0.4,0.3,'S-Line',0.,6)
    call symbol(1.,NlineYco+0.4,0.3,'N-Line',0.,6)
    call symbolc(-0.5,dy2*120.,0.2,'Lat.(N)',90.,8)
    call symbolc(dx2*80.*2.5,-0.6,0.2,'Long.(E)',0.,9)

    call plot(-3.*(xlength+1.5),-4.25,-3)


    do line_num = 1,2
        do k = 50,200,50
            do m = 1,month
                do y = 1, year_max
                    if (potemp_5(y,m,line_num,station_num,k) /= 0.) then   !水温に関して
                        temp_sum = first_temp_sum + potemp_5(y,m,line_num,station_num,k)
                        first_temp_sum = temp_sum
                        tempnum = tempnum +1
                    else
                        ! first_temp_sum = first_temp_sum
                        ! tempnum = tempnum                 not necessary
                    end if
                    
                    if (sal_5(y,m,line_num,station_num,k) >= valid_sal_min .and. sal_5(y,m,line_num,station_num,k)<=valid_sal_max) then
                        sal_sum = first_sal_sum + sal_5(y,m,line_num,station_num,k)
                        first_sal_sum = sal_sum
                        salnum = salnum +1                                            !１５年分の和を得る
                    else 
                    end if

                end do                              !end of y years 
                if(tempnum /= 0) then
                    temp_av = temp_sum/real(tempnum)
                    do yagain = 1, year_max         !もう一度yのループを行い，それぞれの年のデータと，先ほどのループで得られた平均値との差を比較する　→　temp_SDのために
                        if (potemp_5(yagain,m,line_num,station_num,k)/=0.) then
                            temp_devsq_sum = first_temp_devsq + (potemp_5(yagain,m,line_num,station_num,k) - temp_av)**2     !temp_devsq_sumは各データにおける平均との偏差の二乗
                            first_temp_devsq = temp_devsq_sum
                        else
                            !first_temp_devsq = first_temp_devsq
                        end if 
                    end do !end of yagain
                    temp_var = temp_devsq_sum/real(tempnum-1)
                    temp_SD = sqrt(temp_var)
                    temp_SD_array(m,station_num,k) = temp_SD
                    temp_avarray(m,station_num,k) = temp_av                                                                                                                 !図で使うaverageとtemp_SD　ゲット
                else
                    temp_avarray(m,station_num,k) = 0.
                    temp_SD_array(m,station_num,k) = 0.
                end if                                                                                                                                      !ある月の15年分のavとtemp_SDをそれぞれ配列に入れた
                
                if (salnum /= 0) then
                    sal_av = sal_sum/real(salnum)
                    do yagain = 1, year_max         !もう一度yのループを行い，それぞれの年のデータと，先ほどのループで得られた平均値との差を比較する　→　temp_SDのために
                        if (sal_5(yagain,m,line_num,station_num,k) >= valid_sal_min .and. sal_5(yagain,m,line_num,station_num,k)<=valid_sal_max) then
                            sal_devsq_sum = first_sal_devsq + (sal_5(yagain,m,line_num,station_num,k) - sal_av)**2     !temp_devsq_sumは各データにおける平均との偏差の二乗
                            first_sal_devsq = sal_devsq_sum
                        else
                        end if 
                    end do !end of yagain 
                    sal_var = sal_devsq_sum/real(salnum-1)
                    sal_SD = sqrt(sal_var)
                    sal_SD_array(m,station_num,k) = sal_SD
                    sal_avarray(m,station_num,k) = sal_av                                                                                                                 !図で使うaverageとtemp_SD　ゲット
                else
                    sal_avarray(m,station_num,k) = 0.
                    sal_SD_array(m,station_num,k) = 0.
                end if       
                        
                tempnum = 0
                first_temp_devsq = 0.
                first_temp_sum = 0.
                salnum = 0
                first_sal_devsq = 0.
                first_sal_sum = 0.
            end do                                  !end of m months                                                                                        !12ヶ月分の15年分データを配列に入れ終わった
                call newpen2(3)
                if (k == 50 .and. line_num == 1) then
                    call symbol(-.7,3.5,0.7,'N-Line',0.,len('N-Line'))
                    call symbolc(xlength/2.,3.0,0.5,'50db',0.,4)
                    call symbolc(-.7,ylength/2.,.4,'Temp',90.,4)
                    call symbolc(-.7,-4.+ylength/2.,.4,'Sal',90.,4)
                else if(k==50 .and. line_num ==2) then
                    call symbol(-.7,3.5,0.7,'S-Line',0.,len('S-Line'))
                    call symbolc(xlength/2.,3.0,0.5,'50db',0.,4)
                    call symbolc(-.7,ylength/2.,.4,'Temp',90.,4)
                    call symbolc(-.7,-4.+ylength/2.,.4,'Sal',90.,4)
                else if (k==100) then
                    call symbolc(xlength/2.,3.,0.5,'100db',0.,5)
                else if(k==150) then
                    call symbolc(xlength/2.,3.,0.5,'150db',0.,5)
                else 
                    call symbolc(xlength/2.,3.,0.5,'200db',0.,5)
                end if

                call plot(0.,0.,3)
                call plot(0.,ylength,2)
                call plot(xlength,ylength,2)
                call plot(xlength,0.,2)
                call plot(0.,0.,2)                  !枠作り →　kが変わるごとに枠が出来る
                do xpoints = 1,month                  !目盛
                    call plot(dx*(real(xpoints))-dx/2.,0.,3)                  !xpoints個目の幅dxの箱の中央にプロット
                    call plot(dx*(real(xpoints))-dx/2.,-0.05,2) 
                    call symbolc(dx*(real(xpoints))-dx/2.,-0.4,0.2,month_names(xpoints),0.,3)
                end do

                do ypoints = 0,axis_temp_range  !目盛
           
                    if (mod(ypoints,2) == 0) then
                    call plot(0.,temp_dy*(real(ypoints)),3)
                    call plot(-0.05,temp_dy*(real(ypoints)),2)
                    call numberc(-0.3,temp_dy*(real(ypoints)),0.15,real(ypoints)+real(axis_temp_min),0.,1)
                    else 
                    end if
                end do                                                                                          !tempに関する枠作り完成
                    call symbolc(-0.3,temp_dy*real(temp_range)+0.3,0.2,'deg.(C)',0.,len('deg.(C)'))

                call plot(0.,-4.,-3)                                                                            !原点ずらし

                call plot(0.,0.,3)
                call plot(0.,ylength,2)
                call plot(xlength,ylength,2)
                call plot(xlength,0.,2)
                call plot(0.,0.,2)                  !枠作り →　kが変わるごとに枠が出来る
                do xpoints = 1,month                  !目盛
                    call plot(dx*(real(xpoints))-dx/2.,0.,3) !-1で原点スタート                 xpoints個目の幅dxの箱の中央にプロット
                    call plot(dx*(real(xpoints))-dx/2.,-0.05,2) !-1で原点スタート
                    call symbolc(dx*(real(xpoints))-dx/2.,-0.4,0.2,month_names(xpoints),0.,3)
                end do

                do ypoints = 0,axis_sal_range  !目盛
                    if (mod(ypoints,4) == 0 ) then
                    call plot(0.,sal_dy*(real(ypoints)),3)
                    call plot(-0.05,sal_dy*(real(ypoints)),2)
                    call numberc(-0.3,sal_dy*(real(ypoints)),0.15,valid_sal_min+(real(sal_range)/real(axis_sal_range))*(real(ypoints)),0.,2)
                    else 
                    end if
                end do                                                                                  !salに関する枠作り完成
                    call symbolc(-0.3,temp_dy*real(temp_range)+0.3,0.2,'per mille',0.,len('per mille'))
                call plot(0.,4.,-3)                                                                      !原点をtempのところまで戻す
                if (line_num == 2 .and. k == 50) then
                    print*,sal_avarray(9,station_num,k)       !=34.27657   33.8<=sal<=34.5
                    print*,sal_SD_array(9,station_num,k)      !=0.1940791    
                else 
                end if 
                do magain = 1,month                                                                     !tempとsalに関する平均値と標準偏差をプロット
                    d = 4.
                    temp_av_yplot = ylength*((temp_avarray(magain,station_num,k)-real(axis_temp_min))/real(axis_temp_range))
                    sal_av_yplot = ylength*((sal_avarray(magain,station_num,k)-valid_sal_min)/real(sal_range))
                    xplot = dx*(real(magain)-1./2.)
                    plot_temp_SD = ylength*((temp_SD_array(magain,station_num,k))/real(axis_temp_range))  !y軸に対する比でのSD,avの点を基準に使う
                    plot_sal_SD = ylength*((sal_SD_array(magain,station_num,k))/real(sal_range))    !←salは最小値が0でないので相対的に考えている。34.20-33.80==0.4

                    call gmark(xplot, temp_av_yplot, 0.1, 1)                                            !temp平均値プロット
                    call plot(xplot, temp_av_yplot + plot_temp_SD, 3)                                   !temp標準偏差プロット
                    call plot(xplot,temp_av_yplot - plot_temp_SD, 2)

                    call gmark(xplot, sal_av_yplot-d, 0.1, 1) 
                    call plot(xplot, sal_av_yplot+plot_sal_SD-d, 3)                                     !sal平均値
                    call plot(xplot,sal_av_yplot-plot_sal_SD-d, 2)                                      !sal標準偏差プロット
                end do
                    ! call newpen2(3)
                    ! call plot(-1.5,-1./2.*(4.-ylength),3)
                    ! call plot((xlength+1.5),-1./2.*(4.-ylength),2) !横切る線
                    call newpen2(-1)
                    call plot(xlength+0.75,3.,3)
                    call plot(xlength+0.75,-6.,2)

                    call plot(xlength+1.5,0.,-3)                                                        !原点をx軸方向に動かす→次の図（縦に２個）の原点
                                                                 

        end do                                      !end of k depth                  この段階で原点は(4(xlength+0.5))
        call plot(-4.*(xlength+1.5), -9.5, -3)                                       !下に平行移動→S-Lineのために
    end do                                          !end of line_num
    call plote
end program

                


