program monthly_av_and_sd_for_Takatsuki_san
    implicit none

    integer,parameter::month=12,seasons=4,year_start=2008,year_max=15,station_y=2,station_x=9,depth_max=500 !station_xは深層見れない時7
    !line_num=緯度別の観測、st_x=経度別の観測列、年、月、深さ、塩分または水温
    real,dimension(year_max,month,station_y,station_x,depth_max)::potemp_5,sal_5
    character::aa*9,filename*999,line_name*20,moon*9,months*9,filename_tempav*999,filename_tempsd*999,filename_salav*999,filename_salsd*999   !*数字は文字数を表す。trimすると空白の部分が抜かれる。　　　　　　　　　!要実験!!!!!!!!!!!!!!!!!!!!
    character(len=*), parameter::pass_CTD = 'Yuta_edit_median_potemp'   !高槻さんのデータ引用　後で変える → done
    character(len=*), parameter::newpass = 'monthly_average_and_sd_csv'
    character(len=3),dimension(12)::month_names
    integer,dimension(month,station_x,depth_max)::temp_data_quantity,sal_data_quantity
    real,dimension(station_x,depth_max)::temp_avarray,temp_SD_array,sal_avarray,sal_SD_array
    integer,parameter::axis_temp_min = 0, axis_temp_max = 24, station_num = 9                           !温度目盛りの範囲とステーションを指定できる
    real,parameter:: valid_sal_min = 30.00, valid_sal_max = 34.65, sal_range = valid_sal_max - valid_sal_min  !ふさわしいデータとして扱う塩分範囲を指定できる
    ! integer,parameter::axis_temp_range = axis_temp_max-axis_temp_min,temp_range = axis_temp_max-axis_temp_min, axis_sal_range = 40 
    integer,dimension(station_x,depth_max)::temp_mask,sal_mask
    integer::kagain,xagain

    real,parameter::xlength=1.5, ylength=3.
    real,parameter::dx = xlength/2., temp_dy = -ylength/real(depth_max), sal_dy = -ylength/real(depth_max),dy = -ylength/real(depth_max)
    integer::is=station_num-1,js=1,n,q,x,memori,tempnum,salnum,temp_numtotal,sal_numtotal,i,j,k,y,m,line_num,year,mon,stations,gain,yagain,xpoints,ypoints,magain
    real::temp_sum,first_temp_sum,temp_av,first_temp_devsq,temp_devsq_sum,temp_SD,temp_var,g,r,b,blue,rosso,verde,azul,xco,yco,d
    real::sal_sum,first_sal_sum,sal_av,first_sal_devsq,sal_devsq_sum,sal_SD,sal_var,temp_av_yplot,sal_av_yplot,xplot,plot_temp_SD,plot_sal_SD


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
        ! filename=trim(pass_CTD)//'/'//trim(line_name)//'/lon.csv'
        ! open(33,file=filename,status='old',action='read')
        !     read(33,102) (lon(line_num,i),i =1, station_x) 
        ! close(33)                                                                                !読み込みの段階ではstation_x全て配列にぶち込んである   使うのはx = 9 のみ

    end do
    end do;end do  
        
    do line_num = 1,2
        if(line_num ==1) then
            line_name = 'N-line'
        else
            line_name = 'S-line'   !高槻さん用lineは小文字
        end if
        do m = 1,month
            write(months,'(i2.2)')m
            do x = 1,station_num
                do k = 1,depth_max
                    do y = 1, year_max
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
                        temp_avarray(x,k) = 0.
                        temp_SD_array(x,k) = 0.
                        temp_mask(x,k) = 0
                    end if                                                                                                                                      !ある月の15年分のavとtemp_SDをそれぞれ配列に入れた
                
                if (salnum /= 0) then
                    sal_av = sal_sum/real(salnum)
                    do yagain = 1, year_max         !もう一度yのループを行い，それぞれの年のデータと，先ほどのループで得られた平均値との差を比較する　→　temp_SDのために
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
            end do                                  !end of k depth                    図面毎の計算終了      

        end do                                      !end of x                  
                filename_tempav = trim(newpass)//'/'//trim(line_name)//'/'//trim(months)//'_temp_av.csv'
                filename_tempsd = trim(newpass)//'/'//trim(line_name)//'/'//trim(months)//'_temp_sd.csv'
                filename_salav = trim(newpass)//'/'//trim(line_name)//'/'//trim(months)//'_sal_av.csv'
                filename_salsd = trim(newpass)//'/'//trim(line_name)//'/'//trim(months)//'_sal_sd.csv'

                open(7,file = filename_tempav, status = 'replace')
                do kagain = 1,depth_max
                    write(7,102)(temp_avarray(xagain,kagain),xagain = 1,station_num)
                end do

                open(8,file = filename_tempsd, status = 'replace')
                do kagain = 1,depth_max
                    write(8,102)(temp_SD_array(xagain,kagain),xagain = 1,station_num)
                end do

                open(9,file = filename_salav, status = 'replace')
                do kagain = 1,depth_max
                    write(9,102)(sal_avarray(xagain,kagain),xagain = 1,station_num)
                end do

                open(10,file = filename_salsd, status = 'replace')
                do kagain = 1,depth_max
                    write(10,102)(sal_SD_array(xagain,kagain),xagain = 1,station_num)
                end do
                
                close(7);close(8);close(9);close(10)
    end do                                          !end of m months
      
end do                                              !end of line_num 


end program

                


