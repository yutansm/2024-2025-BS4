program dataedit_attempt1
    implicit none
    integer,parameter::month=12,seasons=4,year_start=2008,year_max=15,station_y=2,station_x=9,depth_max=1000 !station_xは深層見れない時7
    !line_num=緯度別の観測、st_x=経度別の観測列、年、月、深さ、塩分または水温
    real,dimension(year_max,month,station_y,station_x,depth_max+100)::potemp_5,potemp_mdn,sal_5,sal_mdn
    real,dimension(depth_max+100)::mirror,datacolumn_org,origin
    character::yyyy*9,filename*999,newfilename*999,line_name*20,mm*9,months*9
    ! character(len=*), parameter::TempPass = 'Yuta_edit_potemp'
    character(len=*), parameter::SalPass = 'Yuta_unedited'
    character(len=*), parameter::AnotherPass = 'Yuta_edit_potemp_median'
    character(len=*), parameter::NewPass = 'Yuta_edit_median_potemp'

    integer::stations,m,line_num,y,x,k,ini_dep,year,loop,i,j,kagain,xagain,initial_depth,starting_depth
    real::numvault,median


102 format(9(f9.4))

do m=1,month
    write(mm,'(i2.2)') m
    do line_num = 1,2
        if (line_num==1) then
            line_name = 'S-Line'
        else 
            line_name = 'N-Line'
        end if
        do y = 1, year_max
            year = y + year_start
            write(yyyy,'(i4.4)') year
            
            filename = trim(SalPass)//'/'//trim(line_name)//'/'//trim(mm)//'/01sal'//trim(yyyy)//'.csv'
            open(31,file = filename, status = 'old', action = 'read')
            do k=1, depth_max
                read(31,102)(sal_5(y,m,line_num,x,k), x=1,station_x)
            end do
            close(31)
        end do
    end do
end do   !sal_5の配列にデータが入った
print*, sal_5(1,12,1,4,10)


do m = 1,month
    write(months,'(i2.2)')m
    do line_num = 1,2
        if (line_num ==1) then
            line_name = 'S-Line'
        else
            line_name = 'N-Line'
        end if
        do y = 1,year_max
            year = y + year_start
            write(yyyy,'(i4.4)') year
            do x = 1,station_x
                do k =1, depth_max
                    origin(k) = sal_5(y,m,line_num,x,k)
                    mirror(k) = sal_5(y,m,line_num,x,k)
                end do !end of depth
                    do kagain = 1, 50
                        if (origin(kagain) /= 0.) then
                            initial_depth = kagain
                            exit
                        else if (origin(50)==0.) then
                            initial_depth = 0
                        else
                        end if                                    ! 各station でのinitial_depthの検索
                    end do
                do loop = 1,depth_max
                    mirror(:) = origin(:)
                    starting_depth = initial_depth + loop -1
                    do i = starting_depth,starting_depth+49            !initial_depth 以降を並び替える    
                        do j = starting_depth +1, starting_depth +50
                            if (mirror(i) > mirror(j)) then
                                numvault = mirror(i)   !vaultに数字を一旦貯蓄
                                mirror(i) = mirror(j)
                                mirror(j) = numvault        !51この数字を小さいものから順に並べる
                            else 
                            end if
                        end do !end of j
                    end do     !end of i                                 !mirror配列の並び替え完了
                    median = mirror(starting_depth + 25)                    !最初の中央値は26こ目   
                    sal_mdn(y,m,line_num,x,starting_depth+25) = median  !メジアンを基の配列に入れる 
                end do          !end of loop  
            end do !end of x station_x
            filename = trim(NewPass)//'/'//trim(line_name)//'/'//trim(months)//'/'//'sal_51mdn'//trim(yyyy)//'.csv'
            ! print *, filename
            ! print*, mm
            open(7,file = filename, status = 'replace')
                do kagain = 1,1000
                    write(7,102)(sal_mdn(y,m,line_num,xagain,kagain), xagain = 1,station_x)
                end do   !新しいファイルがyyyy*mm*line_num 分作成されていることを願う限り
            !     filename = trim(AnotherPass)//'/'//trim(line_name)//'/'//trim(months)//'/'//'sal_51mdn'//trim(yyyy)//'.csv'
            ! ! print *, filename
            ! ! print*, mm
            ! open(7,file = filename, status = 'replace')
            !     do kagain = 1,400
            !         write(7,102)(sal_mdn(y,m,line_num,xagain,kagain), xagain = 1,station_x)
            !     end do   !新しいファイルがyyyy*mm*line_num 分作成されていることを願う限り
               
        end do !end of y years
    end do  !     end do !end of line num
end do  ! end do !end of m month

close(7)

end program


! do m = 12,12,1
!     write(months,'(i2.2)') m
!     do line_num = 1,1,1
!         if (line_num==1) then
!             line_name = 'S-Line'
!             ! stations = station_x-1
!         else
!             line_name = 'N-Line'
!             ! stations = station_x
!         end if

!             do y = 1,1,1
!                 year = y + year_start
!                 write(yyyy,'(i4.4)') year
!                 do x = 1, station_x
!                     do k = 1,depth_max+25    !datacolumn(425)まで存在　k = 400の値を算出するため
!                         datacolumn_org(k) = potemp_5(y,m,line_num,x,k)
!                         mirror(k) = potemp_5(y,m,line_num,x,k)
!                     end do !end of k depth
!                     ini_dep = 1
!                         do loop =1,depth_max-25      !400-25で十分 中央値計算                          1 to 375
!                             mirror(:) = datacolumn_org(:) 
!                             do i = ini_dep,ini_dep+49                                 !1 t0 50
!                                 do j = ini_dep +1,ini_dep +50                         !2 to 51
!                                     if (mirror(i) > mirror(j)) then
!                                         numvault = mirror(i)   !vaultに数字を一旦貯蓄
!                                         mirror(i) = mirror(j)
!                                         mirror(j) = numvault        !51この数字を小さいものから順に並べる
!                                     else 
!                                     end if
!                                 end do !end of j
!                             end do     !end of i                                 !datacolumnの並び替え完了
!                             median = mirror(ini_dep + 25)                     !最初の中央値は26こ目   
!                             potemp_5(y,m,line_num,x,ini_dep+25) = median  !メジアンを基の配列に入れる 
!                             ini_dep = ini_dep + 1       !2loop目はdatacolumn(2)から始まる     
!                         end do          !end of loop                            !400このデータが所属する区分の中央値に置き換わったと願いたい        
                        
!                 end do !end of x station_x
!                     filename = 'Yuta_edit'//'/'//trim(line_name)//'/'//trim(months)//'/'//'51potemp'//trim(yyyy)//'.csv'
!                     ! print *, filename
!                     ! print*, mm
!                     open(7,file = filename, status = 'replace')
!                         do kagain = 1,400
!                             write(7,102)(potemp_5(y,m,line_num,xagain,kagain), xagain = 1,station_x)
!                         end do   !新しいファイルがyyyy*mm*line_num 分作成されていることを願う限り
                       
!                 end do !end of y years
!             end do  !     end do !end of line num
!         end do  ! end do !end of m month

!         close(7)










