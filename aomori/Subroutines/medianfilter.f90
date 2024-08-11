program median_filter_25
    use ifport
    implicit none
    integer,parameter::years = 15, months = 12, lines = 2, stations = 9, depth = 1000 !station_xは深層見れない時7
    !line_num=緯度別の観測、st_x=経度別の観測列、年、月、深さ、塩分または水温
    real,dimension(years,months,lines,stations,depth)::temp_5,sal_5
    real,dimension(years,months,lines,stations,depth)::potemp25,sal25
    real,dimension(depth)::temp_st,sal_st
    ! real,dimension(depth_max+100)::mirror,datacolumn_org,origin
    character::filename*999,yyyy*9,mm*9,line*99,directory*999
    integer::y,m,l,st,d,initial_depth,starting_depth,i,j
    real::numvault,median
    logical(4) result

!     do l = 1,lines
!         if(l ==1) then;line = 'N-Line'
!         else;line = 'S-Line'
!         end if
!         result = makedirqq('/LARGE0/gr10291/nishimori2/aomori/Median_TempSal/'//trim(line))
!             do m = 1, months
!                 write(mm,'(i2.2)')m
!                 result = makedirqq('/LARGE0/gr10291/nishimori2/aomori/Median_TempSal/'//trim(line)//'/'//trim(mm))
!             end do
!     end do

call temsal(temp_5,sal_5)

102 format(9(f9.4))
! do l = 1, lines
!     if (l==1) then;line = 'N-Line'
!     else;line = 'S-Line'
!     end if
!     do y = 1,years
!         write(yyyy,'(i4.4)')y+2008
!         do m = 1,months
!             write(mm,'(i2.2)')m
!             do st = 1, stations
!                 temp_st(1:depth) = temp_5(y,m,l,st,1:depth)
!                 if (all(temp_st(1:50)==0.)) then
!                     initial_depth = 0
!                 else
!                     do d = 1,50
!                         if(temp_st(d)/=0.) then
!                             initial_depth = d;exit
!                         else;end if
!                     end do
!                 end if
!                 ! print*,st,'initial depth',initial_depth
!                 do d = 1,depth-25
!                     starting_depth = initial_depth + d-1
!                     do i = starting_depth,starting_depth+24
!                         do j = starting_depth+1,starting_depth+25
!                             if(temp_st(i)>temp_st(j)) then
!                                 numvault = temp_st(i)
!                                 temp_st(i) = temp_st(j);temp_st(j) = numvault
!                             else;end if
!                         end do
!                     end do !あるステーションについてそのd配列25個分が小さいものから順に並んだ
!                     median = temp_st(starting_depth+12)
!                     potemp25(y,m,l,st,starting_depth+12) = median
!                     temp_st(1:depth) = temp_5(y,m,l,st,1:depth)
!                 end do
!                 ! do d =1,200
!                 !  print*,m,st,d,potemp25(y,m,l,st,d)
!                 ! end do
!             end do !end of stations
!             filename = '/LARGE0/gr10291/nishimori2/aomori/Median_TempSal/'//trim(line)//'/'//trim(mm)//'/25temp'//trim(yyyy)//'.csv'
!             open(77,file = filename,status = 'replace')
!             do d = 1, depth
!                 write(77,102)(potemp25(y,m,l,st,d),st = 1, stations)
!             end do
!             close(77)
!         end do !end of months
!     end do ! end of years
! end do ! end of lines


! do l = 1, lines
!     if (l==1) then;line = 'N-Line'
!     else;line = 'S-Line'
!     end if
!     do y = 1,years
!         write(yyyy,'(i4.4)')y+2008
!         do m = 1,months
!             write(mm,'(i2.2)')m
!             do st = 1, stations
!                 sal_st(1:depth) = sal_5(y,m,l,st,1:depth)
!                 ! sal_origin(1:stations,1:depth) = sal_5(y,m,l,st,1:depth)
!                 if (all(sal_st(1:50)==0.)) then
!                     initial_depth = 0
!                 else
!                     do d = 1,50
!                         if(sal_st(d)/=0.) then
!                             initial_depth = d;exit
!                         else;end if
!                     end do
!                 end if
!                 print*,st,'initial depth',initial_depth
!                 do d = 1,depth-25
!                     starting_depth = initial_depth + d-1
!                     do i = starting_depth,starting_depth+24
!                         do j = starting_depth+1,starting_depth+25
!                             if(sal_st(i)>sal_st(j)) then
!                                 numvault = sal_st(i)
!                                 sal_st(i) = sal_st(j);sal_st(j) = numvault
!                             else;end if
!                         end do
!                     end do !あるステーションについてそのd配列25個分が小さいものから順に並んだ
!                     median = sal_st(starting_depth+12)
!                     sal25(y,m,l,st,starting_depth+12) = median
!                     sal_st(1:depth) = sal_5(y,m,l,st,1:depth)
!                 end do
!                 ! do d =1,200
!                 !  print*,m,st,d,potemp25(y,m,l,st,d)
!                 ! end do
!             end do !end of stations
!             filename = '/LARGE0/gr10291/nishimori2/aomori/Median_TempSal/'//trim(line)//'/'//trim(mm)//'/25sal'//trim(yyyy)//'.csv'
!             open(11,file = filename,status = 'replace')
!             do d = 1, depth
!                 write(11,102)(sal25(y,m,l,st,d),st = 1, stations)
!             end do
!             close(11)
!         end do !end of months
!     end do ! end of years
! end do ! end of lines


! 51db median filter

! do l = 1, lines
!     if (l==1) then;line = 'N-Line'
!     else;line = 'S-Line'
!     end if
!     do y = 1,years
!         write(yyyy,'(i4.4)')y+2008
!         do m = 1,months
!             write(mm,'(i2.2)')m
!             do st = 1, stations
!                 temp_st(1:depth) = temp_5(y,m,l,st,1:depth)
!                 if (all(temp_st(1:50)==0.)) then
!                     initial_depth = 0
!                 else
!                     do d = 1,50
!                         if(temp_st(d)/=0.) then
!                             initial_depth = d;exit
!                         else;end if
!                     end do
!                 end if
!                 ! print*,st,'initial depth',initial_depth
!                 do d = 1,depth-51
!                     starting_depth = initial_depth + d-1
!                     do i = starting_depth,starting_depth+50
!                         do j = starting_depth+1,starting_depth+51
!                             if(temp_st(i)>temp_st(j)) then
!                                 numvault = temp_st(i)
!                                 temp_st(i) = temp_st(j);temp_st(j) = numvault
!                             else;end if
!                         end do
!                     end do !あるステーションについてそのd配列25個分が小さいものから順に並んだ
!                     median = temp_st(starting_depth+25)
!                     potemp25(y,m,l,st,starting_depth+25) = median
!                     temp_st(1:depth) = temp_5(y,m,l,st,1:depth)
!                 end do
!                 ! do d =1,200
!                 !  print*,m,st,d,potemp25(y,m,l,st,d)
!                 ! end do
!             end do !end of stations
!             filename = '/LARGE0/gr10291/nishimori2/aomori/Median_TempSal/'//trim(line)//'/'//trim(mm)//'/51temp'//trim(yyyy)//'.csv'
!             open(77,file = filename,status = 'replace')
!             do d = 1, depth
!                 write(77,102)(potemp25(y,m,l,st,d),st = 1, stations)
!             end do
!             close(77)
!         end do !end of months
!     end do ! end of years
! end do ! end of lines

!temp もsal も25の名の配列に入れているけれど問題はない　ややこしいこと止めようね
do l = 1, lines
    if (l==1) then;line = 'N-Line'
    else;line = 'S-Line'
    end if
    do y = 1,years
        write(yyyy,'(i4.4)')y+2008
        do m = 1,months
            write(mm,'(i2.2)')m
            do st = 1, stations
                sal_st(1:depth) = sal_5(y,m,l,st,1:depth)
                ! sal_origin(1:stations,1:depth) = sal_5(y,m,l,st,1:depth)
                if (all(sal_st(1:50)==0.)) then
                    initial_depth = 0
                else
                    do d = 1,50
                        if(sal_st(d)/=0.) then
                            initial_depth = d;exit
                        else;end if
                    end do
                end if
                ! print*,st,'initial depth',initial_depth
                do d = 1,depth-51
                    starting_depth = initial_depth + d-1
                    do i = starting_depth,starting_depth+50
                        do j = starting_depth+1,starting_depth+51
                            if(sal_st(i)>sal_st(j)) then
                                numvault = sal_st(i)
                                sal_st(i) = sal_st(j);sal_st(j) = numvault
                            else;end if
                        end do
                    end do !あるステーションについてそのd配列25個分が小さいものから順に並んだ
                    median = sal_st(starting_depth+25)
                    sal25(y,m,l,st,starting_depth+25) = median
                    sal_st(1:depth) = sal_5(y,m,l,st,1:depth)
                end do
                ! do d =1,200
                !  print*,m,st,d,potemp25(y,m,l,st,d)
                ! end do
            end do !end of stations
            filename = '/LARGE0/gr10291/nishimori2/aomori/Median_TempSal/'//trim(line)//'/'//trim(mm)//'/51sal'//trim(yyyy)//'.csv'
            open(11,file = filename,status = 'replace')
            do d = 1, depth
                write(11,102)(sal25(y,m,l,st,d),st = 1, stations)
            end do
            close(11)
        end do !end of months
    end do ! end of years
end do ! end of lines

end program













