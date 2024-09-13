! creating temp_5,sal_5 using original data
subroutine temsal(temp_5,sal_5)
    implicit none
    integer,parameter::years = 15, months = 12,lines = 2, stations = 9,depth = 1000
    real,dimension(years,months,lines,stations,depth),intent(out)::temp_5,sal_5
    character::yyyy*9,tempfile*999,salfile*999,line*20,mm*9
    integer::y,m,l,st,d

    102 format(9(f9.4))
    do m = 1, months
        write(mm,'(i2.2)')m
        do l = 1,lines
            if(l==1) then;line = 'N-Line'
            else;line = 'S-Line';end if
                do y = 1, years
                    write(yyyy,'(i4.4)')y+2008
                    tempfile = '/Users/yuta/Desktop/nishimori2/aomori/Yuta_unedited/'//'/'//trim(line)//'/'//trim(mm)//'/01tem'//trim(yyyy)//'.csv'
                    salfile = '/Users/yuta/Desktop/nishimori2/aomori/Yuta_unedited/'//'/'//trim(line)//'/'//trim(mm)//'/01sal'//trim(yyyy)//'.csv'
                    open(11,file = tempfile,status = 'old', action = 'read')
                    do d = 1, depth
                        read(11,102)(temp_5(y,m,l,st,d),st = 1,stations)
                    end do
                    close(11)
                    open(77,file = salfile,status ='old',action = 'read')
                    do d = 1,depth
                        read(77,102)(sal_5(y,m,l,st,d),st = 1,stations)
                    end do
                    close(77)
                end do
        end do
    end do
end subroutine

! creating potemp and sal from 51db median filtered data
subroutine potempsal_51(potemp_5,sal_5)
    implicit none
    integer,parameter::years = 15, months = 12,lines = 2, stations = 9,depth = 400
    real,dimension(years,months,lines,stations,depth),intent(out)::potemp_5,sal_5
    character::yyyy*9,tempfile*999,salfile*999,line*20,mm*9
    integer::y,m,l,st,d

    102 format(9(f9.4))
    do m = 1, months
        write(mm,'(i2.2)')m
        do l = 1,lines
            if(l==1) then;line = 'N-Line'
            else;line = 'S-Line';end if
                do y = 1, years
                    write(yyyy,'(i4.4)')y+2008
                    tempfile = '../../aomori/51_Median'//'/'//trim(line)//'/'//trim(mm)//'/'//'51potemp'//trim(yyyy)//'.csv'  
                    salfile = '../../aomori/51_Median'//'/'//trim(line)//'/'//trim(mm)//'/51sal'//trim(yyyy)//'.csv'
                    open(11,file = tempfile,status = 'old', action = 'read')
                    do d = 1, depth
                        read(11,102)(potemp_5(y,m,l,st,d),st = 1,stations)
                    end do
                    close(11)
                    open(77,file = salfile,status ='old',action = 'read')
                    do d = 1,depth
                        read(77,102)(sal_5(y,m,l,st,d),st = 1,stations)
                    end do
                    close(77)
                end do
        end do
    end do
    
end subroutine

! creating potemp and sal from 25db median filtered data
subroutine potempsal_25(potemp_5,sal_5)
    implicit none
    integer,parameter::years = 15, months = 12,lines = 2, stations = 9,depth = 400
    real,dimension(years,months,lines,stations,depth),intent(out)::potemp_5,sal_5
    character::yyyy*9,tempfile*999,salfile*999,line*20,mm*9
    integer::y,m,l,st,d

    102 format(9(f9.4))
    do m = 1, months
        write(mm,'(i2.2)')m
        do l = 1,lines
            if(l==1) then;line = 'N-Line'
            else;line = 'S-Line';end if
                do y = 1, years
                    write(yyyy,'(i4.4)')y+2008
                    tempfile = '/Users/yuta/Desktop/nishimori2/aomori/25_Median'//'/'//trim(line)//'/'//trim(mm)//'/25potemp'//trim(yyyy)//'.csv'
                    salfile = '/Users/yuta/Desktop/nishimori2/aomori/25_Median'//'/'//trim(line)//'/'//trim(mm)//'/25sal'//trim(yyyy)//'.csv'
                    open(11,file = tempfile,status = 'old', action = 'read')
                    do d = 1, depth
                        read(11,102)(potemp_5(y,m,l,st,d),st = 1,stations)
                    end do
                    close(11)
                    open(77,file = salfile,status ='old',action = 'read')
                    do d = 1,depth
                        read(77,102)(sal_5(y,m,l,st,d),st = 1,stations)
                    end do
                    close(77)
                end do
        end do
    end do
    
end subroutine



                                            !ABOVE DATA IS NOT REALLY FOR USE!

                                            ! SUBROUTINES FOR DATA OBTAINMENT !

! from a rectangle numeric file to a 2D array
subroutine csv2array(filename,format,column_quan,row_quan,twoD_array)
    implicit none
    real,dimension(column_quan,row_quan),intent(out)::twoD_array
    integer,intent(in)::column_quan,row_quan
    character,intent(in)::filename*999,format*99
    integer::c,r,ios

    open(11,file = filename,status = 'old',action='read')
    do r = 1, row_quan
        read(11,format,iostat=ios)(twoD_array(c,r),c = 1, column_quan)
        if(ios /= 0) then
            exit
        else;end if
    end do
    close(11)
end subroutine

! calibrated data for potempsal_51
subroutine calibrated_data51(potemp_c5,sal_c5)
    implicit none
    integer,parameter:: years = 15, months = 12, lines = 2, stations = 9, depth = 400
    real,dimension(:,:,:,:,:),allocatable::potemp_5,sal_5
    real,dimension(years,months,lines,stations,depth),intent(out)::potemp_c5,sal_c5
    real,parameter::standard_sal_400 = 34.07
    real::diff,initial_num
    integer::y,m,l,st,d,initial_depth

    allocate(potemp_5(years,months,lines,stations,depth))
    allocate(sal_5(years,months,lines,stations,depth))
    potemp_5 = 0.;sal_5 = 0.
    call potempsal_51(potemp_5,sal_5)
    sal_5(1,12,2,5,1:depth)=0.;sal_5(1,12,2,6,1:depth)=0.;sal_5(1,12,2,7,1:depth)=0.!May 10 金サロ前 Sline dgf
    sal_5(2,4,1,4,1:depth)=0. ! 2010 april Nline station 4 erased
    sal_5(4,1:months,1:lines,1:stations,1:depth)=0. !2012 data all erased

    potemp_5(1,12,2,5,1:depth)=0.;potemp_5(1,12,2,6,1:depth)=0.;potemp_5(1,12,2,7,1:depth)=0.!May 10 金サロ前 Sline dgf
    potemp_5(2,4,1,4,1:depth)=0. ! 2010 april Nline station 4 erased
    potemp_5(4,1:months,1:lines,1:stations,1:depth)=0. !2012 data all erased

  !データが最初に存在する深さの数値を表面まで持ってくる。
    do y = 1, years 
        do m = 1, months
            do l = 1, lines
                do st = 1,stations
                    if (potemp_5(y,m,l,st,50)/=0.) then
                        do d = 1,50
                            if (potemp_5(y,m,l,st,d) /= 0.) then
                                initial_depth = d
                                initial_num = potemp_5(y,m,l,st,d)
                                exit
                            else;end if
                        end do
                    potemp_5(y,m,l,st,1:initial_depth-1) = initial_num
                    else !(if potemp_5(y,m,l,st,50)==0.)
                    end if
                    if (sal_5(y,m,l,st,50)/=0.) then
                        do d = 1,50
                            if (sal_5(y,m,l,st,d) /= 0.) then
                                initial_depth = d
                                initial_num = sal_5(y,m,l,st,d)
                                exit
                            else;end if
                        end do
                    sal_5(y,m,l,st,1:initial_depth-1) = initial_num
                    else !(if sal_5(y,m,l,st,50)==0.)
                    end if
                end do
            end do
        end do
    end do

  !底の塩分を全ての月において34.07にしてしまう
  !2023 august station 9 はデータが348までしかないので以下のプログラムにより欠測になる
    do y = 1, years
        do m = 1,months
            do l = 1,lines
                do st = 1, stations
                    if (sal_5(y,m,l,st,depth)<30. .or. sal_5(y,m,l,st,depth)>35.) then
                        sal_5(y,m,l,st,1:depth) = 0.  !一列全部0 データが元から無い列も0だからこれに含まれる
                        potemp_5(y,m,l,st,1:depth) = 0.
                    else
                        diff = sal_5(y,m,l,st,depth) - standard_sal_400 
                    end if
                    do d = 1, depth
                        if(sal_5(y,m,l,st,d)/=0.) then
                        sal_5(y,m,l,st,d) = sal_5(y,m,l,st,d) - diff
                        else; end if
                    end do
                    !diff = 0.
                end do 
            end do
        end do
    end do

    potemp_c5(1:years,1:months,1:lines,1:stations,1:depth) = potemp_5(1:years,1:months,1:lines,1:stations,1:depth)
    sal_c5(1:years,1:months,1:lines,1:stations,1:depth) = sal_5(1:years,1:months,1:lines,1:stations,1:depth)
    deallocate(potemp_5);deallocate(sal_5)
    
end subroutine

! calibrated_data for potempsal_25
subroutine calibrated_data25(potemp_c5,sal_c5)
    implicit none
    integer,parameter:: years = 15, months = 12, lines = 2, stations = 9, depth = 400
    real,dimension(:,:,:,:,:),allocatable::potemp_5,sal_5
    real,dimension(years,months,lines,stations,depth),intent(out)::potemp_c5,sal_c5
    real,parameter::standard_sal_400 = 34.07
    real::diff,initial_num
    integer::y,m,l,st,d,initial_depth

    allocate(potemp_5(years,months,lines,stations,depth))
    allocate(sal_5(years,months,lines,stations,depth))
    potemp_5 = 0.;sal_5 = 0.
    call potempsal_25(potemp_5,sal_5)
    sal_5(1,12,2,5,1:depth)=0.;sal_5(1,12,2,6,1:depth)=0.;sal_5(1,12,2,7,1:depth)=0.!May 10 金サロ前 Sline dgf
    sal_5(2,4,1,4,1:depth)=0. ! 2010 april Nline station 4 erased
    sal_5(4,1:months,1:lines,1:stations,1:depth)=0. 

    potemp_5(1,12,2,5,1:depth)=0.;potemp_5(1,12,2,6,1:depth)=0.;potemp_5(1,12,2,7,1:depth)=0.!May 10 金サロ前 Sline dgf
    potemp_5(2,4,1,4,1:depth)=0. ! 2010 april Nline station 4 erased
    potemp_5(4,1:months,1:lines,1:stations,1:depth)=0. !2012 data all erased

    sal_5(5,5,2,6,272:279) = 34.0304    !途中で0が続く変な時があるSline 前後の値と同じにした

  !データが最初に存在する深さの数値を表面まで持ってくる。
    do y = 1, years 
        do m = 1, months
            do l = 1, lines
                do st = 1,stations
                    if (potemp_5(y,m,l,st,50)/=0.) then
                        do d = 1,50
                            if (potemp_5(y,m,l,st,d) /= 0.) then
                                initial_depth = d
                                initial_num = potemp_5(y,m,l,st,d)
                                exit
                            else;end if
                        end do
                    potemp_5(y,m,l,st,1:initial_depth-1) = initial_num
                    else !(if potemp_5(y,m,l,st,50)==0.)
                    end if
                    if (sal_5(y,m,l,st,50)/=0.) then
                        do d = 1,50
                            if (sal_5(y,m,l,st,d) /= 0.) then
                                initial_depth = d
                                initial_num = sal_5(y,m,l,st,d)
                                exit
                            else;end if
                        end do
                    sal_5(y,m,l,st,1:initial_depth-1) = initial_num
                    else !(if sal_5(y,m,l,st,50)==0.)
                    end if
                end do
            end do
        end do
    end do

  !底の塩分を全ての月において34.07としてしまう。
  !2023 august station 9 はデータが348までしかないので以下のプログラムにより欠測になる
    do y = 1, years
        do m = 1,months
            do l = 1,lines
                do st = 1, stations
                    if (sal_5(y,m,l,st,depth)<30. .or. sal_5(y,m,l,st,depth)>35.) then
                        sal_5(y,m,l,st,1:depth) = 0.  !一列全部0 データが元から無い列も0だからこれに含まれる
                        potemp_5(y,m,l,st,1:depth) = 0.
                    else
                        diff = sal_5(y,m,l,st,depth) - standard_sal_400 
                    end if
                    do d = 1, depth
                        if(sal_5(y,m,l,st,d)/=0.) then
                        sal_5(y,m,l,st,d) = sal_5(y,m,l,st,d) - diff
                        else; end if
                    end do
                    !diff = 0.
                end do 
            end do
        end do
    end do

    potemp_c5(1:years,1:months,1:lines,1:stations,1:depth) = potemp_5(1:years,1:months,1:lines,1:stations,1:depth)
    sal_c5(1:years,1:months,1:lines,1:stations,1:depth) = sal_5(1:years,1:months,1:lines,1:stations,1:depth)
    deallocate(potemp_5);deallocate(sal_5)
    
end subroutine

! put geostrophic velocity into an array for 25 or 51db median filtered data
subroutine geovel_array(medianfiltertype,geovel_5) 
    implicit none
    integer,parameter::years = 15, months = 12, lines = 2, stations = 9, depth = 400
    real,dimension(years,months,lines,stations,depth),intent(out)::geovel_5
    integer,intent(in)::medianfiltertype !25 or 51
    integer::y,m,l,st,d
    character::mm*9,aaaa*9,filename*999,line*9

    102 format(9(f9.4))
    if (medianfiltertype == 25) then
        do l = 1,lines
            if(l==1) then;line = 'N-Line';else; line='S-Line';end if
            do m = 1, months
                write(mm,'(i2.2)')m
                do y = 1, years
                    write(aaaa,'(i4.4)')y+2008
                    filename = '/Users/yuta/Desktop/nishimori2/aomori/Geostrophy/'//trim(line)//'/'//trim(mm)//'/Velocity_25median'//trim(aaaa)//'.csv'
                    open(77,file = filename,status = 'old',action = 'read')
                    do d = 1,depth
                        read(77,102)(geovel_5(y,m,l,st,d),st = 1,stations)
                    end do
                end do
            end do
        end do
    else if(medianfiltertype == 51) then
        do l = 1,lines
            if(l==1) then;line = 'N-Line';else; line='S-Line';end if
            do m = 1, months
                write(mm,'(i2.2)')m
                do y = 1, years
                    write(aaaa,'(i4.4)')y+2008
                    filename = '/Users/yuta/Desktop/nishimori2/aomori/Geostrophy/'//trim(line)//'/'//trim(mm)//'/Velocity_51median'//trim(aaaa)//'.csv'
                    open(77,file = filename,status = 'old',action = 'read')
                    do d = 1,depth
                        read(77,102)(geovel_5(y,m,l,st,d),st = 1,stations)
                    end do
                end do
            end do
        end do
    else;print*,'select either 25 or 51 for type of median filter'
    end if

end subroutine

! monthly geostrophic transport only through Station1-6(4-9)
subroutine geotransport(medianfiltertype,geotrans)
    implicit none
    integer,parameter::years = 15, months = 12, lines = 2
    real,dimension(years,months,lines),intent(out)::geotrans
    integer,intent(in)::medianfiltertype
    integer::y,m,l
    character::yyyy*9,filename*999,mm*9,line*9

    
    102 format(f9.4)
    if (medianfiltertype == 25) then
        do l = 1, lines
            if(l==1) then;line = 'N-Line';else; line='S-Line';end if
            do m = 1, months
                write(mm,'(i2.2)')m
                do y = 1, years
                    write(yyyy,'(i4.4)')y+2008
                    filename = '/Users/yuta/Desktop/nishimori2/aomori/Geostrophy/'//trim(line)//'/'//trim(mm)//'/Transport_25Median'//trim(yyyy)//'.csv'
                    open(21,file = filename,status = 'old',action = 'read')
                        read(21,102)geotrans(y,m,l)
                    close(21)
                end do
            end do
        end do
    else if(medianfiltertype == 51) then
        do l = 1, lines
            if(l==1) then;line = 'N-Line';else; line='S-Line';end if
            do m = 1, months
                write(mm,'(i2.2)')m
                do y = 1, years
                    write(yyyy,'(i4.4)')y+2008
                    filename = '/Users/yuta/Desktop/nishimori2/aomori/Geostrophy/'//trim(line)//'/'//trim(mm)//'/Transport_51Median'//trim(yyyy)//'.csv'
                    open(31,file = filename,status = 'old',action = 'read')
                        read(31,102)geotrans(y,m,l)
                    close(31)
                end do
            end do
        end do
    else;print*,'select either 25 or 51 for type of median filter'
    end if

end subroutine

! gives an array of monthly mean sea surface height and pressure in Fukaura 2009-2023
subroutine fukauraSSH(SSH_array,SSP_array)
    implicit none
    integer,parameter::years = 15, months = 12
    real,dimension(years,months),intent(out)::SSH_array,SSP_array
    integer::y,m,ios,yyyy,mm,nodata
    real::SSH,SSP
    character::filename*999

    ! 102 format((i4),(i2.2),(f9.4))
    ! 103 format((i4),(i2.2),(f9.4),(i2),(f9.4))
    filename = '/Users/yuta/Desktop/nishimori2/aomori/SSH/Fukaura_Tides.csv'
    open(92,file = filename, status = 'old',action = 'read')
    do y = 1,years
        do m = 1,12
            read(92,*,iostat = ios)yyyy,mm,SSH,nodata,SSP
            ! print*,yyyy,mm,SSH,nodata,SSP
            SSH_array(y,m) = SSH;SSP_array(y,m) = SSP
        end do
    end do
    close(92)
    do y = 1, years
        do m = 1, months
            if(SSH_array(y,m)/=0.) then
                SSH_array(y,m) = SSH_array(y,m) + (SSP_array(y,m)-10130)
            else;end if 
        end do
    end do
end subroutine

subroutine calibrated_fukauraSSH(calibrated_SSH_array)
    implicit none
    integer,parameter::years = 15, months = 12
    real,dimension(years,months),intent(out)::calibrated_SSH_array
    real,dimension(years,months)::SSH_array=0.,SSP_array=0.
    integer::y,m,ios,yyyy,mm,nodata
    real::SSH,SSP
    character::filename*999

    ! 102 format((i4),(i2.2),(f9.4))
    ! 103 format((i4),(i2.2),(f9.4),(i2),(f9.4))
    filename = '../SSH/Fukaura_Tides.csv'
    open(92,file = filename, status = 'old',action = 'read')
    do y = 1,years
        do m = 1,12
            read(92,*,iostat = ios)yyyy,mm,SSH,nodata,SSP
            ! print*,yyyy,mm,SSH,nodata,SSP
            SSH_array(y,m) = SSH;SSP_array(y,m) = SSP
        end do
    end do
    close(92)
    do y = 1,years
        do m = 1,months
            if(SSH_array(y,m)/=0.) then
                calibrated_SSH_array(y,m) = SSH_array(y,m) + (SSP_array(y,m)-10130)
            else;calibrated_SSH_array(y,m) = 0.
            end if
        end do
    end do

end subroutine

! SSH and SSP at Tappi
subroutine tappiSSH(SSH_array,SSP_array)
    implicit none
    integer,parameter::years = 15, months = 12
    real,dimension(years,months),intent(out)::SSH_array,SSP_array
    integer::y,m,ios,yyyy,mm,nodata
    real::SSH,SSP
    character::filename*999

    ! 102 format((i4),(i2.2),(f9.4))
    ! 103 format((i4),(i2.2),(f9.4),(i2),(f9.4))
    filename = '/Users/yuta/Desktop/nishimori2/aomori/SSH/Tappi_Tides.csv'
    open(92,file = filename, status = 'old',action = 'read')
    do y = 1,years
        do m = 1,12
            read(92,*,iostat = ios)yyyy,mm,SSH,nodata,SSP
            ! print*,yyyy,mm,SSH,nodata,SSP
            SSH_array(y,m) = SSH;SSP_array(y,m) = SSP
        end do
    end do
    close(92)
    do y = 1, years
        do m = 1, months
            if(SSH_array(y,m)/=0.) then
                SSH_array(y,m) = SSH_array(y,m) + (SSP_array(y,m)-10130)
            else;end if 
        end do
    end do
end subroutine

subroutine calibrated_tappiSSH(calibrated_SSH_array)
    implicit none
    integer,parameter::years = 15, months = 12
    real,dimension(years,months),intent(out)::calibrated_SSH_array
    real,dimension(years,months)::SSH_array=0.,SSP_array=0.
    integer::y,m,ios,yyyy,mm,nodata
    real::SSH,SSP
    character::filename*999

    ! 102 format((i4),(i2.2),(f9.4))
    ! 103 format((i4),(i2.2),(f9.4),(i2),(f9.4))
    filename = '../SSH/Tappi_Tides.csv'
    open(92,file = filename, status = 'old',action = 'read')
    do y = 1,years
        do m = 1,12
            read(92,*,iostat = ios)yyyy,mm,SSH,nodata,SSP
            ! print*,yyyy,mm,SSH,nodata,SSP
            SSH_array(y,m) = SSH;SSP_array(y,m) = SSP
        end do
    end do
    close(92)
    do y = 1,years
        do m = 1,months
            if(SSH_array(y,m)/=0.) then
                calibrated_SSH_array(y,m) = SSH_array(y,m) + (SSP_array(y,m)-10130)
            else;calibrated_SSH_array(y,m) = 0.
            end if
        end do
    end do

end subroutine

                                            ! SUBROUTINES FOR DATA OBTAINMENT !

                                            ! SUBROUTINES FOR DATA MANIPULATION !

! getting monthly average and sd from an array, (years,months,lines,stations,depth) to (months,lines,stations,depth) single precision
subroutine avsd_dataquan(input_array,average_array,sd_array,dataquan_array)
    implicit none
    integer,parameter::years=15, months=12, lines=2, stations = 9, depth = 400
    real,dimension(years,months,lines,stations,depth),intent(in)::input_array
    real,dimension(months,lines,stations,depth),intent(out)::average_array,sd_array
    integer,dimension(months,lines,stations,depth),intent(out)::dataquan_array
    real::sum=0.,first_sum=0.,average,variance,SD,devsq_sum=0.,first_devsq=0.
    integer::y,m,l,st,d,data_quan=0

    !データの選別はこのサブルーチンを通す前に行う。これは平均と標準偏差を計算してくれるだけ。後全年データ数
    do l = 1,lines
        do m = 1, months 
            do st = 1, stations
                do d = 1, depth
                    do y = 1, years
                        if (input_array(y,m,l,st,d)/=0.) then
                            sum = first_sum + input_array(y,m,l,st,d)
                            first_sum = sum
                            data_quan = data_quan +1
                        else;end if
                    end do
                    dataquan_array(m,l,st,d) = data_quan
                    if (data_quan/=0) then
                        average = sum/real(data_quan)
                        do y = 1, years !loop to get SD
                            if (input_array(y,m,l,st,d)/=0.) then
                                devsq_sum = first_devsq + (input_array(y,m,l,st,d) - average)**2.
                                first_devsq = devsq_sum
                            else;end if
                        end do
                        variance = devsq_sum/real(data_quan -1)
                        SD = sqrt(variance)
                        average_array(m,l,st,d) = average
                        sd_array(m,l,st,d) = SD
                    else;average = 0.;SD = 0.;average_array(m,l,st,d) = 0.;sd_array(m,l,st,d) = 0.
                    end if
                    first_sum = 0.
                    data_quan = 0
                    first_devsq = 0.
                end do
            end do
        end do
    end do


end subroutine

! getting monthly average and sd from (years,months,lines) to array(months,lines) single precision for geostrophic transport
subroutine avsd_dataquan2(input_array,average_array,sd_array,dataquan_array)
    implicit none
    integer,parameter::years=15, months=12, lines=2
    real,dimension(years,months,lines),intent(in)::input_array
    real,dimension(months,lines),intent(out)::average_array,sd_array
    integer,dimension(months,lines),intent(out)::dataquan_array !データ数は月々
    real::sum=0.,first_sum=0.,average,variance,SD,devsq_sum=0.,first_devsq=0.
    integer::y,m,l,data_quan=0


    !データの選別はこのサブルーチンを通す前に行う。これは平均と標準偏差を計算してくれるだけ。後全年データ数
    do l = 1,lines
        do m = 1, months 
            do y = 1, years
                if (input_array(y,m,l)/=0.) then
                    sum = first_sum + input_array(y,m,l)
                    first_sum = sum
                    data_quan = data_quan +1
                else;end if
            end do
                dataquan_array(m,l) = data_quan
                if (data_quan/=0) then
                    average = sum/real(data_quan)
                    do y = 1, years !loop to get SD
                        if (input_array(y,m,l)/=0.) then
                            devsq_sum = first_devsq + (input_array(y,m,l) - average)**2.
                            first_devsq = devsq_sum
                        else;end if
                    end do
                        variance = devsq_sum/real(data_quan -1)
                        SD = sqrt(variance)
                        average_array(m,l) = average
                        sd_array(m,l) = SD
                else;average = 0.;SD = 0.;average_array(m,l) = 0.;sd_array(m,l) = 0.
                end if
                first_sum = 0.
                data_quan = 0
                first_devsq = 0.
        end do
    end do

end subroutine

! getting monthly average and (an estimate of) standard error of the mean(SEM). 5D to 4D 
subroutine avsem_dataquan(input_array,average_array,sem_array,dataquan_array)
    implicit none
    integer,parameter::years=15, months=12, lines=2, stations = 9, depth = 400
    real,dimension(years,months,lines,stations,depth),intent(in)::input_array
    real,dimension(months,lines,stations,depth),intent(out)::average_array,sem_array
    integer,dimension(months,lines,stations,depth),intent(out)::dataquan_array
    real::sum,first_sum=0.,average,variance,SEM,devsq_sum,first_devsq=0.
    integer::y,m,l,st,d,data_quan=0

    !データの選別はこのサブルーチンを通す前に行う。これは平均と標準誤差を計算してくれるだけ。後全年データ数
    do l = 1,lines
        do m = 1, months 
            do st = 1, stations
                do d = 1, depth
                    do y = 1, years
                        if (input_array(y,m,l,st,d)/=0.) then
                            sum = first_sum + input_array(y,m,l,st,d)
                            first_sum = sum
                            data_quan = data_quan +1
                        else;end if
                    end do
                    dataquan_array(m,l,st,d) = data_quan
                    if (data_quan/=0) then
                        average = sum/real(data_quan)
                        do y = 1, years !loop to get SEM
                            if (input_array(y,m,l,st,d)/=0.) then
                                devsq_sum = first_devsq + (input_array(y,m,l,st,d) - average)**2.
                                first_devsq = devsq_sum
                            else;end if
                        end do
                        variance = devsq_sum/real(data_quan -1)/real(data_quan) !variance of means in this case
                        SEM = sqrt(variance)
                        average_array(m,l,st,d) = average
                        sem_array(m,l,st,d) = SEM
                    else;average = 0.;SEM = 0.;average_array(m,l,st,d) = 0.;sem_array(m,l,st,d) = 0.
                    end if
                    first_sum = 0.
                    data_quan = 0
                    first_devsq = 0.
                end do
            end do
        end do
    end do

end subroutine
! array(years,months) to array(months)
subroutine avsem_dataquan3(input_array,average_array,sem_array,dataquan_array)
    implicit none
    integer,parameter::years=15, months=12
    real,dimension(years,months),intent(in)::input_array 
    real,dimension(months),intent(out)::average_array,sem_array
    integer,dimension(months),intent(out)::dataquan_array !�f�[�^���͌��X
    real::sum,first_sum=0.,average,variance,SEM,devsq_sum,first_devsq=0.
    integer::y,m,data_quan=0


    !�f�[�^�̑I�ʂ͂��̃T�u���[�`����ʂ��O�ɍs���B����͕��ςƕW���΍����v�Z���Ă���邾���B��S�N�f�[�^��
        do m = 1, months 
            do y = 1, years
                if (input_array(y,m)/=0.) then
                    sum = first_sum + input_array(y,m)
                    first_sum = sum
                    data_quan = data_quan +1
                else;end if
            end do
                dataquan_array(m) = data_quan
                if (data_quan/=0) then
                    average = sum/real(data_quan)
                    do y = 1, years !loop to get SD
                        if (input_array(y,m)/=0.) then
                            devsq_sum = first_devsq + (input_array(y,m) - average)**2.
                            first_devsq = devsq_sum
                        else;end if
                    end do
                    variance = devsq_sum/real(data_quan -1)/real(data_quan) !variance of means in this case
                    SEM = sqrt(variance)
                    average_array(m) = average
                    sem_array(m) = SEM
                else;average = 0.;SEM = 0.;average_array(m) = 0.;sem_array(m) = 0.
                end if
                first_sum = 0.
                data_quan = 0
                first_devsq = 0.
        end do
end subroutine

! Is a fusion of avsd_dataquan and avsem_dataquan
subroutine avsdsem_dataquan(input_array,average_array,sd_array,sem_array,dataquan_array)
    implicit none
    integer,parameter::years=15, months=12, lines=2, stations = 9, depth = 400
    real,dimension(years,months,lines,stations,depth),intent(in)::input_array
    real,dimension(months,lines,stations,depth),intent(out)::average_array,sd_array,sem_array
    integer,dimension(months,lines,stations,depth),intent(out)::dataquan_array
    real::sum,first_sum=0.,average,variance,SD,SEM,devsq_sum,first_devsq=0.
    integer::y,m,l,st,d,data_quan=0

    !データの選別はこのサブルーチンを通す前に行う。これは平均と標準偏差と標準誤差を計算してくれるだけ。後全年データ数
    do l = 1,lines
        do m = 1, months 
            do st = 1, stations
                do d = 1, depth
                    do y = 1, years
                        if (input_array(y,m,l,st,d)/=0.) then
                            sum = first_sum + input_array(y,m,l,st,d)
                            first_sum = sum
                            data_quan = data_quan +1
                        else;end if
                    end do
                    dataquan_array(m,l,st,d) = data_quan
                    if (data_quan/=0) then
                        average = sum/real(data_quan)
                        do y = 1, years !loop to get SD
                            if (input_array(y,m,l,st,d)/=0.) then
                                devsq_sum = first_devsq + (input_array(y,m,l,st,d) - average)**2.
                                first_devsq = devsq_sum
                            else;end if
                        end do
                        variance = devsq_sum/real(data_quan -1)  ! sample variance
                        SD = sqrt(variance) ! sample standard deviation
                        SEM = SD/sqrt(real(data_quan)) ! standard deviation of means or standard error of the mean
                        average_array(m,l,st,d) = average
                        sd_array(m,l,st,d) = SD
                        sem_array(m,l,st,d) = SEM
                    else;average_array(m,l,st,d) = 0.;sd_array(m,l,st,d) = 0.;sem_array(m,l,st,d) = 0.
                    end if
                    first_sum = 0.
                    data_quan = 0
                    first_devsq = 0.
                end do
            end do
        end do
    end do

end subroutine

! double precision version of avsd_dataquan
subroutine dp_avsd_dataquan(input_array,average_array,sd_array,dataquan_array)
    implicit none
    integer,parameter::years=15, months=12, lines=2, stations = 9, depth = 400
    double precision,intent(in)::input_array(years,months,lines,stations,depth)
    double precision,intent(out)::average_array(months,lines,stations,depth)
    double precision,intent(out)::sd_array(months,lines,stations,depth)
    integer,dimension(months,lines,stations,depth),intent(out)::dataquan_array
    ! real::sum,first_sum,data_quan,average,variance,SD,devsq_sum,first_devsq
    double precision:: sum; double precision::first_sum=0.0d0; double precision::average
    double precision:: variance; double precision:: SD; double precision::devsq_sum; double precision:: first_devsq=0.0d0
    ! double precision::one
    integer::y,m,l,st,d,data_quan=0


    !データの選別はこのサブルーチンを通す前に行う。これは平均と標準偏差を計算してくれるだけ。後全年データ数
    do l = 1,lines
        do m = 1, months 
            do st = 1, stations
                do d = 1, depth
                    do y = 1, years
                        if (input_array(y,m,l,st,d)/=0.) then
                            sum = first_sum + input_array(y,m,l,st,d)
                            first_sum = sum
                            data_quan = data_quan +1
                        else;end if
                    end do
                    dataquan_array(m,l,st,d) = data_quan
                    if (data_quan/=0) then
                        average = sum/real(data_quan)
                        do y = 1, years !loop to get SD
                            if (input_array(y,m,l,st,d)/=0.) then
                                devsq_sum = first_devsq + (input_array(y,m,l,st,d) - average)**2.
                                first_devsq = devsq_sum
                            else;end if
                        end do
                        variance = devsq_sum/real(data_quan -1)
                        SD = sqrt(variance)
                        average_array(m,l,st,d) = average
                        sd_array(m,l,st,d) = SD
                    else;average = 0.;SD = 0.;average_array(m,l,st,d) = 0.;sd_array(m,l,st,d) = 0.
                    end if
                    first_sum = 0.
                    data_quan = 0
                    first_devsq = 0.
                end do
            end do
        end do
    end do


end subroutine

! double precision version of avsd_dataquan2
subroutine dp_avsd_dataquan2(input_array,average_array,sd_array,dataquan_array)
    implicit none
    integer,parameter::years=15, months=12, lines=2
    double precision,intent(in)::input_array(years,months,lines)
    double precision,intent(out)::average_array(months,lines)
    double precision,intent(out)::sd_array(months,lines)
    integer,dimension(months,lines),intent(out)::dataquan_array !データ数は月々
    ! real::sum,first_sum,data_quan,average,variance,SD,devsq_sum,first_devsq
    double precision:: sum; double precision::first_sum=0.0d0; double precision::average
    double precision:: variance; double precision:: SD; double precision::devsq_sum; double precision:: first_devsq=0.0d0
    ! double precision::one
    integer::y,m,l,data_quan=0


    !データの選別はこのサブルーチンを通す前に行う。これは平均と標準偏差を計算してくれるだけ。後全年データ数
    do l = 1,lines
        do m = 1, months 
            do y = 1, years
                if (input_array(y,m,l)/=0.) then
                    sum = first_sum + input_array(y,m,l)
                    first_sum = sum
                    data_quan = data_quan +1
                else;end if
            end do
                dataquan_array(m,l) = data_quan
                if (data_quan/=0) then
                    average = sum/real(data_quan)
                    do y = 1, years !loop to get SD
                        if (input_array(y,m,l)/=0.) then
                            devsq_sum = first_devsq + (input_array(y,m,l) - average)**2.
                            first_devsq = devsq_sum
                        else;end if
                    end do
                        variance = devsq_sum/real(data_quan -1)
                        SD = sqrt(variance)
                        average_array(m,l) = average
                        sd_array(m,l) = SD
                else;average = 0.;SD = 0.;average_array(m,l) = 0.;sd_array(m,l) = 0.
                end if
                first_sum = 0.
                data_quan = 0
                first_devsq = 0.
        end do
    end do

end subroutine

! creating sigma array from potemp_5 and sal_5
subroutine create_sigma_array(temp_array,sal_array,sigma_array)
    implicit none
    integer,parameter::years = 15, months = 12, lines = 2, stations = 9, depth = 400
    real,dimension(years,months,lines,stations,depth),intent(in)::temp_array,sal_array
    real,dimension(years,months,lines,stations,depth),intent(out)::sigma_array
    real::sigma
    integer::y,m,l,st,d

    do y = 1,years
        do m = 1, months
            do l = 1, lines
                do st = 1, stations
                    do d = 1, depth
                        call sigma_T_S(sigma,temp_array(y,m,l,st,d),sal_array(y,m,l,st,d))
                        sigma_array(y,m,l,st,d) = sigma
                    end do
                end do
            end do
        end do
    end do
end subroutine

! creating an array of dynamic height from sigma array. 
subroutine create_DH_array(sigma_array,DH_array)
    implicit none
    integer,parameter:: years = 15, months = 12, lines = 2, stations = 9, depth = 400
    real,dimension(years,months,lines,stations,depth),intent(in)::sigma_array
    real,dimension(years,months,lines,stations,depth),intent(out)::DH_array
    integer::y,m,l,st,d
    double precision::hiyou
    double precision::zero;double precision::one;double precision::four;double precision::ten;double precision::thousand
    double precision::firstsum;double precision::DH;double precision::g
    one = 1.0d0;four = 4.0d0;ten = 10.0d0;thousand = 1000.0d0; g = 9.8d0

    do y = 1, years
        do m = 1, months
            do l = 1, lines
                do st = 1,stations
                    do d = 1, depth
                        if(sigma_array(y,m,l,st,d)/=0.)then
                            hiyou = one/((thousand+sigma_array(y,m,l,st,d))) !m^3/kg
                        else;hiyou = zero
                        end if
                        DH = firstsum + hiyou*ten**(four)/g
                        firstsum = DH
                        DH_array(y,m,l,st,d) = real(DH)
                    end do
                    firstsum = zero
                end do
            end do
        end do
    end do

end subroutine

! for calculating potential density as sigma
subroutine sigma_T_S(sigma,potemp,sal)

    real,intent(in)::potemp,sal
    real,intent(out)::sigma
    !密度計算
            !!!Keisan Parameter
        double precision::a0,a1,a2,a3,a4,a5
        double precision::b0,b1,b2,b3,b4
        double precision::c0,c1,c2,c3
        double precision::d0,d1
        double precision::e0,e1,e2,e3,e4
        double precision::f0,f1,f2,f3
        double precision::g0,g1,g2
        double precision::h0,h1,h2,h3
        double precision::i0,i1,i2
        double precision::j0
        double precision::k0,k1,k2
        double precision::m0,m1,m2
        double precision::S,t,pp,rho,rhow,KK,Kt,AAAA,BB,Kw,Aw,Bw,rhoafter
        ! double precision::q,Gamma,Theta,xk
    
    
    
    !密度パラメータ
        a0=3.5803E-5
        a1=8.5258E-6
        a2=-6.8360E-8
        a3=6.6228E-10
        b0=1.8932E-6
        b1=-4.2393E-8
        c0=1.8741E-8
        c1=-6.7795E-10
        c2=8.733E-12
        c3=-5.4481E-14
        d0=-1.1351E-10
        d1=2.7759E-12
        e0=-4.6206E-13
        e1=1.8676E-14
        e2=-2.1687E-16
      
        
        a0=999.842594
        a1=6.793952E-2
        a2=-9.095290E-3
        a3=1.001685E-4
        a4=-1.120083E-6
        a5=6.536332E-9
        b0=8.24493E-1
        b1=-4.0899E-3
        b2=7.6438E-5
        b3=-8.2467E-7
        b4=5.3875E-9
        c0=-5.72466E-3
        c1=1.0227E-4
        c2=-1.6546E-6
        d0=4.8314E-4
        e0=19652.21
        e1=148.4206
        e2=-2.327105
        e3=1.360477E-2
        e4=-5.155288E-5
        f0=54.6746
        f1=-0.603459
        f2=1.09987E-2
        f3=-6.1670E-5
        g0=7.944E-2
        g1=1.6483E-2
        g2=-5.3009E-4
        h0=3.239908
        h1=1.43713E-3
        h2=1.16092E-4
        h3=-5.77905E-7
        i0=2.2838E-3
        i1=-1.0981E-5
        i2=-1.6078E-6
        j0=1.91075E-4
        k0=8.50935E-5
        k1=-6.12293E-6
        k2=5.2787E-8
        m0=-9.9348E-7
        m1=2.0816E-8
        m2=9.1697E-10
    !パラメータ終わり
    
    !密度計算
            t=potemp;pp=0.;S=sal
            rhow=a0+a1*t+a2*t**2.+a3*t**3.+a4*t**4.+a5*t**5.
            rho=rhow+(b0+b1*t+b2*t**2.+b3*t**3.+b4*t**4.)*S+(c0+c1*t+c2*t**2.)*S**(3./2.)+d0*S**2 
            Kw=e0+e1*t+e2*t**2.+e3*t**3.+e4*t**4.
            Aw=h0+h1*t+h2*t**2.+h3*t**3.
            Bw=k0+k1*t+k2*t**2.
            AAAA=Aw+(i0+i1*t+i2*t**2.)*S+j0*S**(3./2.)
            BB=Bw+(m0+m1*t+m2*t**2.)*S
            Kt=Kw+(f0+f1*t+f2*t**2.+f3*t**3.)*S+(g0+g1*t+g2*t**2.)*S**(3./2.)
            KK=Kt+AAAA*pp+BB*pp**2.
            rhoafter=rho/(1.-pp/KK)
            if (S==0) then
            sigma=0.
            else
            sigma=real(rhoafter)-1000.
            end if 
    !密度計算終わり
    
    
end subroutine

! for calculating potential temperature
subroutine potemp_T_S_depth(potemp,tem,sal,depth)
    real,intent(in)::tem,sal,depth
    real,intent(out)::potemp
    !密度計算
    !!!Keisan Parameter
    double precision::a0,a1,a2,a3
    double precision::b0,b1
    double precision::c0,c1,c2,c3
    double precision::d0,d1
    double precision::e0,e1,e2
    ! double precision::f0,f1,f2,f3
    ! double precision::g0,g1,g2
    ! double precision::h0,h1,h2,h3
    ! double precision::i0,i1,i2
    ! double precision::j0
    ! double precision::k0,k1,k2
    ! double precision::m0,m1,m2
    double precision::d,S,t,p
    double precision::q,Gamma,Theta,xk
        a0=3.5803E-5
        a1=8.5258E-6
        a2=-6.8360E-8
        a3=6.6228E-10
        b0=1.8932E-6
        b1=-4.2393E-8
        c0=1.8741E-8
        c1=-6.7795E-10
        c2=8.733E-12
        c3=-5.4481E-14
        d0=-1.1351E-10
        d1=2.7759E-12
        e0=-4.6206E-13
        e1=1.8676E-14
        e2=-2.1687E-16

        !ポテンシャル水温計算
        S=sal
        t=tem
        d=0.-depth
        Gamma=a0+a1*t+a2*(t**2.)+a3*(t**3.)+(S-35.)*(b0+b1*t)+depth*(c0+c1*t+c2*(t**2.)+c3*(t**3.))+depth*(S-35.)*(d0+d1*t)+(depth**2.)*(e0+e1*t+e2*(t**2.))
        xk=d*Gamma
        t=t+0.5*xk
        q=xk
        p=depth+0.5*d
        Gamma=a0+a1*t+a2*(t**2.)+a3*(t**3.)+(S-35.)*(b0+b1*t)+p*(c0+c1*t+c2*(t**2.)+c3*(t**3.))+p*(S-35.)*(d0+d1*t)+(p**2.)*(e0+e1*t+e2*(t**2.))
        xk=d*Gamma
        t=t+0.29289322*(xk-q)
        q=0.58578644*xk+0.121320344*q
        Gamma=a0+a1*t+a2*(t**2.)+a3*(t**3.)+(S-35.)*(b0+b1*t)+p*(c0+c1*t+c2*(t**2.)+c3*(t**3.))+p*(S-35.)*(d0+d1*t)+(p**2.)*(e0+e1*t+e2*(t**2.))
        xk=d*Gamma
        t=t+1.707106781*(xk-q)
        q=3.414213562*xk-4.121320344*q
        p=p+0.5*d
        Gamma=a0+a1*t+a2*(t**2.)+a3*(t**3.)+(S-35.)*(b0+b1*t)+p*(c0+c1*t+c2*(t**2.)+c3*(t**3.))+p*(S-35.)*(d0+d1*t)+(p**2.)*(e0+e1*t+e2*(t**2.))
        xk=d*Gamma
        Theta=(t+(xk-2.0*q)/6.)
        if(S==0) then
        potemp=0.
        else
        potemp=real(Theta)
        end if  
    
    
end subroutine

!for calbrating SSH using SSP units must be in mm 
subroutine calibrate_SSH(SSH,SSP,calibratedSSH)
    implicit none
    real,intent(in)::SSH,SSP
    real,intent(out)::calibratedSSH

    if(SSH/=0.) then
        calibratedSSH = SSH + (SSP-10130)
    else;end if 
end subroutine 

                                            ! SUBROUTINES FOR DATA MANIPULATION !

                                            ! SUBROUTINES FOR GRAPHS AND SHIT !

! creates a box
subroutine create_box(width,height,thickness)
    implicit none
    real,intent(in)::width,height
    integer,intent(in)::thickness

    call newpen2(thickness)
    call plot(0.,0.,3);call plot(width,0.,2);call plot(width,height,2);call plot(0.,height,2);call plot(0.,0.,2)
end subroutine

! create num memori
subroutine num_memori(ini_num,fin_num,iterations,symbol_freq,symbol_size,float_quantity,length,angle,lessthan,morethan)
    implicit none
    real,intent(in)::ini_num,fin_num,symbol_size,length
    integer,intent(in)::iterations,symbol_freq,angle,lessthan,morethan,float_quantity
    real::memori_diff,num_diff
    integer::n

    
    if(morethan==0 .and. lessthan==0) then
        call newpen2(3)
        memori_diff = length/real(iterations); num_diff = (fin_num-ini_num)/real(iterations)
        if(angle == 0) then
            do n = 0, iterations
                if(mod(n,symbol_freq)==0) then
                    call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,-0.1,2)
                    call numberc(real(n)*memori_diff,-1.2*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                else; call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,-0.05,2)
                end if
            end do
        else if(angle == 90) then
            do n = 0, iterations
                if(mod(n,symbol_freq)==0) then
                    call plot(0.,real(n)*memori_diff,3);call plot(0.1,real(n)*memori_diff,2)
                    call number(0.5*symbol_size,real(n)*memori_diff,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                else;call plot(0.,real(n)*memori_diff,3);call plot(0.05,real(n)*memori_diff,2)
                end if
            end do
        else if (angle == -90) then
            do n = 0, iterations
                if(mod(n,symbol_freq)==0) then
                    call plot(0.,real(n)*memori_diff,3);call plot(-0.1,real(n)*memori_diff,2)
                    call numberr(-0.1,real(n)*memori_diff,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                else;call plot(0.,real(n)*memori_diff,3);call plot(-0.05,real(n)*memori_diff,2)
                end if
            end do
        else;end if
    else;end if

    if(morethan==1 .and. lessthan==1) then
        call newpen2(3)
        memori_diff = length/real(iterations); num_diff = (fin_num-ini_num)/real(iterations)
        if(angle == 0) then
            do n = 0, iterations
                if(mod(n,symbol_freq)==0) then;call numberc(real(n)*memori_diff,-1.2*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,-0.1,2)
                else;call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,-0.05,2)
                end if
            end do
            call symbolr(-memori_diff*1.6,-2.0*symbol_size,symbol_size,'<',0.,1);call number(-memori_diff*1.6,-2.0*symbol_size,symbol_size,ini_num,0.,float_quantity)
            call symbol(length+memori_diff*1.6,-2.0*symbol_size,symbol_size,'<',0.,1);call numberr(length+memori_diff*1.6,-2.0*symbol_size,symbol_size,fin_num,0.,float_quantity)
        else !(angle == 90)
            do n = 0, iterations
                if(mod(n,symbol_freq)==0) then;call number(0.5*symbol_size,real(n)*memori_diff,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                call plot(0.,real(n)*memori_diff,3);call plot(0.01,real(n)*memori_diff,2)
                else;call plot(0.,real(n)*memori_diff,3);call plot(0.05,real(n)*memori_diff,2)
                end if
            end do
            call symbolr(1.4*symbol_size,-memori_diff*1.1,symbol_size,'<',0.,1);call number(1.4*symbol_size,-memori_diff*1.1,symbol_size,ini_num,0.,float_quantity)
            call symbolr(1.4*symbol_size,length+memori_diff*1.1,symbol_size,'>',0.,1);call number(1.4*symbol_size,length+memori_diff*1.1,symbol_size,fin_num,0.,float_quantity)
        end if
    else;end if 

   if(morethan==1 .and. lessthan==0) then
    call newpen2(3)
        memori_diff = length/real(iterations); num_diff = (fin_num-ini_num)/real(iterations)
        if(angle == 0) then
            do n = 0, iterations
                if(mod(n,symbol_freq)==0) then;call numberc(n*memori_diff,-1.2*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                call plot(n*memori_diff,0.,3);call plot(n*memori_diff,-0.1,2)
                else;call plot(n*memori_diff,0.,3);call plot(n*memori_diff,-0.05,2)
                end if
            end do
            call symbol(length+memori_diff*1.6,-2.0*symbol_size,symbol_size,'<',0.,1);call numberr(length+memori_diff*1.6,-2.0*symbol_size,symbol_size,fin_num,0.,float_quantity)
        else !(angle == 90)
            do n = 0, iterations
                if(mod(n,symbol_freq)==0) then;call number(0.5*symbol_size,real(n)*memori_diff,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                call plot(0.,n*memori_diff,3);call plot(0.01,n*memori_diff,2)
                else;call plot(0.,n*memori_diff,3);call plot(0.05,n*memori_diff,2)
                end if
            end do
            call symbolr(1.4*symbol_size,length+memori_diff*1.1,symbol_size,'>',0.,1);call number(1.4*symbol_size,length+memori_diff*1.1,symbol_size,fin_num,0.,float_quantity)
        end if
    else;end if

    if(morethan==0 .and. lessthan==1) then
        call newpen2(3)
            memori_diff = length/real(iterations); num_diff = (fin_num-ini_num)/real(iterations)
            if(angle == 0) then
                do n = 0, iterations
                    if(mod(n,symbol_freq)==0) then;call numberc(n*memori_diff,-1.2*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                    call plot(n*memori_diff,0.,3);call plot(n*memori_diff,-0.1,2)
                    else;call plot(n*memori_diff,0.,3);call plot(n*memori_diff,-0.05,2)
                    end if
                end do
                call symbolr(-memori_diff*1.6,-2.0*symbol_size,symbol_size,'<',0.,1);call number(-memori_diff*1.6,-2.0*symbol_size,symbol_size,ini_num,0.,float_quantity)
            else !(angle == 90)
                do n = 0, iterations
                    if(mod(n,symbol_freq)==0) then;call number(0.5*symbol_size,real(n)*memori_diff,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                    call plot(0.,n*memori_diff,3);call plot(0.1,n*memori_diff,2)
                    else;call plot(0.,n*memori_diff,3);call plot(0.05,n*memori_diff,2)
                    end if
                end do
                call symbolr(1.4*symbol_size,-memori_diff*1.1,symbol_size,'<',0.,1);call number(1.4*symbol_size,-memori_diff*1.1,symbol_size,ini_num,0.,float_quantity)
            end if
    else;end if   




end subroutine

! create month memori
subroutine month_memori(symbol_size,length)
    implicit none
    real,intent(in)::symbol_size,length
    real::dx
    integer::n
    character(len=4),dimension(12)::month_names
    
    call newpen2(2)
    call month_str_array(month_names)
    dx = length/13.
    call plot(0.,0.,3);call plot(length,0.,2)
    do n = 1,12
        call plot(real(n)*dx,0.,3);call plot(real(n)*dx,-0.05,2)
        call symbolc(real(n)*dx,-1.2*symbol_size,symbol_size,month_names(n),0.,4)
    end do
end subroutine

subroutine mod12_memori(iterations,symbol_size,length,x,y)
    implicit none
    real,intent(in)::symbol_size,length,x,y
    integer,intent(in)::iterations
    real::dx
    integer::n,m

    call plot(x,y,-3)

    call newpen2(3)
    dx = length/real(iterations+1)
    call plot(0.,0.,3);call plot(length,0.,2)
    do n = 1,iterations
        if (mod(n,12)/=0) then;m = mod(n,12)
        else if(mod(n,12)==0) then;m = 12
        else;end if
        call plot(real(n)*dx,0.,3);call plot(real(n)*dx,-0.1,2)
        call numberc(real(n)*dx,-1.2*symbol_size,symbol_size,real(m),0.,-1)
    end do
    call symbolc(length/2.,-symbol_size*2.,symbol_size*0.8,'Months',0.,len('months'))

    call plot(-x,-y,-3)
end subroutine

! month names array
subroutine month_str_array(month_names)
    implicit none
    character(len=4),dimension(12),intent(out)::month_names

    month_names = (/'Jan.','Feb.','Mar.','Apr.','May ','Jun.','Jul.','Aug.','Sep.','Oct.','Nov.','Dec.'/)
end subroutine

! creating ps frame
subroutine psframe(ini_st,fin_st,depth,width,height,memori_size)
    integer,intent(in)::ini_st,fin_st,depth
    real,intent(in)::width,height,memori_size
    real::dx,dy
    integer::x_memori,y_memori,st_quan,depth_quan
    integer,parameter::increment = 50

    dx = width/real(fin_st-ini_st+1); dy = height/real(depth)
    st_quan = fin_st - ini_st + 1; depth_quan = depth/increment
    call newpen2(3)
    call plot(0.,0.,3);call plot(0.,-height,2);call plot(width,-height,2);call plot(width,0.,2);call plot(0.,0.,2)
    call newpen2(3)
    do x_memori = 1,st_quan
        call plot(dx*real(x_memori)-dx/2.,-height,3);call plot(dx*real(x_memori)-dx/2.,-height-memori_size,2)
        call numberc(dx*real(x_memori)-dx/2.,-height-memori_size*4.,memori_size*3.,real(st_quan-x_memori+1),0.,-1)
    end do
    call newpen2(3)
    do y_memori = 0,depth_quan
        call plot(0.,-dy*real(increment)*real(y_memori),3);call plot(-memori_size,-dy*real(increment)*real(y_memori),2)
        call numberc(-memori_size*4.,-dy*real(increment)*real(y_memori),memori_size*3.,real(y_memori*increment),0.,-1)
    end do
end subroutine

! creating map 
subroutine create_map(ini_lat,fin_lat,ini_long,fin_long,ini_st,fin_st,width)
    implicit none
    integer,intent(in)::ini_lat,fin_lat,ini_long,fin_long,ini_st,fin_st
    real,intent(in):: width
    intrinsic sin,cos,tan,asin,acos
    integer,parameter::imax = 2080,jmax = 2640,station_x = 9, station_y = 2
    real,dimension(:,:),allocatable::dep
    integer,dimension(:,:),allocatable::dep_m
    integer::i,j,is,ie,js,je,n,line_num
    real::dx,dy,height,ratio,pi,xco,NLineYco,SLineYco
    character::line_name*10,filename*999
    real,dimension(station_y,station_x)::lon

    allocate(dep(imax,jmax));allocate(dep_m(imax,jmax))
    dep = 0.;dep_m = 0
    open(21,file='/Users/yuta/Desktop/nishimori2/aomori/japan1km122-148_24-46.bin',form='unformatted',status='old')
    do j=jmax,1,-1
        read(21)(dep(i,j),i=1,imax)
        dep(i,j)=-dep(i,j)    
    end do
    close(21)

    do i=1,imax
        do j=1,jmax
            dep_m(i,j)=1
        end do
    end do

    if (ini_lat<24 .or. ini_lat>46 .or. fin_lat<24 .or. fin_lat>46 .or. ini_long<122 .or. ini_long>148 .or. fin_long<122 .or. fin_long>148 .or.ini_lat>fin_lat .or. ini_long>fin_long) then
        print*, 'Your map coordinates must be within 24-46N and 122-148E'
    else
        js = (ini_lat-24)*120+1
        je = (fin_lat-24)*120
        is = (ini_long-122)*80+1
        ie = (fin_long-122)*80
        pi = 2.*asin(1.)
        ratio = 6357./6378./cos((ini_lat+fin_lat)/2.*pi/180.)
        height = width*ratio*real(fin_lat-ini_lat)/real(fin_long-ini_long)
        dx = width/real(ie-is)
        dy = height/real(je-js)

        ! call plot(x,y,-3)
        call newpen2(3)
        call rgbk(0.,0.,0.)
        call pscont3(dx,dy,dep,dep_m,is,ie,js,je,imax,jmax,1,0.,10.)
    end if
    call rgbk(0.,0.,0.)
    call plot(0.,0.,3);call plot(width,0.,2);call plot(width,height,2);call plot(0.,height,2);call plot(0.,0.,2)
    do n = 0,fin_long-ini_long
        call plot(dx*80.*real(n),0.,3);call plot(dx*80.*real(n),-0.1,2);call numberc(dx*80.*real(n),-0.4,0.2,real(n+ini_long),0.,-1)
    end do
    do n = 0,fin_lat-ini_lat
        call plot(0.,dy*120.*real(n),3);call plot(-0.1,dy*120.*real(n),2);call numberc(-0.3,dy*120.*real(n),0.2,real(n+ini_lat),0.,-1)
    end do
    call symbolc(width/2.,-0.6,0.2,'Long (E)',0.,len('Long (e)'))
    call symbolc(-0.6,height/2.,0.2,'Lat (N)',90.,len('Lat (n)'))
    if(ini_long<=137 .and.fin_long>=140 .and. ini_lat<=40 .and. fin_lat>=41) then
        do line_num = 1,station_y;if(line_num == 1) then; line_name = 'N-Line';else;line_name = 'S-Line';end if
            filename = '/Users/yuta/Desktop/nishimori2/aomori/Coordinates/'//trim(line_name)//'/lon.csv'
            NLineYco = dy*(41.-real(ini_lat))*120.;SLineYco = dy*(40.6-real(ini_lat))*120.
            open(32,file=filename,status = 'old',action = 'read')
            read(32,'(9(f9.4))')(lon(line_num,i),i = 1,station_x)
            close(32)
            do n = ini_st,fin_st; xco = dx*(lon(line_num,n)-real(ini_long))*80.
                if (line_num ==1) then
                    if(n==1.or.n==2.or.n==3)then;call gmark(xco,NLineYco,0.1,1);call numberc(xco,NLineYco+0.1,0.15,real(10-n),0.,-1)
                    else if(n==4.or.n==5.or.n==6)then;call gmark(xco,NLineYco,0.1,6);call numberc(xco,NLineYco+0.1,0.15,real(10-n),0.,-1)
                    else if(n==7.or.n==8.or.n==9)then;call gmark(xco,NLineYco,0.1,8);call numberc(xco,NLineYco+0.1,0.15,real(10-n),0.,-1)
                    else;end if
                else if(line_num ==2) then
                    if(n==1.or.n==2.or.n==3)then;call gmark(xco,SLineYco,0.1,1);call numberc(xco,SLineYco-0.2,0.15,real(10-n),0.,-1)
                    else if(n==4.or.n==5.or.n==6)then;call gmark(xco,SLineYco,0.1,6);call numberc(xco,SLineYco-0.2,0.15,real(10-n),0.,-1)
                    else if(n==7.or.n==8.or.n==9)then;call gmark(xco,SLineYco,0.1,8);call numberc(xco,SLineYco-0.2,0.15,real(10-n),0.,-1)
                    else;end if
            else;end if;end do
        end do
        call symbol(dx*(lon(1,1)-real(ini_long))*80.,NLineYco+0.3,0.2,'N-Line',0.,6)
        call symbol(dx*(lon(2,1)-real(ini_long))*80.,SLineYco-0.4,0.2,'S-Line',0.,6)
    else
        print*,'stations are just outside of your map, like a perfect flower that is just beyond your reach...(mj)'
    end if
    deallocate(dep);deallocate(dep_m)

    ! call plot(-x,-y,-3)
        
end subroutine

! only creates a frame unfortunately
subroutine TS_diagram(temp_min,temp_max,sal_min,sal_max,sal_memoriterations,sal_symbolfreq,symbol_size,width,height)
    implicit none
    integer,parameter::ds_total = 101, dt_total =101
    integer,parameter::xs = 1,xe = ds_total,ys = 1,ye = dt_total
    real,parameter:: cont_inc = 0.2
    real,intent(in)::sal_min,sal_max,width,height,symbol_size
    ! integer,intent(in)::arraysize
    integer,intent(in)::temp_min,temp_max,sal_memoriterations,sal_symbolfreq
    ! real,dimension(arraysize),intent(in)::temp_array,sal_array
    real::sal,temp,sal_range,whatyouneed,dx,dy,real_sigma,y,sigma_print_y,min_sigma,max_sigma,first_sigma,sal_memoridiff,sal_numdiff
    integer::n,s,t,temp_range,cont_quantity
    integer,dimension(ds_total,dt_total)::mask=0
    real,dimension(ds_total,dt_total)::sigma_array=0.
    dx = width/real(ds_total-1); dy = height/real(dt_total-1)
    temp_range = temp_max - temp_min; sal_range = sal_max - sal_min
    do s=1,ds_total
        do t=1,dt_total
            mask(s,t)=1
        end do
    end do  !just for the use of Kuroda's subroutine

    
    call newpen2(3)
    call plot(0.,0.,3);call plot(width,0.,2);call plot(width,height,2);call plot(0.,height,2);call plot(0.,0.,2)
    call symbolc(width/2.,-2.2*symbol_size,symbol_size,'Salinity',0.,len('salinity'))
    call symbolc(-2.5*symbol_size,height/2.,symbol_size,'Temperature',90.,len('temperature'))
    call newpen2(3)

    do n = 0,sal_memoriterations
        call newpen2(4)
        sal_memoridiff = width/real(sal_memoriterations);sal_numdiff = (sal_max-sal_min)/real(sal_memoriterations)
        if (mod(n,sal_symbolfreq)==0) then
            call plot(real(n)*sal_memoridiff,0.,3);call plot(real(n)*sal_memoridiff,-0.2,2)
            call numberc(real(n)*sal_memoridiff,-1.2*symbol_size,symbol_size,sal_min+real(n)*sal_numdiff,0.,2)
        else
            call plot(real(n)*sal_memoridiff,0.,3);call plot(real(n)*sal_memoridiff,-0.1,2)
        end if
    end do
    do n = 0,temp_range
        call newpen2(4)
        if(mod(n,5)==0) then
            call plot(0.,real(n)*real(height)/real(temp_range),3);call plot(-0.2,real(n)*real(height)/real(temp_range),2)
            call numberr(-0.3,real(n)*real(height)/real(temp_range),symbol_size*0.9,real(temp_min+temp_range)*real(n)/real(temp_range),0.,1)
        else
            call plot(0.,real(n)*real(height)/real(temp_range),3);call plot(-0.1,real(n)*real(height)/real(temp_range),2)  
        end if
    end do

    do s = 1,ds_total
        do t = 1,dt_total
           temp = temp_min+temp_range*real(t)/real(dt_total); sal = sal_min+sal_range*real(s)/real(ds_total)
            call sigma_T_S(whatyouneed,temp,sal)
            sigma_array(s,t) = whatyouneed
        end do
    end do
    min_sigma = minval(sigma_array) 
    max_sigma = maxval(sigma_array)  
    ! print*,min_sigma,max_sigma
    y = 0.
    call newpen2(3)
    cont_quantity = int(max_sigma-min_sigma)*5+10
    first_sigma = int(min_sigma)
    call plot(-dx/2.,-dy/2.,-3)
    call symbol(width+symbol_size*0.2,height+symbol_size*0.4,symbol_size*0.8,'sigma:',0.,len('sigma:'))
    do n = 0,cont_quantity
        real_sigma = first_sigma+real(n)*cont_inc
        if(int(real_sigma)==real_sigma .and. real_sigma>=min_sigma .and. real_sigma<=max_sigma) then
            call newpen2(5);call rgbk(0.,0.,0.);call pscont3(dx,dy,sigma_array,mask,xs,xe,ys,ye,ds_total,dt_total,1,real_sigma,0.)
            sigma_print_y = height-0.8*symbol_size-y
            call numberc(width+symbol_size,sigma_print_y,0.8*symbol_size,real_sigma,0.,1)
            y = y +0.8*symbol_size
        else
            call newpen2(2);call rgbk(0.,0.,0.);call pscont3(dx,dy,sigma_array,mask,xs,xe,ys,ye,ds_total,dt_total,1,real_sigma,0.)
        end if
    end do
    y = 0.
    call plot(dx/2.,dy/2.,-3)

    ! do n = 1, arraysize
    !     if(temp_array(n)/=0. .and. sal_array(n)/=0.)then
    !         plot_x = (sal_array(n)-sal_min)*width/real(sal_range)
    !         plot_y = (temp_array(n)-real(temp_min))*height/real(temp_range)
    !         call gmark(plot_x,plot_y,0.05,1)
    !     else;end if
    ! end do

end subroutine

! gives a 1D coordinate of input_value 
subroutine gmark_ratio(input_value,min,max,length,output)
    implicit none
    real,intent(in)::input_value,min,max,length
    real,intent(out)::output

    output = (input_value-min)*length/(max-min)

end subroutine

! betanuri version of ps color, only works for 1D arrays
subroutine betcolork(starting_x,dx,dy,oneD_array,array_size,mask,ini_value,fin_value,r,g,b)
    implicit none
    integer,intent(in)::array_size
    real,dimension(array_size),intent(in)::oneD_array
    integer,dimension(array_size),intent(in)::mask
    real,intent(in)::starting_x,ini_value,fin_value,r,g,b,dx,dy
    integer::n

    do n = 1, array_size
        if(mask(n)/=0) then
            if(ini_value<=oneD_array(n) .and. oneD_array(n)<=fin_value) then
                call betsqk(starting_x,real(n-1)*dy,starting_x+dx,real(n)*dy,r,g,b)
            else;end if
        else;end if
    end do

end subroutine

! betcolork but for integers such as data quantity
subroutine betcolorI(starting_x,dx,dy,oneD_array,array_size,mask,some_integer,r,g,b)
    implicit none
    integer,intent(in)::array_size
    integer,dimension(array_size),intent(in)::oneD_array,mask
    real,intent(in)::starting_x,dx,dy,r,g,b 
    integer,intent(in)::some_integer
    integer::n 
    
    do n = 1, array_size
        if(mask(n)/=0) then
            if(oneD_array(n) == some_integer) then
                call betsqk(starting_x,real(n-1)*dy,starting_x+dx,real(n)*dy,r,g,b)
            else;end if
        else;end if
    end do

end subroutine


                                            ! COLOR SCALES AND COLOR SCALE LEGENDS !

! color scale maker                                                                   !!NOT VERY USEFUL WOULD USE COLORSCALE CREATOR INSTEAD!!
subroutine colorscale(r1,g1,b1,r2,g2,b2,iterations,width,height,lessthan,morethan,reset_starting_point)
    implicit none
    real,intent(in)::r1,g1,b1,r2,g2,b2,width,height,reset_starting_point
    integer,intent(in)::iterations,morethan,lessthan !1で原点を最初の位置に,0で原点は継続（カラースケール後,1で以上以下を付与0で無
    real::r,g,b
    integer::n
    real,dimension(3)::lefty_x1,lefty_y1,righty_x1,righty_y1,bottomy_x1,bottomy_y1,toppy_x1,toppy_y1

    lefty_x1(1) = -width;lefty_x1(2) = -width;lefty_x1(3) = -width-width/real(iterations)
    lefty_y1(1) = 0.;lefty_y1(2) = height ;lefty_y1(3) = height/2.
    righty_x1(1) = 0. ;righty_x1(2) = 0. ;righty_x1(3) = width/real(iterations)
    righty_y1(1) = 0. ;righty_y1(2) = height ;righty_y1(3) = height/2.
    bottomy_x1(1) = 0. ;bottomy_x1(2) = -width ;bottomy_x1(3) = -width/2.
    bottomy_y1(1) = -height ;bottomy_y1(2) = -height ;bottomy_y1(3) = -height-height/real(iterations)
    toppy_x1(1) = 0. ;toppy_x1(2) =  -width ;toppy_x1(3) = -width/2.
    toppy_y1(1) = 0. ;toppy_y1(2) = 0. ;toppy_y1(3) = height/real(iterations)
 
    if (morethan==0 .and. lessthan==0) then
        call newpen2(3)
         do n = 1,iterations
            r = r1+real(n-1)*real(r2-r1)/real(iterations-1); g = g1+real(n-1)*real(g2-g1)/real(iterations-1); b = b1+real(n-1)*real(b2-b1)/real(iterations-1)
            if(width>=height) then
                call betsqk(0.,0.,width/real(iterations),height,r,g,b)
                if(n==1)then;call plot(0.,0.,3);call plot(width/real(iterations),0.,2);call plot(width/real(iterations),height,2);call plot(0.,height,2);call plot(0.,0.,2)
                else;call plot(0.,0.,3);call plot(width/real(iterations),0.,2);call plot(width/real(iterations),height,2);call plot(0.,height,2)
                end if
                call plot(width/real(iterations),0.,-3)
             else !(if width<=height) 
                call betsqk(0.,0.,-width,height/real(iterations),r,g,b)
                if (n ==1) then;call plot(0.,0.,3);call plot(0.,height/real(iterations),2);call plot(-width,height/real(iterations),2);call plot(-width,0.,2);call plot(0.,0.,2)
                else;call plot(0.,0.,3);call plot(0.,height/real(iterations),2);call plot(-width,height/real(iterations),2);call plot(-width,0.,2)
                end if
                call plot(0.,height/real(iterations),-3)!from bottom to top
            end if
        end do    
    else;end if
    if(morethan==1 .and. lessthan ==1) then
        call newpen2(3)
        do n = 1,iterations
            r = r1+real(n)*real(r2-r1)/real(iterations+1); g = g1+real(n)*real(g2-g1)/real(iterations+1); b = b1+real(n)*real(b2-b1)/real(iterations+1)
            if(width>=height) then
                call betsqk(0.,0.,width/real(iterations),height,r,g,b)
                if(n==1)then;call plot(0.,0.,3);call plot(width/real(iterations),0.,2);call plot(width/real(iterations),height,2);call plot(0.,height,2);call plot(0.,0.,2)
                else;call plot(0.,0.,3);call plot(width/real(iterations),0.,2);call plot(width/real(iterations),height,2);call plot(0.,height,2)
                end if
                call plot(width/real(iterations),0.,-3)
             else !(if width<=height) 
                call betsqk(0.,0.,-width,height/real(iterations),r,g,b)
                if (n ==1) then;call plot(0.,0.,3);call plot(0.,height/real(iterations),2);call plot(-width,height/real(iterations),2);call plot(-width,0.,2);call plot(0.,0.,2)
                else;call plot(0.,0.,3);call plot(0.,height/real(iterations),2);call plot(-width,height/real(iterations),2);call plot(-width,0.,2)
                end if
                call plot(0.,height/real(iterations),-3)!from bottom to top
            end if
        end do   
            if (width>=height) then
                call betmlk(lefty_x1,lefty_y1,3,3,r1,g1,b1);call betmlk(righty_x1,righty_y1,3,3,r2,g2,b2)
                ! call plot(lefty_x1(1),lefty_y1(1),3);call plot(lefty_x1(3),lefty_x1(3),2);call plot(lefty_x1(2),lefty_y1(2),2)
                ! call plot(righty_x1(1),righty_y1(1),3);call plot(righty_x1(3),righty_x1(3),2);call plot(righty_x1(2),righty_y1(2),2)
                call plot(-width,0.,3);call plot(-width-width/real(iterations),height/2.,2);call plot(-width,height,2)
                call plot(0.,0.,3);call plot(width/real(iterations),height/2.,2);call plot(0.,height,2)
            else if (height>=width) then
                call betmlk(bottomy_x1,bottomy_y1,3,3,r1,g1,b1);call betmlk(toppy_x1,toppy_y1,3,3,r2,g2,b2)
                ! call plot(bottomy_x1(1),bottomy_y1(1),3);call plot(bottomy_x1(3),bottomy_x1(3),2);call plot(bottomy_x1(2),bottomy_y1(2),2)
                ! call plot(toppy_x1(1),toppy_y1(1),3);call plot(toppy_x1(3),toppy_x1(3),2);call plot(toppy_x1(2),toppy_y1(2),2)
                call plot(0.,-height,3);call plot(-width/2.,-height-height/real(iterations),2);call plot(-width,-height,2)
                call plot(0.,0.,3);call plot(-width/2.,height/real(iterations),2);call plot(-width,0.,2)
            else;end if
    else;end if
    
    if (morethan==1 .and. lessthan==0) then
        call newpen2(3)
        do n = 1,iterations
            r = r1+real(n-1)*real(r2-r1)/real(iterations); g = g1+real(n-1)*real(g2-g1)/real(iterations); b = b1+real(n-1)*real(b2-b1)/real(iterations)
            if(width>=height) then
                call betsqk(0.,0.,width/real(iterations),height,r,g,b)
                if(n==1)then;call plot(0.,0.,3);call plot(width/real(iterations),0.,2);call plot(width/real(iterations),height,2);call plot(0.,height,2);call plot(0.,0.,2)
                else; call plot(0.,0.,3);call plot(width/real(iterations),0.,2);call plot(width/real(iterations),height,2);call plot(0.,height,2)
                end if
                call plot(width/real(iterations),0.,-3)
             else !(if width<=height) 
                call betsqk(0.,0.,-width,height/real(iterations),r,g,b)
                if (n ==1) then;call plot(0.,0.,3);call plot(0.,height/real(iterations),2);call plot(-width,height/real(iterations),2);call plot(-width,0.,2);call plot(0.,0.,2)
                else;call plot(0.,0.,3);call plot(0.,height/real(iterations),2);call plot(-width,height/real(iterations),2);call plot(-width,0.,2)
                end if
                call plot(0.,height/real(iterations),-3)!from bottom to top
            end if
        end do 
        if (width>=height) then
            call betmlk(righty_x1,righty_y1,3,3,r2,g2,b2)
            ! call plot(righty_x1(1),righty_y1(1),3);call plot(righty_x1(3),righty_x1(3),2);call plot(righty_x1(2),righty_y1(2),2)
            call plot(0.,0.,3);call plot(width/real(iterations),height/2.,2);call plot(0.,height,2)
        else if (height>=width) then
            call betmlk(toppy_x1,toppy_y1,3,3,r2,g2,b2)
            ! call plot(toppy_x1(1),toppy_y1(1),3);call plot(toppy_x1(3),toppy_x1(3),2);call plot(toppy_x1(2),toppy_y1(2),2)
            call plot(0.,0.,3);call plot(-width/2.,height/real(iterations),2);call plot(-width,0.,2)
        else;end if  
    else;end if

        if(morethan==0 .and. lessthan ==1) then
            call newpen2(3)
            do n = 1, iterations
                r = r1+real(n)*real(r2-r1)/real(iterations); g = g1+real(n)*real(g2-g1)/real(iterations); b = b1+real(n)*real(b2-b1)/real(iterations)
                if(width>=height) then
                    call betsqk(0.,0.,width/real(iterations),height,r,g,b)
                    if(n==1)then;call plot(0.,0.,3);call plot(width/real(iterations),0.,2);call plot(width/real(iterations),height,2);call plot(0.,height,2);call plot(0.,0.,2)
                    else;call plot(0.,0.,3);call plot(width/real(iterations),0.,2);call plot(width/real(iterations),height,2);call plot(0.,height,2)
                    end if
                    call plot(width/real(iterations),0.,-3)
                 else !(if width<=height) 
                    call betsqk(0.,0.,-width,height/real(iterations),r,g,b)
                    if (n ==1) then;call plot(0.,0.,3);call plot(0.,height/real(iterations),2);call plot(-width,height/real(iterations),2);call plot(-width,0.,2);call plot(0.,0.,2)
                    else;call plot(0.,0.,3);call plot(0.,height/real(iterations),2);call plot(-width,height/real(iterations),2);call plot(-width,0.,2)
                    end if
                    call plot(0.,height/real(iterations),-3)!from bottom to top
                end if
            end do 
            if (width>=height) then
                call betmlk(lefty_x1,lefty_y1,3,3,r1,g1,b1)
                ! call plot(lefty_x1(1),lefty_y1(1),3);call plot(lefty_x1(3),lefty_x1(3),2);call plot(lefty_x1(2),lefty_y1(2),2)
                call plot(-width,0.,3);call plot(-width-width/real(iterations),height/2.,2);call plot(-width,height,2)
            else if (height>=width) then
                call betmlk(bottomy_x1,bottomy_y1,3,3,r1,g1,b1)
                ! call plot(bottomy_x1(1),bottomy_y1(1),3);call plot(bottomy_x1(3),bottomy_x1(3),2);call plot(bottomy_x1(2),bottomy_y1(2),2)
                call plot(0.,-height,3);call plot(-width/2.,-height-height/real(iterations),2);call plot(-width,-height,2)
            else;end if  
        else;end if

        if (width>=height) then;call plot(reset_starting_point,0.,-3)
        else if(width<height) then; call plot(0.,reset_starting_point,-3)
            
        else
        end if
    
end subroutine

! color gradient from blue to red no grey r,g,b are arrays with size(0:iterations+1)
subroutine b2r_colorgrad(iterations,midpoint,r,g,b)
    implicit none
    integer,intent(in)::iterations,midpoint
    real,dimension(0:iterations+1),intent(out)::r,g,b
    integer::n
    
    do n = 1, iterations
        if (n <= midpoint) then 
            r(n) = 0.+(real(n-1)/real(midpoint-1))*0.8
            g(n) = 0.+(real(n-1)/real(midpoint-1))*0.8
            b(n) = 1.
        else
            r(n) = 1.
            g(n) = 0.8-(real(n-midpoint-1)/real(iterations-midpoint-1))*0.8
            b(n) = 0.8-(real(n-midpoint-1)/real(iterations-midpoint-1))*0.8
        end if
    end do

    r(0) = 0.; g(0) = 0.; b(0) = 0.8
    r(iterations+1) = 0.8 ; g(iterations+1) = 0. ; b(iterations+1) = 0.

end subroutine

! color gradient from blue to grey to red putting emphasis on zero
subroutine b2gy2r_colorgrad(iterations,midpoint,r,g,b)
    implicit none
    integer,intent(in)::iterations,midpoint
    real,dimension(0:iterations+1),intent(out)::r,g,b
    integer::n
    
    do n = 1, midpoint-1
            r(n) = 0.+(real(n-1)/real(midpoint-2))*0.8
            g(n) = 0.+(real(n-1)/real(midpoint-2))*0.8
            b(n) = 1.
    end do
    do n = midpoint+1,iterations
            r(n) = 1.
            g(n) = 0.8-(real(n-midpoint-1)/real(iterations-midpoint-1))*0.8
            b(n) = 0.8-(real(n-midpoint-1)/real(iterations-midpoint-1))*0.8
    end do
    r(midpoint) = 0.85;g(midpoint) = 0.85;b(midpoint) = 0.85



    r(0) = 0.; g(0) = 0.; b(0) = 0.8
    r(iterations+1) = 0.8 ; g(iterations+1) = 0. ; b(iterations+1) = 0.
end subroutine

! color gradient from red to green useful for data num. size of rgb array is (iterations)
subroutine r2g_colorgrad(iterations,midpoint,r,g,b)
    implicit none
    integer,intent(in)::iterations,midpoint
    real,dimension(iterations),intent(out)::r,g,b
    integer::n 

    do n = 1,iterations
        if(n<=midpoint) then
            r(n) = 1.
            g(n) = real(n-1)/real(midpoint-1)
            b(n) = 0.
        else
            r(n) = 1.-real(n-midpoint)/real(iterations-midpoint)
            g(n) = 1.
            b(n) = 0.
        end if
    end do
    ! r(0) = 0.; g(0) = 0.; b(0) = 0.
end subroutine

! black to red, then gradient from red to green. useful for data num that starts with 0. array size is the same as above
subroutine bk2r2g_colorgrad(iterations,midpoint,r,g,b)
    implicit none
    integer,intent(in)::iterations,midpoint
    real,dimension(iterations),intent(out)::r,g,b 
    integer::n 

    r(1) = 0.; g(1) = 0.; b(1) = 0.
    do n = 2,iterations
        if(n<=midpoint) then
            r(n) = 1.
            g(n) = real(n-2)/real(midpoint-2)
            b(n) = 0.
        else 
            r(n) = 1.-real(n-midpoint)/real(iterations-midpoint)
            g(n) = 1.
            b(n) = 0.
        end if
    end do

end subroutine

! makes a colorscale legend corresponding to arrays of r,g,b. ARRAY SIZE HAS TO BE (ITERATIONS+2). otherwise you will shit yourself
subroutine colorscale_creator(iterations,r,g,b,ini_num,fin_num,symbol_freq,symbol_size,float_quantity,length,width,angle,lessthan,morethan)
    implicit none
    integer,intent(in)::iterations,symbol_freq,float_quantity,angle,lessthan,morethan
    real,intent(in)::ini_num,fin_num,symbol_size,length,width
    real,dimension(0:iterations+1),intent(in)::r,g,b
    integer::n
    real::memori_diff,num_diff
    real,dimension(3)::lefty_x=0.,lefty_y=0.,righty_x=0.,righty_y=0.,bottomy_x=0.,bottomy_y=0.,toppy_x=0.,toppy_y=0.
    memori_diff = length/real(iterations); num_diff = (fin_num-ini_num)/real(iterations)

    lefty_x(1) = 0.;lefty_x(2) = -memori_diff; lefty_x(3) = 0.
    lefty_y(1) = 0.;lefty_y(2) = width/2. ;lefty_y(3) = width
    righty_x(1) = length ;righty_x(2) = length+memori_diff ;righty_x(3) = length
    righty_y(1) = 0. ;righty_y(2) = width/2. ;righty_y(3) = width
    bottomy_x(1) = 0. ;bottomy_x(2) = -width/2. ;bottomy_x(3) = -width
    bottomy_y(1) = 0. ;bottomy_y(2) = -memori_diff ;bottomy_y(3) = 0.
    toppy_x(1) = 0. ;toppy_x(2) =  -width/2. ;toppy_x(3) = -width
    toppy_y(1) = length ;toppy_y(2) = length+memori_diff ;toppy_y(3) = length

    if(morethan==0 .and. lessthan==0) then
        if(angle == 0) then
            call newpen2(3)
            do n = 1, iterations
                call betsqk(real(n-1)*memori_diff,0.,real(n)*memori_diff,width,r(n),g(n),b(n))
            end do
            call plot(0.,0.,3);call plot(length,0.,2);call plot(0.,width,3);call plot(length,width,2)
            do n = 0, iterations
                call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,width,2)
                if(mod(n,symbol_freq)/=0) then
                    call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,-0.05,2)
                else;call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,-0.1,2)
                end if
                if(mod(n,symbol_freq)==0) then;call numberc(real(n)*memori_diff,-1.4*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                else;end if
            end do
        else !(angle == 90)
            call newpen2(3)
            do n = 1, iterations
                call betsqk(0.,real(n-1)*memori_diff,-width,real(n)*memori_diff,r(n),g(n),b(n))
            end do
            call plot(0.,0.,3);call plot(0.,length,2);call plot(-width,0.,3);call plot(-width,length,2)
            do n = 0, iterations
                call plot(0.,real(n)*memori_diff,3);call plot(-width,real(n)*memori_diff,2)
                if(mod(n,symbol_freq)/=0) then
                    call plot(0.,real(n)*memori_diff,3);call plot(0.05,real(n)*memori_diff,2)
                else;call plot(0.,real(n)*memori_diff,3);call plot(0.1,real(n)*memori_diff,2)
                end if
                if(mod(n,symbol_freq)==0) then;call number(symbol_size*0.5,real(n)*memori_diff,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                else;end if
            end do
        end if
    else;end if

    if(morethan==1 .and. lessthan==1) then
        if(angle == 0) then
            call newpen2(3)
            do n = 1, iterations
                call betsqk(real(n-1)*memori_diff,0.,real(n)*memori_diff,width,r(n),g(n),b(n))
            end do
            call plot(0.,0.,3);call betmlk(lefty_x,lefty_y,3,3,r(0),g(0),b(0));call betmlk(righty_x,righty_y,3,3,r(iterations+1),g(iterations+1),b(iterations+1))

            call newpen2(3);call plot(0.,0.,3);call plot(length,0.,2);call plot(0.,width,3);call plot(length,width,2)
            call plot(0.,0.,3);call plot(-memori_diff,width/2.,2);call plot(0.,width,2)
            call plot(length,0.,3);call plot(length+memori_diff,width/2.,2);call plot(length,width,2)
            do n = 0, iterations
                call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,width,2)
                if(mod(n,symbol_freq)/=0) then
                    call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,-0.05,2)
                else;call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,-0.1,2)
                end if
                if(mod(n,symbol_freq)==0) then;call numberc(real(n)*memori_diff,-1.4*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                else;end if
            end do
            call symbolr(-memori_diff*1.6,-2.0*symbol_size,symbol_size*0.7,'<',0.,1);call number(-memori_diff*1.6,-2.0*symbol_size,symbol_size*0.7,ini_num,0.,float_quantity)
            call symbol(length+memori_diff*1.6,-2.0*symbol_size,symbol_size*0.7,'<',0.,1);call numberr(length+memori_diff*1.6,-2.0*symbol_size,symbol_size*0.7,fin_num,0.,float_quantity)
        else !(angle == 90)
            call newpen2(3)
            do n = 1, iterations
                call betsqk(0.,real(n-1)*memori_diff,-width,real(n)*memori_diff,r(n),g(n),b(n))
            end do
            call plot(0.,0.,3);call betmlk(bottomy_x,bottomy_y,3,3,r(0),g(0),b(0));call betmlk(toppy_x,toppy_y,3,3,r(iterations+1),g(iterations+1),b(iterations+1))

            call newpen2(3);call plot(0.,0.,3);call plot(0.,length,2);call plot(-width,0.,3);call plot(-width,length,2)
            call plot(0.,0.,3);call plot(-width/2.,-memori_diff,2);call plot(-width,0.,2)
            call plot(0.,length,3);call plot(-width/2.,length+memori_diff,2);call plot(-width,length,2)
            do n = 0, iterations
                call plot(0.,real(n)*memori_diff,3);call plot(-width,real(n)*memori_diff,2)
                if(mod(n,symbol_freq)/=0) then
                    call plot(0.,real(n)*memori_diff,3);call plot(0.05,real(n)*memori_diff,2)
                else;call plot(0.,real(n)*memori_diff,3);call plot(0.1,real(n)*memori_diff,2)
                end if
                if(mod(n,symbol_freq)==0) then;call number(symbol_size*0.5,real(n)*memori_diff,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                else;end if
            end do
            call symbolr(1.4*symbol_size,-memori_diff*1.4,symbol_size*0.7,'<',0.,1);call number(1.4*symbol_size,-memori_diff*1.4,symbol_size*0.7,ini_num,0.,float_quantity)
            call symbolr(1.4*symbol_size,length+memori_diff*1.4,symbol_size*0.7,'>',0.,1);call number(1.4*symbol_size,length+memori_diff*1.4,symbol_size*0.7,fin_num,0.,float_quantity)
        end if
    else;end if 

    if(morethan==1 .and. lessthan==0) then
        if(angle == 0) then
        call newpen2(3)
        do n = 1, iterations
            call betsqk(real(n-1)*memori_diff,0.,real(n)*memori_diff,width,r(n),g(n),b(n))
        end do
        call plot(0.,0.,3);call betmlk(righty_x,righty_y,3,3,r(iterations+1),g(iterations+1),b(iterations+1))

        call newpen2(3);call plot(0.,0.,3);call plot(length,0.,2);call plot(0.,width,3);call plot(length,width,2)
        call plot(length,0.,3);call plot(length+memori_diff,width/2.,2);call plot(length,width,2)
        do n = 0, iterations
            call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,width,2)
            if(mod(n,symbol_freq)/=0) then
                call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,-0.05,2)
            else;call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,-0.1,2)
            end if
            if(mod(n,symbol_freq)==0) then;call numberc(real(n)*memori_diff,-1.4*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
            else;end if
        end do
            call symbol(length+memori_diff*1.6,-2.0*symbol_size,symbol_size,'<',0.,1);call numberr(length+memori_diff*1.6,-2.0*symbol_size,symbol_size,fin_num,0.,float_quantity)
        else !(angle == 90)
            call newpen2(3)
            do n = 1, iterations
                call betsqk(0.,real(n-1)*memori_diff,-width,real(n)*memori_diff,r(n),g(n),b(n))
            end do
            call plot(0.,0.,3);call betmlk(toppy_x,toppy_y,3,3,r(iterations+1),g(iterations+1),b(iterations+1))

            call newpen2(3);call plot(0.,0.,3);call plot(0.,length,2);call plot(-width,0.,3);call plot(-width,length,2)
            call plot(0.,length,3);call plot(-width/2.,length+memori_diff,2);call plot(-width,length,2)
            do n = 0, iterations
                call plot(0.,real(n)*memori_diff,3);call plot(-width,real(n)*memori_diff,2)
                if(mod(n,symbol_freq)/=0) then
                    call plot(0.,real(n)*memori_diff,3);call plot(0.05,real(n)*memori_diff,2)
                else;call plot(0.,real(n)*memori_diff,3);call plot(0.1,real(n)*memori_diff,2)
                end if
                if(mod(n,symbol_freq)==0) then;call number(symbol_size*0.5,real(n)*memori_diff,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                else;end if
            end do
            call symbolr(1.4*symbol_size,length+memori_diff*1.4,symbol_size*0.7,'>',0.,1);call number(1.4*symbol_size,length+memori_diff*1.4,symbol_size*0.7,fin_num,0.,float_quantity)
        end if
    else;end if

    if(morethan==0 .and. lessthan==1) then
        if(angle == 0) then
            call newpen2(3)
            do n = 1, iterations
                call betsqk(real(n-1)*memori_diff,0.,real(n)*memori_diff,width,r(n),g(n),b(n))
            end do
            call plot(0.,0.,3);call betmlk(lefty_x,lefty_y,3,3,r(0),g(0),b(0))

            call newpen2(3);call plot(0.,0.,3);call plot(length,0.,2);call plot(0.,width,3);call plot(length,width,2)
            call plot(0.,0.,3);call plot(-memori_diff,width/2.,2);call plot(0.,width,2)
            do n = 0, iterations
                call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,width,2)
                if(mod(n,symbol_freq)/=0) then
                    call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,-0.05,2)
                else;call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,-0.1,2)
                end if
                if(mod(n,symbol_freq)==0) then;call numberc(real(n)*memori_diff,-1.4*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                else;end if
            end do
            call symbolr(-memori_diff*1.6,-2.0*symbol_size,symbol_size,'<',0.,1);call number(-memori_diff*1.6,-2.0*symbol_size,symbol_size,ini_num,0.,float_quantity)
        else !(angle == 90)
            call newpen2(3)
            do n = 1, iterations
                call betsqk(0.,real(n-1)*memori_diff,-width,real(n)*memori_diff,r(n),g(n),b(n))
            end do
            call plot(0.,0.,3);call betmlk(bottomy_x,bottomy_y,3,3,r(0),g(0),b(0))

            call newpen2(3);call plot(0.,0.,3);call plot(0.,length,2);call plot(-width,0.,3);call plot(-width,length,2)
            call plot(0.,0.,3);call plot(-width/2.,-memori_diff,2);call plot(-width,0.,2)
            do n = 0, iterations
                call plot(0.,real(n)*memori_diff,3);call plot(-width,real(n)*memori_diff,2)
                if(mod(n,symbol_freq)/=0) then
                    call plot(0.,real(n)*memori_diff,3);call plot(0.05,real(n)*memori_diff,2)
                else;call plot(0.,real(n)*memori_diff,3);call plot(0.1,real(n)*memori_diff,2)
                end if
                if(mod(n,symbol_freq)==0) then;call number(symbol_size*0.5,real(n)*memori_diff,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                else;end if
            end do
            call symbolr(1.4*symbol_size,-memori_diff*1.4,symbol_size*0.7,'<',0.,1);call number(1.4*symbol_size,-memori_diff*1.4,symbol_size*0.7,ini_num,0.,float_quantity)
        end if
    else;end if   

end subroutine

! creates colorscale for data quantity min and max are integers. array size for colors is (iterations)
subroutine colorscale_data(iterations,r,g,b,min,max,symbol_freq,symbol_size,length,width,angle)
    implicit none
    integer,intent(in)::iterations,min,max,symbol_freq,angle
    real,intent(in)::symbol_size,length,width
    real,dimension(iterations),intent(in)::r,g,b 
    integer::n,quotient
    real::memori_diff
    
    if(mod(max-min+1,iterations)/=0) then
        print*,'Something Wrong with the Number of Colors in subroutine (colorscale_data)'
    else;quotient = (max-min+1)/iterations
    end if

        memori_diff = length/real(iterations)
        if(angle == 0) then
            call newpen2(2)
            do n = 1,iterations
                call betsqk(real(n-1)*memori_diff,0.,real(n)*memori_diff,width,r(n),g(n),b(n))
            end do
            call plot(0.,0.,3);call plot(length,0.,2);call plot(0.,width,3);call plot(length,width,2)
            do n = 0, iterations
                call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,width,2)
            end do
            do n = 0,iterations-1
                call plot(real(n)*memori_diff+memori_diff/2.,0.,3);call plot(real(n)*memori_diff+memori_diff/2.,-0.1,2)
                if(mod(n,symbol_freq)==0) then
                    call numberc(real(n)*memori_diff+memori_diff/2.,-1.4*symbol_size,symbol_size,real(min)+real(n)*real(quotient),0.,-1)
                else;end if
            end do
        else !(angle == 90)
            call newpen2(2)
            do n = 1, iterations
                call betsqk(0.,real(n-1)*memori_diff,-width,real(n)*memori_diff,r(n),g(n),b(n))
            end do
            call plot(0.,0.,3);call plot(0.,length,2);call plot(-width,0.,3);call plot(-width,length,2)
            do n = 0, iterations
                call plot(0.,real(n)*memori_diff,3);call plot(-width,real(n)*memori_diff,2)
            end do
            do n = 0,iterations-1
                call plot(0.,real(n)*memori_diff+memori_diff/2.,3);call plot(0.1,real(n)*memori_diff+memori_diff/2.,2)
                if(mod(n,symbol_freq)==0) then
                    call number(1.4*symbol_size,real(n)*memori_diff+memori_diff/2.,symbol_size,real(min)+real(n)*real(quotient),0.,-1)
                else;end if
            end do
        end if

end subroutine

! an array of 12 bright colors
subroutine brightcolors(r,g,b)
    implicit none
    real,dimension(12),intent(out)::r,g,b
    integer::n

    do n = 1,2
        r(n)=1.;g(n)=real(n-1)/2.;b(n)=0.
    end do
    do n = 1,2
        r(n+2)=1.-real(n-1)/2.;g(n+2)=1.;b(n+2)= 0.
    end do
    do n = 1,2
        r(n+4)=0.;g(n+4)=1.;b(n+4)=real(n-1)/2.
    end do
    do n = 1,2
        r(n+6)=0.;g(n+6)=1.-real(n-1)/2.;b(n+6)=1.
    end do
    do n = 1,2
        r(n+8)=real(n-1)/2.;g(n+8)=0.;b(n+8)=1.
    end do
    do n = 1,2
        r(n+10)=1.;g(n+10)=0.;b(n+10)=1.-real(n-1)/2.
    end do

end subroutine

                                            ! Statistics !

! creates standard normal distribution curve for some fucking reason lol
subroutine standard_normal_distribution(length)
    implicit none
    intrinsic sin,cos,tan,asin,acos
    integer::n 
    real::x,y,pi
    real,intent(in)::length
    pi = 2.*asin(1.)
    call newpen2(2)
    call plot(0.,0.,3);call plot(length,0.,2);call plot(length,length,2);call plot(0.,length,2);call plot(0.,0.,2)
    call num_memori(-3.,3.,6,1,0.2,-1,length,0,0,0);call plot(length,0.,-3);call num_memori(0.,1.,10,1,0.2,1,length,90,0,0)
    call plot(-length/2.,0.,-3)
    do n = -100,100
        x = length/2.*real(n)/100.
        y = length*1./(1.*sqrt(2.*pi))*exp(-0.5*((x-0.)/1.)**2.)
        call gmark(x,y,0.02,1)
    end do
    call plot(-length/2.,0.,-3)
end subroutine

! t value for t distribution 95 percent confidence interval df<=30
subroutine t95_value(t95) !this is and array of 31 values, dimension is(0:30)
    implicit none
    real,dimension(0:30),intent(out)::t95

    t95(1) = 12.706 ; t95(11) = 2.2010 ; t95(21) = 2.0796
    t95(2) = 4.3026 ; t95(12) = 2.1788 ; t95(22) = 2.0739
    t95(3) = 3.1824 ; t95(13) = 2.1604 ; t95(23) = 2.0687
    t95(4) = 2.7765 ; t95(14) = 2.1448 ; t95(24) = 2.0639
    t95(5) = 2.5706 ; t95(15) = 2.1315 ; t95(25) = 2.0595
    t95(6) = 2.4469 ; t95(16) = 2.1191 ; t95(26) = 2.0555
    t95(7) = 2.3646 ; t95(17) = 2.1098 ; t95(27) = 2.0518
    t95(8) = 2.3060 ; t95(18) = 2.1009 ; t95(28) = 2.0484
    t95(9) = 2.2621 ; t95(19) = 2.0930 ; t95(29) = 2.0452
    t95(10) = 2.2281 ;t95(20) = 2.0860 ; t95(30) = 2.0423

    t95(0) = 0. !just for the sake of programs
end subroutine

! welch's t test for difference in 2 population means(mean1-mean2). A difference with a = 0.05 on both sides is returned as 0,otherwise 1. 119 means insufficient data or fuck you
subroutine welchttest(mean1,s1,dataquan1,mean2,s2,dataquan2,result,larger_or_smaller)
    implicit none
    real,intent(in)::mean1,s1,mean2,s2
    integer,intent(in)::dataquan1,dataquan2
    integer,intent(out)::result
    real,dimension(0:30)::t_95=0.
    real::diff_mean,n1,n2,df,sem,bottomCI,topCI
    character::larger_or_smaller*5

    if(mean1 /=0. .and. mean2/=0. .and. dataquan1/=0 .and. dataquan2/=0) then
        diff_mean = mean1 - mean2
        n1 = real(dataquan1);n2 = real(dataquan2)
        sem = sqrt((s1**2./n1)+(s2**2./n2)) 
        df = (((s1**2.)/n1)+((s2**2.)/n2))**2./(((s1**2./n1)**2./(n1-1))+((s2**2./n2)**2./(n2-1)))
        call t95_value(t_95)
        bottomCI = diff_mean - t_95(int(df))*sem ; topCI = diff_mean + t_95(int(df))*sem
        ! print*,diff_mean,bottomCI,topCI,int(df)
        if(bottomCI>0.) then
            result = 0;larger_or_smaller = 'LARGE'
        else if(topCI<0.) then
            result = 0;larger_or_smaller = 'small'
        else;result = 1;larger_or_smaller = 'NODIF'
        end if
    else;result = 911;larger_or_smaller = '911!!'
    end if

end subroutine

                                            ! Statistics !


