! median potemp data into potemp_5 array
subroutine create_potemp_5(year_max,month,station_y,station_x,depth_max,potemp_5)
    implicit none
    integer,intent(in)::year_max,month,station_y,station_x,depth_max
    real,dimension(year_max,month,station_y,station_x,depth_max),intent(out)::potemp_5
    character::aa*9,line_name*20,moon*9,filename*999
    character(len=*), parameter::pass_CTD = "/LARGE0/gr10291/nishimori2/aomori/Yuta_edit_median_potemp"
    integer::y,m,line_num,x,k,i,year
    integer,parameter::year_start = 2008

    102 format(9(f9.4))
    do line_num=1,station_y
        if (line_num==1) then
            line_name='N-Line'
        else if (line_num==2) then
            line_name='S-Line'  
        else;end if 
        do m=1,month
        write(moon,'(i2.2)') m  
        do y=1,year_max
        year=y+year_start;write(aa,'(i4.4)') year
        filename=trim(pass_CTD)//'/'//trim(line_name)//'/'//trim(moon)//'/'//'potemp_tem_sal51mdn'//trim(aa)//'.csv'  
        open(31,file=filename,status='old',action='read')
        do k=1,depth_max
            read(31,102) (potemp_5(y,m,line_num,i,k),i=1,station_x)
        end do
        close(31)
    end do
    end do;end do

end subroutine

! median salinity data into sal_5 array
subroutine create_sal_5(year_max,month,station_y,station_x,depth_max,sal_5)
    implicit none
    integer,intent(in)::year_max,month,station_y,station_x,depth_max
    real,dimension(year_max,month,station_y,station_x,depth_max),intent(out)::sal_5
    character::aa*9,line_name*20,moon*9,filename*999
    character(len=*), parameter::pass_CTD = "/LARGE0/gr10291/nishimori2/aomori/Yuta_edit_median_potemp"
    integer::y,m,line_num,x,k,i,year
    integer,parameter::year_start = 2008

    102 format(9(f9.4))
    do line_num=1,station_y
        if (line_num==1) then
            line_name='N-Line'
        else if (line_num==2) then
            line_name='S-Line'  
        else;end if 
        do m=1,month
        write(moon,'(i2.2)') m  
        do y=1,year_max
        year=y+year_start;write(aa,'(i4.4)') year
        filename=trim(pass_CTD)//'/'//trim(line_name)//'/'//trim(moon)//'/sal_51mdn'//trim(aa)//'.csv'
        open(32,file=filename,status='old',action='read')
        do k=1,depth_max
            read(32,102) (sal_5(y,m,line_num,i,k),i=1,station_x)
        end do
        close(32)

    end do
    end do;end do

end subroutine

