program random_shit
    implicit none
    integer,parameter::years = 15, months = 12, lines = 2,stations = 9,depth = 400
    real,parameter::omega = 7.2921*(10.**(-5.))
    ! real,parameter:: width1 =18., height1 = 12.,width2 = 4.,height2 = 3.
    real,dimension(years,months,lines,stations,depth)::potemp_c5, sal_c5, sigma_c5,DH_array
    real,dimension(years,months,lines)::geotrans
    real,dimension(years,months)::SSH,SSP,calibratedSSH
    real,dimension(months,lines):: geo_av,geo_sd
    integer,dimension(months,lines)::geo_data
    real,dimension(months)::SSHav,SSHsem
    integer,dimension(months)::SSHdata
    integer,dimension(2)::location
    ! character::mm*9,filename1*999,filename2*999,filename3*999,line*9,datatype*99
    integer::y,m,l,st,d,n,i,result
    real::dx,dy,SEM,pi,delta_x

    call calibrated_tappiSSH(SSH)
    do y = 1, years
        do m = 1, months
            print*,SSH(y,m)
        end do
    end do


    call plots(2.,16.,13,'/LARGE0/gr10291/nishimori2/aomori/Subroutines/testfile.ps')
    call create_box(22.,6.,3)
!     call symbol(3.,2.5,0.8,'type random shit like you always do here bitch',0.,len('type random shit like you always do here bitch'))
!     call create_box(10.,-6.,2);call plot(10.+3.,0.,-3);call create_box(10.,-6.,2)
!     call plot(-13.,-(6.+3.),-3);call create_box(10.,-6.,2);call plot(10.+3.,0.,-3);call create_box(10.,-6.,2)
    
!     call calibrated_data51(potemp_c5,sal_c5)
!     call create_sigma_array(potemp_c5,sal_c5,sigma_c5)
!     call avsdsem_dataquan(potemp_c5,potemp_av,potemp_sd,potemp_sem,potemp_data)
!     call avsdsem_dataquan(sal_c5,sal_av,sal_sd,sal_sem,sal_data)
!     call avsdsem_dataquan(sigma_c5,sigma_av,sigma_sd,sigma_sem,sigma_data)
    

!     call create_DH_array(sigma_c5,DH_array)
!     call avsdsem_dataquan(DH_array,DH_av,DH_sd,DH_sem,DH_data)
    
!     call geotransport(51,geotrans)
!     call avsd_dataquan2(geotrans,geo_av,geo_sd,geo_data)
! do l =1,1
!     do m = 1,12
!         print*,m,geo_av(m,l),geo_sd(m,l),geo_data(m,l)
!     end do
! end do
    
!     do m = 2,months
!         l = 1
!         ! print*,m,geo_av(m,l),geo_sd(m,l),geo_data(m,l)
!         ! print*,m-1,geo_av(m-1,l),geo_sd(m-1,l),geo_data(m-1,l)
!         call welchttest(geo_av(m,l),geo_sd(m,l),geo_data(m,l),geo_av(m-1,l),geo_sd(m-1,l),geo_data(m-1,l),result)
!         print*,m,m-1,result
!     end do

! call welchttest(3.,0.5,15,3.,0.3,14,result)
! print*,result




!     pi = 3.1415926535
!     print*,omega,2.*omega*sin(41.*pi/180.)
!     print*,'delta_x = ',2.*pi*6378.*1000.*cos(41.*pi/180.)/360.*1./3.
!     print*, 1./(2.*omega*sin(41.*pi/180.)*2.*pi*6378.*1000.*cos(41.*pi/180.)/360.*1./3.)*100.

 

    ! do l = 1, lines
    !     if (l == 1) then; line = 'N-Line';else;line = 'S-Line';end if
    !     do n = 1,3
    !         if(n == 1)then;datatype = '51potemp';else if(n == 2)then;datatype = '51sal';else;datatype = 'sigma_theta';end if
    !         do m = 1, months
    !             write(mm,'(i2.2)')m
    !             ! filename1 = '/LARGE0/gr10291/nishimori2/aomori/Monthly_avsdsem_csv/'//trim(line)//'/'//trim(datatype)//'/'//trim(mm)//'mean.csv'
    !             filename2 = '/LARGE0/gr10291/nishimori2/aomori/Monthly_avsdsem_csv/'//trim(line)//'/'//trim(datatype)//'/'//trim(mm)//'SEM.csv'
    !             ! filename3 = '/LARGE0/gr10291/nishimori2/aomori/Monthly_avsdsem_csv/'//trim(line)//'/'//trim(datatype)//'/'//trim(mm)//'data_quantity.csv'
    !             ! open(11,file = filename1, status = 'replace')
    !             open(22,file = filename2, status = 'replace')
    !             ! open(33,file = filename3, status = 'replace')

    !             if(n == 1) then
    !                 do d = 1, depth
    !                     ! write(11,102)(potemp_av(m,l,st,d),st = 1,stations)
    !                     write(22,102)(potemp_sem(m,l,st,d),st = 1,stations)
    !                     ! write(33,103)(potemp_data(m,l,st,d),st = 1,stations)
    !                 end do
    !             else if(n ==2) then
    !                 do d = 1, depth
    !                     ! write(11,102)(sal_av(m,l,st,d),st = 1,stations)
    !                     write(22,102)(sal_sem(m,l,st,d),st = 1,stations)
    !                     ! write(33,103)(sal_data(m,l,st,d),st = 1,stations)
    !                 end do
    !             else
    !                 do d = 1, depth
    !                     ! write(11,102)(sigma_av(m,l,st,d),st = 1,stations)
    !                     write(22,102)(sigma_sem(m,l,st,d),st = 1,stations)
    !                     ! write(33,103)(sigma_data(m,l,st,d),st = 1,stations)
    !                 end do
    !             end if
    !         end do
    !     end do
    ! end do



call plote
end program