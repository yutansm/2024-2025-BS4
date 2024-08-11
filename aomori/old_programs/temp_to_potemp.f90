program temptopotemp
    implicit none
    integer,parameter::month=12,seasons=4,year_start=2008,year_max=15,station_y=2,station_x=9,depth_max=1000 !station_xは深層見れない時7
    !line_num=緯度別の観測、st_x=経度別の観測列、年、月、深さ、塩分または水温
    real,dimension(year_max,month,station_y,station_x,depth_max)::temp_5,sal_5,potemp_5
    ! real,dimension(depth_max+25)::datacolumn_shuffle,datacolumn_org
    character::yyyy*9,filename*999,newfilename*999,line_name*20,mm*9,months*9,tempfile*999,salfile*999
    character(len=*), parameter::temppass = 'Yuta_unedited'           !unedited temperature
    character(len=*), parameter::salpass = 'Yuta_edit_median_potemp'  !median filtered salinity
    character(len=*), parameter::newpass = 'Yuta_edit_median_potemp'

    integer::stations,m,line_num,y,x,k,ini_dep,year,loop,i,j,kagain,xagain
    real::numvault,median,potemp,temp,sal
    external::potemp_tem_sal_cal


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
            
            tempfile = trim(temppass)//'/'//trim(line_name)//'/'//trim(mm)//'/01tem'//trim(yyyy)//'.csv'   !unedited temperature を読む
            salfile = trim(salpass)//'/'//trim(line_name)//'/'//trim(mm)//'/sal_51mdn'//trim(yyyy)//'.csv'
            open(31,file = tempfile, status = 'old', action = 'read')
            do k=1, depth_max
                read(31,102)(temp_5(y,m,line_num,x,k), x=1,station_x)
            end do
            close(31)
            open(32,file = salfile,status = 'old', action = 'read')
            do k = 1,depth_max
                read(32,102)(sal_5(y,m,line_num,x,k), x=1,station_x)
            end do
            close(32)
        end do
    end do
end do   !temp_5とsal_5の配列にデータが入った
print*, temp_5(1,12,1,4,10)


do m = 1,month
    write(months,'(i2.2)') m
    do line_num = 1,2
        if (line_num==1) then
            line_name = 'S-Line'
        else
            line_name = 'N-Line'
        end if
            do y = 1,year_max
                year = y + year_start
                write(yyyy,'(i4.4)') year
                do x = 1, station_x
                    do k = 1,depth_max   
                        temp = temp_5(y,m,line_num,x,k)
                        sal = sal_5(y,m,line_num,x,k) 

                        if(temp/=0. .and. sal>30.) then  !tempとsalのデータがある，もしくは妥当なものであるなら，
                        call potemp_tem_sal_cal(potemp,temp,sal,real(k))     !depth(k) = 1の時のpotempから順に算出
                        potemp_5(y,m,line_num,x,k) = potemp                                                     !初めて使うpotemp_5の配列に入れる
                        
                        else
                        potemp_5(y,m,line_num,x,k) = 0.
                        end if
                    end do !end of k depth
                end do !end of x station_x
                    filename = trim(newpass)//'/'//trim(line_name)//'/'//trim(months)//'/'//'potemp_tem_sal51mdn'//trim(yyyy)//'.csv'
                    ! print *, filename
                    ! print*, mm
                    open(7,file = filename, status = 'replace')
                        do kagain = 1,depth_max
                            write(7,102)(potemp_5(y,m,line_num,xagain,kagain), xagain = 1,station_x)
                        end do   !新しいファイルがyyyy*mm*line_num 分作成されていることを願う限り
                       
                end do !end of y years
            end do  !     end do !end of line num
        end do  ! end do !end of m month

        close(7)
call potemp_tem_sal_cal(potemp,10.,35.,400.)
print*, potemp

end program

subroutine potemp_tem_sal_cal(potemp,tem,sal,depth)
    real,intent(in)::tem,sal,depth
    real,intent(out)::potemp
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
    double precision::d,S,t,p,rho,rhow,K,Kt,A,B,Kw,Aw,Bw,rhoafter
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
        potemp=Theta
        end if  
    
    
    end subroutine