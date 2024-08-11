program temp_to_potemp
    use ifport
    implicit none
    logical(4)result
    integer::y,m,l,st,d
    integer,parameter::years=15,months=12,lines=2,stations=9,depth=400
    character::line*9,mm*9,yyyy*9,tempfile*999,salfile*999
    real,dimension(years,months,lines,stations,depth)::temp_5,sal_5,potemp_5
    real::potemp
102 format(9(f9.4))
! do l = 1,2
!         if(l ==1) then;line = 'N-Line'
!         else;line = 'S-Line'
!         end if
!         result = makedirqq('/LARGE0/gr10291/nishimori2/aomori/25_Median/'//trim(line))
!             do m = 1, months
!                 write(mm,'(i2.2)')m
!                 result = makedirqq('/LARGE0/gr10291/nishimori2/aomori/25_Median/'//trim(line)//'/'//trim(mm))
!             end do
!     end do

! end program

    ! call potempsal_51(temp_5,sal_5) もう使えないよ

    do l = 1, lines
        if(l==1) then;line = 'N-Line';else; line='S-Line';end if
        do m = 1, months
            write(mm,'(i2.2)')m
            do y = 1, years
                write(yyyy,'(i4.4)')y+2008
                do st = 1, stations
                    do d = 1, depth
                        call potemp_T_S_depth(potemp,temp_5(y,m,l,st,d),sal_5(y,m,l,st,d),real(d))
                        potemp_5(y,m,l,st,d)=potemp
                    end do
                end do !end of stations
                tempfile = '/LARGE0/gr10291/nishimori2/aomori/51_Median/'//trim(line)//'/'//trim(mm)//'/'//'51potemp'//trim(yyyy)//'.csv'
                salfile = '/LARGE0/gr10291/nishimori2/aomori/51_Median/'//trim(line)//'/'//trim(mm)//'/'//'51sal'//trim(yyyy)//'.csv'
                open(11,file=tempfile,status = 'replace')
                do d = 1,depth
                    write(11,102)(potemp_5(y,m,l,st,d),st = 1, stations)
                end do
                open(11,file=salfile,status = 'replace')
                do d = 1,depth
                    write(11,102)(sal_5(y,m,l,st,d),st = 1, stations)
                end do
                close(11)
            end do
        end do
    end do

end program


