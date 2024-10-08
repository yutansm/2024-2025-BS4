module constants
    implicit none
    integer, parameter :: years = 15, months = 12, lines = 2, stations = 9, depth = 400
    real,dimension(years,months,lines,stations,depth):: temp_5=0.,potemp_5=0.,sal_5=0.,sigma_5=0.,potemp_c5=0.,sal_c5=0.,sigma_c5=0.,geovel_5=0.
    real,dimension(:),allocatable::r,g,b,r1,g1,b1,r2,g2,b2,r3,g3,b3,r4,g4,b4,r5,g5,b5,r6,g6,b6
    integer::y=0,m=0,l=0,st=0,d=0,i=0,j=0,n=0,x=0,z=0,h=0
    real::dx=0.,dy=0.
end module constants

module psstat
    implicit none
    logical :: stoff, land, pageend
    integer :: ipage
    real :: xorig, yorig
    integer,parameter::ounit = 16
end module psstat

module qbase
    implicit none
    real::qcxp,qcyp
    integer::ip
end module qbase

module subroutines 
    implicit none
    contains
! COLORGRAD
    ! blue to red
    subroutine b2r_colorgrad(iterations,midpoint,r,g,b)
        implicit none
        integer,intent(in)::iterations,midpoint
        real,dimension(:),allocatable,intent(out)::r,g,b
        integer::n
        
        allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1))
        do n = 1, iterations
            if (n <= midpoint) then 
                r(n) = 0.+(real(n-1)/real(midpoint-1))*0.85
                g(n) = 0.+(real(n-1)/real(midpoint-1))*0.85
                b(n) = 1.
            else
                r(n) = 1.
                g(n) = 0.85-(real(n-midpoint-1)/real(iterations-midpoint-1))*0.85
                b(n) = 0.85-(real(n-midpoint-1)/real(iterations-midpoint-1))*0.85
            end if
        end do

        r(0) = 0.; g(0) = 0.; b(0) = 0.8
        r(iterations+1) = 0.8 ; g(iterations+1) = 0. ; b(iterations+1) = 0.

    end subroutine

    ! blue to grey to red
    subroutine b2gy2r_colorgrad(iterations,midpoint,r,g,b)
        implicit none
        integer,intent(in)::iterations,midpoint
        real,dimension(:),allocatable,intent(out)::r,g,b
        integer::n
        
        allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1))
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

    ! red to green
    subroutine r2g_colorgrad(iterations,midpoint,r,g,b)
        implicit none
        integer,intent(in)::iterations,midpoint
        real,dimension(:),allocatable,intent(out)::r,g,b
        integer::n 

        allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1))
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

    ! black to red, then gradient from red to green. 
    subroutine bk2r2g_colorgrad(iterations,midpoint,r,g,b)
        implicit none
        integer,intent(in)::iterations,midpoint
        real,dimension(:),allocatable,intent(out)::r,g,b 
        integer::n 

        allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1))
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

    ! b<breakpoint1>g<breakpoint2>y<breakpoint3>r. bluecyan==dark->light, yellowred==light->dark
    subroutine b2cy2y2r_colorgrad(iterations,breakpoint1,breakpoint2,breakpoint3,r,g,b)
        implicit none
        integer,intent(in)::iterations,breakpoint1,breakpoint2,breakpoint3
        real,dimension(:),allocatable,intent(out)::r,g,b
        integer::n

        allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1))
        r(0)=0.;g(0)=0.;b(0)=0.9
        ! from dark blue to light blue
        do n = 1, breakpoint1
            r(n) = 0.+(real(n-1)/real(breakpoint1-1))*0.8
            g(n) = 0.+(real(n-1)/real(breakpoint1-1))*0.8
            b(n) = 1.
        end do
        ! from dark cyan to light cyan
        do n = 1,breakpoint2-breakpoint1
            r(n+breakpoint1) = (real(n-1)/real(breakpoint2-breakpoint1-1))*0.9
            g(n+breakpoint1) = 1.
            b(n+breakpoint1) = 1.
        end do

        ! from light yellow to dark yellow
        do n = 1, breakpoint3-breakpoint2
            r(n+breakpoint2) = 1.
            g(n+breakpoint2) = 1.
            b(n+breakpoint2) = 0.9-(real(n-1)/real(breakpoint3-breakpoint2-1))*0.9
        end do
        ! from light red to dark red
        do n = 1, iterations-breakpoint3
            r(n+breakpoint3) = 1.
            g(n+breakpoint3) = 0.9-(real(n-1)/real(iterations-breakpoint3-1))*0.9
            b(n+breakpoint3) = 0.9-(real(n-1)/real(iterations-breakpoint3-1))*0.9
        end do
        r(iterations+1) = 0.9;g(iterations+1) = 0.;b(iterations+1) = 0.
    end subroutine

    ! b,zeropoint,g->y->r. blue==dark->light, greenyellowred==light->dark
    subroutine b2g2y2r_colorgrad(iterations,breakpoint1,breakpoint2,breakpoint3,r,g,b)
        implicit none
        integer,intent(in)::iterations,breakpoint1,breakpoint2,breakpoint3
        real,dimension(:),allocatable,intent(out)::r,g,b
        integer::n

        allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1))
        r(0)=0.;g(0)=0.;b(0)=0.9
        ! from dark blue to light blue
        do n = 1, breakpoint1
            r(n) = 0.+(real(n-1)/real(breakpoint1-1))*0.8
            g(n) = 0.+(real(n-1)/real(breakpoint1-1))*0.8
            b(n) = 1.
        end do
        ! from dark green to light green
        do n = 1,breakpoint2-breakpoint1
            r(n+breakpoint1) = 0.9-(real(n-1)/real(breakpoint2-breakpoint1-1))*0.9
            g(n+breakpoint1) = 1.
            b(n+breakpoint1) = 0.9-(real(n-1)/real(breakpoint2-breakpoint1-1))*0.9
        end do

        ! from light yellow to dark yellow
        do n = 1, breakpoint3-breakpoint2
            r(n+breakpoint2) = 1.
            g(n+breakpoint2) = 1.
            b(n+breakpoint2) = 0.9-(real(n-1)/real(breakpoint3-breakpoint2-1))*0.9
        end do
        ! from light red to dark red
        do n = 1, iterations-breakpoint3
            r(n+breakpoint3) = 1.
            g(n+breakpoint3) = 0.9-(real(n-1)/real(iterations-breakpoint3-1))*0.9
            b(n+breakpoint3) = 0.9-(real(n-1)/real(iterations-breakpoint3-1))*0.9
        end do
        r(iterations+1) = 0.9;g(iterations+1) = 0.;b(iterations+1) = 0.
    end subroutine

    ! an array of 12 bright colors
    ! subroutine brightcolors(r,g,b)
    !     implicit none
    !     real,dimension(:),allocatable,intent(out)::r,g,b
    !     integer::n

    !     allocate(r(12),g(12),b(12))
    !     do n = 1,2
    !         r(n)=1.;g(n)=real(n-1)/2.;b(n)=0.
    !     end do
    !     do n = 1,2
    !         r(n+2)=1.-real(n-1)/2.;g(n+2)=1.;b(n+2)= 0.
    !     end do
    !     do n = 1,2
    !         r(n+4)=0.;g(n+4)=1.;b(n+4)=real(n-1)/2.
    !     end do
    !     do n = 1,2
    !         r(n+6)=0.;g(n+6)=1.-real(n-1)/2.;b(n+6)=1.
    !     end do
    !     do n = 1,2
    !         r(n+8)=real(n-1)/2.;g(n+8)=0.;b(n+8)=1.
    !     end do
    !     do n = 1,2
    !         r(n+10)=1.;g(n+10)=0.;b(n+10)=1.-real(n-1)/2.
    !     end do

    ! end subroutine

    subroutine dozencolors(r,g,b)
        real, dimension(:),allocatable,intent(out) :: r, g, b
        allocate (r(12), g(12), b(12))
        r = (/ 1.0, 0.0, 0.0, .8, 0.0, 1.0, 1.0, 0.75, 1.0, 0.5, 0.0, 0.65 /)
        g = (/ 0.0, 1.0, 0.0, .8, 1.0, 0.0, 0.65, 1.0, 0.41, 0.0, 0.5, 0.16 /)
        b = (/ 0.0, 0.0, 1.0, 0.0, 1.0, .8, 0.0, 0.0, 0.71, 0.5, 0.5, 0.16 /)
    end subroutine
! END COLORGRAD
! PLOTS
    subroutine create_box(width,height,thickness,x,y)
        implicit none
        real,intent(in)::width,height
        real,intent(in),optional::x,y
        integer,intent(in)::thickness
        if(present(x).and.present(y))call plot(x,y,-3)
        if(present(x).and. .not.present(y))call plot(x,0.,-3)
        if(present(y).and. .not.present(x))call plot(0.,y,-3)
        call newpen2(thickness)
        call plot(0.,0.,3);call plot(width,0.,2);call plot(width,height,2);call plot(0.,height,2);call plot(0.,0.,2)
        if(present(x).and.present(y))call plot(-x,-y,-3)
        if(present(x).and. .not.present(y))call plot(-x,0.,-3)
        if(present(y).and. .not.present(x))call plot(0.,-y,-3)
    end subroutine    
    subroutine floating_numbers(ini_num,num_inc,iterations,symbol_size,x_inc,y_inc,rangle,float_quantity,x,y)
        real,intent(in)::symbol_size,x_inc,y_inc,rangle,ini_num,num_inc
        real,intent(in),optional::x,y
        integer,intent(in)::iterations,float_quantity
        integer::n
        real::printnum

        if(present(x).and.present(y))call plot(x,y,-3)
        if(present(x).and. .not.present(y))call plot(x,0.,-3)
        if(present(y).and. .not.present(x))call plot(0.,y,-3)
        do n = 1, iterations
            printnum = ini_num + num_inc*real(n-1)
            call numberc(real(n-1)*x_inc,real(n-1)*y_inc,symbol_size,printnum,rangle,float_quantity)
        end do
        if(present(x).and.present(y))call plot(-x,-y,-3)
        if(present(x).and. .not.present(y))call plot(-x,0.,-3)
        if(present(y).and. .not.present(x))call plot(0.,-y,-3)
    end subroutine
    subroutine floating_lines(length,rangle,iterations,line_thickness,x_inc,y_inc,x,y)
        use psstat
        real,intent(in)::length,x_inc,y_inc,rangle
        real,intent(in),optional::x,y
        integer,intent(in)::iterations,line_thickness
        integer::n

        if(present(x).and.present(y))call plot(x,y,-3)
        if(present(x).and. .not.present(y))call plot(x,0.,-3)
        if(present(y).and. .not.present(x))call plot(0.,y,-3)

        call newpen2(line_thickness)
        write(ounit,*) "% begin floating_lines"
        do n = 1, iterations
            write(ounit,'(f10.4,2x,a4)' ) rangle , ' ro ' 
            call plot(0.,0.,3);call plot(length,0.,2)
            write(ounit,'(f9.4,2x,a4)' ) -rangle , ' ro ' 
            call plot(x_inc,y_inc,-3)
        end do
        write(ounit,*) "% end floating_lines"
        call plot(-real(iterations)*x_inc,-real(iterations)*y_inc,-3)

        if(present(x).and.present(y))call plot(-x,-y,-3)
        if(present(x).and. .not.present(y))call plot(-x,0.,-3)
        if(present(y).and. .not.present(x))call plot(0.,-y,-3)
        return
    end subroutine
    ! defaults for optional arguments are, rangle=0.,symbol_start=0,symbol_TorB='B',lessthan=0,morethan=0,symbol_start=0,x=0.,y=0.
    subroutine colorscale(iterations,r,g,b,ini_num,fin_num,symbol_freq,symbol_size,float_quantity,length,width,lessthan,morethan,rangle,symbol_TorB,symbol_start,x,y)
        use psstat
        implicit none
        integer,intent(in)::iterations,symbol_freq,float_quantity
        real,intent(in)::ini_num,fin_num,symbol_size,length,width
        integer,intent(in),optional::lessthan,morethan,symbol_start
        character(len=*),intent(in),optional::symbol_TorB
        real,intent(in),optional::rangle,x,y
        real,dimension(0:iterations+1),intent(in)::r,g,b
        integer::n,intquan
        real::memori_diff,num_diff
        real,dimension(3)::lefty_x=0.,lefty_y=0.,righty_x=0.,righty_y=0.
        character(len=20)::min,max,format1,format2
        do n = 1,10
            if(ini_num/(10.**real(n))>=1.)then;cycle
            else;intquan=n;exit;end if
        end do
        if(float_quantity>=0)then
            write(format1,'(A,I0,A,I0,A)') "(f",intquan+float_quantity+3,'.',float_quantity,")"
            write(min,format1)ini_num
        else;write(format1,'(A,I0,A)') "(i",intquan+2,")"
            write(min,format1)int(ini_num)
        end if
        ! write(min,format1)ini_num
        do n = 1,10
            if(fin_num/(10.**real(n))>=1.)then;cycle
            else;intquan=n;exit;endif
        end do
        if(float_quantity>=0)then
            write(format2,'(A,I0,A,I0,A)') "(f",intquan+float_quantity+3,'.',float_quantity,")"
            write(max,format2)fin_num
        else;write(format2,'(A,I0,A)') "(i",intquan+2,")"
            write(max,format2)int(fin_num)
        end if
        ! write(max,format2)fin_num
        ! print*,format1,format2,max,min

        memori_diff = length/real(iterations); num_diff = (fin_num-ini_num)/real(iterations)

        lefty_x(1) = 0.;lefty_x(2) = -memori_diff; lefty_x(3) = 0.
        lefty_y(1) = 0.;lefty_y(2) = width/2. ;lefty_y(3) = width
        righty_x(1) = length ;righty_x(2) = length+memori_diff ;righty_x(3) = length
        righty_y(1) = 0. ;righty_y(2) = width/2. ;righty_y(3) = width

        if(present(x).and.present(y))call plot(x,y,-3)
        if(present(x).and. .not.present(y))call plot(x,0.,-3)
        if(present(y).and. .not.present(x))call plot(0.,y,-3)
        write(ounit,*) "% begin colorscale"
            if(present(rangle)) then
                write(ounit,'(f10.4,2x,a4)' ) rangle , ' ro '
            end if
            call newpen2(3)
            do n = 1, iterations
                call betsqk(real(n-1)*memori_diff,0.,real(n)*memori_diff,width,r(n),g(n),b(n))
            end do

            call plot(0.,0.,3);call plot(length,0.,2);call plot(0.,width,3);call plot(length,width,2);call plot(0.,0.,3)
            if(present(lessthan))then
                call betmlk(lefty_x,lefty_y,3,3,r(0),g(0),b(0));call newpen2(3);call plot(0.,0.,3)
                call plot(lefty_x(1),lefty_y(1),3)
                call plot(lefty_x(2),lefty_y(2),2);call plot(lefty_x(3),lefty_y(3),2)
            end if
            if(present(morethan))then
                call plot(0.,0.,3)
                call betmlk(righty_x,righty_y,3,3,r(iterations+1),g(iterations+1),b(iterations+1));call plot(0.,0.,3)
                call newpen2(3);call plot(righty_x(1),righty_y(1),3);call plot(righty_x(2),righty_y(2),2);call plot(righty_x(3),righty_y(3),2)
            end if
            do n = 0, iterations
                call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,width,2)
                if(present(symbol_TorB).and.symbol_TorB=='T')then
                    call plot(real(n)*memori_diff,width,3);call plot(real(n)*memori_diff,width+symbol_size/8.,2)
                else
                    call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,-symbol_size/8.,2)
                end if
                if(present(symbol_start))then
                    if(mod(n-symbol_start,symbol_freq)==0 ) then
                        if(present(symbol_TorB).and.symbol_TorB=='T')then
                            call plot(real(n)*memori_diff,width,3);call plot(real(n)*memori_diff,width+symbol_size/4.,2)
                            if(present(rangle).and.rangle/=0.) then
                                call numberc(real(n)*memori_diff,width+1.5*symbol_size,symbol_size,ini_num+num_diff*real(n),-rangle,float_quantity)
                            else;call numberc(real(n)*memori_diff,width+.5*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                            end if
                        else ! symbol_TorB=='B' or nada
                            call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,-symbol_size/4.,2)
                            if(present(rangle).and.rangle/=0.) then
                                call numberc(real(n)*memori_diff,-1.6*symbol_size,symbol_size,ini_num+num_diff*real(n),-rangle,float_quantity)
                            else;call numberc(real(n)*memori_diff,-1.3*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                            end if
                        end if
                    end if
                else if(.not.present(symbol_start))then
                    if(mod(n,symbol_freq)==0 ) then
                        if(present(symbol_TorB).and.symbol_TorB=='T')then
                            call plot(real(n)*memori_diff,width,3);call plot(real(n)*memori_diff,width+symbol_size/4.,2)
                            if(present(rangle).and.rangle/=0.) then
                                call numberc(real(n)*memori_diff,width+1.5*symbol_size,symbol_size,ini_num+num_diff*real(n),-rangle,float_quantity)
                            else;call numberc(real(n)*memori_diff,width+.5*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                            end if
                        else ! symbol_TorB=='B' or nada
                            call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,-symbol_size/4.,2)
                            if(present(rangle).and.rangle/=0.) then
                                call numberc(real(n)*memori_diff,-1.6*symbol_size,symbol_size,ini_num+num_diff*real(n),-rangle,float_quantity)
                            else;call numberc(real(n)*memori_diff,-1.3*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                            end if
                        end if
                    end if
                end if
            end do

        if(present(lessthan))then
            if(present(symbol_TorB).and.symbol_TorB=='T')then
                if(present(rangle).and.rangle/=0.)then
                    call symbolc(-1.2*symbol_size,+1.5*symbol_size,symbol_size*0.7,'<'//trim(min),-rangle,len('<'//trim(min)))
                else;call symbolc(-2.*symbol_size,+1.*symbol_size,symbol_size*0.7,'<'//trim(min),0.,len('<'//trim(min)))
                end if
            else ! symbol_TorB=='B' or nada
                if(present(rangle).and.rangle/=0.)then
                    call symbolc(-1.2*symbol_size,-1.0*symbol_size,symbol_size*0.7,'<'//trim(min),-rangle,len('<'//trim(min)))
                else;call symbolc(-2.*symbol_size,-0.5*symbol_size,symbol_size*0.7,'<'//trim(min),0.,len('<'//trim(min)))
                end if
            end if
        end if
        if (present(morethan))then
            if(present(symbol_TorB).and.symbol_TorB=='T')then
                if(present(rangle).and.rangle/=0.)then
                    call symbolc(length+1.*symbol_size,+1.5*symbol_size,symbol_size*0.7,trim(max)//'<',-rangle,len('>'//trim(max)))
                else;call symbolc(length+2.*symbol_size,+1.*symbol_size,symbol_size*0.7,trim(max)//'<',0.,len('>'//trim(max)))
                end if
            else ! symbol_TorB=='B' or nada
                if(present(rangle).and.rangle/=0.)then
                    call symbolc(length+1.*symbol_size,-1.0*symbol_size,symbol_size*0.7,trim(max)//'<',-rangle,len('>'//trim(max)))
                else;call symbolc(length+2.*symbol_size,-0.5*symbol_size,symbol_size*0.7,trim(max)//'<',0.,len('>'//trim(max)))
                end if
            end if
        end if
        if(present(rangle)) then
            write(ounit,'(f9.4,2x,a4)' ) -rangle , ' ro '
        end if
        if(present(x).and.present(y))call plot(-x,-y,-3)
        if(present(x).and. .not.present(y))call plot(-x,0.,-3)
        if(present(y).and. .not.present(x))call plot(0.,-y,-3)
    end subroutine
! MEMORI
    subroutine memori(iterations,memori_size,bimemori_freq,length,rangle,x,y)
        use psstat
        implicit none
        real,intent(in)::memori_size,length,rangle
        integer,intent(in)::iterations,bimemori_freq
        real,intent(in),optional::x,y
        real::memori_diff
        integer::n
    
        if(present(x).and.present(y))call plot(x,y,-3)
        if(present(x).and. .not.present(y))call plot(x,0.,-3)
        if(present(y).and. .not.present(x))call plot(0.,y,-3)
        write(ounit,'(f10.4,2x,a4)' ) rangle , ' ro ' 
        call newpen2(3)
        memori_diff = length/real(iterations)
        do n = 0, iterations
            if(mod(n,bimemori_freq)==0) then
                call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,-memori_size*2.,2)
            else; call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,-memori_size,2)
            end if
        end do
        write(ounit,'(f9.4,2x,a4)' ) -rangle , ' ro ' 
        if(present(x).and.present(y))call plot(-x,-y,-3)
        if(present(x).and. .not.present(y))call plot(-x,0.,-3)
        if(present(y).and. .not.present(x))call plot(0.,-y,-3)

    end subroutine
    subroutine num_memori(ini_num,fin_num,iterations,symbol_freq,symbol_size,float_quantity,length,angle,lessthan,morethan,x,y)
        implicit none
        real,intent(in)::ini_num,fin_num,symbol_size,length
        integer,intent(in)::iterations,symbol_freq,angle,float_quantity
        integer,intent(in),optional::lessthan,morethan
        real,intent(in),optional::x,y
        real::memori_diff,num_diff
        integer::n
    
        if(present(x).and.present(y))call plot(x,y,-3)
        if(present(x).and. .not.present(y))call plot(x,0.,-3)
        if(present(y).and. .not.present(x))call plot(0.,y,-3)

        if(.not.present(lessthan).and..not.present(morethan)) then
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
    
        if(present(lessthan).and.present(morethan)) then
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
    
       if(present(morethan) .and. .not.present(lessthan)) then
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
    
        if(.not.present(morethan) .and. present(lessthan)) then
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

        if(present(x).and.present(y))call plot(-x,-y,-3)
        if(present(x).and. .not.present(y))call plot(-x,0.,-3)
        if(present(y).and. .not.present(x))call plot(0.,-y,-3)
    
    end subroutine
    subroutine st_memori(ini_st,fin_st,width,top_bottom,symbol_size,gap,x,y)
        implicit none
        integer,intent(in)::ini_st,fin_st,top_bottom,gap
        real,intent(in)::width,symbol_size
        real,intent(in),optional::x,y
        real::dx,gappy
        integer::n
        if(present(x).and.present(y))call plot(x,y,-3)
        if(present(x).and. .not.present(y))call plot(x,0.,-3)
        if(present(y).and. .not.present(x))call plot(0.,y,-3)
        if(symbol_size<=0.2) then;call newpen2(2)
        else if(symbol_size>=0.2 .and. symbol_size<=0.4) then;call newpen2(3)
        else;call newpen2(4);end if
        if(gap == 2) then
        dx = width/real(fin_st-ini_st+1);gappy = dx/2.
        else if(gap == 1) then
            dx = width/real(fin_st-ini_st+2);gappy = dx
        else if(gap == 0) then
            dx = width/real(fin_st-ini_st);gappy = 0.
        end if
        if(top_bottom == 1) then
            do n = 1,fin_st-ini_st+1
                call plot(gappy+real(n-1)*dx,0.,3);call plot(gappy+real(n-1)*dx,-symbol_size*0.2,2)
                call numberc(gappy+real(n-1)*dx,-symbol_size*1.3,symbol_size,real(fin_st-n+1),0.,-1)
            end do
        else if(top_bottom == 0) then
            do n = 1,fin_st-ini_st+1
                call plot(gappy+real(n-1)*dx,0.,3);call plot(gappy+real(n-1)*dx,symbol_size*0.2,2)
                call numberc(gappy+real(n-1)*dx,symbol_size*0.8,symbol_size,real(fin_st-n+1),0.,-1)
            end do
        end if 
        if(present(x).and.present(y))call plot(-x,-y,-3)
        if(present(x).and. .not.present(y))call plot(-x,0.,-3)
        if(present(y).and. .not.present(x))call plot(0.,-y,-3)
    end subroutine
    ! gap bw memori and box. gap=0:no gap, gap=1:dx, gap=2:dx/2. get dxval if needed
    subroutine mod12_memori(iterations,symbol_size,angle,length,inc_dec,gap,x,y,dxval)
        implicit none
        real,intent(in)::symbol_size,length
        real,intent(in),optional::x,y
        real,intent(out),optional::dxval
        integer,intent(in)::iterations,angle,inc_dec,gap
        real::dx,gappy
        integer::n,m,printm

        if(present(x).and.present(y))call plot(x,y,-3)
        if(present(x).and. .not.present(y))call plot(x,0.,-3)
        if(present(y).and. .not.present(x))call plot(0.,y,-3)

        if(gap == 2) then
            dx = length/real(iterations);gappy = dx/2.
            else if(gap == 1) then
                dx = length/real(iterations+1);gappy = dx
            else if(gap == 0) then
                dx = length/real(iterations-1);gappy = 0.
        end if
        if(present(dxval)) then;dxval = dx;else;end if
        if(angle==0) then
            call newpen2(3)
            call plot(0.,0.,3);call plot(length,0.,2)
            do n = 1,iterations
                if (mod(n,12)/=0) then;m = mod(n,12)
                else if(mod(n,12)==0) then;m = 12
                else;end if
                call plot(gappy+real(n-1)*dx,0.,3);call plot(gappy+real(n-1)*dx,-0.1,2)
                if(inc_dec == 1) then;printm = 13-m;else;printm = m;end if
                call numberc(gappy+real(n-1)*dx,-1.2*symbol_size,symbol_size,real(printm),0.,-1)
            end do
        else if(angle == -90) then
            call newpen2(3)
            call plot(0.,0.,3);call plot(0.,length,2)
            do n = 1,iterations
                if (mod(n,12)/=0) then;m = mod(n,12)
                else if(mod(n,12)==0) then;m = 12
                else;end if
                call plot(0.,gappy+real(n-1)*dx,3);call plot(-0.1,gappy+real(n-1)*dx,2)
                if(inc_dec == 1) then;printm = 13-m;else;printm = m;end if
                call numberc(-1.3*symbol_size,gappy+real(n-1)*dx,symbol_size,real(printm),0.,-1)
            end do
        end if

        if(present(x).and.present(y))call plot(-x,-y,-3)
        if(present(x).and. .not.present(y))call plot(-x,0.,-3)
        if(present(y).and. .not.present(x))call plot(0.,-y,-3)        

    end subroutine

! BASIC STATISTICS
    ! calculates mean, s, and sem per LOOP of a 1D array make sure LOOP*ITERATIONS < ARRAYSIZE, size of arrays is ITERATIONS
    subroutine avsemloop_1D(array_1D,dim1,loop,iterations,mean_1D,s_1D,sem_1D,dataquan_1D)
        implicit none
        integer,intent(in)::dim1,loop,iterations
        real,intent(in)::array_1D(:)
        real,dimension(iterations),intent(out),optional::mean_1D,s_1D,sem_1D
        integer,dimension(iterations),intent(out),optional::dataquan_1D
        integer::l,i,count=0
        real::smean, s,sem,sum0=0.,sum1=0.

        if(dim1 < loop*iterations) then;print*,'Array size < loop*iterations';stop;end if
        if(size(array_1D)/=dim1) then;print*,'Array size /= dim1';stop;end if
        do i = 1, iterations
            count = 0;sum0=0.;sum1 = 0.
            do l = 1+loop*(i-1),loop*i
                if(array_1D(l)/=0.) then
                    count = count +1;sum0 = sum0 + array_1D(l)
                end if
            end do
            if(count <=1) then
                smean = 0.;s = 0.;sem = 0.
            else
                smean = sum0 / real(count)
                do l = 1+loop*(i-1),loop*i
                    if(array_1D(l)/=0.) then
                        sum1 = sum1 + (array_1D(l) - smean)**2.
                    end if
                end do
                s = sqrt(sum1 / real(count-1))
                sem = s / sqrt(real(count))
            end if
            if(present(dataquan_1D)) dataquan_1D(i) = count
            if(present(mean_1D)) mean_1D(i) = smean
            if(present(s_1D)) s_1D(i) = s
            if(present(sem_1D)) sem_1D(i) = sem
        end do
    end subroutine
    ! calulates mean, s, and sem of numbers intermittently in a 1D array. JUMP is the number of elements to skip. array(jump)
    subroutine avsemjump_1D(array_1D,dim1,jump,loops,mean_1D,s_1D,sem_1D,dataquan_1D)
        implicit none
        integer,intent(in)::dim1,jump,loops
        real,intent(in)::array_1D(:)
        real,dimension(jump),intent(out),optional::mean_1D,s_1D,sem_1D
        integer,dimension(jump),intent(out),optional::dataquan_1D
        integer::l,i,count=0
        real::smean, s,sem,sum0=0.,sum1=0.

        if(dim1 < jump*loops) then;print*,'Array size < jump*iterations';stop;end if
        if(size(array_1D)/=dim1) then;print*,'Array size /= dim1';stop;end if
        do i = 1, jump
            count = 0;sum0=0.;sum1 = 0.
            do l = i,i+jump*(loops-1),jump
                if(array_1D(l)/=0.) then
                    count = count +1;sum0 = sum0 + array_1D(l)
                end if
            end do
            if(count <=1) then
                smean = 0.;s = 0.;sem = 0.
            else
                smean = sum0 / real(count)
                do l = i,jump*(loops-1),jump
                    if(array_1D(l)/=0.) then
                        sum1 = sum1 + (array_1D(l) - smean)**2.
                    end if
                end do
                s = sqrt(sum1 / real(count-1))
                sem = s / sqrt(real(count))
            end if
            if(present(dataquan_1D)) dataquan_1D(i) = count
            if(present(mean_1D)) mean_1D(i) = smean
            if(present(s_1D)) s_1D(i) = s
            if(present(sem_1D)) sem_1D(i) = sem
        end do
    end subroutine
    ! welch's t test for difference in 2 population means(mean1-mean2). A difference with a = 0.05 on both sides is returned as 1,otherwise 0. 119 means insufficient data or fuck you
    subroutine welchttest(mean1,s1,dataquan1,mean2,s2,dataquan2,result,LorS)
        implicit none
        real,intent(in)::mean1,s1,mean2,s2
        integer,intent(in)::dataquan1,dataquan2
        character(len=*),intent(out),optional::LorS
        integer,intent(out)::result
        real,dimension(0:30)::t_95=0.
        real::diff_mean,n1,n2,df,sem,bottomCI,topCI

        if(mean1 /=0. .and. mean2/=0. .and. dataquan1/=0 .and. dataquan2/=0) then
            diff_mean = mean1 - mean2
            n1 = real(dataquan1);n2 = real(dataquan2)
            sem = sqrt((s1**2./n1)+(s2**2./n2)) 
            df = (((s1**2.)/n1)+((s2**2.)/n2))**2./(((s1**2./n1)**2./(n1-1))+((s2**2./n2)**2./(n2-1)))
            call t95_value(t_95)
            bottomCI = diff_mean - t_95(int(df))*sem ; topCI = diff_mean + t_95(int(df))*sem
            ! print*,diff_mean,bottomCI,topCI,int(df)
            if(bottomCI>0.) then
                result = 1;if(present(LorS)) LorS = 'LARGE'
            else if(topCI<0.) then
                result = 1;if(present(LorS)) LorS = 'small'
            else;result = 0;if(present(LorS)) LorS = 'NODIF'
            end if
        else;result = 911;if(present(LorS)) LorS = '911!!'
        end if

    end subroutine
    
    ! avsdsemdataquan better series
        ! gives an array of mean arrays    DO NOT USE INSIDE A LOOP ALLOCATION IS TRICKY
    subroutine avsemdata_2D(array_2D,dim1,dim2,dec_dim,mean_1D,s_1D,sem_1D,dataquan_1D)
        implicit none
        integer,intent(in)::dim1,dim2
        character(len=*),intent(in)::dec_dim
        real,intent(in)::array_2D(:,:)
        real,dimension(:),allocatable,intent(out),optional::mean_1D,s_1D,sem_1D
        integer,dimension(:),allocatable,intent(out),optional::dataquan_1D
        integer::n,i,count=0
        real::smean, s, sem, sum0=0.,sum1=0.

        if(size(array_2D,1)<dim1 .or. size(array_2D,2)<dim2) then 
            print*,'Array size < dim1 or dim2';stop
        end if
        if (dec_dim == 'dim1') then
            if (present(mean_1D)) allocate(mean_1D(dim2))
            if (present(s_1D)) allocate(s_1D(dim2))
            if (present(sem_1D)) allocate(sem_1D(dim2))
            if (present(dataquan_1D)) allocate(dataquan_1D(dim2))
            do n = 1, dim2
                count = 0;sum0=0.;sum1 = 0.
                do i = 1, dim1
                    if (array_2D(i, n) /= 0.0)then;count = count + 1;sum0 = sum0 + array_2D(i,n);end if
                end do
                if (count <=1 ) then
                    smean = 0.;s = 0.;sem = 0.
                else
                    smean = sum0 / real(count)
                    do i = 1, dim1
                        if(array_2D(i,n)/=0.) then
                            sum1 = sum1 + (array_2D(i,n) - smean)**2.
                        end if
                    end do
                    s = sqrt(sum1 / real(count-1))
                    sem = s / sqrt(real(count))
                end if
                    if (present(dataquan_1D)) dataquan_1D(n) = count
                    if (present(mean_1D)) mean_1D(n) = smean
                    if (present(s_1D)) s_1D(n) = s
                    if (present(sem_1D)) sem_1D(n) = sem
            end do
        else if (dec_dim == 'dim2') then
            if (present(mean_1D)) allocate(mean_1D(dim1))
            if (present(s_1D)) allocate(s_1D(dim1))
            if (present(sem_1D)) allocate(sem_1D(dim1))
            if (present(dataquan_1D)) allocate(dataquan_1D(dim1))
            do n = 1, dim1
                count = 0;sum0=0.;sum1 = 0.
                do i = 1, dim2
                    if (array_2D(n, i) /= 0.0)then;count = count + 1;sum0 = sum0 + array_2D(n,i);end if
                    ! print*,n,i,count,sum0
                end do
                if (count <=1 ) then
                    smean = 0.;s = 0.;sem = 0.
                else
                    smean = sum0 / real(count)
                    do i = 1, dim2
                        if(array_2D(n,i)/=0.) then
                            sum1 = sum1 + (array_2D(n,i) - smean)**2.
                        end if
                    end do
                    s = sqrt(sum1/ real(count-1))
                    sem = s / sqrt(real(count))
                end if
                    if (present(dataquan_1D)) dataquan_1D(n) = count
                    if (present(mean_1D)) mean_1D(n) = smean
                    if (present(s_1D)) s_1D(n) = s
                    if (present(sem_1D)) sem_1D(n) = sem
            end do
        else
            print *, 'Choose a dimension to take mean of (descard)'
            stop
        end if


        ! if(present(mean_1D)) deallocate(mean_1D)
        ! if(present(s_1D)) deallocate(s_1D)
        ! if(present(sem_1D)) deallocate(sem_1D)
        ! if(present(dataquan_1D)) deallocate(dataquan_1D)

    end subroutine

    subroutine avsemdata_3D(array_3D,dim1,dim2,dim3,dec_dim,mean_2D,s_2D,sem_2D,dataquan_2D)
        implicit none
        integer,intent(in)::dim1,dim2,dim3
        character(len=*),intent(in)::dec_dim
        real,intent(in)::array_3D(:,:,:)
        real,dimension(:,:),allocatable,intent(out),optional::mean_2D,s_2D,sem_2D
        integer,dimension(:,:),allocatable,intent(out),optional::dataquan_2D
        integer::l1,l2,l3,count=0,loop1,loop2,loop3
        real::smean, s, sem, sum0=0.,sum1=0.

        if(size(array_3D,1)<dim1 .or. size(array_3D,2)<dim2 .or. size(array_3D,3)<dim3) then 
            print*,'Array size < dim1 or dim2 or dim3';stop
        end if
        ! loop determination
            if (dec_dim == 'dim1') then
                loop1 = dim2
                loop2 = dim3
                loop3 = dim1
                if(present(mean_2D)) allocate(mean_2D(dim2,dim3))
                if(present(s_2D)) allocate(s_2D(dim2,dim3))
                if(present(sem_2D)) allocate(sem_2D(dim2,dim3))
                if(present(dataquan_2D)) allocate(dataquan_2D(dim2,dim3))
            elseif (dec_dim == 'dim2') then
                loop1 = dim1
                loop2 = dim3
                loop3 = dim2
                if(present(mean_2D)) allocate(mean_2D(dim1,dim3))
                if(present(s_2D)) allocate(s_2D(dim1,dim3))
                if(present(sem_2D)) allocate(sem_2D(dim1,dim3))
                if(present(dataquan_2D)) allocate(dataquan_2D(dim1,dim3))
            elseif (dec_dim == 'dim3') then
                loop1 = dim1
                loop2 = dim2
                loop3 = dim3
                if(present(mean_2D)) allocate(mean_2D(dim1,dim2))
                if(present(s_2D)) allocate(s_2D(dim1,dim2))
                if(present(sem_2D)) allocate(sem_2D(dim1,dim2))
                if(present(dataquan_2D)) allocate(dataquan_2D(dim1,dim2))
            else
                print*, 'Invalid dec_dim value'
                stop
            end if
        ! loop determination end
        do l1 = 1, loop1
            do l2 = 1, loop2
                count = 0; sum0 = 0.;sum1 = 0.
                do l3 = 1, loop3 !dec_dim loop
                    if(dec_dim=='dim1') then
                        if(array_3D(l3,l1,l2)/=0.) then
                            count = count + 1
                            sum0 = sum0 + array_3D(l3,l1,l2)
                        end if
                    else if(dec_dim=='dim2') then
                        if(array_3D(l1,l3,l2)/=0.) then
                            count = count + 1
                            sum0 = sum0 + array_3D(l1,l3,l2)
                        end if
                    else if(dec_dim=='dim3') then
                        if(array_3D(l1,l2,l3)/=0.) then
                            count = count + 1
                            sum0 = sum0 + array_3D(l1,l2,l3)
                        end if
                    end if
                end do
                    if(count<=1) then;smean= 0.;s=0.;sem=0.
                    else;smean = sum0/real(count)
                        do l3 = 1, loop3
                            if(dec_dim=='dim1') then
                                if(array_3D(l3,l1,l2)/=0.) then
                                    sum1 = sum1 + (array_3D(l3,l1,l2) - smean)**2.
                                end if
                            else if(dec_dim=='dim2') then
                                if(array_3D(l1,l3,l2)/=0.) then
                                    sum1 = sum1 + (array_3D(l1,l3,l2) - smean)**2.
                                end if
                            else if(dec_dim=='dim3') then
                                if(array_3D(l1,l2,l3)/=0.) then
                                    sum1 = sum1 + (array_3D(l1,l2,l3) - smean)**2.
                                end if
                            end if
                        end do
                        s = sqrt(sum1/real(count-1))
                        sem = s / sqrt(real(count))
                    end if
                    if(present(mean_2D)) mean_2D(l1,l2) = smean
                    if(present(s_2D)) s_2D(l1,l2) = s
                    if(present(sem_2D)) sem_2D(l1,l2) = sem
                    if(present(dataquan_2D)) dataquan_2D(l1,l2) = count
            end do
        end do
        ! end do



    end subroutine

    subroutine avsemdata_4D(array_4D,dim1,dim2,dim3,dim4,dec_dim,mean_3D,s_3D,sem_3D,dataquan_3D)
        implicit none
        integer,intent(in)::dim1,dim2,dim3,dim4
        character(len=*),intent(in)::dec_dim
        real,intent(in)::array_4D(:,:,:,:)
        real,dimension(:,:,:),allocatable,intent(out),optional::mean_3D,s_3D,sem_3D
        integer,dimension(:,:,:),allocatable,intent(out),optional::dataquan_3D
        integer::l1,l2,l3,l4,count=0,loop1,loop2,loop3,loop4
        real::smean, s, sem, sum0=0.,sum1=0.

        if(size(array_4D,1)<dim1 .or. size(array_4D,2)<dim2 .or. size(array_4D,3)<dim3 .or. size(array_4D,4)<dim4) then 
            print*,'Array size < dim1 or dim2 or dim3 or dim4';stop
        end if
        ! loop determination
            if (dec_dim == 'dim1') then
                loop1 = dim2
                loop2 = dim3
                loop3 = dim4
                loop4 = dim1
                if(present(mean_3D)) allocate(mean_3D(dim2,dim3,dim4))
                if(present(s_3D)) allocate(s_3D(dim2,dim3,dim4))
                if(present(sem_3D)) allocate(sem_3D(dim2,dim3,dim4))
                if(present(dataquan_3D)) allocate(dataquan_3D(dim2,dim3,dim4))
            elseif (dec_dim == 'dim2') then
                loop1 = dim1
                loop2 = dim3
                loop3 = dim4
                loop4 = dim2
                if(present(mean_3D)) allocate(mean_3D(dim1,dim3,dim4))
                if(present(s_3D)) allocate(s_3D(dim1,dim3,dim4))
                if(present(sem_3D)) allocate(sem_3D(dim1,dim3,dim4))
                if(present(dataquan_3D)) allocate(dataquan_3D(dim1,dim3,dim4))
            elseif (dec_dim == 'dim3') then
                loop1 = dim1
                loop2 = dim2
                loop3 = dim4
                loop4 = dim3
                if(present(mean_3D)) allocate(mean_3D(dim1,dim2,dim4))
                if(present(s_3D)) allocate(s_3D(dim1,dim2,dim4))
                if(present(sem_3D)) allocate(sem_3D(dim1,dim2,dim4))
                if(present(dataquan_3D)) allocate(dataquan_3D(dim1,dim2,dim4))
            elseif (dec_dim == 'dim4') then
                loop1 = dim1
                loop2 = dim2
                loop3 = dim3
                loop4 = dim4
                if(present(mean_3D)) allocate(mean_3D(dim1,dim2,dim3))
                if(present(s_3D)) allocate(s_3D(dim1,dim2,dim3))
                if(present(sem_3D)) allocate(sem_3D(dim1,dim2,dim3))
                if(present(dataquan_3D)) allocate(dataquan_3D(dim1,dim2,dim3))
            else;print*, 'Invalid dec_dim value';stop
            end if
        ! loop determination end
        do l1 = 1, loop1
            do l2 = 1, loop2
                do l3 = 1, loop3
                    count = 0; sum0 = 0.;sum1 = 0.
                    do l4 = 1, loop4 !dec_dim loop
                        if(dec_dim=='dim1') then
                            if(array_4D(l4,l1,l2,l3)/=0.) then
                                count = count + 1
                                sum0 = sum0 + array_4D(l4,l1,l2,l3)
                            end if
                        else if(dec_dim=='dim2') then
                            if(array_4D(l1,l4,l2,l3)/=0.) then
                                count = count + 1
                                sum0 = sum0 + array_4D(l1,l4,l2,l3)
                            end if
                        else if(dec_dim=='dim3') then
                            if(array_4D(l1,l2,l4,l3)/=0.) then
                                count = count + 1
                                sum0 = sum0 + array_4D(l1,l2,l4,l3)
                            end if
                        else if(dec_dim=='dim4') then
                            if(array_4D(l1,l2,l3,l4)/=0.) then
                                count = count + 1
                                sum0 = sum0 + array_4D(l1,l2,l3,l4)
                            end if
                        end if
                    end do
                        if(count<=1) then;smean= 0.;s=0.;sem=0.
                        else;smean = sum0/real(count)
                            do l4 = 1, loop4
                                if(dec_dim=='dim1') then
                                    if(array_4D(l4,l1,l2,l3)/=0.) then
                                        sum1 = sum1 + (array_4D(l4,l1,l2,l3) - smean)**2.
                                    end if
                                else if(dec_dim=='dim2') then
                                    if(array_4D(l1,l4,l2,l3)/=0.) then
                                        sum1 = sum1 + (array_4D(l1,l4,l2,l3) - smean)**2.
                                    end if
                                else if(dec_dim=='dim3') then
                                    if(array_4D(l1,l2,l4,l3)/=0.) then
                                        sum1 = sum1 + (array_4D(l1,l2,l4,l3) - smean)**2.
                                    end if
                                else if(dec_dim=='dim4') then
                                    if(array_4D(l1,l2,l3,l4)/=0.) then
                                        sum1 = sum1 + (array_4D(l1,l2,l3,l4) - smean)**2.
                                    end if
                                end if
                            end do
                            s = sqrt(sum1/real(count-1))
                            sem = s / sqrt(real(count))
                        end if  
                        if(present(mean_3D)) mean_3D(l1,l2,l3) = smean
                        if(present(s_3D)) s_3D(l1,l2,l3) = s
                        if(present(sem_3D)) sem_3D(l1,l2,l3) = sem
                        if(present(dataquan_3D)) dataquan_3D(l1,l2,l3) = count                   
                end do
            end do
        end do

    end subroutine

    subroutine avsemdata_5D(array_5D,dim1,dim2,dim3,dim4,dim5,dec_dim,mean_4D,s_4D,sem_4D,dataquan_4D)
        implicit none
        integer,intent(in)::dim1,dim2,dim3,dim4,dim5
        character(len=*),intent(in)::dec_dim
        real,intent(in)::array_5D(:,:,:,:,:)
        real,dimension(:,:,:,:),allocatable,intent(out),optional::mean_4D,s_4D,sem_4D
        integer,dimension(:,:,:,:),allocatable,intent(out),optional::dataquan_4D
        integer::l1,l2,l3,l4,l5,count=0,loop1,loop2,loop3,loop4,loop5
        real::smean, s, sem, sum0=0.,sum1=0.

        if(size(array_5D,1)<dim1 .or. size(array_5D,2)<dim2 .or. size(array_5D,3)<dim3 .or. size(array_5D,4)<dim4 .or. size(array_5D,5)<dim5) then 
            print*,'Array size < dim1 or dim2 or dim3 or dim4 or dim5';stop
        end if
        ! loop determination
            if (dec_dim == 'dim1') then
                loop1 = dim2
                loop2 = dim3
                loop3 = dim4
                loop4 = dim5
                loop5 = dim1
                if(present(mean_4D)) allocate(mean_4D(dim2,dim3,dim4,dim5))
                if(present(s_4D)) allocate(s_4D(dim2,dim3,dim4,dim5))
                if(present(sem_4D)) allocate(sem_4D(dim2,dim3,dim4,dim5))
                if(present(dataquan_4D)) allocate(dataquan_4D(dim2,dim3,dim4,dim5))
            elseif (dec_dim == 'dim2') then
                loop1 = dim1
                loop2 = dim3
                loop3 = dim4
                loop4 = dim5
                loop5 = dim2
                if(present(mean_4D)) allocate(mean_4D(dim1,dim3,dim4,dim5))
                if(present(s_4D)) allocate(s_4D(dim1,dim3,dim4,dim5))
                if(present(sem_4D)) allocate(sem_4D(dim1,dim3,dim4,dim5))
                if(present(dataquan_4D)) allocate(dataquan_4D(dim1,dim3,dim4,dim5))
            elseif (dec_dim == 'dim3') then
                loop1 = dim1
                loop2 = dim2
                loop3 = dim4
                loop4 = dim5
                loop5 = dim3
                if(present(mean_4D)) allocate(mean_4D(dim1,dim2,dim4,dim5))
                if(present(s_4D)) allocate(s_4D(dim1,dim2,dim4,dim5))
                if(present(sem_4D)) allocate(sem_4D(dim1,dim2,dim4,dim5))
                if(present(dataquan_4D)) allocate(dataquan_4D(dim1,dim2,dim4,dim5))
            elseif (dec_dim == 'dim4') then
                loop1 = dim1
                loop2 = dim2
                loop3 = dim3
                loop4 = dim5
                loop5 = dim4
                if(present(mean_4D)) allocate(mean_4D(dim1,dim2,dim3,dim5))
                if(present(s_4D)) allocate(s_4D(dim1,dim2,dim3,dim5))
                if(present(sem_4D)) allocate(sem_4D(dim1,dim2,dim3,dim5))
                if(present(dataquan_4D)) allocate(dataquan_4D(dim1,dim2,dim3,dim5))
            elseif (dec_dim == 'dim5') then
                loop1 = dim1
                loop2 = dim2
                loop3 = dim3
                loop4 = dim4
                loop5 = dim5
                if(present(mean_4D)) allocate(mean_4D(dim1,dim2,dim3,dim4))
                if(present(s_4D)) allocate(s_4D(dim1,dim2,dim3,dim4))
                if(present(sem_4D)) allocate(sem_4D(dim1,dim2,dim3,dim4))
                if(present(dataquan_4D)) allocate(dataquan_4D(dim1,dim2,dim3,dim4))
            else;print*, 'Invalid dec_dim value';stop
            end if

        ! loop determination end
        do l1 = 1, loop1
            do l2 = 1, loop2
                do l3 = 1, loop3
                    do l4 = 1, loop4
                        count = 0; sum0 = 0.;sum1 = 0.
                        do l5 = 1, loop5 !dec_dim loop
                            if(dec_dim=='dim1') then
                                if(array_5D(l5,l1,l2,l3,l4)/=0.) then
                                    count = count + 1
                                    sum0 = sum0 + array_5D(l5,l1,l2,l3,l4)
                                end if
                            else if(dec_dim=='dim2') then
                                if(array_5D(l1,l5,l2,l3,l4)/=0.) then
                                    count = count + 1
                                    sum0 = sum0 + array_5D(l1,l5,l2,l3,l4)
                                end if
                            else if(dec_dim=='dim3') then
                                if(array_5D(l1,l2,l5,l3,l4)/=0.) then
                                    count = count + 1
                                    sum0 = sum0 + array_5D(l1,l2,l5,l3,l4)
                                end if
                            else if(dec_dim=='dim4') then
                                if(array_5D(l1,l2,l3,l5,l4)/=0.) then
                                    count = count + 1
                                    sum0 = sum0 + array_5D(l1,l2,l3,l5,l4)
                                end if
                            else if(dec_dim=='dim5') then
                                if(array_5D(l1,l2,l3,l4,l5)/=0.) then
                                    count = count + 1
                                    sum0 = sum0 + array_5D(l1,l2,l3,l4,l5)
                                end if
                            end if
                        end do
                            if(count<=1) then;smean= 0.;s=0.;sem=0.
                            else;smean = sum0/real(count)
                                do l5 = 1, loop5
                                    if(dec_dim=='dim1') then
                                        if(array_5D(l5,l1,l2,l3,l4)/=0.) then
                                            sum1 = sum1 + (array_5D(l5,l1,l2,l3,l4) - smean)**2.
                                        end if
                                    else if(dec_dim=='dim2') then
                                        if(array_5D(l1,l5,l2,l3,l4)/=0.) then
                                            sum1 = sum1 + (array_5D(l1,l5,l2,l3,l4) - smean)**2.
                                        end if
                                    else if(dec_dim=='dim3') then
                                        if(array_5D(l1,l2,l5,l3,l4)/=0.) then
                                            sum1 = sum1 + (array_5D(l1,l2,l5,l3,l4) - smean)**2.
                                        end if
                                    else if(dec_dim=='dim4') then
                                        if(array_5D(l1,l2,l3,l5,l4)/=0.) then
                                            sum1 = sum1 + (array_5D(l1,l2,l3,l5,l4) - smean)**2.
                                        end if
                                    else if(dec_dim=='dim5') then
                                        if(array_5D(l1,l2,l3,l4,l5)/=0.) then
                                            sum1 = sum1 + (array_5D(l1,l2,l3,l4,l5) - smean)**2.
                                        end if
                                    end if
                                end do
                                s = sqrt(sum1/real(count-1))
                                sem = s / sqrt(real(count))
                            end if
                            if(present(mean_4D)) mean_4D(l1,l2,l3,l4) = smean
                            if(present(s_4D)) s_4D(l1,l2,l3,l4) = s
                            if(present(sem_4D)) sem_4D(l1,l2,l3,l4) = sem
                            if(present(dataquan_4D)) dataquan_4D(l1,l2,l3,l4) = count
                    end do 
                end do            
            end do
        end do
    end subroutine

    ! ps boys are good bois 
    subroutine butler_psk(array_2D,dim1,dim2,width,height,maskval,ival,fval,inc,colorscheme,iterations,bpt1,bpt2,bpt3,contquan,conti,continc,r,g,b)
        implicit none
        integer,intent(in)::dim1,dim2,iterations,bpt1
        real,intent(in)::maskval,ival,fval,inc,width,height
        integer,intent(in),optional::bpt2,bpt3,contquan
        real,intent(in),optional::conti,continc
        real,intent(in)::array_2D(:,:)
        integer,dimension(size(array_2D,1),size(array_2D,2))::mask
        character(len=*),intent(in)::colorscheme
        real,dimension(:),allocatable::r1,g1,b1
        real,dimension(:),allocatable,intent(out),optional::r,g,b
        real::dx,dy
        integer::i,j,n

        if(size(array_2D,1)<dim1 .or. size(array_2D,2)<dim2) then 
            print*,'Array size < dim1 or dim2';stop
        end if
        if((ival+inc*real(iterations))/=fval)then;print*,'i=',ival,'inc=',inc,'iter=',iterations,'f=',fval,ival+inc*real(iterations),'i+inc*iter/=f';end if
        dx = width/real(dim1-1);dy = height/real(dim2-1)
        call create_box(width,height,3)
        call plot(-dx/2.,-dy/2.,-3)
        select case(colorscheme)
        case('b2r');call b2r_colorgrad(iterations,bpt1,r1,g1,b1)
        case('b2gy2r');call b2gy2r_colorgrad(iterations,bpt1,r1,g1,b1)
        case('r2g');call r2g_colorgrad(iterations,bpt1,r1,g1,b1)
        case('bk2r2g');call bk2r2g_colorgrad(iterations,bpt1,r1,g1,b1)
        case('b2cy2y2r');if(present(bpt2).and.present(bpt3)) then;call b2cy2y2r_colorgrad(iterations,bpt1,bpt2,bpt3,r1,g1,b1);else;print*,'bpt2 and bpt3 are required';stop;end if
        case('b2g2y2r');if(present(bpt2).and.present(bpt3)) then;call b2g2y2r_colorgrad(iterations,bpt1,bpt2,bpt3,r1,g1,b1);else;print*,'bpt2 and bpt3 are required';stop;end if
        case default;print*,'Invalid colorscheme';stop
        end select

        if(present(r).and.present(g).and.present(b)) then
            allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1));r = r1;g = g1;b = b1
        ! else;print*,'color arrays are not allocated'
        end if

        do i = 1, dim1
            do j = 1, dim2
                if(array_2D(i,j)==maskval) then;mask(i,j)=0
                else;mask(i,j)=1
                end if
            end do
        end do

        call pscolork(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,ival-100.,ival,r1(0),g1(0),b1(0))
        call pscolork(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,fval,fval+100.,r1(iterations+1),g1(iterations+1),b1(iterations+1))
        do n = 1, iterations
            call pscolork(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,ival+real(n-1)*inc,ival+real(n)*inc,r1(n),g1(n),b1(n))
        end do

        if(present(conti).and.present(continc).and.present(contquan)) then
            call pscont3(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,contquan,conti,continc)
        ! else;print*,'no contour'
        end if
        

        deallocate(r1,g1,b1)


        call plot(dx/2.,dy/2.,-3)

    end subroutine

    subroutine butler_psbet(array_2D,dim1,dim2,width,height,maskval,ival,fval,inc,colorscheme,iterations,bpt1,bpt2,bpt3,contquan,conti,continc,r,g,b)
        implicit none
        integer,intent(in)::dim1,dim2,iterations,bpt1
        real,intent(in)::maskval,ival,fval,inc,width,height
        integer,intent(in),optional::bpt2,bpt3,contquan
        real,intent(in),optional::conti,continc
        real,intent(in)::array_2D(:,:)
        integer,dimension(size(array_2D,1),size(array_2D,2))::mask
        character(len=*),intent(in)::colorscheme
        real,dimension(:),allocatable::r1,g1,b1
        real,dimension(:),allocatable,intent(out),optional::r,g,b
        real::dx,dy
        integer::i,j,n

        if(size(array_2D,1)<dim1 .or. size(array_2D,2)<dim2) then 
            print*,'Array size < dim1 or dim2';stop
        end if
        if((ival+inc*real(iterations))/=fval)then;print*,'i=',ival,'inc=',inc,'iter=',iterations,'f=',fval,'i+inc*iter/=f';stop;end if
        dx = width/real(dim1);dy = height/real(dim2)
        select case(colorscheme)
        case('b2r');call b2r_colorgrad(iterations,bpt1,r1,g1,b1)
        case('b2gy2r');call b2gy2r_colorgrad(iterations,bpt1,r1,g1,b1)
        case('r2g');call r2g_colorgrad(iterations,bpt1,r1,g1,b1)
        case('bk2r2g');call bk2r2g_colorgrad(iterations,bpt1,r1,g1,b1)
        case('b2cy2y2r');if(present(bpt2).and.present(bpt3)) then;call b2cy2y2r_colorgrad(iterations,bpt1,bpt2,bpt3,r1,g1,b1);else;print*,'bpt2 and bpt3 are required';stop;end if
        case('b2g2y2r');if(present(bpt2).and.present(bpt3)) then;call b2g2y2r_colorgrad(iterations,bpt1,bpt2,bpt3,r1,g1,b1);else;print*,'bpt2 and bpt3 are required';stop;end if
        case default;print*,'Invalid colorscheme';stop
        end select

        if(present(r).and.present(g).and.present(b)) then
            allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1));r = r1;g = g1;b = b1
        ! else;print*,'color arrays are not allocated'
        end if

        do i = 1, dim1
            do j = 1, dim2
                if(array_2D(i,j)==maskval) then;mask(i,j)=0
                else;mask(i,j)=1
                end if
            end do
        end do

        call betcolork2(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,ival-100.,ival,r1(0),g1(0),b1(0))
        call betcolork2(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,fval,fval+100.,r1(iterations+1),g1(iterations+1),b1(iterations+1))
        do n = 1, iterations
            call betcolork2(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,ival+real(n-1)*inc,ival+real(n)*inc,r1(n),g1(n),b1(n))
        end do

        if(present(conti).and.present(continc).and.present(contquan)) then
            call pscont3(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,contquan,conti,continc)
        else;print*,'no contour'
        end if
        deallocate(r1,g1,b1)
    end subroutine
    ! paints areas within range with color given, other regions will not be painted. (betcolork2)
    subroutine butler_mask(array_2D,dim1,dim2,width,height,mask_ini,mask_fin,r,g,b)
        implicit none
        integer,intent(in)::dim1,dim2
        real,intent(in)::width,height
        real,intent(in)::array_2D(:,:)
        real,intent(in)::r,g,b,mask_ini,mask_fin
        integer,dimension(size(array_2D,1),size(array_2D,2))::mask
        real::dx,dy
        integer::i,j
        
        write(16,*)"% begin butler_mask"

        if(size(array_2D,1)<dim1 .or. size(array_2D,2)<dim2) then 
            print*,'Array size < dim1 or dim2';stop
        end if
        dx = width/real(dim1);dy = height/real(dim2)
        do i = 1, dim1
            do j = 1, dim2
                if(mask_ini<=array_2D(i,j).and.array_2D(i,j)<=mask_fin) then;mask(i,j)=0
                else;mask(i,j)=1
                end if

            end do
        end do
        ! print*,mask

        call betcolork2(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,-1000.,1000.,r,g,b)
        write(16,*)"% end butler_mask"
    end subroutine
    ! subroutine butler_mask(mask_2D,dim1,dim2,width,height,maskval,r,g,b)
    !     implicit none
    !     integer,intent(in)::dim1,dim2,maskval
    !     real,intent(in)::width,height
    !     integer,intent(in)::mask_2D(:,:)
    !     real,intent(in)::r,g,b
    !     real,dimension(size(mask_2D,1),size(mask_2D,2))::mask
    !     real::dx,dy
    !     integer::i,j
        
    !     write(16,*)"% begin butler_mask"

    !     if(size(mask_2D,1)<dim1 .or. size(mask_2D,2)<dim2) then 
    !         print*,'Array size < dim1 or dim2';stop
    !     end if
    !     dx = width/real(dim1);dy = height/real(dim2)
    !     do i = 1, dim1
    !         do j = 1, dim2
    !             if(mask_2D(i,j)==maskval) then;mask(i,j)=1
    !             else;mask(i,j)=0
    !             end if
    !         end do
    !     end do

    !     call betcolork2(dx,dy,mask_2D,mask,1,dim1,1,dim2,dim1,dim2,-10000000.,10000000.,r,g,b)
    !     write(16,*)"% end butler_mask"
    ! end subroutine

        !
end module subroutines

module test
    use constants
    contains
    subroutine printl()
        print*,l
        l = l +1
    end subroutine
end module