
program fortran 
    implicit none 
    integer ::x
    real::f,g,h,i,j,l,m
intrinsic sin,cos,asin,acos
real,parameter::pi = 2.*asin(1.)

call newpen2(4)
    call plots(0.,10.,13,'second.ps')
    call plot(0.,-5.,3)
    call plot(0.,5.,2)
    call plot(0.,0.,3)
    call plot(10.,0.,2)

    call rgbk(0.1,0.1,0.1)
    do x=0,200
        f = cos(real(x)*pi/100.)
        if(x==0) then 
            call plot(0.,1.,3)
        else
        
            call plot(real(x)*pi/100.,f,2)
        end if
    end do
    call rgbk(0.1,0.2,0.3)
    do x=0,200
        f = sin(real(x)*pi/100.)
        if(x==0) then 
            call plot(0.,0.,3)
        else
            call plot(real(x)*pi/100.,f,2)
        end if

    end do
    call rgbk(0.4,0.5,0.6)
    do x=0,200
        f = cos(real(x-50)*pi/200.)
        g = cos(pi*50./200.)
        if(x==0) then 
            call plot(0.,g,3)
        else
            call plot(real(x)*pi/100.,f,2)
        end if
    end do
    call rgbk(0.6,0.5,0.4)
    do x=0,200
        f = sin(real(x-50)*pi/200.)
        g = -1.*sin(pi*50./200.)
        if (x==0) then 
            call plot(0.,g,3)
        else
            call plot(real(x)*pi/100.,f,2)
        end if
    end do
    call rgbk(0.3,0.2,0.1)
    do x=0,200
        f = cos(real(x+50)*pi/50.)
        g = -1.
        if(x==0) then 
            call plot(0.,g,3)
        else
            call plot(real(x)*pi/100.,f,2)
        end if
    end do
    call rgbk(0.9,0.1,0.7)
    do x=0,200
        g = sin(real(x+75)*pi/50.)
        h = sin(real(75)*pi/50.)
        if(x==0) then
            call plot(0.,h,3)
        else
            call plot(real(x)*pi/100.,g,2)
        end if
    end do    




    call plote
end program