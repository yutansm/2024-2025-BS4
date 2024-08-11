program fortran

    call plots(5.,5.,13,'first.ps')
    call plot(5.,5.,3)
    call plot(10.,5.,2)
    call plot(10.,10.,2)
    call plot(5.,10.,2)
    call plot(5.,5.,2)


    call symbol(5.0,14.0,1.0,'Yuta Nishimori',0.0,14)
    call symbolC(2.0,6.0,1.0,'Yuta Nishimori',90.0,14)
    call symbolR(5.0,10.0,1.0,'Yuta Nishimori',180.0,14)

    call axis(0.0,0.0,'Xaxis',-5,3.,0.0,0.0,2.)
    call axis(0.0,0.0,'Yaxis',5,10.0,90.0,0.0,1.0)

    call plote

end program



