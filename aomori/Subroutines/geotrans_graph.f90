program geostrophic_transport_graph
    implicit none
    integer,parameter::years = 15, months = 12, lines = 2
    real,parameter:: width1 =18., height1 = 12.,width2 = 4.,height2 = 3.
    real,dimension(years,months,lines)::geotrans
    real,dimension(months,lines)::geotrans_av,geotrans_sd
    integer,dimension(months,lines)::geotrans_quan
    integer::y,m,l,n,df
    real::dx1,dx2,av,sem,plot95,plotav
    real,dimension(0:30)::t95

    call plots(4.,3.,13,'/LARGE0/gr10291/nishimori2/aomori/Geostrophy/graphs/mean_se_t95/geostrophic_transport_51_3.ps')
    call symbolc(9.0,height1+2.,.8,'Monthly Geostrophic Transport at NLine (51db Median Filtered)',0.,len('Monthly Geostrophic Transport at NLine (mmdb Median Filtered)'))
    dx1 = width1/15.;dx2 = width2/15.
    l = 1
    call geotransport(51,geotrans)
    call avsd_dataquan2(geotrans,geotrans_av,geotrans_sd,geotrans_quan)
    call t95_value(t95)
    call newpen2(3)
    call create_box(width1,height1,3)
    call num_memori(0.,4.,40,5,0.25,1,height1,-90,0,0)
    ! call month_memori(0.3,width1)
    call mod12_memori(14,.3,width1)
    call symbol(0.,height1+0.5,0.4,'n',0.,1)
    call symbol(0.,height1+1.,0.4,'df',0.,2)
    ! print*,t95(1:30)
    do n = 1,14
        if(mod(n,12)/=0) then;m = mod(n,12)
        else if (mod(n,12)==0) then; m = 12;else;end if

        call numberc(real(n)*dx1,height1+0.5,0.3,real(geotrans_quan(m,l)),0.,-1)
        if (geotrans_quan(m,l)/=0) then;df = geotrans_quan(m,l)-1
        else;df = 0;end if
        call numberc(real(n)*dx1,height1+1.,0.3,real(df),0.,-1)           !writing degrees of freedom

        av = geotrans_av(m,l)
        if (av/=0.) then
        sem = geotrans_sd(m,l)/sqrt(real(geotrans_quan(m,l)))
        else;sem = 0.;end if
        plotav = av*height1/4.
        plot95 = t95(df)*sem*height1/4.
        ! print*,av,sem,plot95
        call gmark(real(n)*dx1,plotav,0.1,1)
        ! call plot(real(m)*dx1,av-sem,3);call plot(real(m)*dx1,av+sem,2)
        call plot(real(n)*dx1,plotav-plot95,3);call plot(real(n)*dx1,plotav+plot95,2)
        call numberr(real(n)*dx1,plotav+plot95+0.5,0.3,av,0.,2)
        !以下はたした部分
        call symbol(real(n)*dx1,plotav+plot95+0.5,0.2,'+-',0.,2)
        call number(real(n)*dx1+0.2,plotav+plot95+0.5,0.2,plot95*4./height1,0.,2)
    end do

    call newpage
    call plot(-3.,9.,-3)
    call symbolc(13.2,height2+2.,1.,'Monthly Geostrophic Transport at NLine (2009-2023)',0.,len('monthly mean of Geostrophic Transport at NLine (2009-2023)'))
    do y = 1, 15
        call create_box(width2,height2,2)
        call num_memori(0.,4.,8,1,0.2,1,height2,-90,0,0)
        call month_memori(0.15,width2)
        call numberc(width2/2.,height2+0.5,0.4,real(y+2008),0.,-1)

        do  m = 1, months
            call gmark(real(m)*dx2,real(geotrans(y,m,l))*height2/4.,0.1,1)
            call numberc(real(m)*dx2,real(geotrans(y,m,l))*height2/4.+0.2,0.15,real(geotrans(y,m,l)),0.,2)
        end do
        if (mod(y,5)/=0) then
            call plot(width2+1.,0.,-3)
        else
            call plot(-4.*(width2+1.),-(height2+2.),-3)
        end if
end do

call plote
end program