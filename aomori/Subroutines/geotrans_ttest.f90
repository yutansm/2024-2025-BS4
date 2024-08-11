program geostrophic_transport_t_test
    implicit none
    integer,parameter::years = 15, months = 12, lines = 2
    real,parameter:: width1 =18., height1 = 12.,width2 = 4.,height2 = 3.
    real,dimension(years,months,lines)::geotrans
    real,dimension(months,lines)::geotrans_av,geotrans_sd
    integer,dimension(months,lines)::geotrans_quan
    integer::y,m,l,n,df
    real::dx1,dx2,diff_av,plot_diff_av,sem,plot95,n1,n2,s1,s2,sp2
    real,dimension(0:30)::t95

    call plots(4.,3.,13,'/LARGE0/gr10291/nishimori2/aomori/Geostrophy/graphs/t_test/51transport_ttest.ps')
    call symbolc(9.0,height1+2.,.6,'t Test for Monthly Geostrophic Transport (NLine,51db)',0.,len('t Test for Monthly Geostrophic Transport (NLine,51db)'))
    ! dx1 = width1/13.;dx2 = width2/13.
    dx1 = width1/15.;dx2 = width2/15.
    l = 1
    call geotransport(51,geotrans)
    call avsd_dataquan2(geotrans,geotrans_av,geotrans_sd,geotrans_quan)
    call t95_value(t95)
    call newpen2(3)
    call create_box(width1,height1,3)
    call num_memori(-1.,1.,40,2,0.25,1,height1,-90,0,0)
    ! call month_memori(0.3,width1)     !今回は月々を数字で表せとの要望なので後で消す。
    call mod12_memori(14,.3,width1)
    call symbol(0.,height1+0.5,0.4,'n',0.,1)
    call symbol(0.,height1+1.,0.4,'df',0.,2)
    call plot(0.,height1/2.,-3)
    call plot(0.,0.,3);call plot(width1,0.,2)
    do n = 1,14
        if(mod(n,12)/=0) then;m = mod(n,12)
        else if (mod(n,12)==0) then; m = 12;else;end if
         call numberc(real(n)*dx1,height1/2.+0.5,0.3,real(geotrans_quan(m,l)),0.,-1)
        if (m == 1 .or. m == 7) then
            diff_av = 0.
            sem = 0.
            df = 0
            call symbolr(real(n)*dx1,1.,0.2,'insufficient data',-90.,len('insufficient data'))
        else if(m == 2) then
            diff_av = (geotrans_av(m,l)-geotrans_av(12,l)) !２月は１２月との比較
            n1 = real(geotrans_quan(m,l)); n2 = real(geotrans_quan(12,l));s1 = geotrans_sd(m,l);s2 = geotrans_sd(12,l)
            sp2 = (real(n1-1)*s1**2.+real(n2-1)*s2**2.)/real(n1+n2-2) !sp2 is sp squared
            sem = sqrt(sp2*(1./real(n1)+1./real(n2))) ! pooled variance
            df = int(n1+n2)-2 ! degrees of freedom
        else if (m == 8) then
            diff_av = (geotrans_av(m,l)-geotrans_av(6,l)) !８月は６月との比較
            n1 = real(geotrans_quan(m,l)); n2 = real(geotrans_quan(6,l));s1 = geotrans_sd(m,l);s2 = geotrans_sd(6,l)
            sp2 = (real(n1-1)*s1**2.+real(n2-1)*s2**2.)/real(n1+n2-2) !sp2 is sp squared
            sem = sqrt(sp2*(1./real(n1)+1./real(n2))) ! pooled variance
            df = int(n1+n2)-2 ! degrees of freedom
        else ! m = 3,4,5,6,9,10,11,12
            diff_av = (geotrans_av(m,l)-geotrans_av(m-1,l))  !x(n)bar - x(n-1) bar (scaled)
            n1 = real(geotrans_quan(m,l)); n2 = real(geotrans_quan(m-1,l));s1 = geotrans_sd(m,l);s2 = geotrans_sd(m-1,l)
            sp2 = (real(n1-1)*s1**2.+real(n2-1)*s2**2.)/real(n1+n2-2) !sp2 is sp squared
            sem = sqrt(sp2*(1./real(n1)+1./real(n2))) ! pooled variance
            df = int(n1+n2)-2 ! degrees of freedom
        end if
        print*,m,sem,df
            plot_diff_av = diff_av*height1/2.
            plot95 = t95(df)*sem*height1/2.
            call gmark(real(n)*dx1,plot_diff_av,0.1,1)
            call plot(real(n)*dx1,plot_diff_av-plot95,3);call plot(real(n)*dx1,plot_diff_av+plot95,2)
            call numberr(real(n)*dx1,plot_diff_av+0.5,0.3,diff_av,0.,2);call symbol(real(n)*dx1,plot_diff_av+0.5,0.3,'+-',0.,2)
            call number(real(n)*dx1+0.3,plot_diff_av+0.5,0.3,t95(df)*sem,0.,2)
            call numberc(real(n)*dx1,height1/2.+1.,0.4,real(df),0.,-1)
    end do

!     call newpage
!     call plot(-3.,9.,-3)
!     call symbolc(13.2,height2+2.,1.,'Monthly Geostrophic Transport at NLine (2009-2023)',0.,len('monthly mean of Geostrophic Transport at NLine (2009-2023)'))
!     do y = 1, 15
!         call create_box(width2,height2,2)
!         call num_memori(0.,4.,8,1,0.2,1,height2,-90,0,0)
!         call month_memori(0.15,width2)
!         call numberc(width2/2.,height2+0.5,0.4,real(y+2008),0.,-1)

!         do  m = 1, months
!             call gmark(real(m)*dx2,real(geotrans(y,m,l))*height2/4.,0.1,1)
!             call numberc(real(m)*dx2,real(geotrans(y,m,l))*height2/4.+0.2,0.15,real(geotrans(y,m,l)),0.,2)
!         end do
!         if (mod(y,5)/=0) then
!             call plot(width2+1.,0.,-3)
!         else
!             call plot(-4.*(width2+1.),-(height2+2.),-3)
!         end if
! end do

call plote
end program