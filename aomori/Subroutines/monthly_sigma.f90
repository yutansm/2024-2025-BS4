program monthly_sigma
    implicit none
    integer,parameter::years = 15,months = 12,lines = 2, stations = 9,depth = 400
    real,parameter::ini_sigma = 24., fin_sigma = 27.6, interval = 0.1 , ini_sd = 0. , fin_sd = .7, interval_sd = 0.025
    integer,parameter::iterations = int((fin_sigma-ini_sigma)/interval), iterations_sd = int((fin_sd-ini_sd)/interval_sd)
    integer,parameter::ini_st = 8, fin_st = 9
    real,dimension(years,months,lines,stations,depth)::potemp_c5,sal_c5,sigma_5
    real,dimension(months,lines,stations,depth)::sigma_av,sigma_sd,sigma_data
    real,dimension(stations,depth)::ps_av,ps_sd,ps_data
    integer,dimension(stations,depth)::mask_av,mask_sd
    real,dimension(iterations)::r,g,b,r2,g2,b2
    real,parameter::width = 1.5,height = 3., standard_sal_450 = 34.07
    character(len=4),dimension(12)::month_names
    integer::y,m,l,st,d,n,i
    real::dx,dy,diff

    dx = width/real(fin_st-ini_st+1) ; dy = -height/400.
    call plots(1.5,13.,13,'/LARGE0/gr10291/nishimori2/aomori/Monthly_av_and_sd/monthly_sigma_NLine_coast.ps')
    call symbol(1.5,3.,1.,'Monthly Water Density Profiles at NLine coast',0.,len('monthly water density profiles at nline coast'))
    
    call calibrated_data(potemp_c5,sal_c5)
    call br_colorgrad(iterations,24,r,g,b);call month_str_array(month_names)
    call br_colorgrad(iterations_sd,18,r2,g2,b2)
    ! print*,sigma_sd(2,1,1:9,1:100)
    
    call create_sigma_array(potemp_c5,sal_c5,sigma_5)
    call avsd_dataquan(sigma_5,sigma_av,sigma_sd,sigma_data)
    ! do n = 1,iterations
    !     call betsqk(0.,0.,.3,.3,r(n),g(n),b(n))
    !     call plot(.3,0.,-3)
    ! end do
    
    ! do n = 1,iterations_sd
    !     call betsqk(0.,0.,.3,.3,r2(n),g2(n),b2(n))
    !     call plot(.3,0.,-3)
    ! end do
    ! call plot(-0.3*real(iterations),-1.,-3)

    ! call plot(-0.3*real(iterations_sd),-1.,-3)

    sigma_av(1,1,8,1:400) = sigma_av(1,1,9,1:400);sigma_av(7,1,8,1:400) = sigma_av(7,1,9,1:400)
    sigma_sd(1,1,8,1:400) = sigma_sd(1,1,9,1:400);sigma_sd(7,1,8,1:400) = sigma_sd(7,1,9,1:400)
    ! print*,sigma_sd(1,1,8,1:400)
    call symbolc(-0.5,-height/2.,0.3,'average',90.,len('average'))
    do m = 1,months
        call symbolc(width/2.,.7,.5,month_names(m),0.,4)
        do l = 1,1
            do st = 8,9
                do d = 1,depth
                    if(sigma_av(m,l,st,d)/=0.) then
                        mask_av(st,d) = 1
                    else;mask_av(st,d) = 0
                    end if
                    ps_av(st,d) = sigma_av(m,l,st,d)
                end do !end of d
            end do !end of st
            call pscolork(dx,dy,ps_av,mask_av,ini_st,fin_st,1,400,9,400,2.,ini_sigma,0.,0.,1.)
            call pscolork(dx,dy,ps_av,mask_av,ini_st,fin_st,1,400,9,400,fin_sigma,100.,1.,0.,0.)
            do n = 1,iterations
            call pscolork(dx,dy,ps_av,mask_av,ini_st,fin_st,1,400,9,400,ini_sigma+interval*real(n-1),ini_sigma+interval*real(n),r(n),g(n),b(n))
            end do
            call pscont3(dx,dy,ps_av,mask_av,ini_st,fin_st,1,400,9,400,9,24.,0.4)
        end do
        call psframe(ini_st,fin_st,400,1.5,3.,0.05)
        call plot(2.,0.,-3)
    end do
    call plot(-2.*12.,-5.,-3)

    call symbolc(-0.5,-height/2.,0.3,'sd',90.,len('sd'))
    do m = 1,months
        do l = 1,1
            do st = 8,9
                do d = 1, depth
                    if(sigma_sd(m,l,st,d)/=0.) then
                        mask_sd(st,d) = 1
                    else;mask_sd(st,d) = 0
                    end if
                    ps_sd(st,d) = sigma_sd(m,l,st,d)
                end do
            end do
            call pscolork(dx,dy,ps_sd,mask_sd,ini_st,fin_st,1,400,9,400,fin_sd,2.,1.,0.,0.)
            do n = 1, iterations_sd
                call pscolork(dx,dy,ps_sd,mask_sd,ini_st,fin_st,1,400,9,400,ini_sd+interval_sd*real(n-1),ini_sd+interval_sd*real(n),r2(n),g2(n),b2(n))
            end do
            call pscont3(dx,dy,ps_sd,mask_sd,ini_st,fin_st,1,400,9,400,7,0.,0.1)
        end do
        call psframe(ini_st,fin_st,400,1.5,3.,0.05)
        call plot(2.,0.,-3)
    end do

    call plot(-2.*12.,-6.,-3);call symbol(0.,0.4,0.3,'average density scale',0.,len('average density scale'))
    call colorscale(0.,0.,1.,0.8,0.8,1.,24,8.*2./3.,0.3,1,0,0);call colorscale(0.8,0.8,1.,1.,0.,0.,12,8.*1./3.,0.3,0,1,-8.)
    call num_memori(24.,27.6,36,4,0.12,1,8.,0,1,1)

    call plot(9.,0.,-3);call symbol(0.,0.4,0.3,'density deviation scale',0.,len('density deviation scale'))
    call colorscale(0.,0.,1.,0.8,0.8,1.,18,8.*18./28.,0.3,0,0,0);call colorscale(0.8,0.8,1.,1.,0.,0.,10,8.*10./28.,0.3,0,1,-8.)
    call num_memori(0.,0.7,28,4,0.12,2,8.,0,0,1)

   call plote
end program



