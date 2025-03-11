program gomibako
    use always
    implicit none 
    type(JODC_TS)::temp,sal
    type(JODC_RHO)::den,dh,mdiff_dh
    character(len=100)::title
    real::ratio,height1,width1 = 6.,width2 = 4.,min,height2
    integer::ini_lat = 32,fin_lat = 46,ini_long = 126,fin_long = 142,dep,month
    real,dimension(:),allocatable::r1,g1,b1,r2,g2,b2,r3,g3,b3
    logical::stat
    
    allocate(mdiff_dh%mean(12,181,361,33))
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                    ! Data Obtainment
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        call JODC_data2(potemp = temp,sal = sal,den = den,dh = dh,info = .true.,calibrate1 = .true.)
        print*,size(temp%mean)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                            ! Plotting
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call plots2(oopt = 'otops',mode = 'landscape',nnfile = 'JODC_Annual&SeasonalMeans_JMMJSN',x = 1.7,y = -5.5,h = 'Annual Means')
    call plotsave('original')
    ratio = 6357./6378./cos((ini_lat+fin_lat)/2.*pi/180.)
    height1 = width1*ratio*real(fin_lat-ini_lat)/real(fin_long-ini_long)
    height2 = width2*ratio*real(fin_lat-ini_lat)/real(fin_long-ini_long) ! smaller one

            call plot(-0.5,-1.5,-3);call plotsave('first')

            do i = 1, 3
                call symbolc(width1/2.,height1+0.2,1.,int2str(i*100)//'m',0.)
                    stat = .true.
                    j = i * 100
                call butler_psk(sal%mean(0,ini_long:fin_long,ini_lat:fin_lat,JODC_dep2index(j,info = .true.)),width1,height1,0.,33.9,34.3,0.05,'b2r',8,bpt1 = 5,r = r1, g = g1,b = b1) ! 100m
                call butler_cont(temp%mean(0,ini_long:fin_long,ini_lat:fin_lat,JODC_dep2index(j)),width1,height1,0.,maskn = .true.,conti = 0.,continc = 1.,thicc = 5) ! 100m
                call simple_map(ini_lat,fin_lat,ini_long,fin_long,width1,symbol_size = 0.8,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat,paintland = .true.)
                call plot(width1 + 3.5,0.,-3)
            end do
            call header('Seasonal Means at depth 100m',y = -height1-4.5)
            call plotback('first');call plot(-1.,-height2-5.,-3)
            do i = 1, 6
                month = 2 * i - 1

                call symbolc(width2/2.,height2+0.2,0.8,monthnames(month),0.)
                if(i == 1)then 
                    ! call symbolr(-1.,height2/2.,0.6,'100m',ang = 0.)
                    stat = .true.
                else
                    stat = .false.
                end if
                call butler_psk(sal%mean(month,ini_long:fin_long,ini_lat:fin_lat,6),width2,height2,0.,34.2,34.4,0.025,'b2r',8,bpt1 = 5,r = r2, g = g2,b = b2) ! 100m
                call butler_cont(temp%mean(month,ini_long:fin_long,ini_lat:fin_lat,6),width2,height2,0.,maskn = .true.,conti = 0.,continc = 1.,thicc = 5) ! 100m
                call simple_map(ini_lat,fin_lat,ini_long,fin_long,width2,symbol_size = 0.5,r = 0.8,g = 0.9, b = 0.1,symbol_freq = 4,symbols = stat,paintland = .true.)
                call plot(width2 + 0.5,0.,-3)
            end do

            call newpage(y = -5.)
            call colorscale(r1,g1,b1,33.9,34.3,2,0.8,2,10.,0.3,lt = 1, gt = 1,rangle = 90.,symbol_start = 1)
            call plot(6.,0.,-3)
            call colorscale(r2,g2,b2,34.2,34.4,2,0.8,2,10.,0.3,lt = 1, gt = 1,rangle = 90.)

    call plote
end program 
