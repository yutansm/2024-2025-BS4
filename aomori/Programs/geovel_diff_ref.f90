program geovel_with_different_reference_level
    use always
    real,dimension(:,:),allocatable::geovel2d
    real,dimension(:,:,:),allocatable::geovel400mean,geovel200mean,geovel150mean,geovel100mean,geovel50mean
    real,dimension(years,months,5,400)::geovel400
    real,dimension(years,months,5,200)::geovel200
    real,dimension(years,months,5,150)::geovel150
    real,dimension(years,months,5,100)::geovel100
    real,dimension(years,months,5,50)::geovel50
    real,dimension(:),allocatable::surf50meanv_st12,surf50meanv_st23,surf50meanv_st34,surf50meanv_st45,surf50meanv_st56
    real,dimension(:),allocatable::surf50meanv_st14,surf50meanv_st16,surf50semv_st12,surf50semv_st14,surf50semv_st16
    real,dimension(14)::loop12,loop23,loop34,loop45,loop56,loop14,loop16

    width = 2.3;height = 4.;width2 = 12.;height2 = 8.
    call calibrated_data2(potemp_5,sal_5) ! 15 * 12 * 2 * 9 * 400
    print*,count(potemp_5/=0.),count(sal_5/=0.) ! check if there are any 0s

    ! geovel400
        do y = 1, 15
            do m = 1, 12
                ! call calc_density(potemp_5(y,m,1,4:9,:),sal_5(y,m,1,4:9,:),den_5,.true.)
                call calc_geovel(geovel2d,delta_x,temp_2D = potemp_5(y,m,1,4:9,:400),sal_2D = sal_5(y,m,1,4:9,:400),lat = 41.,units = 'cm')
                ! call calc_geovel(geovel2d,delta_x,den_2D = den_5(y,m,1,1:6,:),lat = 41.)
                geovel400(y,m,:,:) = geovel2d
                deallocate(geovel2d)
            end do
        end do
        call avsemdata_4D(geovel400,'dim1',mean_3D = geovel400mean)

    ! geovel200
        do y = 1, 15
            do m = 1, 12
                call calc_geovel(geovel2d,delta_x,temp_2D = potemp_5(y,m,1,4:9,:200),sal_2D = sal_5(y,m,1,4:9,:200),lat = 41.,units = 'cm')
                geovel200(y,m,:,:) = geovel2d
                deallocate(geovel2d)
            end do
        end do
        call avsemdata_4D(geovel200,'dim1',mean_3D = geovel200mean)
    ! geovel150
        do y = 1, 15
            do m = 1, 12
                call calc_geovel(geovel2d,delta_x,temp_2D = potemp_5(y,m,1,4:9,:150),sal_2D = sal_5(y,m,1,4:9,:150),lat = 41.,units = 'cm')
                geovel150(y,m,:,:) = geovel2d
                deallocate(geovel2d)
            end do
        end do
        call avsemdata_4D(geovel150,'dim1',mean_3D = geovel150mean)
    ! geovel100
        do y = 1, 15
            do m = 1, 12
                call calc_geovel(geovel2d,delta_x,temp_2D = potemp_5(y,m,1,4:9,:100),sal_2D = sal_5(y,m,1,4:9,:100),lat = 41.,units = 'cm')
                geovel100(y,m,:,:) = geovel2d
                deallocate(geovel2d)
            end do
        end do
        call avsemdata_4D(geovel100,'dim1',mean_3D = geovel100mean)
    ! geovel50
        do y = 1, 15
            do m = 1, 12
                call calc_geovel(geovel2d,delta_x,temp_2D = potemp_5(y,m,1,4:9,:50),sal_2D = sal_5(y,m,1,4:9,:50),lat = 41.,units = 'cm')
                geovel50(y,m,:,:) = geovel2d
                deallocate(geovel2d)
            end do
        end do
        call avsemdata_4D(geovel50,'dim1',mean_3D = geovel50mean)
    ! end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! index 6 corresponds to station 1, it is flipped !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! plotting
    ! geovel400
        call plots2(nnfile = 'geovel_with_different_reference_levels',oopt = 'otops',x = 0.,y = 0.,h = 'Geovel ref 400m')

        call plotsave('first')
        do m = 1, 12
            if(m == 1 .or. m == 7)cycle
            call symbolc(width/2.,0.2,0.8,monthnames(m))
            call butler_psmask(geovel400mean(m,:,:),width,-height,10.,20.,1.,0.8,0.8)
            call butler_psmask(geovel400mean(m,:,:),width,-height,20.,30.,1.,0.3,0.3)
            call butler_psmask(geovel400mean(m,:,:),width,-height,30.,40.,.7,0.,0.)
            call butler_cont(geovel400mean(m,:,:),width,-height,0.,0.,2.,thicc = 5,maskn = .true.)
            if(m == 2)call num_memori2(0.,400.,-height,50.,-90.,2,float_quantity = -1)
            if(m == 2)call st_memori(1,6,width,1,0.5,gap = 2,y = -height)
            call plot(width+0.5,0.,-3)
        end do
        call plotback('first');call plot(1.5,-height-10.,-3);call plotsave('second')
        call avsemdata_2D(geovel400mean(:,5,:50),'dim2',mean_1D = surf50meanv_st12) ! surface mean velo for st1-2 using 400m reference data
        call avsemdata_2D(geovel400mean(:,4,:50),'dim2',mean_1D = surf50meanv_st23) ! surface mean velo for st2-3 using 400m reference data
        call avsemdata_2D(geovel400mean(:,3,:50),'dim2',mean_1D = surf50meanv_st34) ! surface mean velo for st3-4 using 400m reference data
        call avsemdata_2D(geovel400mean(:,2,:50),'dim2',mean_1D = surf50meanv_st45) ! surface mean velo for st4-5 using 400m reference data
        call avsemdata_2D(geovel400mean(:,1,:50),'dim2',mean_1D = surf50meanv_st56) ! surface mean velo for st5-6 using 400m reference data
        loop12(:12) = surf50meanv_st12;loop12(1) = (loop12(12)+loop12(2))/2.;loop12(7) = (loop12(6)+loop12(8))/2.;loop12(13:14) = loop12(1:2)! looping and LI
        loop23(:12) = surf50meanv_st23;loop23(1) = (loop23(12)+loop23(2))/2.;loop23(7) = (loop23(6)+loop23(8))/2.;loop23(13:14) = loop23(1:2)
        loop34(:12) = surf50meanv_st34;loop34(1) = (loop34(12)+loop34(2))/2.;loop34(7) = (loop34(6)+loop34(8))/2.;loop34(13:14) = loop34(1:2)
        loop45(:12) = surf50meanv_st45;loop45(1) = (loop45(12)+loop45(2))/2.;loop45(7) = (loop45(6)+loop45(8))/2.;loop45(13:14) = loop45(1:2)
        loop56(:12) = surf50meanv_st56;loop56(1) = (loop56(12)+loop56(2))/2.;loop56(7) = (loop56(6)+loop56(8))/2.;loop56(13:14) = loop56(1:2)

        call butler_linegraph(loop12(:13),width2,height2,0.,30.,0.,.true.,memsymfreq = 5,maskbelow = 0.,lthick = 8,rl = 1.,memflqt = -1)
        call butler_linegraph(loop23(:13),width2,height2,0.,30.,0.,.false.,memsymfreq = 5,maskbelow = 0.,lthick = 7,bl = 1.)
        call butler_linegraph(loop34(:13),width2,height2,0.,30.,0.,.false.,memsymfreq = 5,maskbelow = 0.,lthick = 6,gl = 1.)
        call butler_linegraph(loop45(:13),width2,height2,0.,30.,0.,.false.,memsymfreq = 5,maskbelow = 0.,lthick = 5,rl = 1.,bl = 1.)
        call butler_linegraph(loop56(:13),width2,height2,0.,30.,0.,.false.,memsymfreq = 5,maskbelow = 0.,lthick = 4,gl = 1.,bl = 1.)
        call mod12_memori(13,width2,gap = 2)

        call plot(width2+1.5,0.,-3);call plotsave('third')
        call butler_linegraph(loop12(:13),width2,height2,0.,30.,0.,.false.,memsymfreq = 5,lthick = 8,rl = 1.)
        call butler_linegraph((loop12(:13)+loop23(:13)+loop34(:13))/3.,width2,height2,0.,30.,0.,.false.,memsymfreq = 5,lthick = 7,bl = 1.)
        call butler_linegraph((loop12(:13)+loop23(:13)+loop34(:13)+loop45(:13)+loop56(:13))/5.,width2,height2,0.,30.,0.,.false.,memsymfreq = 5,lthick = 6,gl = 1.)
        call mod12_memori(13,width2,gap = 2)
        
    ! geovel200
        call newpage(h = 'Geovel ref 200m')
        call plotback('first')
        do m = 1, 12
            if(m == 1 .or. m == 7)cycle
            call symbolc(width/2.,0.2,0.8,monthnames(m))
            call butler_psmask(geovel200mean(m,:,:),width,-height,10.,20.,1.,0.8,0.8)
            call butler_psmask(geovel200mean(m,:,:),width,-height,20.,30.,1.,0.3,0.3)
            call butler_psmask(geovel200mean(m,:,:),width,-height,30.,40.,.7,0.,0.)
            call butler_cont(geovel200mean(m,:,:),width,-height,0.,0.,2.,thicc = 5,maskn = .true.)
            if(m == 2)call num_memori2(0.,200.,-height,50.,-90.,1,float_quantity = -1)
            if(m == 2)call st_memori(1,6,width,1,0.5,gap = 2,y = -height)
            call plot(width+0.5,0.,-3)
        end do
        call plotback('second')
        call avsemdata_2D(geovel200mean(:,5,:50),'dim2',mean_1D = surf50meanv_st12) ! surface mean velo for st1-2 using 400m reference data
        call avsemdata_2D(geovel200mean(:,4,:50),'dim2',mean_1D = surf50meanv_st23) ! surface mean velo for st2-3 using 400m reference data
        call avsemdata_2D(geovel200mean(:,3,:50),'dim2',mean_1D = surf50meanv_st34) ! surface mean velo for st3-4 using 400m reference data
        call avsemdata_2D(geovel200mean(:,2,:50),'dim2',mean_1D = surf50meanv_st45) ! surface mean velo for st4-5 using 400m reference data
        call avsemdata_2D(geovel200mean(:,1,:50),'dim2',mean_1D = surf50meanv_st56) ! surface mean velo for st5-6 using 400m reference data
        loop12(:12) = surf50meanv_st12;loop12(1) = (loop12(12)+loop12(2))/2.;loop12(7) = (loop12(6)+loop12(8))/2.;loop12(13:14) = loop12(1:2)! looping and LI
        loop23(:12) = surf50meanv_st23;loop23(1) = (loop23(12)+loop23(2))/2.;loop23(7) = (loop23(6)+loop23(8))/2.;loop23(13:14) = loop23(1:2)
        loop34(:12) = surf50meanv_st34;loop34(1) = (loop34(12)+loop34(2))/2.;loop34(7) = (loop34(6)+loop34(8))/2.;loop34(13:14) = loop34(1:2)
        loop45(:12) = surf50meanv_st45;loop45(1) = (loop45(12)+loop45(2))/2.;loop45(7) = (loop45(6)+loop45(8))/2.;loop45(13:14) = loop45(1:2)
        loop56(:12) = surf50meanv_st56;loop56(1) = (loop56(12)+loop56(2))/2.;loop56(7) = (loop56(6)+loop56(8))/2.;loop56(13:14) = loop56(1:2)

        call butler_linegraph(loop12(:13),width2,height2,0.,30.,0.,.true.,memsymfreq = 5,maskbelow = 0.,lthick = 8,rl = 1.,memflqt = -1)
        call butler_linegraph(loop23(:13),width2,height2,0.,30.,0.,.false.,memsymfreq = 5,maskbelow = 0.,lthick = 7,bl = 1.)
        call butler_linegraph(loop34(:13),width2,height2,0.,30.,0.,.false.,memsymfreq = 5,maskbelow = 0.,lthick = 6,gl = 1.)
        call butler_linegraph(loop45(:13),width2,height2,0.,30.,0.,.false.,memsymfreq = 5,maskbelow = 0.,lthick = 5,rl = 1.,bl = 1.)
        call butler_linegraph(loop56(:13),width2,height2,0.,30.,0.,.false.,memsymfreq = 5,maskbelow = 0.,lthick = 4,gl = 1.,bl = 1.)
        call mod12_memori(13,width2,gap = 2)

        call plotback('third')
        call butler_linegraph(loop12(:13),width2,height2,0.,30.,0.,.false.,memsymfreq = 5,lthick = 8,rl = 1.)
        call butler_linegraph((loop12(:13)+loop23(:13)+loop34(:13))/3.,width2,height2,0.,30.,0.,.false.,memsymfreq = 5,lthick = 7,bl = 1.)
        call butler_linegraph((loop12(:13)+loop23(:13)+loop34(:13)+loop45(:13)+loop56(:13))/5.,width2,height2,0.,30.,0.,.false.,memsymfreq = 5,lthick = 6,gl = 1.)
        call mod12_memori(13,width2,gap = 2)
        
    ! geovel150
        call newpage(h = 'Geovel ref 150m')
        call plotback('first')
        do m = 1, 12
            if(m == 1 .or. m == 7)cycle
            call symbolc(width/2.,0.2,0.8,monthnames(m))
            call butler_psmask(geovel150mean(m,:,:),width,-height,10.,20.,1.,0.8,0.8)
            call butler_psmask(geovel150mean(m,:,:),width,-height,20.,30.,1.,0.3,0.3)
            call butler_psmask(geovel150mean(m,:,:),width,-height,30.,40.,.7,0.,0.)
            call butler_cont(geovel150mean(m,:,:),width,-height,0.,0.,2.,thicc = 5,maskn = .true.)
            if(m == 2)call num_memori2(0.,150.,-height,50.,-90.,1,float_quantity = -1)
            if(m == 2)call st_memori(1,6,width,1,0.5,gap = 2,y = -height)
            call plot(width+0.5,0.,-3)
        end do
                call plotback('second')
        call avsemdata_2D(geovel150mean(:,5,:50),'dim2',mean_1D = surf50meanv_st12) ! surface mean velo for st1-2 using 400m reference data
        call avsemdata_2D(geovel150mean(:,4,:50),'dim2',mean_1D = surf50meanv_st23) ! surface mean velo for st2-3 using 400m reference data
        call avsemdata_2D(geovel150mean(:,3,:50),'dim2',mean_1D = surf50meanv_st34) ! surface mean velo for st3-4 using 400m reference data
        call avsemdata_2D(geovel150mean(:,2,:50),'dim2',mean_1D = surf50meanv_st45) ! surface mean velo for st4-5 using 400m reference data
        call avsemdata_2D(geovel150mean(:,1,:50),'dim2',mean_1D = surf50meanv_st56) ! surface mean velo for st5-6 using 400m reference data
        loop12(:12) = surf50meanv_st12;loop12(1) = (loop12(12)+loop12(2))/2.;loop12(7) = (loop12(6)+loop12(8))/2.;loop12(13:14) = loop12(1:2)! looping and LI
        loop23(:12) = surf50meanv_st23;loop23(1) = (loop23(12)+loop23(2))/2.;loop23(7) = (loop23(6)+loop23(8))/2.;loop23(13:14) = loop23(1:2)
        loop34(:12) = surf50meanv_st34;loop34(1) = (loop34(12)+loop34(2))/2.;loop34(7) = (loop34(6)+loop34(8))/2.;loop34(13:14) = loop34(1:2)
        loop45(:12) = surf50meanv_st45;loop45(1) = (loop45(12)+loop45(2))/2.;loop45(7) = (loop45(6)+loop45(8))/2.;loop45(13:14) = loop45(1:2)
        loop56(:12) = surf50meanv_st56;loop56(1) = (loop56(12)+loop56(2))/2.;loop56(7) = (loop56(6)+loop56(8))/2.;loop56(13:14) = loop56(1:2)

        call butler_linegraph(loop12(:13),width2,height2,0.,30.,0.,.true.,memsymfreq = 5,maskbelow = 0.,lthick = 8,rl = 1.,memflqt = -1)
        call butler_linegraph(loop23(:13),width2,height2,0.,30.,0.,.false.,memsymfreq = 5,maskbelow = 0.,lthick = 7,bl = 1.)
        call butler_linegraph(loop34(:13),width2,height2,0.,30.,0.,.false.,memsymfreq = 5,maskbelow = 0.,lthick = 6,gl = 1.)
        call butler_linegraph(loop45(:13),width2,height2,0.,30.,0.,.false.,memsymfreq = 5,maskbelow = 0.,lthick = 5,rl = 1.,bl = 1.)
        call butler_linegraph(loop56(:13),width2,height2,0.,30.,0.,.false.,memsymfreq = 5,maskbelow = 0.,lthick = 4,gl = 1.,bl = 1.)
        call mod12_memori(13,width2,gap = 2)

        call plotback('third')
        call butler_linegraph(loop12(:13),width2,height2,0.,30.,0.,.false.,memsymfreq = 5,lthick = 8,rl = 1.)
        call butler_linegraph((loop12(:13)+loop23(:13)+loop34(:13))/3.,width2,height2,0.,30.,0.,.false.,memsymfreq = 5,lthick = 7,bl = 1.)
        call butler_linegraph((loop12(:13)+loop23(:13)+loop34(:13)+loop45(:13)+loop56(:13))/5.,width2,height2,0.,30.,0.,.false.,memsymfreq = 5,lthick = 6,gl = 1.)
        call mod12_memori(13,width2,gap = 2)
    ! geovel100
        call newpage(h = 'Geovel ref 100m')
        call plotback('first')
        do m = 1, 12
            if(m == 1 .or. m == 7)cycle
            call symbolc(width/2.,0.2,0.8,monthnames(m))
            call butler_psmask(geovel100mean(m,:,:),width,-height,10.,20.,1.,0.8,0.8)
            call butler_psmask(geovel100mean(m,:,:),width,-height,20.,30.,1.,0.3,0.3)
            call butler_psmask(geovel100mean(m,:,:),width,-height,30.,40.,.7,0.,0.)
            call butler_cont(geovel100mean(m,:,:),width,-height,0.,0.,2.,thicc = 5,maskn = .true.)
            if(m == 2)call num_memori2(0.,100.,-height,50.,-90.,1,float_quantity = -1)
            if(m == 2)call st_memori(1,6,width,1,0.5,gap = 2,y = -height)
            call plot(width+0.5,0.,-3)
        end do
                call plotback('second')
        call avsemdata_2D(geovel100mean(:,5,:50),'dim2',mean_1D = surf50meanv_st12) ! surface mean velo for st1-2 using 400m reference data
        call avsemdata_2D(geovel100mean(:,4,:50),'dim2',mean_1D = surf50meanv_st23) ! surface mean velo for st2-3 using 400m reference data
        call avsemdata_2D(geovel100mean(:,3,:50),'dim2',mean_1D = surf50meanv_st34) ! surface mean velo for st3-4 using 400m reference data
        call avsemdata_2D(geovel100mean(:,2,:50),'dim2',mean_1D = surf50meanv_st45) ! surface mean velo for st4-5 using 400m reference data
        call avsemdata_2D(geovel100mean(:,1,:50),'dim2',mean_1D = surf50meanv_st56) ! surface mean velo for st5-6 using 400m reference data
        loop12(:12) = surf50meanv_st12;loop12(1) = (loop12(12)+loop12(2))/2.;loop12(7) = (loop12(6)+loop12(8))/2.;loop12(13:14) = loop12(1:2)! looping and LI
        loop23(:12) = surf50meanv_st23;loop23(1) = (loop23(12)+loop23(2))/2.;loop23(7) = (loop23(6)+loop23(8))/2.;loop23(13:14) = loop23(1:2)
        loop34(:12) = surf50meanv_st34;loop34(1) = (loop34(12)+loop34(2))/2.;loop34(7) = (loop34(6)+loop34(8))/2.;loop34(13:14) = loop34(1:2)
        loop45(:12) = surf50meanv_st45;loop45(1) = (loop45(12)+loop45(2))/2.;loop45(7) = (loop45(6)+loop45(8))/2.;loop45(13:14) = loop45(1:2)
        loop56(:12) = surf50meanv_st56;loop56(1) = (loop56(12)+loop56(2))/2.;loop56(7) = (loop56(6)+loop56(8))/2.;loop56(13:14) = loop56(1:2)

        call butler_linegraph(loop12(:13),width2,height2,0.,30.,0.,.true.,memsymfreq = 5,maskbelow = 0.,lthick = 8,rl = 1.,memflqt = -1)
        call butler_linegraph(loop23(:13),width2,height2,0.,30.,0.,.false.,memsymfreq = 5,maskbelow = 0.,lthick = 7,bl = 1.)
        call butler_linegraph(loop34(:13),width2,height2,0.,30.,0.,.false.,memsymfreq = 5,maskbelow = 0.,lthick = 6,gl = 1.)
        call butler_linegraph(loop45(:13),width2,height2,0.,30.,0.,.false.,memsymfreq = 5,maskbelow = 0.,lthick = 5,rl = 1.,bl = 1.)
        call butler_linegraph(loop56(:13),width2,height2,0.,30.,0.,.false.,memsymfreq = 5,maskbelow = 0.,lthick = 4,gl = 1.,bl = 1.)
        call mod12_memori(13,width2,gap = 2)

        call plotback('third');call plot(0.5,0.,-3);call plotsave('forth')
        call butler_linegraph(loop12(:13),width2,height2,0.,20.,0.,.true.,memsymfreq = 5,lthick = 8,rl = 1.,memflqt = -1)
        call butler_linegraph((loop12(:13)+loop23(:13)+loop34(:13))/3.,width2,height2,0.,20.,0.,.false.,memsymfreq = 5,lthick = 7,bl = 1.)
        call butler_linegraph((loop12(:13)+loop23(:13)+loop34(:13)+loop45(:13)+loop56(:13))/5.,width2,height2,0.,20.,0.,.false.,memsymfreq = 5,lthick = 6,gl = 1.)
        call mod12_memori(13,width2,gap = 2)
    ! geovel50
        call newpage(h = 'Geovel ref 50m')
        call plotback('first')
        do m = 1, 12
            if(m == 1 .or. m == 7)cycle
            call symbolc(width/2.,0.2,0.8,monthnames(m))
            call butler_psmask(geovel50mean(m,:,:),width,-height,10.,20.,1.,0.8,0.8)
            call butler_psmask(geovel50mean(m,:,:),width,-height,20.,30.,1.,0.3,0.3)
            call butler_psmask(geovel50mean(m,:,:),width,-height,30.,40.,.7,0.,0.)
            call butler_cont(geovel50mean(m,:,:),width,-height,0.,0.,2.,thicc = 5,maskn = .true.)
            if(m == 2)call num_memori2(0.,50.,-height,50.,-90.,1,float_quantity = -1)
            if(m == 2)call st_memori(1,6,width,1,0.5,gap = 2,y = -height)
            call plot(width+0.5,0.,-3)
        end do
                call plotback('second')
        call avsemdata_2D(geovel50mean(:,5,:50),'dim2',mean_1D = surf50meanv_st12) ! surface mean velo for st1-2 using 400m reference data
        call avsemdata_2D(geovel50mean(:,4,:50),'dim2',mean_1D = surf50meanv_st23) ! surface mean velo for st2-3 using 400m reference data
        call avsemdata_2D(geovel50mean(:,3,:50),'dim2',mean_1D = surf50meanv_st34) ! surface mean velo for st3-4 using 400m reference data
        call avsemdata_2D(geovel50mean(:,2,:50),'dim2',mean_1D = surf50meanv_st45) ! surface mean velo for st4-5 using 400m reference data
        call avsemdata_2D(geovel50mean(:,1,:50),'dim2',mean_1D = surf50meanv_st56) ! surface mean velo for st5-6 using 400m reference data
        loop12(:12) = surf50meanv_st12;loop12(1) = (loop12(12)+loop12(2))/2.;loop12(7) = (loop12(6)+loop12(8))/2.;loop12(13:14) = loop12(1:2)! looping and LI
        loop23(:12) = surf50meanv_st23;loop23(1) = (loop23(12)+loop23(2))/2.;loop23(7) = (loop23(6)+loop23(8))/2.;loop23(13:14) = loop23(1:2)
        loop34(:12) = surf50meanv_st34;loop34(1) = (loop34(12)+loop34(2))/2.;loop34(7) = (loop34(6)+loop34(8))/2.;loop34(13:14) = loop34(1:2)
        loop45(:12) = surf50meanv_st45;loop45(1) = (loop45(12)+loop45(2))/2.;loop45(7) = (loop45(6)+loop45(8))/2.;loop45(13:14) = loop45(1:2)
        loop56(:12) = surf50meanv_st56;loop56(1) = (loop56(12)+loop56(2))/2.;loop56(7) = (loop56(6)+loop56(8))/2.;loop56(13:14) = loop56(1:2)

        call butler_linegraph(loop12(:13),width2,height2,0.,30.,0.,.true.,memsymfreq = 5,maskbelow = 0.,lthick = 8,rl = 1.,memflqt = -1)
        call butler_linegraph(loop23(:13),width2,height2,0.,30.,0.,.false.,memsymfreq = 5,maskbelow = 0.,lthick = 7,bl = 1.)
        call butler_linegraph(loop34(:13),width2,height2,0.,30.,0.,.false.,memsymfreq = 5,maskbelow = 0.,lthick = 6,gl = 1.)
        call butler_linegraph(loop45(:13),width2,height2,0.,30.,0.,.false.,memsymfreq = 5,maskbelow = 0.,lthick = 5,rl = 1.,bl = 1.)
        call butler_linegraph(loop56(:13),width2,height2,0.,30.,0.,.false.,memsymfreq = 5,maskbelow = 0.,lthick = 4,gl = 1.,bl = 1.)
        call mod12_memori(13,width2,gap = 2)

        call plotback('forth')
        call butler_linegraph(loop12(:13),width2,height2,0.,10.,0.,.true.,memsymfreq = 5,lthick = 8,rl = 1.,memflqt = -1)
        call butler_linegraph((loop12(:13)+loop23(:13)+loop34(:13))/3.,width2,height2,0.,10.,0.,.false.,memsymfreq = 5,lthick = 7,bl = 1.)
        call butler_linegraph((loop12(:13)+loop23(:13)+loop34(:13)+loop45(:13)+loop56(:13))/5.,width2,height2,0.,10.,0.,.false.,memsymfreq = 5,lthick = 6,gl = 1.)
        call mod12_memori(13,width2,gap = 2)
    ! end
        call plote

end program