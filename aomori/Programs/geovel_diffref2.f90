program geovel_with_different_reference_level
    use always
    real,dimension(:,:),allocatable::geovel2d
    real,dimension(:,:,:),allocatable::geovel400mean,geovel400sem
    real,dimension(:,:),allocatable::tempomean ! y m st temporary array for (surface mean velocity) for st12 23 34 45 56
    real,dimension(:,:,:),allocatable::tempomean2 ! y m st temporary array for (surface mean velocity) for st14 st16
    real,dimension(years,months,5,400)::geovel400
    real,dimension(:),allocatable::surf50meanv_st12,surf50meanv_st23,surf50meanv_st34,surf50meanv_st45,surf50meanv_st56,surf50semv_st12,surf50semv_st23,surf50semv_st34,surf50semv_st45,surf50semv_st56
    real,dimension(:),allocatable::surf50meanv_st14,surf50meanv_st16,surf50semv_st14,surf50semv_st16
    real,dimension(14)::loop12,loop23,loop34,loop45,loop56,loop14,loop16,loopsem12,loopsem23,loopsem34,loopsem45,loopsem56,loopsem14,loopsem16 ! jan~dec,jan,feb

    width = 2.3;height = 4.;width2 = 12.;height2 = 8.
    call calibrated_data2(potemp_5,sal_5) ! 15 * 12 * 2 * 9 * 400 indices are flipped
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
        call avsemdata_4D(geovel400,'dim1',mean_3D = geovel400mean,sem_3D = geovel400sem) ! for pscolor

        ! for each station
            call avsemdata_3D(geovel400(:,:,5,:50),'dim3',mean_2D = tempomean) ! surface mean velo for st1-2 using 400m reference data
            call avsemdata_2D(tempomean,'dim1',mean_1D = surf50meanv_st12,sem_1D = surf50semv_st12) ! surface mean velo for st1-2 using 400m reference data
            call avsemdata_3D(geovel400(:,:,4,:50),'dim3',mean_2D = tempomean) ! surface mean velo for st1-4 using 400m reference data
            call avsemdata_2D(tempomean,'dim1',mean_1D = surf50meanv_st23,sem_1D = surf50semv_st23) ! surface mean velo for st2-3 using 400m reference data
            call avsemdata_3D(geovel400(:,:,3,:50),'dim3',mean_2D = tempomean) ! surface mean velo for st2-4 using 400m reference data
            call avsemdata_2D(tempomean,'dim1',mean_1D = surf50meanv_st34,sem_1D = surf50semv_st34) ! surface mean velo for st3-4 using 400m reference data
            call avsemdata_3D(geovel400(:,:,2,:50),'dim3',mean_2D = tempomean) ! surface mean velo for st3-5 using 400m reference data
            call avsemdata_2D(tempomean,'dim1',mean_1D = surf50meanv_st45,sem_1D = surf50semv_st45) ! surface mean velo for st4-5 using 400m reference data
            call avsemdata_3D(geovel400(:,:,1,:50),'dim3',mean_2D = tempomean) ! surface mean velo for st4-6 using 400m reference data
            call avsemdata_2D(tempomean,'dim1',mean_1D = surf50meanv_st56,sem_1D = surf50semv_st56) ! surface mean velo for st5-6 using 400m reference data

        ! for stations 1-4 and 1-6
            call avsemdata_4D(geovel400(:,:,3:5,:50),'dim3',mean_3D = tempomean2) ! surface mean velo for st1-4 using 400m reference data
            call avsemdata_3D(tempomean2,'dim3',mean_2D = tempomean) ! surface mean velo for st1-4 using 400m reference data    
            call avsemdata_2D(tempomean,'dim1',mean_1D = surf50meanv_st14,sem_1D = surf50semv_st14) ! monthly surface mean velo
            call avsemdata_4D(geovel400(:,:,1:5,:50),'dim3',mean_3D = tempomean2) ! surface mean velo for st1-6 using 400m reference data
            call avsemdata_3D(tempomean2,'dim3',mean_2D = tempomean) ! surface mean velo for st1-6 using 400m reference data
            call avsemdata_2D(tempomean,'dim1',mean_1D = surf50meanv_st16,sem_1D = surf50semv_st16) ! monthly surface mean velo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! index 6 corresponds to station 1, it is flipped !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! plotting
    ! geovel400
        call plots2(nnfile = 'geovel_diffref2',oopt = 'otops',x = 0.,y = 0.,h = 'Geovel ref 400m')

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
        call plotback('first');call plot(0.5,-height-11.,-3);call plotsave('second')
        ! call avsemdata_2D(geovel400mean(:,5,:50),'dim2',mean_1D = surf50meanv_st12,sem_1D = surf50semv_st12) ! surface mean velo for st1-2 using 400m reference data
        ! call avsemdata_2D(geovel400mean(:,4,:50),'dim2',mean_1D = surf50meanv_st23,sem_1D = surf50semv_st23) ! surface mean velo for st2-3 using 400m reference data
        ! call avsemdata_2D(geovel400mean(:,3,:50),'dim2',mean_1D = surf50meanv_st34,sem_1D = surf50semv_st34) ! surface mean velo for st3-4 using 400m reference data
        ! call avsemdata_2D(geovel400mean(:,2,:50),'dim2',mean_1D = surf50meanv_st45,sem_1D = surf50semv_st45) ! surface mean velo for st4-5 using 400m reference data
        ! call avsemdata_2D(geovel400mean(:,1,:50),'dim2',mean_1D = surf50meanv_st56,sem_1D = surf50semv_st56) ! surface mean velo for st5-6 using 400m reference data
        loop12(:12) = surf50meanv_st12;loop12(1) = (loop12(12)+loop12(2))/2.;loop12(7) = (loop12(6)+loop12(8))/2.;loop12(13:14) = loop12(1:2)! looping and LI
        loop23(:12) = surf50meanv_st23;loop23(1) = (loop23(12)+loop23(2))/2.;loop23(7) = (loop23(6)+loop23(8))/2.;loop23(13:14) = loop23(1:2)
        loop34(:12) = surf50meanv_st34;loop34(1) = (loop34(12)+loop34(2))/2.;loop34(7) = (loop34(6)+loop34(8))/2.;loop34(13:14) = loop34(1:2)
        loop45(:12) = surf50meanv_st45;loop45(1) = (loop45(12)+loop45(2))/2.;loop45(7) = (loop45(6)+loop45(8))/2.;loop45(13:14) = loop45(1:2)
        loop56(:12) = surf50meanv_st56;loop56(1) = (loop56(12)+loop56(2))/2.;loop56(7) = (loop56(6)+loop56(8))/2.;loop56(13:14) = loop56(1:2)
        loopsem12(:12) = surf50semv_st12;loopsem12(1) = 0.;loopsem12(7) = 0.;loopsem12(13:14) = loopsem12(1:2)! looping and LI
        loopsem23(:12) = surf50semv_st23;loopsem23(1) = 0.;loopsem23(7) = 0.;loopsem23(13:14) = loopsem23(1:2)
        loopsem34(:12) = surf50semv_st34;loopsem34(1) = 0.;loopsem34(7) = 0.;loopsem34(13:14) = loopsem34(1:2)
        loopsem45(:12) = surf50semv_st45;loopsem45(1) = 0.;loopsem45(7) = 0.;loopsem45(13:14) = loopsem45(1:2)
        loopsem56(:12) = surf50semv_st56;loopsem56(1) = 0.;loopsem56(7) = 0.;loopsem56(13:14) = loopsem56(1:2)
        loop14(:12) = surf50meanv_st14;loop14(1) = (loop14(12)+loop14(2))/2.;loop14(7) = (loop14(6)+loop14(8))/2.;loop14(13:14) = loop14(1:2)
        loop16(:12) = surf50meanv_st16;loop16(1) = (loop16(12)+loop16(2))/2.;loop16(7) = (loop16(6)+loop16(8))/2.;loop16(13:14) = loop16(1:2)
        loopsem14(:12) = surf50semv_st14;loopsem14(1) = 0.;loopsem14(7) = 0.;loopsem14(13:14) = loopsem14(1:2)
        loopsem16(:12) = surf50semv_st16;loopsem16(1) = 0.;loopsem16(7) = 0.;loopsem16(13:14) = loopsem16(1:2)


        call butler_linegraph(loop12,width2,height2,0.,30.,0.,.true.,memsymfreq = 5,maskbelow = 0.,lthick = 8,rl = 1.,memflqt = -1,error_1D = loopsem12)
        call butler_linegraph(loop23,width2,height2,0.,30.,0.,.false.,memsymfreq = 5,maskbelow = 0.,lthick = 7,bl = 1.,error_1D = loopsem23)
        call butler_linegraph(loop34,width2,height2,0.,30.,0.,.false.,memsymfreq = 5,maskbelow = 0.,lthick = 6,gl = 1.,error_1D = loopsem34)
        call butler_linegraph(loop45,width2,height2,0.,30.,0.,.false.,memsymfreq = 5,maskbelow = 0.,lthick = 5,rl = 1.,bl = 1.,error_1D = loopsem45)
        call butler_linegraph(loop56,width2,height2,0.,30.,0.,.false.,memsymfreq = 5,maskbelow = 0.,lthick = 4,gl = 1.,bl = 1.,error_1D = loopsem56)
        call mod12_memori(14,width2,gap = 2)

        call plot(width2+2.5,0.,-3);call plotsave('third')
        call butler_linegraph(loop12,width2,height2,0.,30.,0.,.true.,memsymfreq = 5,memflqt = -1,lthick = 8,rl = 1.,error_1D = loopsem12)
        ! call butler_linegraph((loop12+loop23+loop34)/3.,width2,height2,0.,30.,0.,.false.,memsymfreq = 5,lthick = 7,bl = 1.,gl = 1.)
        call butler_linegraph(loop14,width2,height2,0.,30.,0.,.false.,memsymfreq = 5,lthick = 7,bl = 1.,error_1D = loopsem23)
        call butler_linegraph(loop16,width2,height2,0.,30.,0.,.false.,memsymfreq = 5,lthick = 6,gl = 1.,error_1D = loopsem16)
        call mod12_memori(14,width2,gap = 2)
        
        call plote

end program
