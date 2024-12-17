program station1_final
    use always
    implicit none
    real,parameter::width = 7.,height = 5.
    real,dimension(years,months,depth)::potemp,sal,sigma
    real,dimension(:,:),allocatable::av_potemp,av_sal,av_sigma,sem_potemp,sem_sal,sem_sigma,s_potemp,s_sal,s_sigma
    real,dimension(13,depth)::av_potemploop,av_salloop,av_sigmaloop,sem_potemploop,sem_salloop,sem_sigmaloop,s_potemploop,s_salloop,s_sigmaloop
    integer,dimension(:,:),allocatable::data_potemp,data_sal,data_sigma
    integer,dimension(13,depth)::data_potemploop,data_salloop,data_sigmaloop
    real,dimension(:),allocatable::r1,g1,b1,r2,g2,b2,r3,g3,b3,r4,g4,b4
    integer,dimension(12,depth)::resultt,results,resultsig
    integer::month1,month2

    call calibrated_data51(potemp_c5,sal_c5)
    call create_sigma_array(potemp_c5,sal_c5,sigma_c5)
    potemp = potemp_c5(:,:,1,9,:)! N line Station 1
    sal = sal_c5(:,:,1,9,:)
    sigma = sigma_c5(:,:,1,9,:)
    call avsemdata_3D(potemp,'dim1',mean_2D=av_potemp,sem_2D=sem_potemp,dataquan_2D=data_potemp,s_2D=s_potemp)
    call avsemdata_3D(sal,'dim1',mean_2D=av_sal,sem_2D=sem_sal,dataquan_2D=data_sal,s_2D=s_sal)
    call avsemdata_3D(sigma,'dim1',mean_2D=av_sigma,sem_2D=sem_sigma,dataquan_2D=data_sigma,s_2D=s_sigma)
    av_potemploop(1:12,:) = av_potemp ; av_potemploop(13,:) = av_potemp(1,:)
    av_salloop(1:12,:) = av_sal ; av_salloop(13,:) = av_sal(1,:)
    av_sigmaloop(1:12,:) = av_sigma ; av_sigmaloop(13,:) = av_sigma(1,:)
    sem_potemploop(1:12,:) = sem_potemp ; sem_potemploop(13,:) = sem_potemp(1,:)
    sem_salloop(1:12,:) = sem_sal ; sem_salloop(13,:) = sem_sal(1,:)
    sem_sigmaloop(1:12,:) = sem_sigma ; sem_sigmaloop(13,:) = sem_sigma(1,:)
    s_potemploop(1:12,:) = s_potemp ; s_potemploop(13,:) = s_potemp(1,:)
    s_salloop(1:12,:) = s_sal ; s_salloop(13,:) = s_sal(1,:)
    s_sigmaloop(1:12,:) = s_sigma ; s_sigmaloop(13,:) = s_sigma(1,:)
    data_potemploop(1:12,:) = data_potemp ; data_potemploop(13,:) = data_potemp(1,:)
    data_salloop(1:12,:) = data_sal ; data_salloop(13,:) = data_sal(1,:)
    data_sigmaloop(1:12,:) = data_sigma ; data_sigmaloop(13,:) = data_sigma(1,:)

    print*,minex0(D2=av_potemp),maxval(av_potemp),'av_potemp'
    print*,minex0(D2=av_sal),maxval(av_sal),'av_sal'
    print*,minex0(D2=av_sigma),maxval(av_sigma),'av_sigma'
    print*,minex0(D2=sem_potemp),maxval(sem_potemp),'sem_potemp'
    print*,minex0(D2=sem_sal),maxval(sem_sal),'sem_sal'
    print*,minex0(D2=sem_sigma),maxval(sem_sigma),'sem_sigma'

    call plots2('../Plots/Favorites/station1_final.ps')
    call header('Monthly Data of N-Line, Station 1')
    call otops(2.)

call plotsave('P')
call symbolc(width/2.,0.4,0.8,'Mean',0.)
! Potemp 
    !mean
        call symbolc(-2.,-height/2.,0.7,'PT',0.)
        call num_memori(0.,400.,41,10,0.5,-1,-height,-90);call mod12_memori(13,width,gap = 0,y = -height,symbol_size = 0.55)
        ! call butler_psk(av_potemploop,width,-height,0.,10.,30.,5.,'red',4,centralize = 0,r=r1,g=g1,b=b1,conti = 0.,continc = 1.,thicc = 5)
        call butler_cont(av_potemploop,width,-height,0.,0.,1.,thicc=5)
        ! call colorscale(4,r1,g1,b1,10.,30.,2,.5,1,height,0.3,lt = 1,gt = 1,rangle=90.,x= width+.2,y = -height/2.,symbol_start = 2)

    !sem
    call plot(width+2.,0.,-3)
call symbolc(width/2.,0.4,0.8,'Standard Error',0.)

        call memori(40,0.1,10,height,-90.,y=-height/2.);call mod12_memori(13,width,gap = 0,y = -height,symbol_size = 0.55)
        call butler_psk(sem_potemploop,width,-height,0.,0.,0.6,0.12,'wred',5,r=r1,g=g1,b=b1,centralize = 1)
        call colorscale(5,r1,g1,b1,0.,0.6,1,0.5,1,height,0.3,gt = 1,rangle=90.,x= width+.2,y = -height/2.)

    !ttest
    call plot(width+2.,0.,-3)
call symbolc(width/2.,0.9,0.7,"Welch's;T-test",0.)

        do m = 1,12
            if(m==12)then;month1 = 1 ; month2 = 12
            else;month1 = m+1 ; month2 = m
            end if
            do d = 1, depth
                resultt(m,d) = fwelcht(av_potemp(month1,d),s_potemp(month1,d),data_potemp(month1,d),av_potemp(month2,d),s_potemp(month2,d),data_potemp(month2,d))
            end do
        end do
        call butler_imask(resultt,width,-height,0)
        call butler_imask(resultt,width,-height,1,r=1.,g=0.5,b=0.5)
        call butler_imask(resultt,width,-height,-1,r=0.5,g=0.5,b=1.)
        call butler_imask(resultt,width,-height,911,r=0.,g=1.,b=0.)
        call memori(40,0.1,10,height,-90.,y=-height/2.);call mod12_memori(13,width,gap = 0,y=-height,symbol_size = 0.55)
        
call plotback('P')
call plot(0.,-height-1.,-3)
call plotsave('S')
! Sal
    !mean
        call symbolc(-2.,-height/2.,0.7,'Sal',0.)
        call num_memori(0.,400.,41,10,0.5,-1,-height,-90);call mod12_memori(13,width,gap = 0,y = -height,symbol_size = 0.55)
        call butler_psk(av_salloop,width,-height,0.,33.95,34.3,0.05,'b2w2r',7,bpt1=4,r=r2,g=g2,b=b2,centralize = 4,conti=33.,continc=0.1,thicc = 1)
        call colorscale(7,r2,g2,b2,33.95,34.3,2,0.5,2,height,0.3,lt = 1,gt = 1,rangle=90.,x= width+.2,y = -height/2.,symbol_start = 2)

    !sem
    call plot(width+2.,0.,-3)
    
        call memori(40,0.1,10,height,-90.,y=-height/2.);call mod12_memori(13,width,gap = 0,y = -height,symbol_size = 0.55)
        call butler_psk(sem_salloop,width,-height,0.,0.,0.1,0.02,'wred',5,r=r3,g=g3,b=b3,centralize = 1)
        call colorscale(5,r3,g3,b3,0.,0.1,1,0.5,2,height,0.3,gt = 1,rangle=90.,x= width+.2,y = -height/2.)
    
    !ttest
    call plot(width+2.,0.,-3)

        do m = 1, months
            if(m==12)then;month1 = 1 ; month2 = 12
            else;month1 = m+1 ; month2 = m
            end if
            do d = 1, depth
                results(m,d) = fwelcht(av_sal(month1,d),s_sal(month1,d),data_sal(month1,d),av_sal(month2,d),s_sal(month2,d),data_sal(month2,d))
            end do
        end do
        call butler_imask(results,width,-height,0)
        call butler_imask(results,width,-height,1,r=1.,g=0.5,b=0.5)
        call butler_imask(results,width,-height,-1,r=0.5,g=0.5,b=1.)
        call butler_imask(results,width,-height,911,r=0.,g=1.,b=0.)
        call memori(40,0.1,10,height,-90.,y=-height/2.);call mod12_memori(13,width,gap = 0,y=-height,symbol_size = 0.55)

call plotback('S')
call plot(0.,-height-1.,-3)
call plotsave('Sigma')

! Sigma
    !mean
        call symbolc(-2.3,-height/2.,0.7,'Sigma;Theta',0.)
        call num_memori(0.,400.,41,10,0.5,-1,-height,-90);call mod12_memori(13,width,gap = 0,y = -height,symbol_size = 0.55)
        call butler_cont(av_sigmaloop,width,-height,0.,20.,.2,thicc=5)

    !sem
    call plot(width+2.,0.,-3)
    
        call memori(40,0.1,10,height,-90.,y=-height/2.);call mod12_memori(13,width,gap = 0,y = -height,symbol_size = 0.55)
        call butler_psk(sem_sigmaloop,width,-height,0.,0.,0.1,0.02,'wred',5,r=r4,g=g4,b=b4,centralize = 1)
        call colorscale(5,r4,g4,b4,0.,0.1,1,0.5,2,height,0.3,gt = 1,rangle=90.,x= width+.2,y = -height/2.)

    !ttest
    call plot(width+2.,0.,-3)

        do m = 1, months
            if(m==12)then;month1 = 1 ; month2 = 12
            else;month1 = m+1 ; month2 = m
            end if
            do d = 1, depth
                resultsig(m,d) = fwelcht(av_sigma(month1,d),s_sigma(month1,d),data_sigma(month1,d),av_sigma(month2,d),s_sigma(month2,d),data_sigma(month2,d))
            end do
        end do
        call butler_imask(resultsig,width,-height,0)
        call butler_imask(resultsig,width,-height,1,r=1.,g=0.5,b=0.5)
        call butler_imask(resultsig,width,-height,-1,r=0.5,g=0.5,b=1.)
        call butler_imask(resultsig,width,-height,911,r=0.,g=1.,b=0.)
        call memori(40,0.1,10,height,-90.,y=-height/2.);call mod12_memori(13,width,gap = 0,y=-height,symbol_size = 0.55)


    call plote



end program
