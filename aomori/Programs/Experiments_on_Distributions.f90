program reproducible_random
    use always
    implicit none
    integer,parameter::size = 200000,sample_size = 20,sample_size2 = 100
    real,parameter::ini = -4.,fin = 4.,lambda = 0.8
    integer :: seed(2,8) ! size for the seed array in gfortran is 8
    real,dimension(:,:),allocatable::Z,T,expsamples,Z1
    real,dimension(:),allocatable::dx,fn,X12,X123,X1234,gn,Chi,Chi3,Chi4,dx2,fT,Exponential,expsamplemeans,expscaledmean,expsamplevarsq
    real::mu,sigma
    
    call plots2(nnfile = 'Standard_Normal3+',oopt = 'obottoms',x = 2., y = 5.)
    ! do i = 1,2
    !     do j =  1, 8
    !         seed(i,j) = 2**(i+j)
    !     end do
    ! end do
    call Z_variables(Z,size)
    call Z_variables(Z1,size)
    call T_variables(T,size,sample_size)
    call exp_variables(Exponential,size,lambda)
    call avsemdata_1D(Exponential,mean = mu, s = sigma)
    print*,mu,sigma
    call exp_var_samples(expsamples,size,sample_size2,lambda)
    allocate(expsamplemeans(size),expscaledmean(size),expsamplevarsq(size))
    do i = 1, size
        call avsemdata_1D(expsamples(i,:),mean = expsamplemeans(i), s = expsamplevarsq(i))
        expscaledmean(i) = (expsamplemeans(i) - mu)/sigma*sqrt(real(sample_size2))
        ! expscaledmean(i) = (expsamplemeans(i) - mu)/expsamplevarsq(i)*sqrt(real(sample_size2))
    end do
    ! print*,T
    allocate(X12(size),X123(size),X1234(size))
    allocate(dx(size),dx2(size))
    X12 = Z(1,:)**2 + Z(2,:)**2
    X123 = Z(1,:)**2 + Z(2,:)**2 + Z1(1,:)**2
    X1234 = Z(1,:)**2 + Z(2,:)**2 + Z1(1,:)**2 + Z1(2,:)**2

    call xixfdx(dx,ini,fin,iter = 100,info = .true.)
    call xixfdx(dx2,0.1,5.,iter = 100,info = .true.)
    allocate(fn(100),gn(100),Chi(100),fT(100))
    fn = 1./sqrt(2*pi)*exp(-0.5*dx**2)
    gn = gamma(dx2)
    Chi = f_Chisq(dx2,2)
    Chi3 = f_Chisq(dx2,3)
    Chi4 = f_Chisq(dx2,4)
    fT = f_T(dx,sample_size)

    do i = 1, 100
        ! print*,dx(i),fn(i)
        ! print*,gn(i)
        if(ieee_is_nan(dx(i)))print*,'dx is nan at ',i
        if(ieee_is_nan(fn(i)))print*,'fn is nan at ',i
        if(ieee_is_nan(gn(i)))then;print*,'gn is nan at ',i;gn(i) = 0.;end if
        if(ieee_is_nan(Chi(i)))print*,'Chi is nan at ',i
        if(ieee_is_nan(fT(i)))print*,'fT is nan at ',i,fT(i)
    end do


    call symbolc(5.,10.2,0.8,int2str(size))
    call histogram_PMF(Z(1,:),10.,10.,ini,fin,0.05,yf = 0.5)
    call num_memori2(ini,fin,10.,0.1,symbol_freq = 20,symbol_size = 1.3)
    call num_memori2(0.,.5,10.,0.1,-90.)
    call helper_linegraph(fn,10.,10.,0.,.5,rl = 1.,lthick = 6)
    ! call helper_linegraph(fT,10.,10.,0.,.5,gl = 1.,lthick = 4)

    call plot(13.,0.,-3)
    call symbolc(5.,10.2,0.8,int2str(4))
    call histogram_PMF(X1234,10.,10.,0.,5.,0.05)
    call num_memori2(0.,1.,10.,0.1,-90.)
    call num_memori2(0.,5.,10.,0.1,0.,symbol_freq = 10)
    call helper_linegraph(Chi,10.,10.,0.,1.,rl = 1.)
    call helper_linegraph(Chi3,10.,10.,0.,1.,bl = 1.)
    call helper_linegraph(Chi4,10.,10.,0.,1.,gl = 1.)

    call newpage(x = 2.,y = 5.)
    call symbolc(5.,10.2,0.8,'sample size : '//int2str(sample_size))
    call histogram_PMF(T(1,:),10.,10.,ini,fin,0.05,yf = 0.5)
    call num_memori2(ini,fin,10.,0.1,symbol_freq = 20,symbol_size = 1.3)
    call num_memori2(0.,.5,10.,0.1,-90.)
    call helper_linegraph(fT,10.,10.,0.,.5,gl = 1.,lthick = 10)
    call helper_linegraph(fn,10.,10.,0.,.5,rl = 1.,lthick = 6)

    call newpage(x = 2.,y = 5.)
    call histogram_PMF(Exponential,10.,10.,0.,5.,0.05)
    call num_memori2(0.,1.,10.,0.1,-90.)
    call num_memori2(0.,5.,10.,0.1,0.,symbol_freq = 10)

    call plot(13.5,0.,-3)
    ! print*,real2str(mu,2),real2str(sigma,2)
    ! call symbolc(5.,12.2,0.8,'mean = '//real2str(mu,2))

    call numberc(5.,12.2,0.8,mu,0.,4)
    call numberc(5.,11.2,0.8,sigma,0.,4)
    call symbolc(5.,10.2,0.8,'sample size : '//int2str(sample_size2))
    call histogram_PMF(expscaledmean,10.,10.,ini,fin,0.05,yf = 0.5)
    call num_memori2(0.,1.,10.,0.1,-90.)
    call num_memori2(ini,fin,10.,0.1,0.,symbol_freq = 20)
    call helper_linegraph(fn,10.,10.,0.,0.5,rl = 1.)


    call newpage(x = 5.)
    call butler_linegraph(gn,10.,10.,0.,5.,rl = 1.,mem = .true.)
    call num_memori2(0.,5.,10.,1.,0.)

    call plote

    deallocate(Z)

end program reproducible_random