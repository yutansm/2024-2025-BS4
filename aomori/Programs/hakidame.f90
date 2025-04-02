program reproducible_random
    use always
    implicit none
    integer,parameter::size = 200000,sample_size = 2,sample_size2 = 5
    real,parameter::ini = -4.,fin = 4.,lambda = 0.8
    integer :: seed(2,8) ! size for the seed array in gfortran is 8
    real,dimension(:,:),allocatable::Z,T,expsamples
    real,dimension(:),allocatable::dx,fn,X12,gn,Chi,dx2,fT,Exponential,expsamplemeans,expscaledmean,expsamplevarsq
    real::mu,sigma
    
    call plots2(nnfile = 'Standard_Normal1',oopt = 'obottoms',x = 2., y = 5.)
    ! do i = 1,2
    !     do j =  1, 8
    !         seed(i,j) = 2**(i+j)
    !     end do
    ! end do
    call Z_variables(Z,size)
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
    allocate(X12(size))
    X12 = Z(1,:)**2 + Z(2,:)**2

    call xixfdx(dx,ini,fin,iter = 100,info = .true.)
    call xixfdx(dx2,0.,5.,iter = 100,info = .true.)
    allocate(fn(100),gn(100),Chi(100),fT(100))
    fn = 1./sqrt(2*pi)*exp(-0.5*dx**2)
    gn = gamma(dx)
    Chi = f_Chisq(dx2,2)
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


    call histogram_PMF(Z(1,:),10.,10.,ini,fin,0.05,yf = 0.5)
    call num_memori2(ini,fin,10.,0.1,symbol_freq = 20,symbol_size = 1.3)
    call num_memori2(0.,.5,10.,0.1,-90.)
    call helper_linegraph(fn,10.,10.,0.,.5,rl = 1.,lthick = 6)
    ! call helper_linegraph(fT,10.,10.,0.,.5,gl = 1.,lthick = 4)

    call plot(13.,0.,-3)
    call histogram_PMF(X12,10.,10.,0.,5.,0.05)
    call num_memori2(0.,1.,10.,0.1,-90.)
    call num_memori2(0.,5.,10.,0.1,0.,symbol_freq = 10)
    call helper_linegraph(Chi,10.,10.,0.,1.,rl = 1.)

    call newpage(x = 2.,y = 5.)
    call histogram_PMF(T(1,:),10.,10.,ini,fin,0.05,yf = 0.5)
    call num_memori2(ini,fin,10.,0.1,symbol_freq = 20,symbol_size = 1.3)
    call num_memori2(0.,.5,10.,0.1,-90.)
    call helper_linegraph(fT,10.,10.,0.,.5,gl = 1.)

    call newpage(x = 2.,y = 5.)
    call histogram_PMF(Exponential,10.,10.,0.,5.,0.05)
    call num_memori2(0.,1.,10.,0.1,-90.)
    call num_memori2(0.,5.,10.,0.1,0.,symbol_freq = 10)

    call plot(13.5,0.,-3)
    call histogram_PMF(expscaledmean,10.,10.,ini,fin,0.05,yf = 0.5)
    call num_memori2(0.,1.,10.,0.1,-90.)
    call num_memori2(ini,fin,10.,0.1,0.,symbol_freq = 20)
    call helper_linegraph(fn,10.,10.,0.,.5,rl = 1.)


    call newpage(x = 5.)
    call butler_linegraph(gn,10.,10.,-5.,5.,rl = 1.,mem = .true.,dots = .true.)

    call plote

    deallocate(Z)

end program reproducible_random