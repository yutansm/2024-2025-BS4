program hakidame
    use always
    real,dimension(:,:,:,:),allocatable::U,V,T,S
    real,dimension(:,:,:),allocatable::Eta
    real,dimension(10)::lg1,lgs1
    character(len=256)::ncfile

    do i = 1, 10
        lg1(i) =  (-1.5)**real(i)
    end do
    print*,lg1
    do i = 1, 10
        lgs1(i) =  10.
    end do
    ! ncfile = "../MITgcm/verification/Yuta's_first_model/run_bdlvr/lv6xbeta100days/results/state.nc"
    ! call state2mat(ncfile,Uc=U,Vc=V,T=T,S=S,Eta=Eta,info = .true.)

    call plots2(oopt = 'ocenter',mode = 'portrait')
    ! call butler_vector(U(:,:,1,40),V(:,:,1,40),10.,20.,scalef = 20.,line_thickness = 2,arrowtype = 4,thinfx = 1,thinfy = 1,gap = 1)
    ! call griddedbox(10.,20.,3,60,100,dashy = -6)
    call butler_linegraph(lg1,10.,5.,-60.,60.,memiter = 6)
    call plote



end program 
