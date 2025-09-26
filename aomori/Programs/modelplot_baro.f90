program model_plot
    use always
    real,dimension(:,:,:,:),allocatable::Uc,Vc
    real,dimension(:,:,:),allocatable::eta
    real,dimension(:),allocatable::r, g, b
    character(len = 10)::unit_of_t
    width = 11.*0.7;height = 5.*0.7


    call state2mat('../MITgcm/verification/yuta_baro/testrun_OB2_0.2sv_rightopen/with_shelf/results/state.nc',info = .true.,Uc = Uc, Vc = Vc, eta = eta)
    call plots2('../MITgcm/verification/yuta_baro/testrun_OB2_0.2sv_rightopen/with_shelf/results/state.ps',oopt = 'otops',x = 1.,y = -height+1.,mode = 'portrait',h = 'first few hours')

    unit_of_t = 'hour'
    interval_t = 6
    itotal_t = 168

    n = itotal_t / interval_t + 1

    do i = 1, n
        call symbolr(-0.1,height/2.,0.6,trim(unit_of_t)//trim(int2str(interval_t*(i-1))))
        call butler_vector(Uc(:,:,1,interval_t*(i-1)+1),Vc(:,:,1,interval_t*(i-1)+1),width,height,thinfx = 10,thinfy = 10,scalef = 500.)
        call arrow(width + 1.,0.,width + 1.,1.);call symbolc(width + 1.,1.3,0.5,'0.2cm/s')
        call plot(width + 2.5,0.,-3)
        call butler_psk(eta(:,:,interval_t*(i-1)+1),width,height,0.,-0.005,0.005,0.001,'b2r',10,6,conti = -0.01,continc = 0.001,thicc = 2,r = r, g = g, b = b)
        call plot(-(width + 2.5),-height-.5,-3)
        if(mod(i,6) == 0)then 
            call ocenter(y = -12.)
            call colorscale(r,g,b,-0.5,0.5,2,0.6,2,15.,0.4,lt = 1, gt = 1);call symbolc(0.,-1.8,0.6,'eta(cm)')
            call newpage
        end if
    end do

    call plote
    print*,sum(Vc(:300,2,1,2) * 1000.* 2.*10.**3)*10.**(-6) ! sv

    print*,(Vc(:300,2,1,2))

end program 