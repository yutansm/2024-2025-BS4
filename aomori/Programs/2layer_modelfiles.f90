program modelfiles  
    use always
    real,dimension(200,200) :: bathy
    real,dimension(200,200,32)::T1,S1,V1
    real,dimension(200,200,34)::T2,S2,V2
    real,dimension(200,200,100)::T1plot,S1plot,V1plot,T2plot,S2plot,V2plot
    real,dimension(200,200)::surface_height
    real::eta1,eta2
    integer::internal_displacement_grids

    ! ! bathymetry
    !     bathy = -950.
    !     bathy(126:200,:) = 0. !land
    !     bathy(126:200,116:125) = -100. ! strait
    !     open(123,file = '../MITgcm/verification/yuta_2layer/input1/bathy.bin',form='unformatted',status='replace',convert='big_endian')
    !     write(123) bathy
    !     close(123)
        bathy = -1000.
        bathy(126:200,:) = 0. !land
        bathy(126:200,116:125) = -100. ! strait
        open(123,file = '../MITgcm/verification/yuta_2layer/input2/bathy2.bin',form='unformatted',status='replace',convert='big_endian')
        write(123) bathy
        close(123)
    ! ! bathymetry
    ! ! T1S1V1
    !     T1 = 0.5;S1 = 34.1;surface_height = 0.;V1 = 0. ! initial condition
    !     do i = 125,1,-1
    !         eta2 = -100. - 100.*exp(-real(125-i)*4000./14000.)
    !         eta1 = 0.1 * exp(-real(125-i)*4000./14000.) ! surface displacement
    !         ! print*,'internal displacement from east to west',i,eta2,exp(-real(i)*4000./14000.)
    !         ! print*,'internal displacement nearest 10s' ,i,10 * nint(eta2/10.)
    !         ! print*,'internal displacement as grids',i, 10 * nint(eta2/10.)/(-10)
    !         ! print*,'surface displacement from east to west',i,eta1
    !         ! print*,'surface displacement as grids',i, 10 * nint((-50.*exp(-real(125-i)*4000./14000.))/10.)
    !         internal_displacement_grids = nint(-eta2/10.)
    !         T1(i,:,:internal_displacement_grids) = 10.
    !         S1(i,:,:internal_displacement_grids) = 34.1
    !         surface_height(i,:) = eta1
    !         V1(i,:,:internal_displacement_grids) = -(eta2+100.)*0.007 ! initial velocity proportional to surface displacement
    !         print*,V1(i,120,1)
    !         ! V1(i,:,internal_displacement_grids+1:) = -(eta2+100.)*0.006*0.1 ! pycnocline velocity is -1/4 of above. induce the same amount in the opposite direction to counter no net flow
    !         ! print*,i, -(eta2+100.)*0.006 * 0.25
    !     end do
    !     surface_height(126:200,116:125) = 0.1 ! rho1, strait region is layer1

    !     T1(126:200,116:125,:10) = spread(T1(125,116:125,:10),1,75) ! strait region is rho1,100m depth
    !     S1(126:200,116:125,:10) = spread(S1(125,116:125,:10),1,75) ! strait region is rho1,100m depth
    !     ! strait region velocity is 0 at initial
    !     ! print*,T1(:,120,:)
    !     open(123,file = '../MITgcm/verification/yuta_2layer/input2/T1.bin',form='unformatted',status='replace',convert='big_endian')
    !     write(123) T1
    !     close(123)
    !     open(123,file = '../MITgcm/verification/yuta_2layer/input2/S1.bin',form='unformatted',status='replace',convert='big_endian')
    !     write(123) S1
    !     close(123)
    !     open(123,file = '../MITgcm/verification/yuta_2layer/input2/surface_height.bin',form='unformatted',status='replace',convert='big_endian')
    !     write(123) surface_height
    !     close(123)
    !     open(123,file = '../MITgcm/verification/yuta_2layer/input2/V1.bin',form='unformatted',status='replace',convert='big_endian')
    !     write(123) V1
    !     close(123)
        

    !     T1plot(:,:,:25) = T1(:,:,1:25);T1plot(:,:,26:100) = spread(T1(:,:,25),3,75)
    !     S1plot(:,:,:25) = S1(:,:,1:25);S1plot(:,:,26:100) = spread(S1(:,:,25),3,75)
    !     V1plot(:,:,:25) = V1(:,:,1:25);V1plot(:,:,26:100) = spread(V1(:,:,25),3,75)

    !     call plots2(psfile = '../MITgcm/verification/yuta_2layer/input1/T1S1.ps',x = 1., y = -3.,oopt = 'otops')
    !     call butler_linegraph(surface_height(:,120),10.,3.,0.,0.1,0.,.true.,memfreq = 0.01,memsymfreq = 2,memflqt = 2)
    !     call butler_linegraph(V1plot(:,120,1),10.,3.,0.,1.,0.,.true.,memloc = 'right',memfreq = 0.2,gl = 1.)
    !     call butler_linegraph(V1plot(:,120,30),10.,3.,-0.,1.,0.,.true.,memloc = 'right',memfreq = 0.2,rl = 1.)
    !     call butler_psk(S1plot(:,120,:),10.,-10.,0.,33.95,34.3,0.05,'b2w2r',7,bpt1 = 4)
    !     call num_memori2(0.,1000.,-10.,100.,-90.,2,float_quantity = -1,symbol_size = 0.6)
    !     call num_memori2(0.,800.,10.,200.,0.,1,y = -10.,float_quantity = -1,symbol_size = 0.6)
    !     call butler_cont(T1plot(:,120,:),10.,-10.,0.,0.,5.,thicc = 2)

    !     call plot(12.,-4.,-3)
    !     call butler_psbet(bathy,8.,8.,0.,-1000.,0.,100.,'b2cy',10,6)
    !     call plot(0.,-1.,-3)
    !     call butler_psk(V1plot(:125,120,:20),6.,-6.,0.,0.,0.7,0.1,'red',7,4,conti = 0.,continc = 0.05)

    ! ! T1S1V1
    ! ! T2S2V2
            T2 = 0.5;S2 = 34.1;surface_height = 0.;V2 = 0. ! initial condition
            do i = 125,1,-1
                eta2 = -100. - 100.*exp(-real(125-i)*4000./14000.)
                eta1 = 0.1 * exp(-real(125-i)*4000./14000.) ! surface displacement
                ! print*,'internal displacement from east to west',i,eta2,exp(-real(i)*4000./14000.)
                ! print*,'internal displacement nearest 10s' ,i,10 * nint(eta2/10.)
                ! print*,'internal displacement as grids',i, 10 * nint(eta2/10.)/(-10)
                ! print*,'surface displacement from east to west',i,eta1
                ! print*,'surface displacement as grids',i, 10 * nint((-50.*exp(-real(125-i)*4000./14000.))/10.)
                internal_displacement_grids = nint(-eta2/10.)
                T2(i,:,:internal_displacement_grids) = 10.
                S2(i,:,:internal_displacement_grids) = 34.1
                surface_height(i,:) = eta1
                V2(i,:,:internal_displacement_grids) = -(eta2+100.)*0.007 ! initial velocity proportional to surface displacement
                print*,V2(i,120,1)
                ! V2(i,:,internal_displacement_grids+1:) = -(eta2+100.)*0.006*0.1 ! pycnocline velocity is -1/4 of above. induce the same amount in the opposite direction to counter no net flow
                ! print*,i, -(eta2+100.)*0.006 * 0.25
            end do
            surface_height(126:200,116:125) = 0.1 ! rho1, strait region is layer1

            T2(126:200,116:125,:10) = spread(T2(125,116:125,:10),1,75) ! strait region is rho1,100m depth
            S2(126:200,116:125,:10) = spread(S2(125,116:125,:10),1,75) ! strait region is rho1,100m depth
            ! strait region velocity is 0 at initial
            ! print*,T1(:,120,:)
            open(123,file = '../MITgcm/verification/yuta_2layer/input2/T2.bin',form='unformatted',status='replace',convert='big_endian')
            write(123) T2
            close(123)
            open(123,file = '../MITgcm/verification/yuta_2layer/input2/S2.bin',form='unformatted',status='replace',convert='big_endian')
            write(123) S2
            close(123)
            open(123,file = '../MITgcm/verification/yuta_2layer/input2/surface_height.bin',form='unformatted',status='replace',convert='big_endian')
            write(123) surface_height
            close(123)
            open(123,file = '../MITgcm/verification/yuta_2layer/input2/V2.bin',form='unformatted',status='replace',convert='big_endian')
            write(123) V2
            close(123)
            

            T2plot(:,:,:26) = T2(:,:,1:26);T2plot(:,:,27:100) = spread(T2(:,:,27),3,74)
            S2plot(:,:,:26) = S2(:,:,1:26);S2plot(:,:,27:100) = spread(S2(:,:,27),3,74)
            V2plot(:,:,:26) = V2(:,:,1:26);V2plot(:,:,27:100) = spread(V2(:,:,27),3,74)

            call plots2(psfile = '../MITgcm/verification/yuta_2layer/input2/T2S2.ps',x = 1., y = -3.,oopt = 'otops')
            call butler_linegraph(surface_height(:,120),10.,3.,0.,0.1,0.,.true.,memfreq = 0.01,memsymfreq = 2,memflqt = 2)
            call butler_linegraph(V2plot(:,120,1),10.,3.,0.,1.,0.,.true.,memloc = 'right',memfreq = 0.2,gl = 1.)
            call butler_linegraph(V2plot(:,120,30),10.,3.,-0.,1.,0.,.true.,memloc = 'right',memfreq = 0.2,rl = 1.)
            call butler_psk(S2plot(:,120,:),10.,-10.,0.,33.95,34.3,0.05,'b2w2r',7,bpt1 = 4)
            call num_memori2(0.,1000.,-10.,100.,-90.,2,float_quantity = -1,symbol_size = 0.6)
            call num_memori2(0.,800.,10.,200.,0.,1,y = -10.,float_quantity = -1,symbol_size = 0.6)
            call butler_cont(T2plot(:,120,:),10.,-10.,0.,0.,5.,thicc = 2)

            call plot(12.,-4.,-3)
            call butler_psbet(bathy,8.,8.,0.,-1000.,0.,100.,'b2cy',10,6)
            call plot(0.,-1.,-3)
            call butler_psk(V2plot(:125,120,:26),6.,-6.,0.,0.,0.7,0.1,'red',7,4,conti = 0.,continc = 0.05)
    ! ! T2S2V2

    !     call plot(0.,-1.,-3)
    !     call butler_psk(V2plot(:125,120,:20),6.,-6.,0.,0.,0.7,0.1,'red',7,4,conti = 0.,continc = 0.05)
    !     call butler_psk(S2plot(:125,120,:20),6.,-6.,0.,0.,0.7,0.1,'blue',7,4,conti = 0.,continc = 0.05)
    !     call butler_psk(T2plot(:125,120,:20),6.,-6.,0.,0.,0.7,0.1,'green',7,4,conti = 0.,continc = 0.05)

    call plote




end program