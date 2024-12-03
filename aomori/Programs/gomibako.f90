program test
    use always
    real,dimension(:),allocatable::dh
    real,dimension(6,1)::dh2
    real,dimension(120,1)::obj
    real,dimension(120,200)::inieta
    real,dimension(120,20)::temp,sal,vel
    real,dimension(:,:),allocatable::geovel,realgeo
    real,dimension(120,20)::newobsv
    real,dimension(120,200,20)::newiniv
    real,dimension(:,:,:,:),allocatable::avsigma,avpotemp,avsal
    call calibrated_data51(potemp_c5,sal_c5) ! 15*12*2*9*400
    call create_sigma_array(potemp_c5, sal_c5, sigma_c5)
    call avsemdata_5D(potemp_c5,'dim1',mean_4D = avpotemp)
    call avsemdata_5D(sal_c5,'dim1',mean_4D = avsal)
    call avsemdata_5D(sigma_c5,'dim1',mean_4D = avsigma)
    call calc_dh(dh,den_2D = avsigma(9,1,4:9,:))
    open(unit=20, file="../MITgcm/verification/yuta's_first_model/input_f_bigdomain_bigvisc/OBS_T.bin", status='old', action='read', form='unformatted', access='direct', recl=4*120*20, convert='big_endian')
    read(20,rec=1) temp
    close(20)
    open(unit=21, file="../MITgcm/verification/yuta's_first_model/input_f_bigdomain_bigvisc/OBS_S.bin", status='old', action='read', form='unformatted', access='direct', recl=4*120*20, convert='big_endian')
    read(21,rec=1) sal
    close(21)
    call calc_geovel(geovel,5000.,delta_zdb = 20.,temp_2D = temp,sal_2D = sal,lat = 41.)
    call calc_geovel(realgeo,28000.,temp_2D = avpotemp(9,1,4:9,:),sal_2D = avsal(9,1,4:9,:),lat = 41.)
    print*,size(geovel,1),size(geovel,2)
    newobsv(1:119,:) = geovel;newobsv(120,:) = geovel(119,:)
    open(unit=22, file="../MITgcm/verification/yuta's_first_model/input_f_bigdomain_bigvisc/OBS_V_lowres.bin", status='unknown', action='write', form='unformatted', access='direct', recl=4*120*20, convert='big_endian')
    write(22,rec=1) newobsv
    close(22)
    do i = 1,200
        newiniv(:,i,:) = newobsv
    end do
    open(unit=23, file="../MITgcm/verification/yuta's_first_model/input_f_bigdomain_bigvisc/initial_V.bin", status='replace', action='write', form='unformatted', access='direct', recl=4*120*200*20, convert='big_endian')
    write(23,rec=1) newiniv
    close(23)

    call calc_dh(dh,temp_2D = temp,sal_2D = sal,delta_zdb = 20.)
    print*,size(dh)
    open(unit=24, file="../MITgcm/verification/yuta's_first_model/input_f_bigdomain_bigvisc/OBS_Eta.bin", status='replace', action='write', form='unformatted', access='direct', recl=4*120, convert='big_endian')
    write(24,rec=1) dh
    close(24)
    do i = 1, 200
        inieta(:,i) = dh
    end do
    print*,size(inieta)
    open(unit=24, file="../MITgcm/verification/yuta's_first_model/input_f_bigdomain_bigvisc/initial_Eta.bin", status='replace', action='write', form='unformatted', access='direct', recl=4*120*200, convert='big_endian')
    write(24,rec=1) inieta
    close(24)
    call plots2(psfile = "../MITgcm/verification/yuta's_first_model/input_f_bigdomain_bigvisc/geovellowres.ps",oopt = 'otops',h = 'geovel')
    call butler_cont(newobsv,5.,-10.,1000.,-0.2,0.02,thicc = 5,maskn = .true.) ! lowres v
    call plot(6.,0.,-3)
    call butler_cont(newiniv(:,45,:),5.,-10.,1000.,-0.2,0.02,thicc = 5,maskn = .true.) ! random slice of lowres v for checking
    call plot(6.,0.,-3)
    call butler_cont(realgeo,5.,-10.,10000.,-0.2,0.02,thicc = 5,maskn = .true.) ! ctd data
    call obottoms
    dx = 20./121.
    call box(20.,10.,3)
    do i = 1, 120
        call gmark_ratio(dh(i),396.,398.,10.,ploty)
        call gmark(dx*real(i),ploty,0.1,1)
    end do
    call plote
end program