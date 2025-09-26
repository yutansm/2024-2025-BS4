program input1
    use always
    real,dimension(100,100)::bathy1
    real,dimension(100,100,20)::initialT,initialS,initialV,array,relaxmask,taurelaxTS
    real,dimension(100,20,4)::OBEu_exp,test
    real,dimension(15,20)::modelTslice,modelSslice ! model array with uneven layer depths
    real,dimension(100,20)::OBSv
    real,dimension(100,100)::eta
    real,dimension(:),allocatable::r,g,b

    ! bathy really is a bitch, she's been wasting my time forever.
        bathy1 = -1000. ! Initialize bathymetry to -1000 m              
        bathy1(67,:) = -800. ! 660-670km meridional line at -800 m      
        bathy1(68,:) = -600. ! 670-680km meridional line at -600 m      
        bathy1(69,:) = -400. ! 680-690km meridional line at -400 m
        bathy1(70,:) = -200. ! 690-700km meridional line at -200 m  created a shelf region      
        bathy1(71:,:) = -100. ! eastern 300km domain at -100 m      tsugaru strait depth
        bathy1(100,57:60) = -100. ! open boudary strait 

        bathy1(71:,:56) = 0. ! isolating the strait this is the southern part of the strait
        bathy1(71:,61:) = 0. ! isolating the strait this is the northern part of the strait

        call plots2(nnfile = 'bathy1',oopt = 'obottoms')
        ! call butler_cont(bathy1,15.,15.,-999.,-100.,0.,r = 1.)
        call butler_psbet(bathy1,15.,15.,-999.,-1000.,0.,200.,'b2cy',5,3,r = r,g = g,b = b)
        ! call rgbk(0.,0.,0.)
        call butler_mask(bathy1,15.,15.,-100.,100.,r = 0.,g = 1.,b = 0.)
        call butler_mask(bathy1,15.,15.,0.,0.,r = 0.,g = 0.,b = 0.)
        call colorscale(r,g,b,-1000.,0.,1,0.7,-1,10.,0.3,rangle = 90.,x = 20.,y = 10.)
        open(123,file = '../MITgcm/verification/yuta_second_model/input1/bathy1.bin',form = 'unformatted',status = 'replace',access = 'direct',recl = 10000*4,convert = 'big_endian')
        write(123,rec = 1) bathy1
        close(123)
    ! bathy

    ! ! initialT_1
    !     open(123,file = '../MITgcm/verification/yuta_second_model/input1/T_Nline_x15*z20.bin',form = 'unformatted',status = 'old',action = 'read',access = 'direct',recl = 4*15*20,convert = 'big_endian')
    !     read(123,rec = 1) modelTslice(15:1:-1,:) ! read the model temperature slice in reverse order
    !     close(123)
    !     ! grid 69 is 400m deep, thus the nearest shore CTD data should be here
    !     ! 55:69 is the N line region,for 70-100 grids i will copy the 69th grid value
    !     initialT(55:69,:,:) = spread(modelTslice,2,100)
    !     initialT(1:54,:,:) = spread(initialT(55,:,:),1,54) ! copy the 55th grid value to the first 54 grids
    !     ! now for the shallow region <200m, 70th grid is 200, 71:100 grids are 100m deep in the strait
    !     initialT(70:100,:,:) = spread(initialT(69,:,:),1,31) ! copy the 69th grid value to the 70-100 grids
    !     ! hope the land area gets masked out

    !     open(123,file = '../MITgcm/verification/yuta_second_model/input1/initialT_1.bin',form = 'unformatted',status = 'replace',action = 'write',access = 'direct',recl = 10000*20*4,convert = 'big_endian')
    !     write(123,rec = 1) initialT
    !     close(123)
    !     ! creating OBSs files
    !     open(123,file = '../MITgcm/verification/yuta_second_model/input1/OBSt_1.bin',form = 'unformatted',status = 'replace',action = 'write',access = 'direct',recl = 100*20*4,convert = 'big_endian')
    !     write(123,rec = 1)initialT(:,1,:) ! OBSs are the first grid values
    !     close(123)
    ! ! initialS_1
    !     open(123,file = '../MITgcm/verification/yuta_second_model/input1/S_Nline_x15*z20.bin',form = 'unformatted',status = 'old',action = 'read',access = 'direct',recl = 4*15*20,convert = 'big_endian')
    !     read(123,rec = 1) modelSslice(15:1:-1,:) ! read the model salinity slice in reverse order
    !     close(123)
    !     initialS(55:69,:,:) = spread(modelSslice,2,100)
    !     initialS(1:54,:,:) = spread(initialS(55,:,:),1,54) ! copy the 55th grid value to the first 54 grids
    !     initialS(70:100,:,:) = spread(initialS(69,:,:),1,31) ! copy the 69th grid value to the 70-100 grids

    !     open(123,file = '../MITgcm/verification/yuta_second_model/input1/initialS_1.bin',form = 'unformatted',status = 'replace',action = 'write',access = 'direct',recl = 10000*20*4,convert = 'big_endian')
    !     write(123,rec = 1) initialS
    !     close(123)
    !     ! creating OBSs files
    !     open(123,file = '../MITgcm/verification/yuta_second_model/input1/OBSs_1.bin',form = 'unformatted',status = 'replace',action = 'write',access = 'direct',recl = 100*20*4,convert = 'big_endian')
    !     write(123,rec = 1)initialS(:,1,:) ! OBSs are the first grid values
    !     close(123)


    ! ! initialS_1
    ! ! initialV_1
    !     initialV_1 = 0.
    !     initialV_1(56:70,:,:) = 0.3 ! 0.3 m/s in the N line region
    !     open(123,file = '../MITgcm/verification/yuta_second_model/input1/initialV_1.bin',form = 'unformatted',status = 'replace',action = 'write',access = 'direct',recl = 4*100*100*20,convert = 'big_endian')
    !     write(123,rec = 1) initialV
    !     close(123)
    ! ! initialV_1
    ! ! OBSv_1
    !     OBSv_1 = 0.3
    !     open(123,file = '../MITgcm/verification/yuta_second_model/input1/OBSv_1.bin',form = 'unformatted',status = 'replace',access = 'direct',recl = 100*20*4,convert = 'big_endian')
    !     write(123,rec = 1) OBSv_1
    !     close(123)
    ! ! OBSv_1
    ! ! eta
    !     eta = 0.
    !     do i = 56, 70
    !         ! Linear interpolation formula: value = start + (i-1)/(n-1) * (end-start)
    !         eta(i,:) = 0.0 + (real(i-56)/14.) * 0.45 
    !     end do
    !     do i = 1, 100
    !         print*,i,eta(i,1)
    !     end do
    !     open(123, file='../MITgcm/verification/yuta_second_model/input1/eta1.bin', &
    !         form='unformatted', status='replace', access='direct',action = 'write', recl=4*100*100, convert='big_endian')
    !     write(123, rec=1) eta
    !     close(123)

    !     ! open(123,file = '../MITgcm/verification/yuta_second_model/input1/initialT_1.bin',form = 'unformatted',status = 'old',access = 'direct',recl = 10000*20*4,convert = 'big_endian')
    !     ! read(123,rec = 1) array
    !     ! close(123)
    ! ! eta

    ! ! relaxmask
    !     relaxmask = 1. ! whole domain equal nudging
    !     open(123,file = '../MITgcm/verification/yuta_second_model/input1/relaxmask.bin',form = 'unformatted',status = 'replace',action = 'write',access = 'direct',recl = 10000*20*4,convert = 'big_endian')
    !     write(123,rec = 1) relaxmask
    !     close(123)
    ! ! relaxmask
    ! relaxmask
        relaxmask = 1. ! 100 day nudging
        relaxmask(:,1,:) = 10. ! 10 day nudging for the first layer
        relaxmask(:,100,:) = 10. ! 10 day nudging for the last layer
        relaxmask(71:,57:60,:5) = 0. ! no nudging in the strait region
        open(123,file = '../MITgcm/verification/yuta_second_model/input1/relaxmask_2.bin',form = 'unformatted',status = 'replace',action = 'write',access = 'direct',recl = 10000*20*4,convert = 'big_endian')
        write(123,rec = 1) relaxmask
        close(123)
    ! relaxmask

    ! OBEu
    ! ! OBEu_exp = 0.375 !0.375 m/s * 4*10km * 100m = 1.5 Sv
    !     OBEu_exp = 0.
    !     OBEu_exp(57:60,:5,1) = 0.375 !0.375 m/s * 4*10km * 100m = 1.5 Sv
    !     OBEu_exp(57:60,:5,2) = 0. !0.375 m/s * 4*10km * 100m = 1.5 Sv
    !     OBEu_exp(57:60,:5,3) = 1. !0.375 m/s * 4*10km * 100m = 1.5 Sv
    !     OBEu_exp(57:60,:5,4) = 2. !0.375 m/s * 4*10km * 100m = 1.5 Sv
    !     ! OBEu_exp(:,:,5) = 3. !0.375 m/s * 4*10km * 100m = 1.5 Sv
    !     open(123,file = '../MITgcm/verification/yuta_second_model/input2/OBEu_exp.bin',form = 'unformatted',status = 'replace',action = 'write',access = 'direct',recl = 4*100*20*4,convert = 'big_endian')
    !     write(123,rec = 1) OBEu_exp
    !     close(123)

    !     open(123,file = '../MITgcm/verification/yuta_second_model/input2/OBEu_exp.bin',form = 'unformatted',status = 'old',action = 'read',access = 'direct',recl = 4*100*20*4,convert = 'big_endian')
    !     read(123,rec = 1) test
    !     close(123)


    !     ! print*, 'OBEu_exp', OBEu_exp
    !     print*, 'test', count(test/= 0.)
    ! ! OBEu
        call plote
end program 