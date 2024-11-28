program test
    use always
    implicit none
    real,dimension(:,:,:,:),allocatable::avpotemp,avsal,avgeovel
    character(len=100):: filenamewoex

    call calibrated_data51(potemp_c5,sal_c5) ! 15*12*2*9*400
    call geovel_array(51,geovel_5)
    call avsemdata_5D(potemp_c5,'dim1',mean_4D = avpotemp)
    call avsemdata_5D(sal_c5,'dim1',mean_4D = avsal)
    call avsemdata_5D(geovel_5,'dim1',mean_4D = avgeovel)
    avgeovel = avgeovel * 0.01


    call DATA2OBJ(0,40,20,86400,avpotemp(9,1,4:9,:),filenamewoex = "../MITgcm/verification/yuta's_first_model/input4/OBS_T",createcsv = .true.,createbin = .true.)
    call DATA2OBJ(0,40,20,86400,avsal(9,1,4:9,:),filenamewoex = "../MITgcm/verification/yuta's_first_model/input4/OBS_S",createcsv = .true.,createbin = .true.)
    call DATA2OBJ(0,40,20,86400,avgeovel(9,1,5:9,:),filenamewoex = "../MITgcm/verification/yuta's_first_model/input4/OBS_V",createcsv = .true.,createbin = .true.)
    ! filenamewoex = "../MITgcm/verification/yuta's_first_model/input3/OBS_T"
    ! print*, change_extension(filenamewoex, "csv")
end program