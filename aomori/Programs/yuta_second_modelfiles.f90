program stupid
    use always
    real,dimension(6,400)::temp,sal
    real,dimension(15,400)::LItemp,LIsal
    real,dimension(15,20)::modelTslice,modelSslice ! model array with uneven layer depths
    real,dimension(15,100)::mTsliceuniform,mSsliceuniform ! an array for plotting; even layers
    real,dimension(5,400)::vel
    real,dimension(100,100,20)::initialT_2,initialS_2
    real,dimension(:),allocatable :: array


    width = 4.; height = 8.
    open(123,file = '../Data/Composites_Binaries/PT_Sep_trapped.bin',form = 'unformatted',status = 'old',access = 'direct',recl = 6*400*4,convert = 'big_endian')
    read(123,rec = 1) temp
    close(123)
    open(123,file = '../Data/Composites_Binaries/S_Sep_trapped.bin',form = 'unformatted',status = 'old',access = 'direct',recl = 6*400*4,convert = 'big_endian')
    read(123,rec = 1) sal
    close(123)
    open(123,file = '../Data/Composites_Binaries/V_Sep_trapped.bin',form = 'unformatted',status = 'old',access = 'direct',recl = 5*400*4,convert = 'big_endian')
    read(123,rec = 1) vel
    close(123)

    call DATA2OBJ(0,15,400,1,temp,OBJ_2D = LItemp) ! the N line st1 - 6 region
    call DATA2OBJ(0,15,400,1,sal,OBJ_2D = LIsal)

    ! creating the first 14 layer of model array (upto 400m) with uneven layer depths
    do i = 1, 14
        if(i<11)then ! 20m * 10 layers
            call avsemdata_2D(LItemp(:,(i-1)*20+1:i*20),'dim2',mean_1D = array)
            print*,i,(i-1)*20+1,i*20,'modeltemp',2*(i-1)+1,2*i,'unideptemp'
            modelTslice(:,i) = array
            mTsliceuniform(:,2*(i-1)+1:2*i) = spread(array,2,2);deallocate(array)
            call avsemdata_2D(LIsal(:,(i-1)*20+1:i*20),'dim2',mean_1D = array)
            modelSslice(:,i) = array
            mSsliceuniform(:6,2*(i-1)+1:2*i) = spread(array,2,2);deallocate(array)

        else ! 50m * 4 layers
            call avsemdata_2D(LItemp(:,(i-11)*50+201:(i-10)*50+200),'dim2',mean_1D = array)
            print*,i,(i-11)*50+201,(i-10)*50+200,'modeltemp',5*(i-11)+21,5*(i-10)+20,'unideptemp'
            modelTslice(:,i) = array
            mTsliceuniform(:,5*(i-11)+21:5*(i-10)+20) = spread(array,2,5);deallocate(array)
            call avsemdata_2D(LIsal(:,(i-11)*50+201:(i-10)*50+200),'dim2',mean_1D = array)
            modelSslice(:,i) = array
            mSsliceuniform(:,5*(i-11)+21:5*(i-10)+20) = spread(array,2,5);deallocate(array)
        end if
    end do
    modelTslice(:,15:20) = 0.8 ! uniform temperature in the bottom 6 layers
    modelSslice(:,15:20) = 34.07 ! uniform salinity in the bottom 6 layers
    mTsliceuniform(:,41:100) = 0.8 ! an array for plotting; even layers
    mSsliceuniform(:,41:100) = 34.07

    open(123,file = '../MITgcm/verification/yuta_second_model/input1/T_Nline_x15*z20.bin',form = 'unformatted',status = 'replace',action = 'write',access = 'direct',recl = 4*15*20,convert = 'big_endian')
    write(123,rec = 1) modelTslice
    close(123)
    open(123,file = '../MITgcm/verification/yuta_second_model/input1/S_Nline_x15*z20.bin',form = 'unformatted',status = 'replace',action = 'write',access = 'direct',recl = 4*15*20,convert = 'big_endian')
    write(123,rec = 1) modelSslice
    close(123)


    call plots2(nnfile = 'PT_Sep_trapped2',oopt = 'otops',x = 1.)
    call symbolc(width/2.,0.8,0.5,'model TS;N line region')
    call num_memori2(0.,1000.,-height,100.,-90.,float_quantity = -1,symbol_size = 0.5)
    call butler_psbet(mSsliceuniform(15:1:-1,:),width,-height,0.,33.95,34.3,0.05,'b2r',7,4)
    call butler_cont(mTsliceuniform(15:1:-1,:),width,-height,-999.,0.,1.,thicc = 5)
    call plot(width + 0.5,0.,-3)
    call symbolc(width/2.,0.2,0.4,'TS not uniform')
    call butler_psbet(modelSslice(15:1:-1,:),width,-height,0.,33.95,34.3,0.05,'b2r',7,4)
    call butler_cont(modelTslice(15:1:-1,:),width,-height,-999.,0.,1.,thicc = 5)


    do i = 1, 100
        print*,i,mTsliceuniform(:,i)
    end do

    call plote

end program
