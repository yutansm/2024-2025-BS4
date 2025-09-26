program era5
    use always

    real,dimension(:,:,:),allocatable::u,v,w,temp ! lon*lat*1020(85years*12months)
    real,dimension(:,:,:,:),allocatable::u2,v2,w2,temp2,theta,uvscaler
    real,dimension(:,:,:),allocatable::umean,vmean,wmean,tempmean,thetamean, thetasem, uvscalersem,uvar,vvar,uvar2,vvar2! monthly means of 2009-2023 data
    real:: tap(15,12), mat(15,12),tap_mat(15,12),tap_mat_monthlyloop(13),tap_mat_semloop(13)
    real,dimension(:),allocatable::tap_mat_monthly,tap_mat_sem


    width = 7.

    ! original array has N20-50,E120-150
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                    ! DATA OBTAINMENT AND MANIPULTAION
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ilon = (139-120)*4+1
        iflon = (142-120)*4+1
        ilat = (40-20)*4+1
        iflat = (42-20)*4+1
        call ERA5_1000hPa(temp,u,v,w,info = .true.)
            allocate(u2(size(u,1),size(u,2),85,12))
            allocate(v2(size(v,1),size(v,2),85,12))
            allocate(w2(size(w,1),size(w,2),85,12))
            allocate(temp2(size(temp,1),size(temp,2),85,12))
            allocate(theta(size(temp,1),size(temp,2),85,12))
            allocate(uvscaler(size(temp,1),size(temp,2),85,12))
        do i = 1, 85 ! turning timeseries data into monthly indices
            do j = 1, 12
                u2(:,:,i,j) = u(:,:,(i-1)*12+j)
                v2(:,:,i,j) = v(:,:,(i-1)*12+j)
                w2(:,:,i,j) = w(:,:,(i-1)*12+j)
                temp2(:,:,i,j) = temp(:,:,(i-1)*12+j)
                theta(:,:,i,j) = atan2(v2(:,:,i,j),u2(:,:,i,j)) ! got the angles for variance
                uvscaler(:,:,i,j) = sqrt(u2(:,:,i,j)**2 + v2(:,:,i,j)**2) ! got the speed scaler for variance
                ! print*,theta(:,:,i,j),'theta',i,j
            end do
        end do ! made 85years*12months
        ! getting means of monthly data
            call avsemdata_4D(u2(:,:,1989-1940+1:2023-1940+1,:),'dim3',mean_3D = umean)
            call avsemdata_4D(v2(:,:,1989-1940+1:2023-1940+1,:),'dim3',mean_3D = vmean)
            call avsemdata_4D(w2(:,:,1989-1940+1:2023-1940+1,:),'dim3',mean_3D = wmean)
            call avsemdata_4D(temp2(:,:,1989-1940+1:2023-1940+1,:),'dim3',mean_3D = tempmean)
            call avsemdata_4D(theta(:,:,1989-1940+1:2023-1940+1,:),'dim3',mean_3D = thetamean,sem_3D = thetasem) ! variance of theta
            call avsemdata_4D(uvscaler(:,:,1989-1940+1:2023-1940+1,:),'dim3',sem_3D = uvscalersem) ! variance of uvscaler
            allocate(uvar(size(u,1),size(u,2),12));allocate(uvar2(size(u,1),size(u,2),12))
            allocate(vvar(size(v,1),size(v,2),12));allocate(vvar2(size(v,1),size(v,2),12))

        ! getting the variance of u and v (variance of scalar magnitude and angle)
            uvar = uvscalersem*cos(atan2(vmean,umean)+thetasem) ! positive sem
            uvar2 = uvscalersem*cos(atan2(vmean,umean)-thetasem) ! negative sem
            vvar = uvscalersem*sin(atan2(vmean,umean)+thetasem) ! positive sem
            vvar2 = uvscalersem*sin(atan2(vmean,umean)-thetasem) ! negative sem

        print*,size(umean,1),size(umean,2),size(umean,3)

        ! obtaining ssh data for tap and mat
        call SSH_data(tap,slabel = '竜飛',calibrate = .true.,diff_from_yearly_mean = .true.,unit = 'cm')
        call SSH_data(mat,ilabel = 4701,calibrate = .true.,diff_from_yearly_mean = .true.,unit = 'cm') ! matsumae
        ! tap - mat
            do i = 1, 15
                do j = 1, 12
                    if(mat(i,j) /= 0. .and. tap(i,j) /= 0.)then 
                        tap_mat(i,j) = tap(i,j) - mat(i,j)
                    else
                        tap_mat(i,j) = 0.
                    end if
                    if(j == 8)print*,tap(i,j),mat(i,j),tap_mat(i,j)
                end do
            end do
            tap_mat(14,8) = 0.
        
        call avsemdata_2D(tap_mat,'dim1',mean_1D = tap_mat_monthly,sem_1D = tap_mat_sem)
        tap_mat_monthlyloop(1:12) = tap_mat_monthly ; tap_mat_monthlyloop(13) = tap_mat_monthly(1)
        tap_mat_semloop(1:12) = tap_mat_sem ; tap_mat_semloop(13) = tap_mat_sem(1)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                    ! PLOTTING
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        call plots2(nnfile = 'ERA5_TAP-MAT',oopt = 'otops',mode = 'land', y = -6.,h = 'ERA5 TAP-MAT')
        call plotsave('first')
        call butler_linegraph(tap_mat_monthlyloop,10.,5.,-6.,6.,mem = .true.,tlabel = 'TAP-MAT',memsymfreq = 2,error_1D = tap_mat_semloop,maskbelow = 0.)
        call plot(0.,-7.,-3)
        call butler_linegraph(reshape(transpose(tap_mat),[15*12]),25.,5.,-8.,8.,mem = .true.,tlabel = 'TAP-MAT',memsymfreq = 2,maskbelow = 0.)
        call mod12_memori(180,25.,symbol_size = 0.4,num_freq = 6,gap = 2)
        

    call plote
end program