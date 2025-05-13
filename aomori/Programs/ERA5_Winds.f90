program era5
    use always

    real,dimension(:,:,:),allocatable::u,v,w,temp ! lon*lat*1020(85years*12months)
    real,dimension(:,:,:,:),allocatable::u2,v2,w2,temp2,theta,uvscaler
    real,dimension(:,:,:),allocatable::umean,vmean,wmean,tempmean,thetamean, thetasem, uvscalersem,uvar,vvar,uvar2,vvar2! monthly means of 2009-2023 data

    width = 5.5

    ! original array has N20-50,E120-150

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
    do i = 1, 85
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

    call avsemdata_4D(u2(:,:,2009-1940+1:2023-1940+1,:),'dim3',mean_3D = umean)
    call avsemdata_4D(v2(:,:,2009-1940+1:2023-1940+1,:),'dim3',mean_3D = vmean)
    call avsemdata_4D(w2(:,:,2009-1940+1:2023-1940+1,:),'dim3',mean_3D = wmean)
    call avsemdata_4D(temp2(:,:,2009-1940+1:2023-1940+1,:),'dim3',mean_3D = tempmean)
    call avsemdata_4D(theta(:,:,2009-1940+1:2023-1940+1,:),'dim3',mean_3D = thetamean,sem_3D = thetasem) ! variance of theta
    call avsemdata_4D(uvscaler(:,:,2009-1940+1:2023-1940+1,:),'dim3',sem_3D = uvscalersem) ! variance of uvscaler
    allocate(uvar(size(u,1),size(u,2),12));allocate(uvar2(size(u,1),size(u,2),12))
    allocate(vvar(size(v,1),size(v,2),12));allocate(vvar2(size(v,1),size(v,2),12))

    
    uvar = uvscalersem*cos(atan2(vmean,umean)+thetasem) ! positive sem
    uvar2 = uvscalersem*cos(atan2(vmean,umean)-thetasem) ! negative sem
    vvar = uvscalersem*sin(atan2(vmean,umean)+thetasem) ! positive sem
    vvar2 = uvscalersem*sin(atan2(vmean,umean)-thetasem) ! negative sem



    print*,size(umean,1),size(umean,2),size(umean,3)


    call plots2(nnfile = 'ERA5_1000hPa_15year_mean',oopt = 'otops',mode = 'land',x = 0.,y = -4.,h = 'ERA5 1000hPa 15 year mean')
    call plotsave('first')
    do i = 1, 12

        call GEBCOmap2(139,142,40,42,width,height = height)
        call symbolc(width/2.,height+0.2,0.7,monthnames(i))
        call griddedbox(width,height,3,13,9)
        call butler_vector(umean(ilon:iflon,ilat:iflat,i),vmean(ilon:iflon,ilat:iflat,i),width,height,arrowwidth = 0.08,scalef = .2,line_thickness = 4,g = 1.)
        ! call butler_vector(uvscalersem(ilon:iflon,ilat:iflat,i)*cos(thetasem(ilon:iflon,ilat:iflat,i)+thetamean(ilon:iflon,ilat:iflat,i)),uvscalersem(ilon:iflon,ilat:iflat,i)*sin(thetasem(ilon:iflon,ilat:iflat,i)+thetamean(ilon:iflon,ilat:iflat,i)),width,height,arrowwidth = 0.04,scalef = .5,line_thickness = 3,r = 1.)
        ! call butler_vector(uvscalersem(ilon:iflon,ilat:iflat,i)*cos(-1.*thetasem(ilon:iflon,ilat:iflat,i)+thetamean(ilon:iflon,ilat:iflat,i)),uvscalersem(ilon:iflon,ilat:iflat,i)*sin(-1.*thetasem(ilon:iflon,ilat:iflat,i)+thetamean(ilon:iflon,ilat:iflat,i)),width,height,arrowwidth = 0.04,scalef = .5,line_thickness = 3,r = 1.)
        call butler_vector(uvar(ilon:iflon,ilat:iflat,i),vvar(ilon:iflon,ilat:iflat,i),width,height,arrowwidth = 0.02,scalef = 1.,line_thickness = 3,r = 1.)
        call butler_vector(uvar2(ilon:iflon,ilat:iflat,i),vvar2(ilon:iflon,ilat:iflat,i),width,height,arrowwidth = 0.02,scalef = 1.,line_thickness = 3,r = 1.)
        if(i == 12)then 
            call rgbk(0.,1.,0.)
            call arrow(width+0.5,0.,width+0.5,1.,0.02) ! 5m/s
            call rgbk(1.,0.,0.)
            call arrow(width+0.8,0.,width+0.8,1.,0.02) ! 1m/s for sem
        end if

        if(i == 4.or.i == 8)then 
            call plotback('first');call plot(0.,i/4*(-height-1.5),-3)
        else 
            call plot(width+1.5,0.,-3)
        end if

    end do

    call plote
end program