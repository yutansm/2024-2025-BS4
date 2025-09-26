program era5
    use always

    real,dimension(:,:,:),allocatable::u,v,w,temp ! lon*lat*1020(85years*12months)
    real,dimension(:,:,:,:),allocatable::u2,v2,w2,temp2,theta,uvscaler
    real,dimension(:,:,:),allocatable::umean,vmean,wmean,tempmean,thetamean, thetasem, uvscalersem,uvar,vvar,uvar2,vvar2! monthly means of 2009-2023 data
    real,dimension(:,:,:),allocatable::umeanxprime,vmeanyprime,zero
    real,dimension(:),allocatable::umeanxprimelocal,vmeanyprimelocal,winddrivenU,delta_eta ! monthly means of 2009-2023 data projected onto x' and y' axis
    real,dimension(13)::tap_mat_mean,tap_mat_sem
    width = 5.5;width2 = 8.;height2 = 5.

    ! original array has N20-50,E120-150

    ilon = (139-120)*4+1
    iflon = (142-120)*4+1
    ilat = (40-20)*4+1
    iflat = (42-20)*4+1
    call ERA5_1000hPa(temp,u,v,w,info = .true.)
    print*,size(u,1),size(u,2),size(u,3)
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

    call avsemdata_4D(u2(:,:,1989-1940+1:2023-1940+1,:),'dim3',mean_3D = umean)
    call avsemdata_4D(v2(:,:,1989-1940+1:2023-1940+1,:),'dim3',mean_3D = vmean)
    call avsemdata_4D(w2(:,:,1989-1940+1:2023-1940+1,:),'dim3',mean_3D = wmean)
    allocate(umeanxprime(size(umean,1),size(umean,2),12))
    allocate(vmeanyprime(size(vmean,1),size(vmean,2),12))
    allocate(zero(size(umean,1),size(umean,2),12)) ! zero array for plotting
    zero = 0.
    umeanxprime = umean*cos(atan2(3.,5.)) + vmean*sin(atan2(3.,5.)) ! umean projected onto the direction of x prime y prime axis
    vmeanyprime = -umean*sin(atan2(3.,5.)) + vmean*cos(atan2(3.,5.)) ! vmean projected onto the direction of x prime y prime axis


    ! call avsemdata_4D(temp2(:,:,1989-1940+1:2023-1940+1,:),'dim3',mean_3D = tempmean)
    ! call avsemdata_4D(theta(:,:,1989-1940+1:2023-1940+1,:),'dim3',mean_3D = thetamean,sem_3D = thetasem) ! variance of theta
    ! call avsemdata_4D(uvscaler(:,:,1989-1940+1:2023-1940+1,:),'dim3',sem_3D = uvscalersem) ! variance of uvscaler
    ! allocate(uvar(size(u,1),size(u,2),12));allocate(uvar2(size(u,1),size(u,2),12))
    ! allocate(vvar(size(v,1),size(v,2),12));allocate(vvar2(size(v,1),size(v,2),12))
    
    ! uvar = uvscalersem*cos(atan2(vmean,umean)+thetasem) ! positive sem
    ! uvar2 = uvscalersem*cos(atan2(vmean,umean)-thetasem) ! negative sem
    ! vvar = uvscalersem*sin(atan2(vmean,umean)+thetasem) ! positive sem
    ! vvar2 = uvscalersem*sin(atan2(vmean,umean)-thetasem) ! negative sem



    print*,size(umean,1),size(umean,2),size(umean,3)


    call plots2(nnfile = 'Wind_driven_current_estimation2',oopt = 'otops',mode = 'land',x = 0.,y = -4.5,h = 'ERA5 1000hPa 35 year mean')
    call plotsave('first')
    ! monthly
    do i = 1, 12

        call GEBCOmap2(139,142,40,42,width,height = height,symbol_freqx = 1,symbol_freqy = 1,paintland = .true.)
        call symbolc(width/2.,height+0.2,0.7,monthnames(i))
        call griddedbox(width,height,3,13,9)
        call butler_vector(umean(ilon:iflon,ilat:iflat,i),vmean(ilon:iflon,ilat:iflat,i),width,height,arrowwidth = 0.08,scalef = .15,line_thickness = 5,r = 1.)

        ! call butler_vector(umeanxprime(ilon:iflon,ilat:iflat,i),zero(ilon:iflon,ilat:iflat,i),width,height,arrowwidth = 0.08,scalef = .15,line_thickness = 3,r = 1.,rangle = atan2(3.,5.)* 180./pi) ! umean projected onto the direction of x prime y prime axis
        ! call butler_vector(zero(ilon:iflon,ilat:iflat,i),vmeanyprime(ilon:iflon,ilat:iflat,i),width,height,arrowwidth = 0.08,scalef = .15,line_thickness = 3,b = 1.,rangle = atan2(3.,5.)* 180./pi) ! vmean projected onto the direction of x prime y prime axis

        ! call butler_vector(uvar(ilon:iflon,ilat:iflat,i),vvar(ilon:iflon,ilat:iflat,i),width,height,arrowwidth = 0.02,scalef = 1.,line_thickness = 3,r = 1.)
        ! call butler_vector(uvar2(ilon:iflon,ilat:iflat,i),vvar2(ilon:iflon,ilat:iflat,i),width,height,arrowwidth = 0.02,scalef = 1.,line_thickness = 3,r = 1.)
        if(i == 12)then 
            call rgbk(1.,0.,0.)
            call arrow(width+1.,0.,width+1.,1.5,0.05,6) ! 10m/s -> 1cm in the plotting = 10m/s
            ! call rgbk(1.,0.,0.)
            ! call arrow(width+0.8,0.,width+0.8,1.,0.02) ! 1m/s for sem
        end if

        if(i == 4.or.i == 8)then 
            call plotback('first');call plot(0.,i/4*(-height-1.5),-3)
        else 
            call plot(width+1.5,0.,-3)
        end if

    end do

    
    call newpage('first')
        
        call GEBCOmap2(139,142,40,42,width,height = height,symbol_freqx = 1,symbol_freqy = 1)
        call griddedbox(width,height,3,13,9)
        dx = width/13.;dy = height/9.
        do i = 0,4
            do j = 0,2
                call box(dx,dy,4,(4+i)*dx,(j+5)*dy,r = 1.)
            end do
        end do
        call newpen2(4)
        ! call plot(6.5*dx,6.5*dy,3);call plot(6.5*dx-5./1.5,6.5*dy-3./1.5,2);call plot(6.5*dx+5./1.5,6.5*dy+3./1.5,2)
        call arrow(6.5*dx-5./1.5,6.5*dy-3./1.5,6.5*dx+5./1.5,6.5*dy+3./1.5,0.3,6,arrowratio = 0.1)
        call arrow(6.5*dx+3./1.5,6.5*dy-5./1.5,6.5*dx-3./1.5,6.5*dy+5./1.5,0.3,6,arrowratio = 0.1)
        call gmark(6.5*dx,6.5*dy,0.2,1)

        !taking regional mean of umeanxprime and vmeanyprime
        allocate(umeanxprimelocal(13))
        allocate(vmeanyprimelocal(13))
        do i = 1,12
            umeanxprimelocal(i) = sum(umeanxprime(ilon+5:ilon+9,ilat+6:ilat+8,i))/15.
            vmeanyprimelocal(i) = sum(vmeanyprime(ilon+5:ilon+9,ilat+6:ilat+8,i))/15.! local regional mean
        end do
        umeanxprimelocal(13) = umeanxprimelocal(1)
        vmeanyprimelocal(13) = vmeanyprimelocal(1)

        call plot(width+3.5,1.,-3)
        call plotsave('second')
        call butler_linegraph(umeanxprimelocal,width2,height2,-4.,4.,0.,.true.,maskbelow = 0.,memlabel = '[m/s]')
        call mod12_memori(13,width2,gap = 2)

        call plot(width2+2.,0.,-3)

        call butler_linegraph(vmeanyprimelocal,width2,height2,-4.,4.,0.,.true.,maskbelow = 0.) ! units are m/s
        call mod12_memori(13,width2,gap = 2)

        call plotback('second')
        call plot(0.,-height2-2.,-3)
        allocate(winddrivenU(13),delta_eta(13))
        winddrivenU = umeanxprimelocal*0.05 ! 5 percent of wind speed, unit is m/s
        call butler_linegraph(winddrivenU*100.,width2,height2,-20.,20.,0.,.true.,maskbelow = 0.,memlabel = '[cm/s]',memsymfreq = 10) ! units are cm/s
        call mod12_memori(13,width2,gap = 2)

        call plot(0.,-height2-1.,-3)

        delta_y = 30.*10.**3 ! 30km in meters [m]
        f = 2.*7.2921*10.**(-5.)*sin(41.*pi/180.) ! coriolis parameter at 41 [/s]
        print*,f ! -> about 1.4e-4
        g = 9.81 ! gravity [m/s^2]
        delta_eta = -f/g*delta_y*winddrivenU ! wind driven current, unit is m/s, negative because the current is in the opposite direction of the wind
        print*,f*delta_y/g ! -> about .3

        ! call butler_linegraph(delta_eta*100.,width2,height2,-6.,6.,0.,.true.,maskbelow = 0.,memlabel = '[cm]',memsymfreq = 2) ! units are cm

        ! call plot(width2+2.,0.,-3)

        call butler_linegraph(-delta_eta*100.,width2,height2,-6.,6.,0.,.true.,maskbelow = 0.,memsymfreq = 2) ! units are cm
        call mod12_memori(13,width2,gap = 2)

        call plot(width2+2.,0.,-3)
        
        open(123,file = '../Data/TAP_MAT_mean.bin',form = 'unformatted',status = 'old',access = 'direct',recl = 13*4,convert = 'big_endian')
        read(123,rec = 1) tap_mat_mean
        read(123,rec = 2) tap_mat_sem
        close(123)

        call butler_linegraph(tap_mat_mean+delta_eta*100.,width2,height2,-10.,10.,0.,.true.,maskbelow = 0.,memsymfreq = 5,tlabel = 'TAP-MAT wo winds') ! units are cm
        call mod12_memori(13,width2,gap = 2)

    call plote
end program