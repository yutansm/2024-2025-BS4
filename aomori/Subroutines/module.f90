module constants
    implicit none
    integer, parameter :: years = 15, months = 12, lines = 2, stations = 9, depth = 400
    real,dimension(years,months,lines,stations,depth):: temp_5=0.,potemp_5=0.,sal_5=0.,sigma_5=0.,potemp_c5=0.,sal_c5=0.,sigma_c5=0.,geovel_5=0.
    real,dimension(0:101)::r=0.,g=0.,b=0.,r1=0.,g1=0.,b1=0.,r2=0.,g2=0.,b2=0.,r3=0.,g3=0.,b3=0.,r4=0.,g4=0.,b4=0.,r5=0.,g5=0.,b5=0.,r6=0.,g6=0.,b6=0.
    integer::y,m,l,st,d,i,j,n
    real::dx,dy
end module constants

module psstat
    implicit none
    logical :: stoff, land, pageend
    integer :: ipage
    real :: xorig, yorig
    integer,parameter::ounit = 16
end module psstat

module qbase
    implicit none
    real::qcxp,qcyp
    integer::ip
end module qbase

module subroutines 
    implicit none
    contains
    ! you can choose the gap between the memori and the box. gap == 0 means no gap, gap == 1 means dx, gap == 2 means dx/2.. it's really stupid what i go thru
    subroutine mod12_memori3(x,y,iterations,symbol_size,angle,length,inc_dec,gap,dxval)
        implicit none
        real,intent(in)::symbol_size,length,x,y
        real,intent(out),optional::dxval
        integer,intent(in)::iterations,angle,inc_dec,gap
        real::dx,gappy
        integer::n,m,printm

        call plot(x,y,-3)

        if(gap == 2) then
            dx = length/real(iterations);gappy = dx/2.
            else if(gap == 1) then
                dx = length/real(iterations+1);gappy = dx
            else if(gap == 0) then
                dx = length/real(iterations-1);gappy = 0.
        end if
        if(present(dxval)) then;dxval = dx;else;end if
        if(angle==0) then
            call newpen2(3)
            call plot(0.,0.,3);call plot(length,0.,2)
            do n = 1,iterations
                if (mod(n,12)/=0) then;m = mod(n,12)
                else if(mod(n,12)==0) then;m = 12
                else;end if
                call plot(gappy+real(n-1)*dx,0.,3);call plot(gappy+real(n-1)*dx,-0.1,2)
                if(inc_dec == 1) then;printm = 13-m;else;printm = m;end if
                call numberc(gappy+real(n-1)*dx,-1.2*symbol_size,symbol_size,real(printm),0.,-1)
            end do
        else if(angle == -90) then
            call newpen2(3)
            call plot(0.,0.,3);call plot(0.,length,2)
            do n = 1,iterations
                if (mod(n,12)/=0) then;m = mod(n,12)
                else if(mod(n,12)==0) then;m = 12
                else;end if
                call plot(0.,gappy+real(n-1)*dx,3);call plot(-0.1,gappy+real(n-1)*dx,2)
                if(inc_dec == 1) then;printm = 13-m;else;printm = m;end if
                call numberc(-1.3*symbol_size,gappy+real(n-1)*dx,symbol_size,real(printm),0.,-1)
            end do
        end if

        call plot(-x,-y,-3)
        

    end subroutine

    ! calculates mean, s, and sem per every LOOP of a 1D array make sure LOOP*ITERATIONS < ARRAYSIZE, size of arrays is ITERATIONS
    subroutine avsemloop_1D(array_1D,dim1,loop,iterations,mean_1D,s_1D,sem_1D,dataquan_1D)
        implicit none
        integer,intent(in)::dim1,loop,iterations
        real,intent(in)::array_1D(:)
        real,dimension(iterations),intent(out),optional::mean_1D,s_1D,sem_1D
        integer,dimension(iterations),intent(out),optional::dataquan_1D
        integer::l,i,count=0
        real::smean, s,sem,sum0=0.,sum1=0.

        if(dim1 < loop*iterations) then;print*,'Array size < loop*iterations';stop;end if
        if(size(array_1D)/=dim1) then;print*,'Array size /= dim1';stop;end if
        do i = 1, iterations
            count = 0;sum0=0.;sum1 = 0.
            do l = 1+loop*(i-1),loop*i
                if(array_1D(l)/=0.) then
                    count = count +1;sum0 = sum0 + array_1D(l)
                end if
            end do
            if(count <=1) then
                smean = 0.;s = 0.;sem = 0.
            else
                smean = sum0 / real(count)
                do l = 1+loop*(i-1),loop*i
                    if(array_1D(l)/=0.) then
                        sum1 = sum1 + (array_1D(l) - smean)**2.
                    end if
                end do
                s = sqrt(sum1 / real(count-1))
                sem = s / sqrt(real(count))
            end if
            if(present(dataquan_1D)) dataquan_1D(i) = count
            if(present(mean_1D)) mean_1D(i) = smean
            if(present(s_1D)) s_1D(i) = s
            if(present(sem_1D)) sem_1D(i) = sem
        end do
    end subroutine

    ! avsdsemdataquan better series
        ! gives an array of mean arrays    DO NOT USE INSIDE A LOOP ALLOCATION IS TRICKY
    subroutine avsemdata_2D(array_2D,dim1,dim2,dec_dim,mean_1D,s_1D,sem_1D,dataquan_1D)
        implicit none
        integer,intent(in)::dim1,dim2,dec_dim
        real,intent(in)::array_2D(:,:)
        real,dimension(:),allocatable,intent(out),optional::mean_1D,s_1D,sem_1D
        integer,dimension(:),allocatable,intent(out),optional::dataquan_1D
        integer::n,i,count=0
        real::smean, s, sem, sum0=0.,sum1=0.

        if(size(array_2D,1)<dim1 .or. size(array_2D,2)<dim2) then 
            print*,'Array size < dim1 or dim2';stop
        end if
        if (dec_dim == dim1) then
            if (present(mean_1D)) allocate(mean_1D(dim2))
            if (present(s_1D)) allocate(s_1D(dim2))
            if (present(sem_1D)) allocate(sem_1D(dim2))
            if (present(dataquan_1D)) allocate(dataquan_1D(dim2))
            do n = 1, dim2
                count = 0;sum0=0.;sum1 = 0.
                do i = 1, dim1
                    if (array_2D(i, n) /= 0.0)then;count = count + 1;sum0 = sum0 + array_2D(i,n);end if
                end do
                if (count <=1 ) then
                    smean = 0.;s = 0.;sem = 0.
                else
                    smean = sum0 / real(count)
                    do i = 1, dim1
                        if(array_2D(i,n)/=0.) then
                            sum1 = sum1 + (array_2D(i,n) - smean)**2.
                        end if
                    end do
                    s = sqrt(sum1 / real(count-1))
                    sem = s / sqrt(real(count))
                end if
                    if (present(dataquan_1D)) dataquan_1D(n) = count
                    if (present(mean_1D)) mean_1D(n) = smean
                    if (present(s_1D)) s_1D(n) = s
                    if (present(sem_1D)) sem_1D(n) = sem
            end do
        else if (dec_dim == dim2) then
            if (present(mean_1D)) allocate(mean_1D(dim1))
            if (present(s_1D)) allocate(s_1D(dim1))
            if (present(sem_1D)) allocate(sem_1D(dim1))
            if (present(dataquan_1D)) allocate(dataquan_1D(dim1))
            do n = 1, dim1
                count = 0;sum0=0.;sum1 = 0.
                do i = 1, dim2
                    if (array_2D(n, i) /= 0.0)then;count = count + 1;sum0 = sum0 + array_2D(n,i);end if
                    ! print*,n,i,count,sum0
                end do
                if (count <=1 ) then
                    smean = 0.;s = 0.;sem = 0.
                else
                    smean = sum0 / real(count)
                    do i = 1, dim2
                        if(array_2D(n,i)/=0.) then
                            sum1 = sum1 + (array_2D(n,i) - smean)**2.
                        end if
                    end do
                    s = sqrt(sum1/ real(count-1))
                    sem = s / sqrt(real(count))
                end if
                    if (present(dataquan_1D)) dataquan_1D(n) = count
                    if (present(mean_1D)) mean_1D(n) = smean
                    if (present(s_1D)) s_1D(n) = s
                    if (present(sem_1D)) sem_1D(n) = sem
            end do
        else
            print *, 'Choose a dimension to observe (keep)'
            stop
        end if


        ! if(present(mean_1D)) deallocate(mean_1D)
        ! if(present(s_1D)) deallocate(s_1D)
        ! if(present(sem_1D)) deallocate(sem_1D)
        ! if(present(dataquan_1D)) deallocate(dataquan_1D)

    end subroutine

    subroutine avsemdata_3D(array_3D,dim1,dim2,dim3,dec_dim,mean_2D,s_2D,sem_2D,dataquan_2D)
        implicit none
        integer,intent(in)::dim1,dim2,dim3,dec_dim
        real,intent(in)::array_3D(:,:,:)
        real,dimension(:,:),allocatable,intent(out),optional::mean_2D,s_2D,sem_2D
        integer,dimension(:,:),allocatable,intent(out),optional::dataquan_2D
        integer::l1,l2,l3,count=0,loop1,loop2,loop3
        real::smean, s, sem, sum0=0.,sum1=0.

        if(size(array_3D,1)<dim1 .or. size(array_3D,2)<dim2 .or. size(array_3D,3)<dim3) then 
            print*,'Array size < dim1 or dim2 or dim3';stop
        end if
        ! loop determination
            if (dec_dim == dim1) then
                loop1 = dim2
                loop2 = dim3
                loop3 = dim1
                if(present(mean_2D)) allocate(mean_2D(dim2,dim3))
                if(present(s_2D)) allocate(s_2D(dim2,dim3))
                if(present(sem_2D)) allocate(sem_2D(dim2,dim3))
                if(present(dataquan_2D)) allocate(dataquan_2D(dim2,dim3))
            elseif (dec_dim == dim2) then
                loop1 = dim1
                loop2 = dim3
                loop3 = dim2
                if(present(mean_2D)) allocate(mean_2D(dim1,dim3))
                if(present(s_2D)) allocate(s_2D(dim1,dim3))
                if(present(sem_2D)) allocate(sem_2D(dim1,dim3))
                if(present(dataquan_2D)) allocate(dataquan_2D(dim1,dim3))
            elseif (dec_dim == dim3) then
                loop1 = dim1
                loop2 = dim2
                loop3 = dim3
                if(present(mean_2D)) allocate(mean_2D(dim1,dim2))
                if(present(s_2D)) allocate(s_2D(dim1,dim2))
                if(present(sem_2D)) allocate(sem_2D(dim1,dim2))
                if(present(dataquan_2D)) allocate(dataquan_2D(dim1,dim2))
            else
                print*, 'Invalid dec_dim value'
                stop
            end if
        ! loop determination end
        do l1 = 1, loop1
            do l2 = 1, loop2
                count = 0; sum0 = 0.;sum1 = 0.
                do l3 = 1, loop3 !dec_dim loop
                    if(dec_dim==dim1) then
                        if(array_3D(l3,l1,l2)/=0.) then
                            count = count + 1
                            sum0 = sum0 + array_3D(l3,l1,l2)
                        end if
                    else if(dec_dim==dim2) then
                        if(array_3D(l1,l3,l2)/=0.) then
                            count = count + 1
                            sum0 = sum0 + array_3D(l1,l3,l2)
                        end if
                    else if(dec_dim==dim3) then
                        if(array_3D(l1,l2,l3)/=0.) then
                            count = count + 1
                            sum0 = sum0 + array_3D(l1,l2,l3)
                        end if
                    end if
                end do
                    if(count<=1) then;smean= 0.;s=0.;sem=0.
                    else;smean = sum0/real(count)
                        do l3 = 1, loop3
                            if(dec_dim==dim1) then
                                if(array_3D(l3,l1,l2)/=0.) then
                                    sum1 = sum1 + (array_3D(l3,l1,l2) - smean)**2.
                                end if
                            else if(dec_dim==dim2) then
                                if(array_3D(l1,l3,l2)/=0.) then
                                    sum1 = sum1 + (array_3D(l1,l3,l2) - smean)**2.
                                end if
                            else if(dec_dim==dim3) then
                                if(array_3D(l1,l2,l3)/=0.) then
                                    sum1 = sum1 + (array_3D(l1,l2,l3) - smean)**2.
                                end if
                            end if
                        end do
                        s = sqrt(sum1/real(count-1))
                        sem = s / sqrt(real(count))
                    end if
                    if(present(mean_2D)) mean_2D(l1,l2) = smean
                    if(present(s_2D)) s_2D(l1,l2) = s
                    if(present(sem_2D)) sem_2D(l1,l2) = sem
                    if(present(dataquan_2D)) dataquan_2D(l1,l2) = count
            end do
        end do
        ! end do



    end subroutine

    subroutine avsemdata_4D(array_4D,dim1,dim2,dim3,dim4,dec_dim,mean_3D,s_3D,sem_3D,dataquan_3D)
        implicit none
        integer,intent(in)::dim1,dim2,dim3,dim4,dec_dim
        real,intent(in)::array_4D(:,:,:,:)
        real,dimension(:,:,:),allocatable,intent(out),optional::mean_3D,s_3D,sem_3D
        integer,dimension(:,:,:),allocatable,intent(out),optional::dataquan_3D
        integer::l1,l2,l3,l4,count=0,loop1,loop2,loop3,loop4
        real::smean, s, sem, sum0=0.,sum1=0.

        if(size(array_4D,1)<dim1 .or. size(array_4D,2)<dim2 .or. size(array_4D,3)<dim3 .or. size(array_4D,4)<dim4) then 
            print*,'Array size < dim1 or dim2 or dim3 or dim4';stop
        end if
        ! loop determination
            if (dec_dim == dim1) then
                loop1 = dim2
                loop2 = dim3
                loop3 = dim4
                loop4 = dim1
                if(present(mean_3D)) allocate(mean_3D(dim2,dim3,dim4))
                if(present(s_3D)) allocate(s_3D(dim2,dim3,dim4))
                if(present(sem_3D)) allocate(sem_3D(dim2,dim3,dim4))
                if(present(dataquan_3D)) allocate(dataquan_3D(dim2,dim3,dim4))
            elseif (dec_dim == dim2) then
                loop1 = dim1
                loop2 = dim3
                loop3 = dim4
                loop4 = dim2
                if(present(mean_3D)) allocate(mean_3D(dim1,dim3,dim4))
                if(present(s_3D)) allocate(s_3D(dim1,dim3,dim4))
                if(present(sem_3D)) allocate(sem_3D(dim1,dim3,dim4))
                if(present(dataquan_3D)) allocate(dataquan_3D(dim1,dim3,dim4))
            elseif (dec_dim == dim3) then
                loop1 = dim1
                loop2 = dim2
                loop3 = dim4
                loop4 = dim3
                if(present(mean_3D)) allocate(mean_3D(dim1,dim2,dim4))
                if(present(s_3D)) allocate(s_3D(dim1,dim2,dim4))
                if(present(sem_3D)) allocate(sem_3D(dim1,dim2,dim4))
                if(present(dataquan_3D)) allocate(dataquan_3D(dim1,dim2,dim4))
            elseif (dec_dim == dim4) then
                loop1 = dim1
                loop2 = dim2
                loop3 = dim3
                loop4 = dim4
                if(present(mean_3D)) allocate(mean_3D(dim1,dim2,dim3))
                if(present(s_3D)) allocate(s_3D(dim1,dim2,dim3))
                if(present(sem_3D)) allocate(sem_3D(dim1,dim2,dim3))
                if(present(dataquan_3D)) allocate(dataquan_3D(dim1,dim2,dim3))
            else;print*, 'Invalid dec_dim value';stop
            end if
        ! loop determination end
        do l1 = 1, loop1
            do l2 = 1, loop2
                do l3 = 1, loop3
                    count = 0; sum0 = 0.;sum1 = 0.
                    do l4 = 1, loop4 !dec_dim loop
                        if(dec_dim==dim1) then
                            if(array_4D(l4,l1,l2,l3)/=0.) then
                                count = count + 1
                                sum0 = sum0 + array_4D(l4,l1,l2,l3)
                            end if
                        else if(dec_dim==dim2) then
                            if(array_4D(l1,l4,l2,l3)/=0.) then
                                count = count + 1
                                sum0 = sum0 + array_4D(l1,l4,l2,l3)
                            end if
                        else if(dec_dim==dim3) then
                            if(array_4D(l1,l2,l4,l3)/=0.) then
                                count = count + 1
                                sum0 = sum0 + array_4D(l1,l2,l4,l3)
                            end if
                        else if(dec_dim==dim4) then
                            if(array_4D(l1,l2,l3,l4)/=0.) then
                                count = count + 1
                                sum0 = sum0 + array_4D(l1,l2,l3,l4)
                            end if
                        end if
                    end do
                        if(count<=1) then;smean= 0.;s=0.;sem=0.
                        else;smean = sum0/real(count)
                            do l4 = 1, loop4
                                if(dec_dim==dim1) then
                                    if(array_4D(l4,l1,l2,l3)/=0.) then
                                        sum1 = sum1 + (array_4D(l4,l1,l2,l3) - smean)**2.
                                    end if
                                else if(dec_dim==dim2) then
                                    if(array_4D(l1,l4,l2,l3)/=0.) then
                                        sum1 = sum1 + (array_4D(l1,l4,l2,l3) - smean)**2.
                                    end if
                                else if(dec_dim==dim3) then
                                    if(array_4D(l1,l2,l4,l3)/=0.) then
                                        sum1 = sum1 + (array_4D(l1,l2,l4,l3) - smean)**2.
                                    end if
                                else if(dec_dim==dim4) then
                                    if(array_4D(l1,l2,l3,l4)/=0.) then
                                        sum1 = sum1 + (array_4D(l1,l2,l3,l4) - smean)**2.
                                    end if
                                end if
                            end do
                            s = sqrt(sum1/real(count-1))
                            sem = s / sqrt(real(count))
                        end if  
                        if(present(mean_3D)) mean_3D(l1,l2,l3) = smean
                        if(present(s_3D)) s_3D(l1,l2,l3) = s
                        if(present(sem_3D)) sem_3D(l1,l2,l3) = sem
                        if(present(dataquan_3D)) dataquan_3D(l1,l2,l3) = count                   
                end do
            end do
        end do

    end subroutine

    subroutine avsemdata_5D(array_5D,dim1,dim2,dim3,dim4,dim5,dec_dim,mean_4D,s_4D,sem_4D,dataquan_4D)
        implicit none
        integer,intent(in)::dim1,dim2,dim3,dim4,dim5,dec_dim
        real,intent(in)::array_5D(:,:,:,:,:)
        real,dimension(:,:,:,:),allocatable,intent(out),optional::mean_4D,s_4D,sem_4D
        integer,dimension(:,:,:,:),allocatable,intent(out),optional::dataquan_4D
        integer::l1,l2,l3,l4,l5,count=0,loop1,loop2,loop3,loop4,loop5
        real::smean, s, sem, sum0=0.,sum1=0.

        if(size(array_5D,1)<dim1 .or. size(array_5D,2)<dim2 .or. size(array_5D,3)<dim3 .or. size(array_5D,4)<dim4 .or. size(array_5D,5)<dim5) then 
            print*,'Array size < dim1 or dim2 or dim3 or dim4 or dim5';stop
        end if
        ! loop determination
            if (dec_dim == dim1) then
                loop1 = dim2
                loop2 = dim3
                loop3 = dim4
                loop4 = dim5
                loop5 = dim1
                if(present(mean_4D)) allocate(mean_4D(dim2,dim3,dim4,dim5))
                if(present(s_4D)) allocate(s_4D(dim2,dim3,dim4,dim5))
                if(present(sem_4D)) allocate(sem_4D(dim2,dim3,dim4,dim5))
                if(present(dataquan_4D)) allocate(dataquan_4D(dim2,dim3,dim4,dim5))
            elseif (dec_dim == dim2) then
                loop1 = dim1
                loop2 = dim3
                loop3 = dim4
                loop4 = dim5
                loop5 = dim2
                if(present(mean_4D)) allocate(mean_4D(dim1,dim3,dim4,dim5))
                if(present(s_4D)) allocate(s_4D(dim1,dim3,dim4,dim5))
                if(present(sem_4D)) allocate(sem_4D(dim1,dim3,dim4,dim5))
                if(present(dataquan_4D)) allocate(dataquan_4D(dim1,dim3,dim4,dim5))
            elseif (dec_dim == dim3) then
                loop1 = dim1
                loop2 = dim2
                loop3 = dim4
                loop4 = dim5
                loop5 = dim3
                if(present(mean_4D)) allocate(mean_4D(dim1,dim2,dim4,dim5))
                if(present(s_4D)) allocate(s_4D(dim1,dim2,dim4,dim5))
                if(present(sem_4D)) allocate(sem_4D(dim1,dim2,dim4,dim5))
                if(present(dataquan_4D)) allocate(dataquan_4D(dim1,dim2,dim4,dim5))
            elseif (dec_dim == dim4) then
                loop1 = dim1
                loop2 = dim2
                loop3 = dim3
                loop4 = dim5
                loop5 = dim4
                if(present(mean_4D)) allocate(mean_4D(dim1,dim2,dim3,dim5))
                if(present(s_4D)) allocate(s_4D(dim1,dim2,dim3,dim5))
                if(present(sem_4D)) allocate(sem_4D(dim1,dim2,dim3,dim5))
                if(present(dataquan_4D)) allocate(dataquan_4D(dim1,dim2,dim3,dim5))
            elseif (dec_dim == dim5) then
                loop1 = dim1
                loop2 = dim2
                loop3 = dim3
                loop4 = dim4
                loop5 = dim5
                if(present(mean_4D)) allocate(mean_4D(dim1,dim2,dim3,dim4))
                if(present(s_4D)) allocate(s_4D(dim1,dim2,dim3,dim4))
                if(present(sem_4D)) allocate(sem_4D(dim1,dim2,dim3,dim4))
                if(present(dataquan_4D)) allocate(dataquan_4D(dim1,dim2,dim3,dim4))
            else;print*, 'Invalid dec_dim value';stop
            end if

        ! loop determination end
        do l1 = 1, loop1
            do l2 = 1, loop2
                do l3 = 1, loop3
                    do l4 = 1, loop4
                        count = 0; sum0 = 0.;sum1 = 0.
                        do l5 = 1, loop5 !dec_dim loop
                            if(dec_dim==dim1) then
                                if(array_5D(l5,l1,l2,l3,l4)/=0.) then
                                    count = count + 1
                                    sum0 = sum0 + array_5D(l5,l1,l2,l3,l4)
                                end if
                            else if(dec_dim==dim2) then
                                if(array_5D(l1,l5,l2,l3,l4)/=0.) then
                                    count = count + 1
                                    sum0 = sum0 + array_5D(l1,l5,l2,l3,l4)
                                end if
                            else if(dec_dim==dim3) then
                                if(array_5D(l1,l2,l5,l3,l4)/=0.) then
                                    count = count + 1
                                    sum0 = sum0 + array_5D(l1,l2,l5,l3,l4)
                                end if
                            else if(dec_dim==dim4) then
                                if(array_5D(l1,l2,l3,l5,l4)/=0.) then
                                    count = count + 1
                                    sum0 = sum0 + array_5D(l1,l2,l3,l5,l4)
                                end if
                            else if(dec_dim==dim5) then
                                if(array_5D(l1,l2,l3,l4,l5)/=0.) then
                                    count = count + 1
                                    sum0 = sum0 + array_5D(l1,l2,l3,l4,l5)
                                end if
                            end if
                        end do
                            if(count<=1) then;smean= 0.;s=0.;sem=0.
                            else;smean = sum0/real(count)
                                do l5 = 1, loop5
                                    if(dec_dim==dim1) then
                                        if(array_5D(l5,l1,l2,l3,l4)/=0.) then
                                            sum1 = sum1 + (array_5D(l5,l1,l2,l3,l4) - smean)**2.
                                        end if
                                    else if(dec_dim==dim2) then
                                        if(array_5D(l1,l5,l2,l3,l4)/=0.) then
                                            sum1 = sum1 + (array_5D(l1,l5,l2,l3,l4) - smean)**2.
                                        end if
                                    else if(dec_dim==dim3) then
                                        if(array_5D(l1,l2,l5,l3,l4)/=0.) then
                                            sum1 = sum1 + (array_5D(l1,l2,l5,l3,l4) - smean)**2.
                                        end if
                                    else if(dec_dim==dim4) then
                                        if(array_5D(l1,l2,l3,l5,l4)/=0.) then
                                            sum1 = sum1 + (array_5D(l1,l2,l3,l5,l4) - smean)**2.
                                        end if
                                    else if(dec_dim==dim5) then
                                        if(array_5D(l1,l2,l3,l4,l5)/=0.) then
                                            sum1 = sum1 + (array_5D(l1,l2,l3,l4,l5) - smean)**2.
                                        end if
                                    end if
                                end do
                                s = sqrt(sum1/real(count-1))
                                sem = s / sqrt(real(count))
                            end if
                            if(present(mean_4D)) mean_4D(l1,l2,l3,l4) = smean
                            if(present(s_4D)) s_4D(l1,l2,l3,l4) = s
                            if(present(sem_4D)) sem_4D(l1,l2,l3,l4) = sem
                            if(present(dataquan_4D)) dataquan_4D(l1,l2,l3,l4) = count
                    end do 
                end do            
            end do
        end do
    end subroutine

    ! ps boys is are good bois
    subroutine butler_psk(array_2D,dim1,dim2,width,height,maskval,ival,fval,inc,colorscheme,iterations,bpt1,bpt2,bpt3,contquan,conti,continc,r1,g1,b1)
        implicit none
        integer,intent(in)::dim1,dim2,iterations,bpt1
        real,intent(in)::maskval,ival,fval,inc,width,height
        integer,intent(in),optional::bpt2,bpt3,contquan
        real,intent(in),optional::conti,continc
        real,intent(in)::array_2D(:,:)
        integer,dimension(size(array_2D,1),size(array_2D,2))::mask
        character(len=*),intent(in)::colorscheme
        real,dimension(0:iterations+1)::r,g,b
        real,dimension(:),allocatable,intent(out),optional::r1,g1,b1
        real::dx,dy
        integer::i,j,n

        if(size(array_2D,1)<dim1 .or. size(array_2D,2)<dim2) then 
            print*,'Array size < dim1 or dim2';stop
        end if
        if((ival+inc*real(iterations))/=fval)then;print*,'i=',ival,'inc=',inc,'iter=',iterations,'f=',fval,'i+inc*iter/=f';stop;end if
        dx = width/real(dim1-1);dy = height/real(dim2-1)
        call plot(-dx/2.,-dy/2.,-3)
        select case(colorscheme)
        case('b2r');call b2r_colorgrad(iterations,bpt1,r,g,b)
        case('b2gy2r');call b2gy2r_colorgrad(iterations,bpt1,r,g,b)
        case('r2g');call r2g_colorgrad(iterations,bpt1,r,g,b)
        case('bk2r2g');call bk2r2g_colorgrad(iterations,bpt1,r,g,b)
        case('b2cy2y2r');if(present(bpt2).and.present(bpt3)) then;call b2cy2y2r_colorgrad(iterations,bpt1,bpt2,bpt3,r,g,b);else;print*,'bpt2 and bpt3 are required';stop;end if
        case('b2g2y2r');if(present(bpt2).and.present(bpt3)) then;call b2g2y2r_colorgrad(iterations,bpt1,bpt2,bpt3,r,g,b);else;print*,'bpt2 and bpt3 are required';stop;end if
        case default;print*,'Invalid colorscheme';stop
        end select

        if(present(r1).and.present(g1).and.present(b1)) then
            allocate(r1(0:iterations+1),g1(0:iterations+1),b1(0:iterations+1));r1 = r;g1 = g;b1 = b
        ! else;print*,'color arrays are not allocated'
        end if

        do i = 1, dim1
            do j = 1, dim2
                if(array_2D(i,j)==maskval) then;mask(i,j)=0
                else;mask(i,j)=1
                end if
            end do
        end do

        call pscolork(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,ival-100.,ival,r(0),g(0),b(0))
        call pscolork(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,fval,fval+100.,r(iterations+1),g(iterations+1),b(iterations+1))
        do n = 1, iterations
            call pscolork(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,ival+real(n-1)*inc,ival+real(n)*inc,r(n),g(n),b(n))
        end do

        if(present(conti).and.present(continc).and.present(contquan)) then
            call pscont3(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,contquan,conti,continc)
        else;print*,'no contour'
        end if
        



        call plot(dx/2.,dy/2.,-3)

    end subroutine

    subroutine butler_psbet(array_2D,dim1,dim2,width,height,maskval,ival,fval,inc,colorscheme,iterations,bpt1,bpt2,bpt3,contquan,conti,continc,r1,g1,b1)
        implicit none
        integer,intent(in)::dim1,dim2,iterations,bpt1
        real,intent(in)::maskval,ival,fval,inc,width,height
        integer,intent(in),optional::bpt2,bpt3,contquan
        real,intent(in),optional::conti,continc
        real,intent(in)::array_2D(:,:)
        integer,dimension(size(array_2D,1),size(array_2D,2))::mask
        character(len=*),intent(in)::colorscheme
        real,dimension(0:iterations+1)::r,g,b
        real,dimension(:),allocatable,intent(out),optional::r1,g1,b1
        real::dx,dy
        integer::i,j,n

        if(size(array_2D,1)<dim1 .or. size(array_2D,2)<dim2) then 
            print*,'Array size < dim1 or dim2';stop
        end if
        if((ival+inc*real(iterations))/=fval)then;print*,'i=',ival,'inc=',inc,'iter=',iterations,'f=',fval,'i+inc*iter/=f';stop;end if
        dx = width/real(dim1);dy = height/real(dim2)
        select case(colorscheme)
        case('b2r');call b2r_colorgrad(iterations,bpt1,r,g,b)
        case('b2gy2r');call b2gy2r_colorgrad(iterations,bpt1,r,g,b)
        case('r2g');call r2g_colorgrad(iterations,bpt1,r,g,b)
        case('bk2r2g');call bk2r2g_colorgrad(iterations,bpt1,r,g,b)
        case('b2cy2y2r');if(present(bpt2).and.present(bpt3)) then;call b2cy2y2r_colorgrad(iterations,bpt1,bpt2,bpt3,r,g,b);else;print*,'bpt2 and bpt3 are required';stop;end if
        case('b2g2y2r');if(present(bpt2).and.present(bpt3)) then;call b2g2y2r_colorgrad(iterations,bpt1,bpt2,bpt3,r,g,b);else;print*,'bpt2 and bpt3 are required';stop;end if
        case default;print*,'Invalid colorscheme';stop
        end select

        if(present(r1).and.present(g1).and.present(b1)) then
            allocate(r1(0:iterations+1),g1(0:iterations+1),b1(0:iterations+1));r1 = r;g1 = g;b1 = b
        ! else;print*,'color arrays are not allocated'
        end if

        do i = 1, dim1
            do j = 1, dim2
                if(array_2D(i,j)==maskval) then;mask(i,j)=0
                else;mask(i,j)=1
                end if
            end do
        end do

        call betcolork2(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,ival-100.,ival,r(0),g(0),b(0))
        call betcolork2(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,fval,fval+100.,r(iterations+1),g(iterations+1),b(iterations+1))
        do n = 1, iterations
            call betcolork2(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,ival+real(n-1)*inc,ival+real(n)*inc,r(n),g(n),b(n))
        end do

        if(present(conti).and.present(continc).and.present(contquan)) then
            call pscont3(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,contquan,conti,continc)
        else;print*,'no contour'
        end if
        
    end subroutine

end module subroutines