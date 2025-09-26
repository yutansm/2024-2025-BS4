! filepath: /Users/yuta/LABWORK/2024-2025-BS4/aomori/Programs/read_amedas.f90
program read_amedas
    use always
    implicit none
    character(len=1000) :: line
    integer :: ios, dummy, dummy2, year, month, icount
    real, dimension(180) :: velo
    character(len=3), dimension(180) :: direction
    real::velo2(15,12),mvelo2(13),semvelo2(13),dx,array(13)
    real,allocatable::mean(:),sem(:)
    
    ! Open the file for Imabetsu
        open(20, file='../Data/AMEDAS/Imabetsu_winds_utf8.csv', &
            status='old', form='formatted', iostat=ios)
        
        if(ios /= 0) then
            print*, 'Error opening file'
            stop
        end if
        
        ! Skip header lines (first 3 lines)
        do i = 1, 4
            read(20, '(A)', iostat=ios) line
            print*, trim(line)  ! Debug output to see header lines
            if(ios /= 0) exit
        end do
        
        ! Initialize counter
        icount = 0
        
        ! Read data rows
        do while(ios == 0 .and. icount < 180)
            read(20, *, iostat=ios) year, month, velo(icount+1), dummy, direction(icount+1), dummy2
            
            if(ios == 0) then
                icount = icount + 1
                ! Debug output to see what's being read
                print*, year, month, velo(icount), direction(icount)
            end if
        end do
        
        close(20)
        
        print*, "Successfully read", icount, "records"
    
    ! Now use the data as needed
    velo2 = transpose(reshape(velo, [12, 15]))
    do i = 1, 15
        do j = 1, 12
            print*, "Velo(", i, ",", j, ") = ", velo2(i, j)
        end do
    end do

    call avsemdata_2D(velo2,'dim1',mean_1D = mean,sem_1D = sem)
    mvelo2(:12) = mean;mvelo2(13) = mvelo2(1)
    semvelo2(:12) = sem;semvelo2(13) = semvelo2(1)
    call plots2(nnfile = 'amedas_winds_Imabetsu',oopt = 'otops',x = 0.,y = -5.5,h = 'AMEDAS Wind')

    dx = 27./180
    call butler_linegraph(velo,27.,5.,0.,3.,0.,.true.,tlabel = 'Imabetsu Winds') ! timeseries
    call mod12_memori(180,27.,0.3,num_freq = 6,gap = 2)
    do i = 1, 180
        call symbolc(dx/2.+(i-1)*dx,-1.,0.2,direction(i),90.)
    end do

    call plot(0.,-9.,-3)
    call butler_linegraph(mvelo2,10.,5.,0.,3.,0.,.true.,tlabel = 'Monthly Means',error_1D = semvelo2)
    call mod12_memori(13,10.,0.3,num_freq = 1,gap = 2)
    dx = 10./13

    do i = 1,13
        call numberc(dx/2.+(i-1)*dx,-1.,0.35,mvelo2(i),0.,2)
    end do

    ! do i = 1, 13
    !     do j = 1, 15
    !         if(i/=13)then 
    !             call symbolc(dx/2.+dx*(i-1),-1.-0.3*(j-1),0.25,direction((j-1)*12+i))
    !         else 
    !             call symbolc(dx/2.+dx*(i-1),-1.-0.3*(j-1),0.25,direction((j-1)*12+1)) ! jan
    !         end if
    !     end do
    ! end do

    call plot(12.,4.6,-3)
    call symbolc(5.,0.8,0.8,'Wind Direction')
    do i = 1, 12
        if(i/=12)call symbolc(dx/2.+dx*(i-1),0.,0.6,int2str(mod(i,12)))
        if(i==12)call symbolc(dx/2.+dx*(i-1),0.,0.6,int2str(12)) ! jan
        do j = 1, 15
            if(i/=13)then 
                call symbolc(dx/2.+dx*(i-1),-0.5-0.3*(j-1),0.25,direction((j-1)*12+i))
            else 
                call symbolc(dx/2.+dx*(i-1),-0.5-0.3*(j-1),0.25,direction((j-1)*12+1)) ! jan
            end if
        end do
    end do

    print*,maxloc(mvelo2),maxval(mvelo2),minloc(mvelo2),minval(mvelo2)

    call plote

end program read_amedas