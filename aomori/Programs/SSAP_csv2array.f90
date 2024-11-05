program SSAP_csv2array
    implicit none

    ! Define a derived type to hold an array of character labels and a 3-dimensional array of real numbers
    type :: labeled_array
        integer, dimension(:,:), allocatable :: num_labels
        character(len=50), dimension(:,:), allocatable :: str_labels
        real, dimension(:,:,:), allocatable :: values
    end type labeled_array

    ! Declare a variable of the derived type
    type(labeled_array) :: SSAP
    integer :: num_years, num_months,num_rows
    character(len=20) :: yyyy
    character(len=999) :: filename, line
    integer :: ios, i, j,n,k
    character(len=50), dimension(:), allocatable :: tokens
    integer :: num_tokens, start_pos, end_pos, limit
    real::rnum

    ! Parameters
    num_rows = 150   ! number of stations
    num_years = 15       ! number of years (files)
    num_months = 12      ! Number of months

    ! Allocate the arrays
    allocate(SSAP%num_labels(num_years,num_rows))
    allocate(SSAP%values(num_rows,num_years, num_months))
    allocate(SSAP%str_labels(num_years,num_rows))
    allocate(tokens(50))  ! Allocate enough space for tokens

    SSAP%values = 0.0; SSAP%num_labels = 0; SSAP%str_labels = ""

    do j = 1,num_years
        print *, 'Processing year:', 2008 + j
        write(yyyy, '(I4.4)') 2008 + j

        filename = "../Data/SSH/data/original/" // trim(yyyy) // "kiatu_utf8.csv"
        open(unit=10, file=filename, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'Error opening file:', filename
            stop
        end if

        n = 1
        ! Read the data from the file
        do
            read(10, '(A)', iostat=ios) line
            ! if(n>10)exit
            if (ios < 0) exit  ! Exit the loop if end of file
            if (ios /= 0) then
                print *, 'Error reading file:', filename
                stop
            end if
            line=adjustl(line)  ! Remove leading and trailing spaces
            line = trim(line)  ! Remove trailing spaces

            ! Skip blank rows or rows that start with commas
            if (len(line) == 0 .or. line(1:1) == ',') cycle

            ! Split the line into tokens based on commas
            num_tokens = 0
            start_pos = 1
            ! if(j+2008<2018)limit = 5
            ! if(j+2008>=2018)limit = 3
            do 
                end_pos = index(line(start_pos:), ',')
                if(end_pos > 5)then ! 5 digits or more eg) 10130 or 10130.0
                    read(line(start_pos:start_pos+end_pos-2),*,iostat=ios) rnum
                    if(ios/=0)then ! is NOT a number
                        num_tokens = num_tokens + 1
                        tokens(num_tokens) = adjustl(line(start_pos:start_pos+end_pos-2))
                        start_pos = start_pos + end_pos
                    else ! is a number
                        if(rnum<999.)then ! for values too small to be neither Pressure or Station Num
                            start_pos = start_pos + end_pos
                        else
                            num_tokens = num_tokens + 1
                            tokens(num_tokens) = adjustl(line(start_pos:start_pos+end_pos-2))
                            start_pos = start_pos + end_pos
                        end if
                    end if
                elseif(end_pos ==5 ) then ! 4 digits
                    read(line(start_pos:start_pos+end_pos-2),*,iostat=ios) rnum
                    if(ios/=0)then ! is Not a number         eg)****                
                        num_tokens = num_tokens + 1
                        tokens(num_tokens) = adjustl(line(start_pos:start_pos+end_pos-2))
                        start_pos = start_pos + end_pos
                    else ! is a number
                        if(rnum<999.)then ! for numbers too small to be neither Pressure or Station Num
                            start_pos = start_pos + end_pos
                        else
                            num_tokens = num_tokens + 1
                            tokens(num_tokens) = adjustl(line(start_pos:start_pos+end_pos-2))
                            start_pos = start_pos + end_pos
                        end if
                    end if
                else if(end_pos == 4)then ! 3 digits eg) 9.0 or 堺
                        read(line(start_pos:start_pos+end_pos-2),*,iostat=ios) rnum
                        if(ios/=0)then ! is Not a number
                            num_tokens = num_tokens + 1
                            tokens(num_tokens) = adjustl(line(start_pos:start_pos+end_pos-2))
                            start_pos = start_pos + end_pos
                        else ! is a number
                            if(rnum<999.)then ! for numbers too small to be neither Pressure or Station Num
                                start_pos = start_pos + end_pos
                            else
                                num_tokens = num_tokens + 1
                                tokens(num_tokens) = adjustl(line(start_pos:start_pos+end_pos-2))
                                start_pos = start_pos + end_pos
                            end if
                        end if
                else if(end_pos < 4.and.end_pos/=0)then ! 2 digits or less but not the last token eg) 40 or comma
                        start_pos = start_pos + end_pos !skip 3 digits or less characters
                else if(end_pos==0)then !where the last token is reached
                    num_tokens = num_tokens + 1
                    tokens(num_tokens) = adjustl(line(start_pos:))
                    exit
                else 
                    print*,'unhandled case, end_pos=',end_pos
                    exit
                end if
                ! if(num_tokens>0)print*,tokens(num_tokens)
            end do

            read(tokens(1), *,iostat=ios) rnum   ! GOTTA PUT THE TOKEN IN A REAL VAR FIRST AND THEN CONVERT IT TO INTEGER
            if (ios /= 0) then
                print*,'cycled loop ',n,tokens(1);cycle !the n loop
            else
                SSAP%num_labels(j,n) = int(rnum)
                ! print*,'success 1 ',tokens(1),SSAP%num_labels(j,n)
            end if

            SSAP%str_labels(j,n) = trim(tokens(2))
            ! print*,trim(tokens(2)),len(trim(tokens(2))),SSAP%str_labels(j,n),len(SSAP%str_labels(j,n))
            ! print*,'2 ',tokens(2),SSAP%str_labels(j,n)
            do i = 1, num_months
                if (i+2 <= num_tokens) then
                    read(tokens(i+2), *, iostat=ios) rnum
                    if (ios /= 0)then 
                        SSAP%values(n,j, i) = -999.0  ! meaning, not fully a number
                        print*,'fail 3 ',i,tokens(i+2)
                    else;
                        SSAP%values(n,j, i) = rnum
                    end if
                else
                    SSAP%values(n,j, i) = -999.0  ! puts -999.0 for missing values
                    print*,'fail 4 ',i,tokens(i+2)
                end if
            end do
            n = n + 1
        end do
        k=1
        do while (SSAP%num_labels(j,k) /= 0)
            ! print *, SSAP%num_labels(j,k), SSAP%str_labels(k), (SSAP%values(k,1,1:num_months))
            k = k + 1
        end do
        print *, 'Total number of rows:', k
        ! Close the file
        close(10)
    end do

    ! end read

    ! begin write

    do j = 1, num_years
        write(yyyy, '(I4.4)') 2008 + j
        open(unit=20, file='../Data/SSH/data/SSAP'//trim(yyyy)//'.csv', status='replace', action='write')
        ! write(20, '(A)') ''
        ! write(20, '(A)') 'Year: ' // trim(yyyy)
        do n = 1, num_rows
            if (SSAP%num_labels(j,n) == 0) cycle
            write(20, '(I4,",",A,",",12(F8.1,","))') SSAP%num_labels(j,n), trim(SSAP%str_labels(j,n)), (SSAP%values(n,j,i), i = 1, num_months)
        end do
        ! write(20, '(A)') ''
        close(20)
    end do

    ! open(unit=20, file='../Data/SSH/data/SSAP_total.csv', status='replace', action='write')

    !     do j = 1, num_years
    !     write(yyyy, '(I4.4)') 2008 + j
    !     write(20, '(A)') ''
    !     write(20, '(A)') 'Year: ' // trim(yyyy)
    !     do n = 1, num_rows
    !         if (SSAP%num_labels(j,n) == 0) cycle
    !         write(20, '(I4,",",A,",",12(F8.1,","))') SSAP%num_labels(j,n), trim(SSAP%str_labels(j,n)), (SSAP%values(n,j,i), i = 1, num_months)
    !     end do
    !     ! write(20, '(A)') ''
    ! end do
    ! close(20)


end program SSAP_csv2array

! if(end_pos > 5)then ! 5 digits or more eg) 10130 or 10130.0
!     num_tokens = num_tokens + 1
!     tokens(num_tokens) = adjustl(line(start_pos:start_pos+end_pos-2))
!     start_pos = start_pos + end_pos
! elseif(end_pos ==5 ) then ! 4 digits
!     if(line(start_pos:start_pos+1)=='40')then 
!         start_pos = start_pos + end_pos !distinguish between 40.0s and 1701 or something (note; no station nums that start with 40XX)
!     else
!         num_tokens = num_tokens + 1
!         tokens(num_tokens) = adjustl(line(start_pos:start_pos+end_pos-2))
!         start_pos = start_pos + end_pos
!     end if
! else if(end_pos == 4)then ! 3 digits eg) 9.0 or 堺
!         if(iachar(line(start_pos:start_pos)) <=127)then !if it is a number (3 digit or less num)
!             start_pos = start_pos + end_pos
!         else
!             num_tokens = num_tokens + 1
!             tokens(num_tokens) = adjustl(line(start_pos:start_pos+end_pos-2))
!             start_pos = start_pos + end_pos 
!         end if
! else if(end_pos < 4.and.end_pos/=0)then ! 2 digits or less but not the last token eg) 40 or comma
!         start_pos = start_pos + end_pos !skip 3 digits or less characters
! else if(end_pos==0)then !where the last token is reached
!     num_tokens = num_tokens + 1
!     tokens(num_tokens) = adjustl(line(start_pos:))
!     exit
! else 
!     print*,'unhandled case, end_pos=',end_pos
!     exit
! end if