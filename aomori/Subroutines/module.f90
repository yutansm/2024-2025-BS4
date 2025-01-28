module psstat
    implicit none
    logical :: stoff, land, pageend
    integer :: ipage
    real :: xorig, yorig
    integer::ounit = 255
end module psstat

module mypsstat
    logical::topstat = .false.,centerstat = .false.,bottomstat = .false.,logstat = .false. 
    integer::savecount = 0,plots2count = 0,tolog = 254
    real,parameter::precision = 10.**(-5)
    real::xn,yn
    character(len=20),dimension(100)::labels=''
    real,dimension(100)::label_x=0.,label_y=0.
end module mypsstat

module functions
    implicit none
    contains
    ! put depth in to calculate Potential Density
    function f_t95(df) result(t95coeff)
        implicit none
        integer,intent(in)::df
        real::t95coeff
        real,dimension(0:1000)::t95

        t95(1) = 12.706 ; t95(11) = 2.2010 ; t95(21) = 2.0796
        t95(2) = 4.3026 ; t95(12) = 2.1788 ; t95(22) = 2.0739
        t95(3) = 3.1824 ; t95(13) = 2.1604 ; t95(23) = 2.0687
        t95(4) = 2.7765 ; t95(14) = 2.1448 ; t95(24) = 2.0639
        t95(5) = 2.5706 ; t95(15) = 2.1315 ; t95(25) = 2.0595
        t95(6) = 2.4469 ; t95(16) = 2.1191 ; t95(26) = 2.0555
        t95(7) = 2.3646 ; t95(17) = 2.1098 ; t95(27) = 2.0518
        t95(8) = 2.3060 ; t95(18) = 2.1009 ; t95(28) = 2.0484
        t95(9) = 2.2621 ; t95(19) = 2.0930 ; t95(29) = 2.0452
        t95(10) = 2.2281 ;t95(20) = 2.0860 ; t95(30) = 2.0423

        if(df>=30 .and. df<=40)then 
            t95(df) = 2.021
        else if(df>=40 .and. df<=50)then
            t95(df) = 2.009
        else if(df>=50 .and. df<=60)then
            t95(df) = 2.000
        else if(df>=60 .and. df<=70)then
            t95(df) = 1.994
        else if(df>=70 .and. df<=80)then
            t95(df) = 1.990
        else if(df>=80 .and. df<=100)then
            t95(df) = 1.984
        else if(df>=100 .and. df<=1000)then
            t95(df) = 1.962
        end if

        t95(0) = 0. !just for the sake of programs

        if(df>=0 .and. df<=1000) then
            t95coeff = t95(df)
        else
            t95coeff = 1.960
             print*, 'df > 1000, approximating df as infinity'
        end if
    end function f_t95
    function f_t90(df) result(t90coeff)
        implicit none
        integer,intent(in)::df
        real::t90coeff
        real,dimension(0:1000)::t90

        t90(1) = 6.3138 ; t90(11) = 1.7959 ; t90(21) = 1.7210
        t90(2) = 2.9200 ; t90(12) = 1.7823 ; t90(22) = 1.7171
        t90(3) = 2.3534 ; t90(13) = 1.7709 ; t90(23) = 1.7139
        t90(4) = 2.1318 ; t90(14) = 1.7613 ; t90(24) = 1.7109
        t90(5) = 2.0150 ; t90(15) = 1.7531 ; t90(25) = 1.7081
        t90(6) = 1.9432 ; t90(16) = 1.7459 ; t90(26) = 1.7056
        t90(7) = 1.8946 ; t90(17) = 1.7396 ; t90(27) = 1.7033
        t90(8) = 1.8595 ; t90(18) = 1.7341 ; t90(28) = 1.7011
        t90(9) = 1.8331 ; t90(19) = 1.7291 ; t90(29) = 1.6991
        t90(10) = 1.8125 ; t90(20) = 1.7250 ; t90(30) = 1.6973

        t90(0) = 0. !just for the sake of programs

        if(df>=30 .and. df<=40)then 
            t90(df) = 1.684
        else if(df>=40 .and. df<=50)then
            t90(df) = 1.676
        else if(df>=50 .and. df<=60)then
            t90(df) = 1.671
        else if(df>=60 .and. df<=70)then
            t90(df) = 1.667
        else if(df>=70 .and. df<=80)then
            t90(df) = 1.664
        else if(df>=80 .and. df<=100)then
            t90(df) = 1.660
        else if(df>=100 .and. df<=1000)then
            t90(df) = 1.646
        end if

        if(df>=0 .and. df<=1000) then
            t90coeff = t90(df)
        else
            t90coeff = 1.645
             print*, 'df > 1000, approximating df as infinity'
        end if
    end function f_t90
    function fwelchdf(s1, dataquan1, s2, dataquan2) result(df)
        implicit none
        real, intent(in) :: s1, s2
        integer, intent(in) :: dataquan1, dataquan2
        real ::  n1, n2
        integer :: df

        n1 = real(dataquan1)
        n2 = real(dataquan2)
        df = int((((s1**2.0) / n1) + ((s2**2.0) / n2))**2.0 / (((s1**2.0 / n1)**2.0 / (n1 - 1)) + ((s2**2.0 / n2)**2.0 / (n2 - 1))))

    end function fwelchdf                                                                                                                                                                                          
    function fwelcht(mean1, s1, dataquan1, mean2, s2, dataquan2) result(result)
        implicit none
        real, intent(in) :: mean1, s1, mean2, s2
        integer, intent(in) :: dataquan1, dataquan2
        integer :: result
        ! real, dimension(0:30) :: t_95 = 0.0
        real :: diff_mean, n1, n2, df, sem, bottomCI, topCI
    
        if (mean1 /= 0.0 .and. mean2 /= 0.0 .and. dataquan1 /= 0 .and. dataquan2 /= 0) then
            diff_mean = mean1 - mean2
            n1 = real(dataquan1)
            n2 = real(dataquan2)
            sem = sqrt((s1**2.0 / n1) + (s2**2.0 / n2))
            df = (((s1**2.0) / n1) + ((s2**2.0) / n2))**2.0 / (((s1**2.0 / n1)**2.0 / (n1 - 1)) + ((s2**2.0 / n2)**2.0 / (n2 - 1)))
            ! call t95_value(t_95)
            bottomCI = diff_mean - f_t95(nint(df)) * sem
            topCI = diff_mean + f_t95(nint(df)) * sem
            ! print*, diff_mean, bottomCI, topCI, int(df)
            if (bottomCI > 0.0) then
                result = 1 ! larger
            else if (topCI < 0.0) then
                result = -1 ! smaller
            else
                result = 0 ! no difference in the desired level
            end if
        else
            result = 911 ! error
        end if
    
    end function fwelcht
    function fwelcht_onetailed(mean1, s1, dataquan1, mean2, s2, dataquan2) result(result)
        implicit none
        real, intent(in) :: mean1, s1, mean2, s2
        integer, intent(in) :: dataquan1, dataquan2
        integer :: result
        real :: diff_mean, n1, n2, df, sem, bottomCI, topCI
    
        if (mean1 /= 0.0 .and. mean2 /= 0.0 .and. dataquan1 /= 0 .and. dataquan2 /= 0) then
            diff_mean = mean1 - mean2
            n1 = real(dataquan1)
            n2 = real(dataquan2)
            sem = sqrt((s1**2.0 / n1) + (s2**2.0 / n2))
            df = (((s1**2.0) / n1) + ((s2**2.0) / n2))**2.0 / (((s1**2.0 / n1)**2.0 / (n1 - 1)) + ((s2**2.0 / n2)**2.0 / (n2 - 1)))
            topCI = diff_mean + f_t90(nint(df)) * sem
            bottomCI = diff_mean - f_t90(nint(df)) * sem ! using critical t values for 90% since it is one-tailed, the significance level is 95%
            if(diff_mean > 0.)then ! assumes mean1 > mean2
                if (bottomCI > 0.0) then
                    result = 1 ! mean1 is significantly greater than mean2
                else
                    result = 0 ! no significant difference
                end if
            elseif(diff_mean < 0.)then  ! assumes mean1 < mean2
                if (topCI < 0.) then
                    result = -1 ! mean1 is significantly smaller than mean2
                else
                    result = 0 ! no significant difference
                end if
            elseif(diff_mean == 0.)then
                result = 0 ! no difference in mean at all
            end if 
        else
            result = 911 ! error
        end if

    end function fwelcht_onetailed
    function fwelcht_greater(mean1, s1, dataquan1, mean2, s2, dataquan2) result(result)
        implicit none
        real, intent(in) :: mean1, s1, mean2, s2
        integer, intent(in) :: dataquan1, dataquan2
        integer :: result
        real :: diff_mean, n1, n2, df, sem, bottomCI
    
        if (mean1 /= 0.0 .and. mean2 /= 0.0 .and. dataquan1 /= 0 .and. dataquan2 /= 0) then
            diff_mean = mean1 - mean2
            n1 = real(dataquan1)
            n2 = real(dataquan2)
            sem = sqrt((s1**2.0 / n1) + (s2**2.0 / n2))
            df = (((s1**2.0) / n1) + ((s2**2.0) / n2))**2.0 / (((s1**2.0 / n1)**2.0 / (n1 - 1)) + ((s2**2.0 / n2)**2.0 / (n2 - 1)))
            bottomCI = diff_mean - f_t90(nint(df)) * sem
            if (bottomCI > 0.0) then
                result = 1 ! mean1 is significantly greater than mean2
            else
                result = 0 ! no significant difference
            end if
        else
            result = 911 ! error
        end if
    end function fwelcht_greater
    function fwelcht_smaller(mean1, s1, dataquan1, mean2, s2, dataquan2) result(result)
        implicit none
        real, intent(in) :: mean1, s1, mean2, s2
        integer, intent(in) :: dataquan1, dataquan2
        integer :: result
        real :: diff_mean, n1, n2, df, sem, topCI
    
        if (mean1 /= 0.0 .and. mean2 /= 0.0 .and. dataquan1 /= 0 .and. dataquan2 /= 0) then
            diff_mean = mean1 - mean2
            n1 = real(dataquan1)
            n2 = real(dataquan2)
            sem = sqrt((s1**2.0 / n1) + (s2**2.0 / n2))
            df = (((s1**2.0) / n1) + ((s2**2.0) / n2))**2.0 / (((s1**2.0 / n1)**2.0 / (n1 - 1)) + ((s2**2.0 / n2)**2.0 / (n2 - 1)))
            topCI = diff_mean + f_t90(nint(df)) * sem
            if (topCI < 0.0) then
                result = -1 ! mean1 is significantly smaller than mean2
            else
                result = 0 ! no significant difference
            end if
        else
            result = 911 ! error
        end if
    end function fwelcht_smaller
    function fwelcht90(mean1, s1, dataquan1, mean2, s2, dataquan2) result(result)
        implicit none
        real, intent(in) :: mean1, s1, mean2, s2
        integer, intent(in) :: dataquan1, dataquan2
        integer :: result
        ! real, dimension(0:30) :: t_90 = 0.0
        real :: diff_mean, n1, n2, df, sem, bottomCI, topCI

        if (mean1 /= 0.0 .and. mean2 /= 0.0 .and. dataquan1 /= 0 .and. dataquan2 /= 0) then
            diff_mean = mean1 - mean2
            n1 = real(dataquan1)
            n2 = real(dataquan2)
            sem = sqrt((s1**2.0 / n1) + (s2**2.0 / n2))
            df = (((s1**2.0) / n1) + ((s2**2.0) / n2))**2.0 / (((s1**2.0 / n1)**2.0 / (n1 - 1)) + ((s2**2.0 / n2)**2.0 / (n2 - 1)))
            ! call t90_value(t_90)
            bottomCI = diff_mean - f_t90(nint(df)) * sem
            topCI = diff_mean + f_t90(nint(df)) * sem
            ! print*, diff_mean, bottomCI, topCI, int(df)
            if (bottomCI > 0.0) then
                result = 1 ! larger
            else if (topCI < 0.0) then
                result = -1 ! smaller
            else
                result = 0 ! no difference in the desired level
            end if
        else
            result = 911 ! error
        end if
    end function fwelcht90

    function fcorrecoeff(array_1D, array_1D2) result(r)
        implicit none
        real, intent(in) :: array_1D(:), array_1D2(:)
        real :: r
        real :: mean1, mean2, s1, s2, sum0, sum1, sum2, covariance
        integer :: n, count, dim
    
        dim = size(array_1D)
        ! if (size(array_1D2) /= dim) then
        !     print *, 'Arrays must have the same size.'
        !     stop
        ! end if
    
        sum0 = 0.0
        sum1 = 0.0
        sum2 = 0.0
        count = 0
    
        do n = 1, dim
            if (array_1D(n) /= 0.0 .and. array_1D2(n) /= 0.0) then
                count = count + 1
                sum1 = sum1 + array_1D(n)
                sum2 = sum2 + array_1D2(n)
            end if
        end do
    
        if (count <= 1) then
            r = 0.0
        else
            mean1 = sum1 / real(count)
            mean2 = sum2 / real(count)
            sum0 = 0.0
            sum1 = 0.0
            sum2 = 0.0
            do n = 1, dim
                if (array_1D(n) /= 0.0 .and. array_1D2(n) /= 0.0) then
                    sum0 = sum0 + (array_1D(n) - mean1) * (array_1D2(n) - mean2)
                    sum1 = sum1 + (array_1D(n) - mean1) ** 2
                    sum2 = sum2 + (array_1D2(n) - mean2) ** 2
                end if
            end do
            s1 = sqrt(sum1 / real(count - 1))
            s2 = sqrt(sum2 / real(count - 1))
            covariance = sum0 / real(count - 1)
            if (s1 == 0.0 .or. s2 == 0.0) then
                r = 0.0
            else
                r = covariance / (s1 * s2)
            end if
        end if
    end function fcorrecoeff
    ! returns the t-score for a given r value and n
    ! n is the number of (pairs of) data points not degrees of freedom
    function f_rtscore(n,r) result(tscore)
        implicit none
        real, intent(in) :: r
        integer, intent(in) :: n
        real :: tscore

        tscore = r * sqrt(real(n - 2) / (1.0 - r**2))

    end function f_rtscore
    ! n is the quantity of data point pairs. not degrees of freedom
    ! critical_values is an array of critical values for 95 percent confidence on both sides,column 1 gives positive critical value, column 2 gives negative critical value
    function f_rcritical95(n) result(critical_values)
        ! use functions
        implicit none 
        integer, intent(in) :: n
        real :: critical_values(2)
        integer :: df
        real :: r
    
        if(n<=2)then 
            df = 1
        else 
            df = n - 2
        end if
        r = sqrt(f_t95(df)**2.0 / (df + f_t95(df)**2.0))
        critical_values(1) = r
        critical_values(2) = -r
    end function f_rcritical95
    function int2str(i,form) result(str)
        integer, intent(in) :: i
        character(:), allocatable :: str
        character(range(i)+2) :: tmp
        character(len=*),intent(in),optional::form
        if(present(form))then
            write(tmp,trim(adjustl(form))) i
        else
            write(tmp, '(i0)') i
        end if
        str = trim(tmp)
    end function int2str
    function real2str(r,digits_after_point) result(str)
        real, intent(in) :: r
        integer, intent(in), optional :: digits_after_point
        character(:), allocatable :: str
        character(len=32) :: tmp
        if (present(digits_after_point)) then
            write(tmp, '(f0.'//int2str(digits_after_point)//')') r
        else
            write(tmp, '(f0.1)') r
        end if
        str = trim(tmp)
    end function real2str
    function remove_extension(filename) result(filenamewoex)
        implicit none
        character(len=*), intent(in) :: filename
        character(len=len(filename)) :: filenamewoex
        integer :: dot_position

        ! Initialize filenamewoex with the input filename
        filenamewoex = filename

        ! Find the position of the last dot in the filename
        dot_position = index(filename, '.', back=.true.)
        if (dot_position > 3) then
            filenamewoex = filename(:dot_position-1)
        else;print*,'no extension'
        end if

    end function remove_extension
    function append_extension(filename, extension) result(new_filename)
        implicit none
        character(len=*), intent(in) :: filename, extension
        character(len=len(filename) + len(extension)+1) :: new_filename

        ! Append the extension to the filename
        new_filename = trim(adjustl(filename)) // '.' // trim(adjustl(extension))

    end function append_extension
    function change_extension(filename, new_extension) result(new_filename)
        implicit none
        character(len=*), intent(in) :: filename, new_extension
        character(len=len(filename) + len(new_extension)+1) :: new_filename
        character(len=len(filename)) :: basename
        integer :: dot_position

        ! Initialize basename with the input filename
        basename = filename

        ! Find the position of the last dot in the filename
        dot_position = index(filename, '.', back=.true.)
        if (dot_position > 3) then
            basename = filename(:dot_position-1)
        else
            basename = filename
        end if

        ! Append the new extension to the basename
        new_filename = trim(adjustl(basename)) // '.' // trim(adjustl(new_extension))

    end function change_extension
    function intdigits(number) result(number_of_digits)
        implicit none
        integer, intent(in) :: number
        character(len=20) :: num_str
        integer ::   number_of_digits

        ! Convert the number to a string
        write(num_str, '(I0)') number

        ! Get the length of the string
        number_of_digits = len_trim(num_str)
    end function intdigits
    function rsign(number) result(real_sign)
        implicit none
        real, intent(in) :: number
        real :: real_sign

        if (number > 0.0) then
            real_sign = 1.
        elseif (number < 0.0) then
            real_sign = -1.
        else
            real_sign = 0.
        end if
    end function rsign
    function month_names(int) result(month_name)
        implicit none
        integer, intent(in) :: int
        character(len=4) :: month_name

        select case(int)
            case(1)
                month_name = 'Jan.'
            case(2)
                month_name = 'Feb.'
            case(3)
                month_name = 'Mar.'
            case(4)
                month_name = 'Apr.'
            case(5)
                month_name = 'May'
            case(6)
                month_name = 'Jun.'
            case(7)
                month_name = 'Jul.'
            case(8)
                month_name = 'Aug.'
            case(9)
                month_name = 'Sep.'
            case(10)
                month_name = 'Oct.'
            case(11)
                month_name = 'Nov.'
            case(12)
                month_name = 'Dec.'
            case default
                month_name = 'Err.'
        end select
    end function month_names

    !!! Random
    function JODC_dep_to_index(depth,info) result(index)
        implicit none
        integer,intent(in)::depth
        logical,intent(in),optional::info
        integer::index
        logical::info_local
        info_local = .false.
        if(present(info))info_local = info
        if(depth == 0)index = 0
        if(depth == 10)index = 1
        if(depth == 20)index = 2
        if(depth == 30)index = 3
        if(depth == 50)index = 4
        if(depth == 75)index = 5
        if(depth == 100)index = 6
        if(depth == 125)index = 7
        if(depth == 150)index = 8
        if(depth == 200)index = 9
        if(depth == 250)index = 10
        if(depth == 300)index = 11
        if(depth == 400)index = 12
        if(depth == 500)index = 13
        if(depth == 600)index = 14
        if(depth == 700)index = 15
        if(depth == 800)index = 16
        if(depth == 900)index = 17
        if(depth == 1000)index = 18
        if(depth == 1100)index = 19
        if(depth == 1200)index = 20
        if(depth == 1300)index = 21
        if(depth == 1400)index = 22
        if(depth == 1500)index = 23
        if(depth == 1750)index = 24
        if(depth == 2000)index = 25
        if(depth == 2500)index = 26
        if(depth == 3000)index = 27
        if(depth == 3500)index = 28
        if(depth == 4000)index = 29
        if(depth == 4500)index = 30
        if(depth == 5000)index = 31
        if(depth == 5500)index = 32

        if(depth>5500.or.depth<0)then;print*,'out of range depth in JODC_data:',depth;stop;endif
        if(info_local)then
            print*,'depth:',depth,'index:',index
        end if

    end function JODC_dep_to_index
    function JODC_index_to_dep(index,info) result(depth)
        implicit none
        integer,intent(in)::index
        logical,intent(in),optional::info
        integer::depth
        logical::info_local
        info_local = .false.
        if(present(info))info_local = info
        select case(index)
            case(0);depth = 0
            case(1);depth = 10
            case(2);depth = 20
            case(3);depth = 30
            case(4);depth = 50
            case(5);depth = 75
            case(6);depth = 100
            case(7);depth = 125
            case(8);depth = 150
            case(9);depth = 200
            case(10);depth = 250
            case(11);depth = 300
            case(12);depth = 400
            case(13);depth = 500
            case(14);depth = 600
            case(15);depth = 700
            case(16);depth = 800
            case(17);depth = 900
            case(18);depth = 1000
            case(19);depth = 1100
            case(20);depth = 1200
            case(21);depth = 1300
            case(22);depth = 1400
            case(23);depth = 1500
            case(24);depth = 1750
            case(25);depth = 2000
            case(26);depth = 2500
            case(27);depth = 3000
            case(28);depth = 3500
            case(29);depth = 4000
            case(30);depth = 4500
            case(31);depth = 5000
            case(32);depth = 5500
        end select
        if(index>32.or.index<0)then;print*,'out of range index in JODC_data:',index;stop;endif
        if(info_local)then
            print*,'index:',index,'depth:',depth
        end if
    end function JODC_index_to_dep
end module functions

module origin
    use psstat
    use mypsstat
    contains
    subroutine plots(x,y,mode,psfile)
        character psfile*(*)
        character*24 date
        character*20 head
        character(len=8) :: date_str, time_str
        integer :: values(8)
        xorig=x
        yorig=y
        ipage=0
        stoff=.false.
        land =.false.
        pageend =.true.
        ! if(lu.le.0) lu=8

        if(mod(mode/2,2).eq.1) stoff=.true.
        if(mod(mode/4,2).eq.1) land =.true.

        if(mod(mode,2).eq.0) then
            open(unit = ounit,file='xys.ps')
        else 
            open(unit = ounit,file=psfile)
        endif

        call date_and_time(values = values)
        write(date_str, '(i4.4, i2.2, i2.2)') values(1), values(2), values(3)
        write(time_str, '(i2.2, i2.2, i2.2)') values(5), values(6), values(7)
        date = trim(date_str) // 'T' // trim(time_str) // 'Z'
        head = 'Yuta Nishimori'

        ! write(ounit,'(a)') "%!PS-Adobe-2.1"
        write(ounit,'(a)') "%!PS-Adobe-3.0"
        write(ounit,'(a)') "%%Creator: PS-LibraryV2 97.08.08" 
        ! write(ounit,'(a)') "%%Creator: Yuta Nishimori" 

        write(ounit,'(a)') "%%Copyright: PS-LibraryV2  Fukuda & Saito & little bit of Yuta" 
        write(ounit,'(2a)') "%%CreationDate: ",date
        write(ounit,'(2a)') "%%Title: ",trim(head)
        
        write(ounit,'(a)') "%%Pages: (atend)"
        write(ounit,'(a)') "%%BoundingBox: 0 0 596 841 "
        write(ounit,'(a)') "%%EndComments: AS CHILDREN OF THE REPUBLIC"
        write(ounit,'(a)') "%%BeginPlolog"
        write(ounit,'(a)') "%%%PS-LibraryV2(Spring-Tulip) define start %%%"
        write(ounit,'(3a)') "/head ( ",trim(head)," ) def" 

        write(ounit,'(a)') "<< /PageSize [596 841] >> setpagedevice" !841 1191
        write(ounit,'(a)') "/np  { newpath } def"
        write(ounit,'(a)') "/mv  { moveto } def"
        write(ounit,'(a)') "/ln  { lineto } def"
        write(ounit,'(a)') "/sh  { show } def"
        write(ounit,'(a)') "/sl  { setlinewidth } def"
        write(ounit,'(a)') "/sn  { stroke newpath } def" 

        write(ounit,'(a)') "/fo  { /Helvetica-Bold  findfont } def"
        write(ounit,'(a)') "/kfo { /GothicBBB-Medium-H  findfont } def"
        ! write(ounit,'(a)') "/jfo { /Ryumin-Light findfont } def"
        ! write(ounit,'(a)') "/shs { /SourceHanSansJP-Regular findfont } def"
        ! write(ounit,'(a)') "/Helvetica-Bold /CIDFont findresource"
        ! write(ounit,'(a)') "/Identity-H /CMap findresource"
        ! write(ounit,'(a)') "composefont /Helvetica-Bold-Japanese def"
        ! write(ounit,'(a)') "/fo  { /Helvetica-Bold-Japanese findfont } def"

        write(ounit,'(a)') "/sf  { scalefont } def"
        write(ounit,'(a)') "/se  { setfont } def"
        write(ounit,'(a)') "/ro  { rotate } def"
        write(ounit,'(a)') "/sd  { setdash } def"
        write(ounit,'(a)') "/st  { stroke } def"
        write(ounit,'(a)') "/sp  { stroke showpage } def"
        write(ounit,'(a)') "/tl  { translate } def"
        write(ounit,'(a)') "/sc  { scale } def"
        write(ounit,'(a)') "/gs  { gsave } def"
        write(ounit,'(a)') "/gr  { grestore } def"
        write(ounit,'(a)') "/crg { setrgbcolor } def"
        write(ounit,'(a)') "/chs { sethsbcolor } def"
        write(ounit,'(a)') "/sg  { setgray } def"
        write(ounit,'(a)') "/av  { add 2 div } def"
        write(ounit,'(a)') "/ed  { exch def } def"
        write(ounit,'(a)') "/atp { arcto pop pop pop pop } def"

        write(ounit,'(a)') "/roundRect {"
        write(ounit,'(a)') "  np width 0.1 sub 0 mv"
        write(ounit,'(a)') "  width 0 width height 0.1 atp"
        write(ounit,'(a)') "  width height 0 height 0.1 atp"
        write(ounit,'(a)') "  0 height 0 0 0.1 atp"
        write(ounit,'(a)') "  0 0 width 0 0.1 atp closepath"
        write(ounit,'(a)') "} def"

        write(ounit,'(a)') "/Stamp {"
        write(ounit,'(a)') "  fo 0.3 sf se"
        write(ounit,'(a)') "  gs clippath pathbbox gr"
        write(ounit,'(a)') "  pop /urx ed /lly ed pop"
        write(ounit,'(a)') "  urx 0.2 sub lly 0.2 add"
        write(ounit,'(a)') "  gs"
        write(ounit,'(a)') "    tl" 
        write(ounit,'(a)') "    0.01 sl"
        write(ounit,'(a)') "    0 0 mv" 
        write(ounit,'(a)') "    3 string cvs dup false charpath" 
        write(ounit,'(a)') "    ( : Page ) dup false charpath" 
        write(ounit,'(a)') "    head dup false charpath" 
        write(ounit,'(a)') "    pathbbox" 
        write(ounit,'(a)') "    /y1 ed /x1 ed /y0 ed /x0 ed"
        write(ounit,'(a)') "    /width x1 x0 sub 0.2 add def"
        write(ounit,'(a)') "    /height y1 y0 sub 0.2 add def"
        write(ounit,'(a)') "    x1 neg 0 tl"
        write(ounit,'(a)') "    gs"
        write(ounit,'(a)') "      x0 0.1 sub y0 0.1 sub tl roundRect"
        write(ounit,'(a)') "      st"
        write(ounit,'(a)') "    gr"
        write(ounit,'(a)') "    0 0 mv"
        write(ounit,'(a)') "    sh sh sh"
        write(ounit,'(a)') "  gr"
        write(ounit,'(a)') "} def"

        write(ounit,'(a)') "/tocm { 72.0 2.54 div dup sc } def"
        write(ounit,'(a)') "/a4x 21.0 def"
        write(ounit,'(a)') "/a4y 29.7 def"
        ! write(ounit,'(a)') "/tocm { 72.0 2.54 div dup sc } def"
        ! write(ounit,'(a)') "/a4x 29.7 def"
        ! write(ounit,'(a)') "/a4y 42.0 def"

        write(ounit,'(a)') "/landscape {  90.0 ro 0.0 a4x neg tl } def" 

        write(ounit,'(a)') "/boxf " 
        write(ounit,'(a)') "{-0.5 -0.5 rmoveto"
        write(ounit,'(a)') "  0.0  1.0 rlineto" 
        write(ounit,'(a)') "  1.0  0.0 rlineto"
        write(ounit,'(a)') "  0.0 -1.0 rlineto"
        write(ounit,'(a)') " -1.0  0.0 rlineto"
        write(ounit,'(a)') "  closepath"
        write(ounit,'(a)') "  fill"
        write(ounit,'(a)') "} def"

        write(ounit,'(a)') "/box " 
        write(ounit,'(a)') "{-0.5 -0.5 rmoveto"
        write(ounit,'(a)') "  0.0  1.0 rlineto" 
        write(ounit,'(a)') "  1.0  0.0 rlineto"
        write(ounit,'(a)') "  0.0 -1.0 rlineto"
        write(ounit,'(a)') " -1.0  0.0 rlineto"
        write(ounit,'(a)') "  closepath   "
        write(ounit,'(a)') "} def     "

        write(ounit,'(a)') "/circle  { 0.0 360.0 arc st } def  "
        write(ounit,'(a)') "/circle2 { 0.0 360.0 arc st } def  "

        write(ounit,'(a)') "/plus"
        write(ounit,'(a)') "{ 0.0 -0.5 rmoveto"
        write(ounit,'(a)') "  0.0  1.0 rlineto"
        write(ounit,'(a)') " -0.5 -0.5 rmoveto"
        write(ounit,'(a)') "  1.0  0.0 rlineto"
        write(ounit,'(a)') "} def"

        write(ounit,'(a)') "/X"
        write(ounit,'(a)') "{   45.0   ro" 
        write(ounit,'(a)') "  0.0 -0.5 rmoveto"
        write(ounit,'(a)') "  0.0  1.0 rlineto"
        write(ounit,'(a)') " -0.5 -0.5 rmoveto"
        write(ounit,'(a)') "  1.0  0.0 rlineto"
        write(ounit,'(a)') "   -45.0   ro"
        write(ounit,'(a)') "} def"

        write(ounit,'(a)') "/star "
        write(ounit,'(a)') "{ 0.0    0.5   rmoveto"
        write(ounit,'(a)') "  0.105 -0.35  rlineto"
        write(ounit,'(a)') "  0.36   0.0   rlineto"
        write(ounit,'(a)') " -0.3   -0.21  rlineto"
        write(ounit,'(a)') "  0.1   -0.35  rlineto"
        write(ounit,'(a)') " -0.27   0.205 rlineto"
        write(ounit,'(a)') " -0.27  -0.205 rlineto"
        write(ounit,'(a)') "  0.1    0.35  rlineto"
        write(ounit,'(a)') " -0.3    0.21  rlineto"
        write(ounit,'(a)') "  0.36   0.0   rlineto"
        write(ounit,'(a)') "  closepath"
        write(ounit,'(a)') "} def"

        write(ounit,'(a)') "/dot { 0.0 360.0 arc fill st } def"
        write(ounit,'(a)') 
        write(ounit,'(a)') "/tranf    "
        write(ounit,'(a)') "{ 0.5 -0.288 rmoveto"
        write(ounit,'(a)') " -0.5  0.866 rlineto"
        write(ounit,'(a)') " -0.5 -0.866 rlineto"
        write(ounit,'(a)') "  1.0  0.0   rlineto"
        write(ounit,'(a)') "  closepath"
        write(ounit,'(a)') "  fill"
        write(ounit,'(a)') "} def"

        write(ounit,'(a)') "/tran     "
        write(ounit,'(a)') "{ 0.5 -0.288 rmoveto"
        write(ounit,'(a)') " -0.5  0.866 rlineto"
        write(ounit,'(a)') " -0.5 -0.866 rlineto"
        write(ounit,'(a)') "  1.0  0.0   rlineto"
        write(ounit,'(a)') "  closepath"
        write(ounit,'(a)') "} def"

        write(ounit,'(a)') "/PslNewPage {"
        write(ounit,'(a)') " save"
        write(ounit,'(a)') " tocm " 
        write(ounit,'(a)') " 2 setlinejoin"
        write(ounit,'(a)') " 0.00  sl"
        write(ounit,'(a)') " [ ] 0 sd"  
        write(ounit,'(a)') "} def"

        write(ounit,'(a)') "/PslEndPage {"
        write(ounit,'(a)') " st"
        write(ounit,'(a)') " showpage"
        write(ounit,'(a)') " restore"
        write(ounit,'(a)') "} def"
        write(ounit,'(a)') "%%EndPlolog"
        write(ounit,'(a)') "%%% PS-Library define end %%%"

        call newpage

        return
    end
    ! index nnfile does not need an extension
    subroutine plots2(psfile,nnfile,mode,h,hsize,oopt,x,y)
        character(len=*),intent(in),optional:: psfile,mode,h,oopt,nnfile
        integer::intmode
        character(len=3)::countstr
        real,intent(in),optional:: x,y,hsize

        plots2count = plots2count + 1
        ! ounit = ounit - 1
        write(countstr,'(i3)') plots2count
        countstr = adjustl(countstr)
        if(present(mode))then
            if(mode == 'land'.or.mode == 'landscape'.or.mode =='Landscape'.or.mode =='landScape'.or.mode =='LandScape'.or.mode =='LANDSCAPE')then
                intmode = 13
            else
                intmode = 9
            end if
        else
            intmode = 13
        end if
        if(present(psfile).and..not.present(nnfile))then
            call plots(0.,0.,intmode,psfile)
        elseif(present(nnfile).and..not.present(psfile))then 
            call plots(0.,0.,intmode,'../nonames/'//trim(nnfile)//'.ps')
        elseif(.not.present(psfile).and..not.present(nnfile))then
            call plots(0.,0.,intmode,'../nonames/'//trim(countstr)//'.ps')
        else;print*,'you cannot specify both psfile and nnfile';stop
        end if
        if(present(h))then
            if(present(hsize))then
                call header(h,symbol_size = hsize)
            else
                call header(h)
            end if
        end if
        if(present(oopt))then
            if(oopt == 'otops')then
                call otops;topstat = .true.
            else if(oopt == 'ocenter')then
                call ocenter;centerstat = .true.
            else if(oopt == 'obottoms')then
                call obottoms;bottomstat = .true.
            else
                call otops;topstat = .true.;print*,'otops is default'
            end if
        end if
        if(present(x))call plot(x,0.,-3)
        if(present(y))call plot(0.,y,-3)
        return
    end 
    subroutine newpage(h,x,y)
        implicit none
        character(len=*),optional,intent(in):: h
        real,intent(in),optional:: x,y
            call endpage
            call inipage      

            ! call plot(0.,0.,-3)
            if(present(h))then
                call header(h)
            end if
            if(present(x))call plot(x,0.,-3)
            if(present(y))call plot(0.,y,-3)

        return
    end
    subroutine plote
        call endpage
        write(ounit,'(a)') "%%Trailer"
        write(ounit,'(a,i3)') "%%Pages: ",ipage
        write(ounit,'(a,i3)') "%%EOF"
        close(ounit)
        return
    end 
    subroutine plot(x1,y1,im)
        use ieee_arithmetic

        ! if(ieee_is_nan(x1))then;print*,'x1 is NaN at subroutine plot';stop;endif
        ! if(ieee_is_nan(y1))then;print*,'y1 is NaN at subroutine plot';stop;endif
        ! if(ieee_is_finite(x1).eqv. .false.)then;print*,'x1 is Inf at subroutine plot';stop;endif
        ! if(ieee_is_finite(y1).eqv..false.)then;print*,'y1 is Inf at subroutine plot';stop;endif

        ! ! if(land.eqv..true.)then
        !     if(x1+xn>29.7)then;print*,'definitely drawing out of paper ','xn=',xn,'x1=',x1;stop;endif
        !     if(y1+yn>29.7)then;print*,'definitely drawing out of paper ','yn=',yn,'y1=',y1;stop;endif
        ! else
        !     if(x1+xn>21.0)then;print*,'drawing out of paper ','xn=',xn,'x1=',x1;stop;endif
        !     if(y1+yn>29.7)then;print*,'drawing out of paper ','yn=',yn,'y1=',y1;stop;endif
        ! end if
        
        if(im.eq.3) then 
            write(ounit, '(a)') " sn"
            write(ounit, *) x1 
            ! write(ounit, '(2x)')  ! Two spaces
            write(ounit, *) y1  
            write(ounit, '(2x,a3)') " mv"  ! two spaces and move
        elseif(im.eq.2) then
            write(ounit, *) x1 
            ! write(ounit, '(2x)')  ! Two spaces
            write(ounit, *) y1  
            write(ounit, '(2x,a3)') " ln"  ! two spaces and move
        elseif(im.eq.-3) then
            xn = xn + x1; yn = yn + y1
            write(ounit, *) x1 
            ! write(ounit, '(2x)')  ! Two spaces
            write(ounit, *) y1  
            write(ounit, '(2x,a3)') " tl"  ! two spaces and move
            write(ounit, '(a)') " sn"
            write(ounit, '(a)') " 0.0 0.0 mv"
        endif
        return
    end
    subroutine plotsave(label)
        character(len=*), intent(in) :: label
        if(len_trim(label)>20)then;print*,'your label is too long';stop;endif
        if(savecount>100)then;print*,'you have too many labels';stop;endif
        write(ounit,*) '% begin plotsave'
        do i = 1, savecount
            if(labels(i)==label)then
                print*,label,' label already exists'
                stop;return
            end if
        end do
        savecount = savecount +1
        labels(savecount) = trim(label)
        label_x(savecount) = xn;label_y(savecount) = yn
        ! print*,'label',savecount,'=',labels(savecount)
        write(ounit,*) '% end plotsave'
        return
    end 
    subroutine plotback(label)
        character(len=*), intent(in) :: label 
        do i = 1, savecount
            if(labels(i)==label)then
                call plot(label_x(i)-xn,label_y(i)-yn,-3)
                exit
            endif
            if(i == savecount .and. labels(i)/=label)then
                print*,label,' label not found'
                stop
            end if
        end do
    end 
    subroutine plotomit
        labels = ''
        label_x = 0.
        label_y = 0.
    end subroutine
    ! x and y are ratios
    subroutine plotmove(x,y)
        real,intent(in)::x,y
        if(x>1.)print*,'x /> 1, x is the ratio'
        if(y>1.)print*,'y /> 1, y is the ratio'
        if(land .eqv. .true.)then
            call plot(-xn,-yn,-3);call plot(29.7*x,21.*y,-3)
        else;call plot(-xn,-yn,-3);call plot(21.*x,29.7*y,-3)
        end if
        return
    end
    subroutine plotmove2(x,y)
        real,intent(in)::x,y
        if(land.eqv..true.)then  
            if(x>29.7)print*,'x > 29.7, x is the distance in cm from 0,0'
            if(y>21.0)print*,'y > 21.0, y is the distance in cm from 0,0'
        else
            if(x>21.0)print*,'x > 21.0, x is the distance in cm from 0,0'
            if(y>29.7)print*,'y > 29.7, y is the distance in cm from 0,0'
        end if
            call plot(-xn,-yn,-3);call plot(x,y,-3)
        return
    end
    ! y is in cm
    subroutine header(head_str,symbol_size,rangle,y)
        implicit none
        character(len=*),intent(in)::head_str
        real,intent(in),optional::symbol_size,y,rangle
        real::xnn,ynn

        xnn = xn;ynn = yn
        write(ounit,*) '% begin header'
        if(land)then;
            if(present(y))then
                call plotmove2(29.7*0.5,21.*0.93+y)
            else 
                call plotmove2(29.7*0.5,21.*0.93)
            end if
        else
            if(present(y))then
                call plotmove2(21.*0.5,29.7*0.95+y)
            else 
                call plotmove2(21.*0.5,29.7*0.95)
            end if
        end if

        if(present(symbol_size))then
            if(present(rangle))then
                call symbolc(0.,0.,symbol_size,head_str,rangle)
            else 
                call symbolc(0.,0.,symbol_size,head_str,0.)
            end if
        else
            if(present(rangle))then
                call symbolc(0.,0.,1.,head_str,rangle)
            else
                call symbolc(0.,0.,1.,head_str,0.)
            end if
        end if
        call plotmove2(xnn,ynn)
    end
    !x and y are diffs (in cm) from static point
    subroutine otops(x,y)
        implicit none
        real,intent(in),optional::x,y

        call plotmove(0.,0.)        
        if(land)then
            if(present(x))then
                if(present(y))then
                    call plotmove2(1.5+x,21.*0.93-1.5+y)
                else
                    call plotmove2(1.5+x,21.*0.93-1.5)
                end if
            else
                if(present(y))then
                    call plotmove2(1.5,21.*0.93-1.5+y)
                else
                    call plotmove2(1.5,21.*0.93-1.5)
                end if
            end if
        else
            if(present(x))then
                if(present(y))then
                    call plotmove2(1.5+x,29.7*0.95-1.5+y)
                else
                    call plotmove2(1.5+x,29.7*0.95-1.5)
                end if
            else 
                if(present(y))then
                    call plotmove2(1.5,29.7*0.95-1.5+y)
                else
                    call plotmove2(1.5,29.7*0.95-1.5)
                end if
            end if
        end if
    end
    subroutine ocenter(x,y)
        implicit none
        real,intent(in),optional::x,y

        call plotmove(0.,0.)
        if(land)then
            if(present(x))then
                if(present(y))then
                    call plotmove2(29.7*0.5+x,21.*0.5+y)
                else
                    call plotmove2(29.7*0.5+x,21.*0.5)
                end if
            else
                if(present(y))then
                    call plotmove2(29.7*0.5,21.*0.5+y)
                else
                    call plotmove2(29.7*0.5,21.*0.5)
                end if
            end if
        else
            if(present(x))then
                if(present(y))then
                    call plotmove2(21.*0.5+x,29.7*0.5+y)
                else
                    call plotmove2(21.*0.5+x,29.7*0.5)
                end if
            else
                if(present(y))then
                    call plotmove2(21.*0.5,29.7*0.5+y)
                else
                    call plotmove2(21.*0.5,29.7*0.5)
                end if
            end if
        end if
    end 
    subroutine obottoms(x,y)
        implicit none
        real,intent(in),optional::x,y

        call plotmove(0.,0.)
        if(land)then
            if(present(x))then
                if(present(y))then
                    call plotmove2(1.5+x,1.5+y)
                else
                    call plotmove2(1.5+x,1.5)
                end if
            else
                if(present(y))then
                    call plotmove2(1.5,1.5+y)
                else
                    call plotmove2(1.5,1.5)
                end if
            end if
        else
            if(present(x))then
                if(present(y))then
                    call plotmove2(1.5+x,1.5+y)
                else
                    call plotmove2(1.5+x,1.5)
                end if
            else 
                if(present(y))then
                    call plotmove2(1.5,1.5+y)
                else
                    call plotmove2(1.5,1.5)
                end if
            end if
        end if

    end
    subroutine inipage
            if(.not.pageend) return

            ipage=ipage+1
            write(ounit,'(a,2i3)') "%%Page:",ipage,ipage
            if(land) then 
                write(ounit,'(a)') "%%PageOrientation: Landscape"
            else
                write(ounit,'(a)') "%%PageOrientation: Portrait"
            endif
            write(ounit,'(a,2i2)') "%%BeginPageSetup:"
            write(ounit,'(a)') "PslNewPage"
            if(land) then 
                write(ounit,'(a)') " landscape"  
            endif
            write(ounit,'(a,2i2)') "%%EndPageSetup:"
            write(ounit,'(2f9.4,a)') xorig,yorig," tl" 
            write(ounit,'(a)') " np 0.0 0.0  mv" 
            pageend=.false.
            xn = xorig;yn = yorig
            if(topstat)call otops
            if(centerstat)call ocenter
            if(bottomstat)call obottoms
            return
    end      
    subroutine endpage     
        if(pageend) return
        write(ounit,'(a)') "PslEndPage"
        pageend=.true.
                    
        return
    end
    subroutine rgbK(red,gre,blu)     
        real red,gre,blu
        call plot(0.0,0.0,3)
        if(red.gt.1.0) stop 'argument error in rgb !!!'
        if(red.lt.0.0) stop 'argument error in rgb !!!'
        if(gre.gt.1.0) stop 'argument error in rgb !!!'
        if(gre.lt.0.0) stop 'argument error in rgb !!!'
        if(blu.gt.1.0) stop 'argument error in rgb !!!'
        if(blu.lt.0.0) stop 'argument error in rgb !!!'
        write(ounit,*) "% begin rgb " 
        write(ounit,'(3f9.4,a4)' ) red ,gre ,blu      ,' crg' 
        write(ounit,*) "% end rgb "
        return
    end
    subroutine betmlK(x,y,m,n,red,gre,blu) 
            dimension x(n),y(n)
            real red,gre,blu
            write(ounit,*) 'newpath'
            call rgbK(red,gre,blu) 
            ! call newpen2(1)
            call plot(x(1),y(1),3)
            do 10 i=2 , m 
            call plot(x(i),y(i),2)
        10      continue 
            call plot(x(1),y(1),2)
            write(ounit,*) 'closepath'
            write(ounit,*) 'fill'
            call color(0)
            return
    end
    subroutine color(ic)   
        integer    ic    
        real red,gre,blu
        write(ounit,*) "% begin color " ,ic
        red = 0.0
        gre = 0.0
        blu = 0.0
        if(ic.eq.1) red = 1.0 
        if(ic.eq.5) red = 1.0 
        if(ic.eq.6) red = 1.0 
        if(ic.eq.7) red = 1.0
        if(ic.eq.2) gre = 1.0
        if(ic.eq.4) gre = 1.0
        if(ic.eq.5) gre = 1.0
        if(ic.eq.7) gre = 1.0
        if(ic.eq.3) blu = 1.0 
        if(ic.eq.4) blu = 1.0 
        if(ic.eq.6) blu = 1.0 
        if(ic.eq.7) blu = 1.0
        write(ounit,'(3f9.4,a4)' ) red ,gre ,blu      ,' crg' 
        write(ounit,*) "% end color "
        return
    end
    subroutine newpen(ip)
        if((ip.ge.4).or.(ip.le.-4)) then
        write(6,*) '--< Attention >--'
        write(6,*) ' You have to re-write [newpen] --> [newpen2]'
        end if
        if((ip.ge.4).or.(ip.le.-4)) return
        write(ounit,*) "% begin newpen " ,ip
        write(ounit,*) "sn"
        if (ip.ge. 0) write(ounit,*) "[] 0 sd"
            if (ip.eq.1) write(ounit,*) ' 0.01  sl '
            if (ip.eq.2) write(ounit,*) ' 0.03  sl '
            if (ip.eq.3) write(ounit,*) ' 0.07  sl '
            if(ip.eq.-1) write(ounit,*) "[0.4 0.1] 0 sd"
            if(ip.eq.-2) write(ounit,*) "[0.2 0.1] 0 sd"
            if(ip.eq.-4) write(ounit,*) "[0.6 0.1] 0 sd"
            if(ip.eq.-3) write(ounit,*) "[0.1 0.2] 0 sd"
            if(ip.eq.-5) write(ounit,*) "[0.1 0.1] 0 sd"
        write(ounit,*) "% end newpen"
        return
    end
    subroutine newpen2(ip)
        write(ounit,*) "% begin newpen2 " ,ip
        write(ounit,*) "sn"
        if (ip.ge. 0) write(ounit,*) "[] 0 sd"
        if (ip.eq. 12) write(ounit,*) ' 0.20  sl '
        if (ip.eq. 11) write(ounit,*) ' 0.18  sl '
        if (ip.eq. 10) write(ounit,*) ' 0.16  sl '
        if (ip.eq. 9) write(ounit,*) ' 0.14  sl '
        if (ip.eq. 8) write(ounit,*) ' 0.12  sl '
        if (ip.eq. 7) write(ounit,*) ' 0.10  sl '
        if (ip.eq. 6) write(ounit,*) ' 0.08  sl '
        if (ip.eq. 5) write(ounit,*) ' 0.06  sl '
        if (ip.eq. 4) write(ounit,*) ' 0.04  sl '
        if (ip.eq. 3) write(ounit,*) ' 0.02  sl '
        if (ip.eq. 2) write(ounit,*) ' 0.01  sl '
        if (ip.eq. 1) write(ounit,*) ' 0.00  sl '
        if (ip.eq.-1) write(ounit,*) "[0.6 0.1] 0 sd"
        if (ip.eq.-2) write(ounit,*) "[0.4 0.1] 0 sd"
        if (ip.eq.-3) write(ounit,*) "[0.2 0.1] 0 sd"
        if (ip.eq.-4) write(ounit,*) "[0.2 0.2] 0 sd"
        if (ip.eq.-5) write(ounit,*) "[0.1 0.1] 0 sd"
        if (ip.eq.-6) write(ounit,*) "[0.05 0.05] 0 sd"
        if (ip.eq.-7) write(ounit,*) "[0.02 0.02] 0 sd"
        write(ounit,*) "% end newpen2"
        return
    end
    subroutine gmark(xp,yp,hi,markty)
        character*8 marka(0:13)
        real xp,yp,hi,hi2
        integer markty
        data marka / "        " , "dot     ",&
                    "plus    " , "star    ","circle  ","X       ", &
                    "tranf   ",  "tran    ","boxf    ","box     ", &
                    "circle2 ",  "        ","        ","        " /
        ! c     mark type 

        write(ounit,*) "% begin gmark "
        if(markty.eq.1.or.markty.eq.4)then 
        write(ounit,*)" sn "
        write(ounit,*) " gs "
        hi2 = hi/2.0
        write(ounit,' (3f12.6) ' ) xp,yp,hi2
        write(ounit,*) marka (markty)
        write(ounit,*) " st "
        write(ounit,*) " gr "
        ! c     update   93/07/10
        call plot(xp,yp,3)
        else  
        write(ounit,*)" sn "
        call plot(xp,yp,3)
        write(ounit,*) " gs "
        write(ounit,'(2f10.5,2x,a4)') hi ,hi, " sc "
        write(ounit,*) marka (markty)
        write(ounit,*) " st "
        write(ounit,*) " gr "
        endif
        write(ounit,*) "% end gmark "
        return
    end
    subroutine arohd(x0,y0,x1,y1,al,aw,ic)

            if(x0.eq.x1.and.y0.eq.y1) return

            write(ounit,*) "% begin arohd"
            xo=x0
            yo=y0
            xd=x1
            yd=y1

            i=ic/10
            j=mod(ic,10)

            dx=xd-xo
            dy=yd-yo
            r0=sqrt(xo*xo+yo*yo)
            dr=sqrt(dx*dx+dy*dy)

            pi=acos(-1.)
            theta=atan2(dy,dx)/pi*180.
        
            write(ounit, '(a)') " sn"
            if(i.eq.0) then
                write(ounit, '(2(f9.4,2x),a3)')  xd,yd," mv"
            else
                write(ounit, '(2(f9.4,2x),a3)')  xo ,yo," mv"
                write(ounit, '(2(f9.4,2x),a3)')  xd,yd," ln"
            endif

        10   write(ounit, '(2(f9.4,2x),a3)')  xd,yd," tl"
            write(ounit, '(f9.4,a3)')  theta," ro"
            write(ounit, '(a)') " sn 0.0 0.0 mv" 

            if(j.eq.7) then
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," mv"      
                write(ounit, '(2(f9.4,2x),a3)') -al,-aw," ln"      
                write(ounit, '(2(f9.4,2x),a3)') -al, 0.," ln"      
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," ln"      
                write(ounit, *) " gs" 
                write(ounit,'(f9.4,a)') 0.0, " sg fill"
                write(ounit, *) " gr st"
            elseif(j.eq.6) then
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," mv"      
                write(ounit, '(2(f9.4,2x),a3)') -al, aw," ln"      
                write(ounit, '(2(f9.4,2x),a3)') -al,-aw," ln"      
                write(ounit, *) " gs" 
                write(ounit,'(f9.4,a)') 0.0, " sg fill"
                write(ounit, *) " gr st"
            elseif(j.eq.5) then
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," mv"      
                write(ounit, '(2(f9.4,2x),a3)') -al,-aw," ln"      
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," mv"      
                write(ounit, '(2(f9.4,2x),a3)') -al, 0.," ln"      
            elseif(j.eq.4) then
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," mv"      
                write(ounit, '(2(f9.4,2x),a3)') -al,-aw," ln"      
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," mv"      
                write(ounit, '(2(f9.4,2x),a3)') -al, aw," ln"      
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," mv"      
                write(ounit, '(2(f9.4,2x),a3)') -al, 0.," ln"      
            elseif(j.eq.3) then
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," mv"      
                write(ounit, '(2(f9.4,2x),a3)') -al,-aw," ln"      
                write(ounit, '(2(f9.4,2x),a3)') -al, 0.," ln"
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," ln"            
            elseif(j.eq.2) then
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," mv"      
                write(ounit, '(2(f9.4,2x),a3)') -al, aw," ln"      
                write(ounit, '(2(f9.4,2x),a3)') -al,-aw," ln"      
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," ln"      
                write(ounit, '(2(f9.4,2x),a3)') -al, 0.," ln"      
            else
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," mv"      
                write(ounit, '(2(f9.4,2x),a3)') -al, aw," ln"      
                write(ounit, '(2(f9.4,2x),a3)') -al,-aw," ln"      
                write(ounit, '(2(f9.4,2x),a3)')  0., 0.," ln"      
                write(ounit, *) " gs" 
                write(ounit,'(f9.4,a)') 1.0, " sg fill"
                write(ounit, *) " gr st"
            endif
        
            write(ounit, '(f9.4,a3)') -theta," ro"
            write(ounit, '(2(f9.4,2x),a3)') -xd,-yd," tl"
        
            if(i.ge.2) then
                xo=x1
                yo=y1
                xd=x0
                yd=y0
                i=1
                theta=theta+180
                go to 10
            endif
            write(ounit,*) "% end arohd"
            return
    end
    ! types = 1~7 
    subroutine arrow(x0,y0,x1,y1,width,line_thickness,arrowtype)
        implicit none
        real,intent(in)::x0,y0,x1,y1
        real,intent(in),optional::width
        integer,intent(in),optional::arrowtype,line_thickness
        real:: xl,yl,width_local
        integer::type_local
        
        if(present(width))then
            width_local = width
        else
            width_local = sqrt((x1-x0)**2+(y1-y0)**2)/10.
        end if
        if(present(line_thickness))then 
            call newpen2(line_thickness)
        else
            call newpen2(3)
        end if
        if(present(arrowtype))then
            type_local = arrowtype
        else
            type_local = 4
        end if
        xl = x0+real(x1-x0)/4.*3.
        yl = y0+real(y1-y0)/4.*3.

        call gmark(x0,y0,sqrt((x1-x0)**2+(y1-y0)**2)/1000.,1)
        call plot(x0,y0,3)
        call plot(xl,yl,2)
        call arohd(xl,yl,x1,y1,sqrt((x1-xl)**2+(y1-yl)**2),width_local,type_local)

        return
    end 
    subroutine factor(fct)
        write(ounit, '(2f9.4,2x,a4)') fct,fct , " sc "
        return
    end 
    subroutine number(x,y,h,anu,ang,n)
        character isymb*16,form*16
        ! c
        write(ounit,*) "% begin number"
        zero= 0.0
        one = 1.0
        ten =10.0
        if(n.lt.0) then
            np=0
            nd=int(ten)**abs(n+1)
            tmp=anu/nd
            if(tmp.lt.-one) then
        ! c           nf=log10(-tmp)+1
                pnf=log10(-tmp)
                nf = int(pnf)+1
                np=np+1
            elseif(tmp.lt.zero) then
                nf=1
                np=np+1
            elseif(tmp.lt.one) then
                nf=1
                np=np
            else
        ! c           nf=log10(tmp)+1
                pnf=log10(tmp)
                nf = int(pnf)+1
            endif
            nw=nf+np
            write(form,'(a2,i2,a1)') '(i',nw,')'
            itmp=int(tmp)
            write(isymb,form) itmp
        else
            np=1
            tmp=anu
            if(tmp.lt.-one) then
        ! c           nf=log10(-tmp)+1
                pnf=log10(-tmp)
                nf = int(pnf)+1
                np=np+1
            elseif(tmp.lt.zero) then
                nf=1
                np=np+1
            elseif(tmp.lt.one) then
                nf=1
                np=np
            else
        ! c           nf=log10(tmp)+1
                pnf=log10(tmp)
                nf = int(pnf)+1
                np=np
            endif
            nw=nf+np+n
            write(form,'(a2,i2,a1,i2,a1)') '(f',nw,'.',n,')'
            write(isymb,form) tmp
        endif

        write(ounit,*) "fo"
        write(ounit,10) h
        10   format(f8.4," sf")
        write(ounit,*) "se"
        if(x.ge.9999.0.or.y.ge.9999.0)then
        else
            write(ounit,'(2f10.4,2x,a3)' ) x,y, " mv" 
        endif 
        write(ounit,'(f10.4,2x,a4)' ) ang , ' ro ' 
        write(ounit,*) "(",isymb(1:nw),") sh" 
        write(ounit,'(f9.4,2x,a4)' ) -ang , ' ro ' 
        write(ounit,*) "% end number"
        return
    end
    subroutine numberc(x,y,h,anu,ang,n)
        character isymb*16,form*16
        ! c
        write(ounit,*) "% begin numberc"
        zero= 0.0
        one = 1.0
        ten =10.0
        if(n.lt.0) then
            np=0
            nd=int(ten)**abs(n+1)
            tmp=anu/nd
            if(tmp.lt.-one) then
        ! c           nf=log10(-tmp)+1
                pnf=log10(-tmp)
                nf = int(pnf)+1
                np=np+1
            elseif(tmp.lt.zero) then
                nf=1
                np=np+1
            elseif(tmp.lt.one) then
                nf=1
                np=np
            else
        ! c           nf=log10(tmp)+1
                pnf=log10(tmp)
                nf = int(pnf)+1
            endif
            nw=nf+np
            write(form,'(a2,i2,a1)') '(i',nw,')'
            itmp=int(tmp)
            write(isymb,form) itmp
        else
            np=1
            tmp=anu
            if(tmp.lt.-one) then
        ! c           nf=log10(-tmp)+1
                pnf=log10(-tmp)
                nf = int(pnf)+1
                np=np+1
            elseif(tmp.lt.zero) then
                nf=1
                np=np+1
            elseif(tmp.lt.one) then
                nf=1
                np=np
            else
        ! c           nf=log10(tmp)+1
                pnf=log10(tmp)
                nf = int(pnf)+1
                np=np
            endif
            nw=nf+np+n
            write(form,'(a2,i2,a1,i2,a1)') '(f',nw,'.',n,')'
            write(isymb,form) tmp
        endif

        write(ounit,*) "fo"
        write(ounit,10) h
        10   format(f8.4," sf")
        write(ounit,*) "se"
        if(x.ge.9999.0.or.y.ge.9999.0)then
        else
            write(ounit,'(2f10.4,2x,a3)' ) x,y, " mv" 
        endif 
        write(ounit,'(f10.4,2x,a4)' ) ang , ' ro '
        write(ounit,*) "(",isymb(1:nw),") stringwidth pop " 
        write(ounit,*) 'neg 2 div 0 rmoveto '
        write(ounit,*) "(",isymb(1:nw),") sh" 
        write(ounit,'(f9.4,2x,a4)' ) -ang , ' ro ' 
        write(ounit,*) "% end numberc"
        return
    end
    subroutine numberr(x,y,h,anu,ang,n)
        character isymb*16,form*16
        ! c
        write(ounit,*) "% begin numberr"
        zero= 0.0
        one = 1.0
        ten =10.0
        if(n.lt.0) then
            np=0
            nd=int(ten)**abs(n+1)
            tmp=anu/nd
            if(tmp.lt.-one) then
        ! c           nf=log10(-tmp)+1
                pnf=log10(-tmp)
                nf = int(pnf)+1
                np=np+1
            elseif(tmp.lt.zero) then
                nf=1
                np=np+1
            elseif(tmp.lt.one) then
                nf=1
                np=np
            else
        ! c           nf=log10(tmp)+1
                pnf=log10(tmp)
                nf = int(pnf)+1
            endif
            nw=nf+np
            write(form,'(a2,i2,a1)') '(i',nw,')'
            itmp=int(tmp)
            write(isymb,form) itmp
        else
            np=1
            tmp=anu
            if(tmp.lt.-one) then
        ! c           nf=log10(-tmp)+1
                pnf=log10(-tmp)
                nf = int(pnf)+1
                np=np+1
            elseif(tmp.lt.zero) then
                nf=1
                np=np+1
            elseif(tmp.lt.one) then
                nf=1
                np=np
            else
        ! c           nf=log10(tmp)+1
                pnf=log10(tmp)
                nf = int(pnf)+1
                np=np
            endif
            nw=nf+np+n
            write(form,'(a2,i2,a1,i2,a1)') '(f',nw,'.',n,')'
            write(isymb,form) tmp
        endif

        write(ounit,*) "fo"
        write(ounit,10) h
        10   format(f8.4," sf")
        write(ounit,*) "se"
        if(x.ge.9999.0.or.y.ge.9999.0)then
        else
            write(ounit,'(2f10.4,2x,a3)' ) x,y, " mv" 
        endif 
        write(ounit,'(f10.4,2x,a4)' ) ang , ' ro ' 
        write(ounit,*) "(",isymb(1:nw),") stringwidth pop " 
        write(ounit,*) 'neg 0 rmoveto '
        write(ounit,*) "(",isymb(1:nw),") sh" 
        write(ounit,'(f9.4,2x,a4)' ) -ang , ' ro ' 
        write(ounit,*) "% end numberr"
        return
    end
    subroutine pscont3(dx,dy,a,ib,ips,ipe,jps,jpe &
                                    ,mx,my,icnu,contst,contint)
        dimension conta(1000),a(mx,my),ib(mx,my),x(5),y(5)
        ! real,dimension
        !-------------------schematic of contour-----------------------
        !---> x direction is (i)
        ! c   |  ---      a(i,j)-----x(1),y(1)-----a(i+1,j)
        ! c   |   |         |                         |
        ! c   |   |         |                         |
        ! c   |   |         |                         |
        ! c   y   dy    x(2),y(2)                 x(4),y(4)
        ! c       |         |                         |
        ! c   i   |         |                         |
        ! c   s   |         |                         |
        ! c      ---     a(i,j+1)----x(3),y(3)-----a(i+1,j+1)
        ! c  (j)            |<-----------dx---------->|
        ! c
        ! c       | 
        ! c       |<-yycop
        ! c       |____xxcop_
        ! c
        ! c********correct of x and y axis**********
        !-----------------------
            pxm=-dx*float(ips-1)
            pym=-dy*float(jps-1)
        !-----------------------
            xxcop= 0.5*dx + pxm
            yycop= 0.5*dy + pym
        ! c***********************
        !------ The positions of contour line are 1~4x,y 
        ! c            The contour lines are drawn by these position
        !---------------------------------------
        !------setting the value of contour line
        do 200 i=1,icnu
        conta(i)=contst+float(i-1)*contint
        200  continue

            x5=0.
            y5=0.
            x4=0.
            y4=0.
        ! c
            xdx1=0.
            xdx2=0.
            xdx3=0.
            xdx4=0.
            ydy1=0.
            ydy2=0.
            ydy3=0.
            ydy4=0.
        !---
            do kk=1,4
            x(kk)=0.
            y(kk)=0.
            enddo
        ! c<<<<<<<<<<<<<<<<<<<<<<<<start>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        !------------cont.        
        do 300 k=1,icnu
        !------------y axis        
        do 280 j=jps,jpe-1
        !------------x axis        
        do 270 i=ips,ipe-1
            if ((ib(i,j).eq.0).or.(ib(i+1,j).eq.0) &
                                .or.(ib(i,j+1).eq.0).or.(ib(i+1,j+1).eq.0)) &
                go to 270
        ! !------------compare value of conta. to value of a(i,j)
            if ((a(i,j).eq.conta(k)).and.(a(i+1,j).eq.conta(k)) &
            .and.(a(i,j+1).eq.conta(k)).and.(a(i+1,j+1).eq.conta(k))) then
            go to 270
            end if
            if ((a(i,j).lt.conta(k)).and.(a(i+1,j).lt.conta(k)) &
            .and.(a(i,j+1).lt.conta(k)).and.(a(i+1,j+1).lt.conta(k))) then

            go to 270
            end if
        ! c
            if (a(i,j).eq.conta(k)) then
            a( i , j )=a( i , j )+0.001*contint
            end if
        ! c
            if (a(i+1,j).eq.conta(k)) then
            a(i+1, j )=a(i+1, j )+0.001*contint
            end if
        ! c
            if (a(i,j+1).eq.conta(k)) then
            a( i ,j+1)=a( i ,j+1)+0.001*contint
            end if
            if (a(i+1,j+1).eq.conta(k)) then
            a(i+1,j+1)=a(i+1,j+1)+0.001*contint
            end if
        ! c
        ! c==========search position (1)============
            dex1=a(i,j)-conta(k)
            dex2=a(i+1,j)-conta(k)
            dex3=dex1*dex2
            if (dex3.le.0.) then
            go to 211
            else
            go to 212
            end if
        !-----------
        211     dex4=a(i,j)-a(i+1,j)
            if (dex4.eq.0.) then
            go to 212
            end if

        ! !----------judgment of land and sea
            jnd1=ib(i,j)*ib(i+1,j)
            if (jnd1 .eq. 0) then
            go to 212
            end if
        
        ! c*********position (1)
            x(1)=dx*float(i-1)+abs(dex1)*dx/abs(dex4)+xxcop
            y(1)=dy*float((j-1))+yycop

        !         write(*,*) 'x(1),y(1)',x(1),y(1)
        ! c==========search position (2)================
        212     dex1=a(i,j)-conta(k)
            dex2=a(i,j+1)-conta(k)
            dex3=dex1*dex2
            if (dex3.le.0.) then
            go to 213
            else
            go to 214
            end if
        !-----------
        213     dex4=a(i,j)-a(i,j+1)
            if (dex4.eq.0.) then
            go to 214
            else
            end if

        ! !----------judgment of land and sea
            jnd2=ib(i,j)*ib(i,j+1)
            if (jnd2 .eq. 0) then
            go to 214
            end if
        ! c
        ! c**********position (2)
                x(2)=dx*float(i-1)+xxcop
                y(2)=dy*float(j-1)+abs(dex1)*dy/abs(dex4) &
                    +yycop 

        ! !         write(*,*) 'x(2),y(2)',x(2),y(2)
        ! c==========search position (3)================
        214    dex1=a(i,j+1)-conta(k)
            dex2=a(i+1,j+1)-conta(k)
            dex3=dex1*dex2
            if (dex3.le.0.) then
            go to 215
            else
            go to 216
            end if
        !-----------
        215    dex4=a(i,j+1)-a(i+1,j+1)
            if (dex4.eq.0.) then
            go to 216
            end if
        ! c
        ! !----------judgment of land and sea
            jnd3=ib(i,j+1)*ib(i+1,j+1)
            if (jnd3 .eq. 0) then
            go to 216
            end if
        ! c
        ! c*********position (3)
            x(3)=dx*float(i-1)+abs(dex1)*dx/abs(dex4)+xxcop
            y(3)=dy*float((j))+yycop

        !         write(*,*) 'x(3),y(3)',x(3),y(3)
        ! c==========search position (4)================
        216     dex1=a(i+1,j)-conta(k)
            dex2=a(i+1,j+1)-conta(k)
            dex3=dex1*dex2
            if (dex3.le.0.) then
            go to 217
            else
            go to 240
            end if
        !-----------
        217     dex4=a(i+1,j)-a(i+1,j+1)
            if (dex4.eq.0.) then
            go to 230
            end if
        ! c
        ! !----------judgment of land and sea
            jnd4=ib(i+1,j)*ib(i+1,j+1)
            if (jnd4 .eq. 0) then
            go to 240
            end if
        ! c
        ! c**********position (4)
                x(4)=dx*float(i)+xxcop
                y(4)=dy*float(j-1)+abs(dex1)*dy/abs(dex4) &
                    +yycop
        !         write(*,*) 'x(4),y(4)',x(4),y(4)
        ! c#########################################################
        ! c===========drawing contour line==========================
        ! !-----------case of four positions 
        230    if ((x(1).ne.0.).and.(x(2).ne.0.).and.(x(3).ne.0.).and. &
                (x(4).ne.0.)) then

            go to 235
            else
            go to 240
            end if
        ! c
        235     xlong1=(x(1)-x(2))**2+(y(1)-y(2))**2
            xlong2=(x(1)-x(4))**2+(y(1)-y(4))**2
                if (xlong1.le.xlong2) then
                go to 236
                else
                goto 237
                end if
        ! c
        236   call plot(x(1),y(1),3)
            call plot(x(2),y(2),2)
            call plot(x(3),y(3),3)
            call plot(x(4),y(4),2)
        
            

        !       write(*,*) 1,i,j,x(1),y(1)
        !       write(*,*) 1,i,j,x(2),y(2)
        !       write(*,*) 1,i,j,x(3),y(3)
        !       write(*,*) 1,i,j,x(4),y(4)
        ! C       stop 

            go to 250
        
        237   call plot(x(1),y(1),3)
            call plot(x(4),y(4),2)
            call plot(x(2),y(2),3)
            call plot(x(3),y(3),2)

        !       write(*,*) 2,i,j,x(1),y(1)
        !       write(*,*) 2,i,j,x(2),y(2)
        !       write(*,*) 2,i,j,x(3),y(3)
        !       write(*,*) 2,i,j,x(4),y(4)
        ! C       stop 

            go to 250
        ! c
        ! !-----------else case /  
        240   do 245 kk=1,4
        !        if(kk.eq.1) write(*,*)0,(x(jm),y(jm),jm=1,4) 
                    if (x(kk).eq.0.) then
        !               write(*,*) 'x(kk)',kk,x(kk)
                    go to 245
                    end if
                    x4=x(kk)
                    y4=y(kk)
        !       write(*,*) 'U',kk,x4,y4

                    if (x5.eq.0.) then
                    go to 238
                    end if

            call plot(x4,y4,3)
            call plot(x5,y5,2)

        !        write(*,*) 3,i,j,x4,y4
        !        write(*,*) 3,i,j,x5,y5


        238          x5=x4
                    y5=y4
        !       write(*,*) 'L',kk,x4,y4,x5,y5
                    x(kk)=0.
        245    continue
        250     x5=0.
            y5=0.
            x4=0.
            y4=0.

            xdx1=0.
            xdx2=0.
            xdx3=0.
            xdx4=0.
            ydy1=0.
            ydy2=0.
            ydy3=0.
            ydy4=0.
        !---
            do 260 kk=1,4
            x(kk)=0.
            y(kk)=0.
        260    continue
        !---

        270    continue
        280    continue
        300    continue
        ! c<<<<<<<<<<<<<<<<<<<<<<<<end>>>>>>>>>>>>>>>>>>>>>>>>>>>>
            return
    end
    ! draws dotted line (skips every other point)
    subroutine pscont4(dx,dy,a,ib,ips,ipe,jps,jpe &
        ,mx,my,icnu,contst,contint)
        dimension conta(1000),a(mx,my),ib(mx,my),x(5),y(5)
        ! real,dimension
        !-------------------schematic of contour-----------------------
        !---> x direction is (i)
        ! c   |  ---      a(i,j)-----x(1),y(1)-----a(i+1,j)
        ! c   |   |         |                         |
        ! c   |   |         |                         |
        ! c   |   |         |                         |
        ! c   y   dy    x(2),y(2)                 x(4),y(4)
        ! c       |         |                         |
        ! c   i   |         |                         |
        ! c   s   |         |                         |
        ! c      ---     a(i,j+1)----x(3),y(3)-----a(i+1,j+1)
        ! c  (j)            |<-----------dx---------->|
        ! c
        ! c       | 
        ! c       |<-yycop
        ! c       |____xxcop_
        ! c
        ! c********correct of x and y axis**********
        !-----------------------
        pxm=-dx*float(ips-1)
        pym=-dy*float(jps-1)
        !-----------------------
        xxcop= 0.5*dx + pxm
        yycop= 0.5*dy + pym
        ! c***********************
        !------ The positions of contour line are 1~4x,y 
        ! c            The contour lines are drawn by these position
        !---------------------------------------
        !------setting the value of contour line
        do 200 i=1,icnu
        conta(i)=contst+float(i-1)*contint
        200  continue

        x5=0.
        y5=0.
        x4=0.
        y4=0.
        ! c
        xdx1=0.
        xdx2=0.
        xdx3=0.
        xdx4=0.
        ydy1=0.
        ydy2=0.
        ydy3=0.
        ydy4=0.
        !---
        do kk=1,4
        x(kk)=0.
        y(kk)=0.
        enddo
        ! c<<<<<<<<<<<<<<<<<<<<<<<<start>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        !------------cont.        
        do 300 k=1,icnu
        !------------y axis        
        do 280 j=jps,jpe-1
        !------------x axis        
        do 270 i=ips,ipe-1
        if ((ib(i,j).eq.0).or.(ib(i+1,j).eq.0) &
            .or.(ib(i,j+1).eq.0).or.(ib(i+1,j+1).eq.0)) &
        go to 270
        ! !------------compare value of conta. to value of a(i,j)
        if ((a(i,j).eq.conta(k)).and.(a(i+1,j).eq.conta(k)) &
        .and.(a(i,j+1).eq.conta(k)).and.(a(i+1,j+1).eq.conta(k))) then
        go to 270
        end if
        if ((a(i,j).lt.conta(k)).and.(a(i+1,j).lt.conta(k)) &
        .and.(a(i,j+1).lt.conta(k)).and.(a(i+1,j+1).lt.conta(k))) then

        go to 270
        end if
        ! c
        if (a(i,j).eq.conta(k)) then
        a( i , j )=a( i , j )+0.001*contint
        end if
        ! c
        if (a(i+1,j).eq.conta(k)) then
        a(i+1, j )=a(i+1, j )+0.001*contint
        end if
        ! c
        if (a(i,j+1).eq.conta(k)) then
        a( i ,j+1)=a( i ,j+1)+0.001*contint
        end if
        if (a(i+1,j+1).eq.conta(k)) then
        a(i+1,j+1)=a(i+1,j+1)+0.001*contint
        end if
        ! c
        ! c==========search position (1)============
        dex1=a(i,j)-conta(k)
        dex2=a(i+1,j)-conta(k)
        dex3=dex1*dex2
        if (dex3.le.0.) then
        go to 211
        else
        go to 212
        end if
        !-----------
        211     dex4=a(i,j)-a(i+1,j)
        if (dex4.eq.0.) then
        go to 212
        end if

        ! !----------judgment of land and sea
        jnd1=ib(i,j)*ib(i+1,j)
        if (jnd1 .eq. 0) then
        go to 212
        end if

        ! c*********position (1)
        x(1)=dx*float(i-1)+abs(dex1)*dx/abs(dex4)+xxcop
        y(1)=dy*float((j-1))+yycop

        !         write(*,*) 'x(1),y(1)',x(1),y(1)
        ! c==========search position (2)================
        212     dex1=a(i,j)-conta(k)
        dex2=a(i,j+1)-conta(k)
        dex3=dex1*dex2
        if (dex3.le.0.) then
        go to 213
        else
        go to 214
        end if
        !-----------
        213     dex4=a(i,j)-a(i,j+1)
        if (dex4.eq.0.) then
        go to 214
        else
        end if

        ! !----------judgment of land and sea
        jnd2=ib(i,j)*ib(i,j+1)
        if (jnd2 .eq. 0) then
        go to 214
        end if
        ! c
        ! c**********position (2)
        x(2)=dx*float(i-1)+xxcop
        y(2)=dy*float(j-1)+abs(dex1)*dy/abs(dex4) &
        +yycop 

        ! !         write(*,*) 'x(2),y(2)',x(2),y(2)
        ! c==========search position (3)================
        214    dex1=a(i,j+1)-conta(k)
        dex2=a(i+1,j+1)-conta(k)
        dex3=dex1*dex2
        if (dex3.le.0.) then
        go to 215
        else
        go to 216
        end if
        !-----------
        215    dex4=a(i,j+1)-a(i+1,j+1)
        if (dex4.eq.0.) then
        go to 216
        end if
        ! c
        ! !----------judgment of land and sea
        jnd3=ib(i,j+1)*ib(i+1,j+1)
        if (jnd3 .eq. 0) then
        go to 216
        end if
        ! c
        ! c*********position (3)
        x(3)=dx*float(i-1)+abs(dex1)*dx/abs(dex4)+xxcop
        y(3)=dy*float((j))+yycop

        !         write(*,*) 'x(3),y(3)',x(3),y(3)
        ! c==========search position (4)================
        216     dex1=a(i+1,j)-conta(k)
        dex2=a(i+1,j+1)-conta(k)
        dex3=dex1*dex2
        if (dex3.le.0.) then
        go to 217
        else
        go to 240
        end if
        !-----------
        217     dex4=a(i+1,j)-a(i+1,j+1)
        if (dex4.eq.0.) then
        go to 230
        end if
        ! c
        ! !----------judgment of land and sea
        jnd4=ib(i+1,j)*ib(i+1,j+1)
        if (jnd4 .eq. 0) then
        go to 240
        end if
        ! c
        ! c**********position (4)
        x(4)=dx*float(i)+xxcop
        y(4)=dy*float(j-1)+abs(dex1)*dy/abs(dex4) &
        +yycop
        !         write(*,*) 'x(4),y(4)',x(4),y(4)
        ! c#########################################################
        ! c===========drawing contour line==========================
        ! !-----------case of four positions 
        230    if ((x(1).ne.0.).and.(x(2).ne.0.).and.(x(3).ne.0.).and. &
        (x(4).ne.0.)) then

        go to 235
        else
        go to 240
        end if
        ! c
        235     xlong1=(x(1)-x(2))**2+(y(1)-y(2))**2
        xlong2=(x(1)-x(4))**2+(y(1)-y(4))**2
        if (xlong1.le.xlong2) then
        go to 236
        else
        goto 237
        end if
        ! c
        236  if(mod(j,2).eq.0) then
        call plot(x(1),y(1),3)
        call plot(x(2),y(2),2)
        call plot(x(3),y(3),3)
        call plot(x(4),y(4),2)
        end if


        !       write(*,*) 1,i,j,x(1),y(1)
        !       write(*,*) 1,i,j,x(2),y(2)
        !       write(*,*) 1,i,j,x(3),y(3)
        !       write(*,*) 1,i,j,x(4),y(4)
        ! C       stop 

        go to 250

        237 if(mod(j,2).eq.0) then
        call plot(x(1),y(1),3)
        call plot(x(4),y(4),2)
        call plot(x(2),y(2),3)
        call plot(x(3),y(3),2)
        end if
        !       write(*,*) 2,i,j,x(1),y(1)
        !       write(*,*) 2,i,j,x(2),y(2)
        !       write(*,*) 2,i,j,x(3),y(3)
        !       write(*,*) 2,i,j,x(4),y(4)
        ! C       stop 

        go to 250
        ! c
        ! !-----------else case /  
        240   do 245 kk=1,4
        !        if(kk.eq.1) write(*,*)0,(x(jm),y(jm),jm=1,4) 
        if (x(kk).eq.0.) then
        !               write(*,*) 'x(kk)',kk,x(kk)
        go to 245
        end if
        x4=x(kk)
        y4=y(kk)
        !       write(*,*) 'U',kk,x4,y4

        if (x5.eq.0.) then
        go to 238
        end if

        if(mod(j,2).eq.0) then
        call plot(x4,y4,3)
        call plot(x5,y5,2)
        end if
        !        write(*,*) 3,i,j,x4,y4
        !        write(*,*) 3,i,j,x5,y5


        238          x5=x4
        y5=y4
        !       write(*,*) 'L',kk,x4,y4,x5,y5
        x(kk)=0.
        245    continue
        250     x5=0.
        y5=0.
        x4=0.
        y4=0.

        xdx1=0.
        xdx2=0.
        xdx3=0.
        xdx4=0.
        ydy1=0.
        ydy2=0.
        ydy3=0.
        ydy4=0.
        !---
        do 260 kk=1,4
        x(kk)=0.
        y(kk)=0.
        260    continue
        !---

        270    continue
        280    continue
        300    continue
        ! c<<<<<<<<<<<<<<<<<<<<<<<<end>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        return
    end
    ! draws dotted line (skips point every 3 times)
    subroutine pscont5(dx,dy,a,ib,ips,ipe,jps,jpe &
        ,mx,my,icnu,contst,contint)
        dimension conta(1000),a(mx,my),ib(mx,my),x(5),y(5)
        ! real,dimension
        !-------------------schematic of contour-----------------------
        !---> x direction is (i)
        ! c   |  ---      a(i,j)-----x(1),y(1)-----a(i+1,j)
        ! c   |   |         |                         |
        ! c   |   |         |                         |
        ! c   |   |         |                         |
        ! c   y   dy    x(2),y(2)                 x(4),y(4)
        ! c       |         |                         |
        ! c   i   |         |                         |
        ! c   s   |         |                         |
        ! c      ---     a(i,j+1)----x(3),y(3)-----a(i+1,j+1)
        ! c  (j)            |<-----------dx---------->|
        ! c
        ! c       | 
        ! c       |<-yycop
        ! c       |____xxcop_
        ! c
        ! c********correct of x and y axis**********
        !-----------------------
        pxm=-dx*float(ips-1)
        pym=-dy*float(jps-1)
        !-----------------------
        xxcop= 0.5*dx + pxm
        yycop= 0.5*dy + pym
        ! c***********************
        !------ The positions of contour line are 1~4x,y 
        ! c            The contour lines are drawn by these position
        !---------------------------------------
        !------setting the value of contour line
        do 200 i=1,icnu
        conta(i)=contst+float(i-1)*contint
        200  continue

        x5=0.
        y5=0.
        x4=0.
        y4=0.
        ! c
        xdx1=0.
        xdx2=0.
        xdx3=0.
        xdx4=0.
        ydy1=0.
        ydy2=0.
        ydy3=0.
        ydy4=0.
        !---
        do kk=1,4
        x(kk)=0.
        y(kk)=0.
        enddo
        ! c<<<<<<<<<<<<<<<<<<<<<<<<start>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        !------------cont.        
        do 300 k=1,icnu
        !------------y axis        
        do 280 j=jps,jpe-1
        !------------x axis        
        do 270 i=ips,ipe-1
        if ((ib(i,j).eq.0).or.(ib(i+1,j).eq.0) &
            .or.(ib(i,j+1).eq.0).or.(ib(i+1,j+1).eq.0)) &
        go to 270
        ! !------------compare value of conta. to value of a(i,j)
        if ((a(i,j).eq.conta(k)).and.(a(i+1,j).eq.conta(k)) &
        .and.(a(i,j+1).eq.conta(k)).and.(a(i+1,j+1).eq.conta(k))) then
        go to 270
        end if
        if ((a(i,j).lt.conta(k)).and.(a(i+1,j).lt.conta(k)) &
        .and.(a(i,j+1).lt.conta(k)).and.(a(i+1,j+1).lt.conta(k))) then

        go to 270
        end if
        ! c
        if (a(i,j).eq.conta(k)) then
        a( i , j )=a( i , j )+0.001*contint
        end if
        ! c
        if (a(i+1,j).eq.conta(k)) then
        a(i+1, j )=a(i+1, j )+0.001*contint
        end if
        ! c
        if (a(i,j+1).eq.conta(k)) then
        a( i ,j+1)=a( i ,j+1)+0.001*contint
        end if
        if (a(i+1,j+1).eq.conta(k)) then
        a(i+1,j+1)=a(i+1,j+1)+0.001*contint
        end if
        ! c
        ! c==========search position (1)============
        dex1=a(i,j)-conta(k)
        dex2=a(i+1,j)-conta(k)
        dex3=dex1*dex2
        if (dex3.le.0.) then
        go to 211
        else
        go to 212
        end if
        !-----------
        211     dex4=a(i,j)-a(i+1,j)
        if (dex4.eq.0.) then
        go to 212
        end if

        ! !----------judgment of land and sea
        jnd1=ib(i,j)*ib(i+1,j)
        if (jnd1 .eq. 0) then
        go to 212
        end if

        ! c*********position (1)
        x(1)=dx*float(i-1)+abs(dex1)*dx/abs(dex4)+xxcop
        y(1)=dy*float((j-1))+yycop

        !         write(*,*) 'x(1),y(1)',x(1),y(1)
        ! c==========search position (2)================
        212     dex1=a(i,j)-conta(k)
        dex2=a(i,j+1)-conta(k)
        dex3=dex1*dex2
        if (dex3.le.0.) then
        go to 213
        else
        go to 214
        end if
        !-----------
        213     dex4=a(i,j)-a(i,j+1)
        if (dex4.eq.0.) then
        go to 214
        else
        end if

        ! !----------judgment of land and sea
        jnd2=ib(i,j)*ib(i,j+1)
        if (jnd2 .eq. 0) then
        go to 214
        end if
        ! c
        ! c**********position (2)
        x(2)=dx*float(i-1)+xxcop
        y(2)=dy*float(j-1)+abs(dex1)*dy/abs(dex4) &
        +yycop 

        ! !         write(*,*) 'x(2),y(2)',x(2),y(2)
        ! c==========search position (3)================
        214    dex1=a(i,j+1)-conta(k)
        dex2=a(i+1,j+1)-conta(k)
        dex3=dex1*dex2
        if (dex3.le.0.) then
        go to 215
        else
        go to 216
        end if
        !-----------
        215    dex4=a(i,j+1)-a(i+1,j+1)
        if (dex4.eq.0.) then
        go to 216
        end if
        ! c
        ! !----------judgment of land and sea
        jnd3=ib(i,j+1)*ib(i+1,j+1)
        if (jnd3 .eq. 0) then
        go to 216
        end if
        ! c
        ! c*********position (3)
        x(3)=dx*float(i-1)+abs(dex1)*dx/abs(dex4)+xxcop
        y(3)=dy*float((j))+yycop

        !         write(*,*) 'x(3),y(3)',x(3),y(3)
        ! c==========search position (4)================
        216     dex1=a(i+1,j)-conta(k)
        dex2=a(i+1,j+1)-conta(k)
        dex3=dex1*dex2
        if (dex3.le.0.) then
        go to 217
        else
        go to 240
        end if
        !-----------
        217     dex4=a(i+1,j)-a(i+1,j+1)
        if (dex4.eq.0.) then
        go to 230
        end if
        ! c
        ! !----------judgment of land and sea
        jnd4=ib(i+1,j)*ib(i+1,j+1)
        if (jnd4 .eq. 0) then
        go to 240
        end if
        ! c
        ! c**********position (4)
        x(4)=dx*float(i)+xxcop
        y(4)=dy*float(j-1)+abs(dex1)*dy/abs(dex4) &
        +yycop
        !         write(*,*) 'x(4),y(4)',x(4),y(4)
        ! c#########################################################
        ! c===========drawing contour line==========================
        ! !-----------case of four positions 
        230    if ((x(1).ne.0.).and.(x(2).ne.0.).and.(x(3).ne.0.).and. &
        (x(4).ne.0.)) then

        go to 235
        else
        go to 240
        end if
        ! c
        235     xlong1=(x(1)-x(2))**2+(y(1)-y(2))**2
        xlong2=(x(1)-x(4))**2+(y(1)-y(4))**2
        if (xlong1.le.xlong2) then
        go to 236
        else
        goto 237
        end if
        ! c
        236  if(mod(j,3).eq.0) then
        call plot(x(1),y(1),3)
        call plot(x(2),y(2),2)
        call plot(x(3),y(3),3)
        call plot(x(4),y(4),2)
        end if


        !       write(*,*) 1,i,j,x(1),y(1)
        !       write(*,*) 1,i,j,x(2),y(2)
        !       write(*,*) 1,i,j,x(3),y(3)
        !       write(*,*) 1,i,j,x(4),y(4)
        ! C       stop 

        go to 250

        237 if(mod(j,3).eq.0) then
        call plot(x(1),y(1),3)
        call plot(x(4),y(4),2)
        call plot(x(2),y(2),3)
        call plot(x(3),y(3),2)
        end if
        !       write(*,*) 2,i,j,x(1),y(1)
        !       write(*,*) 2,i,j,x(2),y(2)
        !       write(*,*) 2,i,j,x(3),y(3)
        !       write(*,*) 2,i,j,x(4),y(4)
        ! C       stop 

        go to 250
        ! c
        ! !-----------else case /  
        240   do 245 kk=1,4
        !        if(kk.eq.1) write(*,*)0,(x(jm),y(jm),jm=1,4) 
        if (x(kk).eq.0.) then
        !               write(*,*) 'x(kk)',kk,x(kk)
        go to 245
        end if
        x4=x(kk)
        y4=y(kk)
        !       write(*,*) 'U',kk,x4,y4

        if (x5.eq.0.) then
        go to 238
        end if

        if(mod(j,3).eq.0) then
        call plot(x4,y4,3)
        call plot(x5,y5,2)
        end if
        !        write(*,*) 3,i,j,x4,y4
        !        write(*,*) 3,i,j,x5,y5


        238          x5=x4
        y5=y4
        !       write(*,*) 'L',kk,x4,y4,x5,y5
        x(kk)=0.
        245    continue
        250     x5=0.
        y5=0.
        x4=0.
        y4=0.

        xdx1=0.
        xdx2=0.
        xdx3=0.
        xdx4=0.
        ydy1=0.
        ydy2=0.
        ydy3=0.
        ydy4=0.
        !---
        do 260 kk=1,4
        x(kk)=0.
        y(kk)=0.
        260    continue
        !---

        270    continue
        280    continue
        300    continue
        ! c<<<<<<<<<<<<<<<<<<<<<<<<end>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        return
    end
    ! subroutine pscont4(dx,dy,a,ib,ips,ipe,jps,jpe &
    !     ,mx,my,icnu,contst,contint)
    !     dimension conta(1000),x(5),y(5)
    !     real::a(:,:)
    !     integer::ib(:,:)
        
    !     mx = size(a,1); my = size(a,2)
    !     ! print*, mx, my,'yo'
    !     ! real,dimension
    !     !-------------------schematic of contour-----------------------
    !     !---> x direction is (i)
    !     ! c   |  ---      a(i,j)-----x(1),y(1)-----a(i+1,j)
    !     ! c   |   |         |                         |
    !     ! c   |   |         |                         |
    !     ! c   |   |         |                         |
    !     ! c   y   dy    x(2),y(2)                 x(4),y(4)
    !     ! c       |         |                         |
    !     ! c   i   |         |                         |
    !     ! c   s   |         |                         |
    !     ! c      ---     a(i,j+1)----x(3),y(3)-----a(i+1,j+1)
    !     ! c  (j)            |<-----------dx---------->|
    !     ! c
    !     ! c       | 
    !     ! c       |<-yycop
    !     ! c       |____xxcop_
    !     ! c
    !     ! c********correct of x and y axis**********
    !     !-----------------------
    !     pxm=-dx*float(ips-1)
    !     pym=-dy*float(jps-1)
    !     !-----------------------
    !     xxcop= 0.5*dx + pxm
    !     yycop= 0.5*dy + pym
    !     ! c***********************
    !     !------ The positions of contour line are 1~4x,y 
    !     ! c            The contour lines are drawn by these position
    !     !---------------------------------------
    !     !------setting the value of contour line
    !     do 200 i=1,icnu
    !     conta(i)=contst+float(i-1)*contint
    !     200  continue

    !     x5=0.
    !     y5=0.
    !     x4=0.
    !     y4=0.
    !     ! c
    !     xdx1=0.
    !     xdx2=0.
    !     xdx3=0.
    !     xdx4=0.
    !     ydy1=0.
    !     ydy2=0.
    !     ydy3=0.
    !     ydy4=0.
    !     !---
    !     do kk=1,4
    !     x(kk)=0.
    !     y(kk)=0.
    !     enddo
    !     ! c<<<<<<<<<<<<<<<<<<<<<<<<start>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !     !------------cont.        
    !     do 300 k=1,icnu
    !     !------------y axis        
    !     do 280 j=jps,jpe-1
    !     !------------x axis        
    !     do 270 i=ips,ipe-1
    !     if ((ib(i,j).eq.0).or.(ib(i+1,j).eq.0) &
    !         .or.(ib(i,j+1).eq.0).or.(ib(i+1,j+1).eq.0)) &
    !     go to 270
    !     ! !------------compare value of conta. to value of a(i,j)
    !     if ((a(i,j).eq.conta(k)).and.(a(i+1,j).eq.conta(k)) &
    !     .and.(a(i,j+1).eq.conta(k)).and.(a(i+1,j+1).eq.conta(k))) then
    !     go to 270
    !     end if
    !     if ((a(i,j).lt.conta(k)).and.(a(i+1,j).lt.conta(k)) &
    !     .and.(a(i,j+1).lt.conta(k)).and.(a(i+1,j+1).lt.conta(k))) then

    !     go to 270
    !     end if
    !     ! c
    !     if (a(i,j).eq.conta(k)) then
    !     a( i , j )=a( i , j )+0.001*contint
    !     end if
    !     ! c
    !     if (a(i+1,j).eq.conta(k)) then
    !     a(i+1, j )=a(i+1, j )+0.001*contint
    !     end if
    !     ! c
    !     if (a(i,j+1).eq.conta(k)) then
    !     a( i ,j+1)=a( i ,j+1)+0.001*contint
    !     end if
    !     if (a(i+1,j+1).eq.conta(k)) then
    !     a(i+1,j+1)=a(i+1,j+1)+0.001*contint
    !     end if
    !     ! c
    !     ! c==========search position (1)============
    !     dex1=a(i,j)-conta(k)
    !     dex2=a(i+1,j)-conta(k)
    !     dex3=dex1*dex2
    !     if (dex3.le.0.) then
    !     go to 211
    !     else
    !     go to 212
    !     end if
    !     !-----------
    !     211     dex4=a(i,j)-a(i+1,j)
    !     if (dex4.eq.0.) then
    !     go to 212
    !     end if

    !     ! !----------judgment of land and sea
    !     jnd1=ib(i,j)*ib(i+1,j)
    !     if (jnd1 .eq. 0) then
    !     go to 212
    !     end if

    !     ! c*********position (1)
    !     x(1)=dx*float(i-1)+abs(dex1)*dx/abs(dex4)+xxcop
    !     y(1)=dy*float((j-1))+yycop

    !     !         write(*,*) 'x(1),y(1)',x(1),y(1)
    !     ! c==========search position (2)================
    !     212     dex1=a(i,j)-conta(k)
    !     dex2=a(i,j+1)-conta(k)
    !     dex3=dex1*dex2
    !     if (dex3.le.0.) then
    !     go to 213
    !     else
    !     go to 214
    !     end if
    !     !-----------
    !     213     dex4=a(i,j)-a(i,j+1)
    !     if (dex4.eq.0.) then
    !     go to 214
    !     else
    !     end if

    !     ! !----------judgment of land and sea
    !     jnd2=ib(i,j)*ib(i,j+1)
    !     if (jnd2 .eq. 0) then
    !     go to 214
    !     end if
    !     ! c
    !     ! c**********position (2)
    !     x(2)=dx*float(i-1)+xxcop
    !     y(2)=dy*float(j-1)+abs(dex1)*dy/abs(dex4) &
    !     +yycop 

    !     ! !         write(*,*) 'x(2),y(2)',x(2),y(2)
    !     ! c==========search position (3)================
    !     214    dex1=a(i,j+1)-conta(k)
    !     dex2=a(i+1,j+1)-conta(k)
    !     dex3=dex1*dex2
    !     if (dex3.le.0.) then
    !     go to 215
    !     else
    !     go to 216
    !     end if
    !     !-----------
    !     215    dex4=a(i,j+1)-a(i+1,j+1)
    !     if (dex4.eq.0.) then
    !     go to 216
    !     end if
    !     ! c
    !     ! !----------judgment of land and sea
    !     jnd3=ib(i,j+1)*ib(i+1,j+1)
    !     if (jnd3 .eq. 0) then
    !     go to 216
    !     end if
    !     ! c
    !     ! c*********position (3)
    !     x(3)=dx*float(i-1)+abs(dex1)*dx/abs(dex4)+xxcop
    !     y(3)=dy*float((j))+yycop

    !     !         write(*,*) 'x(3),y(3)',x(3),y(3)
    !     ! c==========search position (4)================
    !     216     dex1=a(i+1,j)-conta(k)
    !     dex2=a(i+1,j+1)-conta(k)
    !     dex3=dex1*dex2
    !     if (dex3.le.0.) then
    !     go to 217
    !     else
    !     go to 240
    !     end if
    !     !-----------
    !     217     dex4=a(i+1,j)-a(i+1,j+1)
    !     if (dex4.eq.0.) then
    !     go to 230
    !     end if
    !     ! c
    !     ! !----------judgment of land and sea
    !     jnd4=ib(i+1,j)*ib(i+1,j+1)
    !     if (jnd4 .eq. 0) then
    !     go to 240
    !     end if
    !     ! c
    !     ! c**********position (4)
    !     x(4)=dx*float(i)+xxcop
    !     y(4)=dy*float(j-1)+abs(dex1)*dy/abs(dex4) &
    !     +yycop
    !     !         write(*,*) 'x(4),y(4)',x(4),y(4)
    !     ! c#########################################################
    !     ! c===========drawing contour line==========================
    !     ! !-----------case of four positions 
    !     230    if ((x(1).ne.0.).and.(x(2).ne.0.).and.(x(3).ne.0.).and. &
    !     (x(4).ne.0.)) then

    !     go to 235
    !     else
    !     go to 240
    !     end if
    !     ! c
    !     235     xlong1=(x(1)-x(2))**2+(y(1)-y(2))**2
    !     xlong2=(x(1)-x(4))**2+(y(1)-y(4))**2
    !     if (xlong1.le.xlong2) then
    !     go to 236
    !     else
    !     goto 237
    !     end if
    !     ! c
    !     236   call plot(x(1),y(1),3)
    !     call plot(x(2),y(2),2)
    !     call plot(x(3),y(3),3)
    !     call plot(x(4),y(4),2)



    !     !       write(*,*) 1,i,j,x(1),y(1)
    !     !       write(*,*) 1,i,j,x(2),y(2)
    !     !       write(*,*) 1,i,j,x(3),y(3)
    !     !       write(*,*) 1,i,j,x(4),y(4)
    !     ! C       stop 

    !     go to 250

    !     237   call plot(x(1),y(1),3)
    !     call plot(x(4),y(4),2)
    !     call plot(x(2),y(2),3)
    !     call plot(x(3),y(3),2)

    !     !       write(*,*) 2,i,j,x(1),y(1)
    !     !       write(*,*) 2,i,j,x(2),y(2)
    !     !       write(*,*) 2,i,j,x(3),y(3)
    !     !       write(*,*) 2,i,j,x(4),y(4)
    !     ! C       stop 

    !     go to 250
    !     ! c
    !     ! !-----------else case /  
    !     240   do 245 kk=1,4
    !     !        if(kk.eq.1) write(*,*)0,(x(jm),y(jm),jm=1,4) 
    !     if (x(kk).eq.0.) then
    !     !               write(*,*) 'x(kk)',kk,x(kk)
    !     go to 245
    !     end if
    !     x4=x(kk)
    !     y4=y(kk)
    !     !       write(*,*) 'U',kk,x4,y4

    !     if (x5.eq.0.) then
    !     go to 238
    !     end if

    !     call plot(x4,y4,3)
    !     call plot(x5,y5,2)

    !     !        write(*,*) 3,i,j,x4,y4
    !     !        write(*,*) 3,i,j,x5,y5


    !     238          x5=x4
    !     y5=y4
    !     !       write(*,*) 'L',kk,x4,y4,x5,y5
    !     x(kk)=0.
    !     245    continue
    !     250     x5=0.
    !     y5=0.
    !     x4=0.
    !     y4=0.

    !     xdx1=0.
    !     xdx2=0.
    !     xdx3=0.
    !     xdx4=0.
    !     ydy1=0.
    !     ydy2=0.
    !     ydy3=0.
    !     ydy4=0.
    !     !---
    !     do 260 kk=1,4
    !     x(kk)=0.
    !     y(kk)=0.
    !     260    continue
    !     !---

    !     270    continue
    !     280    continue
    !     300    continue
    !     ! c<<<<<<<<<<<<<<<<<<<<<<<<end>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !     return
    ! end
    subroutine pscolorK(px,py,ss,is,ist,ied,jst,jed,ix,iy,cst,ced,rr,gg,bb)
        ! ***********************************************************************
        ! *     cst <==> ced   : Range of painting 
        ! *    (RR,gg,bb)      : RGB color   
            Parameter(nx=20)
                dimension ss(ix,iy),is(ix,iy)
                dimension x(nx),y(nx)


        ! *###################################################################
        ! *################### ZERO STEP  ####################################
        ! *###################################################################

            do i=ist,ied-1 
                lp = 0
            do j=jst,jed-1  

        ! **************************************************************
                IF(is( i , j ).ne.0.and.is(i+1, j ).ne.0 .and.is( i ,j+1).ne.0.and.is(i+1,j+1).ne.0) THEN
        ! **************************************************************

                        s1 = ss( i , j )
                        s2 = ss(i+1, j )
                        s3 = ss( i ,j+1)
                        s4 = ss(i+1,j+1)

        ! *#(1-1)#*-----------
                if(((s1.ge.cst).and.(s1.le.ced)).and.&
                   ((s2.ge.cst).and.(s2.le.ced)).and.&
                   ((s3.ge.cst).and.(s3.le.ced)).and.&
                   ((s4.ge.cst).and.(s4.le.ced))&
                               .and.lp.eq.0.and.j.ne.jed-1) then 
                        lp = 1
                        xx = px*(i-ist) + px/2.0
                        yy = py*(j-jst) + py/2.0
                    xlef = xx
                    ylef = yy

                            is( i , j ) = 2  
                            is(i+1, j ) = 2  
                            is( i ,j+1) = 2  
                            is(i+1,j+1) = 2  

        ! *#(1-2)#*-----------
                    elseif((s1.ge.cst.and.s1.le.ced.and.&
                          s2.ge.cst.and.s2.le.ced.and.&
                          s3.ge.cst.and.s3.le.ced.and.&
                          s4.ge.cst.and.s4.le.ced)&   
                                   .and.lp.eq.1.and.j.ne.jed-1) then 

                            is( i , j ) = 2  
                            is(i+1, j ) = 2  
                            is( i ,j+1) = 2  
                            is(i+1,j+1) = 2  


        ! *#(1-3)#*-----------
                    elseif((s1.lt.cst .or. s1.gt.ced  .or.&
                          s2.lt.cst .or. s2.gt.ced  .or.&
                          s3.lt.cst .or. s3.gt.ced  .or.&
                          s4.lt.cst .or. s4.gt.ced)    .and.lp.eq.1) then
                        lp = 0
                        xx = px*(i+1 - ist) + px/2.0
                        yy = py*( j  - jst) + py/2.0
                    xrig = xx
                    yrig = yy
                        call betsqK(xlef,ylef,xrig,yrig,rr,gg,bb)


        ! *#(1-4)#*-----------
                    elseif((s1.ge.cst.and.s1.le.ced.and.&
                          s2.ge.cst.and.s2.le.ced.and.&
                          s3.ge.cst.and.s3.le.ced.and.&
                          s4.ge.cst.and.s4.le.ced) &
                                   .and.lp.eq.1.and.j.eq.jed-1) then 

                            is( i , j ) = 2  
                            is(i+1, j ) = 2  
                            is( i ,j+1) = 2  
                            is(i+1,j+1) = 2  

                        xx = px*(i+1 - ist) + px/2.0
                        yy = py*(j+1 - jst) + py/2.0
                    xrig = xx
                    yrig = yy
                        call betsqK(xlef,ylef,xrig,yrig,rr,gg,bb)
                        


        ! *#(1-5)#*-----------
                    elseif((s1.ge.cst.and.s1.le.ced.and.&
                          s2.ge.cst.and.s2.le.ced.and.&
                          s3.ge.cst.and.s3.le.ced.and.&
                          s4.ge.cst.and.s4.le.ced)&   
                                   .and.lp.eq.0.and.j.eq.jed-1) then 
                            is( i , j ) = 2  
                            is(i+1, j ) = 2  
                            is( i ,j+1) = 2  
                            is(i+1,j+1) = 2  

                        xx = px*(i - ist) + px/2.0
                        yy = py*(j - jst) + py/2.0
                    xlef = xx
                    ylef = yy
                    xrig = xx + px
                    yrig = yy + py
                        call betsqK(xlef,ylef,xrig,yrig,rr,gg,bb)

                endif
        ! *****************
            ELSE
        ! *****************
                    if(lp.eq.1) then 
                        lp = 0
                        xx = px*(i+1 - ist) + px/2.0
                        yy = py*( j  - jst) + py/2.0
                    xrig = xx
                    yrig = yy
                        call betsqK(xlef,ylef,xrig,yrig,rr,gg,bb)
                    endif 

        ! ********************
            ENDIF
        ! ********************
            enddo
            enddo


        ! *###################################################################
        ! *################### FIRST STEP ####################################
        ! *###################################################################
            do 20 j=jst,jed-1 
            do 10 i=ist,ied-1  
            if(is(i  ,j  ).eq.0) goto 10 
            if(is(i+1,j  ).eq.0) goto 10 
            if(is(i  ,j+1).eq.0) goto 10 
            if(is(i+1,j+1).eq.0) goto 10 

        ! ************************************************************
            if(is( i , j ).eq.2 .and. is(i+1, j ).eq.2  .and.&
                is( i ,j+1).eq.2 .and. is(i+1,j+1).eq.2 ) goto 10 
        ! ************************************************************

                s1 = ss( i , j )
                s2 = ss(i+1, j )
                s3 = ss( i ,j+1)
                s4 = ss(i+1,j+1)
                    xlef = real( i -ist)*px + px/2.0 
                    ylef = real( j -jst)*py + py/2.0
                    xrig = real(i+1-ist)*px + px/2.0 
                    yrig = real(j+1-jst)*py + py/2.0
        ! *       ------------------------------------------------------------
                if(((s1.ge.cst).and.(s1.le.ced)).and.&
                   ((s2.ge.cst).and.(s2.le.ced)).and.&
                   ((s3.ge.cst).and.(s3.le.ced)).and.&
                   ((s4.ge.cst).and.(s4.le.ced))) then 
                    x(1) = xlef
                    x(2) = xlef 
                    x(3) = xrig
                    x(4) = xrig 
                    y(1) = ylef
                    y(2) = yrig 
                    y(3) = yrig 
                    y(4) = ylef
                call betmlK(x,y,4,nx,rr,gg,bb)
        ! *       ------------------------------------------------------------
                elseif(((s1.ge.cst).and.(s1.le.ced)).and.&
                      ((s2.ge.cst).and.(s2.le.ced)).and.&
                      ((s3.ge.cst).and.(s3.le.ced)).and.&
                      ((s4.lt.cst).or.(s4.gt.ced))) then 
        ! *((((1))))
                dxa = abs(s4-s3)
                if(s4.gt.ced) then 
                ddx = abs(s3 - ced)
                else
                ddx = abs(s3 - cst)
                endif 
                x(3) = xlef + (ddx/dxa)*px 
        ! *((((2)))) 
                dxa = abs(s4-s2)        
                if(s4.gt.ced) then 
                ddx = abs(s2 - ced)
                else
                ddx = abs(s2 - cst)
                endif 
                y(4) = ylef + (ddx/dxa)*py 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = xrig
                    x(5) = xrig
                    y(1) = ylef
                    y(2) = yrig
                    y(3) = yrig 
                    y(4) = y(4)
                    y(5) = ylef  
                call betmlK(x,y,5,nx,rr,gg,bb)
        ! *       -------------------------------------------------------------
                elseif(((s1.ge.cst).and.(s1.le.ced)).and.&
                      ((s2.ge.cst).and.(s2.le.ced)).and.&
                      ((s3.lt.cst).or.(s3.gt.ced)).and.&
                      ((s4.ge.cst).and.(s4.le.ced))) then 
        ! *((((1)))) 
                dxa = abs(s3-s1)
                if(s3.gt.ced) then 
                ddx = abs(s1 - ced)
                else
                ddx = abs(s1 - cst)
                endif 
                y(2) = ylef + (ddx/dxa)*py 
        ! *((((2)))) 
                dxa = abs(s3-s4)
                if(s3.gt.ced) then 
                ddx = abs(s3 - ced)
                else
                ddx = abs(s3 - cst)
                endif 
                x(3) = xlef + (ddx/dxa)*px 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = xrig
                    x(5) = xrig
                    y(1) = ylef 
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig
                    y(5) = ylef 
                call betmlK(x,y,5,nx,rr,gg,bb)
        ! *       -------------------------------------------------------------
                elseif(((s1.ge.cst).and.(s1.le.ced)).and.&
                      ((s2.lt.cst).or.(s2.gt.ced)).and.&
                      ((s3.ge.cst).and.(s3.le.ced)).and.&
                      ((s4.ge.cst).and.(s4.le.ced))) then 
        ! *((((1)))) 
                dxa = abs(s2-s1)
                if(s2.gt.ced) then 
                ddx = abs(s1 - ced)
                else
                ddx = abs(s1 - cst)
                endif 
                x(5) = xlef + (ddx/dxa)*px 
        ! *((((2)))) 
                dxa = abs(s2-s4)
                if(s2.gt.ced) then 
                ddx = abs(s2 - ced)
                else
                ddx = abs(s2 - cst)
                endif 
                y(4) = ylef + (ddx/dxa)*py 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = xrig 
                    x(4) = xrig 
                    x(5) = x(5)
                    y(1) = ylef
                    y(2) = yrig
                    y(3) = yrig
                    y(4) = y(4)
                    y(5) = ylef 
                call betmlK(x,y,5,nx,rr,gg,bb)
        ! *       -------------------------------------------------------------
                elseif(((s1.lt.cst).or.(s1.gt.ced)).and.&
                       ((s2.ge.cst).and.(s2.le.ced)).and.&
                       ((s3.ge.cst).and.(s3.le.ced)).and.&
                       ((s4.ge.cst).and.(s4.le.ced))) then 
        ! *((((1)))) 
                dxa = abs(s3-s1)
                if(s1.gt.ced) then 
                ddx = abs(s1 - ced)
                else
                ddx = abs(s1 - cst)
                endif 
                y(1) = ylef  + (ddx/dxa)*py 
        ! *((((2)))) 
                dxa = abs(s2-s1)
                if(s1.gt.ced) then 
                ddx = abs(s1 - ced)
                else
                ddx = abs(s1 - cst)
                endif 
                x(5) = xlef + (ddx/dxa)*px  
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = xrig
                    x(4) = xrig 
                    x(5) = x(5)
                    y(1) = y(1)
                    y(2) = yrig
                    y(3) = yrig
                    y(4) = ylef
                    y(5) = ylef 
                call betmlK(x,y,5,nx,rr,gg,bb)
        ! *       -------------------------------------------------------------
                elseif(((s1.ge.cst).and.(s1.le.ced)).and.&
                       ((s2.ge.cst).and.(s2.le.ced)).and.&
                       ((s3.lt.cst).or.(s3.gt.ced)).and.&
                       ((s4.lt.cst).or.(s4.gt.ced))) then 
                if((s3.gt.ced).and.(s4.lt.cst)) then 
                dxa = abs(s3-s1)
                ddx = abs(s1 - ced)
                y(2) = ylef + (ddx/dxa)*py 
                dxa = abs(s3-s4)
                ddx = abs(s3-ced)
                x(3) = xlef + (ddx/dxa)*px 
                ddx = abs(s3-cst)
                x(4) = xlef + (ddx/dxa)*px
                dxa = abs(s4-s2)
                ddx = abs(s2-cst)
                y(5) = ylef + (ddx/dxa)*py
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = x(4)
                    x(5) = xrig
                    x(6) = xrig 
                    y(1) = ylef 
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig 
                    y(5) = y(5)
                    y(6) = ylef 
                call betmlK(x,y,6,nx,rr,gg,bb)
                else if((s3.lt.cst).and.(s4.gt.ced)) then 
                dxa = abs(s3-s1)
                ddx = abs(s1 - cst)
                y(2)= ylef + (ddx/dxa)*py 
                dxa = abs(s3-s4)
                ddx = abs(s3-cst)
                x(3) = xlef + (ddx/dxa)*px 
                ddx = abs(s3-ced)
                x(4) = xlef + (ddx/dxa)*px
                dxa = abs(s4-s2)
                ddx = abs(s2-ced)
                y(5) = ylef + (ddx/dxa)*py
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = x(4)
                    x(5) = xrig
                    x(6) = xrig 
                    y(1) = ylef 
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig 
                    y(5) = y(5)
                    y(6) = ylef 
                call betmlK(x,y,6,nx,rr,gg,bb)
                else if((s3.gt.ced).and.(s4.gt.ced)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                y(3) = ylef + (ddx/dxa)*py 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = xrig
                    x(4) = xrig 
                    y(1) = ylef
                    y(2) = y(2)
                    y(3) = y(3)
                    y(4) = ylef 
                call betmlK(x,y,4,nx,rr,gg,bb)
                else if((s3.lt.cst).and.(s4.lt.cst)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                y(3) = ylef + (ddx/dxa)*py 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = xrig
                    x(4) = xrig 
                    y(1) = ylef
                    y(2) = y(2)
                    y(3) = y(3)
                    y(4) = ylef 
                call betmlK(x,y,4,nx,rr,gg,bb)
                endif  
        ! *       -------------------------------------------------------------
                elseif(((s1.ge.cst).and.(s1.le.ced)).and.&
                       ((s2.lt.cst).or.(s2.gt.ced)).and.&
                       ((s3.lt.cst).or.(s3.gt.ced)).and.&
                       ((s4.ge.cst).and.(s4.le.ced))) then 
                if((s3.gt.ced).and.(s2.lt.cst)) then 
                    dxa = abs(s3-s1)
                    ddx = abs(s1-ced)
                y(2) = ylef + (ddx/dxa)*py
                    dxa = abs(s4-s3) 
                    ddx = abs(s3-ced) 
                x(3) = xlef + (ddx/dxa)*px
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                x(6) = xlef + (ddx/dxa)*px
                    dxa = abs(s4-s2)
                    ddx = abs(s2-cst)
                y(5) = ylef + (ddx/dxa)*py
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = xrig
                    x(5) = xrig 
                    x(6) = x(6)
                    y(1) = ylef
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig
                    y(5) = y(5)
                    y(6) = ylef 
                call betmlK(x,y,6,nx,rr,gg,bb)
                elseif((s3.lt.cst).and.(s2.gt.ced)) then 
                    dxa = abs(s3-s1)
                    ddx = abs(s1-cst)
                y(2) = ylef + (ddx/dxa)*py
                    dxa = abs(s4-s3) 
                    ddx = abs(s3-cst) 
                x(3) = xlef + (ddx/dxa)*px
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                x(6) = xlef + (ddx/dxa)*px
                    dxa = abs(s4-s2)
                    ddx = abs(s2-ced)
                y(5) = ylef + (ddx/dxa)*py
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = xrig
                    x(5) = xrig 
                    x(6) = x(6)
                    y(1) = ylef
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig
                    y(5) = y(5)
                    y(6) = ylef 
                call betmlK(x,y,6,nx,rr,gg,bb)
                elseif((s3.lt.cst).and.(s2.lt.cst)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x01 = xlef + (ddx/dxa)*px
                    y01 = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-cst)
                    x02 = xlef
                    y02 = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x03 = xlef + (ddx/dxa)*px 
                    y03 = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-cst)
                    x04 = xrig 
                    y04 = ylef + (ddx/dxa)*py
                    eng1 = (x01-x02)**2 + (y01-y02)**2 
                    eng2 = (x01-x04)**2 + (y01-y04)**2 
        ! ***POST***
                if(eng2.lt.eng1) then 
                    x(1) = xlef 
                    x(2) = x02
                    x(3) = x03  
                    x(4) = xrig 
                    x(5) = x04 
                    x(6) = x01 
                    y(1) = ylef
                    y(2) = y02 
                    y(3) = y03
                    y(4) = yrig 
                    y(5) = y04
                    y(6) = y01
                call betmlK(x,y,6,nx,rr,gg,bb)
                else 
                    x(1) = xlef
                    x(2) = x02
                    x(3) = x01
                    y(1) = ylef
                    y(2) = y02
                    y(3) = y01
                call betmlK(x,y,3,nx,rr,gg,bb)
                    x(1) = x03 
                    x(2) = xrig 
                    x(3) = x04
                    y(1) = y03
                    y(2) = yrig
                    y(3) = y04 
                call betmlK(x,y,3,nx,rr,gg,bb)
                endif
                elseif((s3.gt.ced).and.(s2.gt.ced)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x01 = xlef + (ddx/dxa)*px
                    y01 = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-ced)
                    x02 = xlef
                    y02 = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x03 = xlef + (ddx/dxa)*px 
                    y03 = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-ced)
                    x04 = xrig 
                    y04 = ylef + (ddx/dxa)*py
                    eng1 = (x01-x02)**2 + (y01-y02)**2 
                    eng2 = (x01-x04)**2 + (y01-y04)**2 
        ! ***POST***
                if(eng2.lt.eng1) then 
                    x(1) = xlef 
                    x(2) = x02
                    x(3) = x03  
                    x(4) = xrig 
                    x(5) = x04 
                    x(6) = x01 
                    y(1) = ylef
                    y(2) = y02 
                    y(3) = y03
                    y(4) = yrig 
                    y(5) = y04
                    y(6) = y01
                call betmlK(x,y,6,nx,rr,gg,bb)
                else 
                    x(1) = xlef
                    x(2) = x02
                    x(3) = x01
                    y(1) = ylef
                    y(2) = y02
                    y(3) = y01
                call betmlK(x,y,3,nx,rr,gg,bb)
                    x(1) = x03 
                    x(2) = xrig 
                    x(3) = x04
                    y(1) = y03
                    y(2) = yrig
                    y(3) = y04 
                call betmlK(x,y,3,nx,rr,gg,bb)
                endif
                endif 
        ! *       -------------------------------------------------------------
                elseif(((s1.lt.cst).or.(s1.gt.ced)).and.&
                       ((s2.ge.cst).and.(s2.le.ced)).and.&
                       ((s3.lt.cst).or.(s3.gt.ced)).and.&
                       ((s4.ge.cst).and.(s4.le.ced))) then 
                if((s1.lt.cst).and.(s3.gt.ced)) then 
                dxa = abs(s1-s3)
                ddx = abs(s1-cst)
                y(1) = ylef + (ddx/dxa)*py
                ddx = abs(s1-ced)
                y(2) = ylef + (ddx/dxa)*py
                dxa = abs(s3-s4)
                ddx = abs(s3-ced)
                x(3) = xlef + (ddx/dxa)*px  
                dxa = abs(s1-s2)
                ddx = abs(s1-cst)
                x(6) = xlef + (ddx/dxa)*px 
                    x(1) = xlef 
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = xrig
                    x(5) = xrig 
                    x(6) = x(6)
                    y(1) = y(1)
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig
                    y(5) = ylef
                    y(6) = ylef 
                call betmlK(x,y,6,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s3.lt.cst)) then 
                dxa = abs(s1-s3)
                ddx = abs(s1-ced)
                y(1) = ylef + (ddx/dxa)*py
                ddx = abs(s1-cst)
                y(2) = ylef + (ddx/dxa)*py
                dxa = abs(s3-s4)
                ddx = abs(s3-cst)
                x(3) = xlef + (ddx/dxa)*px  
                dxa = abs(s1-s2)
                ddx = abs(s1-ced)
                x(6) = xlef + (ddx/dxa)*px 
                    x(1) = xlef 
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = xrig
                    x(5) = xrig 
                    x(6) = x(6)
                    y(1) = y(1)
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig
                    y(5) = ylef
                    y(6) = ylef 
                call betmlK(x,y,6,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s3.lt.cst)) then 
                dxa = abs(s1-s2)
                ddx = abs(s1-cst)
                x(1) = xlef + (ddx/dxa)*px 
                dxa = abs(s3-s4)
                ddx = abs(s3-cst)
                x(2) = xlef + (ddx/dxa)*px 
                    x(1) = x(1)
                    x(2) = x(2)        
                    x(3) = xrig        
                    x(4) = xrig        
                    y(1) = ylef
                    y(2) = yrig        
                    y(3) = yrig        
                    y(4) = ylef         
                call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s3.gt.ced)) then 
                dxa = abs(s1-s2)
                ddx = abs(s1-ced)
                x(1) = xlef + (ddx/dxa)*px 
                dxa = abs(s3-s4)
                ddx = abs(s3-ced)
                x(2) = xlef + (ddx/dxa)*px 
                    x(1) = x(1)
                    x(2) = x(2)        
                    x(3) = xrig        
                    x(4) = xrig        
                    y(1) = ylef
                    y(2) = yrig        
                    y(3) = yrig        
                    y(4) = ylef         
                call betmlK(x,y,4,nx,rr,gg,bb)
                endif 
        ! *       -------------------------------------------------------------
                elseif(((s1.lt.cst).or.(s1.gt.ced)).and.&
                       ((s2.lt.cst).or.(s2.gt.ced)).and.&
                       ((s3.ge.cst).and.(s3.le.ced)).and.&
                       ((s4.ge.cst).and.(s4.le.ced))) then 
                if((s1.gt.ced).and.(s2.lt.cst)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                y(1) = ylef + (ddx/dxa)*py 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                y(4) = ylef + (ddx/dxa)*py
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                x(5) = xlef + (ddx/dxa)*px 
                    ddx = abs(s1-ced)
                x(6) = xlef + (ddx/dxa)*px 
                    x(1) = xlef 
                    x(2) = xlef 
                    x(3) = xrig 
                    x(4) = xrig 
                    x(5) = x(5) 
                    x(6) = x(6) 
                    y(1) = y(1) 
                    y(2) = yrig 
                    y(3) = yrig 
                    y(4) = y(4) 
                    y(5) = ylef 
                    y(6) = ylef 
                call betmlK(x,y,6,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.gt.ced)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                y(1) = ylef + (ddx/dxa)*py 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                y(4) = ylef + (ddx/dxa)*py
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                x(5) = xlef + (ddx/dxa)*px 
                    ddx = abs(s1-cst)
                x(6) = xlef + (ddx/dxa)*px 
                    x(1) = xlef 
                    x(2) = xlef 
                    x(3) = xrig 
                    x(4) = xrig 
                    x(5) = x(5) 
                    x(6) = x(6) 
                    y(1) = y(1) 
                    y(2) = yrig 
                    y(3) = yrig 
                    y(4) = y(4) 
                    y(5) = ylef 
                    y(6) = ylef 
                call betmlK(x,y,6,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.lt.cst)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                y(1) = ylef + (ddx/dxa)*py 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                y(4) = ylef + (ddx/dxa)*py 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = xrig 
                    x(4) = xrig 
                    y(1) = y(1) 
                    y(2) = yrig
                    y(3) = yrig
                    y(4) = y(4) 
                call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s2.gt.ced)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                y(1) = ylef + (ddx/dxa)*py 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                y(4) = ylef + (ddx/dxa)*py 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = xrig 
                    x(4) = xrig 
                    y(1) = y(1) 
                    y(2) = yrig
                    y(3) = yrig
                    y(4) = y(4)
                call betmlK(x,y,4,nx,rr,gg,bb)
                endif 
        ! *       -------------------------------------------------------------
                elseif(((s1.lt.cst).or.(s1.gt.ced)).and.&
                       ((s2.ge.cst).and.(s2.le.ced)).and.&
                       ((s3.ge.cst).and.(s3.le.ced)).and.&
                       ((s4.lt.cst).or.(s4.gt.ced))) then 
                if((s1.lt.cst).and.(s4.gt.ced)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                    y(1) = ylef + (ddx/dxa)*py
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x(3) = xlef + (ddx/dxa)*px 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                    y(4) = ylef + (ddx/dxa)*py 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x(6) = xlef + (ddx/dxa)*px 
                    x(1) = xlef 
                    x(2) = xlef
                    x(3) = x(3) 
                    x(4) = xrig 
                    x(5) = xrig  
                    x(6) = x(6) 
                    y(1) = y(1) 
                    y(2) = yrig 
                    y(3) = yrig 
                    y(4) = y(4) 
                    y(5) = ylef 
                    y(6) = ylef  
                call betmlK(x,y,6,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s4.lt.cst)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                    y(1) = ylef + (ddx/dxa)*py
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x(3) = xlef + (ddx/dxa)*px 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                    y(4) = ylef + (ddx/dxa)*py 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x(6) = xlef + (ddx/dxa)*px 
                    x(1) = xlef 
                    x(2) = xlef
                    x(3) = x(3) 
                    x(4) = xrig 
                    x(5) = xrig  
                    x(6) = x(6) 
                    y(1) = y(1) 
                    y(2) = yrig 
                    y(3) = yrig 
                    y(4) = y(4) 
                    y(5) = ylef 
                    y(6) = ylef  
                call betmlK(x,y,6,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s4.lt.cst)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst) 
                    x01 = xlef + (ddx/dxa)*px 
                    y01 = ylef 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                    x02 = xlef 
                    y02 = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x03 = xlef + (ddx/dxa)*px
                    y03 = yrig 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                    x04 = xrig 
                    y04 = ylef + (ddx/dxa)*py 
                    eng1 = (x01-x02)**2 + (y01-y02)**2 
                    eng2 = (x01-x04)**2 + (y01-y04)**2 
        ! ***POST***
                if(eng2.lt.eng1) then 
                    x(1) = x02  
                    x(2) = xlef 
                    x(3) = x03  
                    y(1) = y02 
                    y(2) = yrig 
                    y(3) = y03   
                    call betmlK(x,y,3,nx,rr,gg,bb)
                    x(1) = x01  
                    x(2) = x04 
                    x(3) = xrig  
                    y(1) = y01 
                    y(2) = y04 
                    y(3) = ylef
                    call betmlK(x,y,3,nx,rr,gg,bb)
                    else 
                    x(1) = x02  
                    x(2) = xlef 
                    x(3) = x03 
                    x(4) = x04 
                    x(5) = xrig 
                    x(6) = x01  
                    y(1) = y02 
                    y(2) = yrig 
                    y(3) = y03 
                    y(4) = y04 
                    y(5) = ylef 
                    y(6) = y01 
                    call betmlK(x,y,6,nx,rr,gg,bb)
                    endif 
                elseif((s1.gt.ced).and.(s4.gt.ced)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced) 
                    x01 = xlef + (ddx/dxa)*px 
                    y01 = ylef 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                    x02 = xlef 
                    y02 = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x03 = xlef + (ddx/dxa)*px
                    y03 = yrig 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                    x04 = xrig 
                    y04 = ylef + (ddx/dxa)*py 
                    eng1 = (x01-x02)**2 + (y01-y02)**2 
                    eng2 = (x01-x04)**2 + (y01-y04)**2 
        ! ***POST***
                if(eng2.lt.eng1) then 
                    x(1) = x02  
                    x(2) = xlef 
                    x(3) = x03  
                    y(1) = y02 
                    y(2) = yrig 
                    y(3) = y03   
                    call betmlK(x,y,3,nx,rr,gg,bb)
                    x(1) = x01  
                    x(2) = x04 
                    x(3) = xrig  
                    y(1) = y01 
                    y(2) = y04 
                    y(3) = ylef
                    call betmlK(x,y,3,nx,rr,gg,bb)
                    else 
                    x(1) = x02  
                    x(2) = xlef 
                    x(3) = x03 
                    x(4) = x04 
                    x(5) = xrig 
                    x(6) = x01  
                    y(1) = y02 
                    y(2) = yrig 
                    y(3) = y03 
                    y(4) = y04 
                    y(5) = ylef 
                    y(6) = y01 
                    call betmlK(x,y,6,nx,rr,gg,bb)
                    endif 
                endif  
        ! *       -------------------------------------------------------------
                elseif(((s1.ge.cst).and.(s1.le.ced)).and.&
                       ((s2.lt.cst).or.(s2.gt.ced)).and.&
                       ((s3.ge.cst).and.(s3.le.ced)).and.&
                       ((s4.lt.cst).or.(s4.gt.ced))) then 
                if((s2.lt.cst).and.(s4.gt.ced)) then 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x(3) = xlef + (ddx/dxa)*px
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                    y(4) = ylef + (ddx/dxa)*py 
                    ddx = abs(s2-cst)
                    y(5) = ylef + (ddx/dxa)*py
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x(6) = xlef + (ddx/dxa)*px 
                    x(1) = xlef 
                    x(2) = xlef 
                    x(3) = x(3) 
                    x(4) = xrig 
                    x(5) = xrig 
                    x(6) = x(6) 
                    y(1) = ylef 
                    y(2) = yrig 
                    y(3) = yrig 
                    y(4) = y(4) 
                    y(5) = y(5)  
                    y(6) = ylef  
                call betmlK(x,y,6,nx,rr,gg,bb)
                elseif((s2.gt.ced).and.(s4.lt.cst)) then 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x(3) = xlef + (ddx/dxa)*px
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                    y(4) = ylef + (ddx/dxa)*py 
                    ddx = abs(s2-ced)
                    y(5) = ylef + (ddx/dxa)*py
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x(6) = xlef + (ddx/dxa)*px 
                    x(1) = xlef 
                    x(2) = xlef 
                    x(3) = x(3) 
                    x(4) = xrig 
                    x(5) = xrig 
                    x(6) = x(6) 
                    y(1) = ylef 
                    y(2) = yrig 
                    y(3) = yrig 
                    y(4) = y(4) 
                    y(5) = y(5)  
                    y(6) = ylef  
                call betmlK(x,y,6,nx,rr,gg,bb)
                elseif((s2.lt.cst).and.(s4.lt.cst)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x(4) = xlef + (ddx/dxa)*px 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x(3) = xlef + (ddx/dxa)*px
                        x(1) = xlef
                        x(2) = xlef
                        x(3) = x(3)
                        x(4) = x(4)
                        y(1) = ylef
                        y(2) = yrig
                        y(3) = yrig
                        y(4) = ylef 
                call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s2.gt.ced).and.(s4.gt.ced)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x(4) = xlef + (ddx/dxa)*px 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x(3) = xlef + (ddx/dxa)*px
                        x(1) = xlef  
                        x(2) = xlef
                        x(3) = x(3)
                        x(4) = x(4)
                        y(1) = ylef
                        y(2) = yrig
                        y(3) = yrig
                        y(4) = ylef 
                call betmlK(x,y,4,nx,rr,gg,bb)
                endif 
        ! *       -------------------------------------------------------------
                elseif(((s1.ge.cst).and.(s1.le.ced)).and.&
                       ((s2.lt.cst).or.(s2.gt.ced)).and.&
                       ((s3.lt.cst).or.(s3.gt.ced)).and.&
                       ((s4.lt.cst).or.(s4.gt.ced))) then 
                if((s2.lt.cst).and.(s3.gt.ced).and.(s4.gt.ced)) then  
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                y(3) = ylef + (ddx/dxa)*py 
                    ddx = abs(s2-cst)
                y(4) = ylef + (ddx/dxa)*py  
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                x(5) = xlef + (ddx/dxa)*px 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = xrig 
                    x(4) = xrig 
                    x(5) = x(5)
                    y(1) = ylef 
                    y(2) = y(2)
                    y(3) = y(3)
                    y(4) = y(4)
                    y(5) = ylef  
                call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s2.lt.cst).and.(s3.gt.ced).and.(s4.lt.cst)) then  
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                x(3) = xlef + (ddx/dxa)*px 
                    ddx = abs(s3-cst)
                x(4) = xlef + (ddx/dxa)*px 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                x(5) = xlef + (ddx/dxa)*px 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3) 
                    x(4) = x(4) 
                    x(5) = x(5)
                    y(1) = ylef 
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig
                    y(5) = ylef  
                call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s2.lt.cst).and.(s3.lt.cst).and.(s4.lt.cst)) then  
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                x(3) = xlef + (ddx/dxa)*px 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3) 
                    y(1) = ylef
                    y(2) = y(2)
                    y(3) = ylef 
                call betmlK(x,y,3,nx,rr,gg,bb)
                elseif((s2.gt.ced).and.(s3.gt.ced).and.(s4.gt.ced)) then  
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                x(3) = xlef + (ddx/dxa)*px 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3) 
                    y(1) = ylef
                    y(2) = y(2)
                    y(3) = ylef 
                call betmlK(x,y,3,nx,rr,gg,bb)
                elseif((s2.gt.ced).and.(s3.lt.cst).and.(s4.lt.cst)) then  
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                y(3) = ylef + (ddx/dxa)*py 
                    ddx = abs(s2-ced)
                y(4) = ylef + (ddx/dxa)*py 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                x(5) = xlef + (ddx/dxa)*px 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = xrig
                    x(4) = xrig 
                    x(5) = x(5)
                    y(1) = ylef
                    y(2) = y(2)
                    y(3) = y(3) 
                    y(4) = y(4) 
                    y(5) = ylef  
                call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s2.gt.ced).and.(s3.lt.cst).and.(s4.gt.ced)) then  
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                x(3) = xlef + (ddx/dxa)*px 
                    ddx = abs(s3-ced)
                x(4) = xlef + (ddx/dxa)*px 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                x(5) = xlef + (ddx/dxa)*px 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = x(4) 
                    x(5) = x(5)
                    y(1) = ylef
                    y(2) = y(2)
                    y(3) = yrig 
                    y(4) = yrig 
                    y(5) = ylef   
                call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s2.gt.ced).and.(s3.gt.cst).and.(s4.lt.cst)) then  
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x01 = xlef + (ddx/dxa)*px
                    y01 = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-ced)
                    x02 = xlef
                    y02 = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x03 = xlef + (ddx/dxa)*px 
                    y03 = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-ced)
                    x04 = xrig 
                    y04 = ylef + (ddx/dxa)*py
                    eng1 = (x01-x02)**2 + (y01-y02)**2 
                    eng2 = (x01-x04)**2 + (y01-y04)**2 

                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x(4) = xlef + (ddx/dxa)*px 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-cst)
                    y(5) = ylef + (ddx/dxa)*py
        ! ***POST***
                if(eng2.lt.eng1) then 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x03
                    x(4) = x(4)
                    x(5) = xrig
                    x(6) = xrig
                    x(7) = x01
                    y(1) = ylef
                    y(2) = y02
                    y(3) = yrig
                    y(4) = yrig 
                    y(5) = y(5)
                    y(6) = y04
                    y(7) = ylef 
                    call betmlK(x,y,7,nx,rr,gg,bb)
                else
                    x(1) = xlef
                    x(2) = xlef 
                    x(3) = x01  
                    y(1) = ylef
                    y(2) = y02
                    y(3) = ylef  
                    call betmlK(x,y,3,nx,rr,gg,bb)
                    x(1) = x03
                    x(2) = x(4)
                    x(3) = xrig
                    x(4) = xrig 
                    y(1) = yrig
                    y(2) = yrig
                    y(3) = y(5)
                    y(4) = y04 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                endif
                elseif((s2.lt.cst).and.(s3.lt.cst).and.(s4.gt.ced)) then  
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x01 = xlef + (ddx/dxa)*px
                    y01 = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-cst)
                    x02 = xlef
                    y02 = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x03 = xlef + (ddx/dxa)*px 
                    y03 = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-cst)
                    x04 = xrig 
                    y04 = ylef + (ddx/dxa)*py
                    eng1 = (x01-x02)**2 + (y01-y02)**2 
                    eng2 = (x01-x04)**2 + (y01-y04)**2 

                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x(4) = xlef + (ddx/dxa)*px 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-ced)
                    y(5) = ylef + (ddx/dxa)*py
        ! ***POST***
                if(eng2.lt.eng1) then 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x03
                    x(4) = x(4)
                    x(5) = xrig
                    x(6) = xrig
                    x(7) = x01
                    y(1) = ylef
                    y(2) = y02
                    y(3) = yrig
                    y(4) = yrig 
                    y(5) = y(5)
                    y(6) = y04
                    y(7) = ylef 
                    call betmlK(x,y,7,nx,rr,gg,bb)
                else
                    x(1) = xlef
                    x(2) = xlef 
                    x(3) = x01  
                    y(1) = ylef
                    y(2) = y02
                    y(3) = ylef  
                    call betmlK(x,y,3,nx,rr,gg,bb)
                    x(1) = x03
                    x(2) = x(4)
                    x(3) = xrig
                    x(4) = xrig 
                    y(1) = yrig
                    y(2) = yrig
                    y(3) = y(5)
                    y(4) = y04 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                endif
                endif 
        ! *       -------------------------------------------------------------
                elseif(((s1.lt.cst).or.(s1.gt.ced)).and.&
                       ((s2.ge.cst).and.(s2.le.ced)).and.&
                       ((s3.lt.cst).or.(s3.gt.ced)).and.&
                       ((s4.lt.cst).or.(s4.gt.ced))) then 
                if((s4.lt.cst).and.(s1.gt.ced).and.(s3.gt.ced)) then  
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                x(1) = xlef + (ddx/dxa)*px 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                x(2) = xlef + (ddx/dxa)*px 
                    ddx = abs(s3-cst)
                x(3) = xlef + (ddx/dxa)*px  
                    dxa = abs(s4-s2)
                    ddx = abs(s2-cst)
                y(4) = ylef + (ddx/dxa)*py 
                    x(1) = x(1)
                    x(2) = x(2)
                    x(3) = x(3)
                    x(4) = xrig 
                    x(5) = xrig
                    y(1) = ylef
                    y(2) = yrig
                    y(3) = yrig
                    y(4) = y(4)
                    y(5) = ylef 
                    call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s4.lt.cst).and.(s1.gt.ced).and.(s3.lt.cst)) then  
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                x(1) = xlef + (ddx/dxa)*px 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                y(2) = ylef + (ddx/dxa)*py 
                    ddx = abs(s1-cst)
                y(3) = ylef + (ddx/dxa)*py 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                y(4) = ylef + (ddx/dxa)*py 
                    x(1) = x(1)
                    x(2) = xlef
                    x(3) = xlef
                    x(4) = xrig 
                    x(5) = xrig
                    y(1) = ylef
                    y(2) = y(2)
                    y(3) = y(3)
                    y(4) = y(4)
                    y(5) = ylef 
                    call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s4.lt.cst).and.(s1.lt.cst).and.(s3.lt.cst)) then  
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                x(1) = xlef + (ddx/dxa)*px 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                y(2) = ylef + (ddx/dxa)*py 
                    x(1) = x(1)
                    x(2) = xrig
                    x(3) = xrig
                    y(1) = ylef
                    y(2) = y(2)
                    y(3) = ylef 
                    call betmlK(x,y,3,nx,rr,gg,bb)
                elseif((s4.gt.ced).and.(s1.gt.ced).and.(s3.gt.ced)) then  
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                x(1) = xlef + (ddx/dxa)*px 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                y(2) = ylef + (ddx/dxa)*py 
                    x(1) = x(1)
                    x(2) = xrig
                    x(3) = xrig
                    y(1) = ylef
                    y(2) = y(2)
                    y(3) = ylef 
                    call betmlK(x,y,3,nx,rr,gg,bb)
                elseif((s4.gt.ced).and.(s1.lt.cst).and.(s3.lt.cst)) then  
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                x(1) = xlef + (ddx/dxa)*px 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                x(2) = xlef + (ddx/dxa)*px 
                    ddx = abs(s3-ced)
                x(3) = xlef + (ddx/dxa)*px 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                y(4) = ylef + (ddx/dxa)*py 
                    x(1) = x(1)
                    x(2) = x(2)
                    x(3) = x(3)
                    x(4) = xrig
                    x(5) = xrig 
                    y(1) = ylef
                    y(2) = yrig
                    y(3) = yrig 
                    y(4) = y(4)
                    y(5) = ylef 
                    call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s4.gt.ced).and.(s1.lt.cst).and.(s3.gt.ced)) then  
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                x(1) = xlef + (ddx/dxa)*px 
                    dxa = abs(s3-s1)
                    ddx = abs(s1-cst)
                y(2) = ylef + (ddx/dxa)*py 
                    ddx = abs(s1-ced)
                y(3) = ylef + (ddx/dxa)*py 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-ced)
                y(4) = ylef + (ddx/dxa)*py 
                    x(1) = x(1)
                    x(2) = xlef
                    x(3) = xlef
                    x(4) = xrig
                    x(5) = xrig 
                    y(1) = ylef
                    y(2) = y(2)
                    y(3) = y(3) 
                    y(4) = y(4)
                    y(5) = ylef 
                    call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s4.gt.ced).and.(s1.gt.ced).and.(s3.lt.cst)) then  
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x01 = xlef + (ddx/dxa)*px
                    y01 = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-ced)
                    x02 = xlef
                    y02 = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x03 = xlef + (ddx/dxa)*px 
                    y03 = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-ced)
                    x04 = xrig 
                    y04 = ylef + (ddx/dxa)*py
                    eng1 = (x01-x02)**2 + (y01-y02)**2 
                    eng2 = (x01-x04)**2 + (y01-y04)**2 

                    dxa = abs(s3-s1)
                    ddx = abs(s1-cst)
                    y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x(3) = xlef + (ddx/dxa)*px
        ! ***POST***
                if(eng2.lt.eng1) then 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = x03
                    y(1) = y02
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                    x(1) = x01 
                    x(2) = xrig
                    x(3) = xrig
                    y(1) = ylef
                    y(2) = y04
                    y(3) = ylef 
                    call betmlK(x,y,3,nx,rr,gg,bb)
                else 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = x03
                    x(5) = xrig
                    x(6) = xrig 
                    x(7) = x01
                    y(1) = y02
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig
                    y(5) = y04
                    y(6) = ylef
                    y(7) = ylef 
                    call betmlK(x,y,7,nx,rr,gg,bb)
                endif 
                elseif((s4.lt.cst).and.(s1.lt.cst).and.(s3.gt.ced)) then  
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x01 = xlef + (ddx/dxa)*px
                    y01 = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-cst)
                    x02 = xlef
                    y02 = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x03 = xlef + (ddx/dxa)*px 
                    y03 = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-cst)
                    x04 = xrig 
                    y04 = ylef + (ddx/dxa)*py
                    eng1 = (x01-x02)**2 + (y01-y02)**2 
                    eng2 = (x01-x04)**2 + (y01-y04)**2 

                    dxa = abs(s3-s1)
                    ddx = abs(s1-ced)
                    y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x(3) = xlef + (ddx/dxa)*px
        ! ***POST***
                if(eng2.lt.eng1) then 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = x03
                    y(1) = y02
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                    x(1) = x01 
                    x(2) = xrig
                    x(3) = xrig
                    y(1) = ylef
                    y(2) = y04
                    y(3) = ylef 
                    call betmlK(x,y,3,nx,rr,gg,bb)
                else 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = x03
                    x(5) = xrig
                    x(6) = xrig 
                    x(7) = x01
                    y(1) = y02
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig
                    y(5) = y04
                    y(6) = ylef
                    y(7) = ylef 
                    call betmlK(x,y,7,nx,rr,gg,bb)
                endif 
                endif 
        ! *       -------------------------------------------------------------
                elseif(((s1.lt.cst).or.(s1.gt.ced)).and.&
                       ((s2.lt.cst).or.(s2.gt.ced)).and.&
                       ((s3.ge.cst).and.(s3.le.ced)).and.&
                       ((s4.lt.cst).or.(s4.gt.ced))) then 
                if((s1.lt.cst).and.(s2.gt.ced).and.(s4.gt.ced)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                x(1) = xlef + (ddx/dxa)*px 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                x(4) = xlef + (ddx/dxa)*px 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                x(5) = xlef + (ddx/dxa)*px 
                    x(1) = x(1)
                    x(2) = xlef
                    x(3) = xlef 
                    x(4) = x(4)
                    x(5) = x(5)
                    y(1) = ylef 
                    y(2) = y(2) 
                    y(3) = yrig
                    y(4) = yrig 
                    y(5) = ylef 
                    call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.lt.cst).and.(s4.gt.ced)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                y(1) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                x(3) = xlef + (ddx/dxa)*px 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                y(4) = ylef + (ddx/dxa)*py 
                    ddx = abs(s2-cst)
                y(5) = ylef + (ddx/dxa)*py 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3) 
                    x(4) = xrig
                    x(5) = xrig
                    y(1) = y(1) 
                    y(2) = yrig 
                    y(3) = yrig
                    y(4) = y(4) 
                    y(5) = y(5) 
                    call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.lt.cst).and.(s4.lt.cst)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                y(1) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                x(3) = xlef + (ddx/dxa)*px 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    y(1) = y(1)
                    y(2) = yrig
                    y(3) = yrig 
                    call betmlK(x,y,3,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s2.gt.ced).and.(s4.gt.ced)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                y(1) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                x(3) = xlef + (ddx/dxa)*px 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    y(1) = y(1)
                    y(2) = yrig
                    y(3) = yrig 
                    call betmlK(x,y,3,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s2.lt.cst).and.(s4.lt.cst)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                y(1) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                x(3) = xlef + (ddx/dxa)*px 
                    dxa = abs(s2-s1)
                    ddx = abs(s1-cst)
                x(4) = xlef + (ddx/dxa)*px 
                    ddx = abs(s1-ced)
                x(5) = xlef + (ddx/dxa)*px 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = x(4)
                    x(5) = x(5)
                    y(1) = y(1)
                    y(2) = yrig
                    y(3) = yrig
                    y(4) = ylef
                    y(5) = ylef
                    call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s2.gt.ced).and.(s4.lt.cst)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                y(1) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                x(3) = xlef + (ddx/dxa)*px 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                y(4) = ylef + (ddx/dxa)*py 
                    ddx = abs(s2-ced)
                y(5) = ylef + (ddx/dxa)*py 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x(3)
                    x(4) = xrig
                    x(5) = xrig
                    y(1) = y(1)
                    y(2) = yrig
                    y(3) = yrig
                    y(4) = y(4)
                    y(5) = y(5)
                    call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s2.lt.cst).and.(s4.gt.ced)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x01 = xlef + (ddx/dxa)*px
                    y01 = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-ced)
                    x02 = xlef
                    y02 = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x03 = xlef + (ddx/dxa)*px 
                    y03 = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-ced)
                    x04 = xrig 
                    y04 = ylef + (ddx/dxa)*py
                    eng1 = (x01-x02)**2 + (y01-y02)**2 
                    eng2 = (x01-x04)**2 + (y01-y04)**2 

                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                    y(5) = ylef + (ddx/dxa)*py 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x(6) = xlef + (ddx/dxa)*px
        ! ***POST***
                if(eng2.lt.eng1) then 
                    x(1) = xlef 
                    x(2) = xlef 
                    x(3) = x03 
                    y(1) = y02 
                    y(2) = yrig 
                    y(3) = yrig 
                    call betmlK(x,y,3,nx,rr,gg,bb)
                    x(1) = x01
                    x(2) = xrig
                    x(3) = xrig
                    x(4) = x(6)
                    y(1) = ylef
                    y(2) = y04
                    y(3) = y(5)
                    y(4) = ylef 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                else
                    x(1) = xlef
                    x(2) = xlef 
                    x(3) = x03 
                    x(4) = xrig
                    x(5) = xrig
                    x(6) = x(6)
                    x(7) = x01
                    y(1) = y02
                    y(2) = yrig
                    y(3) = yrig
                    y(4) = y04
                    y(5) = y(5)
                    y(6) = ylef
                    y(7) = ylef 
                    call betmlK(x,y,7,nx,rr,gg,bb)
                endif 
                elseif((s1.lt.cst).and.(s2.gt.ced).and.(s4.lt.cst)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x01 = xlef + (ddx/dxa)*px
                    y01 = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-cst)
                    x02 = xlef
                    y02 = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x03 = xlef + (ddx/dxa)*px 
                    y03 = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-cst)
                    x04 = xrig 
                    y04 = ylef + (ddx/dxa)*py
                    eng1 = (x01-x02)**2 + (y01-y02)**2 
                    eng2 = (x01-x04)**2 + (y01-y04)**2 

                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                    y(5) = ylef + (ddx/dxa)*py 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x(6) = xlef + (ddx/dxa)*px
        ! ***POST***
                if(eng2.lt.eng1) then 
                    x(1) = xlef 
                    x(2) = xlef 
                    x(3) = x03 
                    y(1) = y02 
                    y(2) = yrig 
                    y(3) = yrig 
                    call betmlK(x,y,3,nx,rr,gg,bb)
                    x(1) = x01
                    x(2) = xrig
                    x(3) = xrig
                    x(4) = x(6)
                    y(1) = ylef
                    y(2) = y04
                    y(3) = y(5)
                    y(4) = ylef 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                else
                    x(1) = xlef
                    x(2) = xlef 
                    x(3) = x03 
                    x(4) = xrig
                    x(5) = xrig
                    x(6) = x(6)
                    x(7) = x01
                    y(1) = y02
                    y(2) = yrig
                    y(3) = yrig
                    y(4) = y04
                    y(5) = y(5)
                    y(6) = ylef
                    y(7) = ylef 
                    call betmlK(x,y,7,nx,rr,gg,bb)
                endif 
                endif 
        ! *       -------------------------------------------------------------
                elseif(((s1.lt.cst).or.(s1.gt.ced)).and.&
                       ((s2.lt.cst).or.(s2.gt.ced)).and.&
                       ((s3.lt.cst).or.(s3.gt.ced)).and.&
                       ((s4.ge.cst).and.(s4.le.ced))) then 
                if((s1.gt.ced).and.(s2.gt.ced).and.(s3.lt.cst)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                    y(1) = ylef + (ddx/dxa)*py 
                    ddx = abs(s1-cst)
                    y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x(3) = xlef + (ddx/dxa)*px
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                    y(5) = ylef + (ddx/dxa)*py 
                    x(1) = xlef
                    x(2) = xlef 
                    x(3) = x(3)
                    x(4) = xrig
                    x(5) = xrig
                    y(1) = y(1)
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig 
                    y(5) = y(5)
                    call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.gt.ced).and.(s3.lt.cst)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x(1) = xlef + (ddx/dxa)*px
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x(2) = xlef + (ddx/dxa)*px
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                    y(4) = ylef + (ddx/dxa)*py 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x(5) = xlef + (ddx/dxa)*px
                    x(1) = x(1)
                    x(2) = x(2) 
                    x(3) = xrig
                    x(4) = xrig
                    x(5) = x(5)
                    y(1) = ylef
                    y(2) = yrig
                    y(3) = yrig
                    y(4) = y(4) 
                    y(5) = ylef 
                    call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.lt.cst).and.(s3.lt.cst)) then 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x(1) = xlef + (ddx/dxa)*px
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                    y(3) = ylef + (ddx/dxa)*py 
                    x(1) = x(1)
                    x(2) = xrig 
                    x(3) = xrig
                    y(1) = yrig
                    y(2) = yrig
                    y(3) = y(3)
                    call betmlK(x,y,3,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s2.gt.ced).and.(s3.gt.ced)) then 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x(1) = xlef + (ddx/dxa)*px
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                    y(3) = ylef + (ddx/dxa)*py 
                    x(1) = x(1)
                    x(2) = xrig 
                    x(3) = xrig
                    y(1) = yrig
                    y(2) = yrig
                    y(3) = y(3)
                    call betmlK(x,y,3,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.lt.cst).and.(s3.gt.ced)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                    y(1) = ylef + (ddx/dxa)*py 
                    ddx = abs(s1-ced)
                    y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x(3) = xlef + (ddx/dxa)*px
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                    y(5) = ylef + (ddx/dxa)*py 
                    x(1) = xlef
                    x(2) = xlef 
                    x(3) = x(3)
                    x(4) = xrig
                    x(5) = xrig
                    y(1) = y(1)
                    y(2) = y(2)
                    y(3) = yrig
                    y(4) = yrig
                    y(5) = y(5)
                    call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s2.lt.cst).and.(s3.gt.ced)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                x(1) = xlef + (ddx/dxa)*px
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                x(2) = xlef + (ddx/dxa)*px
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                y(4) = ylef + (ddx/dxa)*py 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                x(5) = xlef + (ddx/dxa)*px
                    x(1) = x(1)
                    x(2) = x(2) 
                    x(3) = xrig
                    x(4) = xrig
                    x(5) = x(5)
                    y(1) = ylef 
                    y(2) = yrig
                    y(3) = yrig
                    y(4) = y(4)
                    y(5) = ylef
                    call betmlK(x,y,5,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.gt.ced).and.(s3.gt.ced)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x01 = xlef + (ddx/dxa)*px
                    y01 = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-ced)
                    x02 = xlef
                    y02 = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x03 = xlef + (ddx/dxa)*px 
                    y03 = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-ced)
                    x04 = xrig 
                    y04 = ylef + (ddx/dxa)*py
                    eng1 = (x01-x02)**2 + (y01-y02)**2 
                    eng2 = (x01-x04)**2 + (y01-y04)**2 

                    dxa = abs(s3-s1)
                    ddx = abs(s1-cst)
                    y05 = ylef + (ddx/dxa)*py 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x06 = xlef + (ddx/dxa)*px
        ! ***POST***
                if(eng1.le.eng2) then 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x01
                    x(4) = x06
                    y(1) = y05
                    y(2) = y02
                    y(3) = ylef
                    y(4) = ylef 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                    x(1) = x03
                    x(2) = xrig
                    x(3) = xrig 
                    y(1) = yrig
                    y(2) = yrig
                    y(3) = y04 
                    call betmlK(x,y,3,nx,rr,gg,bb)
                    else
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x03
                    x(4) = xrig
                    x(5) = xrig
                    x(6) = x01
                    x(7) = x06
                    y(1) = y05
                    y(2) = y02
                    y(3) = yrig
                    y(4) = yrig
                    y(5) = y04
                    y(6) = ylef
                    y(7) = ylef
                    call betmlK(x,y,7,nx,rr,gg,bb)
                    endif 
                elseif((s1.gt.ced).and.(s2.lt.cst).and.(s3.lt.cst)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x01 = xlef + (ddx/dxa)*px
                    y01 = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-cst)
                    x02 = xlef
                    y02 = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x03 = xlef + (ddx/dxa)*px 
                    y03 = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-cst)
                    x04 = xrig 
                    y04 = ylef + (ddx/dxa)*py
                    eng1 = (x01-x02)**2 + (y01-y02)**2 
                    eng2 = (x01-x04)**2 + (y01-y04)**2 

                    dxa = abs(s3-s1)
                    ddx = abs(s1-ced)
                    y05 = ylef + (ddx/dxa)*py 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x06 = xlef + (ddx/dxa)*px
        ! ***POST***
                if(eng1.le.eng2) then 
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x01
                    x(4) = x06
                    y(1) = y05
                    y(2) = y02
                    y(3) = ylef
                    y(4) = ylef 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                    x(1) = x03
                    x(2) = xrig
                    x(3) = xrig 
                    y(1) = yrig
                    y(2) = yrig
                    y(3) = y04 
                    call betmlK(x,y,3,nx,rr,gg,bb)
                    else
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x03
                    x(4) = xrig
                    x(5) = xrig
                    x(6) = x01
                    x(7) = x06
                    y(1) = y05
                    y(2) = y02
                    y(3) = yrig
                    y(4) = yrig
                    y(5) = y04
                    y(6) = ylef
                    y(7) = ylef
                    call betmlK(x,y,7,nx,rr,gg,bb)
                    endif 
                endif
                elseif(((s1.lt.cst).or.(s1.gt.ced)).and.&
                       ((s2.lt.cst).or.(s2.gt.ced)).and.&
                       ((s3.lt.cst).or.(s3.gt.ced)).and.&
                       ((s4.lt.cst).or.(s4.gt.ced))) then 
                if((s1.gt.ced).and.(s2.gt.ced).and.&
                   (s3.lt.cst).and.(s4.gt.ced)) then 
                    dxa = abs(s3-s1)
                    ddx = abs(s1-ced)
                    y(1) = ylef + (ddx/dxa)*py 
                    ddx = abs(s1-cst)
                    y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x(3) = xlef + (ddx/dxa)*px
                    ddx = abs(s3-ced)
                    x(4) = xlef + (ddx/dxa)*px
                        x(1) = xlef
                        x(2) = xlef 
                        x(3) = x(3)
                        x(4) = x(4)
                        y(1) = y(1)
                        y(2) = y(2)
                        y(3) = yrig
                        y(4) = yrig 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s2.gt.ced).and.&
                      (s3.gt.ced).and.(s4.lt.cst)) then 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x(1) = xlef + (ddx/dxa)*px
                    ddx = abs(s3-cst)
                    x(2) = xlef + (ddx/dxa)*px
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                    y(3) = ylef + (ddx/dxa)*py 
                    ddx = abs(s2-ced)
                    y(4) = ylef + (ddx/dxa)*py 
                        x(1) = x(1)
                        x(2) = x(2) 
                        x(3) = xrig
                        x(4) = xrig 
                        y(1) = yrig
                        y(2) = yrig 
                        y(3) = y(3)
                        y(4) = y(4) 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.gt.ced).and.&
                       (s3.gt.ced).and.(s4.gt.ced)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                    y(1) = ylef + (ddx/dxa)*py 
                    ddx = abs(s1-ced)
                    y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x(3) = xlef + (ddx/dxa)*px
                    ddx = abs(s1-cst)
                    x(4) = xlef + (ddx/dxa)*px
                        x(1) = xlef
                        x(2) = xlef 
                        x(3) = x(3)
                        x(4) = x(4)
                        y(1) = y(1)
                        y(2) = y(2)
                        y(3) = ylef
                        y(4) = ylef 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s2.lt.cst).and.&
                       (s3.gt.ced).and.(s4.gt.ced)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x(1) = xlef + (ddx/dxa)*px
                    ddx = abs(s1-cst)
                    x(2) = xlef + (ddx/dxa)*px
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                    y(3) = ylef + (ddx/dxa)*py 
                    ddx = abs(s2-ced)
                    y(4) = ylef + (ddx/dxa)*py 
                        x(1) = x(1)
                        x(2) = x(2) 
                        x(3) = xrig
                        x(4) = xrig 
                        y(1) = ylef
                        y(2) = ylef 
                        y(3) = y(3)
                        y(4) = y(4) 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.lt.cst).and.&
                       (s3.gt.ced).and.(s4.gt.ced)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                    y(1) = ylef + (ddx/dxa)*py 
                    ddx = abs(s1-ced)
                    y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                    y(3) = ylef + (ddx/dxa)*py 
                    ddx = abs(s2-cst)
                    y(4) = ylef + (ddx/dxa)*py 
                        x(1) = xlef
                        x(2) = xlef 
                        x(3) = xrig
                        x(4) = xrig 
                        y(1) = y(1)
                        y(2) = y(2)
                        y(3) = y(3)
                        y(4) = y(4) 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s2.lt.cst).and.&
                       (s3.gt.ced).and.(s4.lt.cst)) then 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x(1) = xlef + (ddx/dxa)*px
                    ddx = abs(s3-cst)
                    x(2) = xlef + (ddx/dxa)*px
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x(3) = xlef + (ddx/dxa)*px
                    ddx = abs(s1-ced)
                    x(4) = xlef + (ddx/dxa)*px
                        x(1) = x(1)
                        x(2) = x(2) 
                        x(3) = x(3)
                        x(4) = x(4) 
                        y(1) = yrig
                        y(2) = yrig 
                        y(3) = ylef
                        y(4) = ylef  
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s2.gt.ced).and.&
                       (s3.lt.cst).and.(s4.lt.cst)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                    y(1) = ylef + (ddx/dxa)*py 
                    ddx = abs(s1-cst)
                    y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s2-s4)
                    ddx = abs(s2-cst)
                    y(3) = ylef + (ddx/dxa)*py 
                    ddx = abs(s2-ced)
                    y(4) = ylef + (ddx/dxa)*py 
                        x(1) = xlef
                        x(2) = xlef 
                        x(3) = xrig
                        x(4) = xrig 
                        y(1) = y(1)
                        y(2) = y(2)
                        y(3) = y(3)
                        y(4) = y(4) 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.gt.ced).and.&
                       (s3.lt.cst).and.(s4.gt.ced)) then 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x(1) = xlef + (ddx/dxa)*px
                    ddx = abs(s3-ced)
                    x(2) = xlef + (ddx/dxa)*px
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x(3) = xlef + (ddx/dxa)*px
                    ddx = abs(s1-cst)
                    x(4) = xlef + (ddx/dxa)*px
                        x(1) = x(1)
                        x(2) = x(2) 
                        x(3) = x(3)
                        x(4) = x(4) 
                        y(1) = yrig
                        y(2) = yrig 
                        y(3) = ylef
                        y(4) = ylef  
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.gt.ced).and.&
                       (s3.lt.cst).and.(s4.lt.cst)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x(1) = xlef + (ddx/dxa)*px
                    ddx = abs(s1-ced)
                    x(2) = xlef + (ddx/dxa)*px
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                    y(3) = ylef + (ddx/dxa)*py 
                    ddx = abs(s2-cst)
                    y(4) = ylef + (ddx/dxa)*py 
                        x(1) = x(1)
                        x(2) = x(2) 
                        x(3) = xrig
                        x(4) = xrig
                        y(1) = ylef
                        y(2) = ylef 
                        y(3) = y(3)
                        y(4) = y(4)   
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.gt.ced).and.(s2.lt.cst).and.&
                       (s3.lt.cst).and.(s4.lt.cst)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-ced)
                    y(1) = ylef + (ddx/dxa)*py 
                    ddx = abs(s1-cst)
                    y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x(3) = xlef + (ddx/dxa)*px
                    ddx = abs(s1-ced)
                    x(4) = xlef + (ddx/dxa)*px
                        x(1) = xlef
                        x(2) = xlef 
                        x(3) = x(3)
                        x(4) = x(4)
                        y(1) = y(1)
                        y(2) = y(2) 
                        y(3) = ylef
                        y(4) = ylef    
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.lt.cst).and.&
                      (s3.lt.cst).and.(s4.gt.ced)) then 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x(1) = xlef + (ddx/dxa)*px
                    ddx = abs(s3-ced)
                    x(2) = xlef + (ddx/dxa)*px
                    dxa = abs(s2-s4)
                    ddx = abs(s2-ced)
                    y(3) = ylef + (ddx/dxa)*py 
                    ddx = abs(s2-cst)
                    y(4) = ylef + (ddx/dxa)*py 
                        x(1) = x(1)
                        x(2) = x(2) 
                        x(3) = xrig
                        x(4) = xrig
                        y(1) = yrig
                        y(2) = yrig 
                        y(3) = y(3)
                        y(4) = y(4)    
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.lt.cst).and.&
                       (s3.gt.ced).and.(s4.lt.cst)) then 
                    dxa = abs(s1-s3)
                    ddx = abs(s1-cst)
                    y(1) = ylef + (ddx/dxa)*py 
                    ddx = abs(s1-ced)
                    y(2) = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x(3) = xlef + (ddx/dxa)*px
                    ddx = abs(s3-cst)
                    x(4) = xlef + (ddx/dxa)*px
                        x(1) = xlef
                        x(2) = xlef 
                        x(3) = x(3)
                        x(4) = x(4)
                        y(1) = y(1)
                        y(2) = y(2) 
                        y(3) = yrig
                        y(4) = yrig     
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((s1.lt.cst).and.(s2.gt.ced).and.&
                       (s3.gt.ced).and.(s4.lt.cst)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x01u = xlef + (ddx/dxa)*px
                    y01u = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-ced)
                    x02u = xlef
                    y02u = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x03u = xlef + (ddx/dxa)*px 
                    y03u = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-ced)
                    x04u = xrig 
                    y04u = ylef + (ddx/dxa)*py
                    engu1 = (x01u-x02u)**2 + (y01u-y02u)**2 
                    engu2 = (x01u-x04u)**2 + (y01u-y04u)**2 

                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x01d = xlef + (ddx/dxa)*px
                    y01d = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-cst)
                    x02d = xlef
                    y02d = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x03d = xlef + (ddx/dxa)*px 
                    y03d = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-cst)
                    x04d = xrig 
                    y04d = ylef + (ddx/dxa)*py
                    engd1 = (x01d-x02d)**2 + (y01d-y02d)**2 
                    engd2 = (x01d-x04d)**2 + (y01d-y04d)**2 
                    
        ! cc             engu3 = (x01u-x03u)**2 + (y01u-y03u)**2 
        ! cc             engu4 = (x02u-x03u)**2 + (y02u-y03u)**2 
        ! cc             engu5 = (x02u-x04u)**2 + (y02u-y04u)**2 
        ! cc             engu6 = (x03u-x04u)**2 + (y03u-y04u)**2 

        ! cc             engd3 = (x01d-x03d)**2 + (y01d-y03d)**2 
        ! cc             engd4 = (x02d-x03d)**2 + (y02d-y03d)**2 
        ! cc             engd5 = (x02d-x04d)**2 + (y02d-y04d)**2 
        ! cc             engd6 = (x03d-x04d)**2 + (y03d-y04d)**2 

        ! cc            write(*,*) 'u' 
        ! cc            write(*,*) engu1,engu2,engu3 
        ! cc            write(*,*) engu4,engu5,engu6 
        ! cc            write(*,*) 'd'
        ! cc            write(*,*) engd1,engd2,engd3 
        ! cc            write(*,*) engd4,engd5,engd6 
        ! ***POST***
                if((engu1.le.engu2).and.(engd1.le.engd2)) then  
                        x(1) = xlef
                        x(2) = xlef
                        x(3) = x01u
                        x(4) = x01d
                        y(1) = y02d
                        y(2) = y02u
                        y(3) = ylef
                        y(4) = ylef 
                    call betmlK(x,y,4,nx,rr,gg,bb)
                        x(1) = x03u
                        x(2) = x03d
                        x(3) = xrig
                        x(4) = xrig
                        y(1) = yrig
                        y(2) = yrig
                        y(3) = y04d
                        y(4) = y04u  
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((engu1.le.engu2).and.(engd1.gt.engd2)) then  
                    write(*,*) 'subroutine contor-or.f 2148'
                elseif((engu1.gt.engu2).and.(engd1.le.engd2)) then  
                        x(1) = xlef
                        x(2) = xlef
                        x(3) = x03u
                        x(4) = x03d
                        x(5) = xrig
                        x(6) = xrig
                        x(7) = x01u
                        x(8) = x01d 
                        y(1) = y02d
                        y(2) = y02u
                        y(3) = yrig
                        y(4) = yrig
                        y(5) = y04d
                        y(6) = y04u
                        y(7) = ylef 
                        y(8) = ylef 
                    call betmlK(x,y,8,nx,rr,gg,bb)
                elseif((engu1.gt.engu2).and.(engd1.gt.engd2)) then  
                        x(1) = xlef
                        x(2) = xlef
                        x(3) = x03u
                        x(4) = x03d
                        y(1) = y02d
                        y(2) = y02u
                        y(3) = yrig
                        y(4) = yrig  
                    call betmlK(x,y,4,nx,rr,gg,bb)
                        x(1) = xrig
                        x(2) = xrig
                        x(3) = x01u
                        x(4) = x01d
                        y(1) = y04d
                        y(2) = y04u
                        y(3) = ylef
                        y(4) = ylef   
                    call betmlK(x,y,4,nx,rr,gg,bb)
                endif 
                elseif((s1.gt.ced).and.(s2.lt.cst).and.&
                       (s3.lt.cst).and.(s4.gt.ced)) then 
                    dxa = abs(s1-s2)
                    ddx = abs(s1-ced)
                    x01u = xlef + (ddx/dxa)*px
                    y01u = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-ced)
                    x02u = xlef
                    y02u = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-ced)
                    x03u = xlef + (ddx/dxa)*px 
                    y03u = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-ced)
                    x04u = xrig 
                    y04u = ylef + (ddx/dxa)*py
                    engu1 = (x01u-x02u)**2 + (y01u-y02u)**2 
                    engu2 = (x01u-x04u)**2 + (y01u-y04u)**2 

                    dxa = abs(s1-s2)
                    ddx = abs(s1-cst)
                    x01d = xlef + (ddx/dxa)*px
                    y01d = ylef  
                    dxa = abs(s3-s1)
                    ddx = abs(s1-cst)
                    x02d = xlef
                    y02d = ylef + (ddx/dxa)*py 
                    dxa = abs(s3-s4)
                    ddx = abs(s3-cst)
                    x03d = xlef + (ddx/dxa)*px 
                    y03d = yrig 
                    dxa = abs(s4-s2)
                    ddx = abs(s2-cst)
                    x04d = xrig 
                    y04d = ylef + (ddx/dxa)*py
                    engd1 = (x01d-x02d)**2 + (y01d-y02d)**2 
                    engd2 = (x01d-x04d)**2 + (y01d-y04d)**2 

        ! ***POST***
                if((engu1.le.engu2).and.(engd1.le.engd2)) then  
                    x(1) = xlef
                    x(2) = xlef 
                    x(3) = x01d
                    x(4) = x01u
                    y(1) = y02u
                    y(2) = y02d
                    y(3) = ylef
                    y(4) = ylef  
                    call betmlK(x,y,4,nx,rr,gg,bb)
                    x(1) = x03d
                    x(2) = x03u 
                    x(3) = xrig
                    x(4) = xrig
                    y(1) = yrig
                    y(2) = yrig 
                    y(3) = y04u
                    y(4) = y04d   
                    call betmlK(x,y,4,nx,rr,gg,bb)
                elseif((engu1.le.engu2).and.(engd1.gt.engd2)) then  
                    x(1) = xlef
                    x(2) = xlef
                    x(3) = x03d
                    x(4) = x03u
                    x(5) = xrig
                    x(6) = xrig
                    x(7) = x01d
                    x(8) = x01u
                    y(1) = y02u
                    y(2) = y02d
                    y(3) = yrig
                    y(4) = yrig
                    y(5) = y04u
                    y(6) = y04d
                    y(7) = ylef
                    y(8) = ylef 
                    call betmlK(x,y,8,nx,rr,gg,bb)
                elseif((engu1.gt.engu2).and.(engd1.le.engd2)) then  
                    write(*,*) 'subroutine contor-or.f 2148'
                elseif((engu1.gt.engu2).and.(engd1.gt.engd2)) then  
                    x(1) = xlef
                    x(2) = xlef 
                    x(3) = x03d
                    x(4) = x03u
                    y(1) = y02u
                    y(2) = y02d 
                    y(3) = yrig
                    y(4) = yrig   
                    call betmlK(x,y,4,nx,rr,gg,bb)
                    x(1) = x01u
                    x(2) = xrig 
                    x(3) = xrig
                    x(4) = x01d
                    y(1) = ylef
                    y(2) = y04u 
                    y(3) = y04d
                    y(4) = ylef    
                    call betmlK(x,y,4,nx,rr,gg,bb)
                    endif
                    endif
                endif
        10    continue 
        20    continue
            call color(0)

            do i=ist , ied 
            do j=jst , jed 
                if(is(i,j).eq.2) is(i,j) = 1 
            enddo 
            enddo 

            return    
        ! c
        ! c     Programed by Hiroshi Kuroda (2003/02/22)
        ! c
        ! ***********************************************************************
    end 
    subroutine pscmaskK(px,py,imask,is,ie,js,je,ix,iy,r,g,b)
        ! ***********************************************************************
                dimension imask(ix,iy)

            do 20 i=is,ie
                lp = 0 
            do 10 j=js,je 
                if(imask(i,j).eq.0.and.lp.eq.0) then
                    lp = 1 
                    xx = px*(i-is)
                    yy = py*(j-js)
                    xlef = xx 
                    ylef = yy 
                elseif(imask(i,j).eq.1.and.lp.eq.1) then 
                    lp = 0 
                    xx = px*(i-is)
                    yy = py*(j-js)
                    xrig = xx + px 
                    yrig = yy  
                    call betsqK(xlef,ylef,xrig,yrig,r,g,b)  
                elseif(imask(i,j).eq.0 .and. lp.eq.1 .and. j.eq.je) then 
                    xx = px*(i-is)
                    yy = py*(j-js)
                    xrig = xx + px 
                    yrig = yy + py 
                    call betsqK(xlef,ylef,xrig,yrig,r,g,b)  
                elseif(imask(i,j).eq.0 .and. lp.eq.0 .and. j.eq.je) then 
                    xx = px*(i-is)
                    yy = py*(j-js)
                    xlef = xx 
                    ylef = yy 
                    xrig = xx + px 
                    yrig = yy + py 
                    call betsqK(xlef,ylef,xrig,yrig,r,g,b)  
                endif
        10   continue
        20   continue
            return
    end 
    subroutine betsqK(xlef,ylef,xrig,yrig,rr,gg,bb)
        ! ***********************************************************************
        ! c     xlef ==> Hidari Shita No x Zahyou
        ! c     ylef ==> Hidari Shita No y Zahyou
        ! c     xrig ==> Migi Ue No x Zahyou
        ! c     yrig ==> Migi Ue No y Zahyou
            call rgbK(rr,gg,bb)
            call plot(xlef,ylef,3)
            call plot(xrig,ylef,2)
            call plot(xrig,yrig,2)
            call plot(xlef,yrig,2)
            call plot(xlef,ylef,2)
            write(ounit,*) 'closepath'
            write(ounit,*) 'fill'
            call color(0)
            return
    end
    subroutine symbol(x, y, h, isymb, ang, n)
        implicit none
        character(len=*), intent(in) :: isymb
        integer, intent(in), optional :: n
        real, intent(in) :: x, y, h
        real, intent(in), optional :: ang
        character(len=256) :: segment
        integer :: i, iend, istart,count = 0,lol
    
        if (present(n)) lol = 420 ! no need to specify length anymore
        if (len_trim(isymb) > 256) print*, 'ARE YOU WRITING A BOOK??'
        write(ounit,*) "% begin symbol"
        write(ounit,*) "fo"
        write(ounit, 10) h
        10  format(f8.4, " sf")
        write(ounit,*) "se"
    
        call plot(x,y,-3)
        if(present(ang))write(ounit, '(f9.4, 2x, a4)') ang, ' ro '
        istart = 1;count=0
        ! print*,len_trim(isymb)
        do i = 1, len_trim(isymb)
            if (isymb(i:i) == ';' .or. i == len_trim(isymb)) then
                if (isymb(i:i) == ';') then
                    iend = i - 1
                else
                    iend = i
                end if
                segment = isymb(istart:iend)
                write(ounit, '(3a)') "(", trim(segment), ") sh"
                if (isymb(i:i) == ';')then;call plot(0.,-h,-3);count = count+1;end if
                istart = iend +2
            end if
        end do
        call plot(0.,h*real(count),-3)
        if(present(ang))write(ounit, '(f9.4, 2x, a4)') -ang, ' ro '
        call plot(-x,-y,-3)
        write(ounit,*) "% end symbol"
        return
    end 
    subroutine symbolc(x,y,h,isymb,ang,n)
        implicit none
        character(len=*), intent(in) :: isymb
        integer, intent(in), optional :: n
        real, intent(in) :: x, y, h
        real,intent(in),optional :: ang
        character(len=256) :: segment
        integer :: i, iend, istart,count = 0,lol
        if (present(n)) lol = 420 ! no need to specify length anymore
        if (len_trim(isymb) > 256) print*, 'ARE YOU WRITING A BOOK??'
        write(ounit,*) "% begin symbol"
        ! do i = 1, len_trim(adjustl((isymb)))
        !     if(iachar(isymb(i:i))<=127 )then
                write(ounit,*) "fo"
            ! else
                ! write(ounit,*) "jfo"
        !     end if
        ! end do
            ! print*,len_trim(isymb),isymb
        write(ounit, 10) h
        10  format(f8.4, " sf")
        write(ounit,*) "se"
    
        call plot(x,y,-3)
        if(present(ang))write(ounit, '(f9.4, 2x, a4)') ang, ' ro '
        istart = 1;count=0
        ! print*,len_trim(isymb)
        do i = 1, len_trim(isymb)
            if (isymb(i:i) == ';' .or. i == len_trim(isymb)) then
                if (isymb(i:i) == ';') then
                    iend = i - 1
                else
                    iend = i
                end if
                segment = isymb(istart:iend)
                write(ounit,*) "(",trim(segment),") stringwidth pop " 
                write(ounit,*) 'neg 2 div 0 rmoveto '
                write(ounit, '(3a)') "(", trim(segment), ") sh"
                if (isymb(i:i) == ';')then;call plot(0.,-h,-3);count = count+1
                endif
                istart = iend +2
            end if
        end do
        call plot(0.,h*real(count),-3)
        if(present(ang))write(ounit, '(f9.4, 2x, a4)') -ang, ' ro '
        call plot(-x,-y,-3)
        write(ounit,*) "% end symbolc"
        return
    end     
    subroutine symbolr(x,y,h,isymb,ang,n)
        implicit none
        character(len=*), intent(in) :: isymb
        integer, intent(in), optional :: n
        real, intent(in) :: x, y, h
        real,intent(in),optional :: ang
        character(len=256) :: segment
        integer :: i, iend, istart,count = 0,lol
        if (present(n)) lol = 420 ! no need to specify length anymore
        if (len_trim(isymb) > 256) print*, 'ARE YOU WRITING A BOOK??'
        write(ounit,*) "% begin symbol"
        write(ounit,*) "fo"
        write(ounit, 10) h
        10  format(f8.4, " sf")
        write(ounit,*) "se"

        call plot(x,y,-3)
        if(present(ang))write(ounit, '(f9.4, 2x, a4)') ang, ' ro '
        istart = 1;count=0
        ! print*,len_trim(isymb)
        do i = 1, len_trim(isymb)
            if (isymb(i:i) == ';' .or. i == len_trim(isymb)) then
                if (isymb(i:i) == ';') then
                    iend = i - 1
                else
                    iend = i
                end if
                segment = isymb(istart:iend)
                write(ounit,*) "(",trim(segment),") stringwidth pop " 
                write(ounit,*) 'neg 0 rmoveto '
                write(ounit, '(3a)') "(", trim(segment), ") sh"
                if (isymb(i:i) == ';')then;call plot(0.,-h,-3);count = count+1;endif
                istart = iend +2
            end if
        end do
        call plot(0.,h*real(count),-3)
        if(present(ang))write(ounit, '(f9.4, 2x, a4)') -ang, ' ro '
        call plot(-x,-y,-3)
        write(ounit,*) "% end symbolr"
        return
    end
    subroutine openlog(omit,basename)
        implicit none
        character(len=*), intent(in),optional :: basename
        character(len=256) :: filename,date_str,time_str,date,status
        logical,intent(in),optional :: omit
        integer :: values(8)
        if(logstat)then
            print*, 'log file is already open'
            return
        end if
        if(present(basename))then
            filename = 'log/'//trim(adjustl(basename))
        else 
            filename = 'log/output.log'
        end if
        if(present(omit))then
            if(omit)then
                status = 'replace'
            else 
                status = 'unknown'
            end if
        else;status = 'unknown'
        end if
        open(tolog, file=filename, status=trim(adjustl(status)),action = 'write',position='append')
        logstat = .true.
        call date_and_time(values = values)
            write(date_str, '(i4.4, i2.2, i2.2)') values(1), values(2), values(3)
            write(time_str, '(i2.2, i2.2, i2.2)') values(5), values(6), values(7)
            date = trim(date_str) // 'T' // trim(time_str) // 'Z'
            write(tolog,*) 'Log file opened on ', date   
        return
    end
    subroutine closelog()
        integer :: values(8)
        character(len=256) :: date_str,time_str,date
        if(logstat)then
            call date_and_time(values = values)
            write(date_str, '(i4.4, i2.2, i2.2)') values(1), values(2), values(3)
            write(time_str, '(i2.2, i2.2, i2.2)') values(5), values(6), values(7)
            date = trim(date_str) // 'T' // trim(time_str) // 'Z'
            write(tolog,*) 'Log file closed on ', date
            close(tolog)
            logstat = .false.
            return
        else
            print*, 'log file is not open'
        end if
    end
        
end module origin

module oldsubs
    use origin
    implicit none
    contains
    ! creating temp_5,sal_5 using original data
    subroutine temsal(temp_5,sal_5)
        implicit none
        integer,parameter::years = 15, months = 12,lines = 2, stations = 9,depth = 1000
        real,dimension(years,months,lines,stations,depth),intent(out)::temp_5,sal_5
        character::yyyy*9,tempfile*999,salfile*999,line*20,mm*9
        integer::y,m,l,st,d

        102 format(9(f9.4))
        do m = 1, months
            write(mm,'(i2.2)')m
            do l = 1,lines
                if(l==1) then;line = 'N-Line'
                else;line = 'S-Line';end if
                    do y = 1, years
                        write(yyyy,'(i4.4)')y+2008
                        tempfile = '../Data/Yuta_unedited/'//'/'//trim(line)//'/'//trim(mm)//'/01tem'//trim(yyyy)//'.csv'
                        salfile = '../Data/Yuta_unedited/'//'/'//trim(line)//'/'//trim(mm)//'/01sal'//trim(yyyy)//'.csv'
                        open(11,file = tempfile,status = 'old', action = 'read')
                        do d = 1, depth
                            read(11,102)(temp_5(y,m,l,st,d),st = 1,stations)
                        end do
                        close(11)
                        open(77,file = salfile,status ='old',action = 'read')
                        do d = 1,depth
                            read(77,102)(sal_5(y,m,l,st,d),st = 1,stations)
                        end do
                        close(77)
                    end do
            end do
        end do
    end subroutine
    ! creating potemp and sal from 51db median filtered data
    subroutine potempsal_51(potemp_5,sal_5)
        implicit none
        integer,parameter::years = 15, months = 12,lines = 2, stations = 9,depth = 400
        real,dimension(years,months,lines,stations,depth),intent(out)::potemp_5,sal_5
        character::yyyy*9,tempfile*999,salfile*999,line*20,mm*9
        integer::y,m,l,st,d

        102 format(9(f9.4))
        do m = 1, months
            write(mm,'(i2.2)')m
            do l = 1,lines
                if(l==1) then;line = 'N-Line'
                else;line = 'S-Line';end if
                    do y = 1, years
                        write(yyyy,'(i4.4)')y+2008
                        tempfile = '../Data/51_Median'//'/'//trim(line)//'/'//trim(mm)//'/'//'51potemp'//trim(yyyy)//'.csv'  
                        salfile = '../Data/51_Median'//'/'//trim(line)//'/'//trim(mm)//'/51sal'//trim(yyyy)//'.csv'
                        open(11,file = tempfile,status = 'old', action = 'read')
                        do d = 1, depth
                            read(11,102)(potemp_5(y,m,l,st,d),st = 1,stations)
                        end do
                        close(11)
                        open(77,file = salfile,status ='old',action = 'read')
                        do d = 1,depth
                            read(77,102)(sal_5(y,m,l,st,d),st = 1,stations)
                        end do
                        close(77)
                    end do
            end do
        end do
        
    end subroutine
    ! creating potemp and sal from 25db median filtered data
    subroutine potempsal_25(potemp_5,sal_5)
        implicit none
        integer,parameter::years = 15, months = 12,lines = 2, stations = 9,depth = 400
        real,dimension(years,months,lines,stations,depth),intent(out)::potemp_5,sal_5
        character::yyyy*9,tempfile*999,salfile*999,line*20,mm*9
        integer::y,m,l,st,d

        102 format(9(f9.4))
        do m = 1, months
            write(mm,'(i2.2)')m
            do l = 1,lines
                if(l==1) then;line = 'N-Line'
                else;line = 'S-Line';end if
                    do y = 1, years
                        write(yyyy,'(i4.4)')y+2008
                        tempfile = '../Data/25_Median'//'/'//trim(line)//'/'//trim(mm)//'/25potemp'//trim(yyyy)//'.csv'
                        salfile = '../Data/25_Median'//'/'//trim(line)//'/'//trim(mm)//'/25sal'//trim(yyyy)//'.csv'
                        open(11,file = tempfile,status = 'old', action = 'read')
                        do d = 1, depth
                            read(11,102)(potemp_5(y,m,l,st,d),st = 1,stations)
                        end do
                        close(11)
                        open(77,file = salfile,status ='old',action = 'read')
                        do d = 1,depth
                            read(77,102)(sal_5(y,m,l,st,d),st = 1,stations)
                        end do
                        close(77)
                    end do
            end do
        end do
        
    end subroutine
                                                !ABOVE DATA IS NOT REALLY FOR USE!

                                                ! SUBROUTINES FOR DATA OBTAINMENT !

    ! from a rectangle numeric file to a 2D array
    subroutine csv2array(filename,format,column_quan,row_quan,twoD_array)
        implicit none
        real,dimension(column_quan,row_quan),intent(out)::twoD_array
        integer,intent(in)::column_quan,row_quan
        character,intent(in)::filename*999,format*99
        integer::c,r,ios

        open(11,file = filename,status = 'old',action='read')
        do r = 1, row_quan
            read(11,format,iostat=ios)(twoD_array(c,r),c = 1, column_quan)
            if(ios /= 0) then
                exit
            else;end if
        end do
        close(11)
    end subroutine
    ! calibrated data for potempsal_51
    subroutine calibrated_data51(potemp_c5,sal_c5)
        implicit none
        integer,parameter:: years = 15, months = 12, lines = 2, stations = 9, depth = 400
        real,dimension(:,:,:,:,:),allocatable::potemp_5,sal_5
        ! real,dimension(years,months,lines,stations,depth)::potemp_5,sal_5
        real,dimension(years,months,lines,stations,depth),intent(out)::potemp_c5,sal_c5
        real,parameter::standard_sal_400 = 34.07
        real::diff,initial_num
        integer::y,m,l,st,d,initial_depth

        ! print*,'----------------------------------------'
        ! print*,'You are using an obsolete calibrated_data subroutine, the station labels do not match the array indices'
        ! print*,'----------------------------------------'
        allocate(potemp_5(years,months,lines,stations,depth))
        allocate(sal_5(years,months,lines,stations,depth))
        potemp_5 = 0.;sal_5 = 0.
        call potempsal_51(potemp_5,sal_5)
        ! print*,sal_5(1,8,1,7,1:depth)
        sal_5(1,12,2,5,1:depth)=0.;sal_5(1,12,2,6,1:depth)=0.;sal_5(1,12,2,7,1:depth)=0.!May 10 金サロ前 Sline dgf
        sal_5(2,4,1,4,1:depth)=0. ! 2010 april Nline station 4 erased
        sal_5(4,1:months,1:lines,1:stations,1:depth)=0. !2012 data all erased

        potemp_5(1,12,2,5,1:depth)=0.;potemp_5(1,12,2,6,1:depth)=0.;potemp_5(1,12,2,7,1:depth)=0.!May 10 金サロ前 Sline dgf
        potemp_5(2,4,1,4,1:depth)=0. ! 2010 april Nline station 4 erased
        potemp_5(4,1:months,1:lines,1:stations,1:depth)=0. !2012 data all erased

        !データが最初に存在する深さの数値を表面まで持ってくる。
        do y = 1, years 
            do m = 1, months
                do l = 1, lines
                    do st = 1,stations
                        if (potemp_5(y,m,l,st,50)/=0.) then
                            do d = 1,50
                                if (potemp_5(y,m,l,st,d) /= 0.) then
                                    initial_depth = d
                                    initial_num = potemp_5(y,m,l,st,d)
                                    exit
                                else;end if
                            end do
                        potemp_5(y,m,l,st,1:initial_depth-1) = initial_num
                        else !(if potemp_5(y,m,l,st,50)==0.)
                        end if
                        if (sal_5(y,m,l,st,50)/=0.) then
                            do d = 1,50
                                if (sal_5(y,m,l,st,d) /= 0.) then
                                    initial_depth = d
                                    initial_num = sal_5(y,m,l,st,d)
                                    exit
                                else;end if
                            end do
                        sal_5(y,m,l,st,1:initial_depth-1) = initial_num
                        else !(if sal_5(y,m,l,st,50)==0.)
                        end if
                    end do
                end do
            end do
        end do

        !底の塩分を全ての月において34.07にしてしまう
        !2023 august station 9 はデータが348までしかないので以下のプログラムにより欠測になる
        do y = 1, years
            do m = 1,months
                do l = 1,lines
                    do st = 1, stations
                        if (sal_5(y,m,l,st,depth)<30. .or. sal_5(y,m,l,st,depth)>35.) then
                            sal_5(y,m,l,st,1:depth) = 0.  !一列全部0 データが元から無い列も0だからこれに含まれる
                            potemp_5(y,m,l,st,1:depth) = 0.
                        else
                            diff = sal_5(y,m,l,st,depth) - standard_sal_400 
                        end if
                        do d = 1, depth
                            if(sal_5(y,m,l,st,d)/=0.) then
                            sal_5(y,m,l,st,d) = sal_5(y,m,l,st,d) - diff
                            else; end if
                        end do
                        !diff = 0.
                    end do 
                end do
            end do
        end do

        potemp_c5(1:years,1:months,1:lines,1:stations,1:depth) = potemp_5(1:years,1:months,1:lines,1:stations,1:depth)
        sal_c5(1:years,1:months,1:lines,1:stations,1:depth) = sal_5(1:years,1:months,1:lines,1:stations,1:depth)
        ! print*,sal_c5(1,8,1,7,1:depth)
        deallocate(potemp_5);deallocate(sal_5)
        
    end subroutine
    ! calibrated_data for potempsal_25
    subroutine calibrated_data25(potemp_c5,sal_c5)
        implicit none
        integer,parameter:: years = 15, months = 12, lines = 2, stations = 9, depth = 400
        real,dimension(:,:,:,:,:),allocatable::potemp_5,sal_5
        real,dimension(years,months,lines,stations,depth),intent(out)::potemp_c5,sal_c5
        real,parameter::standard_sal_400 = 34.07
        real::diff,initial_num
        integer::y,m,l,st,d,initial_depth

        ! print*,'----------------------------------------'
        ! print*,'You are using an obsolete calibrated_data subroutine, the station labels do not match the array indices'
        ! print*,'----------------------------------------'
        allocate(potemp_5(years,months,lines,stations,depth))
        allocate(sal_5(years,months,lines,stations,depth))
        potemp_5 = 0.;sal_5 = 0.
        call potempsal_25(potemp_5,sal_5)
        sal_5(1,12,2,5,1:depth)=0.;sal_5(1,12,2,6,1:depth)=0.;sal_5(1,12,2,7,1:depth)=0.!May 10 金サロ前 Sline dgf
        sal_5(2,4,1,4,1:depth)=0. ! 2010 april Nline station 4 erased
        sal_5(4,1:months,1:lines,1:stations,1:depth)=0. 

        potemp_5(1,12,2,5,1:depth)=0.;potemp_5(1,12,2,6,1:depth)=0.;potemp_5(1,12,2,7,1:depth)=0.!May 10 金サロ前 Sline dgf
        potemp_5(2,4,1,4,1:depth)=0. ! 2010 april Nline station 4 erased
        potemp_5(4,1:months,1:lines,1:stations,1:depth)=0. !2012 data all erased

        sal_5(5,5,2,6,272:279) = 34.0304    !途中で0が続く変な時があるSline 前後の値と同じにした

        !データが最初に存在する深さの数値を表面まで持ってくる。
        do y = 1, years 
            do m = 1, months
                do l = 1, lines
                    do st = 1,stations
                        if (potemp_5(y,m,l,st,50)/=0.) then
                            do d = 1,50
                                if (potemp_5(y,m,l,st,d) /= 0.) then
                                    initial_depth = d
                                    initial_num = potemp_5(y,m,l,st,d)
                                    exit
                                else;end if
                            end do
                        potemp_5(y,m,l,st,1:initial_depth-1) = initial_num
                        else !(if potemp_5(y,m,l,st,50)==0.)
                        end if
                        if (sal_5(y,m,l,st,50)/=0.) then
                            do d = 1,50
                                if (sal_5(y,m,l,st,d) /= 0.) then
                                    initial_depth = d
                                    initial_num = sal_5(y,m,l,st,d)
                                    exit
                                else;end if
                            end do
                        sal_5(y,m,l,st,1:initial_depth-1) = initial_num
                        else !(if sal_5(y,m,l,st,50)==0.)
                        end if
                    end do
                end do
            end do
        end do

        !底の塩分を全ての月において34.07としてしまう。
        !2023 august station 9 はデータが348までしかないので以下のプログラムにより欠測になる
        do y = 1, years
            do m = 1,months
                do l = 1,lines
                    do st = 1, stations
                        if (sal_5(y,m,l,st,depth)<30. .or. sal_5(y,m,l,st,depth)>35.) then
                            sal_5(y,m,l,st,1:depth) = 0.  !一列全部0 データが元から無い列も0だからこれに含まれる
                            potemp_5(y,m,l,st,1:depth) = 0.
                        else
                            diff = sal_5(y,m,l,st,depth) - standard_sal_400 
                        end if
                        do d = 1, depth
                            if(sal_5(y,m,l,st,d)/=0.) then
                            sal_5(y,m,l,st,d) = sal_5(y,m,l,st,d) - diff
                            else; end if
                        end do
                        !diff = 0.
                    end do 
                end do
            end do
        end do

        potemp_c5(1:years,1:months,1:lines,1:stations,1:depth) = potemp_5(1:years,1:months,1:lines,1:stations,1:depth)
        sal_c5(1:years,1:months,1:lines,1:stations,1:depth) = sal_5(1:years,1:months,1:lines,1:stations,1:depth)
        deallocate(potemp_5);deallocate(sal_5)
        
    end subroutine
    ! put geostrophic velocity into an array for 25 or 51db median filtered data
    subroutine geovel_array(medianfiltertype,geovel_5) 
        implicit none
        integer,parameter::years = 15, months = 12, lines = 2, stations = 9, depth = 400
        real,dimension(years,months,lines,stations,depth),intent(out)::geovel_5
        integer,intent(in)::medianfiltertype !25 or 51
        integer::y,m,l,st,d
        character::mm*9,aaaa*9,filename*999,line*9

        102 format(9(f9.4))
        if (medianfiltertype == 25) then
            do l = 1,lines
                if(l==1) then;line = 'N-Line';else; line='S-Line';end if
                do m = 1, months
                    write(mm,'(i2.2)')m
                    do y = 1, years
                        write(aaaa,'(i4.4)')y+2008
                        filename = '../Data/Geostrophy/'//trim(line)//'/'//trim(mm)//'/Velocity_25median'//trim(aaaa)//'.csv'
                        open(77,file = filename,status = 'old',action = 'read')
                        do d = 1,depth
                            read(77,102)(geovel_5(y,m,l,st,d),st = 1,stations)
                        end do
                    end do
                end do
            end do
        else if(medianfiltertype == 51) then
            do l = 1,lines
                if(l==1) then;line = 'N-Line';else; line='S-Line';end if
                do m = 1, months
                    write(mm,'(i2.2)')m
                    do y = 1, years
                        write(aaaa,'(i4.4)')y+2008
                        filename = '../Data/Geostrophy/'//trim(line)//'/'//trim(mm)//'/Velocity_51median'//trim(aaaa)//'.csv'
                        open(77,file = filename,status = 'old',action = 'read')
                        do d = 1,depth
                            read(77,102)(geovel_5(y,m,l,st,d),st = 1,stations)
                        end do
                    end do
                end do
            end do
        else;print*,'select either 25 or 51 for type of median filter'
        end if

    end subroutine
    ! monthly geostrophic transport only through Station1-6(4-9)
    subroutine geotransport(medianfiltertype,geotrans)
        implicit none
        integer,parameter::years = 15, months = 12, lines = 2
        real,dimension(years,months,lines),intent(out)::geotrans
        integer,intent(in)::medianfiltertype
        integer::y,m,l
        character::yyyy*9,filename*999,mm*9,line*9

        
        102 format(f9.4)
        if (medianfiltertype == 25) then
            do l = 1, lines
                if(l==1) then;line = 'N-Line';else; line='S-Line';end if
                do m = 1, months
                    write(mm,'(i2.2)')m
                    do y = 1, years
                        write(yyyy,'(i4.4)')y+2008
                        filename = '../Data/Geostrophy/'//trim(line)//'/'//trim(mm)//'/Transport_25Median'//trim(yyyy)//'.csv'
                        open(21,file = filename,status = 'old',action = 'read')
                            read(21,102)geotrans(y,m,l)
                        close(21)
                    end do
                end do
            end do
        else if(medianfiltertype == 51) then
            do l = 1, lines
                if(l==1) then;line = 'N-Line';else; line='S-Line';end if
                do m = 1, months
                    write(mm,'(i2.2)')m
                    do y = 1, years
                        write(yyyy,'(i4.4)')y+2008
                        filename = '../Data/Geostrophy/'//trim(line)//'/'//trim(mm)//'/Transport_51Median'//trim(yyyy)//'.csv'
                        open(31,file = filename,status = 'old',action = 'read')
                            read(31,102)geotrans(y,m,l)
                        close(31)
                    end do
                end do
            end do
        else;print*,'select either 25 or 51 for type of median filter'
        end if

    end subroutine
    ! gives an array of monthly mean sea surface height and pressure in Fukaura 2009-2023
    subroutine fukauraSSH(SSH_array)
        implicit none
        integer,parameter::years = 15, months = 12
        real,dimension(years,months),intent(out)::SSH_array
        integer::y,m,ios,yyyy,mm,nodata
        real::SSH,SSP
        character::filename*999

        ! 102 format((i4),(i2.2),(f9.4))
        ! 103 format((i4),(i2.2),(f9.4),(i2),(f9.4))
        filename = '../Data/SSH/Fukaura_Tides.csv'
        open(92,file = filename, status = 'old',action = 'read')
        do y = 1,years
            do m = 1,12
                read(92,*,iostat = ios)yyyy,mm,SSH,nodata,SSP
                ! print*,yyyy,mm,SSH,nodata,SSP
                SSH_array(y,m) = SSH
            end do
        end do
        close(92)
        ! calibratedの方から逆補正，SSPの影響を足す
        ! SSH_array(2,11) = 1677.7 - (10163.-10130.)
    end subroutine
    subroutine calibrated_fukauraSSH(calibrated_SSH_array)
        implicit none
        integer,parameter::years = 15, months = 12
        real,dimension(years,months),intent(out)::calibrated_SSH_array
        real,dimension(years,months)::SSH_array=0.,SSP_array=0.
        integer::y,m,ios,yyyy,mm,nodata
        real::SSH,SSP
        character::filename*999

        ! 102 format((i4),(i2.2),(f9.4))
        ! 103 format((i4),(i2.2),(f9.4),(i2),(f9.4))
        filename = '../Data/SSH/Fukaura_Tides.csv'
        open(92,file = filename, status = 'old',action = 'read')
        do y = 1,years
            do m = 1,12
                read(92,*,iostat = ios)yyyy,mm,SSH,nodata,SSP
                ! print*,yyyy,mm,SSH,nodata,SSP
                SSH_array(y,m) = SSH;SSP_array(y,m) = SSP
            end do
        end do
        close(92)
        do y = 1,years
            do m = 1,months
                if(SSH_array(y,m)/=0.) then
                    calibrated_SSH_array(y,m) = SSH_array(y,m) + (SSP_array(y,m)-10130)
                else;calibrated_SSH_array(y,m) = 0.
                end if
            end do
        end do
        ! calibrated_SSH_array(2,11) = (sum(calibrated_SSH_array(1:15,11))/real(years-1))

    end subroutine
    ! SSH and SSP at Tappi
    subroutine tappiSSH(SSH_array)
        implicit none
        integer,parameter::years = 15, months = 12
        real,dimension(years,months),intent(out)::SSH_array
        integer::y,m,ios,yyyy,mm,nodata
        real::SSH,SSP
        character::filename*999

        ! 102 format((i4),(i2.2),(f9.4))
        ! 103 format((i4),(i2.2),(f9.4),(i2),(f9.4))
        filename = '../Data/SSH/Tappi_Tides.csv'
        open(92,file = filename, status = 'old',action = 'read')
        do y = 1,years
            do m = 1,12
                read(92,*,iostat = ios)yyyy,mm,SSH,nodata,SSP
                ! print*,yyyy,mm,SSH,nodata,SSP
                SSH_array(y,m) = SSH
            end do
        end do
        close(92)
        ! calibratedの方から逆補正，SSPの影響を足す
        ! SSH_array(11,2) = 983.5 - (10177.-10130.)
    end subroutine
    subroutine calibrated_tappiSSH(calibrated_SSH_array)
        implicit none
        integer,parameter::years = 15, months = 12
        real,dimension(years,months),intent(out)::calibrated_SSH_array
        real,dimension(years,months)::SSH_array=0.,SSP_array=0.
        integer::y,m,ios,yyyy,mm,nodata
        real::SSH,SSP
        character::filename*999

        ! 102 format((i4),(i2.2),(f9.4))
        ! 103 format((i4),(i2.2),(f9.4),(i2),(f9.4))
        filename = '../Data/SSH/Tappi_Tides.csv'
        open(92,file = filename, status = 'old',action = 'read')
        do y = 1,years
            do m = 1,12
                read(92,*,iostat = ios)yyyy,mm,SSH,nodata,SSP
                ! print*,yyyy,mm,SSH,nodata,SSP
                SSH_array(y,m) = SSH;SSP_array(y,m) = SSP
            end do
        end do
        close(92)
        do y = 1,years
            do m = 1,months
                if(SSH_array(y,m)/=0.) then
                    calibrated_SSH_array(y,m) = SSH_array(y,m) + (SSP_array(y,m)-10130)
                else;calibrated_SSH_array(y,m) = 0.
                end if
            end do
        end do
        ! calibrated_SSH_array(11,2) = sum(calibrated_SSH_array(1:15,2))/real(years-2)
    end subroutine
    subroutine matsumaeSSH(SSH_array)
        implicit none
        integer,parameter::years = 15, months = 12
        real,dimension(years,months),intent(out)::SSH_array
        integer::y,m,ios,yyyy,mm,nodata
        real::SSH,SSP
        character::filename*999

        ! 102 format((i4),(i2.2),(f9.4))
        ! 103 format((i4),(i2.2),(f9.4),(i2),(f9.4))
        filename = '../Data/SSH/Matsumae_Tides.csv'
        open(92,file = filename, status = 'old',action = 'read')
        do y = 1,years
            do m = 1,12
                read(92,*,iostat = ios)yyyy,mm,SSH,nodata,SSP
                ! print*,yyyy,mm,SSH,nodata,SSP
                SSH_array(y,m) = SSH
            end do
        end do
        close(92)
    end subroutine
    subroutine calibrated_matsumaeSSH(calibrated_SSH_array)
        implicit none
        integer,parameter::years = 15, months = 12
        real,dimension(years,months),intent(out)::calibrated_SSH_array
        real,dimension(years,months)::SSH_array=0.,SSP_array=0.
        integer::y,m,ios,yyyy,mm,nodata
        real::SSH,SSP
        character::filename*999

        ! 102 format((i4),(i2.2),(f9.4))
        ! 103 format((i4),(i2.2),(f9.4),(i2),(f9.4))
        filename = '../Data/SSH/Matsumae_Tides.csv'
        open(92,file = filename, status = 'old',action = 'read')
        do y = 1,years
            do m = 1,12
                read(92,*,iostat = ios)yyyy,mm,SSH,nodata,SSP
                ! print*,ios,yyyy,mm,SSH,nodata,SSP
                SSH_array(y,m) = SSH;SSP_array(y,m) = SSP
            end do
        end do
        close(92)
        do y = 1,years
            do m = 1,months
                if(SSH_array(y,m)/=0.) then
                    calibrated_SSH_array(y,m) = SSH_array(y,m) + (SSP_array(y,m)-10130)
                else;calibrated_SSH_array(y,m) = 0.
                end if
            end do
        end do

    end subroutine
                                                ! SUBROUTINES FOR DATA OBTAINMENT !

                                                ! SUBROUTINES FOR DATA MANIPULATION !

    ! creating sigma array from potemp_5 and sal_5
    subroutine create_sigma_array(temp_array,sal_array,sigma_array)
        implicit none
        integer,parameter::years = 15, months = 12, lines = 2, stations = 9, depth = 400
        real,dimension(years,months,lines,stations,depth),intent(in)::temp_array,sal_array
        real,dimension(years,months,lines,stations,depth),intent(out)::sigma_array
        real::sigma
        integer::y,m,l,st,d

        do y = 1,years
            do m = 1, months
                do l = 1, lines
                    do st = 1, stations
                        do d = 1, depth
                            call sigma_T_S(sigma,temp_array(y,m,l,st,d),sal_array(y,m,l,st,d))
                            sigma_array(y,m,l,st,d) = sigma
                        end do
                    end do
                end do
            end do
        end do
    end subroutine
    ! creating an array of dynamic height from sigma array. 
    subroutine create_DH_array(sigma_array,DH_array)
        implicit none
        integer,parameter:: years = 15, months = 12, lines = 2, stations = 9, depth = 400
        real,dimension(years,months,lines,stations,depth),intent(in)::sigma_array
        real,dimension(years,months,lines,stations,depth),intent(out)::DH_array
        integer::y,m,l,st,d
        double precision::hiyou
        double precision::zero;double precision::one;double precision::four;double precision::ten;double precision::thousand
        double precision::firstsum=0.0d0;double precision::DH=0.0d0;double precision::g
        zero = 0.0d0;one = 1.0d0;four = 4.0d0;ten = 10.0d0;thousand = 1000.0d0; g = 9.8d0

        do y = 1, years
            do m = 1, months
                do l = 1, lines
                    do st = 1,stations
                        do d = 1, depth
                            if(sigma_array(y,m,l,st,d)/=0.)then
                                hiyou = one/((thousand+sigma_array(y,m,l,st,d))) !m^3/kg
                            else;hiyou = zero
                            end if
                            DH = firstsum + hiyou*ten**(four)/g
                            firstsum = DH
                            DH_array(y,m,l,st,d) = real(DH)
                        end do
                        firstsum = zero
                    end do
                end do
            end do
        end do

    end subroutine
    ! for calculating potential density as sigma
    subroutine sigma_T_S(sigma,potemp,sal)

        real,intent(in)::potemp,sal
        real,intent(out)::sigma
        !密度計算
                !!!Keisan Parameter
            double precision::a0,a1,a2,a3,a4,a5
            double precision::b0,b1,b2,b3,b4
            double precision::c0,c1,c2,c3
            double precision::d0,d1
            double precision::e0,e1,e2,e3,e4
            double precision::f0,f1,f2,f3
            double precision::g0,g1,g2
            double precision::h0,h1,h2,h3
            double precision::i0,i1,i2
            double precision::j0
            double precision::k0,k1,k2
            double precision::m0,m1,m2
            double precision::S,t,pp,rho,rhow,KK,Kt,AAAA,BB,Kw,Aw,Bw,rhoafter
            ! double precision::q,Gamma,Theta,xk
        
        
        
        !密度パラメータ
            a0=3.5803E-5
            a1=8.5258E-6
            a2=-6.8360E-8
            a3=6.6228E-10
            b0=1.8932E-6
            b1=-4.2393E-8
            c0=1.8741E-8
            c1=-6.7795E-10
            c2=8.733E-12
            c3=-5.4481E-14
            d0=-1.1351E-10
            d1=2.7759E-12
            e0=-4.6206E-13
            e1=1.8676E-14
            e2=-2.1687E-16
        
            
            a0=999.842594
            a1=6.793952E-2
            a2=-9.095290E-3
            a3=1.001685E-4
            a4=-1.120083E-6
            a5=6.536332E-9
            b0=8.24493E-1
            b1=-4.0899E-3
            b2=7.6438E-5
            b3=-8.2467E-7
            b4=5.3875E-9
            c0=-5.72466E-3
            c1=1.0227E-4
            c2=-1.6546E-6
            d0=4.8314E-4
            e0=19652.21
            e1=148.4206
            e2=-2.327105
            e3=1.360477E-2
            e4=-5.155288E-5
            f0=54.6746
            f1=-0.603459
            f2=1.09987E-2
            f3=-6.1670E-5
            g0=7.944E-2
            g1=1.6483E-2
            g2=-5.3009E-4
            h0=3.239908
            h1=1.43713E-3
            h2=1.16092E-4
            h3=-5.77905E-7
            i0=2.2838E-3
            i1=-1.0981E-5
            i2=-1.6078E-6
            j0=1.91075E-4
            k0=8.50935E-5
            k1=-6.12293E-6
            k2=5.2787E-8
            m0=-9.9348E-7
            m1=2.0816E-8
            m2=9.1697E-10
        !パラメータ終わり
        
        !密度計算
                t=potemp;pp=0.;S=sal
                rhow=a0+a1*t+a2*t**2.+a3*t**3.+a4*t**4.+a5*t**5.
                rho=rhow+(b0+b1*t+b2*t**2.+b3*t**3.+b4*t**4.)*S+(c0+c1*t+c2*t**2.)*S**(3./2.)+d0*S**2 
                Kw=e0+e1*t+e2*t**2.+e3*t**3.+e4*t**4.
                Aw=h0+h1*t+h2*t**2.+h3*t**3.
                Bw=k0+k1*t+k2*t**2.
                AAAA=Aw+(i0+i1*t+i2*t**2.)*S+j0*S**(3./2.)
                BB=Bw+(m0+m1*t+m2*t**2.)*S
                Kt=Kw+(f0+f1*t+f2*t**2.+f3*t**3.)*S+(g0+g1*t+g2*t**2.)*S**(3./2.)
                KK=Kt+AAAA*pp+BB*pp**2.
                rhoafter=rho/(1.-pp/KK)
                if (S==0.) then
                sigma=0.
                else
                sigma=real(rhoafter)-1000.
                end if 
        !密度計算終わり
        
        
    end subroutine
    ! for calculating potential temperature
    subroutine potemp_T_S_depth(potemp,tem,sal,depth)
        real,intent(in)::tem,sal,depth
        real,intent(out)::potemp
        !密度計算
        !!!Keisan Parameter
        double precision::a0,a1,a2,a3
        double precision::b0,b1
        double precision::c0,c1,c2,c3
        double precision::d0,d1
        double precision::e0,e1,e2
        ! double precision::f0,f1,f2,f3
        ! double precision::g0,g1,g2
        ! double precision::h0,h1,h2,h3
        ! double precision::i0,i1,i2
        ! double precision::j0
        ! double precision::k0,k1,k2
        ! double precision::m0,m1,m2
        double precision::d,S,t,p
        double precision::q,Gamma,Theta,xk
            a0=3.5803E-5
            a1=8.5258E-6
            a2=-6.8360E-8
            a3=6.6228E-10
            b0=1.8932E-6
            b1=-4.2393E-8
            c0=1.8741E-8
            c1=-6.7795E-10
            c2=8.733E-12
            c3=-5.4481E-14
            d0=-1.1351E-10
            d1=2.7759E-12
            e0=-4.6206E-13
            e1=1.8676E-14
            e2=-2.1687E-16

            !ポテンシャル水温計算
            S=sal
            t=tem
            d=0.-depth
            Gamma=a0+a1*t+a2*(t**2.)+a3*(t**3.)+(S-35.)*(b0+b1*t)+depth*(c0+c1*t+c2*(t**2.)+c3*(t**3.))+depth*(S-35.)*(d0+d1*t)+(depth**2.)*(e0+e1*t+e2*(t**2.))
            xk=d*Gamma
            t=t+0.5*xk
            q=xk
            p=depth+0.5*d
            Gamma=a0+a1*t+a2*(t**2.)+a3*(t**3.)+(S-35.)*(b0+b1*t)+p*(c0+c1*t+c2*(t**2.)+c3*(t**3.))+p*(S-35.)*(d0+d1*t)+(p**2.)*(e0+e1*t+e2*(t**2.))
            xk=d*Gamma
            t=t+0.29289322*(xk-q)
            q=0.58578644*xk+0.121320344*q
            Gamma=a0+a1*t+a2*(t**2.)+a3*(t**3.)+(S-35.)*(b0+b1*t)+p*(c0+c1*t+c2*(t**2.)+c3*(t**3.))+p*(S-35.)*(d0+d1*t)+(p**2.)*(e0+e1*t+e2*(t**2.))
            xk=d*Gamma
            t=t+1.707106781*(xk-q)
            q=3.414213562*xk-4.121320344*q
            p=p+0.5*d
            Gamma=a0+a1*t+a2*(t**2.)+a3*(t**3.)+(S-35.)*(b0+b1*t)+p*(c0+c1*t+c2*(t**2.)+c3*(t**3.))+p*(S-35.)*(d0+d1*t)+(p**2.)*(e0+e1*t+e2*(t**2.))
            xk=d*Gamma
            Theta=(t+(xk-2.0*q)/6.)
            if(S==0.) then
            potemp=0.
            else
            potemp=real(Theta)
            end if  
        
        
    end subroutine
    ! for calbrating SSH using SSP units must be in mm 
    ! subroutine calibrate_SSH(SSH,SSAP)
    !     implicit none
    !     real,intent(in)::SSAP
    !     real,intent(inout)::SSH

    !     if(SSH/=0..and.SSAP/=0.) then
    !         SSH = SSH + (SSAP-10130.)
    !     else;end if 
    ! end subroutine 

                                                ! SUBROUTINES FOR DATA MANIPULATION !

                                                ! SUBROUTINES FOR GRAPHS AND SHIT !

    subroutine rotate(angle)
        implicit none
        real,intent(in)::angle
        write(ounit,*) "% begin rotate"
        write(ounit,'(f10.4,2x,a8)' ) angle , ' ro ' 
        write(ounit,*) "% end rotate"

    end subroutine
    ! creating ps frame
    subroutine psframe(ini_st,fin_st,depth,width,height,memori_size)
        integer,intent(in)::ini_st,fin_st,depth
        real,intent(in)::width,height,memori_size
        real::dx,dy
        integer::x_memori,y_memori,st_quan,depth_quan
        integer,parameter::increment = 50

        dx = width/real(fin_st-ini_st+1); dy = height/real(depth)
        st_quan = fin_st - ini_st + 1; depth_quan = depth/increment
        call newpen2(3)
        call plot(0.,0.,3);call plot(0.,-height,2);call plot(width,-height,2);call plot(width,0.,2);call plot(0.,0.,2)
        call newpen2(3)
        do x_memori = 1,st_quan
            call plot(dx*real(x_memori)-dx/2.,-height,3);call plot(dx*real(x_memori)-dx/2.,-height-memori_size,2)
            call numberc(dx*real(x_memori)-dx/2.,-height-memori_size*4.,memori_size*3.,real(st_quan-x_memori+1),0.,-1)
        end do
        call newpen2(3)
        do y_memori = 0,depth_quan
            call plot(0.,-dy*real(increment)*real(y_memori),3);call plot(-memori_size,-dy*real(increment)*real(y_memori),2)
            call numberc(-memori_size*4.,-dy*real(increment)*real(y_memori),memori_size*3.,real(y_memori*increment),0.,-1)
        end do
    end subroutine
    ! only creates a frame unfortunately
    subroutine TS_diagram(temp_min,temp_max,sal_min,sal_max,sal_memoriterations,sal_symbolfreq,symbol_size,width,height)
        implicit none
        integer,parameter::ds_total = 101, dt_total =101
        integer,parameter::xs = 1,xe = ds_total,ys = 1,ye = dt_total
        real,parameter:: cont_inc = 0.2
        real,intent(in)::sal_min,sal_max,width,height,symbol_size
        ! integer,intent(in)::arraysize
        integer,intent(in)::temp_min,temp_max,sal_memoriterations,sal_symbolfreq
        ! real,dimension(arraysize),intent(in)::temp_array,sal_array
        real::sal,temp,sal_range,whatyouneed,dx,dy,real_sigma,y,sigma_print_y,min_sigma,max_sigma,first_sigma,sal_memoridiff,sal_numdiff
        integer::n,s,t,temp_range,cont_quantity
        integer,dimension(ds_total,dt_total)::mask=0
        real,dimension(ds_total,dt_total)::sigma_array=0.
        dx = width/real(ds_total-1); dy = height/real(dt_total-1)
        temp_range = temp_max - temp_min; sal_range = sal_max - sal_min
        do s=1,ds_total
            do t=1,dt_total
                mask(s,t)=1
            end do
        end do  !just for the use of Kuroda's subroutine

        
        call newpen2(3)
        call plot(0.,0.,3);call plot(width,0.,2);call plot(width,height,2);call plot(0.,height,2);call plot(0.,0.,2)
        call symbolc(width/2.,-2.2*symbol_size,symbol_size,'Salinity',0.,len('salinity'))
        call symbolc(-2.5*symbol_size,height/2.,symbol_size,'Temperature',90.,len('temperature'))
        call newpen2(3)

        do n = 0,sal_memoriterations
            call newpen2(4)
            sal_memoridiff = width/real(sal_memoriterations);sal_numdiff = (sal_max-sal_min)/real(sal_memoriterations)
            if (mod(n,sal_symbolfreq)==0) then
                call plot(real(n)*sal_memoridiff,0.,3);call plot(real(n)*sal_memoridiff,-0.2,2)
                call numberc(real(n)*sal_memoridiff,-1.2*symbol_size,symbol_size,sal_min+real(n)*sal_numdiff,0.,2)
            else
                call plot(real(n)*sal_memoridiff,0.,3);call plot(real(n)*sal_memoridiff,-0.1,2)
            end if
        end do
        do n = 0,temp_range
            call newpen2(4)
            if(mod(n,5)==0) then
                call plot(0.,real(n)*real(height)/real(temp_range),3);call plot(-0.2,real(n)*real(height)/real(temp_range),2)
                call numberr(-0.3,real(n)*real(height)/real(temp_range),symbol_size*0.9,real(temp_min+temp_range)*real(n)/real(temp_range),0.,1)
            else
                call plot(0.,real(n)*real(height)/real(temp_range),3);call plot(-0.1,real(n)*real(height)/real(temp_range),2)  
            end if
        end do

        do s = 1,ds_total
            do t = 1,dt_total
            temp = temp_min+temp_range*real(t)/real(dt_total); sal = sal_min+sal_range*real(s)/real(ds_total)
                call sigma_T_S(whatyouneed,temp,sal)
                sigma_array(s,t) = whatyouneed
            end do
        end do
        min_sigma = minval(sigma_array) 
        max_sigma = maxval(sigma_array)  
        ! print*,min_sigma,max_sigma
        y = 0.
        call newpen2(3)
        cont_quantity = int(max_sigma-min_sigma)*5+10
        first_sigma = int(min_sigma)
        call plot(-dx/2.,-dy/2.,-3)
        call symbol(width+symbol_size*0.2,height+symbol_size*0.4,symbol_size*0.8,'sigma:',0.,len('sigma:'))
        do n = 0,cont_quantity
            real_sigma = first_sigma+real(n)*cont_inc
            if(int(real_sigma)==real_sigma .and. real_sigma>=min_sigma .and. real_sigma<=max_sigma) then
                call newpen2(5);call rgbk(0.,0.,0.);call pscont3(dx,dy,sigma_array,mask,xs,xe,ys,ye,ds_total,dt_total,1,real_sigma,0.)
                sigma_print_y = height-0.8*symbol_size-y
                call numberc(width+symbol_size,sigma_print_y,0.8*symbol_size,real_sigma,0.,1)
                y = y +0.8*symbol_size
            else
                call newpen2(2);call rgbk(0.,0.,0.);call pscont3(dx,dy,sigma_array,mask,xs,xe,ys,ye,ds_total,dt_total,1,real_sigma,0.)
            end if
        end do
        y = 0.
        call plot(dx/2.,dy/2.,-3)

        ! do n = 1, arraysize
        !     if(temp_array(n)/=0. .and. sal_array(n)/=0.)then
        !         plot_x = (sal_array(n)-sal_min)*width/real(sal_range)
        !         plot_y = (temp_array(n)-real(temp_min))*height/real(temp_range)
        !         call gmark(plot_x,plot_y,0.05,1)
        !     else;end if
        ! end do

    end subroutine
    ! the output is the ratio of the input value to the max value relative to the length of the box or the axis
    subroutine gmark_ratio(input_value,min,max,length,output)
        implicit none
        real,intent(in)::input_value,min,max,length
        real,intent(out)::output

        output = (input_value-min)*length/(max-min)

    end subroutine
    ! month names array
    subroutine month_str_array(month_names)
        implicit none
        character(len=4),dimension(12),intent(out)::month_names

        month_names = (/'Jan.','Feb.','Mar.','Apr.','May ','Jun.','Jul.','Aug.','Sep.','Oct.','Nov.','Dec.'/)
    end subroutine
    ! betanuri version of ps color, only works for 1D arrays 
    subroutine betcolork(starting_x,dx,dy,oneD_array,array_size,mask,ini_value,fin_value,r,g,b)
        implicit none
        integer,intent(in)::array_size
        real,dimension(array_size),intent(in)::oneD_array
        integer,dimension(array_size),intent(in)::mask
        real,intent(in)::starting_x,ini_value,fin_value,r,g,b,dx,dy
        integer::n

        do n = 1, array_size
            ! print*,oneD_array(n)
            if(mask(n)/=0) then
                if(ini_value<=oneD_array(n) .and. oneD_array(n)<=fin_value) then
                    call betsqk(starting_x,real(n-1)*dy,starting_x+dx,real(n)*dy,r,g,b)
                else;end if
            else;end if
        end do
        ! print*,"ongongo"

    end subroutine
    subroutine betcolork2(dx,dy,twoD_array,mask,ix,ex,iy,ey,x_size,y_size,ini_value,fin_value,r,g,b)
        implicit none
        integer,intent(in)::ix,ex,iy,ey,x_size,y_size
        real,dimension(x_size,y_size),intent(in)::twoD_array
        integer,dimension(x_size,y_size),intent(in)::mask
        real,intent(in)::ini_value,fin_value,r,g,b,dx,dy
        integer::n,m

        do n = ix, ex
            do m = iy,ey
                ! if(mask(n,m)/=0) then
                    if(mask(n,m)/=0.and.ini_value<=twoD_array(n,m) .and. twoD_array(n,m)<=fin_value) then
                        call betsqk(real(n-1)*dx,real(m-1)*dy,real(n)*dx,real(m)*dy,r,g,b)
                    else;end if
                ! else;end if
            end do
        end do

    end subroutine
    ! betcolork but for integers such as data quantity
    subroutine betcolorI(dx,dy,twoD_array,mask,ix,ex,iy,ey,x_size,y_size,someint,r,g,b)
        implicit none
        integer,intent(in)::ix,ex,iy,ey,x_size,y_size,someint
        integer,dimension(x_size,y_size),intent(in)::twoD_array,mask
        real,intent(in)::r,g,b,dx,dy
        integer::n,m

        do n = ix, ex
            do m = iy,ey
                ! if(mask(n,m)/=0) then
                    if(mask(n,m)/=0.and.twoD_array(n,m)==someint) then
                        call betsqk(real(n-1)*dx,real(m-1)*dy,real(n)*dx,real(m)*dy,r,g,b)
                    else;end if
                ! else;end if
            end do
        end do

    end subroutine
    ! creates colorscale for data quantity min and max are integers. array size for colors is (iterations)
    subroutine colorscale_data(iterations,r,g,b,min,max,symbol_freq,symbol_size,length,width,angle)
        implicit none
        integer,intent(in)::iterations,min,max,symbol_freq,angle
        real,intent(in)::symbol_size,length,width
        real,dimension(iterations),intent(in)::r,g,b 
        integer::n,quotient
        real::memori_diff
        
        if(mod(max-min+1,iterations)/=0) then
            print*,'Something Wrong with the Number of Colors in subroutine (colorscale_data)'
        else;quotient = (max-min+1)/iterations
        end if

            memori_diff = length/real(iterations)
            if(angle == 0) then
                call newpen2(2)
                do n = 1,iterations
                    call betsqk(real(n-1)*memori_diff,0.,real(n)*memori_diff,width,r(n),g(n),b(n))
                end do
                call plot(0.,0.,3);call plot(length,0.,2);call plot(0.,width,3);call plot(length,width,2)
                do n = 0, iterations
                    call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,width,2)
                end do
                do n = 0,iterations-1
                    call plot(real(n)*memori_diff+memori_diff/2.,0.,3);call plot(real(n)*memori_diff+memori_diff/2.,-0.1,2)
                    if(mod(n,symbol_freq)==0) then
                        call numberc(real(n)*memori_diff+memori_diff/2.,-1.4*symbol_size,symbol_size,real(min)+real(n)*real(quotient),0.,-1)
                    else;end if
                end do
            else !(angle == 90)
                call newpen2(2)
                do n = 1, iterations
                    call betsqk(0.,real(n-1)*memori_diff,-width,real(n)*memori_diff,r(n),g(n),b(n))
                end do
                call plot(0.,0.,3);call plot(0.,length,2);call plot(-width,0.,3);call plot(-width,length,2)
                do n = 0, iterations
                    call plot(0.,real(n)*memori_diff,3);call plot(-width,real(n)*memori_diff,2)
                end do
                do n = 0,iterations-1
                    call plot(0.,real(n)*memori_diff+memori_diff/2.,3);call plot(0.1,real(n)*memori_diff+memori_diff/2.,2)
                    if(mod(n,symbol_freq)==0) then
                        call number(1.4*symbol_size,real(n)*memori_diff+memori_diff/2.,symbol_size,real(min)+real(n)*real(quotient),0.,-1)
                    else;end if
                end do
            end if

    end subroutine
    ! t value for t distribution 95 percent confidence interval df<=30
    subroutine t95_value(t95) !this is and array of 31 values, dimension is(0:30)
        implicit none
        real,dimension(0:30),intent(out)::t95

        t95(1) = 12.706 ; t95(11) = 2.2010 ; t95(21) = 2.0796
        t95(2) = 4.3026 ; t95(12) = 2.1788 ; t95(22) = 2.0739
        t95(3) = 3.1824 ; t95(13) = 2.1604 ; t95(23) = 2.0687
        t95(4) = 2.7765 ; t95(14) = 2.1448 ; t95(24) = 2.0639
        t95(5) = 2.5706 ; t95(15) = 2.1315 ; t95(25) = 2.0595
        t95(6) = 2.4469 ; t95(16) = 2.1191 ; t95(26) = 2.0555
        t95(7) = 2.3646 ; t95(17) = 2.1098 ; t95(27) = 2.0518
        t95(8) = 2.3060 ; t95(18) = 2.1009 ; t95(28) = 2.0484
        t95(9) = 2.2621 ; t95(19) = 2.0930 ; t95(29) = 2.0452
        t95(10) = 2.2281 ;t95(20) = 2.0860 ; t95(30) = 2.0423

        t95(0) = 0. !just for the sake of programs
    end subroutine
end module oldsubs

module constants
    implicit none
    ! intrinsic::sin,cos,acos,asin,atan,atan2,exp,log,log10,sqrt,abs,mod,aimag,aint,anint,nint
    integer, parameter :: years = 15, months = 12, lines = 2, stations = 9, depth = 400
    real,dimension(years,months,lines,stations,depth):: temp_5=0.,potemp_5=0.,sal_5=0.,sigma_5=0.,potemp_c5=0.,sal_c5=0.,sigma_c5=0.
    real,dimension(years,months,lines,8,depth)::geovel_c5=0.
    character(len=4),dimension(12)::monthnames = (/'Jan.','Feb.','Mar.','Apr.','May ','Jun.','Jul.','Aug.','Sep.','Oct.','Nov.','Dec.'/)
    integer::y,m,l,st,d,i,j,k,n
    real,parameter::pi = 3.14159265358979323846,gravity = 9.81,delta_x = 2.*pi*6378.*1000.*cos(41.*pi/180.)/360.*1./3.
    ! delta_x = 28004.0293 is in meters
    
end module constants

module data_types
    implicit none
    type :: JODC_TS
        integer,dimension(:,:,:,:),allocatable::num_samples
        real,dimension(:,:,:,:),allocatable::mean,max,min,sd,sem
    end type JODC_TS
    type :: JODC_RHO
        real,dimension(:,:,:,:),allocatable::mean
    end type JODC_RHO
    type :: JODC_V
        integer,dimension(:,:,:,:),allocatable::num_samples
        real,dimension(:,:,:,:),allocatable::mean_dir,max_dir,min_dir,mean_vel,max_vel,stability
    end type JODC_V
end module data_types

module subroutines 
    use oldsubs
    implicit none
    contains

    ! DATA obtainment and manipulation
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Be careful of the Array Station Indices, since I am used to drawing from left to right, although the station labels are from right to left
      ! Vg arrays are from 1 to 8
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! New Version of calibrated data, the station labels match the indices in the array if prompted. I should have done this much much earlier........12172024
        subroutine calibrated_data2(potemp_c5,sal_c5,sigma_c5,geovel_c5,match_station_labels_and_array_indices,median_filter_db)
            use functions
            implicit none
            real,dimension(15,12,2,9,400),intent(out)::potemp_c5,sal_c5
            real,dimension(15,12,2,9,400),intent(out),optional::sigma_c5
            real,dimension(15,12,2,8,400),intent(out),optional::geovel_c5
            real,dimension(:,:),allocatable::Vg2d
            integer,intent(in),optional::median_filter_db
            logical,intent(in),optional::match_station_labels_and_array_indices 
            integer::medfilt,y,m,l
            real,parameter::pi = 3.14159265358979323846,delta_x = 2.*pi*6378.*1000.*cos(41.*pi/180.)/360.*1./3.
            ! Choosing the median filter interval

            if(present(median_filter_db))then 
                if(median_filter_db/=25 .and.median_filter_db/=51)then 
                    print*,'Median Filter Interval Must be 25 or 51'
                else;medfilt = median_filter_db
                end if
            else
                medfilt = 51
            end if
            print*,'--------------------------------------------------'
            print*,'[Subroutine calibrated_data], '
            print*, int2str(medfilt),'db median filtered'
            ! get data
            if(medfilt == 51)then 
                call calibrated_data51(potemp_c5,sal_c5)
                if(present(sigma_c5))call create_sigma_array(potemp_c5,sal_c5,sigma_c5)
            else
                call calibrated_data25(potemp_c5,sal_c5)
                if(present(sigma_c5))call create_sigma_array(potemp_c5,sal_c5,sigma_c5)
            end if
            if(present(geovel_c5))then 
                do y = 1, 15
                    do m = 1, 12
                        do l = 1,2
                            call calc_geovel(Vg2d,delta_x,temp_2D = potemp_c5(y,m,l,:,:),sal_2D = sal_c5(y,m,l,:,:),lat = 40.3)
                            geovel_c5(y,m,l,:,:) = Vg2d
                        end do
                    end do
                end do
            end if
            
            if(present(match_station_labels_and_array_indices))then 
                if(match_station_labels_and_array_indices)then 
                    potemp_c5 = potemp_c5(:,:,:,9:1:-1,:)
                    sal_c5 = sal_c5(:,:,:,9:1:-1,:)
                    if(present(sigma_c5))sigma_c5 = sigma_c5(:,:,:,9:1:-1,:)
                    if(present(geovel_c5))geovel_c5 = geovel_c5(:,:,:,8:1:-1,:)
                    print*,'**** Station Labels Match The Array Indices ****'
                else;print*,'**** Station Labels And Array Indices are Flipped ****'
                end if
            else;print*,'**** Station Labels And Array Indices are Flipped ****'
            end if

            print*,'--------------------------------------------------'
            return
        end subroutine 

        ! JODC Data
        ! Potential Temperature and Density are only calculated for the mean values
        subroutine JODC_data(potemp,sal,den,vel,ilat,flat,ilon,flon,info)
            use functions
            use data_types
            implicit none
            type(JODC_TS),intent(out),optional::potemp,sal
            type(JODC_RHO),intent(out),optional::den
            type(JODC_V),intent(out),optional::vel
            type(JODC_TS)::potemp_local,sal_local
            type(JODC_RHO)::den_local
            type(JODC_V)::vel_local 
            integer,intent(in),optional::ilat,flat,ilon,flon
            logical,intent(in),optional::info
            integer::ios,i,j,ilat_local,ilon_local,flat_local,flon_local,month,lat,lon,dep,num
            real::mean,max,min,sd
            character(len=200)::firstrow,filename
            logical::istat

            istat = .false.
            if(present(info))istat = info

            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                        ! Allocation of (local) Arrays
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            ilat_local = -90 ; flat_local = 90 ; ilon_local = -180 ; flon_local = 180 ! local array contains everything
            if(present(potemp))then 
                allocate(potemp_local%num_samples(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(potemp_local%mean(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(potemp_local%max(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(potemp_local%min(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(potemp_local%sd(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(potemp_local%sem(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                potemp_local%num_samples = 0;potemp_local%mean = 0.;potemp_local%max = 0.;potemp_local%min = 0.;potemp_local%sd = 0.;potemp_local%sem = 0.
            end if
            if(present(sal))then
                allocate(sal_local%num_samples(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(sal_local%mean(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(sal_local%max(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(sal_local%min(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(sal_local%sd(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(sal_local%sem(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                sal_local%num_samples = 0;sal_local%mean = 0.;sal_local%max = 0.;sal_local%min = 0.;sal_local%sd = 0.;sal_local%sem = 0.
            end if 
            if(present(den))then 
                allocate(den_local%mean(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                den_local%mean = 0.
            end if
            if(present(vel))then
                allocate(vel_local%num_samples(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(vel_local%mean_dir(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(vel_local%max_dir(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(vel_local%min_dir(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(vel_local%mean_vel(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(vel_local%max_vel(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(vel_local%stability(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                vel_local%num_samples = 0;vel_local%mean_dir = 0.;vel_local%max_dir = 0.;vel_local%min_dir = 0.;vel_local%mean_vel = 0.;vel_local%max_vel = 0.;vel_local%stability = 0.
            end if
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                        ! Reading files and putting the values inside local arrays
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            if(present(potemp).or.present(sal))then 
                if(istat)print*,'--------------------------------'
                if(istat)print*,'   Obtaining Salinity Data'
                if(istat)print*,'--------------------------------'
                do i = 0 ,12
                    filename = '../Data/JODC/Salinity/bss-'//int2str(i,form = '(i2.2)')//'.csv'
                    if(istat)then 
                        if(i == 0)print*,'     Opening Annual Data File'
                        if(i /= 0)print*,'     Opening File of '//month_names(i)
                    end if
                    open(20,file = filename,status = 'old',action = 'read')
                    read(20,'(A)')firstrow
                    do 
                        read(20,*,iostat = ios)month,lat,lon,dep,num,mean,max,min,sd
                        if(ios/=0)exit
                        dep = JODC_dep_to_index(dep)

                        sal_local%num_samples(month,lon,lat,dep) = num
                        sal_local%mean(month,lon,lat,dep) = mean
                        sal_local%max(month,lon,lat,dep) = max
                        sal_local%min(month,lon,lat,dep) = min
                        sal_local%sd(month,lon,lat,dep) = sd
                        sal_local%sem(month,lon,lat,dep) = sd/sqrt(real(num))

                    end do
                    close(20)
                    ! if(istat)print*,'Closing ...'
                    if(istat)print*,'--------------------------------'
                end do
            end if
            if(present(potemp).or.present(sal))then 
                if(istat)print*,'--------------------------------'
                if(istat)print*,'   Obtaining Temperature Data'
                if(istat)print*,'--------------------------------'
                do i = 0 ,12
                    filename = '../Data/JODC/Temperature/bts-'//int2str(i,form = '(i2.2)')//'.csv'
                    if(istat)then 
                        if(i == 0)print*,'     Opening Annual Data File'
                        if(i /= 0)print*,'     Opening File of '//month_names(i)
                    end if
                    open(20,file = filename,status = 'old',action = 'read')
                    read(20,'(A)')firstrow
                    do 
                        read(20,*,iostat = ios)month,lat,lon,dep,num,mean,max,min,sd
                        if(ios/=0)exit
                        dep = JODC_dep_to_index(dep)

                        potemp_local%num_samples(month,lon,lat,dep) = num
                        potemp_local%mean(month,lon,lat,dep) = potemp_TSd(mean,sal_local%mean(month,lon,lat,dep),real(JODC_index_to_dep(dep)))
                        potemp_local%max(month,lon,lat,dep) = max
                        potemp_local%min(month,lon,lat,dep) = min
                        potemp_local%sd(month,lon,lat,dep) = sd
                        potemp_local%sem(month,lon,lat,dep) = sd/sqrt(real(num))
                        
                    end do
                    close(20)
                    ! if(istat)print*,'Closing ...'
                    if(istat)print*,'--------------------------------'
                end do
            end if
            if(present(den))then 
                if(istat)print*,'--------------------------------'
                if(istat)print*,'Calculating Potential Density...'
                if(istat)print*,'--------------------------------'
                do i = 0, 12
                    do j = 0, 32 
                        call calc_density(temp_2D = potemp_local%mean(i,:,:,j),sal_2D = sal_local%mean(i,:,:,j),den_2D = den_local%mean(i,:,:,j))
                    end do
                end do
            end if

            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                        ! Putting the values of the local array into the outgoing array 
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            if(present(ilat))ilat_local = ilat
            if(present(flat))flat_local = flat
            if(present(ilon))ilon_local = ilon
            if(present(flon))flon_local = flon

            if(present(potemp))then 
                allocate(potemp%num_samples(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(potemp%mean(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(potemp%max(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(potemp%min(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(potemp%sd(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(potemp%sem(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))

                potemp%num_samples = potemp_local%num_samples(:,ilon_local:flon_local,ilat_local:flat_local,:)
                potemp%mean = potemp_local%mean(:,ilon_local:flon_local,ilat_local:flat_local,:)
                potemp%max = potemp_local%max(:,ilon_local:flon_local,ilat_local:flat_local,:)
                potemp%min = potemp_local%min(:,ilon_local:flon_local,ilat_local:flat_local,:)
                potemp%sd = potemp_local%sd(:,ilon_local:flon_local,ilat_local:flat_local,:)
                potemp%sem = potemp_local%sem(:,ilon_local:flon_local,ilat_local:flat_local,:)
                deallocate(potemp_local%num_samples,potemp_local%mean,potemp_local%max,potemp_local%min,potemp_local%sd,potemp_local%sem)
            end if

            if(present(sal))then 
                allocate(sal%num_samples(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(sal%mean(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(sal%max(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(sal%min(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(sal%sd(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(sal%sem(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))

                sal%num_samples = sal_local%num_samples(:,ilon_local:flon_local,ilat_local:flat_local,:)
                sal%mean = sal_local%mean(:,ilon_local:flon_local,ilat_local:flat_local,:)
                sal%max = sal_local%max(:,ilon_local:flon_local,ilat_local:flat_local,:)
                sal%min = sal_local%min(:,ilon_local:flon_local,ilat_local:flat_local,:)
                sal%sd = sal_local%sd(:,ilon_local:flon_local,ilat_local:flat_local,:)
                sal%sem = sal_local%sem(:,ilon_local:flon_local,ilat_local:flat_local,:)
                deallocate(sal_local%num_samples,sal_local%mean,sal_local%max,sal_local%min,sal_local%sd,sal_local%sem)
            end if

            if(present(den))then 
                allocate(den%mean(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))

                den%mean = den_local%mean(:,ilon_local:flon_local,ilat_local:flat_local,:)
                deallocate(den_local%mean)
            end if

        end subroutine

        ! Uses Binary File and is much faster than the previous subroutine
        ! Have Not Yet made the program for vel as it is not necessary for the time being
        ! be wary that dynamic height dh is in cm
        subroutine JODC_data2(potemp,sal,den,dh,vel,ilat,flat,ilon,flon,info)
            use functions
            use data_types
            implicit none
            type(JODC_TS),intent(out),optional::potemp,sal
            type(JODC_RHO),intent(out),optional::den
            type(JODC_RHO),intent(out),optional::dh
            type(JODC_V),intent(out),optional::vel
            type(JODC_TS)::potemp_local,sal_local
            type(JODC_RHO)::den_local
            type(JODC_RHO)::dh_local
            type(JODC_V)::vel_local 
            integer,intent(in),optional::ilat,flat,ilon,flon
            logical,intent(in),optional::info
            integer::ilat_local,ilon_local,flat_local,flon_local
            character(len=200)::filename
            logical::istat
            ! real::den_mean,delta_zdb
            ! real(kind = 8)::sum

            istat = .false.
            if(present(info))istat = info
            filename = '../Data/JODC/JODC_DATA.bin'
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                        ! Allocation of (local) Arrays
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                ilat_local = -90 ; flat_local = 90 ; ilon_local = -180 ; flon_local = 180 ! local array contains everything
                if(present(potemp))then 
                    allocate(potemp_local%num_samples(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                    allocate(potemp_local%mean(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                    allocate(potemp_local%max(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                    allocate(potemp_local%min(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                    allocate(potemp_local%sd(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                    allocate(potemp_local%sem(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                    potemp_local%num_samples = 0;potemp_local%mean = 0.;potemp_local%max = 0.;potemp_local%min = 0.;potemp_local%sd = 0.;potemp_local%sem = 0.
                end if
                if(present(sal))then
                    allocate(sal_local%num_samples(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                    allocate(sal_local%mean(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                    allocate(sal_local%max(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                    allocate(sal_local%min(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                    allocate(sal_local%sd(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                    allocate(sal_local%sem(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                    sal_local%num_samples = 0;sal_local%mean = 0.;sal_local%max = 0.;sal_local%min = 0.;sal_local%sd = 0.;sal_local%sem = 0.
                end if 
                if(present(den))then 
                    allocate(den_local%mean(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                    den_local%mean = 0.
                end if
                if(present(dh))then 
                    allocate(dh_local%mean(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                    dh_local%mean = 0.
                end if
                if(present(vel))then
                    allocate(vel_local%num_samples(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                    allocate(vel_local%mean_dir(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                    allocate(vel_local%max_dir(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                    allocate(vel_local%min_dir(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                    allocate(vel_local%mean_vel(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                    allocate(vel_local%max_vel(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                    allocate(vel_local%stability(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                    vel_local%num_samples = 0;vel_local%mean_dir = 0.;vel_local%max_dir = 0.;vel_local%min_dir = 0.;vel_local%mean_vel = 0.;vel_local%max_vel = 0.;vel_local%stability = 0.
                end if
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                        ! Reading files and putting the values inside local arrays
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                if(present(potemp).or.present(dh))then 
                    if(istat)print*,'--------------------------------'
                    if(istat)print*,'   Obtaining Temperature Data'
                    if(istat)print*,'--------------------------------'
                    open(20,file = filename, form = 'unformatted',status = 'old',action = 'read',access = 'direct',recl = 4*13*361*181*33)
                    read(20,rec = 1)potemp_local%num_samples
                    read(20,rec = 2)potemp_local%mean
                    read(20,rec = 3)potemp_local%max
                    read(20,rec = 4)potemp_local%min
                    read(20,rec = 5)potemp_local%sd
                    read(20,rec = 6)potemp_local%sem
                    close(20)
                end if

                if(present(sal).or.present(dh))then 
                    if(istat)print*,'--------------------------------'
                    if(istat)print*,'   Obtaining Salinity Data'
                    if(istat)print*,'--------------------------------'
                    open(20,file = filename, form = 'unformatted',status = 'old',action = 'read',access = 'direct',recl = 4*13*361*181*33)
                    read(20,rec = 7)sal_local%num_samples
                    read(20,rec = 8)sal_local%mean
                    read(20,rec = 9)sal_local%max
                    read(20,rec = 10)sal_local%min
                    read(20,rec = 11)sal_local%sd
                    read(20,rec = 12)sal_local%sem
                    close(20)
                end if

                if(present(den).or.present(dh))then 
                    if(istat)print*,'--------------------------------'
                    if(istat)print*,' Obtaining Potential Density... '
                    if(istat)print*,'--------------------------------'
                    open(20,file = filename, form = 'unformatted',status = 'old',action = 'read',access = 'direct',recl = 4*13*361*181*33)
                    read(20,rec = 13)den_local%mean
                    close(20)
                end if

                if(present(dh))then 
                    if(istat)print*,'--------------------------------'
                    if(istat)print*,' Obtaining Dynamic Height... '
                    if(istat)print*,'--------------------------------'
                    open(20,file = filename, form = 'unformatted',status = 'old',action = 'read',access = 'direct',recl = 4*13*361*181*33)
                    read(20,rec = 14)dh_local%mean
                    close(20)

                    !!!! Below is the initial code for calculating the dynamic height !!!!
                        ! dh_local%mean(:,:,:,0) = 0.
                        ! do i = 0,12
                        !     do j = -180,180
                        !         do k = -90,90
                        !             do l = 1,32
                        !                 if(sal_local%mean(i,j,k,l) < 20..or.potemp_local%mean(i,j,k,l) == 0.)then 
                        !                     den_local%mean(i,j,k,l) = 0.
                        !                 end if

                        !                 if(den_local%mean(i,j,k,l) /= 0.)then 
                        !                     if(den_local%mean(i,j,k,l-1) /= 0.)then ! the layer above is not 0
                        !                         den_mean = (den_local%mean(i,j,k,l)+den_local%mean(i,j,k,l-1))/2.
                        !                     else ! if the layer above is 0
                        !                         den_mean = den_local%mean(i,j,k,l)
                        !                     end if
                        !                     delta_zdb = real(JODC_index_to_dep(l)-JODC_index_to_dep(l-1))
                        !                     dh_local%mean(i,j,k,l) = dh_local%mean(i,j,k,l-1) + fcalc_dh(den_mean,delta_zdb,'cm')
                        !                 else ! if the layer is 0
                        !                     if(l==32)then 
                        !                         dh_local%mean(i,j,k,l) = 0.
                        !                     else
                        !                         if(den_local%mean(i,j,k,l+1) /= 0..and.den_local%mean(i,j,k,l-1) /= 0.)then ! the layers above and below are non zero
                        !                             den_mean = (den_local%mean(i,j,k,l+1)+den_local%mean(i,j,k,l-1))/2.
                        !                             delta_zdb = real(JODC_index_to_dep(l)-JODC_index_to_dep(l-1))
                        !                             dh_local%mean(i,j,k,l) = dh_local%mean(i,j,k,l-1) + fcalc_dh(den_mean,delta_zdb,'cm')
                        !                         else 
                        !                             dh_local%mean(i,j,k,l:32) = 0.;exit
                        !                         end if
                        !                     end if
                        !                 end if
                        !             end do
                        !         end do
                        !     end do
                        ! end do
                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                end if

            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                        ! Putting the values of the local array into the outgoing array 
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            if(present(ilat))ilat_local = ilat
            if(present(flat))flat_local = flat
            if(present(ilon))ilon_local = ilon
            if(present(flon))flon_local = flon

            if(present(potemp))then 
                allocate(potemp%num_samples(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(potemp%mean(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(potemp%max(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(potemp%min(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(potemp%sd(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(potemp%sem(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))

                potemp%num_samples = potemp_local%num_samples(:,ilon_local:flon_local,ilat_local:flat_local,:)
                potemp%mean = potemp_local%mean(:,ilon_local:flon_local,ilat_local:flat_local,:)
                potemp%max = potemp_local%max(:,ilon_local:flon_local,ilat_local:flat_local,:)
                potemp%min = potemp_local%min(:,ilon_local:flon_local,ilat_local:flat_local,:)
                potemp%sd = potemp_local%sd(:,ilon_local:flon_local,ilat_local:flat_local,:)
                potemp%sem = potemp_local%sem(:,ilon_local:flon_local,ilat_local:flat_local,:)
                deallocate(potemp_local%num_samples,potemp_local%mean,potemp_local%max,potemp_local%min,potemp_local%sd,potemp_local%sem)
            end if

            if(present(sal))then 
                allocate(sal%num_samples(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(sal%mean(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(sal%max(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(sal%min(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(sal%sd(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))
                allocate(sal%sem(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))

                sal%num_samples = sal_local%num_samples(:,ilon_local:flon_local,ilat_local:flat_local,:)
                sal%mean = sal_local%mean(:,ilon_local:flon_local,ilat_local:flat_local,:)
                sal%max = sal_local%max(:,ilon_local:flon_local,ilat_local:flat_local,:)
                sal%min = sal_local%min(:,ilon_local:flon_local,ilat_local:flat_local,:)
                sal%sd = sal_local%sd(:,ilon_local:flon_local,ilat_local:flat_local,:)
                sal%sem = sal_local%sem(:,ilon_local:flon_local,ilat_local:flat_local,:)
                deallocate(sal_local%num_samples,sal_local%mean,sal_local%max,sal_local%min,sal_local%sd,sal_local%sem)
            end if

            if(present(den))then 
                allocate(den%mean(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))

                den%mean = den_local%mean(:,ilon_local:flon_local,ilat_local:flat_local,:)
                deallocate(den_local%mean)
            end if

            if(present(dh))then 
                allocate(dh%mean(0:12,ilon_local:flon_local,ilat_local:flat_local,0:32))

                dh%mean = dh_local%mean(:,ilon_local:flon_local,ilat_local:flat_local,:)
                deallocate(dh_local%mean)
            end if

        end subroutine
                    
        ! SSH DATA put st label and get array of 15 years and 12 months.   -999 means no data or insufficient data output array has the size(15,12) regardless of data quantity
        subroutine SSH_data(SSH2D,ilabel,slabel,convert,calibrate)
            implicit none
            type :: labeled_array
                integer, dimension(:,:),allocatable:: num_labels
                character(len=30), dimension(:,:),allocatable:: str_labels
                real, dimension(:,:,:),allocatable:: values
            end type labeled_array

            type(labeled_array) :: localssh
            integer,parameter::num_rows=150,num_years=15,num_months=12
            integer,intent(in),optional::ilabel
            character(len=*),intent(in),optional::slabel
            logical,intent(in),optional::convert,calibrate
            real,dimension(num_years,num_months),intent(out)::SSH2D
            real,dimension(:,:),allocatable::SSAP2D
            integer::n,i,ios,convint=0
            character::yyyy*4,row1*999,convstr*30
            logical::convert_local = .false.,calibrate_local = .false.

            if(present(convert))convert_local = convert
            if(present(calibrate))calibrate_local = calibrate
            print*,'----------------------------------------'
            print*,'[Subroutine SSH_data]'
            allocate(localssh%num_labels(num_years,num_rows))
            allocate(localssh%str_labels(num_years,num_rows))
            allocate(localssh%values(num_years,num_rows,num_months)) ! allocating arrays just to use heap memory
            localssh%num_labels = 0;localssh%str_labels = ' ';localssh%values = 0.
            convstr = ''
            do n = 1, num_years
                write(yyyy,'(i4)') n+2008
                open(unit=20, file='../Data/SSH/data/SSH'//trim(yyyy)//'.csv', status='old', action='read',iostat=ios)
                if(ios/=0) then
                    print*,'Error opening file';stop
                end if
                do i = 1, num_rows
                    read(20,'(A)',iostat = ios)row1
                    if(ios == 0)then
                        call parse_csv_row(row1, localssh%num_labels(n,i), localssh%str_labels(n,i), localssh%values(n,i,:))
                    else if(i<100.and.ios/=0)then
                        print*,'Error reading file';stop
                    else;exit
                    end if
                end do
            end do
            
            if(.not.present(ilabel).and..not.present(slabel))print*,'Provide The Station Label or Index'
            ! print*,localssh%num_labels(1,1:20)
            ! print*,localssh%str_labels(1,1:10)

            if(present(ilabel).and..not.present(slabel))then
                do n = 1, num_years
                    do i = 1, num_rows
                        if(localssh%num_labels(n,i)==ilabel)then
                            SSH2D(n,:) = localssh%values(n,i,:)
                            if(convert_local)then
                                if(n==1)convstr = trim(localssh%str_labels(n,i))
                                if(n/=1.and.convstr/=localssh%str_labels(n,i))then
                                    print*,'inconsistant station labels at',convstr,'and',localssh%str_labels(n,i)
                                end if
                            end if
                            exit
                        end if
                        if(i==num_rows)print*,'Station Index',ilabel,'not found for Year',n+2008
                    end do
                end do
                if(convert_local)print*,ilabel,'-->',convstr
                do n = 1, num_years
                    do i = 1, num_months
                        if(SSH2D(n,i)<100..and.SSH2D(n,i)/=-999.)then
                            print'(12(f6.1,1x),A,i5,A,i2)',SSH2D(n,1:num_months),'SSH<100 at Year ',n+2008,' Month',i
                        end if
                    end do
                    if(all(SSH2D(n,:)==-999.))then
                        print*,'No Data for Year',n+2008,'at Index',ilabel
                    else if(any(SSH2D(n,:)==-999.))then
                        do i = 1, num_months
                            if(SSH2D(n,i)==-999.)then
                                print*,'No Data for',n+2008,'Month',i
                            end if
                        end do
                    end if
                end do
                if(calibrate_local)then
                    allocate(SSAP2D(num_years,num_months))
                    call SSAP_data(SSAP2D,ilabel=ilabel)
                    call SSH_calibration(SSH2D, SSAP2D)
                end if
            else if(.not.present(ilabel).and.present(slabel))then
                do n = 1, num_years
                    do i = 1, num_rows
                        if(trim(localssh%str_labels(n,i))==slabel)then
                            SSH2D(n,:) = localssh%values(n,i,:)
                            if(convert_local)then
                                if(n==1)convint = localssh%num_labels(n,i)
                                if(n/=1.and.convint/=localssh%num_labels(n,i))then
                                    print*,'inconsistant station labels at',convint,'and',localssh%num_labels(n,i)
                                end if
                            end if
                            exit
                        end if
                        if(i==num_rows)print*,'Station Label',slabel,'not found for Year',n+2008
                    end do
                end do
                if(convert_local)print*,slabel,'-->',convint
                    do n = 1, num_years
                        do i = 1, num_months
                            if(SSH2D(n,i)<100..and.SSH2D(n,i)/=-999.)then
                                print'(12(f6.1,1x),A,i5,A,i2)',SSH2D(n,1:num_months),'SSH<100 at Year ',n+2008,' Month',i
                            end if
                        end do
                        if(all(SSH2D(n,:)==-999.))then
                            print*,'No Data for Year',n+2008,'at Station',slabel
                        else if(any(SSH2D(n,:)==-999.))then
                            do i = 1, num_months
                                if(SSH2D(n,i)==-999.)then
                                    print*,'No Data for',n+2008,'Month',i
                                end if
                            end do
                        end if
                    end do
                if(calibrate_local)then
                    allocate(SSAP2D(num_years,num_months))
                    call SSAP_data(SSAP2D,slabel=slabel)
                    call SSH_calibration(SSH2D, SSAP2D)
                end if
            else;print*,'Provide Either Station Label or Index but not both'
            end if
            print*,'----------------------------------------'
            return

            contains
            subroutine parse_csv_row(row, num_label, str_label, values)
                implicit none
                character(len=*), intent(in) :: row
                integer, intent(out) :: num_label
                character(len=30), intent(out) :: str_label
                real, dimension(:), intent(out) :: values
                integer :: pos, start_pos, end_pos,k
            
                ! Initialize positions
                start_pos = 1
                pos = 0
            
                ! Extract num_label
                end_pos = index(row(start_pos:), ',')
                read(row(start_pos:start_pos+end_pos-2), '(I4)', iostat=ios) num_label
                if (ios /= 0) then
                    print *, 'Error parsing num_label'
                    stop
                end if
                start_pos = start_pos + end_pos
            
                ! Extract str_label
                end_pos = index(row(start_pos:), ',')
                str_label = adjustl(row(start_pos:start_pos+end_pos-2))
                start_pos = start_pos + end_pos
            
                ! Extract values
                do k = 1, size(values) ! is 12
                    end_pos = index(row(start_pos:), ',')
                    if (end_pos == 0) end_pos = start_pos ! the last iteration
                    read(row(start_pos:start_pos+end_pos-2), '(F7.1)', iostat=ios) values(k)
                    if (ios /= 0) then
                        print *, 'Error parsing values'
                        stop
                    end if
                    start_pos = start_pos + end_pos
                end do
            end subroutine parse_csv_row

        end subroutine
        subroutine SSAP_data(SSAP2D,ilabel,slabel,convert)
            implicit none
            type :: labeled_array
                integer, dimension(:,:),allocatable:: num_labels
                character(len=30), dimension(:,:),allocatable:: str_labels
                real, dimension(:,:,:),allocatable:: values
            end type labeled_array
            type(labeled_array) :: localssap
            integer,parameter::num_rows=150,num_years=15,num_months=12
            integer,intent(in),optional::ilabel
            character(len=*),intent(in),optional::slabel
            logical,intent(in),optional::convert
            real,dimension(num_years,num_months),intent(out)::SSAP2D
            integer::n,i,ios,convint=0
            character::yyyy*4,row1*999,convstr*30
            logical::convert_local = .false.
            if(present(convert))convert_local = convert

            allocate(localssap%num_labels(num_years,num_rows))
            allocate(localssap%str_labels(num_years,num_rows))
            allocate(localssap%values(num_years,num_rows,num_months)) ! allocating arrays just to use heap memory

            localssap%num_labels = 0;localssap%str_labels = ' ';localssap%values = 0.
            convstr = ''

            do n = 1, num_years
                write(yyyy,'(i4)') n+2008
                open(unit=20, file='../Data/SSH/data/SSAP'//trim(yyyy)//'.csv', status='old', action='read',iostat=ios)
                if(ios/=0) then
                    print*,'Error opening file';stop
                end if
                do i = 1, num_rows
                    read(20,'(A)',iostat = ios)row1
                    if(ios == 0)then
                        call parse_csv_row(row1, localssap%num_labels(n,i), localssap%str_labels(n,i), localssap%values(n,i,:))
                    else if(i<100.and.ios/=0)then
                        print*,'Error reading file';stop
                    else;exit
                    end if
                end do
            end do

            if(.not.present(ilabel).and..not.present(slabel))print*,'Provide The Station Label or Index'

            if(present(ilabel).and..not.present(slabel))then
                do n = 1, num_years
                    do i = 1, num_rows
                        if(localssap%num_labels(n,i)==ilabel)then
                            SSAP2D(n,:) = localssap%values(n,i,:)
                            if(convert_local)then
                                if(n==1)convstr = trim(localssap%str_labels(n,i))
                                if(n/=1.and.convstr/=localssap%str_labels(n,i))then
                                    print*,'inconsistant station labels at',convstr,'and',localssap%str_labels(n,i)
                                end if
                            end if
                            exit
                        end if
                        if(i==num_rows)print*,'Station Index',ilabel,'not found for Year',n+2008
                    end do
                end do
                if(convert_local)print*,ilabel,'-->',convstr
            else if(.not.present(ilabel).and.present(slabel))then
                do n = 1, num_years
                    do i = 1, num_rows
                        if(trim(localssap%str_labels(n,i))==slabel)then
                            SSAP2D(n,:) = localssap%values(n,i,:)
                            if(convert_local)then
                                if(n==1)convint = localssap%num_labels(n,i)
                                if(n/=1.and.convint/=localssap%num_labels(n,i))then
                                    print*,'inconsistant station labels at',convint,'and',localssap%num_labels(n,i)
                                end if
                            end if
                            exit
                        end if
                        if(i==num_rows)print*,'Station Label',slabel,'not found for Year',n+2008
                    end do
                end do
                if(convert_local)print*,slabel,'-->',convint
            else;print*,'Provide Either Station Label or Index but not both'
            end if
            do n = 1, num_years
                do i = 1, num_months
                    if(SSAP2D(n,i)<10000..and.SSAP2D(n,i)/=-999.)then
                        print'(12(f7.1,1x),A,i5,A,i2)',SSAP2D(n,1:num_months),'SSAP<10000 at Year ',n+2008,' Month',i
                    end if
                end do
            end do

            contains
            subroutine parse_csv_row(row, num_label, str_label, values)
                implicit none
                character(len=*), intent(in) :: row
                integer, intent(out) :: num_label
                character(len=30), intent(out) :: str_label
                real, dimension(:), intent(out) :: values
                integer :: pos, start_pos, end_pos,k
            
                ! Initialize positions
                start_pos = 1
                pos = 0
            
                ! Extract num_label
                end_pos = index(row(start_pos:), ',')
                read(row(start_pos:start_pos+end_pos-2), '(I4)', iostat=ios) num_label
                if (ios /= 0) then
                    print *, 'Error parsing num_label'
                    stop
                end if
                start_pos = start_pos + end_pos
            
                ! Extract str_label
                end_pos = index(row(start_pos:), ',')
                str_label = adjustl(row(start_pos:start_pos+end_pos-2))
                start_pos = start_pos + end_pos
            
                ! Extract values
                do k = 1, size(values) ! is 12
                    end_pos = index(row(start_pos:), ',')
                    if (end_pos == 0) end_pos = start_pos ! the last iteration
                    read(row(start_pos:start_pos+end_pos-2), '(F7.1)', iostat=ios) values(k)
                    if (ios /= 0) then
                        print *, 'Error parsing values'
                        stop
                    end if
                    start_pos = start_pos + end_pos
                end do
            end subroutine parse_csv_row    

        end subroutine
        subroutine SSHlabelconversion(ilabel,slabel)
            implicit none
            integer,parameter::num_rows=150,num_years=15,num_months=12
            integer,intent(in),optional::ilabel
            character(len=*),intent(in),optional::slabel
            integer::n,i,ios,convint=0
            integer,dimension(num_years,num_rows)::numlabel
            character::yyyy*4,row1*999,convstr*30
            character(len=30),dimension(:,:),allocatable::strlabel*30

            allocate(strlabel(num_years,num_rows))
            convstr = ''

            do n = 1, num_years
                write(yyyy,'(i4)') n+2008
                open(unit=20, file='../Data/SSH/data/SSAP'//trim(yyyy)//'.csv', status='old', action='read',iostat=ios)
                if(ios/=0) then
                    print*,'Error opening file';stop
                end if
                do i = 1, num_rows
                    read(20,'(A)',iostat = ios)row1
                    if(ios == 0)then
                        call parse_csv_row(row1, numlabel(n,i),strlabel(n,i))
                    else if(i<100.and.ios/=0)then
                        print*,'Error reading file';stop
                    else;exit
                    end if
                end do
            end do

            if(.not.present(ilabel).and..not.present(slabel))print*,'Provide The Station Label or Index'

            if(present(ilabel).and..not.present(slabel))then
                do n = 1, num_years
                    do i = 1, num_rows
                        if(ilabel==numlabel(n,i))then
                            if(n==1)convstr = trim(strlabel(n,i))
                            if(n/=1.and.convstr/=strlabel(n,i))then
                                print*,'inconsistant station labels at',convstr,'and',strlabel(n,i),'at Year',n+2008
                            end if
                            exit
                        end if
                        if(i==num_rows)print*,'Station Index',ilabel,'not found for Year',n+2008
                    end do
                end do
                print*,ilabel,'-->',convstr
            else if(.not.present(ilabel).and.present(slabel))then
                do n = 1, num_years
                    do i = 1, num_rows
                        if(trim(slabel)==trim(strlabel(n,i)))then
                            if(n==1)convint = numlabel(n,i)
                            if(n/=1.and.convint/=numlabel(n,i))then
                                print*,'inconsistant station labels at',convint,'and',numlabel(n,i),'at Year',n+2008
                            end if
                            exit
                        end if
                        if(i==num_rows)print*,'Station Label',slabel,'not found for Year',n+2008
                    end do
                end do
                print*,slabel,'-->',convint
            else;print*,'Provide Either Station Label or Index but not both'
            end if

            contains
            subroutine parse_csv_row(row, num_label, str_label)
                implicit none
                character(len=*), intent(in) :: row
                integer, intent(out) :: num_label
                character(len=30), intent(out) :: str_label
                integer :: pos, start_pos, end_pos
            
                ! Initialize positions
                start_pos = 1
                pos = 0
            
                ! Extract num_label
                end_pos = index(row(start_pos:), ',')
                read(row(start_pos:start_pos+end_pos-2), '(I4)', iostat=ios) num_label
                if (ios /= 0) then
                    print *, 'Error parsing num_label'
                    stop
                end if
                start_pos = start_pos + end_pos
            
                ! Extract str_label
                end_pos = index(row(start_pos:), ',')
                str_label = adjustl(row(start_pos:start_pos+end_pos-2))
                start_pos = start_pos + end_pos
            
            end subroutine parse_csv_row  

        end subroutine
        subroutine SSH_calibration(SSH2D,SSAP2D)
            implicit none
            real,dimension(:,:),intent(inout)::SSH2D,SSAP2D
            integer::y,m

            do y = 1, size(SSH2D,1)
                do m = 1, size(SSH2D,2)
                    if(SSH2D(y,m)==-999..or.SSAP2D(y,m)==-999.)then
                        SSH2D(y,m) = -999.
                    else
                        SSH2D(y,m) = SSH2D(y,m)+(SSAP2D(y,m)-10130.)
                    end if
                end do
            end do
        end subroutine
        subroutine calc_density(temp_2D,sal_2D,den_2D,depth)
            implicit none
            real,intent(in)::temp_2D(:,:),sal_2D(:,:)
            real,intent(out)::den_2D(lbound(temp_2D,1):ubound(temp_2D,1),lbound(temp_2D,2):ubound(temp_2D,2))
            real,intent(in),optional::depth
            integer::i,j,iterations1,iterations2
            ! real::den_local
            
            if(size(temp_2D,1)/=size(sal_2D,1))then;print*,'Temperature and Salinity arrays must have the same size';stop;endif
            if(size(temp_2D,2)/=size(sal_2D,2))then;print*,'Temperature and Salinity arrays must have the same size';stop;endif

            iterations1 = size(temp_2D,1);iterations2 = size(temp_2D,2)

            do i = 1, iterations1
                do j = 1, iterations2
                    if(present(depth))then 
                        den_2D(i,j) = sigma_TS(temp_2D(i,j),sal_2D(i,j),depth)
                    else 
                        den_2D(i,j) = sigma_TS(temp_2D(i,j),sal_2D(i,j))
                    end if
                end do
            end do


        end subroutine
        function sigma_TS(temp,sal,depth) result(sigma)
            implicit none
            real,intent(in)::temp,sal
            real,intent(in),optional::depth
            real::sigma,potemp
    
            if(present(depth))then 
                call potemp_T_S_depth(potemp,temp,sal,depth)
                call sigma_T_S(sigma,potemp,sal)
            else
                call sigma_T_S(sigma,temp,sal)
            end if
        end function sigma_TS
        function potemp_TSd(temp,sal,depth) result(potemp)
            implicit none
            real,intent(in)::temp,sal,depth
            real::potemp
            call potemp_T_S_depth(potemp,temp,sal,depth)
        end function potemp_TSd
        ! calculates dynamic height at the deepest point in [m or cm], a 2d array is treated as a series of column arrays, same with calc_geovel
        subroutine calc_dh(dh_1D, temp_2D, sal_2D, den_2D, delta_zdb, unit)
            implicit none
            real, intent(in), optional :: temp_2D(:,:), sal_2D(:,:)
            real, intent(in), optional :: den_2D(:,:)
            real, dimension(:,:), allocatable :: den_2D_local
            real, intent(out), allocatable :: dh_1D(:)
            real,intent(in),optional::delta_zdb
            integer :: i, j
            real(kind=8) :: sum_p, sum_d, g, delta_zdb_local
            real(kind=8), dimension(:,:), allocatable :: integral_D, a
            character(len=*), intent(in), optional :: unit
            character(len=5)::unit_local

            unit_local = 'm'
            if(present(unit))unit_local = unit

            if (present(temp_2D) .and. present(sal_2D)) then
                if (size(temp_2D, 1) /= size(sal_2D, 1)) then
                    print *, 'Temperature and Salinity arrays must have the same size'
                    stop
                end if
                if (size(temp_2D, 2) /= size(sal_2D, 2)) then
                    print *, 'Temperature and Salinity arrays must have the same size'
                    stop
                end if
                allocate(den_2D_local(lbound(temp_2D, 1):ubound(temp_2D, 1), lbound(temp_2D, 2):ubound(temp_2D, 2)))
                call calc_density(temp_2D, sal_2D, den_2D_local)
            else if (present(den_2D)) then
                allocate(den_2D_local(lbound(den_2D, 1):ubound(den_2D, 1), lbound(den_2D, 2):ubound(den_2D, 2)))
                den_2D_local = den_2D
            else
                print *, 'A pair of Temperature and Salinity arrays or a Density array is required'
                stop
            end if
            ! print*,size(den_2D_local,1),size(den_2D_local,2)
        
            allocate(integral_D(lbound(den_2D_local, 1):ubound(den_2D_local, 1), lbound(den_2D_local, 2):ubound(den_2D_local, 2)))
            allocate(a(lbound(den_2D_local, 1):ubound(den_2D_local, 1), lbound(den_2D_local, 2):ubound(den_2D_local, 2)))                
            g = 9.81d0
            sum_p = 0.0d0
            sum_d = 0.0d0
            if(present(delta_zdb))then
                delta_zdb_local = real(delta_zdb,kind = 8)
            else
                delta_zdb_local = 1.0d0
            end if
            ! Integration of (specific volume) * dp = dynamic height
            do i = lbound(den_2D_local, 1), ubound(den_2D_local, 1)
                do j = lbound(den_2D_local, 2), ubound(den_2D_local, 2)
                    if (den_2D_local(i, j) /= 0.0) then
                        a(i, j) = 1.0d0 / (1000.0d0 + real(den_2D_local(i, j), kind=8)) ! Specific volume
                    else
                        a(i, j) = 0.0d0
                    end if
                    integral_D(i, j) = sum_d + a(i, j) * (10.0d0**(4.0d0)) * real(delta_zdb_local,kind=8) / g !1db is roughly 10^4 pascals
                    sum_d = integral_D(i, j)
                end do
                sum_p = 0.0d0
                sum_d = 0.0d0
            end do
            if(trim(unit_local) == 'm')then 
                dh_1d = real(integral_D(:,ubound(integral_D, 2)),kind=4) ! dynamic height of the deepest point
            else if(trim(unit_local) == 'cm')then
                dh_1d = real(integral_D(:,ubound(integral_D, 2)),kind=4) * 100.0
            else
                print*,'Unit must be either m or cm'
                stop
            end if
            deallocate(integral_D, a, den_2D_local)
            return
        
        end subroutine 
        function fcalc_dh(den,delta_zdb,unit) result(dh) 
            implicit none
            real,intent(in)::den
            real,intent(in),optional::delta_zdb
            real(kind = 4)::dh
            real(kind = 8)::g,delta_zdb_local
            character(len=*), intent(in), optional :: unit
            character(len=5)::unit_local
            unit_local = 'm'
            if(present(unit))unit_local = unit
            if(den == 0.)then ;dh = 0.;return;endif
            if(present(delta_zdb))then
                delta_zdb_local = real(delta_zdb,kind = 8)
            else
                delta_zdb_local = 1.0d0
            end if
            g = 9.81d0
            if(trim(unit_local) == 'm')then 
                dh = real(1.0d0 / (1000.0d0 + real(den, kind=8)) * (10.0d0**(4.0d0)) * real(delta_zdb_local,kind=8) / g,kind = 4)
            else if(trim(unit_local) == 'cm')then
                dh = real(1.0d0 / (1000.0d0 + real(den, kind=8)) * (10.0d0**(4.0d0)) * real(delta_zdb_local,kind=8) / g,kind = 4) * 100.0
            else
                print*,'Unit must be either m or cm'
                stop
            end if
        end function
        ! returns an array with one less column than the input array,arguments are all real(kind=4),the bottom row is the reference surface. unit is[m]
        subroutine calc_geovel(geovel_2D, delta_xm, delta_zdb, temp_2D, sal_2D, den_2D, f, lat)
            implicit none
            real, intent(in), optional :: temp_2D(:,:), sal_2D(:,:)
            real, intent(in), optional :: den_2D(:,:)
            real, dimension(:,:), allocatable :: den_2D_local
            real, intent(out), allocatable :: geovel_2D(:,:)
            real, intent(in) :: delta_xm
            real, intent(in),optional::delta_zdb
            real, intent(in), optional :: f, lat
            intrinsic :: sin, cos, tan, asin, acos
            real, parameter :: omega = 7.2921*(10.**(-5.))
            real :: f_local, pi
            integer :: i, j
            real(kind=8) :: sum_p, sum_d, g, diff, delta_zdb_local
            real(kind=8), dimension(:,:), allocatable :: integral_D, a, delta_D, v_ref0, v_refbottom
        
            ! Calculation of horizontal Coriolis factor
            pi = 2.0 * asin(1.0)
            if (present(f)) then
                f_local = f
            else if (present(lat)) then
                f_local = 2.0 * omega * sin(lat * pi / 180.0)
            else
                print *, 'Horizontal Coriolis factor (f) or Latitude is required'
                stop
            end if
        
            if (present(temp_2D) .and. present(sal_2D)) then
                if (size(temp_2D, 1) /= size(sal_2D, 1)) then
                    print *, 'Temperature and Salinity arrays must have the same size'
                    stop
                end if
                if (size(temp_2D, 2) /= size(sal_2D, 2)) then
                    print *, 'Temperature and Salinity arrays must have the same size'
                    stop
                end if
                allocate(den_2D_local(lbound(temp_2D, 1):ubound(temp_2D, 1), lbound(temp_2D, 2):ubound(temp_2D, 2)))
                call calc_density(temp_2D, sal_2D, den_2D_local)
            else if (present(den_2D)) then
                allocate(den_2D_local(lbound(den_2D, 1):ubound(den_2D, 1), lbound(den_2D, 2):ubound(den_2D, 2)))
                den_2D_local = den_2D
            else
                print *, 'A pair of Temperature and Salinity arrays or a Density array is required'
                stop
            end if
        
            allocate(integral_D(lbound(den_2D_local, 1):ubound(den_2D_local, 1), lbound(den_2D_local, 2):ubound(den_2D_local, 2)))
            allocate(a(lbound(den_2D_local, 1):ubound(den_2D_local, 1), lbound(den_2D_local, 2):ubound(den_2D_local, 2)))
            allocate(delta_D(lbound(den_2D_local, 1):ubound(den_2D_local, 1)-1, lbound(den_2D_local, 2):ubound(den_2D_local, 2)))
            allocate(v_ref0(lbound(den_2D_local, 1):ubound(den_2D_local, 1)-1, lbound(den_2D_local, 2):ubound(den_2D_local, 2)))
            allocate(v_refbottom(lbound(den_2D_local, 1):ubound(den_2D_local, 1)-1, lbound(den_2D_local, 2):ubound(den_2D_local, 2)))
            allocate(geovel_2D(lbound(den_2D_local, 1):ubound(den_2D_local, 1)-1, lbound(den_2D_local, 2):ubound(den_2D_local, 2)))
        
            ! print *, size(den_2D_local, 1), size(den_2D_local, 2), size(geovel_2D, 1), size(geovel_2D, 2)
        
            g = 9.81d0
            sum_p = 0.0d0
            sum_d = 0.0d0

            if(present(delta_zdb))then
                delta_zdb_local = real(delta_zdb,kind = 8)
            else
                delta_zdb_local = 1.0d0
            end if  
            ! Integration of (specific volume) * dp = dynamic height
            do i = lbound(den_2D_local, 1), ubound(den_2D_local, 1)
                do j = lbound(den_2D_local, 2), ubound(den_2D_local, 2)
                    if (den_2D_local(i, j) /= 0.0) then
                        a(i, j) = 1.0d0 / (1000.0d0 + real(den_2D_local(i, j), kind=8)) ! Specific volume
                    else
                        a(i, j) = 0.0d0
                    end if
                    ! if (j == lbound(den_2D_local, 2)) cycle
                    integral_D(i, j) = sum_d + a(i, j) * (10.0d0**(4.0d0)) * real(delta_zdb_local,kind=8) !1db is roughly 10^4 pascals
                    sum_d = integral_D(i, j)
                end do
                sum_p = 0.0d0
                sum_d = 0.0d0
            end do
        
            ! Calculation of differences in dynamic height (delta_D) in reference to the bottom row
            do i = lbound(den_2D_local, 1), ubound(den_2D_local, 1)-1
                do j = lbound(den_2D_local, 2), ubound(den_2D_local, 2)
                    if (integral_D(i, j) /= 0.0 .and. integral_D(i+1, j) /= 0.0) then
                        delta_D(i, j) = (integral_D(i+1, j) - integral_D(i, j))
                    else
                        delta_D(i, j) = 0.0d0
                    end if
                    v_ref0(i, j) = -delta_D(i, j) / (real(f_local, kind=8) * real(delta_xm, kind=8)) 
                end do
                diff = v_ref0(i, ubound(den_2D_local, 2)) * (-1.0d0)
                v_refbottom(i, :) = v_ref0(i, :) + diff
            end do
        
            geovel_2D = real(v_refbottom, kind=4)
            deallocate(integral_D, a, delta_D, v_ref0, v_refbottom, den_2D_local)
            return
        
        end subroutine 
    ! END DATA obtainment and manipulation
    ! COLORGRAD
        ! r,g,b individual color gradient
        subroutine colorgrad(rgb,iterations,r,g,b)
            implicit none
            integer,intent(in)::iterations
            real,dimension(:),allocatable,intent(out)::r,g,b
            integer::n
            character(len=*),intent(in)::rgb
            real::topsy

            allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1))
            if(rgb=='red'.or.rgb=='wred')then
                r(0) = 1.; g(0) = 1.; b(0) = 1.
                r(1) = 1.; g(1) = 0.9; b(1) = 0.9
                topsy=0.9
                if(rgb=='wred')then
                    r(1) = 1.; g(1) = 1.; b(1) = 1.
                    topsy = 1.
                end if
                do n = 2, iterations
                    r(n) = 1.
                    g(n) = topsy-(real(n-1)/real(iterations-1))*topsy
                    b(n) = topsy-(real(n-1)/real(iterations-1))*topsy
                end do
                r(iterations+1) = 0.6 ; g(iterations+1) = 0. ; b(iterations+1) = 0.
            else if(rgb=='green'.or.rgb=='wgreen')then
                r(0) = 1.; g(0) = 1.; b(0) = 1.
                r(1) = 0.9; g(1) = 1.; b(1) = 0.9
                topsy = 0.9
                if(rgb=='wgreen')then
                    r(1) = 1.; g(1) = 1.; b(1) = 1.
                    topsy = 1.
                end if
                do n = 2, iterations
                    r(n) = topsy-(real(n-1)/real(iterations-1))*topsy
                    g(n) = 1.
                    b(n) = topsy-(real(n-1)/real(iterations-1))*topsy
                end do
                r(iterations+1) = 0. ; g(iterations+1) = 0.6 ; b(iterations+1) = 0.
            else if(rgb=='blue'.or.rgb=='wblue')then
                r(0) = 1.; g(0) = 1.; b(0) = 1.
                r(1) = 0.9; g(1) = 0.9; b(1) = 1.
                topsy = 0.9
                if(rgb=='wblue')then
                    r(1) = 1.; g(1) = 1.; b(1) = 1.
                    topsy=1.
                end if
                do n = 2, iterations
                    r(n) = topsy-(real(n-1)/real(iterations-1))*topsy
                    g(n) = topsy-(real(n-1)/real(iterations-1))*topsy
                    b(n) = 1.
                end do
                r(iterations+1) = 0. ; g(iterations+1) = 0. ; b(iterations+1) = 0.6
            else;print*,'Choose From red, green, blue'
            end if
        end subroutine
        ! blue to red
        subroutine b2r_colorgrad(iterations,midpoint,r,g,b)
            implicit none
            integer,intent(in)::iterations,midpoint
            real,dimension(:),allocatable,intent(out)::r,g,b
            integer::n
            
            if(iterations-midpoint<2)then;print*,'midpoint is too close to the end (b2r)';stop;endif
            allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1))
            do n = 1, iterations
                if(midpoint==1.and.n==1)then;r(1)=0.;g(1)=0.;b(1)=1.;cycle;endif
                if (n <= midpoint) then 
                    r(n) = 0.+(real(n-1)/real(midpoint-1))*0.90
                    g(n) = 0.+(real(n-1)/real(midpoint-1))*0.90
                    b(n) = 1.
                else
                    r(n) = 1.
                    g(n) = 0.90-(real(n-midpoint-1)/real(iterations-midpoint-1))*0.90
                    b(n) = 0.90-(real(n-midpoint-1)/real(iterations-midpoint-1))*0.90
                end if
            end do

            r(0) = 0.; g(0) = 0.; b(0) = 0.6
            r(iterations+1) = 0.6 ; g(iterations+1) = 0. ; b(iterations+1) = 0.

        end subroutine
        subroutine b2w2r_colorgrad(iterations,midpoint,r,g,b)
            implicit none
            integer,intent(in)::iterations,midpoint
            real,dimension(:),allocatable,intent(out)::r,g,b
            integer::n
            
            if(iterations-midpoint<2)then;print*,'midpoint is too close to the end (b2w2r)';stop;endif
            allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1))
            do n = 1, iterations
                if(midpoint==1)then;print*,'midpoint of 0 is invalid for b2w2r colorgrad';stop;endif
                if (n < midpoint) then 
                    r(n) = 0.+(real(n-1)/real(midpoint-2))*0.85
                    g(n) = 0.+(real(n-1)/real(midpoint-2))*0.85
                    b(n) = 1.
                else if(n==midpoint)then
                    r(n)=1.;g(n)=1.;b(n)=1.
                else
                    r(n) = 1.
                    g(n) = 0.85-(real(n-midpoint-1)/real(iterations-midpoint-1))*0.85
                    b(n) = 0.85-(real(n-midpoint-1)/real(iterations-midpoint-1))*0.85
                end if
            end do

            r(0) = 0.; g(0) = 0.; b(0) = 0.6
            r(iterations+1) = 0.6 ; g(iterations+1) = 0. ; b(iterations+1) = 0.

        end subroutine
        ! blue to grey to red
        subroutine b2gy2r_colorgrad(iterations,midpoint,r,g,b)
            implicit none
            integer,intent(in)::iterations,midpoint
            real,dimension(:),allocatable,intent(out)::r,g,b
            integer::n
            
            if(iterations-midpoint<2)then;print*,'midpoint is too close to the end (b2gy2r)';stop;endif
            allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1))
            do n = 1, midpoint-1
                    r(n) = 0.+(real(n-1)/real(midpoint-2))*0.8
                    g(n) = 0.+(real(n-1)/real(midpoint-2))*0.8
                    b(n) = 1.
            end do
            do n = midpoint+1,iterations
                    r(n) = 1.
                    g(n) = 0.8-(real(n-midpoint-1)/real(iterations-midpoint-1))*0.8
                    b(n) = 0.8-(real(n-midpoint-1)/real(iterations-midpoint-1))*0.8
            end do
            r(midpoint) = 0.85;g(midpoint) = 0.85;b(midpoint) = 0.85



            r(0) = 0.; g(0) = 0.; b(0) = 0.6
            r(iterations+1) = 0.6 ; g(iterations+1) = 0. ; b(iterations+1) = 0.
        end subroutine
        ! red to green
        subroutine r2g_colorgrad(iterations,midpoint,r,g,b)
            implicit none
            integer,intent(in)::iterations,midpoint
            real,dimension(:),allocatable,intent(out)::r,g,b
            integer::n 

            if(iterations==midpoint)then;print*,'iterations and midpoint cannot be the same value (r2g)';stop;endif
            allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1))
            do n = 1,iterations
                if(n<=midpoint) then
                    r(n) = 1.
                    g(n) = real(n-1)/real(midpoint-1)
                    b(n) = 0.
                else
                    r(n) = 1.-real(n-midpoint)/real(iterations-midpoint)
                    g(n) = 1.
                    b(n) = 0.
                end if
            end do
            ! r(0) = 0.; g(0) = 0.; b(0) = 0.
        end subroutine
        ! black to red, then gradient from red to green. 
        subroutine bk2r2g_colorgrad(iterations,midpoint,r,g,b)
            implicit none
            integer,intent(in)::iterations,midpoint
            real,dimension(:),allocatable,intent(out)::r,g,b 
            integer::n 

            if(iterations==midpoint)then;print*,'iterations and midpoint cannot be the same value (bk2r2g)';stop;endif
            allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1))
            r(1) = 0.; g(1) = 0.; b(1) = 0.
            do n = 2,iterations
                if(n<=midpoint) then
                    r(n) = 1.
                    g(n) = real(n-2)/real(midpoint-2)
                    b(n) = 0.
                else 
                    r(n) = 1.-real(n-midpoint)/real(iterations-midpoint)
                    g(n) = 1.
                    b(n) = 0.
                end if
            end do

        end subroutine
        ! b<breakpoint1>g<breakpoint2>y<breakpoint3>r. bluecyan==dark->light, yellowred==light->dark
        subroutine b2cy2y2r_colorgrad(iterations,breakpoint1,breakpoint2,breakpoint3,r,g,b)
            implicit none
            integer,intent(in)::iterations,breakpoint1,breakpoint2,breakpoint3
            real,dimension(:),allocatable,intent(out)::r,g,b
            integer::n

            allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1))
            r(0)=0.;g(0)=0.;b(0)=0.9
            ! from dark blue to light blue
            do n = 1, breakpoint1
                r(n) = 0.+(real(n-1)/real(breakpoint1-1))*0.8
                g(n) = 0.+(real(n-1)/real(breakpoint1-1))*0.8
                b(n) = 1.
            end do
            ! from dark cyan to light cyan
            do n = 1,breakpoint2-breakpoint1
                r(n+breakpoint1) = (real(n-1)/real(breakpoint2-breakpoint1-1))*0.9
                g(n+breakpoint1) = 1.
                b(n+breakpoint1) = 1.
            end do

            ! from light yellow to dark yellow
            do n = 1, breakpoint3-breakpoint2
                r(n+breakpoint2) = 1.
                g(n+breakpoint2) = 1.
                b(n+breakpoint2) = 0.9-(real(n-1)/real(breakpoint3-breakpoint2-1))*0.9
            end do
            ! from light red to dark red
            do n = 1, iterations-breakpoint3
                r(n+breakpoint3) = 1.
                g(n+breakpoint3) = 0.9-(real(n-1)/real(iterations-breakpoint3-1))*0.9
                b(n+breakpoint3) = 0.9-(real(n-1)/real(iterations-breakpoint3-1))*0.9
            end do
            r(iterations+1) = 0.9;g(iterations+1) = 0.;b(iterations+1) = 0.
        end subroutine
        ! b,zeropoint,g->y->r. blue==dark->light, greenyellowred==light->dark
        subroutine b2g2y2r_colorgrad(iterations,breakpoint1,breakpoint2,breakpoint3,r,g,b)
            implicit none
            integer,intent(in)::iterations,breakpoint1,breakpoint2,breakpoint3
            real,dimension(:),allocatable,intent(out)::r,g,b
            integer::n

            allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1))
            r(0)=0.;g(0)=0.;b(0)=0.9
            ! from dark blue to light blue
            do n = 1, breakpoint1
                r(n) = 0.+(real(n-1)/real(breakpoint1-1))*0.8
                g(n) = 0.+(real(n-1)/real(breakpoint1-1))*0.8
                b(n) = 1.
            end do
            ! from dark green to light green
            do n = 1,breakpoint2-breakpoint1
                r(n+breakpoint1) = 0.9-(real(n-1)/real(breakpoint2-breakpoint1-1))*0.9
                g(n+breakpoint1) = 1.
                b(n+breakpoint1) = 0.9-(real(n-1)/real(breakpoint2-breakpoint1-1))*0.9
            end do

            ! from light yellow to dark yellow
            do n = 1, breakpoint3-breakpoint2
                r(n+breakpoint2) = 1.
                g(n+breakpoint2) = 1.
                b(n+breakpoint2) = 0.9-(real(n-1)/real(breakpoint3-breakpoint2-1))*0.9
            end do
            ! from light red to dark red
            do n = 1, iterations-breakpoint3
                r(n+breakpoint3) = 1.
                g(n+breakpoint3) = 0.9-(real(n-1)/real(iterations-breakpoint3-1))*0.9
                b(n+breakpoint3) = 0.9-(real(n-1)/real(iterations-breakpoint3-1))*0.9
            end do
            r(iterations+1) = 0.9;g(iterations+1) = 0.;b(iterations+1) = 0.
        end subroutine
        subroutine dozencolors(r,g,b)
            real, dimension(:),allocatable,intent(out) :: r, g, b
            allocate (r(12), g(12), b(12))
            r = (/ 1.0, 0.0, 0.0, .8, 0.0, 1.0, 1.0, 0.75, 1.0, 0.5, 0.0, 0.65 /)
            g = (/ 0.0, 1.0, 0.0, .8, 1.0, 0.0, 0.65, 1.0, 0.41, 0.0, 0.5, 0.16 /)
            b = (/ 0.0, 0.0, 1.0, 0.0, 1.0, .8, 0.0, 0.0, 0.71, 0.5, 0.5, 0.16 /)
        end subroutine
        ! END COLORGRAD
        subroutine rgb_to_cmyk(r, g, b, c, m, y, k)
            implicit none
            real, intent(in) :: r, g, b
            real, intent(out) :: c, m, y, k
            real :: r_norm, g_norm, b_norm
        
            r_norm = r / 255.0
            g_norm = g / 255.0
            b_norm = b / 255.0
        
            k = 1.0 - max(r_norm, g_norm, b_norm)
            if (k < 1.0) then
                c = (1.0 - r_norm - k) / (1.0 - k)
                m = (1.0 - g_norm - k) / (1.0 - k)
                y = (1.0 - b_norm - k) / (1.0 - k)
            else
                c = 0.0
                m = 0.0
                y = 0.0
            end if
        end subroutine rgb_to_cmyk
        subroutine cmyk_to_rgb(c, m, y, k, r, g, b)
            implicit none
            real, intent(in) :: c, m, y, k
            real, intent(out) :: r, g, b
        
            r = 255.0 * (1.0 - c) * (1.0 - k)
            g = 255.0 * (1.0 - m) * (1.0 - k)
            b = 255.0 * (1.0 - y) * (1.0 - k)
        end subroutine cmyk_to_rgb
        subroutine centeralize_colors(iterations,midpoint,r,g,b)
            implicit none
            integer,intent(in)::iterations,midpoint
            real,dimension(0:),intent(inout)::r,g,b
            integer::n

            if(lbound(r,1).ne.0 .or. ubound(r,1).ne.(iterations+1))then
                print*,'Iterations and the array size do not match in SUBROUTINE (centeralize_colors)'
            end if
            if(midpoint>3)then
                do n = 2, iterations-1
                    if(n<midpoint-1)then
                        r(n) = r(n) + (r(midpoint)-r(n))/1.7**(real(midpoint-n))
                        g(n) = g(n) + (g(midpoint)-g(n))/1.7**(real(midpoint-n))
                        b(n) = b(n) + (b(midpoint)-b(n))/1.7**(real(midpoint-n))
                    else if(n>midpoint+1)then
                        r(n) = r(n) + (r(midpoint)-r(n))/1.7**(real(n-midpoint))
                        g(n) = g(n) + (g(midpoint)-g(n))/1.7**(real(n-midpoint))
                        b(n) = b(n) + (b(midpoint)-b(n))/1.7**(real(n-midpoint))
                    end if
                end do
            else if(midpoint<3)then ! colors in midpoint dont change
                if(midpoint+1>iterations-1)then
                    print*,'no change in subroutine centralize_colors',midpoint+1,'>',iterations
                    return
                end if 
                if(iterations<3)then
                    do n = midpoint+1,iterations
                        r(n) = r(n) + (r(midpoint)-r(n))/1.7**(real(n-midpoint))
                        g(n) = g(n) + (g(midpoint)-g(n))/1.7**(real(n-midpoint))
                        b(n) = b(n) + (b(midpoint)-b(n))/1.7**(real(n-midpoint))
                    end do
                    print*, 'colors shifted to the left relative of ',midpoint,'(subroutine centeralize_colors)'
                else 
                    do n = midpoint+1,iterations-1
                        r(n) = r(n) + (r(midpoint)-r(n))/1.7**(real(n-midpoint))
                        g(n) = g(n) + (g(midpoint)-g(n))/1.7**(real(n-midpoint))
                        b(n) = b(n) + (b(midpoint)-b(n))/1.7**(real(n-midpoint))
                    end do
                    print*, 'colors shifted to the left relative of ',midpoint,'(subroutine centeralize_colors)'
                end if
            end if
        end subroutine
    ! END COLORGRAD  

    ! PLOTS
        subroutine box(width,height,thickness,x,y)
            implicit none
            real,intent(in)::width,height
            real,intent(in),optional::x,y
            integer,intent(in),optional::thickness
            if(present(x).and.present(y))call plot(x,y,-3)
            if(present(x).and. .not.present(y))call plot(x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,y,-3)
            if(present(thickness))call newpen2(thickness)
            call plot(0.,0.,3);call plot(width,0.,2);call plot(width,height,2);call plot(0.,height,2);call plot(0.,0.,2)
            if(present(x).and.present(y))call plot(-x,-y,-3)
            if(present(x).and. .not.present(y))call plot(-x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,-y,-3)
        end subroutine    
        subroutine griddedbox(width,height,thickness,ngridsx,ngridsy,x,y,dashy)
            implicit none
            real,intent(in)::width,height
            integer,intent(in)::ngridsx,ngridsy,thickness
            integer,intent(in),optional::dashy
            real,intent(in),optional::x,y
            if(present(x).and.present(y))call plot(x,y,-3)
            if(present(x).and. .not.present(y))call plot(x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,y,-3)
            call newpen2(thickness)
            call box(width,height,thickness)
            if(present(dashy))then
                call floating_lines(width,0.,ngridsy-1,thickness,y_inc = height/real(ngridsy),y = height/real(ngridsy),dashy = dashy)
                call floating_lines(height,90.,ngridsx-1,thickness,x_inc = width/real(ngridsx),x = width/real(ngridsx),dashy = dashy)
            else;call floating_lines(width,0.,ngridsy-1,thickness,y_inc = height/real(ngridsy),y = height/real(ngridsy),dashy = dashy)
                call floating_lines(height,90.,ngridsx-1,thickness,x_inc = width/real(ngridsx),x = width/real(ngridsx))
            end if
            if(present(x).and.present(y))call plot(-x,-y,-3)
            if(present(x).and. .not.present(y))call plot(-x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,-y,-3)
        end subroutine
        ! creating map   line_opt == 1 means NLine, line_opt == 2 means SLine, line_opt == 3 means both
        subroutine map(ini_lat,fin_lat,ini_long,fin_long,width,ini_st,fin_st,line_opt,symbol_size,description)
            implicit none
            integer,intent(in)::ini_lat,fin_lat,ini_long,fin_long
            integer,intent(in),optional::ini_st,fin_st,line_opt
            real,intent(in):: width
            real,intent(in),optional::symbol_size
            logical,intent(in),optional::description
            intrinsic sin,cos,tan,asin,acos
            integer,parameter::imax = 2080,jmax = 2640,station_x = 9, station_y = 2
            real,dimension(:,:),allocatable::dep,butler_array
            integer,dimension(:,:),allocatable::dep_0
            integer::i,j,is,ie,js,je,n,line_num,ini_st_local,fin_st_local,line_opt_local   
            real::dx,dy,height,ratio,pi,xco,NLineYco,SLineYco,symbol_size_local
            character::line_name*10,filename*999
            real,dimension(station_y,station_x)::lon
            logical::dstat = .false.

            allocate(dep(imax,jmax));allocate(dep_0(imax,jmax))
            dep = 0.;dep_0 = 1
            open(21,file='../Data/japan1km122-148_24-46.bin',form='unformatted',status='old')
            do j = jmax,1,-1  ! reading in reverse so i can draw map from bottom left
                read(21)dep(:,j)
            end do
            close(21)
            dep = -dep
            ! maxval(dep) == -9784 minval(dep) == 3660 (fuji); Note that the data is in meters and z axis is positive upwards
            if(present(ini_st))then;ini_st_local = ini_st;else;ini_st_local = 1;end if
            if(present(fin_st))then;fin_st_local = fin_st;else;fin_st_local = 6;end if
            if(present(line_opt))then;line_opt_local = line_opt;else;line_opt_local = 3;end if
            if(present(symbol_size))then;symbol_size_local = symbol_size;else;symbol_size_local = width/11.;end if
            if(present(description))then 
                if(description)dstat = .true.
            end if
            if (ini_lat<24 .or. ini_lat>46 .or. fin_lat<24 .or. fin_lat>46 .or. ini_long<122 .or. ini_long>148 .or. fin_long<122 .or. fin_long>148 .or.ini_lat>fin_lat .or. ini_long>fin_long) then
                print*, 'Your map coordinates must be within 24-46N and 122-148E'
            else
                js = (ini_lat-24)*120+1
                je = (fin_lat-24)*120
                is = (ini_long-122)*80+1
                ie = (fin_long-122)*80
                pi = 2.*asin(1.)
                ratio = 6357./6378./cos((ini_lat+fin_lat)/2.*pi/180.)
                height = width*ratio*real(fin_lat-ini_lat)/real(fin_long-ini_long)
                dx = width/real(ie-is)
                dy = height/real(je-js)
                allocate(butler_array(ie-is+1,je-js+1))
                butler_array = dep(is:ie,js:je)
                call butler_psmask(butler_array,width,height,0.,3700.,r=0.8,g=.9,b=0.1)  ! land
                call butler_psmask(butler_array,width,height,-200.,0.,r = 0.8,g=0.9,b = 1.) ! <=200m
                call butler_psmask(butler_array,width,height,-1000.,-200.,r = 0.65,g=0.75,b=1.) ! 200-1000m
                call butler_psmask(butler_array,width,height,-2000.,-1000.,r = 0.5,g=0.6,b=1.) ! 1000-2000m
                call butler_psmask(butler_array,width,height,-3000.,-2000.,r = 0.35,g=0.45,b=.9) ! 2000-3000m
                call butler_psmask(butler_array,width,height,-10000.,-3000.,r = 0.2,g=0.3,b=.8) ! >3000m
                print*,'Deepest Point of Your Map Domain is;',minval(butler_array)
                print*,'Heighest Point of Your Map Domain is;',maxval(butler_array)
                if (symbol_size_local<=0.2) then;call newpen2(1);else if(symbol_size_local>=0.2 .and. symbol_size_local<=0.4) then;call newpen2(2);else;call newpen2(3);end if
                call rgbk(0.,0.,0.) 
                call pscont3(dx,dy,dep,dep_0,is,ie,js,je,imax,jmax,1,0.,0.)
                call pscont3(dx,dy,dep,dep_0,is,ie,js,je,imax,jmax,1,-200.,0.) 
                ! if (symbol_size_local<=0.2) then;call newpen2(2);else if(symbol_size_local>=0.2 .and. symbol_size_local<=0.4) then;call newpen2(3);else;call newpen2(4);end if
                call pscont3(dx,dy,dep,dep_0,is,ie,js,je,imax,jmax,1,-1000.,0.) ! solid
                call pscont3(dx,dy,dep,dep_0,is,ie,js,je,imax,jmax,1,-2000.,0.) ! solid
                call pscont3(dx,dy,dep,dep_0,is,ie,js,je,imax,jmax,1,-3000.,0.) ! solid
                call pscont3(dx,dy,dep,dep_0,is,ie,js,je,imax,jmax,1,-10000.,0.) ! solid
            end if
            call rgbk(0.,0.,0.)
            call box(width,height)
            call num_memori(real(ini_lat),real(fin_lat),(fin_lat-ini_lat)*10+1,10,symbol_size_local,-1,height,-90)
            call num_memori(real(ini_long),real(fin_long),(fin_long-ini_long)*10+1,10,symbol_size_local,-1,width,0)
            
            call symbolc(width/2.,-symbol_size_local*2.6,symbol_size_local*0.8,'Longitude (deg.E)')
            call symbolc(-symbol_size_local*2.5,height/2.,symbol_size_local*0.8,'Latitude (deg.N)',90.)
            if(ini_long<=137 .and.fin_long>=140 .and. ini_lat<=40 .and. fin_lat>=41) then
                do line_num = 1,station_y;if(line_num == 1) then; line_name = 'N-Line';else;line_name = 'S-Line';end if
                    filename = '../Data/Coordinates/'//trim(line_name)//'/lon.csv'
                    open(32,file=filename,status = 'old',action = 'read')
                    read(32,'(9(f9.4))')(lon(line_num,i),i = 1,station_x)
                    close(32)
                    NLineYco = dy*(41.-real(ini_lat))*120.;SLineYco = dy*(40.6-real(ini_lat))*120.
                    if(line_opt_local/=2)then
                        call plot(dx*(lon(1,10-fin_st_local)-real(ini_long))*80.,NLineYco,3);call plot(dx*(140.-real(ini_long))*80.,NLineYco,2)
                    end if
                    if(line_opt_local/=1)then
                        call plot(dx*(lon(2,10-fin_st_local)-real(ini_long))*80.,SLineYco,3);call plot(dx*(139.75-real(ini_long))*80.,SLineYco,2)
                    end if
                    do n = ini_st_local,fin_st_local; xco = dx*(lon(line_num,10-n)-real(ini_long))*80.
                        if (line_num ==1 .and. line_opt_local/=2) then
                            if(n==1.or.n==2.or.n==3)then
                                call rgbk(1.,1.,1.);call gmark(xco,NLineYco,symbol_size_local*0.4,1)
                                call rgbk(0.,0.,0.);call gmark(xco,NLineYco,symbol_size_local*0.3,1)
                                if(dstat)call numberc(xco,NLineYco+symbol_size_local*0.25,symbol_size_local*0.8,real(n),0.,-1)
                            else if(n==4.or.n==5.or.n==6)then
                                call rgbk(1.,1.,1.);call gmark(xco,NLineYco,symbol_size_local*0.4,1)
                                call rgbk(0.,0.,0.);call gmark(xco,NLineYco,symbol_size_local*0.3,1)
                                if(dstat)call numberc(xco,NLineYco+symbol_size_local*0.25,symbol_size_local*0.8,real(n),0.,-1)
                            else if(n==7.or.n==8.or.n==9)then
                                call rgbk(1.,1.,1.);call gmark(xco,NLineYco,symbol_size_local*0.4,1)  ! separating stations by 3 in case i want to change the shapes
                                call rgbk(0.,0.,0.);call gmark(xco,NLineYco,symbol_size_local*0.3,1)
                                if(dstat)call numberc(xco,NLineYco+symbol_size_local*0.25,symbol_size_local*0.8,real(n),0.,-1)
                            else;end if
                        ! else if(line_num ==2 .and. line_opt_local /= 1) then
                        !     if(n==1.or.n==2.or.n==3)then;call gmark(xco,SLineYco,symbol_size_local*0.4,1);call numberc(xco,SLineYco-symbol_size_local*1.2,symbol_size_local*0.8,real(n),0.,-1)
                        !     else if(n==4.or.n==5.or.n==6)then;call gmark(xco,SLineYco,symbol_size_local*0.4,6);call numberc(xco,SLineYco-symbol_size_local*1.2,symbol_size_local*0.8,real(n),0.,-1)
                        !     else if(n==7.or.n==8.or.n==9)then;call gmark(xco,SLineYco,symbol_size_local*0.4,8);call numberc(xco,SLineYco-symbol_size_local*1.2,symbol_size_local*0.8,real(n),0.,-1)
                        !     else;end if
                        else;end if
                    end do
                end do
                if(line_opt_local ==1 ) then
                    if(dstat)call symbolr(dx*(lon(1,6)-real(ini_long))*80.-symbol_size_local/2.,NLineYco+symbol_size_local*1.1,symbol_size_local,'N-Line',0.,6)
                else if(line_opt_local==2) then
                    if(dstat)call symbolr(dx*(lon(2,6)-real(ini_long))*80.-symbol_size_local/2.,SLineYco-symbol_size_local*1.2,symbol_size_local,'S-Line',0.,6)
                else
                    if(dstat) then
                        call symbolr(dx*(lon(1,6)-real(ini_long))*80.-symbol_size_local/2.,NLineYco+symbol_size_local*1.1,symbol_size_local,'N-Line',0.,6)
                        call symbolr(dx*(lon(2,6)-real(ini_long))*80.-symbol_size_local/2.,SLineYco-symbol_size_local*1.2,symbol_size_local,'S-Line',0.,6)
                    end if
                end if
            else
                print*,'stations are just outside of your map, like a perfect flower that is just beyond your reach...(mj)'
            end if
            ! ESA
            if(ini_lat<41.and.fin_lat>41.and.ini_long<140.and.fin_long>140)then
                call rgbk(1.,1.,1.)
                call gmark(dx*(140.+(8./60.)+(33./3600.)-real(ini_long))*80.,dy*(41.+(53./60.)+(59./3600.)-real(ini_lat))*120.,symbol_size_local*0.45,1)
                call rgbk(0.,0.,0.)
                call gmark(dx*(140.+(8./60.)+(33./3600.)-real(ini_long))*80.,dy*(41.+(53./60.)+(59./3600.)-real(ini_lat))*120.,symbol_size_local*0.3,4)
                if(dstat)call symbol(dx*(140.+(8./60.)+(33./3600.)-real(ini_long))*80.+symbol_size_local/3.,dy*(41.+(53./60.)+(59./3600.)-real(ini_lat))*120.,symbol_size_local*0.8,'ESA',0.)
            end if
            ! OKU esa and oku 61km apart
            if(ini_lat<42.and.fin_lat>42.and.ini_long<139.and.fin_long>139)then
                call rgbk(1.,1.,1.)
                call gmark(dx*(139.+(29./60.)+(22./3600.)-real(ini_long))*80.,dy*(42.+(4./60.)+(43./3600.)-real(ini_lat))*120.,symbol_size_local*0.45,1)
                call rgbk(0.,0.,0.)
                call gmark(dx*(139.+(29./60.)+(22./3600.)-real(ini_long))*80.,dy*(42.+(4./60.)+(43./3600.)-real(ini_lat))*120.,symbol_size_local*0.3,4)
                if(dstat)call symbolr(dx*(139.+(29./60.)+(22./3600.)-real(ini_long))*80.-symbol_size_local/3.,dy*(42.+(4./60.)+(43./3600.)-real(ini_lat))*120.,symbol_size_local*0.8,'OKU',0.)
            end if
            ! SAK
            if(ini_lat<39.and.fin_lat>39.and.ini_long<139.and.fin_long>139)then
                call rgbk(1.,1.,1.)
                call gmark(dx*(139.+(49./60.)+(25./3600.)-real(ini_long))*80.,dy*(38.+(55./60.)+(3./3600.)-real(ini_lat))*120.,symbol_size_local*0.45,1)
                call rgbk(0.,0.,0.)
                call gmark(dx*(139.+(49./60.)+(25./3600.)-real(ini_long))*80.,dy*(38.+(55./60.)+(3./3600.)-real(ini_lat))*120.,symbol_size_local*0.3,4)
                if(dstat)call symbol(dx*(139.+(49./60.)+(25./3600.)-real(ini_long))*80.+symbol_size_local/3.,dy*(38.+(55./60.)+(3./3600.)-real(ini_lat))*120.,symbol_size_local*0.8,'SAK',0.)
            end if
            ! TOB sak and tob 38km apart
            if(ini_lat<39.and.fin_lat>39.and.ini_long<139.and.fin_long>139)then
                call rgbk(1.,1.,1.)
                call gmark(dx*(139.+(32./60.)+(51./3600.)-real(ini_long))*80.,dy*(39.+(11./60.)+(8./3600.)-real(ini_lat))*120.,symbol_size_local*0.45,1)
                call rgbk(0.,0.,0.)
                call gmark(dx*(139.+(32./60.)+(51./3600.)-real(ini_long))*80.,dy*(39.+(11./60.)+(8./3600.)-real(ini_lat))*120.,symbol_size_local*0.3,4)
                if(dstat)call symbolr(dx*(139.+(32./60.)+(51./3600.)-real(ini_long))*80.-symbol_size_local/3.,dy*(39.+(11./60.)+(8./3600.)-real(ini_lat))*120.,symbol_size_local*0.8,'TOB',0.)
            end if
            if((ini_lat<41 .and. fin_lat>41 .and. ini_long<140 .and. fin_long>140))then 
                call rgbk(1.,1.,1.)
                call gmark(dx*(139.+(55./60.)+(35./3600.)-real(ini_long))*80.,dy*(40.+(38./60.)+(50./3600.)-real(ini_lat))*120.,symbol_size_local*0.45,1)
                call rgbk(0.,0.,0.)
                call gmark(dx*(139.+(55./60.)+(35./3600.)-real(ini_long))*80.,dy*(40.+(38./60.)+(50./3600.)-real(ini_lat))*120.,symbol_size_local*0.3,4)
                if(dstat)call symbolr(dx*(139.+(32./60.)+(51./3600.)-real(ini_long))*80.-symbol_size_local/3.,dy*(39.+(11./60.)+(8./3600.)-real(ini_lat))*120.,symbol_size_local*0.8,'TOB',0.)
            end if
            deallocate(dep,dep_0,butler_array)
                
        end subroutine
        subroutine simple_map(ini_lat,fin_lat,ini_long,fin_long,width,symbol_size,r,g,b,symbol_freq,symbols,paintland)
            implicit none
            integer,intent(in)::ini_lat,fin_lat,ini_long,fin_long
            integer,intent(in),optional::symbol_freq
            real,intent(in):: width
            real,intent(in),optional::symbol_size,r,g,b
            intrinsic sin,cos,tan,asin,acos
            integer,parameter::imax = 2080,jmax = 2640
            real,dimension(:,:),allocatable::dep,butler_array
            integer,dimension(:,:),allocatable::dep_0
            logical,intent(in),optional::symbols,paintland
            integer::j,is,ie,js,je,symbol_freq_local
            real::dx,dy,height,ratio,pi,symbol_size_local,rl,gl,bl,NLineYco,SLineYco
            logical::symbols_local,paintland_local
            
            !! local parameters
            call rgbk(0.,0.,0.)
            symbols_local = .false.
            if(present(symbols))symbols_local = symbols
            rl = 0.;gl = 0.;bl = 0.
            if(present(r))rl = r
            if(present(g))gl = g
            if(present(b))bl = b
            if(present(symbol_freq))then 
                symbol_freq_local = symbol_freq
            else
                symbol_freq_local = 10
            end if

            paintland_local = .true.
            if(present(paintland))paintland_local = paintland
            !!
            call box(width,height)
            allocate(dep(imax,jmax));allocate(dep_0(imax,jmax))
            dep = 0.;dep_0 = 1
            open(21,file='../Data/japan1km122-148_24-46.bin',form='unformatted',status='old')
            do j = jmax,1,-1  ! reading in reverse so i can draw map from bottom left
                read(21)dep(:,j)
            end do
            close(21)
            dep = -dep
            ! maxval(dep) == -9784 minval(dep) == 3660 (fuji); Note that the data is in meters and z axis is positive upwards
            if(present(symbol_size))then;symbol_size_local = symbol_size;else;symbol_size_local = width/11.;end if

            if (ini_lat<24 .or. ini_lat>46 .or. fin_lat<24 .or. fin_lat>46 .or. ini_long<122 .or. ini_long>148 .or. fin_long<122 .or. fin_long>148 .or.ini_lat>fin_lat .or. ini_long>fin_long) then
                print*, 'Your map coordinates must be within 24-46N and 122-148E'
            else
                js = (ini_lat-24)*120+1
                je = (fin_lat-24)*120
                is = (ini_long-122)*80+1
                ie = (fin_long-122)*80
                pi = 2.*asin(1.)
                ratio = 6357./6378./cos((ini_lat+fin_lat)/2.*pi/180.)
                height = width*ratio*real(fin_lat-ini_lat)/real(fin_long-ini_long)
                dx = width/real(ie-is)
                dy = height/real(je-js)
               
                print*,'Deepest Point of Your Map Domain is;',minval(dep)
                print*,'Heighest Point of Your Map Domain is;',maxval(dep)
                if (width/11.<=0.2) then;call newpen2(2);else if(width/11.>=0.2 .and. width/11.<=0.4) then;call newpen2(3);else;call newpen2(5);end if
                call rgbk(rl,gl,bl) 
                call pscont3(dx,dy,dep,dep_0,is,ie,js,je,imax,jmax,1,0.,0.)
                if(paintland_local)then
                    allocate(butler_array(ie-is+1,je-js+1))
                    butler_array = dep(is:ie,js:je)
                    call butler_psmask(butler_array,width,height,0.,3700.,r=0.8,g=.9,b=0.1)  ! land
                endif 
            end if
            call rgbk(0.6,0.6,0.6)
            NLineYco = dy*(41.-real(ini_lat))*120.;SLineYco = dy*(40.6-real(ini_lat))*120.
            call plot(dx*(138.3333-real(ini_long))*80.,NLineYco,3);call plot(dx*(140.-real(ini_long))*80.,NLineYco,2)
            call plot(dx*(138.3333-real(ini_long))*80.,SLineYco,3);call plot(dx*(139.75-real(ini_long))*80.,SLineYco,2)

            call rgbk(0.,0.,0.)
            if(symbols_local)then 
                call num_memori(real(ini_lat),real(fin_lat),(fin_lat-ini_lat)*2+1,symbol_freq_local,symbol_size_local,-1,height,-90)
                call num_memori(real(ini_long),real(fin_long),(fin_long-ini_long)*2+1,symbol_freq_local*2,symbol_size_local,-1,width,0)
            else
                call memori((fin_lat-ini_lat)*2,symbol_size_local*0.2,symbol_freq,height,-90.,y = height/2.)
                call memori((fin_long-ini_long)*2,symbol_size_local*0.2,symbol_freq,width,0.,x = width/2.)
            end if

            
            ! call symbolc(width/2.,-symbol_size_local*2.6,symbol_size_local*0.8,'Longitude (deg.E)')
            ! call symbolc(-symbol_size_local*2.5,height/2.,symbol_size_local*0.8,'Latitude (deg.N)',90.)
            deallocate(dep,dep_0)
            if(paintland_local)deallocate(butler_array)
        end subroutine
        subroutine floating_numbers(ini_num,num_inc,iterations,symbol_size,x_inc,y_inc,rangle,float_quantity,x,y)
            real,intent(in)::symbol_size,x_inc,y_inc,rangle,ini_num,num_inc
            real,intent(in),optional::x,y
            integer,intent(in)::iterations,float_quantity
            integer::n
            real::printnum

            if(present(x).and.present(y))call plot(x,y,-3)
            if(present(x).and. .not.present(y))call plot(x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,y,-3)
            do n = 1, iterations
                printnum = ini_num + num_inc*real(n-1)
                call numberc(real(n-1)*x_inc,real(n-1)*y_inc,symbol_size,printnum,rangle,float_quantity)
            end do
            if(present(x).and.present(y))call plot(-x,-y,-3)
            if(present(x).and. .not.present(y))call plot(-x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,-y,-3)
        end subroutine
        subroutine floating_lines(length,rangle,iterations,line_thickness,x_inc,y_inc,x,y,dashy)
            real,intent(in)::rangle
            real,intent(in),optional::x,y,length,x_inc,y_inc
            integer,intent(in),optional::dashy
            integer,intent(in)::iterations,line_thickness
            integer::n
            real::incx,incy

            if(present(x).and.present(y))call plot(x,y,-3)
            if(present(x).and. .not.present(y))call plot(x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,y,-3)
            if(present(x_inc))then;incx = x_inc;else;incx = 0.;endif
            if(present(y_inc))then;incy = y_inc;else;incy = 0.;endif
            call newpen2(line_thickness)
            if(present(dashy))call newpen2(dashy)
            write(ounit,*) "% begin floating_lines"
            do n = 1, iterations
                write(ounit,'(f10.4,2x,a4)' ) rangle , ' ro ' 
                call plot(0.,0.,3);call plot(length,0.,2)
                write(ounit,'(f9.4,2x,a4)' ) -rangle , ' ro ' 
                call plot(incx,incy,-3)
            end do
            write(ounit,*) "% end floating_lines"
            call plot(-real(iterations)*incx,-real(iterations)*incy,-3)

            if(present(x).and.present(y))call plot(-x,-y,-3)
            if(present(x).and. .not.present(y))call plot(-x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,-y,-3)
            return
        end subroutine
        ! defaults for optional arguments are, rangle=0.,symbol_start=0,TorB='B',lt=0,gt=0,symbol_start=0,x=0.,y=0.
        subroutine colorscale(iterations,r,g,b,ini_num,fin_num,symbol_freq,symbol_size,float_quantity,length,width,lt,gt,rangle,TorB,symbol_start,x,y)
            implicit none
            intrinsic::sin,cos
            integer,intent(in)::iterations,symbol_freq,float_quantity
            real,intent(in)::ini_num,fin_num,symbol_size,length,width
            integer,intent(in),optional::lt,gt,symbol_start
            character(len=*),intent(in),optional::TorB
            real,intent(in),optional::rangle,x,y
            real,dimension(0:),intent(in)::r,g,b
            integer::n,intquan
            real::memori_diff,num_diff
            real,dimension(3)::lefty_x=0.,lefty_y=0.,righty_x=0.,righty_y=0.
            character(len=20)::min,max,format1,format2

            if(lbound(r,1).ne.0 .or. ubound(r,1).ne.(iterations+1))then
                print*,'Something wrong with the color array'
            end if
            do n = 1,10
                if(ini_num/(10.**real(n))>=1.)then;cycle
                else;intquan=n;exit;end if
            end do
            if(float_quantity>=0)then
                write(format1,'(A,I0,A,I0,A)') "(f",intquan+float_quantity+3,'.',float_quantity,")"
                write(min,format1)ini_num
            else;write(format1,'(A,I0,A)') "(i",intquan+2,")"
                write(min,format1)int(ini_num)
            end if
            ! write(min,format1)ini_num
            do n = 1,10
                if(fin_num/(10.**real(n))>=1.)then;cycle
                else;intquan=n;exit;endif
            end do
            if(float_quantity>=0)then
                write(format2,'(A,I0,A,I0,A)') "(f",intquan+float_quantity+3,'.',float_quantity,")"
                write(max,format2)fin_num
            else;write(format2,'(A,I0,A)') "(i",intquan+2,")"
                write(max,format2)int(fin_num)
            end if
            ! write(max,format2)fin_num
            ! print*,format1,format2,max,min
            memori_diff = length/real(iterations+2); num_diff = (fin_num-ini_num)/real(iterations)

            lefty_x(1) = 0.;lefty_x(2) = -memori_diff; lefty_x(3) = 0.
            lefty_y(1) = 0.;lefty_y(2) = width/2. ;lefty_y(3) = width
            righty_x(1) = length-memori_diff*2. ;righty_x(2) = length-memori_diff;righty_x(3) = length-memori_diff*2.
            righty_y(1) = 0. ;righty_y(2) = width/2. ;righty_y(3) = width
            if(present(x).and.present(y))call plot(x,y,-3)
            if(present(x).and. .not.present(y))call plot(x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,y,-3)
            write(ounit,*) "% begin colorscale"

                if(present(rangle)) then
                    write(ounit,'(f10.4,2x,a4)' ) rangle , ' ro '
                end if
                call plot(-length/2.+memori_diff,-width/2.,-3)
                if(symbol_size<0.3)then;call newpen2(3)
                else if(symbol_size>=0.3.and.symbol_size<0.6)then;call newpen2(4)
                else;call newpen2(5)
                end if
                do n = 1, iterations
                    call betsqk(real(n-1)*memori_diff,0.,real(n)*memori_diff,width,r(n),g(n),b(n))
                end do
                call plot(0.,0.,3);call plot(length-2.*memori_diff,0.,2);call plot(0.,width,3);call plot(length-2.*(memori_diff),width,2);call plot(0.,0.,3)
                if(present(lt))then
                    call betmlk(lefty_x,lefty_y,3,3,r(0),g(0),b(0));call plot(0.,0.,3)
                    call plot(lefty_x(1),lefty_y(1),3)
                    call plot(lefty_x(2),lefty_y(2),2);call plot(lefty_x(3),lefty_y(3),2)
                end if
                if(present(gt))then
                    call plot(0.,0.,3)
                    call betmlk(righty_x,righty_y,3,3,r(iterations+1),g(iterations+1),b(iterations+1));call plot(0.,0.,3)
                    call plot(righty_x(1),righty_y(1),3);call plot(righty_x(2),righty_y(2),2);call plot(righty_x(3),righty_y(3),2)
                end if
                do n = 0, iterations
                    call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,width,2)
                    if(present(TorB).and.TorB=='T')then
                        call plot(real(n)*memori_diff,width,3);call plot(real(n)*memori_diff,width+symbol_size/8.,2)
                    else
                        call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,-symbol_size/8.,2)
                    end if
                    if(present(symbol_start))then
                        if(n>=symbol_start-1.and.mod(n-symbol_start-1,symbol_freq)==0 ) then
                            if(present(TorB).and.TorB=='T')then
                                call plot(real(n)*memori_diff,width,3);call plot(real(n)*memori_diff,width+symbol_size/4.,2)
                                if(present(rangle)) then
                                    if(rangle/=0.)then;call numberc(real(n)*memori_diff-symbol_size*0.3,width+1.5*symbol_size,symbol_size,ini_num+num_diff*real(n),-rangle,float_quantity)
                                    else;call numberc(real(n)*memori_diff,width+.5*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                                    end if
                                else;call numberc(real(n)*memori_diff,width+.5*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                                end if
                            else ! TorB=='B' or nada
                                call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,-symbol_size/4.,2)
                                if(present(rangle)) then
                                    if(rangle/=0.)then;call numberc(real(n)*memori_diff-symbol_size*0.3,-1.6*symbol_size,symbol_size,ini_num+num_diff*real(n),-rangle,float_quantity)
                                    else;call numberc(real(n)*memori_diff,-1.3*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                                    end if
                                elseif(.not.present(rangle))then;call numberc(real(n)*memori_diff,-1.3*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                                end if
                            end if
                        end if
                    else if(.not.present(symbol_start))then
                        if(mod(n,symbol_freq)==0 ) then
                            if(present(TorB).and.TorB=='T')then
                                call plot(real(n)*memori_diff,width,3);call plot(real(n)*memori_diff,width+symbol_size/4.,2)
                                if(present(rangle)) then
                                    if(rangle/=0.)then;call numberc(real(n)*memori_diff-symbol_size*0.3,width+1.5*symbol_size,symbol_size,ini_num+num_diff*real(n),-rangle,float_quantity)
                                    else;call numberc(real(n)*memori_diff,width+.5*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                                    end if
                                else;call numberc(real(n)*memori_diff,width+.5*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                                end if
                            else ! TorB=='B' or nada
                                call plot(real(n)*memori_diff,0.,3);call plot(real(n)*memori_diff,-symbol_size/4.,2)
                                if(present(rangle)) then
                                    if(rangle/=0.)then;call numberc(real(n)*memori_diff-symbol_size*0.3,-1.6*symbol_size,symbol_size,ini_num+num_diff*real(n),-rangle,float_quantity)
                                    else;call numberc(real(n)*memori_diff,-1.3*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                                    end if
                                else;call numberc(real(n)*memori_diff,-1.3*symbol_size,symbol_size,ini_num+num_diff*real(n),0.,float_quantity)
                                end if
                            end if
                        end if
                    end if
                end do
                ! print*,'bpt3'
            if(present(lt))then
                if(present(TorB).and.TorB=='T')then
                    if(present(rangle))then
                        if(rangle/=0.)then;call symbolc(-1.4*symbol_size,+1.8*symbol_size,symbol_size*0.7,'<'//trim(min),-rangle,len('<'//trim(min)))
                        else;call symbolc(-2.*symbol_size,+1.*symbol_size,symbol_size*0.7,'<'//trim(min),0.,len('<'//trim(min)))
                        end if
                    else;call symbolc(-2.*symbol_size,+1.*symbol_size,symbol_size*0.7,'<'//trim(min),0.,len('<'//trim(min)))
                    end if
                else ! TorB=='B' or nada
                    if(present(rangle))then
                        if(rangle/=0.)then;call symbolc(-1.2*symbol_size,-1.2*symbol_size,symbol_size*0.7,'<'//trim(min),-rangle,len('<'//trim(min)))
                        else;call symbolc(-2.*symbol_size,-0.5*symbol_size,symbol_size*0.7,'<'//trim(min),0.,len('<'//trim(min)))
                        end if
                    else;call symbolc(-2.*symbol_size,-0.5*symbol_size,symbol_size*0.7,'<'//trim(min),0.,len('<'//trim(min)))
                    end if
                end if
            end if
            if (present(gt))then
                if(present(TorB).and.TorB=='T')then
                    if(present(rangle))then
                        if(rangle/=0.)then;call symbolc(length-2.*memori_diff+1.*symbol_size,+1.8*symbol_size,symbol_size*0.7,trim(max)//'<',-rangle,len('>'//trim(max)))
                        else;call symbolc(length-2.*memori_diff+1.5*symbol_size,+1.*symbol_size,symbol_size*0.7,trim(max)//'<',0.,len('>'//trim(max)))
                        end if
                    else;call symbolc(length-2.*memori_diff+1.5*symbol_size,+1.*symbol_size,symbol_size*0.7,trim(max)//'<',0.,len('>'//trim(max)))
                    end if
                else ! TorB=='B' or nada
                    if(present(rangle))then
                        if(rangle/=0.)then;call symbolc(length-2.*memori_diff+1.*symbol_size,-1.2*symbol_size,symbol_size*0.7,trim(max)//' <',-rangle,len('>'//trim(max)))
                        else;call symbolc(length-2.*memori_diff+1.5*symbol_size,-0.5*symbol_size,symbol_size*0.7,trim(max)//'<',0.,len('>'//trim(max)))
                        end if
                    else;call symbolc(length-2.*memori_diff+1.5*symbol_size,-0.5*symbol_size,symbol_size*0.7,trim(max)//'<',0.,len('>'//trim(max)))
                    end if
                end if
            end if
            call plot(length/2.-memori_diff,width/2.,-3)
            if(present(rangle)) then
                write(ounit,'(f9.4,2x,a4)' ) -rangle , ' ro '
            end if
            if(present(x).and.present(y))call plot(-x,-y,-3)
            if(present(x).and. .not.present(y))call plot(-x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,-y,-3)

            
        end subroutine
        ! MEMORI
        subroutine memori(iterations,memori_size,bimemori_freq,length,rangle,x,y,gap,lthick)
            implicit none
            real,intent(in)::memori_size,length
            integer,intent(in)::iterations,bimemori_freq
            integer,intent(in),optional::gap,lthick
            real,intent(in),optional::x,y,rangle
            real::dx,gappy
            integer::n
        
            if(present(x).and.present(y))call plot(x,y,-3)
            if(present(x).and. .not.present(y))call plot(x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,y,-3)
            if(present(rangle))write(ounit,'(f10.4,2x,a4)' ) rangle , ' ro ' 
            call plot(-length/2.,0.,-3)
            if(present(lthick))then 
                call newpen2(lthick)
            else
                if(memori_size<=0.05)then;call newpen2(2)
                else if(memori_size>0.05.and.memori_size<=0.12)then;call newpen2(3)
                else if(memori_size>0.12.and.memori_size<=0.2)then;call newpen2(4)
                else;call newpen2(5);end if
            end if
            
            if(present(gap))then
                if(gap == 2) then
                    dx = length/real(iterations);gappy = dx/2.
                else if(gap == 1) then
                    dx = length/real(iterations+1);gappy = dx
                end if
            else;dx = length/real(iterations-1);gappy = 0.
            end if
            do n = 1, iterations
                ! if(gappy/=0.and.n==iterations)cycle
                if(bimemori_freq==0)then;call plot(gappy+real(n-1)*dx,0.,3);call plot(gappy+real(n-1)*dx,-memori_size,2)
                elseif(bimemori_freq/=0) then
                    if(mod(n,bimemori_freq)==0 .or.n==1)then
                    call plot(gappy+real(n-1)*dx,0.,3);call plot(gappy+real(n-1)*dx,-memori_size*1.5,2)
                    else;call plot(gappy+real(n-1)*dx,0.,3);call plot(gappy+real(n-1)*dx,-memori_size,2)
                    end if
                end if
            end do
            call plot(length/2.,0.,-3)
            if(present(rangle))write(ounit,'(f9.4,2x,a4)' ) -rangle , ' ro ' 

            if(present(x).and.present(y))call plot(-x,-y,-3)
            if(present(x).and. .not.present(y))call plot(-x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,-y,-3)

        end subroutine
        subroutine num_memori(ini_num,fin_num,iterations,symbol_freq,symbol_size,float_quantity,length,angle,x,y,gap,num_fac)
            implicit none
            real,intent(in)::ini_num,fin_num,length
            integer,intent(in),optional::iterations,symbol_freq,angle,float_quantity,gap
            ! integer,intent(in),optional::lt,gt
            real,intent(in),optional::symbol_size,x,y,num_fac
            real::memori_diff,num_diff,symbol_size_local,gappy,num_fac_local
            integer::n,i,iterations_local,symbol_freq_local,float_quantity_local,angle_local,gap_local
        
            if(present(x).and.present(y))call plot(x,y,-3)
            if(present(x).and. .not.present(y))call plot(x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,y,-3)

            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                    ! Creating Local Parameters
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                if(present(iterations))then 
                    iterations_local = iterations
                else
                    do i = 1, 10
                        if(mod(abs(fin_num-ini_num),1./(10.**real(i-1)))<=precision)then 
                            iterations_local = abs(int((fin_num-ini_num)*(10.**real(i-1)))) + 1
                            exit
                        end if
                    end do
                end if
                ! print*,iterations_local
                if(present(gap))then 
                    gap_local = gap
                else;gap_local = 0
                end if
                if(iterations_local/=1)then 
                    num_diff = (fin_num-ini_num)/real(iterations_local-1)
                else;num_diff = (fin_num-ini_num)
                end if
                if(gap_local == 2) then
                    memori_diff = length/real(iterations_local);gappy = memori_diff/2.
                else if(gap_local == 1) then
                    memori_diff = length/real(iterations_local+1);gappy = memori_diff
                else 
                    memori_diff = length/real(iterations_local-1);gappy = 0.
                end if

                if(present(symbol_freq))then 
                    symbol_freq_local = symbol_freq
                else;symbol_freq_local = 1
                end if

                if(present(symbol_size))then 
                    symbol_size_local = symbol_size
                else;symbol_size_local = length/8.
                end if
                if(symbol_size_local<=0.2)then;call newpen2(2)
                else if(symbol_size_local>0.2.and.symbol_size_local<=0.5)then;call newpen2(3)
                else if(symbol_size_local>0.5.and.symbol_size_local<=0.8)then;call newpen2(4)
                else;call newpen2(5);end if

                if(present(float_quantity))then 
                    float_quantity_local = float_quantity
                else;float_quantity_local = 1
                end if

                if(present(angle))then 
                    angle_local = angle
                else;angle_local = 0
                end if

                if(present(num_fac))then 
                    num_fac_local = num_fac
                else;num_fac_local = 1.
                end if

            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                                ! Plotting
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                call plot(gappy,0.,-3)
                if(angle_local == 0) then
                    do n = 1, iterations_local
                        if(mod(n-1,symbol_freq_local)==0) then
                            call plot(real(n-1)*memori_diff,0.,3);call plot(real(n-1)*memori_diff,-0.3*symbol_size_local,2)
                            call numberc(real(n-1)*memori_diff,-1.2*symbol_size_local,symbol_size_local,(ini_num+num_diff*real(n-1))/num_fac_local,0.,float_quantity_local)
                        else; call plot(real(n-1)*memori_diff,0.,3);call plot(real(n-1)*memori_diff,-0.2*symbol_size_local,2)
                        end if
                    end do
                else if(angle_local == 90) then
                    do n = 1, iterations_local
                        if(mod(n-1,symbol_freq_local)==0) then
                            call plot(0.,real(n-1)*memori_diff,3);call plot(0.3*symbol_size_local,real(n-1)*memori_diff,2)
                            call number(0.5*symbol_size_local,real(n-1)*memori_diff-symbol_size_local*0.3,symbol_size_local,(ini_num+num_diff*real(n-1))/num_fac_local,0.,float_quantity_local)
                        else;call plot(0.,real(n-1)*memori_diff,3);call plot(0.2*symbol_size_local,real(n-1)*memori_diff,2)
                        end if
                    end do
                else if (angle_local == -90) then
                    do n = 1, iterations_local
                        if(mod(n-1,symbol_freq_local)==0) then
                            call plot(0.,real(n-1)*memori_diff,3);call plot(-0.3*symbol_size_local,real(n-1)*memori_diff,2)
                            call numberr(-0.6*symbol_size_local,real(n-1)*memori_diff-symbol_size_local*0.3,symbol_size_local,(ini_num+num_diff*real(n-1))/num_fac_local,0.,float_quantity_local)
                        else;call plot(0.,real(n-1)*memori_diff,3);call plot(-0.2*symbol_size_local,real(n-1)*memori_diff,2)
                        end if
                    end do
                else;end if 
                call plot(-gappy,0.,-3)  

            if(present(x).and.present(y))call plot(-x,-y,-3)
            if(present(x).and. .not.present(y))call plot(-x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,-y,-3)
        
        end subroutine
        subroutine st_memori(ini_st,fin_st,width,top_bottom,symbol_size,gap,x,y)
            implicit none
            integer,intent(in)::ini_st,fin_st,top_bottom
            real,intent(in)::width,symbol_size
            real,intent(in),optional::x,y
            integer,intent(in),optional::gap
            real::dx,gappy
            integer::n
            if(present(x).and.present(y))call plot(x,y,-3)
            if(present(x).and. .not.present(y))call plot(x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,y,-3)
            if(symbol_size<=0.2)then;call newpen2(2)
            else if(symbol_size>0.2.and.symbol_size<=0.4)then;call newpen2(3)
            else if(symbol_size>0.2.and.symbol_size<=0.4)then;call newpen2(4)
            else;call newpen2(5);end if
            if(present(gap))then
                if(gap == 2) then
                dx = width/real(fin_st-ini_st+1);gappy = dx/2.
                else if(gap == 1) then
                    dx = width/real(fin_st-ini_st+2);gappy = dx
                else if(gap == 0) then
                    dx = width/real(fin_st-ini_st);gappy = 0.
                end if
            else;dx = width/real(fin_st-ini_st);gappy = 0.
            end if
            if(top_bottom == 1) then
                do n = 1,fin_st-ini_st+1
                    call plot(gappy+real(n-1)*dx,0.,3);call plot(gappy+real(n-1)*dx,-symbol_size*0.3,2)
                    call numberc(gappy+real(n-1)*dx,-symbol_size*1.3,symbol_size,real(fin_st-n+1),0.,-1)
                end do
            else if(top_bottom == 0) then
                do n = 1,fin_st-ini_st+1
                    call plot(gappy+real(n-1)*dx,0.,3);call plot(gappy+real(n-1)*dx,symbol_size*0.3,2)
                    call numberc(gappy+real(n-1)*dx,symbol_size*0.8,symbol_size,real(fin_st-n+1),0.,-1)
                end do
            end if 
            if(present(x).and.present(y))call plot(-x,-y,-3)
            if(present(x).and. .not.present(y))call plot(-x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,-y,-3)
        end subroutine
        ! gap bw memori and box. gap=0:no gap, gap=1:dx, gap=2:dx/2. get dxval if needed
        subroutine mod12_memori(iterations,length,symbol_size,angle,gap,num_freq,num_st,x,y,dxval)
            implicit none
            real,intent(in)::length
            real,intent(in),optional::x,y,symbol_size
            integer,intent(in),optional::num_freq,num_st
            real,intent(out),optional::dxval
            integer,intent(in)::iterations
            integer,intent(in),optional::angle,gap
            real::dx,gappy,symbol_size_local
            integer::n,m,printm,angle_local,gap_local

            if(present(x).and.present(y))call plot(x,y,-3)
            if(present(x).and. .not.present(y))call plot(x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,y,-3)
            if(present(symbol_size))then 
                symbol_size_local = symbol_size
            else;symbol_size_local = length/15.
            end if
            if(symbol_size_local<=0.2)then;call newpen2(2)
            else if(symbol_size_local>0.2.and.symbol_size_local<=0.5)then;call newpen2(3)
            else if(symbol_size_local>0.5.and.symbol_size_local<=0.8)then;call newpen2(4)
            else;call newpen2(5);end if
            if(present(angle))then;angle_local = angle;else;angle_local = 0;end if
            if(present(gap))then;gap_local = gap;else;gap_local = 2;end if

            if(gap_local == 2) then
                    dx = length/real(iterations);gappy = dx/2.
                else if(gap_local == 1) then
                    dx = length/real(iterations+1);gappy = dx
                else if(gap_local == 0) then
                    dx = length/real(iterations-1);gappy = 0.
            end if
            if(present(dxval)) then;dxval = dx;else;end if
            if(angle_local==0) then
                call plot(0.,0.,3);call plot(length,0.,2)
                do n = 1,iterations
                    if (mod(n,12)/=0) then;m = mod(n,12)
                    else if(mod(n,12)==0) then;m = 12
                    else;end if
                    call plot(gappy+real(n-1)*dx,0.,3);call plot(gappy+real(n-1)*dx,-0.3*symbol_size_local,2)
                    ! if(inc_dec == 1) then;printm = 13-m;else;printm = m;end if
                    printm = m
                    if(present(num_freq))then
                        if(present(num_st))then
                            if(n>=num_st.and.mod(n,num_freq)==0.or.n==1.or.n==iterations)then
                                call numberc(gappy+real(n-1)*dx,-1.2*symbol_size_local,symbol_size_local,real(printm),0.,-1)
                            end if
                        else;if(mod(n,num_freq)==0)call numberc(gappy+real(n-1)*dx,-1.2*symbol_size_local,symbol_size_local,real(printm),0.,-1)
                        end if
                    else;call numberc(gappy+real(n-1)*dx,-1.2*symbol_size_local,symbol_size_local,real(printm),0.,-1)
                    end if
                end do
            else if(angle_local == -90) then
                call plot(0.,0.,3);call plot(0.,length,2)
                do n = 1,iterations
                    if (mod(n,12)/=0) then;m = mod(n,12)
                    else if(mod(n,12)==0) then;m = 12
                    else;end if
                    call plot(0.,gappy+real(n-1)*dx,3);call plot(-0.3*symbol_size_local,gappy+real(n-1)*dx,2)
                    ! if(inc_dec == 1) then;printm = 13-m;else;printm = m;end if
                    printm = m
                    if(present(num_freq))then
                        if(present(num_st))then
                            if(n>=num_st.and.mod(n,num_freq)==0)then
                                call numberc(-1.*symbol_size_local,gappy+real(n-1)*dx-symbol_size_local*0.3,symbol_size_local,real(printm),0.,-1)
                            end if
                        else;if(mod(n,num_freq)==0)call numberc(-1.*symbol_size_local,gappy+real(n-1)*dx-symbol_size_local*0.3,symbol_size_local,real(printm),0.,-1)
                        end if
                    else;call numberc(-1.*symbol_size_local,gappy+real(n-1)*dx-symbol_size_local*0.3,symbol_size_local,real(printm),0.,-1)
                    end if
                end do
            end if

            if(present(x).and.present(y))call plot(-x,-y,-3)
            if(present(x).and. .not.present(y))call plot(-x,0.,-3)
            if(present(y).and. .not.present(x))call plot(0.,-y,-3)        

        end subroutine
    ! END PLOTS
    ! BASIC STATISTICS
        ! calculates mean, s, and sem per LOOP of a 1D array make sure LOOP*ITERATIONS < ARRAYSIZE, size of arrays is ITERATIONS
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
        ! calulates mean, s, and sem of numbers intermittently in a 1D array. JUMP is the number of elements to skip. array(jump)
        subroutine avsemjump_1D(array_1D,dim1,jump,loops,mean_1D,s_1D,sem_1D,dataquan_1D)
            implicit none
            integer,intent(in)::dim1,jump,loops
            real,intent(in)::array_1D(:)
            real,dimension(jump),intent(out),optional::mean_1D,s_1D,sem_1D
            integer,dimension(jump),intent(out),optional::dataquan_1D
            integer::l,i,count=0
            real::smean, s,sem,sum0=0.,sum1=0.

            if(dim1 < jump*loops) then;print*,'Array size < jump*iterations';stop;end if
            if(size(array_1D)/=dim1) then;print*,'Array size /= dim1';stop;end if
            do i = 1, jump
                count = 0;sum0=0.;sum1 = 0.
                do l = i,i+jump*(loops-1),jump
                    if(array_1D(l)/=0.) then
                        count = count +1;sum0 = sum0 + array_1D(l)
                    end if
                end do
                if(count <=1) then
                    smean = 0.;s = 0.;sem = 0.
                else
                    smean = sum0 / real(count)
                    do l = i,jump*(loops-1),jump
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
        ! welch's t for diff in (mean1-mean2).a = 0.05 on both sides. 0==nodif,1==larger,-1==smaller,911==error
        subroutine welchttest(mean1,s1,dataquan1,mean2,s2,dataquan2,result)
            implicit none
            real,intent(in)::mean1,s1,mean2,s2
            integer,intent(in)::dataquan1,dataquan2
            integer,intent(out)::result
            real,dimension(0:30)::t_95=0.
            real::diff_mean,n1,n2,df,sem,bottomCI,topCI

            if(mean1 /=0. .and. mean2/=0. .and. dataquan1/=0 .and. dataquan2/=0) then
                diff_mean = mean1 - mean2
                n1 = real(dataquan1);n2 = real(dataquan2)
                sem = sqrt((s1**2./n1)+(s2**2./n2)) 
                df = (((s1**2.)/n1)+((s2**2.)/n2))**2./(((s1**2./n1)**2./(n1-1))+((s2**2./n2)**2./(n2-1)))
                call t95_value(t_95)
                bottomCI = diff_mean - t_95(int(df))*sem ; topCI = diff_mean + t_95(int(df))*sem
                ! print*,diff_mean,bottomCI,topCI,int(df)
                if(bottomCI>0.) then
                    result = 1 !larger
                else if(topCI<0.) then
                    result = -1 !smaller
                else;result = 0 !no difference
                end if
            else;result = 911 !error
            end if

        end subroutine
        ! arrays have to be the same size
        subroutine correcoeff(array_1D,array_1D2,dim,r)
            implicit none
            integer,intent(in)::dim
            real,intent(in)::array_1D(:),array_1D2(:)
            real,intent(out)::r
            real::mean1,mean2,s1,s2,sum0=0.,sum1=0.,sum2=0.,covariance
            integer::n,count=0

            if(size(array_1D)/=dim)then; print*,'dim=',dim;print*,'array1 size=',size(array_1D);stop;end if
            if(size(array_1D2)/=dim)then; print*,'dim=',dim;print*,'array2 size=',size(array_1D2);stop;end if
            do n = 1, dim
                if(array_1D(n)/=0. .and. array_1D2(n)/=0.) then
                    count = count + 1
                    sum1 = sum1 + array_1D(n)
                    sum2 = sum2 + array_1D2(n)
                end if
            end do

            if(count <=1) then
                mean1 = 0.;mean2 = 0.;r = 0.
            else
                mean1 = sum1 / real(count);mean2 = sum2 / real(count)
                sum0 = 0.;sum1 = 0.;sum2 = 0.
                do n = 1, dim
                    if(array_1D(n)/=0. .and. array_1D2(n)/=0.) then
                        sum0 = sum0 + (array_1D(n) - mean1)*(array_1D2(n) - mean2) ! product of diffs from each means 
                        sum1 = sum1 + (array_1D(n) - mean1)**2. ! sum of squares of diffs from mean1
                        sum2 = sum2 + (array_1D2(n) - mean2)**2. ! sum of squares of diffs from mean2
                    end if
                end do
                s1 = sqrt(sum1 / real(count-1))
                s2 = sqrt(sum2 / real(count-1))
                covariance = sum0 / real(count-1)
                if(s1 == 0. .or. s2 == 0.) then
                    r = 0.
                else
                r = covariance / (s1*s2)
                end if
            end if


        end subroutine
        ! n is the quantity of data point pairs. not degrees of freedom
        ! critical_values is an array of critical values for 95 percent confidence on both sides,column 1 gives positive critical value, column 2 gives negative critical value
        ! subroutine rcritical95(n,critical_values)
        !     use functions
        !     implicit none 
        !     real,dimension(2,1)::critical_values
        !     integer,intent(in)::n
        !     integer::df
        !     real::r

        !     df = n-2
        !     r = sqrt(f_t95(df)**2./(df+f_t95(df)**2.))
        !     critical_values(1,1) = r
        !     critical_values(2,1) = -r
        ! end subroutine
    
        ! avsdsemdataquan better series
        ! gives an array of mean arrays    DO NOT USE INSIDE A LOOP ALLOCATION IS TRICKY maybe not
        subroutine avsemdata_1D(array_1D,mean,s,sem,dataquan,rmask)
            implicit none
            real,intent(in)::array_1D(:)
            real,intent(out),optional::mean,s,sem
            integer,intent(out),optional::dataquan
            real,intent(in),optional::rmask
            integer::n,count
            real::rmaskl,sum1,sum0,meanl,sl,seml
            if(present(rmask))then;rmaskl = rmask;else;rmaskl = 0.;endif

            count = 0;sum0=0.;sum1 = 0.
            do n = 1, size(array_1D)
                if (array_1D(n) /= 0.0.and. array_1D(n)/=rmaskl)then;count = count + 1;sum0 = sum0 + array_1D(n);end if
            end do
            if (count <=1 ) then
                if(present(mean))mean = 0.
                if(present(s))s = 0.
                if(present(sem))sem = 0.
                if(present(dataquan))dataquan = 0
            else
                meanl = sum0 / real(count)
                if(present(mean))mean = meanl
                if(present(dataquan))dataquan = count
                do n = 1, size(array_1D)
                    if(array_1D(n)/=0..and.array_1D(n)/=rmaskl) then
                        sum1 = sum1 + (array_1D(n) - meanl)**2.
                    end if
                end do
                sl = sqrt(sum1 / real(count-1))
                seml = sl / sqrt(real(count))
                if(present(s))s = sl
                if(present(sem))sem = seml
            end if

        end subroutine

        subroutine avsemdata_2D(array_2D,dec_dim,mean_1D,s_1D,sem_1D,dataquan_1D,rmask)
            implicit none
            ! integer,intent(in)::dim1,dim2
            character(len=*),intent(in)::dec_dim
            real,intent(in)::array_2D(:,:)
            real,dimension(:),allocatable,intent(out),optional::mean_1D,s_1D,sem_1D
            integer,dimension(:),allocatable,intent(out),optional::dataquan_1D
            real,intent(in),optional::rmask
            integer::n,i,count=0,dim1,dim2
            real::smean, s, sem, sum0=0.,sum1=0.,rmaskl
            if(present(rmask))then;rmaskl = rmask;else;rmaskl = 0.;endif

            ! if(size(array_2D,1)<dim1 .or. size(array_2D,2)<dim2) then 
            !     print*,'Array size < dim1 or dim2';stop
            ! end if
            dim1 = size(array_2D,1);dim2 = size(array_2D,2)
            ! print*,'dim1 = ',dim1,'dim2 = ',dim2,'dec_dim = ',dec_dim
            if (dec_dim == 'dim1') then
                if (present(mean_1D)) allocate(mean_1D(dim2))
                if (present(s_1D)) allocate(s_1D(dim2))
                if (present(sem_1D)) allocate(sem_1D(dim2))
                if (present(dataquan_1D)) allocate(dataquan_1D(dim2))
                do n = 1, dim2
                    count = 0;sum0=0.;sum1 = 0.
                    do i = 1, dim1
                        if (array_2D(i, n) /= 0.0.and. array_2D(i,n)/=rmaskl)then;count = count + 1;sum0 = sum0 + array_2D(i,n);end if
                    end do
                    if (count <=1 ) then
                        smean = 0.;s = 0.;sem = 0.
                    else
                        smean = sum0 / real(count)
                        do i = 1, dim1
                            if(array_2D(i,n)/=0..and.array_2D(i,n)/=rmaskl) then
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
            else if (dec_dim == 'dim2') then
                if (present(mean_1D)) allocate(mean_1D(dim1))
                if (present(s_1D)) allocate(s_1D(dim1))
                if (present(sem_1D)) allocate(sem_1D(dim1))
                if (present(dataquan_1D)) allocate(dataquan_1D(dim1))
                do n = 1, dim1
                    count = 0;sum0=0.;sum1 = 0.
                    do i = 1, dim2
                        if (array_2D(n, i) /= 0.0.and.array_2D(n,i)/=rmaskl)then;count = count + 1;sum0 = sum0 + array_2D(n,i);end if
                        ! print*,n,i,count,sum0
                    end do
                    if (count <=1 ) then
                        smean = 0.;s = 0.;sem = 0.
                    else
                        smean = sum0 / real(count)
                        do i = 1, dim2
                            if(array_2D(n,i)/=0..and.array_2D(n,i)/=rmaskl) then
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
                print *, 'Choose a dimension to take mean of (descard)'
                stop
            end if


            ! if(present(mean_1D)) deallocate(mean_1D)
            ! if(present(s_1D)) deallocate(s_1D)
            ! if(present(sem_1D)) deallocate(sem_1D)
            ! if(present(dataquan_1D)) deallocate(dataquan_1D)

        end subroutine

        subroutine avsemdata_3D(array_3D,dec_dim,mean_2D,s_2D,sem_2D,dataquan_2D,rmask)
            implicit none
            ! integer,intent(in)::dim1,dim2,dim3
            real,intent(in),optional::rmask
            character(len=*),intent(in)::dec_dim
            real,intent(in)::array_3D(:,:,:)
            real,dimension(:,:),allocatable,intent(out),optional::mean_2D,s_2D,sem_2D
            integer,dimension(:,:),allocatable,intent(out),optional::dataquan_2D
            integer::l1,l2,l3,count=0,loop1,loop2,loop3,dim1,dim2,dim3
            real::smean, s, sem, sum0=0.,sum1=0.,rmaskl

            ! if(size(array_3D,1)<dim1 .or. size(array_3D,2)<dim2 .or. size(array_3D,3)<dim3) then 
            !     print*,'Array size < dim1 or dim2 or dim3';stop
            ! end if
            dim1 = size(array_3D,1);dim2 = size(array_3D,2);dim3 = size(array_3D,3)
            if(present(rmask))then;rmaskl = rmask;else;rmaskl = 0.;endif
            ! loop determination
                if (dec_dim == 'dim1') then
                    loop1 = dim2
                    loop2 = dim3
                    loop3 = dim1
                    if(present(mean_2D)) allocate(mean_2D(dim2,dim3))
                    if(present(s_2D)) allocate(s_2D(dim2,dim3))
                    if(present(sem_2D)) allocate(sem_2D(dim2,dim3))
                    if(present(dataquan_2D)) allocate(dataquan_2D(dim2,dim3))
                elseif (dec_dim == 'dim2') then
                    loop1 = dim1
                    loop2 = dim3
                    loop3 = dim2
                    if(present(mean_2D)) allocate(mean_2D(dim1,dim3))
                    if(present(s_2D)) allocate(s_2D(dim1,dim3))
                    if(present(sem_2D)) allocate(sem_2D(dim1,dim3))
                    if(present(dataquan_2D)) allocate(dataquan_2D(dim1,dim3))
                elseif (dec_dim == 'dim3') then
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
                        if(dec_dim=='dim1') then
                            if(array_3D(l3,l1,l2)/=0..or.array_3D(l3,l1,l2)/=rmaskl) then
                                count = count + 1
                                sum0 = sum0 + array_3D(l3,l1,l2)
                            end if
                        else if(dec_dim=='dim2') then
                            if(array_3D(l1,l3,l2)/=0..and.array_3D(l1,l3,l2)/=rmaskl) then
                                count = count + 1
                                sum0 = sum0 + array_3D(l1,l3,l2)
                            end if
                        else if(dec_dim=='dim3') then
                            if(array_3D(l1,l2,l3)/=0..and.array_3D(l1,l2,l3)/=rmaskl) then
                                count = count + 1
                                sum0 = sum0 + array_3D(l1,l2,l3)
                            end if
                        end if
                    end do
                        if(count<=1) then;smean= 0.;s=0.;sem=0.
                        else;smean = sum0/real(count)
                            do l3 = 1, loop3
                                if(dec_dim=='dim1') then
                                    if(array_3D(l3,l1,l2)/=0..and.array_3D(l3,l1,l2)/=rmaskl) then
                                        sum1 = sum1 + (array_3D(l3,l1,l2) - smean)**2.
                                    end if
                                else if(dec_dim=='dim2') then
                                    if(array_3D(l1,l3,l2)/=0..and.array_3D(l1,l3,l2)/=rmaskl) then
                                        sum1 = sum1 + (array_3D(l1,l3,l2) - smean)**2.
                                    end if
                                else if(dec_dim=='dim3') then
                                    if(array_3D(l1,l2,l3)/=0..and.array_3D(l1,l2,l3)/=rmaskl) then
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

        subroutine avsemdata_4D(array_4D,dec_dim,mean_3D,s_3D,sem_3D,dataquan_3D,rmask)
            implicit none
            ! integer,intent(in)::dim1,dim2,dim3,dim4
            character(len=*),intent(in)::dec_dim
            integer,intent(in),optional::rmask
            real,intent(in)::array_4D(:,:,:,:)
            real,dimension(:,:,:),allocatable,intent(out),optional::mean_3D,s_3D,sem_3D
            integer,dimension(:,:,:),allocatable,intent(out),optional::dataquan_3D
            integer::l1,l2,l3,l4,count=0,loop1,loop2,loop3,loop4,dim1,dim2,dim3,dim4
            real::smean, s, sem, sum0=0.,sum1=0.,rmaskl

            ! if(size(array_4D,1)<dim1 .or. size(array_4D,2)<dim2 .or. size(array_4D,3)<dim3 .or. size(array_4D,4)<dim4) then 
            !     print*,'Array size < dim1 or dim2 or dim3 or dim4';stop
            ! end if
            dim1 = size(array_4D,1);dim2 = size(array_4D,2);dim3 = size(array_4D,3);dim4 = size(array_4D,4)
            if(present(rmask))then;rmaskl = rmask;else;rmaskl = 0.;endif
            ! loop determination
                if (dec_dim == 'dim1') then
                    loop1 = dim2
                    loop2 = dim3
                    loop3 = dim4
                    loop4 = dim1
                    if(present(mean_3D)) allocate(mean_3D(dim2,dim3,dim4))
                    if(present(s_3D)) allocate(s_3D(dim2,dim3,dim4))
                    if(present(sem_3D)) allocate(sem_3D(dim2,dim3,dim4))
                    if(present(dataquan_3D)) allocate(dataquan_3D(dim2,dim3,dim4))
                elseif (dec_dim == 'dim2') then
                    loop1 = dim1
                    loop2 = dim3
                    loop3 = dim4
                    loop4 = dim2
                    if(present(mean_3D)) allocate(mean_3D(dim1,dim3,dim4))
                    if(present(s_3D)) allocate(s_3D(dim1,dim3,dim4))
                    if(present(sem_3D)) allocate(sem_3D(dim1,dim3,dim4))
                    if(present(dataquan_3D)) allocate(dataquan_3D(dim1,dim3,dim4))
                elseif (dec_dim == 'dim3') then
                    loop1 = dim1
                    loop2 = dim2
                    loop3 = dim4
                    loop4 = dim3
                    if(present(mean_3D)) allocate(mean_3D(dim1,dim2,dim4))
                    if(present(s_3D)) allocate(s_3D(dim1,dim2,dim4))
                    if(present(sem_3D)) allocate(sem_3D(dim1,dim2,dim4))
                    if(present(dataquan_3D)) allocate(dataquan_3D(dim1,dim2,dim4))
                elseif (dec_dim == 'dim4') then
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
                            if(dec_dim=='dim1') then
                                if(array_4D(l4,l1,l2,l3)/=0..and.array_4D(l4,l1,l2,l3)/=rmaskl) then
                                    count = count + 1
                                    sum0 = sum0 + array_4D(l4,l1,l2,l3)
                                end if
                            else if(dec_dim=='dim2') then
                                if(array_4D(l1,l4,l2,l3)/=0..and.array_4D(l1,l4,l2,l3)/=rmaskl) then
                                    count = count + 1
                                    sum0 = sum0 + array_4D(l1,l4,l2,l3)
                                end if
                            else if(dec_dim=='dim3') then
                                if(array_4D(l1,l2,l4,l3)/=0..and.array_4D(l1,l2,l4,l3)/=rmaskl) then
                                    count = count + 1
                                    sum0 = sum0 + array_4D(l1,l2,l4,l3)
                                end if
                            else if(dec_dim=='dim4') then
                                if(array_4D(l1,l2,l3,l4)/=0..and.array_4D(l1,l2,l3,l4)/=rmaskl) then
                                    count = count + 1
                                    sum0 = sum0 + array_4D(l1,l2,l3,l4)
                                end if
                            end if
                        end do
                            if(count<=1) then;smean= 0.;s=0.;sem=0.
                            else;smean = sum0/real(count)
                                do l4 = 1, loop4
                                    if(dec_dim=='dim1') then
                                        if(array_4D(l4,l1,l2,l3)/=0..and.array_4D(l4,l1,l2,l3)/=rmaskl) then
                                            sum1 = sum1 + (array_4D(l4,l1,l2,l3) - smean)**2.
                                        end if
                                    else if(dec_dim=='dim2') then
                                        if(array_4D(l1,l4,l2,l3)/=0..and.array_4D(l1,l4,l2,l3)/=rmaskl) then
                                            sum1 = sum1 + (array_4D(l1,l4,l2,l3) - smean)**2.
                                        end if
                                    else if(dec_dim=='dim3') then
                                        if(array_4D(l1,l2,l4,l3)/=0..and.array_4D(l1,l2,l4,l3)/=rmaskl) then
                                            sum1 = sum1 + (array_4D(l1,l2,l4,l3) - smean)**2.
                                        end if
                                    else if(dec_dim=='dim4') then
                                        if(array_4D(l1,l2,l3,l4)/=0..and.array_4D(l1,l2,l3,l4)/=rmaskl) then
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

        subroutine avsemdata_5D(array_5D,dec_dim,mean_4D,s_4D,sem_4D,dataquan_4D,rmask)
            implicit none
            ! integer,intent(in)::dim1,dim2,dim3,dim4,dim5
            integer,intent(in),optional::rmask
            character(len=*),intent(in)::dec_dim
            real,intent(in)::array_5D(:,:,:,:,:)
            real,dimension(:,:,:,:),allocatable,intent(out),optional::mean_4D,s_4D,sem_4D
            integer,dimension(:,:,:,:),allocatable,intent(out),optional::dataquan_4D
            integer::l1,l2,l3,l4,l5,count=0,loop1,loop2,loop3,loop4,loop5,dim1,dim2,dim3,dim4,dim5
            real::smean, s, sem, sum0=0.,sum1=0.,rmaskl

            ! if(size(array_5D,1)<dim1 .or. size(array_5D,2)<dim2 .or. size(array_5D,3)<dim3 .or. size(array_5D,4)<dim4 .or. size(array_5D,5)<dim5) then 
            !     print*,'Array size < dim1 or dim2 or dim3 or dim4 or dim5';stop
            ! end if
            dim1 = size(array_5D,1);dim2 = size(array_5D,2);dim3 = size(array_5D,3);dim4 = size(array_5D,4);dim5 = size(array_5D,5)
            if(present(rmask))then;rmaskl = rmask;else;rmaskl = 0.;endif
            ! loop determination
                if (dec_dim == 'dim1') then
                    loop1 = dim2
                    loop2 = dim3
                    loop3 = dim4
                    loop4 = dim5
                    loop5 = dim1
                    if(present(mean_4D)) allocate(mean_4D(dim2,dim3,dim4,dim5))
                    if(present(s_4D)) allocate(s_4D(dim2,dim3,dim4,dim5))
                    if(present(sem_4D)) allocate(sem_4D(dim2,dim3,dim4,dim5))
                    if(present(dataquan_4D)) allocate(dataquan_4D(dim2,dim3,dim4,dim5))
                elseif (dec_dim == 'dim2') then
                    loop1 = dim1
                    loop2 = dim3
                    loop3 = dim4
                    loop4 = dim5
                    loop5 = dim2
                    if(present(mean_4D)) allocate(mean_4D(dim1,dim3,dim4,dim5))
                    if(present(s_4D)) allocate(s_4D(dim1,dim3,dim4,dim5))
                    if(present(sem_4D)) allocate(sem_4D(dim1,dim3,dim4,dim5))
                    if(present(dataquan_4D)) allocate(dataquan_4D(dim1,dim3,dim4,dim5))
                elseif (dec_dim == 'dim3') then
                    loop1 = dim1
                    loop2 = dim2
                    loop3 = dim4
                    loop4 = dim5
                    loop5 = dim3
                    if(present(mean_4D)) allocate(mean_4D(dim1,dim2,dim4,dim5))
                    if(present(s_4D)) allocate(s_4D(dim1,dim2,dim4,dim5))
                    if(present(sem_4D)) allocate(sem_4D(dim1,dim2,dim4,dim5))
                    if(present(dataquan_4D)) allocate(dataquan_4D(dim1,dim2,dim4,dim5))
                elseif (dec_dim == 'dim4') then
                    loop1 = dim1
                    loop2 = dim2
                    loop3 = dim3
                    loop4 = dim5
                    loop5 = dim4
                    if(present(mean_4D)) allocate(mean_4D(dim1,dim2,dim3,dim5))
                    if(present(s_4D)) allocate(s_4D(dim1,dim2,dim3,dim5))
                    if(present(sem_4D)) allocate(sem_4D(dim1,dim2,dim3,dim5))
                    if(present(dataquan_4D)) allocate(dataquan_4D(dim1,dim2,dim3,dim5))
                elseif (dec_dim == 'dim5') then
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
                                if(dec_dim=='dim1') then
                                    if(array_5D(l5,l1,l2,l3,l4)/=0..and.array_5D(l5,l1,l2,l3,l4)/=rmaskl) then
                                        count = count + 1
                                        sum0 = sum0 + array_5D(l5,l1,l2,l3,l4)
                                    end if
                                else if(dec_dim=='dim2') then
                                    if(array_5D(l1,l5,l2,l3,l4)/=0..and.array_5D(l1,l5,l2,l3,l4)/=rmaskl) then
                                        count = count + 1
                                        sum0 = sum0 + array_5D(l1,l5,l2,l3,l4)
                                    end if
                                else if(dec_dim=='dim3') then
                                    if(array_5D(l1,l2,l5,l3,l4)/=0..and.array_5D(l1,l2,l5,l3,l4)/=rmaskl) then
                                        count = count + 1
                                        sum0 = sum0 + array_5D(l1,l2,l5,l3,l4)
                                    end if
                                else if(dec_dim=='dim4') then
                                    if(array_5D(l1,l2,l3,l5,l4)/=0..and.array_5D(l1,l2,l3,l5,l4)/=rmaskl) then
                                        count = count + 1
                                        sum0 = sum0 + array_5D(l1,l2,l3,l5,l4)
                                    end if
                                else if(dec_dim=='dim5') then
                                    if(array_5D(l1,l2,l3,l4,l5)/=0..and.array_5D(l1,l2,l3,l4,l5)/=rmaskl) then
                                        count = count + 1
                                        sum0 = sum0 + array_5D(l1,l2,l3,l4,l5)
                                    end if
                                end if
                            end do
                                if(count<=1) then;smean= 0.;s=0.;sem=0.
                                else;smean = sum0/real(count)
                                    do l5 = 1, loop5
                                        if(dec_dim=='dim1') then
                                            if(array_5D(l5,l1,l2,l3,l4)/=0..and.array_5D(l5,l1,l2,l3,l4)/=rmaskl) then
                                                sum1 = sum1 + (array_5D(l5,l1,l2,l3,l4) - smean)**2.
                                            end if
                                        else if(dec_dim=='dim2') then
                                            if(array_5D(l1,l5,l2,l3,l4)/=0..and.array_5D(l1,l5,l2,l3,l4)/=rmaskl) then
                                                sum1 = sum1 + (array_5D(l1,l5,l2,l3,l4) - smean)**2.
                                            end if
                                        else if(dec_dim=='dim3') then
                                            if(array_5D(l1,l2,l5,l3,l4)/=0..and.array_5D(l1,l2,l5,l3,l4)/=rmaskl) then
                                                sum1 = sum1 + (array_5D(l1,l2,l5,l3,l4) - smean)**2.
                                            end if
                                        else if(dec_dim=='dim4') then
                                            if(array_5D(l1,l2,l3,l5,l4)/=0..and.array_5D(l1,l2,l3,l5,l4)/=rmaskl) then
                                                sum1 = sum1 + (array_5D(l1,l2,l3,l5,l4) - smean)**2.
                                            end if
                                        else if(dec_dim=='dim5') then
                                            if(array_5D(l1,l2,l3,l4,l5)/=0..and.array_5D(l1,l2,l3,l4,l5)/=rmaskl) then
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

        subroutine avsemdata_6D(array_6D,dec_dim,mean_5D,s_5D,sem_5D,dataquan_5D,rmask)
            implicit none
            ! integer,intent(in)::dim1,dim2,dim3,dim4,dim5,dim6
            integer,intent(in),optional::rmask
            character(len=*),intent(in)::dec_dim
            real,intent(in)::array_6D(:,:,:,:,:,:)
            real,dimension(:,:,:,:,:),allocatable,intent(out),optional::mean_5D,s_5D,sem_5D
            integer,dimension(:,:,:,:,:),allocatable,intent(out),optional::dataquan_5D
            integer::l1,l2,l3,l4,l5,l6,count=0,loop1,loop2,loop3,loop4,loop5,loop6,dim1,dim2,dim3,dim4,dim5,dim6
            real::smean, s, sem, sum0=0.,sum1=0.,rmaskl

            ! if(size(array_6D,1)<dim1 .or. size(array_6D,2)<dim2 .or. size(array_6D,3)<dim3 .or. size(array_6D,4)<dim4 .or. size(array_6D,5)<dim5 .or. size(array_6D,6)<dim6) then
            !     print*,'Array size < dim1 or dim2 or dim3 or dim4 or dim5 or dim6';stop
            ! end if
            dim1 = size(array_6D,1);dim2 = size(array_6D,2);dim3 = size(array_6D,3);dim4 = size(array_6D,4);dim5 = size(array_6D,5);dim6 = size(array_6D,6)
            if(present(rmask))then;rmaskl = rmask;else;rmaskl = 0.;endif
            ! loop determination
                if (dec_dim == 'dim1') then
                    loop1 = dim2
                    loop2 = dim3
                    loop3 = dim4
                    loop4 = dim5
                    loop5 = dim6
                    loop6 = dim1
                    if(present(mean_5D)) allocate(mean_5D(dim2,dim3,dim4,dim5,dim6))
                    if(present(s_5D)) allocate(s_5D(dim2,dim3,dim4,dim5,dim6))
                    if(present(sem_5D)) allocate(sem_5D(dim2,dim3,dim4,dim5,dim6))
                    if(present(dataquan_5D)) allocate(dataquan_5D(dim2,dim3,dim4,dim5,dim6))
                elseif (dec_dim == 'dim2') then
                    loop1 = dim1
                    loop2 = dim3
                    loop3 = dim4
                    loop4 = dim5
                    loop5 = dim6
                    loop6 = dim2
                    if(present(mean_5D)) allocate(mean_5D(dim1,dim3,dim4,dim5,dim6))
                    if(present(s_5D)) allocate(s_5D(dim1,dim3,dim4,dim5,dim6))
                    if(present(sem_5D)) allocate(sem_5D(dim1,dim3,dim4,dim5,dim6))
                    if(present(dataquan_5D)) allocate(dataquan_5D(dim1,dim3,dim4,dim5,dim6))
                elseif (dec_dim == 'dim3') then
                    loop1 = dim1
                    loop2 = dim2
                    loop3 = dim4
                    loop4 = dim5
                    loop5 = dim6
                    loop6 = dim3
                    if(present(mean_5D)) allocate(mean_5D(dim1,dim2,dim4,dim5,dim6))
                    if(present(s_5D)) allocate(s_5D(dim1,dim2,dim4,dim5,dim6))
                    if(present(sem_5D)) allocate(sem_5D(dim1,dim2,dim4,dim5,dim6))
                    if(present(dataquan_5D)) allocate(dataquan_5D(dim1,dim2,dim4,dim5,dim6))
                elseif (dec_dim == 'dim4') then
                    loop1 = dim1
                    loop2 = dim2
                    loop3 = dim3
                    loop4 = dim5
                    loop5 = dim6
                    loop6 = dim4
                    if(present(mean_5D)) allocate(mean_5D(dim1,dim2,dim3,dim5,dim6))
                    if(present(s_5D)) allocate(s_5D(dim1,dim2,dim3,dim5,dim6))
                    if(present(sem_5D)) allocate(sem_5D(dim1,dim2,dim3,dim5,dim6))
                    if(present(dataquan_5D)) allocate(dataquan_5D(dim1,dim2,dim3,dim5,dim6))
                elseif (dec_dim == 'dim5') then
                    loop1 = dim1
                    loop2 = dim2
                    loop3 = dim3
                    loop4 = dim4
                    loop5 = dim6
                    loop6 = dim5
                    if(present(mean_5D)) allocate(mean_5D(dim1,dim2,dim3,dim4,dim6))
                    if(present(s_5D)) allocate(s_5D(dim1,dim2,dim3,dim4,dim6))
                    if(present(sem_5D)) allocate(sem_5D(dim1,dim2,dim3,dim4,dim6))
                    if(present(dataquan_5D)) allocate(dataquan_5D(dim1,dim2,dim3,dim4,dim6))
                elseif (dec_dim == 'dim6') then
                    loop1 = dim1
                    loop2 = dim2
                    loop3 = dim3
                    loop4 = dim4
                    loop5 = dim5
                    loop6 = dim6
                    if(present(mean_5D)) allocate(mean_5D(dim1,dim2,dim3,dim4,dim5))
                    if(present(s_5D)) allocate(s_5D(dim1,dim2,dim3,dim4,dim5))
                    if(present(sem_5D)) allocate(sem_5D(dim1,dim2,dim3,dim4,dim5))
                    if(present(dataquan_5D)) allocate(dataquan_5D(dim1,dim2,dim3,dim4,dim5))
                else;print*, 'Invalid dec_dim value';stop
                end if

            ! loop determination end

                do l1 = 1, loop1
                    do l2 = 1, loop2
                        do l3 = 1, loop3
                            do l4 = 1, loop4
                                count = 0; sum0 = 0.;sum1 = 0.
                                do l5 = 1, loop5
                                    do l6 = 1, loop6 !dec_dim loop
                                        if(dec_dim=='dim1') then
                                            if(array_6D(l6,l1,l2,l3,l4,l5)/=0..and.array_6D(l6,l1,l2,l3,l4,l5)/=rmaskl) then
                                                count = count + 1
                                                sum0 = sum0 + array_6D(l6,l1,l2,l3,l4,l5)
                                            end if
                                        else if(dec_dim=='dim2') then
                                            if(array_6D(l1,l6,l2,l3,l4,l5)/=0..and.array_6D(l1,l6,l2,l3,l4,l5)/=rmaskl) then
                                                count = count + 1
                                                sum0 = sum0 + array_6D(l1,l6,l2,l3,l4,l5)
                                            end if
                                        else if(dec_dim=='dim3') then
                                            if(array_6D(l1,l2,l6,l3,l4,l5)/=0..and.array_6D(l1,l2,l6,l3,l4,l5)/=rmaskl) then
                                                count = count + 1
                                                sum0 = sum0 + array_6D(l1,l2,l6,l3,l4,l5)
                                            end if
                                        else if(dec_dim=='dim4') then
                                            if(array_6D(l1,l2,l3,l6,l4,l5)/=0..and.array_6D(l1,l2,l3,l6,l4,l5)/=rmaskl) then
                                                count = count + 1
                                                sum0 = sum0 + array_6D(l1,l2,l3,l6,l4,l5)
                                            end if
                                        else if(dec_dim=='dim5') then
                                            if(array_6D(l1,l2,l3,l4,l6,l5)/=0..and.array_6D(l1,l2,l3,l4,l6,l5)/=rmaskl) then
                                                count = count + 1
                                                sum0 = sum0 + array_6D(l1,l2,l3,l4,l6,l5)
                                            end if
                                        else if(dec_dim=='dim6') then
                                            if(array_6D(l1,l2,l3,l4,l5,l6)/=0..and.array_6D(l1,l2,l3,l4,l5,l6)/=rmaskl) then
                                                count = count + 1
                                                sum0 = sum0 + array_6D(l1,l2,l3,l4,l5,l6)
                                            end if
                                        end if
                                    end do

                                    if(count<=1) then;smean= 0.;s=0.;sem=0.
                                    else;smean = sum0/real(count)
                                        do l6 = 1, loop6
                                            if(dec_dim=='dim1') then
                                                if(array_6D(l6,l1,l2,l3,l4,l5)/=0..and.array_6D(l6,l1,l2,l3,l4,l5)/=rmaskl) then
                                                    sum1 = sum1 + (array_6D(l6,l1,l2,l3,l4,l5) - smean)**2.
                                                end if
                                            else if(dec_dim=='dim2') then
                                                if(array_6D(l1,l6,l2,l3,l4,l5)/=0..and.array_6D(l1,l6,l2,l3,l4,l5)/=rmaskl) then
                                                    sum1 = sum1 + (array_6D(l1,l6,l2,l3,l4,l5) - smean)**2.
                                                end if
                                            else if(dec_dim=='dim3') then
                                                if(array_6D(l1,l2,l6,l3,l4,l5)/=0..and.array_6D(l1,l2,l6,l3,l4,l5)/=rmaskl) then
                                                    sum1 = sum1 + (array_6D(l1,l2,l6,l3,l4,l5) - smean)**2.
                                                end if
                                            else if(dec_dim=='dim4') then
                                                if(array_6D(l1,l2,l3,l6,l4,l5)/=0..and.array_6D(l1,l2,l3,l6,l4,l5)/=rmaskl) then
                                                    sum1 = sum1 + (array_6D(l1,l2,l3,l6,l4,l5) - smean)**2.
                                                end if
                                            else if(dec_dim=='dim5') then
                                                if(array_6D(l1,l2,l3,l4,l6,l5)/=0..and.array_6D(l1,l2,l3,l4,l6,l5)/=rmaskl) then
                                                    sum1 = sum1 + (array_6D(l1,l2,l3,l4,l6,l5) - smean)**2.
                                                end if
                                            else if(dec_dim=='dim6') then
                                                if(array_6D(l1,l2,l3,l4,l5,l6)/=0..and.array_6D(l1,l2,l3,l4,l5,l6)/=rmaskl) then
                                                    sum1 = sum1 + (array_6D(l1,l2,l3,l4,l5,l6) - smean)**2.
                                                end if
                                            end if
                                        end do
                                        s = sqrt(sum1/real(count-1))
                                        sem = s / sqrt(real(count))
                                    end if
                                    if(present(mean_5D)) mean_5D(l1,l2,l3,l4,l5) = smean
                                    if(present(s_5D)) s_5D(l1,l2,l3,l4,l5) = s
                                    if(present(sem_5D)) sem_5D(l1,l2,l3,l4,l5) = sem
                                    if(present(dataquan_5D)) dataquan_5D(l1,l2,l3,l4,l5) = count
                            end do
                        end do
                    end do
                end do
            end do
        end subroutine  
    ! END BASIC STATISTICS 
    ! PS boys, NOTE: THE ARRAY LBOUND NEED NOT BE 1 as [assumed shape arrays] reshape the array lbound to 1 (tested)
    ! REMEMBER THAT THESE SUBROUTINES DRAW COLUMNS OF EACH ROW INDEX IN THE ARRAY (i.e. in array(x,y), x need be the row index (or x axis), y need be the column index (or y axis))

        ! if present, gap is dx/2. ;centralization of colors is done relative to integer centralize * dirty due to unnecessary use of bounds
        subroutine butler_psk(array_2D,width,height,maskval,ival,fval,inc,colorscheme,iterations,bpt1,bpt2,bpt3,conti,continc,thicc,r,g,b,gap,centralize)
            implicit none
            integer,intent(in)::iterations
            real,intent(in)::maskval,ival,fval,inc,width,height
            integer,intent(in),optional::bpt1,bpt2,bpt3,gap,thicc,centralize
            real,intent(in),optional::conti,continc
            real,intent(in)::array_2D(:,:)
            integer,dimension(size(array_2D,1),size(array_2D,2))::mask
            character(len=*),intent(in)::colorscheme
            real,dimension(:),allocatable::r1,g1,b1
            real,dimension(:,:),allocatable::another
            integer,dimension(:,:),allocatable::anothermask
            real,dimension(:),allocatable,intent(out),optional::r,g,b
            real::dx,dy
            integer::i,j,n,contquan,zerocolumns,nonzerocol,dim1,dim2
            write(ounit,*)'%begin butler_psk'
            ! if(size(array_2D,1)/=dim1)then;print*,'Array size /= dim1 (butler_psk)';stop;endif
            ! if(size(array_2D,2)/=dim2)then;print*,'Array size /= dim2 (butler_psk)';stop;endif
            dim1 = size(array_2D,1);dim2 = size(array_2D,2)
            ! print*,lbound(array_2D,1),ubound(array_2D,1),lbound(array_2D,2),ubound(array_2D,2)

            if((abs(ival+inc*real(iterations)-fval))>=precision)then;print*,'your f value =',fval,'calculated f value =',ival+inc*real(iterations),abs(ival+inc*real(iterations)-fval),'(butler_psk)';end if
            call box(width,height,3)

            select case(colorscheme)
            case('red');call colorgrad('red',iterations,r1,g1,b1)
            case('wred');call colorgrad('wred',iterations,r1,g1,b1)
            case('green');call colorgrad('green',iterations,r1,g1,b1)
            case('wgreen');call colorgrad('wgreen',iterations,r1,g1,b1)
            case('blue');call colorgrad('blue',iterations,r1,g1,b1)
            case('wblue');call colorgrad('wblue',iterations,r1,g1,b1)
            case('b2r');if(present(bpt1))then;call b2r_colorgrad(iterations,bpt1,r1,g1,b1);else;print*,'bpt1 is required (butler_psk)';stop;end if
            case('b2w2r');if(present(bpt1))then;call b2w2r_colorgrad(iterations,bpt1,r1,g1,b1);else;print*,'bpt1 is required (butler_psk)';stop;end if
            case('b2gy2r');if(present(bpt1))then;call b2gy2r_colorgrad(iterations,bpt1,r1,g1,b1);else;print*,'bpt1 is required (butler_psk)';stop;end if
            case('r2g');if(present(bpt1))then;call r2g_colorgrad(iterations,bpt1,r1,g1,b1);else;print*,'bpt1 is required (butler_psk)';stop;end if
            case('bk2r2g');if(present(bpt1))then;call bk2r2g_colorgrad(iterations,bpt1,r1,g1,b1);else;print*,'bpt1 is required (butler_psk)';stop;end if
            case('b2cy2y2r');if(present(bpt2).and.present(bpt3)) then;call b2cy2y2r_colorgrad(iterations,bpt1,bpt2,bpt3,r1,g1,b1);else;print*,'bpt2 and bpt3 are required (butler_psk)';stop;end if
            case('b2g2y2r');if(present(bpt2).and.present(bpt3)) then;call b2g2y2r_colorgrad(iterations,bpt1,bpt2,bpt3,r1,g1,b1);else;print*,'bpt2 and bpt3 are required (butler_psk)';stop;end if
            case default;print*,'Invalid colorscheme (butler_psk)';stop
            end select

            if(present(centralize))then
                call centeralize_colors(iterations,centralize,r1,g1,b1)
            endif

            if(present(r).and.present(g).and.present(b)) then
                allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1));r = r1;g = g1;b = b1
            ! else;print*,'color arrays are not allocated'
            end if

            do i = lbound(array_2D,1), ubound(array_2D,1)
                do j = lbound(array_2D,2), ubound(array_2D,2)
                    if(array_2D(i,j)==maskval) then;mask(i,j)=0
                    else;mask(i,j)=1
                    end if
                end do
            end do
            zerocolumns = 0
            do i = lbound(mask,1), ubound(mask,1)
                if(all(mask(i,lbound(array_2D,2):ubound(array_2D,2))==0).eqv..true.)then
                    zerocolumns = zerocolumns + 1
                else;nonzerocol = i
                end if
            end do
            if(nonzerocol==0)then;print*,'zero matrix (butler_psk)';return;endif

            if(dim1-zerocolumns>1)then
                if(.not.present(gap))then
                    dx = width/real(dim1-1);dy = height/real(dim2-1);call plot(-dx/2.,-dy/2.,-3)
                else;dx = width/real(dim1);dy = height/real(dim2);call plot(0.,0.,-3)
                end if
            else
                if(.not.present(gap))then
                    dx = width/real(dim1);dy = height/real(dim2)
                else;dx = width/real(dim1)*real(dim1-1)/real(dim1);dy = height/real(dim2);call plot(width/real(dim1)/2.,0.,-3)
                end if
            end if


            if(dim1-zerocolumns>1)then
                call pscolork(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,ival-10.**(10.),ival,r1(0),g1(0),b1(0))
                call pscolork(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,fval,fval+10.**(10.),r1(iterations+1),g1(iterations+1),b1(iterations+1))
                do n = 1, iterations
                    call pscolork(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,ival+real(n-1)*inc,ival+real(n)*inc,r1(n),g1(n),b1(n))
                end do
            else
                call betcolork2(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,ival-10.**(10.),ival,r1(0),g1(0),b1(0))
                call betcolork2(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,fval,fval+10.**(10.),r1(iterations+1),g1(iterations+1),b1(iterations+1))
                do n = 1, iterations
                    call betcolork2(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,ival+real(n-1)*inc,ival+real(n)*inc,r1(n),g1(n),b1(n))
                end do
            end if

            if(present(conti).and.present(continc))then
                contquan = int((maxval(array_2D)-conti)/continc+1)
                    if(dim1-zerocolumns>1)then
                        do n = 0, contquan
                            if(abs(width)<=2.)then;call newpen2(2);elseif(abs(width)>2..and.abs(width)<=4.)then;call newpen2(3);else;call newpen2(4);end if
                            if(present(thicc))then
                                if(mod(n,thicc)==0)then
                                    if(abs(width)<=2.)then;call newpen2(4);elseif(abs(width)>2..and.abs(width)<=4.)then;call newpen2(5);else;call newpen2(6);end if
                                end if
                            end if
                            call pscont3(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,1,conti+continc*real(n),0.)
                        end do
                    else
                        if(dim1-zerocolumns==1)then
                            ! if(present(gap))print*,'Having GAP for an array with one column'
                            print*,'has only one nonzero column=',nonzerocol,'(butler_psk)'
                            allocate(another(lbound(array_2D,1):ubound(array_2D,1)+1,lbound(array_2D,2):ubound(array_2D,2)))
                            allocate(anothermask(lbound(array_2D,1):ubound(array_2D,1)+1,lbound(array_2D,2):ubound(array_2D,2)))
                            another(lbound(array_2D,1):ubound(array_2D,1),lbound(array_2D,2):ubound(array_2D,2)) = array_2D
                            another(nonzerocol+1,lbound(array_2D,2):ubound(array_2D,2)) = another(nonzerocol,lbound(array_2D,2):ubound(array_2D,2))
                            anothermask(lbound(array_2D,1):ubound(array_2D,1),lbound(array_2D,2):ubound(array_2D,2))=mask
                            anothermask(nonzerocol+1,lbound(array_2D,2):ubound(array_2D,2)) = anothermask(nonzerocol,lbound(array_2D,2):ubound(array_2D,2))
                            call plot(-dx/2.,0.,-3)
                            do n = 0,contquan
                                if(abs(width)<=2.)then;call newpen2(2);elseif(abs(width)>2..and.abs(width)<=4.)then;call newpen2(3);else;call newpen2(4);end if
                                if(present(thicc))then
                                    if(mod(n,thicc)==0)then
                                        if(abs(width)<=2.)then;call newpen2(4);elseif(abs(width)>2..and.abs(width)<=4.)then;call newpen2(5);else;call newpen2(6);end if
                                    end if
                                end if
                                call pscont3(dx,dy,another,anothermask,1,dim1+1,1,dim2,dim1+1,dim2,1,conti+continc*real(n),0.)
                            end do
                            deallocate(another);deallocate(anothermask)
                            call plot(dx/2.,0.,-3)
                        end if
                    end if
            end if
            

            deallocate(r1,g1,b1)
            if(dim1-zerocolumns>1)then
                if(.not.present(gap))then
                    call plot(dx/2.,dy/2.,-3)
                end if
            else
                if(.not.present(gap))then
                    ! call plot(-dx/2.,0.,-3)
                else;call plot(-width/real(dim1)/2.,0.,-3)
                end if
            end if
            write(ounit,*)'%end butler_psk'

        end subroutine
        ! can draw contours on 1 column arrays
        subroutine butler_psbet(array_2D,width,height,maskval,ival,fval,inc,colorscheme,iterations,bpt1,bpt2,bpt3,conti,continc,thicc,r,g,b,gap,centralize)
            implicit none
            integer,intent(in)::iterations
            real,intent(in)::maskval,ival,fval,inc,width,height
            integer,intent(in),optional::bpt1,bpt2,bpt3,thicc,centralize,gap
            real,intent(in),optional::conti,continc
            real,intent(in)::array_2D(:,:)
            integer,dimension(size(array_2D,1),size(array_2D,2))::mask
            real,dimension(:,:),allocatable::another
            character(len=*),intent(in)::colorscheme
            real,dimension(:),allocatable::r1,g1,b1
            real,dimension(:),allocatable,intent(out),optional::r,g,b
            real::dx,dy
            integer::i,j,n,contquan,zerocolumns=0,nonzerocol=0,dim1,dim2

            ! if(size(array_2D,1)/=dim1 .or. size(array_2D,2)/=dim2) then 
            !     print*,'Array size /= dim1 or dim2 (butler_psbet)';stop
            ! end if
            
            dim1 = size(array_2D,1);dim2 = size(array_2D,2)
            if((abs(ival+inc*real(iterations)-fval))>=precision)then;print*,'your f value =',fval,'calculated f value =',ival+inc*real(iterations),abs(ival+inc*real(iterations)-fval),'(butler_psbet)';end if
            dx = width/real(dim1);dy = height/real(dim2)
            call box(width,height,3)
            if(.not.present(gap))then
                dx = width/real(dim1);dy = height/real(dim2)
            else;dx = width/real(dim1)*real(dim1-1)/real(dim1);dy = height/real(dim2)*real(dim2-1)/real(dim2);call plot(width/real(dim1)/2.,height/real(dim2)/2.,-3)
            end if
            select case(colorscheme)
            case('red');call colorgrad('red',iterations,r1,g1,b1)
            case('wred');call colorgrad('wred',iterations,r1,g1,b1)
            case('green');call colorgrad('green',iterations,r1,g1,b1)
            case('wgreen');call colorgrad('wgreen',iterations,r1,g1,b1)
            case('blue');call colorgrad('blue',iterations,r1,g1,b1)
            case('wblue');call colorgrad('wblue',iterations,r1,g1,b1)
            case('b2r');if(present(bpt1))then;call b2r_colorgrad(iterations,bpt1,r1,g1,b1);else;print*,'bpt1 is required (butler_psbet)';stop;end if
            case('b2w2r');if(present(bpt1))then;call b2w2r_colorgrad(iterations,bpt1,r1,g1,b1);else;print*,'bpt1 is required (butler_psbet)';stop;end if
            case('b2gy2r');if(present(bpt1))then;call b2gy2r_colorgrad(iterations,bpt1,r1,g1,b1);else;print*,'bpt1 is required (butler_psbet)';stop;end if
            case('r2g');if(present(bpt1))then;call r2g_colorgrad(iterations,bpt1,r1,g1,b1);else;print*,'bpt1 is required (butler_psbet)';stop;end if
            case('bk2r2g');if(present(bpt1))then;call bk2r2g_colorgrad(iterations,bpt1,r1,g1,b1);else;print*,'bpt1 is required (butler_psbet)';stop;end if
            case('b2cy2y2r');if(present(bpt2).and.present(bpt3)) then;call b2cy2y2r_colorgrad(iterations,bpt1,bpt2,bpt3,r1,g1,b1);else;print*,'bpt2 and bpt3 are required (butler_psbet)';stop;end if
            case('b2g2y2r');if(present(bpt2).and.present(bpt3)) then;call b2g2y2r_colorgrad(iterations,bpt1,bpt2,bpt3,r1,g1,b1);else;print*,'bpt2 and bpt3 are required (butler_psbet)';stop;end if
            case default;print*,'Invalid colorscheme (butler_pebet)';stop
            end select

            if(present(centralize))then
                call centeralize_colors(iterations,centralize,r1,g1,b1)
            endif
            
            if(present(r).and.present(g).and.present(b)) then
                allocate(r(0:iterations+1),g(0:iterations+1),b(0:iterations+1));r = r1;g = g1;b = b1
            ! else;print*,'color arrays are not allocated'
            end if

            do i = 1, dim1
                do j = 1, dim2
                    if(array_2D(i,j)==maskval) then;mask(i,j)=0
                    else;mask(i,j)=1
                    end if
                end do
            end do
            zerocolumns = 0
            do i = 1, dim1
                if(all(mask(i,1:dim2)==0).eqv..true.)then
                    zerocolumns = zerocolumns + 1
                else;nonzerocol = i
                end if
            end do
            if(nonzerocol==0)then;print*,'zero matrix (butler_psbet)';endif
            ! print*,'psbet',zerocolumns,nonzerocol
            call betcolork2(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,ival-10.**(10.),ival,r1(0),g1(0),b1(0))
            call betcolork2(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,fval,fval+10.**(10.),r1(iterations+1),g1(iterations+1),b1(iterations+1))
            do n = 1, iterations
                call betcolork2(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,ival+real(n-1)*inc,ival+real(n)*inc,r1(n),g1(n),b1(n))
            end do

            if(present(conti).and.present(continc)) then
                contquan = int((maxval(array_2D)-conti)/continc+1)
                if(dim1-zerocolumns>1)then
                    do n = 0, contquan
                        if(abs(width)<=2.)then;call newpen2(2);elseif(abs(width)>2..and.abs(width)<=4.)then;call newpen2(3);else;call newpen2(4);end if
                        if(present(thicc))then
                            if(mod(n,thicc)==0)then
                                if(abs(width)<=2.)then;call newpen2(4);elseif(abs(width)>2..and.abs(width)<=4.)then;call newpen2(5);else;call newpen2(6);end if
                            end if
                        end if
                        call pscont3(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,1,conti+continc*real(n),0.)
                    end do
                else if(dim1-zerocolumns==1)then
                    print*,'psbet nonzero column=',nonzerocol,'(butler_psbet)'
                    allocate(another(dim1,dim2));another = array_2D;another(nonzerocol-1,1:dim2) = another(nonzerocol,1:dim2)
                    mask(nonzerocol-1,1:dim2) = mask(nonzerocol,1:dim2)
                    call plot(dx/2.,0.,-3)
                    do n = 0,contquan
                        if(abs(width)<=2.)then;call newpen2(2);elseif(abs(width)>2..and.abs(width)<=4.)then;call newpen2(3);else;call newpen2(4);end if
                        if(present(thicc))then
                            if(mod(n,thicc)==0)then
                                if(abs(width)<=2.)then;call newpen2(4);elseif(abs(width)>2..and.abs(width)<=4.)then;call newpen2(5);else;call newpen2(6);end if
                            end if
                        end if
                        call pscont3(dx,dy,another,mask,1,dim1,1,dim2,dim1,dim2,1,conti+continc*real(n),0.)
                    end do
                    call plot(-dx/2.,0.,-3)
                    deallocate(another)
                end if
            end if
            ! else;print*,'no contour'

            deallocate(r1,g1,b1)
            if(present(gap))then;call plot(-width/real(dim1)/2.,-height/real(dim2)/2.,-3);else;end if
            write(ounit,*)'%end butler_psbet'
        end subroutine
        ! can draw contours on 1 column arrays * dirty due to unnecessary use of bounds
        subroutine butler_cont(array_2D,width,height,maskval,conti,continc,thicc,r,g,b,gap,maskn,contq)
            implicit none
            real,intent(in)::maskval,conti,continc,width,height
            real,intent(in)::array_2D(:,:)
            integer,dimension(size(array_2D,1),size(array_2D,2))::mask
            real,dimension(:,:),allocatable::another
            integer,dimension(:,:),allocatable::anothermask
            real,intent(in),optional::r,g,b
            integer,intent(in),optional::thicc,gap,contq
            logical,intent(in),optional::maskn
            real::dx,dy
            integer::i,j,n,contquan,zerocolumns=0,nonzerocol=0,dim1,dim2

            write(ounit,*)'%begin butler_cont'
            ! if(size(array_2D,1)/=dim1 .or. size(array_2D,2)/=dim2) then 
            !     print*,'Array size /= dim1 or dim2 (butler_cont)';stop
            ! end if 
            dim1 = size(array_2D,1);dim2 = size(array_2D,2)
            call box(width,height,3)
            do i = lbound(array_2D,1), ubound(array_2D,1)
                do j = lbound(array_2D,2), ubound(array_2D,2)
                    if(array_2D(i,j)== maskval) then;mask(i,j)=0
                    else;mask(i,j)=1
                    end if
                end do
            end do

            zerocolumns = 0
            do i = lbound(mask,1), ubound(mask,1)
                if(all(mask(i,lbound(array_2D,2):ubound(array_2D,2))==0).eqv..true.)then
                    zerocolumns = zerocolumns + 1
                else;nonzerocol = i
                end if
            end do
            if(present(maskn))then
                if(maskn)then 
                    if(present(gap))then
                        call butler_psmask(array_2D,width,height,-10.**10.,0.,r = 0.6,g = 0.6,b = 0.6,gap = gap)
                    else
                        call butler_psmask(array_2D,width,height,-10.**10.,0.,r = 0.6,g = 0.6,b = 0.6)
                    end if
                end if
            end if
            if(nonzerocol==0)then;print*,'zero matrix (butler_cont)';endif

            if(dim1-zerocolumns>1)then ! normal case
                if(.not.present(gap))then
                    dx = width/real(dim1-1);dy = height/real(dim2-1);call plot(-dx/2.,-dy/2.,-3)
                else;dx = width/real(dim1);dy = height/real(dim2)
                end if
            else ! 1 column array
                if(.not.present(gap))then
                    dx = width/real(dim1);dy = height/real(dim2)
                else;dx = width/real(dim1)*real(dim1-1)/real(dim1);dy = height/real(dim2);call plot(width/real(dim1)/2.,0.,-3)
                end if
            end if

            if(present(contq))then 
                contquan = contq
            else
                contquan = int((maxval(array_2D)-conti)/continc+1)
            end if
            if(present(r).and.present(g).and.present(b))then;call rgbk(r,g,b);else;call rgbk(0.,0.,0.);end if
            if(dim1-zerocolumns>1)then
                do n = 0, contquan
                    if(abs(width)<=2.5)then;call newpen2(2);elseif(abs(width)>2.5.and.abs(width)<=4.)then;call newpen2(3);else;call newpen2(4);end if
                    if(present(thicc))then
                        if(mod(n,thicc)==0)then
                            if(abs(width)<=2.5)then;call newpen2(4);elseif(abs(width)>2.5.and.abs(width)<=4.)then;call newpen2(5);else;call newpen2(6);end if
                        end if
                    end if
                    ! call pscont3(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,1,conti+continc*real(n),0.)
                    call pscont3(dx,dy,array_2D,mask,lbound(array_2D,1),ubound(array_2D,1),lbound(array_2D,2),ubound(array_2D,2),dim1,dim2,1,conti+continc*real(n),0.)
                    ! call pscont4(dx,dy,array_2D,mask,lbound(array_2D,1),ubound(array_2D,1),lbound(array_2D,2),ubound(array_2D,2),dim1,dim2,1,conti+continc*real(n),0.)
                end do
            else
                if(dim1-zerocolumns==1)then
                    print*,'has only one nonzero column=',nonzerocol,'(butler_cont)'
                    ! allocate(another(lbound(array_2D,1):ubound(array_2D,1)+1,lbound(array_2D,2):ubound(array_2D,2)))
                    allocate(another(size(array_2D,1)+1,size(array_2D,2)))
                    ! allocate(anothermask(lbound(array_2D,1):ubound(array_2D,1)+1,lbound(array_2D,2):ubound(array_2D,2)))
                    allocate(anothermask(size(array_2D,1)+1,size(array_2D,2)))
                    ! another(lbound(array_2D,1):ubound(array_2D,1),lbound(array_2D,2):ubound(array_2D,2)) = array_2D
                    another(1:dim1,1:dim2) = array_2D
                    ! another(nonzerocol+1,lbound(array_2D,2):ubound(array_2D,2)) = another(nonzerocol,lbound(array_2D,2):ubound(array_2D,2))
                    another(nonzerocol+1,1:dim2) = another(nonzerocol,1:dim2)
                    ! anothermask(lbound(array_2D,1):ubound(array_2D,1),lbound(array_2D,2):ubound(array_2D,2))=mask
                    anothermask(1:dim1,1:dim2) = mask
                    ! anothermask(nonzerocol+1,lbound(array_2D,2):ubound(array_2D,2)) = anothermask(nonzerocol,lbound(array_2D,2):ubound(array_2D,2))
                    anothermask(nonzerocol+1,1:dim2) = anothermask(nonzerocol,1:dim2)
                    call plot(-dx/2.,0.,-3)
                    do n = 0,contquan
                        if(abs(width)<=2.5)then;call newpen2(2);elseif(abs(width)>2.5.and.abs(width)<=4.)then;call newpen2(3);else;call newpen2(4);end if
                        if(present(thicc))then
                            if(mod(n,thicc)==0)then
                                if(abs(width)<=2.5)then;call newpen2(4);elseif(abs(width)>2.5.and.abs(width)<=4.)then;call newpen2(5);else;call newpen2(6);end if
                            end if
                        end if
                        ! call pscont3(dx,dy,another,anothermask,lbound(array_2D,1),ubound(array_2D,1)+1,lbound(array_2D,2),ubound(array_2D,2),dim1,dim2,1,conti+continc*real(n),0.)
                        call pscont3(dx,dy,another,anothermask,1,dim1+1,1,dim2,dim1+1,dim2,1,conti+continc*real(n),0.)
                    end do
                    deallocate(another);deallocate(anothermask)
                    call plot(dx/2.,0.,-3)
                end if
            end if

            call rgbk(0.,0.,0.)
            if(dim1-zerocolumns>1)then
                if(.not.present(gap))then
                    call plot(dx/2.,dy/2.,-3)
                end if
            else
                if(.not.present(gap))then
                    ! call plot(-dx/2.,0.,-3)
                else;call plot(-width/real(dim1)/2.,0.,-3)
                end if
            end if
            write(ounit,*)'%end butler_cont'
            
        end subroutine
        ! paints areas within range with color given, other regions will not be painted. betcolork2
        subroutine butler_mask(array_2D,width,height,mask_ini,mask_fin,r,g,b,gap)
            implicit none
            integer,intent(in),optional::gap
            real,intent(in),optional::r,g,b
            real,intent(in)::width,height
            real,intent(in)::array_2D(:,:)
            real,intent(in)::mask_ini,mask_fin
            integer,dimension(size(array_2D,1),size(array_2D,2))::mask
            real::dx,dy,r1,g1,b1
            integer::i,j,dim1,dim2
            
            write(16,*)"% begin butler_mask"

            ! if(size(array_2D,1)/=dim1 .or. size(array_2D,2)/=dim2) then 
            !     print*,'Array size /= dim1 or dim2 (butler_mask)';stop
            ! end if
            dim1 = size(array_2D,1);dim2 = size(array_2D,2)
            call box(width,height,3)
            if(.not.present(gap))then
                dx = width/real(dim1);dy = height/real(dim2)
            else;dx = width/real(dim1)*real(dim1-1)/real(dim1);dy = height/real(dim2);call plot(width/real(dim1)/2.,0.,-3)
            end if
            do i = 1, dim1
                do j = 1, dim2
                    if(mask_ini<=array_2D(i,j).and.array_2D(i,j)<=mask_fin) then;mask(i,j)=1
                    else;mask(i,j)=0
                    end if

                end do
            end do
            ! print*,mask

            if(present(r).and.present(g).and.present(b))then;r1=r;g1=g;b1=b;else;r1=0.;g1=0.;b1=0.;end if
            call betcolork2(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,-10.**(10.),10.**(10.),r1,g1,b1)
            if(present(gap))then;call plot(-width/real(dim1)/2.,0.,-3);else;end if
            write(16,*)"% end butler_mask"

        end subroutine
        ! for integer arrays
        subroutine butler_imask(array_2D,width,height,imask,r,g,b,gap)
            implicit none
            integer,intent(in)::imask
            integer,intent(in),optional::gap
            real,intent(in),optional::r,g,b
            real,intent(in)::width,height
            integer,intent(in)::array_2D(:,:)
            integer,dimension(size(array_2D,1),size(array_2D,2))::mask
            real::dx,dy,r1,g1,b1
            integer::i,j,dim1,dim2
            
            write(16,*)"% begin butler_mask"
            dim1 = size(array_2D,1);dim2 = size(array_2D,2)

            ! if(size(array_2D,1)/=dim1 .or. size(array_2D,2)/=dim2) then 
            !     print*,'Array size /= dim1 or dim2(butler_imask)';stop
            ! end if
            call box(width,height,3)
            if(.not.present(gap))then
                dx = width/real(dim1);dy = height/real(dim2)
            else;dx = width/real(dim1)*real(dim1-1)/real(dim1);dy = height/real(dim2)*real(dim2-1)/real(dim2);call plot(width/real(dim1)/2.,height/real(dim2)/2.,-3)
            end if
            do i = 1, dim1
                do j = 1, dim2
                    if(array_2D(i,j)==imask) then;mask(i,j)=1
                    else;mask(i,j)=0
                    end if

                end do
            end do
            ! print*,mask

            if(present(r).and.present(g).and.present(b))then;r1=r;g1=g;b1=b;else;r1=0.;g1=0.;b1=0.;end if
            call betcolorI(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,imask,r1,g1,b1)
            if(present(gap))then;call plot(-width/real(dim1)/2.,-height/real(dim2)/2.,-3);else;end if
            write(16,*)"% end butler_mask"

        end subroutine
        ! paints areas within range with color given, other regions will not be painted. pscolork
        subroutine butler_psmask(array_2D,width,height,mask_ini,mask_fin,r,g,b,gap)
            implicit none
            integer,intent(in),optional::gap
            real,intent(in),optional::r,g,b
            real,intent(in)::width,height
            real,intent(in)::array_2D(:,:)
            real,intent(in)::mask_ini,mask_fin
            integer,dimension(size(array_2D,1),size(array_2D,2))::mask
            real,dimension(:,:),allocatable::another
            integer,dimension(:,:),allocatable::anothermask
            real::dx,dy,r1,g1,b1
            integer::i,j,zerocolumns,nonzerocol,dim1,dim2
            
            write(16,*)"% begin butler_psmask"
            call newpen2(3)
            ! if(size(array_2D,1)/=dim1 .or. size(array_2D,2)/=dim2) then 
            !     print*,'Array size /= dim1 or dim2 (butler_psmask)';stop
            ! end if
            dim1 = size(array_2D,1);dim2 = size(array_2D,2)
            call box(width,height,3)

            do i = 1, dim1
                do j = 1, dim2
                    if(array_2D(i,j)/=0..and.array_2D(i,j)/=0.) then;mask(i,j)=1
                    else;mask(i,j)=0
                    end if

                end do
            end do

            zerocolumns = 0
            do i = 1, dim1
                if(all(mask(i,1:dim2)==0).eqv..true.)then
                    zerocolumns = zerocolumns + 1
                else;nonzerocol = i
                end if
            end do
            if(nonzerocol==0)then;print*,'zero matrix (butler_psmask)';return;endif

            if(dim1-zerocolumns>1)then
                if(.not.present(gap))then
                    dx = width/real(dim1-1);dy = height/real(dim2-1);call plot(-dx/2.,-dy/2.,-3)
                else;dx = width/real(dim1);dy = height/real(dim2)
                end if
            else
                if(.not.present(gap))then
                    dx = width/real(dim1);dy = height/real(dim2)
                else;dx = width/real(dim1)*real(dim1-1)/real(dim1);dy = height/real(dim2);call plot(width/real(dim1)/2.,0.,-3)
                end if
            end if


            if(present(r).and.present(g).and.present(b))then;r1=r;g1=g;b1=b;else;r1=0.;g1=0.;b1=0.;end if
            if(dim1-zerocolumns>1)then
                call pscolork(dx,dy,array_2D,mask,1,dim1,1,dim2,dim1,dim2,mask_ini,mask_fin,r1,g1,b1)
            else 
                allocate(another(dim1+1,dim2));allocate(anothermask(dim1+1,dim2))
                another(1:dim1,1:dim2) = array_2D;another(nonzerocol+1,1:dim2) = another(nonzerocol,1:dim2)
                anothermask(1:dim1,1:dim2)=mask;anothermask(nonzerocol+1,1:dim2) = anothermask(nonzerocol,1:dim2)
                call plot(-dx/2.,0.,-3)
                call pscolork(dx,dy,another,anothermask,1,dim1+1,1,dim2,dim1+1,dim2,mask_ini,mask_fin,r1,g1,b1)
                deallocate(another);deallocate(anothermask)
                call plot(dx/2.,0.,-3)
            end if


            if(dim1-zerocolumns>1)then
                if(.not.present(gap))then
                    call plot(dx/2.,dy/2.,-3)
                end if
            else
                if(.not.present(gap))then
                    ! call plot(-dx/2.,0.,-3)
                else;call plot(-width/real(dim1)/2.,0.,-3)
                end if
            end if
            write(16,*)"% end butler_psmask"
        end subroutine
        ! recognizes x_2D as an array of values in the x axis,y_2D as values in the y axis in a xy plane. or any cartesian plane.
        ! as of now, mask applies to both x and y arrays
        ! nonzero bound values are plotted regardless of the thinning factors
        subroutine butler_vector(x_2D,y_2D,width,height,scalef,thinfx,thinfy,maskini,maskfin,arrowwidth,line_thickness,arrowtype,gap)
            implicit none
            real,intent(in)::x_2D(:,:),y_2D(:,:),width,height
            real,dimension(:,:),allocatable::Vscaler
            real,intent(in),optional::maskini,maskfin,arrowwidth,scalef
            integer,intent(in),optional::gap,arrowtype,line_thickness,thinfx,thinfy
            real::dx,dy,arrowwidth_local,scalef_local,x0,y0,x1,y1
            integer::dim1,dim2,i,j,arrowtype_local,thinf_local_x,thinf_local_y,qx,rx,qy,ry,l,m,n0lbx=0,n0ubx=0,n0lby=0,n0uby=0
            integer,dimension(:),allocatable::leapx,leapy

            write(ounit,*)'%begin butler_vector'
            call box(width,height,3)

            if(size(x_2D,1)/=size(y_2D,1))then;print*,'Array size do not match in the 1st dimension (butler_vector)';stop;endif
            if(size(x_2D,2)/=size(y_2D,2))then;print*,'Array size do not match in the 2nd dimension (butler_vector)';stop;endif

            dim1 = size(x_2D,1);dim2 = size(x_2D,2)
            allocate(Vscaler(dim1,dim2))
            Vscaler = sqrt(x_2D**2. + y_2D**2.)

            if(.not.present(gap))then
                dx = width/real(dim1-1);dy = height/real(dim2-1)
            else;dx = width/real(dim1);dy = height/real(dim2)
            end if
            
            n0lbx = 0;n0ubx = 0;n0lby = 0;n0uby = 0

            if(all(Vscaler(:,:)==0.))then;print*,'zero vector matrix (butler_vector)';return;endif
            do i = 1, dim1
                if(any(Vscaler(i,:)/=0.))then;n0lbx = i;exit;endif ! find the first coloumn with nonzero values
            end do
            if(any(Vscaler(dim1,:)/=0.))then 
                n0ubx = dim1
            else
                do i = n0lbx+1,dim1
                    if(all(Vscaler(i,:)==0.))then
                        if(any(Vscaler(i:dim1,:)/=0.))then;cycle ! find the last coloumn with nonzero values
                        else;n0ubx = i-1;exit
                        end if
                    endif
                end do
            end if
            do i = 1, dim2
                if(any(Vscaler(:,i)/=0.))then;n0lby = i;exit;endif ! find the first row with nonzero values
            end do
            if(any(Vscaler(:,dim2)/=0.))then 
                n0uby = dim2
            else
                do i = n0lby+1,dim2
                    if(all(Vscaler(:,i)==0.))then
                        if(any(Vscaler(:,i:dim2)/=0.))then;cycle ! find the last row with nonzero values
                        else;n0uby = i-1;exit
                        end if
                    endif
                end do
            end if
            ! print*,n0lbx,n0ubx,n0lby,n0uby

            if(present(scalef))then;scalef_local = scalef;else;scalef_local = 1.;end if
            if(present(thinfx))then;thinf_local_x = thinfx;else;thinf_local_x = 1;end if
            if(present(thinfy))then;thinf_local_y = thinfy;else;thinf_local_y = 1;end if
            
            qx = int((n0ubx-n0lbx)/thinf_local_x)
            rx = mod((n0ubx-n0lbx),thinf_local_x)
            qy = int((n0uby-n0lby)/thinf_local_y)
            ry = mod((n0uby-n0lby),thinf_local_y)
            allocate(leapx(qx),leapy(qy))
            leapx(:) = thinf_local_x
            leapy(:) = thinf_local_y
            if(rx/=0)then;leapx(1:rx) = leapx(1:rx) + 1;endif
            if(ry/=0)then;leapy(1:ry) = leapy(1:ry) + 1;endif

            l = n0lbx;m = n0lby
            do i = 1, size(leapx)+1
                do j = 1, size(leapy)+1
                    call process(l,m)
                    if(j/=size(leapy)+1)m = m + leapy(j)
                end do
                if(i/=size(leapx)+1)l = l + leapx(i)
                m = 1
            end do
            deallocate(leapx,leapy,Vscaler)
            write(ounit,*)'%end butler_vector'
            return
            contains
            subroutine process(x,y)
                integer,intent(in)::x,y
                if(.not.present(gap))then
                    x0 = real(x-1)*dx;y0 = real(y-1)*dy
                else;x0 = real(x-1)*dx + dx/2.;y0 = real(y-1)*dy + dy/2.
                endif
                ! if(x_2D(x,y)==0..and.y_2D(x,y)==0.)then;print*,'zero vector (butler_vector)';endif
                x1 = x0 + x_2D(x,y)*scalef_local
                y1 = y0 + y_2D(x,y)*scalef_local
                ! if(i==1.and.j==1)then;print*,'x0,y0,x1,y1 =',x0,y0,x1,y1,'x_2D,y_2D =',x_2D(i,j),y_2D(i,j);endif
                if(present(arrowwidth))then
                    arrowwidth_local = arrowwidth
                else;arrowwidth_local = sqrt((x1-x0)**2.+(y1-y0)**2.)/10.
                end if
                if(present(arrowtype))then;arrowtype_local = arrowtype;else;arrowtype_local = 4;end if
                if(present(maskini).and.present(maskfin))then
                    if(maskini<=sqrt((x1-x0)**2+(y1-y0)**2).and.sqrt((x1-x0)**2+(y1-y0)**2)<=maskfin)then
                        call arrow(x0,y0,x1,y1,arrowwidth_local,line_thickness,arrowtype_local)
                    end if
                else
                    call arrow(x0,y0,x1,y1,arrowwidth_local,line_thickness,arrowtype_local)
                end if
            end subroutine process
        end subroutine
        subroutine butler_linegraph(array_1D,width,height,memi,memf,rmask,mem,memscale,memiter,memsymfreq,memsymsize,memflqt,memloc,memlabel,blabel,tlabel,error_1D,maskbyc,dots,lidots,rl,gl,bl,lthick)
            use functions
            implicit none
            real,intent(in)::array_1D(:),width,height,memi,memf
            real,intent(in),optional::error_1D(:),memsymsize,rmask,memscale,rl,gl,bl
            integer,intent(in),optional::memiter,memsymfreq,memflqt,lthick
            character(len=*),intent(in),optional::memloc,memlabel,blabel,tlabel
            logical,intent(in),optional::maskbyc,dots,mem,lidots
            real,dimension(:),allocatable::ploty,plotysem,betmlkx,betmlky
            real::dx,memsymsize_local,rmask_local,a,b,red,green,blue,memscale_local
            integer::memiter_local,memsymfreq_local,memflqt_local,iangle,i,lthick_local
            logical::maskbyc_local,dots_local,mem_local,lidots_local

                write(ounit,*)'%begin butler_linegraph'
                if(present(error_1D))then
                    if(size(array_1D,1)/=size(error_1D,1))then
                        print*,'Array sizes do not match (butler_linegraph)';stop
                    else
                        allocate(plotysem(size(array_1D)))
                    endif
                end if
                allocate(ploty(size(array_1D)))
                red = 0.6 ; green = 0.6 ; blue = 0.6
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                        ! creating local parameters
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                if(present(mem))then 
                    mem_local = mem
                else;mem_local = .false.
                end if
                if(present(memscale))then 
                    memscale_local = memscale
                else;memscale_local = 1.
                end if
                if(present(memsymsize))then 
                    memsymsize_local = memsymsize
                else;memsymsize_local = height/8.
                end if
                if(present(memiter))then 
                    memiter_local = memiter
                else
                    do i = 1, 10
                        if(mod(abs(memf-memi),1./(10.**real(i-1)))<=precision)then 
                            ! print*,'here',i,memf-memi,(memf-memi)/(10.**real(i-1))
                            memiter_local = abs(int((memf-memi)*(10.**real(i-1)))) + 1
                            exit
                        end if
                    end do
                end if
                if(present(memsymfreq))then 
                    memsymfreq_local = memsymfreq
                else;memsymfreq_local = 1
                end if
                if(present(memflqt))then 
                    memflqt_local = memflqt
                else;memflqt_local = 1
                end if  
                iangle = -90 ! left by default
                if(present(memloc))then 
                    if(memloc=='right'.or.memloc=='Right'.or.memloc=='RIGHT')then 
                        iangle = 90
                    end if
                end if
                if(present(rmask))then 
                    rmask_local = rmask
                else;rmask_local = 0.
                end if
                if(present(maskbyc))then 
                    maskbyc_local = maskbyc
                else;maskbyc_local = .false.
                end if
                if(present(dots))then 
                    dots_local = dots
                else;dots_local = .false.
                end if
                if(present(lthick))then 
                    lthick_local = lthick
                else;
                    if(memsymsize_local<=0.2)then;lthick_local = 2
                    else if(memsymsize_local>0.2.and.memsymsize_local<=0.5)then;lthick_local = 3
                    else if(memsymsize_local>0.5.and.memsymsize_local<=0.8)then;lthick_local = 4
                    else;lthick_local = 5;end if
                end if
                if(present(lidots))then 
                    lidots_local = lidots
                else;lidots_local = .false.
                end if
                ! print*,'parameter values =',memi,memf,memiter_local,memsymfreq_local,memsymsize_local,memflqt_local,height,iangle,rmask_local,maskbyc_local,dots_local
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                 ! creating the box and the center line
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                call rgbk(0.,0.,0.)
                call box(width,height,3) 
                if(present(maskbyc).or.(memf+memi)==0.)then 
                    call rgbk(0.5,0.5,0.5);call plot(0.,height/2.,3);call plot(width,height/2.,2);call rgbk(0.,0.,0.)
                end if
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                    ! creating num_memori and labels
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                if(mem_local)then
                    ! print*,'memiter_local =',memiter_local  
                    call num_memori(memi*memscale_local,memf*memscale_local,memiter_local,memsymfreq_local,memsymsize_local,memflqt_local,height,iangle)
                end if
                if(present(memlabel))then 
                    call symbolc(rsign(real(iangle))*(memsymsize_local+real((abs(memflqt_local))+intdigits(int(memf*memscale_local))/3.)),height/2.,memsymsize_local,trim(adjustl(memlabel)),real(-iangle))
                end if
                if(present(tlabel))then 
                    call symbolc(width/2.,height+memsymsize_local/2.,memsymsize_local*1.3,trim(adjustl(tlabel)),0.)
                end if
                if(present(blabel))then 
                    call symbolc(width/2.,-memsymsize_local*2.25,memsymsize_local,trim(adjustl(blabel)),0.)
                end if
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                    ! getting dx and y values for array_1D
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                dx = width/real(size(array_1D,1))
                do i = 1, size(array_1D)
                    call gmark_ratio(array_1D(i),memi,memf,height,ploty(i)) ! getting y values for array_1D
                    if(present(error_1D))then 
                        plotysem(i) = error_1D(i)*height/(memf-memi) ! for error bar values if there are any
                    end if
                end do
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                    ! painting areas below y center if prompted
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                if(maskbyc_local) then ! mask below y center
                    do i = 1, size(array_1D)
                        if(i>1.and.ploty(i-1)/=0..and.ploty(i)/=0.)then                                             ! painting areas below center 
                            if(ploty(i-1)>height/2. .and. ploty(i)>height/2.)then ! do nothing
                                ! print*,'do nothing, i=',i
                            else if(ploty(i-1)<height/2. .and. ploty(i)<height/2.)then ! paint the trapezoid area below y center
                                allocate(betmlkx(4));allocate(betmlky(4))
                                betmlkx(1) = dx*real(i-2)+dx/2.;betmlky(1) = height/2.
                                betmlkx(2) = dx*real(i-2)+dx/2.;betmlky(2) = ploty(i-1)
                                betmlkx(3) = dx*real(i-1)+dx/2.;betmlky(3) = ploty(i)
                                betmlkx(4) = dx*real(i-1)+dx/2.;betmlky(4) = height/2.
                                call betmlk(betmlkx,betmlky,4,4,red,green,blue)
                                deallocate(betmlkx,betmlky)
                            else if(ploty(i-1)<height/2. .and. ploty(i)>height/2.)then ! paint the triangle area below y center
                                a = (ploty(i)-ploty(i-1))/dx   ! a = (y2-y1)/(x2-x1) = slope
                                b = ploty(i-1) - a*(dx*real(i-2)+dx/2.)  ! b = y1 - (y2-y1)*x1/(x2-x1) = y1 - slope*x1
                                allocate(betmlkx(3));allocate(betmlky(3))
                                betmlkx(1) = dx*real(i-2)+dx/2.;betmlky(1) = height/2.
                                betmlkx(2) = (height/2.-b)/a;betmlky(2) = height/2. ! (yc-b)/a = x
                                betmlkx(3) = dx*real(i-2)+dx/2.;betmlky(3) = ploty(i-1)
                                call betmlk(betmlkx,betmlky,3,3,red,green,blue)
                                deallocate(betmlkx,betmlky)
                            else if(ploty(i-1)>height/2. .and. ploty(i)<height/2.)then 
                                a = (ploty(i)-ploty(i-1))/dx
                                b = ploty(i-1) - a*(dx*real(i-2)+dx/2.)
                                allocate(betmlkx(3));allocate(betmlky(3))
                                betmlkx(1) = (height/2.-b)/a;betmlky(1) = height/2.
                                betmlkx(2) = dx*real(i-1)+dx/2.;betmlky(2) = height/2.
                                betmlkx(3) = dx*real(i-1)+dx/2.;betmlky(3) = ploty(i)
                                call betmlk(betmlkx,betmlky,3,3,red,green,blue)
                                deallocate(betmlkx,betmlky)
                            end if
                        end if
                    end do
                end if
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                            ! plotting values 
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                if(present(rl).and.present(gl).and.present(bl))call rgbk(rl,gl,bl)
                call newpen2(lthick_local)
                do i = 1, size(array_1D)
                    if(array_1D(i) == rmask_local)cycle 

                    !!!!! EVERYTHING BELOW THIS LINE WILL NOT BE EXECUTED IF THE CURRENT VALUE == rmask_local!!!!!

                    if(dots_local)call gmark(dx*real(i-1)+dx/2.,ploty(i),memsymsize_local/4.,1) ! plotting the dots if prompted

                    if(i>1)then                                         ! drawing lines between the dots
                        if(array_1D(i-1) /= rmask_local)then 
                            call plot(dx*real(i-2)+dx/2.,ploty(i-1),3);call plot(dx*real(i-1)+dx/2.,ploty(i),2)
                        end if
                    end if
                    if(lidots_local)then
                        if(i>1.and.array_1D(i) == (array_1D(i+1)+array_1D(i-1))/2.)then
                            call gmark(dx*real(i-1)+dx/2.,ploty(i),memsymsize_local/4.,4) ! the program below only works for 2 elements should fix this later
                        else if(size(array_1D)>12.and.array_1D(1) == array_1D(1+12).and.array_1D(1) == (array_1D(2) + array_1D(12))/2.)then   ! mark presumably linearly interpolated values, not perfect at all
                            call gmark(dx/2.,ploty(1),memsymsize_local/4.,4)
                        end if
                    end if

                    if(present(error_1D))then                           ! drawing error bars
                        call plot(dx*real(i-1)+dx/2.,ploty(i)-plotysem(i),3);call plot(dx*real(i-1)+dx/2.,ploty(i)+plotysem(i),2)
                    end if
                end do

                deallocate(ploty)
                if(present(error_1D))deallocate(plotysem)
                if(present(rl).and.present(gl).and.present(bl))call rgbk(0.,0.,0.)
                return
            
        end subroutine
    ! END PS bois
    ! RANDOM
end module subroutines

module MITgcm
    implicit none 
    contains
    ! creates array (and or corresponding bin, csv files) to be used on Northern or Southern Open Boundary conditions, calculated from any hydrographic data
    subroutine DATA2OBJ(OLx,Nx,Nr,TimeLevels,DATA_2D,filenamewoex,createbin,createcsv,OBJ_2D)
        use functions
        implicit none 
        integer,intent(in)::OLx,Nx,Nr,TimeLevels
        real,intent(in)::DATA_2D(:,:)
        real,dimension(:,:),allocatable::DATA_local
        character(len=*),intent(in),optional::filenamewoex
        real,dimension(1-OLx:Nx+OLx,Nr),intent(out),optional::OBJ_2D
        real,dimension(1-OLx:Nx+OLx,Nr)::OBJ_local
        real,dimension(1-OLx:Nx+OLx,Nr,TimeLevels)::OBJ_file_array
        logical,intent(in),optional::createcsv,createbin
        integer,dimension(:),allocatable::leapx,leapz! one less than the number of columns
        real,dimension(:),allocatable::incx,incz ! for linear interpolation
        integer::i,j,qx,rx,qz,rz,rx2,rz2,x,d,z,st,t
        character(len=255)::csv_name,bin_name

        allocate(DATA_local(size(DATA_2D,1),size(DATA_2D,2)));!print*,'DATA_local size =',size(DATA_local,1),size(DATA_local,2)
        DATA_local = DATA_2D

        qx = int((Nx-1)/(ubound(DATA_local,1)-lbound(DATA_local,1)));!print*,qx,'qx'
        rx = mod((Nx-1),(ubound(DATA_local,1)-lbound(DATA_local,1)));!print*,rx,'rx'
        qz = int((Nr-1)/(ubound(DATA_local,2)-lbound(DATA_local,2)));!print*,qz,'qz'
        rz = mod((Nr-1),(ubound(DATA_local,2)-lbound(DATA_local,2)));!print*,rz,'rz'
        ! if(qx==0.and.rx==0)then;print*,'Nx is too small for the given data';stop;endif
        ! if(qz==0.and.rz==0)then;print*,'Nr is too small for the given data';stop;endif

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                        ! get info about DATA array and OBJ array
                                     ! the discrepencies in their dimensions are calculated 
                ! find the optimal way to fill in the OBJ array (either by spreading or by intermittent filling of DATA array)
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        if(qx/=0)then
            allocate(leapx(ubound(DATA_local,1)-1))
            leapx(:) = qx
            if(rx/=0)then 
                leapx(1:rx) = leapx(1:rx) + 1
            end if
        else if(qx==0)then 
            allocate(leapx(Nx-1))
            leapx(1:rx) = int((ubound(DATA_local,1)-1)/(Nx-1))
            rx2 = mod((ubound(DATA_local,1)-1),(Nx-1))
            if(rx2/=0)then
                leapx(1:rx2) = leapx(1:rx2) + 1
            end if
        end if
        ! print*,leapx
        if(qz/=0)then
            allocate(leapz(ubound(DATA_local,2)-1))
            leapz(:) = qz
            if(rz/=0)then
                leapz(1:rz) = leapz(1:rz) + 1
            end if
        else if(qz==0)then
            allocate(leapz(Nr-1))
            leapz(1:rz) = int((ubound(DATA_local,2)-1)/(Nr-1))
            rz2 = mod((ubound(DATA_local,2)-1),(Nr-1))
            if(rz2/=0)then
                leapz(1:rz2) = leapz(1:rz2) + 1
            end if
        end if
        ! print*,leapz
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                        ! fill the OBJ array with the DATA array
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        st = 1;x = 1
        do i = lbound(leapx,1), ubound(leapx,1)+1
            d = 1;z = 1
            do j = lbound(leapz,1), ubound(leapz,1)+1
                OBJ_local(x,z) = DATA_local(st,d)
                if(j==ubound(leapz,1)+1)then;cycle
                else
                    if(qz/=0)then;z = z + leapz(j);d = d + 1;endif
                    if(qz==0)then;d = d + leapz(j);z = z + 1;endif
                end if
            end do
            if(i==ubound(leapx,1)+1)then;cycle
            else
                if(qx/=0)then;x = x + leapx(i);st = st + 1;endif
                if(qx==0)then;st = st + leapx(i);x = x + 1;endif
            end if
        end do
        ! print*,'final x =',x,'final z =',z,'final st =',st,'final d =',d

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                    ! copy data of the domain edges of the array to the overlapping grid fields
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        OBJ_local(1-OLx:0,:) = spread(OBJ_local(1,:), 1, OLx)
        OBJ_local(Nx+1:Nx+OLx,:) = spread(OBJ_local(Nx,:), 1, OLx)
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                    ! linearly interpolate the data in between if necessary (qx,qz/=0)
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        if(qx/=0)then 
            allocate(incx(Nr))
            x = 1
            do i = lbound(leapx,1), ubound(leapx,1)
                do j = 1, leapx(i)-1
                    incx = (OBJ_local(x+leapx(i),:) - OBJ_local(x,:))/leapx(i) ! tis an array of diffs
                    OBJ_local(x+j,:) = OBJ_local(x,:) + incx*j
                end do
                x = x + leapx(i)
            end do
        else if(qz/=0)then
            allocate(incz(Nx))
            z = 1
            do i = lbound(leapz,1), ubound(leapz,1)
                do j = 1, leapz(i)-1
                    incz = (OBJ_local(:,z+leapz(i)) - OBJ_local(:,z))/leapz(i) ! tis an array of diffs
                    OBJ_local(:,z+j) = OBJ_local(:,z) + incz*j
                end do
                z = z + leapz(i)
            end do
        end if
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                    ! writing data to files or to outgoing array
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        if(present(OBJ_2D))OBJ_2D = OBJ_local

        print*,'OBJ_2D 1st dimension =',lbound(OBJ_local,1),'to',ubound(OBJ_local,1)
        print*,'OBJ_2D 2nd dimension =',lbound(OBJ_local,2),'to',ubound(OBJ_local,2)
        print*,'OBJ TimeLevels =',TimeLevels
        do t = 1, TimeLevels
            OBJ_file_array(:,:,t) = OBJ_local(:,:)
        end do
        if(present(filenamewoex))then
            if(present(createcsv))then
                if(createcsv)then
                    csv_name = change_extension(filenamewoex,'csv')
                    open(234,file = csv_name, status = 'replace', action = 'write', form = 'formatted')
                        do j = 1, Nr
                            write(234,*)OBJ_local(:,j)
                        end do
                    close(234)
                end if
            end if
            if(present(createbin))then
                if(createbin)then
                    bin_name = change_extension(filenamewoex,'bin')
                    open(235,file = bin_name,status = 'replace',form = 'unformatted',action = 'write',access = 'direct',recl = (ubound(OBJ_local,1)-lbound(OBJ_local,1)+1)*Nr*TimeLevels*4,convert='big_endian')
                        write(235,rec=1)OBJ_file_array
                    close(235)
                end if
            end if
        end if
      

        deallocate(DATA_local,leapx,leapz)
    end subroutine
    subroutine handle_err(status)
        use netcdf
        integer, intent(in) :: status
        if (status /= nf90_noerr) then
            print *, trim(nf90_strerror(status))
            stop "Stopped"
        end if
    end subroutine handle_err
    ! Uc and Vc are the velocities at the center of the grid cells, averaged from velocity values on each side of the grids.
    ! Uc and Vc have the same size; (ngrids_x,ngrids_y,ngrids_z,timesteps)
    subroutine state2mat(ncfile,U,Uc,V,Vc,W,T,S,Eta,info)
        use netcdf
        implicit none
        character(len=*),intent(in) :: ncfile
        integer :: ncid, status
        integer :: nvars, ndims, ngatts, unlimdimid
        integer :: varid
        character(len=nf90_max_name) :: varname
        integer, allocatable :: dimids(:), dimlens(:)
        real,dimension(:,:,:,:),allocatable,intent(out),optional::U,V,W,T,S,Uc,Vc
        real,dimension(:,:,:,:),allocatable::U_local,V_local
        real,dimension(:,:,:),allocatable,intent(out),optional::eta
        logical,intent(in),optional::info
        logical::info_local = .false.
        integer :: i,j
        print*,'------------------------'
        print*,'Reading file:',trim(ncfile)
        if(present(info))info_local = info
        ! data obtainment
        ! Open the NetCDF file
        status = nf90_open(trim(ncfile), nf90_nowrite, ncid)
        if (status /= nf90_noerr) call handle_err(status)

        ! Get information about the file
        status = nf90_inquire(ncid, ndims, nvars, ngatts, unlimdimid)
        if (status /= nf90_noerr) call handle_err(status)

        if(info_local)print *, "Number of variables:", nvars

        ! Iterate through all variables to get basic idea of the file
        do varid = 1, nvars
            ! Get variable name and number of dimensions
            status = nf90_inquire_variable(ncid, varid, varname, ndims=ndims)
            if (status /= nf90_noerr) call handle_err(status)

            if(info_local)print *, "Variable ", trim(varname), " has ", ndims, " dimensions"
            if(ndims >=100)then;print*,'ndims >= 100';stop;endif

            ! Allocate arrays for dimension IDs and lengths
            allocate(dimids(ndims), dimlens(ndims))

            ! Get dimension IDs
            status = nf90_inquire_variable(ncid, varid, dimids=dimids)
            if (status /= nf90_noerr) call handle_err(status)

            ! Get dimension lengths
            do i = 1, ndims
                status = nf90_inquire_dimension(ncid, dimids(i), len=dimlens(i))
                if (status /= nf90_noerr) call handle_err(status)
                if(info_local)print *, "  Dimension ", i, " length: ", dimlens(i)
            end do

            ! Allocate arrays based on variable name and then read data
            select case(trim(varname))
            case('U')
                allocate(U_local(dimlens(1), dimlens(2), dimlens(3), dimlens(4)))
                status = nf90_get_var(ncid, varid, U_local)
                if(info_local)then
                    print*,'minimum U:',minval(U_local);print*,'maximum U:',maxval(U_local)
                end if
            case('V')
                allocate(V_local(dimlens(1), dimlens(2), dimlens(3), dimlens(4)))
                status = nf90_get_var(ncid, varid, V_local)
                if(info_local)then
                    print*,'minimum V:',minval(V_local);print*,'maximum V:',maxval(V_local)
                end if
            case('W')
                if(present(W))then
                    allocate(W(dimlens(1), dimlens(2), dimlens(3), dimlens(4)))
                    status = nf90_get_var(ncid, varid, W)
                    if(info_local)then
                        print*,'minimum W:',minval(W);print*,'maximum W:',maxval(W)
                    end if
                end if
            case('Temp')
                if(present(T))then
                    allocate(T(dimlens(1), dimlens(2), dimlens(3), dimlens(4)))
                    status = nf90_get_var(ncid, varid, T)
                    if(info_local)then
                        print*,'minimum Temp:',minval(T);print*,'maximum Temp:',maxval(T)
                    end if
                end if
            case('S')
                if(present(S))then
                    allocate(S(dimlens(1), dimlens(2), dimlens(3), dimlens(4)))
                    status = nf90_get_var(ncid, varid, S)
                    if(info_local)then
                        print*,'minimum S:',minval(S);print*,'maximum S:',maxval(S)
                    end if
                end if
            case('Eta')
                if(present(Eta))then
                    allocate(Eta(dimlens(1), dimlens(2), dimlens(3)))
                    status = nf90_get_var(ncid, varid, Eta)
                    if(info_local)then
                        print*,'minimum Eta:',minval(Eta);print*,'maximum Eta:',maxval(Eta)
                    end if
                end if
            end select
            deallocate(dimids, dimlens)



            if(info_local)print *, "------------------------"
        end do

        if(present(U))then 
            allocate(U(size(U_local,1),size(U_local,2),size(U_local,3),size(U_local,4)))
            U = U_local
        end if
        if(present(V))then 
            allocate(V(size(V_local,1),size(V_local,2),size(V_local,3),size(V_local,4)))
            V = V_local
        end if

        if(present(Uc))then 
            allocate(Uc(size(U_local,1)-1,size(U_local,2),size(U_local,3),size(U_local,4)))
            do i = 1, size(U_local,1)-1
                do j = 1, size(U_local,2)
                    Uc(i,j,:,:) = (U_local(i,j,:,:)+U_local(i+1,j,:,:))/2.
                end do
            end do
            if(info_local)print*,'Giving Grid Averaged U: Uc'
            if(info_local)print*,'size of Uc:',size(Uc,1),size(Uc,2),size(Uc,3),size(Uc,4)
        endif
        if(present(Vc))then 
            allocate(Vc(size(V_local,1),size(V_local,2)-1,size(V_local,3),size(V_local,4)))
            do i = 1, size(V_local,1)
                do j = 1, size(V_local,2)-1
                    Vc(i,j,:,:) = (V_local(i,j,:,:)+V_local(i,j+1,:,:))/2.
                end do
            end do
            if(info_local)print*,'Giving Grid Averaged V: Vc'
            if(info_local)print*,'size of Vc:',size(Vc,1),size(Vc,2),size(Vc,3),size(Vc,4)
        endif
        if(info_local)print *, "------------------------"
        ! Close the file
        status = nf90_close(ncid)
        if (status /= nf90_noerr) call handle_err(status)

        deallocate(U_local,V_local)
        return
        ! data obtainment ends here
    end subroutine
end module

module always
    use subroutines
    use MITgcm
    use constants
    use functions
    use data_types
    contains
end module

module test
    use constants
    contains
    subroutine printl()
        print*,l
        l = l +1
    end subroutine
end module
