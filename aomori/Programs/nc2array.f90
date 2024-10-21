program reading_ncfiles
    use netcdf
    use netcdf_nf_interfaces
    use subroutines
    implicit none

    real, dimension(:,:,:,:,:), allocatable :: array
    integer :: ncid, varid, status, dimids(5), retval
    integer :: years, months, lines, stations, depth  ! Declare dimension variables

    ! Open the NetCDF file
    retval = nf90_open('potemp_c5.nc', nf90_NOWRITE, ncid)
    if (retval /= NF90_NOERR) then
        print *, "Error opening file"
        stop
    end if

    ! Get the variable ID
    retval = nf90_inq_varid(ncid, 'potemp_c5', varid)
    if (retval /= NF90_NOERR) then
        print *, "Error getting variable ID"
        stop
    end if

    ! Get the variable dimensions
    retval = nf90_inquire_variable(ncid, varid, dimids=dimids)
    if (retval /= NF90_NOERR) then
        print *, "Error getting variable dimensions"
        stop
    end if

    ! Get the dimension lengths
    retval = nf_inq_dimlen(ncid, dimids(1), years)
    if (retval /= NF90_NOERR) then
        print *, "Error getting dimension length for years"
        stop
    end if

    retval = nf_inq_dimlen(ncid, dimids(2), months)
    if (retval /= NF90_NOERR) then
        print *, "Error getting dimension length for months"
        stop
    end if

    retval = nf_inq_dimlen(ncid, dimids(3), lines)
    if (retval /= NF90_NOERR) then
        print *, "Error getting dimension length for lines"
        stop
    end if

    retval = nf_inq_dimlen(ncid, dimids(4), stations)
    if (retval /= NF90_NOERR) then
        print *, "Error getting dimension length for stations"
        stop
    end if

    retval = nf_inq_dimlen(ncid, dimids(5), depth)
    if (retval /= NF90_NOERR) then
        print *, "Error getting dimension length for depth"
        stop
    end if

    ! Allocate the array
    allocate(array(years, months, lines, stations, depth))

    ! Read the variable data
    retval = nf90_get_var(ncid, varid, array)
    if (retval /= NF90_NOERR) then
        print *, "Error reading variable"
        stop
    end if

    ! Close the NetCDF file
    retval = nf90_close(ncid)
    if (retval /= NF90_NOERR) then
        print *, "Error closing file"
        stop
    end if

    call plots(4.,13.,9,"test.ps")
    ! Print a specific value from the array
    ! print *, array(9, 9, 1, 6, 201)
    call butler_psk(array(1,6,1,5:9,1:400),5,400,4.,-8.,0.,0.,20.,1.,'b2r',20,bpt1=10,contquan=10,conti=0.,continc=1.)
    call plote

end program reading_ncfiles