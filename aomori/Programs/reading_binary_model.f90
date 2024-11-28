program read_binary_file
    use always
    implicit none
    integer, parameter :: nDims = 2
    integer, parameter :: dim1 = 62, dim2 = 62
    real(kind=4), dimension(dim1, dim2) :: data_array
    integer :: unit_number, ios

    call openlog(omit = .true.)

    ! Open the binary file
    unit_number = 10
    open(unit_number, file='../MITgcm/verification/tutorial_barotropic_gyre/run/Eta.0000077760.001.001.data', form='unformatted', status='old', access='direct',recl = dim1*dim2*4,convert = 'big_endian')

    ! Read the data from the file
    read(unit_number, rec=1, iostat=ios) data_array

    ! Close the file
    close(unit_number)

    ! Print the data to verify (optional)
    do i = 1, dim1
        do j = 1, dim2
            write(tolog,*) 'data_array(', i, ',', j, ') = ', data_array(i, j)
        end do
    end do

    call closelog

    call plots2(oopt = 'obottoms')
    call butler_cont(data_array, dim1, dim2, 10., 10., 0., -0.1, 0.01, 5)
    call plot(12.,0.,-3)
    call create_map(38,43,137,142,1,6,3,9.,0.6)
    call plote

end program read_binary_file