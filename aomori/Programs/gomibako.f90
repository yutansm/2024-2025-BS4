program linear_transfromations
    use always
    implicit none 
    real,dimension(:,:),allocatable::bath

    ! call readGEBCO('/Users/yuta/LABWORK/2024-2025-BS4/aomori/Data/GEBCO/GEBCO_2024.nc',bath,120,145,20,45,info = .true.)

    call plots2(nnfile = 'GEBCO',oopt = 'obottoms',mode = 'portrait', y = 3.)

    call GEBCOmap(123,150,24,46,18.,symbols = .true.,symbol_size = 0.7,paintland = .true.,paintsea = .true.,HIRES = .true.)

    call plote



end program 