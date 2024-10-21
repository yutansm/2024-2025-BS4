program create_netcdf
    use netcdf
    use constants
    implicit none
    integer::ncid,varid,status,dimids(5)
    call calibrated_data51(potemp_c5,sal_c5)

    status = nf90_create("potemp_c5.nc",NF90_CLOBBER,ncid)
    if(status /= NF90_NOERR) then
        print*,"Error creating file"
        stop
    end if

    status = nf90_def_dim(ncid,'Years',years,dimids(1))
    status = nf90_def_dim(ncid,'Months',months,dimids(2))
    status = nf90_def_dim(ncid,'Lines',lines,dimids(3))
    status = nf90_def_dim(ncid,'Stations',stations,dimids(4))
    status = nf90_def_dim(ncid,'Depth',depth,dimids(5))

    status = nf90_def_var(ncid,'potemp_c5',NF90_FLOAT,dimids,varid)
    status = nf90_enddef(ncid)
    status = nf90_put_var(ncid,varid,potemp_c5)
    status = nf90_close(ncid)

end program