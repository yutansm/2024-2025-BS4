program hakidame
    use always
    real::u(10,10),v(10,10)

    do i = 1, 10
        do j = 1, 10
            u(i,j) = real(i);print*,u(i,j)
            v(i,j) = real(j);print*,v(i,j)
        end do
    end do

    call plots2(oopt = 'obottoms')
    call butler_vector(u,v,10.,10.)
    call plote



end program 
