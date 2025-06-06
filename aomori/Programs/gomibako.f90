program check
    use always
    
    call plots2(nnfile = 'test')

    call arrow(0.,0.,2.,0.,rangle = 45.)
    call arrow(0.,0.,2.,0.,rangle = 30.)
    call arrow(0.,0.,2.,0.,rangle = 60.)

    call box(5.,5.)
    call plote

end program