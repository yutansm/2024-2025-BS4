program why
    use always
    implicit none
    real,dimension(15,12)::SSH
    character(len = 30)::stname
    ! interface
    ! subroutine SSH_data(SSH, ilabel)
    !     real, dimension(:,:), intent(out) :: SSH
    !     integer, intent(in) :: ilabel
    ! end subroutine SSH_data
    ! end interface

    ! call SSH_data(SSH,ilabel = 3603,convert = 1)
    call SSH_data(SSH,slabel='竜飛', convert=1)
    ! do y = 1, 15
    !     print*,y+2008,SSH(y,:)
    ! end do
do n = 1,3
    call plots2
    call ocenter
    ! call otops
    call symbolc(0.,0.,0.5,'psmonitor is amazing !!!',0.)

    call plote
end do


end program