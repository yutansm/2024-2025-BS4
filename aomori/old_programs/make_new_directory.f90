program make_new_directory

USE IFPORT
LOGICAL(4) result
result = MAKEDIRQQ('testing')
! IF (result) THEN
!    WRITE (*,*) 'New subdirectory successfully created'
! ELSE
!    WRITE (*,*) 'Failed to create subdirectory'
! END IF

END program