program main

	use partitioner, only : point_t
	use IBM
	use settings
	implicit none

    type(point_t), dimension(4) :: polygon1
	type(point_t) :: p
	logical :: ans
	integer :: n_ib1
	double precision, dimension(:), allocatable :: x_ib1, y_ib1
	integer :: i

	!polygon1 = (/point_t(.75,.04), point_t(.80, .04), point_t(.80, .08), point_t(.75, .08)/)
	
	! Read IB points from 'ib.txt'

	open(unit=30,file='ib.txt',status='old',action='read')
	read (30,*) n_ib1

	allocate(x_ib1(n_ib1))
	allocate(y_ib1(n_ib1))

	do i=1,n_ib1
		read (30,*) x_ib1(i), y_ib1(i)
	end do
	close(30)
	
	do i=1,n_ib1
		polygon1(i) = point_t(x_ib1(i),y_ib1(i))
	end do
    
	p = point_t(.76, .05)
    call isInside(polygon1, n_ib1, p, ans)
	if (ans) then
		print*, 'Yes'
	else
		print*, 'No'
	end if
 
!     p = point_t(.9, .09)
! 	call isInside(polygon1, n, p, ans)
! 	if (ans) then
! 		print*, 'Yes'
! 	else
! 		print*, 'No'
! 	end if
 
end program

