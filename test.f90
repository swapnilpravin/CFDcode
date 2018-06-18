program main

	use partitioner, only : point_t
	use IBM
	implicit none

    type(point_t), dimension(4) :: polygon1
	type(point_t) :: p
	logical :: ans
	integer, parameter :: n = 4

	polygon1 = (/point_t(.75,.04), point_t(.80, .04), point_t(.80, .08), point_t(.75, .08)/)
    
	p = point_t(.8, .06)
    call isInside(polygon1, n, p, ans)
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

