module IBM

use settings, only : ny,nx,Ly,Lx,RADIUS,dt,rho,pi,n_ib,x_ib,y_ib
use partitioner, only : m, field, point_t

contains

	subroutine IBMforce(Hx,Hy,u,v,x,y)
	
	end subroutine IBMforce


	subroutine IBMforceCircle(Hx,Hy,u,v,x,y)

		double precision, dimension(ny,m) :: Hx,Hy,x,y
		double precision, dimension(ny,0:m+1) :: u,v

		integer, dimension(ny,m) :: eta
		double precision :: yc,xc

		yc = Ly/2
		xc = Lx/2

		eta = 0
		where ((x-xc)**2 + (y-yc)**2 < RADIUS**2 ) eta=1

		Hx = -eta*u(:,1:m)/(dt)
		Hy = -eta*v(:,1:m)/(dt)

	end subroutine IBMforceCircle

	subroutine IBMForceOscillatingCircle(Hx,Hy,u,v,x,y,i)
		! circle oscillating in the y direction
		! create a set of points on the circle to be tracked as it moves, recalculate eta 
		! at each step, and set force Hx, Hy using formula that takes circle speed into account

		double precision, dimension(ny,m) :: Hx,Hy,x,y
		double precision, dimension(ny,0:m+1) :: u,v

		integer, dimension(ny,m) :: eta
		double precision :: yc,xc

		integer :: i	! time step

		double precision, parameter :: A = 0.05		! Oscillation amplitude
		double precision, parameter :: T = 100.0		! Oscillation time period

		double precision, parameter :: omega = 2*pi/T
		double precision :: U_ib, V_ib

		xc = Lx/2
		yc = Ly/2 + A*sin(pi/2+omega*i*dt)

		U_ib = 0
		V_ib = A*omega*cos(pi/2+omega*i*dt)

		eta = 0
		where ((x-xc)**2 + (y-yc)**2 < RADIUS**2 ) eta=1

		Hx = eta*(U_ib-u(:,1:m))/(dt)
		Hy = eta*(V_ib-v(:,1:m))/(dt)


	end subroutine IBMforceOscillatingCircle
	
	
	subroutine IBMForceOscillatingSteps(Hx,Hy,u,v,x,y,i)
		! series of steps oscillating in the x direction
		! create a set of points on the circle to be tracked as it moves, recalculate eta 
		! at each step, and set force Hx, Hy using formula that takes circle speed into account

		double precision, dimension(ny,m) :: Hx,Hy,x,y
		double precision, dimension(ny,0:m+1) :: u,v

		!integer, dimension(ny,m) :: eta
		double precision :: yc,xc

		integer :: i	! time step

		double precision, parameter :: A = 0.033		! Oscillation amplitude
		double precision, parameter :: T = 3.2		! Oscillation time period
		double precision, parameter :: k = 0.0127	! side length of block (0.5 inch)
		double precision, parameter :: d = 5*k 	! seperation between adjascent blocks

		double precision, parameter :: omega = 2*pi/T
		double precision :: U_ib, V_ib

		! xc = x_ref, bottom left corner of first block as the reference point
		xc = Lx/4 + A*sin(pi/2+omega*i*dt)
		yc = 0

		U_ib = A*omega*cos(pi/2+omega*i*dt)
		V_ib = 0

		field%eta = 0
		!where ((x-xc)**2 + (y-yc)**2 < RADIUS**2 ) eta=1
		where ( ((x>=xc .and. x<=xc+k) .or. (x>=xc+k+d .and. x<=xc+2*k+d)) .and. &
			  ((y>=yc .and. y<=yc+k)) ) field%eta=1
	    
		Hx = field%eta*(U_ib-u(:,1:m))/(dt)
		Hy = field%eta*(V_ib-v(:,1:m))/(dt)


	end subroutine IBMforceOscillatingSteps


	subroutine IBMforcePolygon(Hx,Hy,u,v,x,y,tstep)

		!use settings, only: n_ib, x_ib, y_ib
		implicit none

		double precision, dimension(ny,m) :: Hx,Hy,x,y
		double precision, dimension(ny,0:m+1) :: u,v
		integer :: tstep	! time step

		logical :: ans
		type(point_t) :: p
		type(point_t), dimension(n_ib) :: polygon

		integer :: i,j,k
		
		double precision, parameter :: A = 0.033		! Oscillation amplitude
		double precision, parameter :: T = 3.2		! Oscillation time period
		
		double precision, parameter :: omega = 2*pi/T
		double precision, dimension(n_ib) :: x_ib_t, y_ib_t
		double precision :: U_ib, V_ib
		
		x_ib_t = x_ib + A*sin(pi/2+omega*tstep*dt)
		y_ib_t = y_ib
		
		U_ib = A*omega*cos(pi/2+omega*tstep*dt)
		V_ib = 0

		do k=1,n_ib
			polygon(k) = point_t(x_ib_t(k), y_ib_t(k))
		end do

		field%eta = 0 !(Ny,m)
		do i=1,Ny
			do j=1,m
				p = point_t(x(i,j), y(i,j))
				call isInside(polygon, n_ib, p, ans)
				if (ans .eqv. .true.) then
					field%eta(i,j) = 1
				end if
			end do
		end do

		Hx = field%eta*(U_ib-u(:,1:m))/(dt)
		Hy = field%eta*(V_ib-v(:,1:m))/(dt)

	end subroutine IBMforcePolygon


	subroutine onSegment(p, q, r, ans)

		implicit none
		type(Point_t) :: p, q, r
		logical :: ans

	    if (q%x <= max(p%x, r%x) .and. q%x >= min(p%x, r%x) .and. &
            q%y <= max(p%y, r%y) .and. q%y >= min(p%y, r%y)) then
			ans = .true.
			return
		end if
		ans = .false.
	end subroutine onSegment
	
	! To find orientation of ordered triplet (p, q, r).
	! The function returns following values
	! 0 --> p, q and r are colinear
	! 1 --> Clockwise
	! 2 --> Counterclockwise
	subroutine orientation(p, q, r, ans)
	
		implicit none
		type(point_t) :: p, q, r
		integer :: ans
		double precision :: val			!! The geeksforgeeks.org website has 'int' data type here, which is a mistake !!

		!print*, p, q, r
		!print*, (q%y-p%y)*(r%x-q%x), (q%x-p%x)*(r%y-q%y)
		
		val = (q%y-p%y)*(r%x-q%x)-(q%x-p%x)*(r%y-q%y)
		
		!print*, val
	 
		if (val == 0) then
			ans = 0  ! colinear
			return 
		end if
	 
		if (val>0) then ! clock or counterclock wise
			ans = 1
		else
			ans = 2
		end if
	end subroutine orientation

	subroutine doIntersect(p1, q1, p2, q2, ans)

		implicit none
		type(point_t) :: p1, q1, p2, q2
		integer :: o1, o2, o3, o4
		logical :: ans
		logical :: temp

		! Find the four orientations needed for general and
		! special cases
		call orientation(p1, q1, p2, o1)
		call orientation(p1, q1, q2, o2)
		call orientation(p2, q2, p1, o3)
		call orientation(p2, q2, q1, o4)

		! General case
		if (o1 /= o2 .and. o3 /= o4) then
			ans = .true.
			return
		end if

		! Special Cases
		! p1, q1 and p2 are colinear and p2 lies on segment p1q1
		call onSegment(p1, p2, q1, temp)
		if (o1 == 0 .and. temp) then
			ans = .true.
			return
		end if

		! p1, q1 and q2 are colinear and q2 lies on segment p1q1
		call onSegment(p1, q2, q1, temp)
		if (o2 == 0 .and. temp) then
			ans = .true.
			return 
		end if

		! p2, q2 and p1 are colinear and p1 lies on segment p2q2
		call onSegment(p2, p1, q2, temp)
		if (o3 == 0 .and. temp) then
			ans = .true.
			return
		end if

		! p2, q2 and q1 are colinear and q1 lies on segment p2q2
		call onSegment(p2, q1, q2, temp)
		if (o4 == 0 .and. temp) then
			ans = .true.
			return
		end if

		ans = .false. ! Doesn't fall in any of the above cases
	end subroutine doIntersect

	! Returns true if the point p lies inside the polygon[] with n vertices
	subroutine isInside(polygon, n, p, ans)

		implicit none
		type(point_t), dimension(:) :: polygon
		integer :: n
		type(point_t) :: p
		logical :: ans

		integer, parameter :: INF = 10000
		type(point_t) :: extreme 
		integer :: i, count, next
		logical :: temp1, temp3
		integer :: temp2
		

		! There must be at least 3 vertices in polygon[]
		if (n < 3) then
			ans = .false.
			return
		end if

		! Create a point for line segment from p to infinite
		extreme%x = INF
	   	extreme%y = p%y

		! Count intersections of the above line with sides of polygon
		count = 0
		do i=1,n
			next = mod(i,n)+1
			
			!print*, i, next

			! Check if the line segment from 'p' to 'extreme' intersects
			! with the line segment from 'polygon[i]' to 'polygon[next]'
			call doIntersect(polygon(i), polygon(next), p, extreme, temp1)
			if (temp1) then
			
				! If the point 'p' is colinear with line segment 'i-next',
				! then check if it lies on segment. If it lies, return true,
				! otherwise false
				call orientation(polygon(i), p, polygon(next), temp2)
				!print*, p, polygon(i), polygon(next)
				if (temp2 == 0) then
					!print*, 'collinear'
					call onSegment(polygon(i), p, polygon(next), temp3)
					!print*, temp3
					ans = temp3
					return
				end if
				
				! Check if the ray p-extreme passes through one of the vertices i or next.
				! If the intersection point is a vertex of a tested polygon side, 
				! then the intersection counts only if the second vertex of the side 
				! lies below the ray. This is effectively equivalent to considering 
				! vertices on the ray as lying slightly above the ray.
				call onSegment(p, polygon(i), extreme, temp3)
				if (temp3 .and. polygon(next)%y>p%y) cycle
				call onSegment(p, polygon(next), extreme, temp3)
				if (temp3 .and. polygon(i)%y>p%y) cycle

				count = count + 1
			end if
			!print*, count
		end do
		
		!print*, count

		! Return true if count is odd, false otherwise
		if (mod(count,2) == 0) then
			ans = .false.
		else 
			ans = .true.
		end if
	end subroutine isInside 

end module IBM
