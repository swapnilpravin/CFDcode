module IBM

use settings, only : ny,nx,Ly,Lx,RADIUS,dt,rho,pi
use partitioner, only : m

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

		integer, dimension(ny,m) :: eta
		double precision :: yc,xc

		integer :: i	! time step

		double precision, parameter :: A = 0.033		! Oscillation amplitude
		double precision, parameter :: T = 3.2		! Oscillation time period
		double precision, parameter :: k = 0.0127	! side length of block (0.5 inch)
		double precision, parameter :: d = 2*k 	! seperation between adjascent blocks

		double precision, parameter :: omega = 2*pi/T
		double precision :: U_ib, V_ib

		! xc = x_ref, bottom left corner of first block as the reference point
		xc = Lx/4 + A*sin(pi/2+omega*i*dt)
		yc = 0

		U_ib = A*omega*cos(pi/2+omega*i*dt)
		V_ib = 0

		eta = 0
		!where ((x-xc)**2 + (y-yc)**2 < RADIUS**2 ) eta=1
		where ( ((x>=xc .and. x<=xc+k) .or. (x>=xc+k+d .and. x<=xc+2*k+d)) .and. &
			  ((y>=yc .and. y<=yc+k)) ) eta=1
	    
		Hx = eta*(U_ib-u(:,1:m))/(dt)
		Hy = eta*(V_ib-v(:,1:m))/(dt)


	end subroutine IBMforceOscillatingSteps
	

end module IBM
