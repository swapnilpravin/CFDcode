module io

use mpi
use settings, only: nx, ny, Lx, Ly, dt, n_mon, x_mon, y_mon
use partitioner, only: mesh, m, field
implicit none

contains
	subroutine printHeader
	
		character(len=100), parameter :: hline = '-----------------------------------------------------------------'
		integer,dimension(8) :: values

		call date_and_time(VALUES=values)
		
		print*, hline
		print*, 'CFD Simulation Program'
		print 150, 'Start time: ',values(1),'/',values(2),'/',values(3),' ',values(5),':',values(6),':', values(7) 
		150 format(A,I4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2)
		print*, hline

		100 format(A20,'(',I0, ' x ',I0, ')')
		print 100, 'Mesh size: ', nx, ny
		101 format(A20,ES10.2)
		print 101, 'Timestep: ',dt
		103 format(A20, ES10.2)
		print 103, 'Channel length: ', Lx
		print 103, 'Channel height: ', Ly
	
		print*, hline
		
		print*, ''
		print*, 'Starting Run'
		print*, ''
	
	end subroutine printHeader
		
	
	
	subroutine writeFlowDataToFile(u,v,w,P,timestamp)
	
	double precision, dimension(:,:,:) :: u,v,w,P
	integer :: timestamp
	
	character(len=100) :: u_file, v_file, w_file, p_file
	integer :: n
	
	n = size(u)
	
	100 format(A2,I0.10,A4)
	write(u_file,100) 'u_',timestamp,'.dat'
	write(v_file,100) 'v_',timestamp,'.dat'
	write(w_file,100) 'w_',timestamp,'.dat'
	write(p_file,100) 'p_',timestamp,'.dat'
	
	
	open(unit=10,file=u_file,status='replace',action='write')
	open(unit=20,file=v_file,status='replace',action='write')
	open(unit=30,file=w_file,status='replace',action='write')
	open(unit=40,file=p_file,status='replace',action='write')
	
	write(10,*) reshape(u,(/n,1/))
	write(20,*) reshape(v,(/n,1/))
	write(30,*) reshape(w,(/n,1/))
	write(40,*) reshape(p,(/n,1/))
	
	close(10)
	close(20)
	close(30)
	close(40)
	
	write(*,*) '--> Data written to file.'
	write(*,*)
	
	end subroutine writeFlowDataToFile


	!-----------------------------------
	! gatherVars: allocate complete arrays on root and gather data from all
	! procs on root
	!----------------------------------
	subroutine gatherVars(x,y,eta,u,v,P,x_all,y_all,eta_all,u_all,v_all,P_all)
		double precision, dimension(ny,1:m) :: x,y,eta
		double precision, dimension(ny,0:m+1) :: u,v,P	
		double precision, dimension(:,:), allocatable ::x_all, y_all, eta_all, u_all, v_all, P_all ! all gathered data (only at root)
        integer, dimension(:), allocatable :: recvcounts, displs ! only at root
		integer :: id, Nproc, ierr
		integer :: i

        call mpi_comm_rank(MPI_COMM_WORLD,id,ierr)
        call mpi_comm_size(MPI_COMM_WORLD,Nproc,ierr)
	

		if (id==0) then
            allocate(x_all(ny,nx))
            allocate(y_all(ny,nx))
            allocate(eta_all(ny,nx))
            allocate(u_all(ny,nx))
            allocate(v_all(ny,nx))
            allocate(P_all(ny,nx))

            allocate(recvcounts(Nproc))
            allocate(displs(Nproc))
        end if

        call mpi_gather(m,1,MPI_INTEGER,recvcounts,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

        if (id==0) then
            recvcounts = ny*recvcounts
            displs(1) = 0
            do i=2,Nproc
                displs(i) = sum(recvcounts(1:i-1))
            end do
        end if

        call mpi_gatherv(x,ny*m,MPI_DOUBLE_PRECISION,x_all,recvcounts,displs,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
        call mpi_gatherv(y,ny*m,MPI_DOUBLE_PRECISION,y_all,recvcounts,displs,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
        call mpi_gatherv(eta,ny*m,MPI_DOUBLE_PRECISION,eta_all,recvcounts,displs,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
        call mpi_gatherv(u(:,1:m),ny*m,MPI_DOUBLE_PRECISION,u_all,recvcounts,displs,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
        call mpi_gatherv(v(:,1:m),ny*m,MPI_DOUBLE_PRECISION,v_all,recvcounts,displs,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
        call mpi_gatherv(P(:,1:m),ny*m,MPI_DOUBLE_PRECISION,P_all,recvcounts,displs,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
		end subroutine gatherVars


	
    !---------------------------------------------
    ! writeToTecplot2D_MPI
    !---------------------------------------------
    subroutine writeToTecplot2D_MPI(x,y,u,v,P,eta,timestamp)
		implicit none
		
		double precision, dimension(ny,1:m) :: x,y,eta
		double precision, dimension(ny,0:m+1) :: u,v,P
		integer :: timestamp

		integer :: imax, jmax
		double precision, dimension(:,:), allocatable ::x_all, y_all, eta_all, u_all, v_all, P_all ! all gathered data (only at root)
		character(30) :: filename

        integer :: id, Nproc, ierr

        call mpi_comm_rank(MPI_COMM_WORLD,id,ierr)
        call mpi_comm_size(MPI_COMM_WORLD,Nproc,ierr)
	
		imax = ny; jmax=nx

		call gatherVars(x,y,eta,u,v,P,x_all,y_all,eta_all,u_all,v_all,P_all)
        
        if (id==0) then
		
            ! WRITE FLOW DATA	
            100 format(A5,I0.10,A4)
            write(filename,100) 'data_',timestamp,'.tec'
        
            open(unit=10,file=filename,status='replace',action='write')
            write(10,*) 'VARIABLES = "X","Y","eta","U","V","P"'
            write(10,*) 'ZONE DATAPACKING=BLOCK, I=', imax, ', J=', jmax
            write(10,*) 'STRANDID=1, SOLUTIONTIME=',timestamp*dt
            
            write(10,*) x_all
            write(10,*) y_all
            write(10,*) eta_all
            write(10,*) u_all
            write(10,*) v_all
            write(10,*) P_all

        
            close(10)
        
            
            write(*,*)		
            write(*,'(A35,A20,A2,A25)') '--> Data written to file: ',filename
            write(*,*)
            
        end if
	
	end subroutine writeToTecplot2D_MPI

	!-----------------------------------------------------------
	! writeImage: Write a greyscale image for colormap of velocity magnitude
	!-----------------------------------------------------------
	subroutine writeImage(x,y,u,v,P,eta,timestamp)
		double precision, dimension(ny,1:m) :: x,y,eta
		double precision, dimension(ny,0:m+1) :: u,v,P
		integer :: timestamp
		
		integer :: imax, jmax
		double precision, dimension(:,:), allocatable ::x_all, y_all, eta_all, u_all, v_all, P_all ! all gathered data (only at root)
		double precision, dimension(:,:), allocatable ::umag_all ! umag (only at root)
		integer, dimension(:,:), allocatable :: img	! pixel data for image
		double precision :: umag_max	! for normalization
		character(30) :: filename

        integer :: id, Nproc, ierr
		integer,parameter :: nlevels = 100, px_max=1000
		integer :: i,j, i_up, j_up, n_box
		logical :: down_sample

        call mpi_comm_rank(MPI_COMM_WORLD,id,ierr)
        call mpi_comm_size(MPI_COMM_WORLD,Nproc,ierr)
	
		if (max(nx,ny)>px_max) then 
			down_sample = .true. 
		else 
			down_sample = .false.
		end if

		call gatherVars(x,y,eta,u,v,P,x_all,y_all,eta_all,u_all,v_all,P_all)
        
        if (id==0) then
			if(.not. allocated(umag_all)) allocate(umag_all(ny,nx))
			umag_all = sqrt(u_all**2+v_all**2)	
			where(eta_all .eq. 1) umag_all = 0	! inside immersed boundary
			umag_max = maxval(umag_all)
			umag_all = umag_all/umag_max	! Normalize umag_all

			if (down_sample) then
				if (nx>ny) then
					jmax = px_max
					imax = int(dble(ny)/dble(nx)*px_max)
				else
					imax = px_max
					jmax = int(dble(nx)/dble(ny)*px_max)
				end if
				if(.not. allocated(img)) allocate(img(imax,jmax))
				n_box = int(dble(ny)/dble(imax))+2
				do i=1,imax
					do j=1,jmax
						i_up = int(dble(i)/imax*ny); j_up = int(dble(j)/jmax*nx)
						img(i,j) = sum(umag_all(i_up-n_box/2:i_up+n_box/2,j_up-n_box/2:j_up+n_box/2))/(n_box+1)**2*nlevels
					end do
				end do
			else
				imax = ny; jmax = nx
				if(.not. allocated(img)) allocate(img(imax,jmax))
				img = int(umag_all*nlevels)
			end if

			!print*, imax, jmax

			100 format(A5,I0.10,A4)
            write(filename,100) 'umag_',timestamp,'.pgm'	! greyscale image
       
            open(unit=10,file=filename,status='replace',action='write')
			write(10,'(A)') 'P2'
			write(10,'(A)') '# u_mag greyscale image in PGM format'
			write(10,'(I0,A,I0)') jmax,' ',imax
			write(10,'(I0)') nlevels
			do i=imax,1,-1
				do j=1,jmax
					write(10,'(I0,A)',advance='no') img(i,j), ' '
				end do
				write(10,*) 	! new line
			end do

			close(10)
			write(*,*)		
			write(*,'(A35,A20,A2,A25)') '--> Image written to file: ',filename
			write(*,*)
		end if
			
		end subroutine writeImage


    !--------------------------------------------------------------
    ! writeToBOV_MPI:
    !--------------------------------------------------------------
	subroutine writeToBOV_MPI(u,v,P,timestamp)
		
		double precision, dimension(ny,0:m+1) :: u,v,P
		integer :: timestamp

		character(30) :: filename

        integer :: id,Nproc,ierr

        call mpi_comm_rank(MPI_COMM_WORLD,id,ierr)

        ! write BOV header file



		! WRITE FLOW DATA	
		
        
        100 format(A,I0,A)
		write(filename,100) 'u_',id,'.dat'
	
		open(unit=10,file=filename,status='replace',action='write', form='unformatted')
        write(10) u
        close(10)
	
		write(filename,100) 'v_',id,'.dat'
	
		open(unit=10,file=filename,status='replace',action='write', form='unformatted')
        write(10) v
        close(10)

        print*, 'data written from ', id
	
	end subroutine writeToBOV_MPI


    !---------------------------------------------------------------------------------------------
    ! writeLogToTerminal:
    ! write requested data to terminal (serial version)
    !---------------------------------------------------------------------------------------------
	subroutine writeLogToTerminal(timestep,p_res,p_err,NITS,Umax,Vmax,Wmax,Pmax,V1_name,V1,V2_name,V2,V3_name,V3,V4_name,V4,V5_name,V5)
	
	integer :: timestep
	double precision :: p_res,p_err
	integer :: NITS									! number of iterations for pressure equation to converge
	double precision :: Umax,Vmax,Wmax,Pmax				! flow variables

	! (optional additional variables to write to terminal)
	character(len=*),optional :: V1_name, V2_name, V3_name, V4_name, V5_name
	double precision, optional :: V1,V2,V3,V4,V5
	
	integer :: i
	
	character(len=100), parameter :: hline = '-----------------------------------------------------------------'
	
	100 format (A1,A25,A1,I10,A30)
	101 format (A1,A25,A1,ES10.2,A30)
	102 format (A1,I10,A2,3ES10.2,A2,3ES10.2,A2)
	
	print *, hline
	print '(A1,A15,I8,A25,ES10.2,A2,A6)', '|','Timestep = ',timestep,' Simulation time = ', timestep*dt, ' s', '|'
	print *, hline
	print 100, '|','Pressure Iterations', '|',NITS, '|'
	print 101, '|','Pressure Error', '|',p_err, '|'
	print *, hline

	! optional variables to be written to terminal
	if (present(V1)) then
		print 101, '|',V1_name, '|',V1, '|'
	end if
	if (present(V2)) then
		print 101, '|',V2_name, '|',V2, '|'
	end if
	if (present(V3)) then
		print 101, '|',V3_name, '|',V3, '|'
	end if
	if (present(V4)) then
		print 101, '|',V4_name, '|',V4, '|'
	end if
	if (present(V5)) then
		print 101, '|',V5_name, '|',V5, '|'
	end if

	print *, hline
	print 101, '|','U_max', '|', Umax, '|'
	print 101, '|','V_max', '|', Vmax, '|'
	print 101, '|','W_max', '|', Wmax, '|'
	print 101, '|','P_max', '|', Pmax, '|'
    print *, hline
	!print*, ''
	
	
	end subroutine writeLogToTerminal



    !------------------------------------------------------------
    ! writeLogToTerminal_MPI:
    ! write requested data to terminal (parallel)
    !-----------------------------------------------------------
    subroutine writeLogToTerminal_MPI(timestep, &
    V1_name,V1_type,V1, &
    V2_name,V2_type,V2, &
    V3_name,V3_type,V3, &
    V4_name,V4_type,V4, &
    V5_name,V5_type,V5, &
	V6_name,V6_type,V6, &
	V7_name,V7_type,V7, &
	V8_name,V8_type,V8, &
	V9_name,V9_type,V9, &
    V10_name,V10_type,V10, &
	V11_name,V11_type,V11)
	
	integer :: timestep

	! (optional variables to write to terminal)
	character(len=*),optional :: V1_name, V2_name, V3_name, V4_name, V5_name, V6_name, V7_name, V8_name, V9_name, V10_name, V11_name
	character(len=*),optional :: V1_type, V2_type, V3_type, V4_type, V5_type, V6_type, V7_type, V8_type, V9_type, V10_type, V11_type ! must be 'integer' or 'double'
	double precision, optional :: V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11
	
	integer :: i
	
    character(len=100), parameter :: error_msg = 'Error (writeToTerminal): type must be "integer" or "double"'
	character(len=100), parameter :: hline = '-----------------------------------------------------------------'
	
	101 format (A1,A25,A1,I10,A30)
	102 format (A1,A25,A1,ES10.2,A30)
	
	print *, hline
	print '(A1,A15,I8,A25,ES10.2,A2,A6)', '|','Timestep = ',timestep,' Simulation time = ', timestep*dt, ' s', '|'
	print *, hline

	! optional variables to be written to terminal
	if (present(V1)) then
        if (V1_type .eq. 'integer') then
		    print 101, '|',V1_name, '|',int(V1), '|'
        else if (V1_type .eq. 'double') then
		    print 102, '|',V1_name, '|',V1, '|'
        else
            print *, error_msg
        end if
	end if
    
    if (present(V2)) then
        if (V2_type .eq. 'integer') then
		    print 101, '|',V2_name, '|',int(V2), '|'
        else if (V2_type .eq. 'double') then
		    print 102, '|',V2_name, '|',V2, '|'
        else
            print *, error_msg
        end if
	end if
	
    if (present(V3)) then
        if (V3_type .eq. 'integer') then
		    print 101, '|',V3_name, '|',int(V3), '|'
        else if (V3_type .eq. 'double') then
		    print 102, '|',V3_name, '|',V3, '|'
        else
            print *, error_msg
        end if
	end if
    
    if (present(V4)) then
        if (V4_type .eq. 'integer') then
		    print 101, '|',V4_name, '|',int(V4), '|'
        else if (V4_type .eq. 'double') then
		    print 102, '|',V4_name, '|',V4, '|'
        else
            print *, error_msg
        end if
	end if
    
    if (present(V5)) then
        if (V5_type .eq. 'integer') then
		    print 101, '|',V5_name, '|',int(V5), '|'
        else if (V5_type .eq. 'double') then
		    print 102, '|',V5_name, '|',V5, '|'
        else
            print *, error_msg
        end if
	end if
	
	if (present(V6)) then
        if (V6_type .eq. 'integer') then
		    print 101, '|',V6_name, '|',int(V6), '|'
        else if (V6_type .eq. 'double') then
		    print 102, '|',V6_name, '|',V6, '|'
        else
            print *, error_msg
        end if
	end if
	
	if (present(V7)) then
        if (V7_type .eq. 'integer') then
		    print 101, '|',V7_name, '|',int(V7), '|'
        else if (V7_type .eq. 'double') then
		    print 102, '|',V7_name, '|',V7, '|'
        else
            print *, error_msg
        end if
	end if
	
	if (present(V8)) then
        if (V8_type .eq. 'integer') then
		    print 101, '|',V8_name, '|',int(V8), '|'
        else if (V8_type .eq. 'double') then
		    print 102, '|',V8_name, '|',V8, '|'
        else
            print *, error_msg
        end if
	end if
	
	if (present(V9)) then
        if (V9_type .eq. 'integer') then
		    print 101, '|',V9_name, '|',int(V9), '|'
        else if (V9_type .eq. 'double') then
		    print 102, '|',V9_name, '|',V9, '|'
        else
            print *, error_msg
        end if
	end if
	
	if (present(V10)) then
        if (V10_type .eq. 'integer') then
		    print 101, '|',V10_name, '|',int(V10), '|'
        else if (V10_type .eq. 'double') then
		    print 102, '|',V10_name, '|',V10, '|'
        else
            print *, error_msg
        end if
	end if

	if (present(V11)) then
        if (V11_type .eq. 'integer') then
		    print 101, '|',V11_name, '|',int(V11), '|'
        else if (V11_type .eq. 'double') then
		    print 102, '|',V11_name, '|',V11, '|'
        else
            print *, error_msg
        end if
	end if

    ! draw a line if at least one additional info was printed
    if (present(V1)) print *, hline

    print*, ''
		
	end subroutine writeLogToTerminal_MPI



    
    !-------------------------------------
    ! write1DArrayToFile
    ! MAKE MPI VERSION
    !-------------------------------------
    subroutine write1DArrayToFile(array,filename)
        implicit none
        
        !Dummy vars
        character (len=*) :: filename
        double precision, dimension(:) :: array
        ! local vars
        integer :: N
        integer :: i
        
        open(unit=10, file=filename, status='replace', action='write')
        
        N = size(array)
        do i=1,N
            write(10,*) array(i)
        enddo
        close(10)
        print *, filename, " written to disk."
    end subroutine write1DArrayToFile



    !--------------------------------------------
    ! write2DArrayToFile
    ! MAKE MPI VERSION
    !--------------------------------------------
    subroutine write2DArrayToFile(array,filename)
        implicit none
        
        !Dummy vars
        character (len=*) :: filename
        double precision, dimension(:,:) :: array
        ! local vars
        integer, dimension(1:2) :: N
        integer :: i, j
        
        open(unit=10, file=filename, status='replace', action='write')
        
        N = shape(array)
        do i=1,N(1)
            write(10,*) ( array(i,j), j = 1,N(2) )
        enddo
        close(10)
        print *, filename, " written to disk."
    end subroutine write2DArrayToFile

	subroutine checkSolverError(Umax, Pmax)
		
		double precision :: Umax, Pmax
		integer :: id, Nproc, ierr
		
		call mpi_comm_rank(MPI_COMM_WORLD,id,ierr)
		call mpi_comm_size(MPI_COMM_WORLD,Nproc,ierr)

		if (id==0) then
		! NaN is not equal to anything, not even itself!
			if (Umax /= Umax .or. Pmax /= Pmax) then
				print*, ''
				print*, 'Error: Solver generated NaNs'
				print*, ''
				call exit()
			end if
		end if
	end subroutine checkSolverError

	subroutine retrieveMonData(mon_data)
		integer :: i
		integer :: id, Nproc, ierr
		integer :: nx_loc
		integer :: i_x, i_y

		double precision :: x_min, x_max
		double precision, dimension(3) :: mon_data_loc	! u,v,p
		double precision, dimension(n_mon,3) :: mon_data	! u,v,p per row

		integer, dimension(MPI_STATUS_SIZE) :: status

		call mpi_comm_rank(MPI_COMM_WORLD, id, ierr)

		x_min = minval(mesh%x)
		x_max = maxval(mesh%x)
		nx_loc = size(mesh%x,2)

		if (id/=0) then	
		do i=1,n_mon 
			if (x_mon(i)>=x_min .and. x_mon(i)<=x_max) then
				i_x = int(nx_loc*(x_mon(i)-x_min)/(x_max-x_min))
				i_y = int(ny*y_mon(i)/Ly)
				mon_data_loc = (/ field%u(i_y,i_x), field%v(i_y,i_x), field%p(i_y,i_x) /)
				!print*, mon_data_loc
			
				call mpi_send(mon_data_loc,3,MPI_DOUBLE_PRECISION,0,100+i,MPI_COMM_WORLD,ierr)
				!end if
				!print*, 'mon point', i, 'sent from', id
			end if
		end do
		end if

		if (id==0) then
			do i=1,n_mon
				if (x_mon(i)>=x_min .and. x_mon(i)<=x_max) then
					i_x = int(nx_loc*(x_mon(i)-x_min)/(x_max-x_min))
					i_y = int(ny*y_mon(i)/Ly)
					mon_data_loc = (/ field%u(i_y,i_x), field%v(i_y,i_x), field%p(i_y,i_x) /)
					mon_data(i,:) = mon_data_loc
				else
					call mpi_recv(mon_data(i,:),3,MPI_DOUBLE_PRECISION,MPI_ANY_SOURCE,100+i,MPI_COMM_WORLD,status,ierr)
				end if
			end do
		end if

		!if (id==0) print*, mon_data
	
		!call mpi_barrier(MPI_COMM_WORLD,ierr)
		call mpi_bcast(mon_data,n_mon*3,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)

	end subroutine retrieveMonData


end module io
