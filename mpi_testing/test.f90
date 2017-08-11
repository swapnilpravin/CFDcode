program main

	use mpi

	implicit none

	integer :: ierr, Nproc, id, color

	integer, parameter :: COLOR_MASTER = 0
	integer, parameter :: COLOR_SLAVE = 1

	! communicator
	integer :: sub_comm

	integer :: sub_size, sub_id 

	call mpi_init(ierr)

	! Plan:
	!
	! create a new communicator
	! assign processors to it?
	! then enquire the rank and size of the communicator
	! possibly, use one communicator as master, use it for graphical display
	! and use the other comm for computing

	call mpi_comm_rank(MPI_COMM_WORLD, id, ierr)
	call mpi_comm_size(MPI_COMM_WORLD, Nproc, ierr)

	! assign color
	if (id==0) then
		color = COLOR_MASTER
	else
		color = COLOR_SLAVE
	end if

	call mpi_comm_split(MPI_COMM_WORLD, color, id, sub_comm, ierr)

	call mpi_comm_rank(sub_comm, sub_id, ierr)
	call mpi_comm_size(sub_comm, sub_size, ierr)

	print*, 'world rank/size', id, Nproc, 'sub_comm rank/size', sub_id, sub_size

	call mpi_finalize(ierr)


end program main
