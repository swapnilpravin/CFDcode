program main

	implicit none

	integer :: ierr, Nproc, id

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

end program main
