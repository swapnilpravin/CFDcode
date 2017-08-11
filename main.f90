program main

    use mpi
    use settings
    use partitioner
    use io, only : printHeader

	implicit none

    integer :: ierr, Nproc, id

	integer :: sub_size, sub_id

    call mpi_init(ierr)
    call mpi_comm_rank(MPI_COMM_WORLD, id, ierr)
    call mpi_comm_size(MPI_COMM_WORLD, Nproc, ierr)

    if (Nproc==1) then
        print*, ''
        print*, 'Error: Run with at least three processes!'
        print*, ''
		!call mpi_finalize(ierr)
        call exit()
    end if

	! assign colors for master and salve comms
	if (id == 0) then
		color = MASTER
	else
		color = SLAVE
	end if

	! create new communicators
	call mpi_comm_split(MPI_COMM_WORLD, color, id, sub_comm, ierr)

	call mpi_comm_rank(sub_comm, sub_id, ierr)
	call mpi_comm_size(sub_comm, sub_size, ierr)

	call setup('setup.txt')

	if (color == MASTER) print*, 'SETUP DONE'

    !call mpi_barrier(MPI_COMM_WORLD,ierr)

    if (color == MASTER) then
        call printHeader()
    end if

    call setPartitions()
	
	call channel()

    call mpi_finalize(ierr)

end program main
