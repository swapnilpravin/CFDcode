#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

int main(int argc, char** argv)
{
	int ierr, Nproc, id;
	MPI_Comm* comm;

	ierr = MPI_Init(&argc, &argv);
	ierr = MPI_Comm_rank(MPI_COMM_WORLD, &id);
	ierr = MPI_Comm_size(MPI_COMM_WORLD, &Nproc);

	ierr = MPI_Comm_split(MPI_COMM_WORLD, id!=0, id, comm);

	if (Nproc==1)
	{
		printf("\nError: This is a parallel program. Select at least two processors!\n\n");
		exit(0);
	}

	setup("setup.txt");

	if (id==0)
		printheader();
	
	setpartitions();

	channel();

	//fort_main();

	MPI_Comm_free(comm);
	ierr = MPI_Finalize();

	return 0;
}
