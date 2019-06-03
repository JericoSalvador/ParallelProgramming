import os


if __name__ == '__main__': 
	os.system('rm *.out *.err')
	os.system('mpif90 parallel.f90 -o gol-parallel')
	os.system('sbatch hb_mpi.cmd')
	os.system('ls')
