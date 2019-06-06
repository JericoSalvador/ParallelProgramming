#!/bin/bash

#SBATCH -p Course 
#SBATCH -J gol-parallel
#SBATCH -e gol%j.err
#SBATCH -o gol%j.out
#SBATCH -N 1
#SBATCH -n 2
#SBATCH -t 00:10:00

mpirun -np 2 ./gol-parallel 
