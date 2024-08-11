#!/bin/bash
#============ Slurm Options ============
#SBATCH -p gr10291b
#SBATCH -t 168:00
#SBATCH --rsc p=1:t=1:c=10:m=30G
#SBATCH -o output.txt
#============ Shell Script ============
srun ./a.out
