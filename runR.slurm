#!/bin/bash
#SBATCH --job-name=ctl_lakes2nc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=32
#SBATCH --time=02:00:00
#SBATCH --account=drewgron0
#SBATCH --mem-per-cpu=4G
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=james.kessler@noaa.gov

module load Rgeospatial;
export R_LIBS_USER=$HOME/.local/lib/R
#./x_obs.R $casename &> obs_$casename.log
Rscript x_lakes2nc.R &> x2nc_ctl.log
