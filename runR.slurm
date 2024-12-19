#!/bin/bash
casename=$1
varname=$2

sbatch << EOT
#!/bin/bash
#“#SBATCH” directives that convey submission options:

#SBATCH --job-name=${casename}_$varname
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=32
#SBATCH --time=12:00:00
#SBATCH --account=drewgron1
#SBATCH --mem-per-cpu=4G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=james.kessler@noaa.gov

module load Rgeospatial;
export R_LIBS_USER=$HOME/.local/lib/R
t0=$(date +%s)
#./x_obs.R $casename &> obs_$casename.log
./x_lakewide.R $casename $varname &> log/lw_$varname.$casename
tf=$(date +%s)

# calculate run time length
#hrs=$(echo "scale=3; $(( tf - t0 ))/3600" | bc)
#echo run took: $hrs hours >> obs_$casename.log

EOT
