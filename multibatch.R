# Calculate the obs pressure anomalies on SPICE

for(year in seq(1800,1870,1)) {
  for(month in seq(1,12)) {
      sink('multistart.auto.slm')
      cat('#!/bin/ksh -l\n')
      cat('#SBATCH --qos=normal\n')
      cat('#SBATCH --output=/scratch/hadpb/slurm_output/bias_%j.out\n')
      cat('#SBATCH --mem=10000\n')
      cat('#SBATCH --ntasks=1\n')
      cat('#SBATCH --ntasks-per-core=1\n')
      cat('#SBATCH --time=5\n')
      cat(sprintf("./obs_pressure_bias.R --year=%04d --month=%d\n",
                  year,month))
      sink()
      system('sbatch multistart.auto.slm')
   }
   Sys.sleep(5)
}
