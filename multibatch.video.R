# Map the obs pressure anomalies on SPICE

library(lubridate)

for(year in seq(1800,1870,1)) {
  for(month in seq(1,12)) {
    for(day in seq(1,days_in_month(ymd(sprintf("%04d%02d01",year,month))))) {
      sink('multistart.auto.slm')
      cat('#!/bin/ksh -l\n')
      cat('#SBATCH --qos=normal\n')
      cat('#SBATCH --output=/scratch/hadpb/slurm_output/bias_%j.out\n')
      cat('#SBATCH --mem=10000\n')
      cat('#SBATCH --ntasks=1\n')
      cat('#SBATCH --ntasks-per-core=1\n')
      cat('#SBATCH --time=5\n')
      cat(sprintf("./obs-clim_map.R --year=%04d --month=%d --day=%d\n",
                  year,month,day))
      sink()
      system('sbatch multistart.auto.slm')
   }
  }
   Sys.sleep(30)
}
