library(sybil)
library(BacArena)
library(parallel)

runSim <- function(i) {
  # params
  save_name <- 'Achromo_25h' # Name of the file as how it is saved
  arena_n <- 50
  arena_m <- 50
  time <- 25
  initial_bacteria <- 2
  sybil::SYBIL_SETTINGS("SOLVER","cplexAPI")
  bac1 <- BacArena::Bac((bac1), limit_growth=F, setAllExInf=T)
  arena <- BacArena::Arena(n=arena_n, m=arena_m, stir=F)
  arena <- BacArena::addOrg(arena, bac1, amount=initial_bacteria)
  arena <- BacArena::addSubs(arena, smax=1, unit='mM', mediac=paste0('EX_', media$compounds, '_e0'))
  # Begin the Simulation
  sim <- BacArena::simEnv(arena, time=time, diff_par = T, cl_size=3, sec_obj = 'mtf')
  saveRDS(sim, paste0(save_name, '_', i, '.RDS'))
}

bac1 <- readRDS('Achromobacter.RDS')
bac1@mod_desc <- "Achromobacter"
media <- read.csv('custom_MS_media_ARE.csv')
replicates <- 3
ncores <- 3
cl <- makeCluster(ncores,type="PSOCK")
clusterExport(cl,c('bac1','media'))
result <- parLapply(cl, 1:replicates, runSim)
stopCluster(cl)
print(result)

