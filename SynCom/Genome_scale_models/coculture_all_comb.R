library(sybil)
library(BacArena)
library(parallel)

# Function to run simulation for a pair of bacteria
runSim <- function(bac1, bac2, i) {
  # params
  save_name <- paste0(bac1@mod_desc, '_', bac2@mod_desc, '_25h') # Updated save_name
  arena_n <- 50  # size of the arena n*m
  arena_m <- 50
  time <- 25
  initial_bacteria <- 2
  sybil::SYBIL_SETTINGS("SOLVER","cplexAPI")
  
  bac1 <- BacArena::Bac(bac1, limit_growth=F, setAllExInf=T)
  bac2 <- BacArena::Bac(bac2, limit_growth=F, setAllExInf=T)
  
  arena <- BacArena::Arena(n=arena_n, m=arena_m, stir=F)
  arena <- BacArena::addOrg(arena, bac1, amount=initial_bacteria)
  arena <- BacArena::addOrg(arena, bac2, amount=initial_bacteria)
  arena <- BacArena::addSubs(arena, smax=1, unit='mM', mediac=paste0('EX_', media$compounds, '_e0'))
  
  # Begin the Simulation
  sim <- BacArena::simEnv(arena, time=time, diff_par = T, cl_size=5, sec_obj = 'mtf')
  saveRDS(sim, paste0(save_name, '_', i, '.RDS'))
}

# Read bacteria files from folder
bacteria_folder <- "/Users/arijitmukherjee/Downloads/all_GMMs/"
bacteria_files <- list.files(bacteria_folder, pattern = "\\.RDS$", full.names = TRUE)

# Create all pairwise combinations
bacteria_combinations <- combn(bacteria_files, 2, simplify = TRUE)
bacteria_combinations
media<-read.csv('/export2/home/arijit/bacarena/custom_MS_media_ARE.csv')
# Set up parallel processing
replicates <- 3 # number of replicates to run in parallel
ncores <- 5
cl <- makeCluster(ncores,type="PSOCK")
clusterExport(cl, c('media', 'runSim','bacteria_combinations'))
bac1<-readRDS(bacteria_combinations[1, i])
bac2<-readRDS(bacteria_combinations[2, i])
bac1@mod_name

# Run simulations for each combination in parallel
result <- parLapply(cl, 1:ncol(bacteria_combinations), function(i) {
  bac1 <- readRDS(bacteria_combinations[1, i])
  bac2 <- readRDS(bacteria_combinations[2, i])
  runSim(bac1, bac2, i)
})