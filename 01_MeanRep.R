##this script calculates the mean  of the 5 replicates for each species/gcm/year

# Load libraries --------------------------------------------
library(pacman)

pacman::p_load(glue, raster, rgdal, rgeos, readxl, stringr, sf, 
               tidyverse, terra, foreach, fs, future.apply, furrr, fst, glue, compiler)

rm(list = ls())

# Load data --------------------------------------------------
path <- 'inputs/predictions'
spcs <- fs::dir_ls(path, type = 'directory') 
print(spcs)


# Function ----------------------------
make_average_reps <- function(sp){
  #sp <- spcs[14] ## Activate for testing
  cat('Start\n', sp, '\n')
  fls <- fs::dir_ls(sp) 
  dir <- sp 
  spn <- basename(sp) 
  nvt <- data.frame(specie = spn, raster = fls) %>%  
    as_tibble() %>%  
    mutate(name = basename(raster), 
           year = str_sub(name, start = nchar(name) - 7, end = nchar(name) - 4))  
  spl <- str_split(pull(nvt, name), pattern = '_') 
  nvt <- nvt %>% 
    mutate(run = sapply(1:length(spl), function(i) spl[[i]][2]), 
           gcm = sapply(1:length(spl), function(i) spl[[i]][1])) 
  nvt <- nvt %>% dplyr::select(specie, year, gcm) 
  dst <- nvt %>% distinct(specie, year, gcm)  
  
  cat('Calculating the mean of all replicates \n')
  
  rsl <- map(.x = 1:nrow(dst), .f = function(k){
    
    ds <- dst %>% slice(k) 
    sp <- pull(ds, 1)
    yr <- pull(ds, 2)
    gc <- pull(ds, 3)
    
    fl <- grep(sp, fls, value = TRUE) %>% 
      grep(yr, ., value = TRUE) %>%  
      grep(gc, ., value = TRUE)  
    
    tr <- terra::rast(fl) 
    av <- mean(tr)  
    ou <- glue('./outputs/{sp}/mean_{sp}_{yr}_{gc}.tif') 
    dr <- dirname(ou) 
    ifelse(!dir.exists(ou), dir.create(dr), print('Folder already exist')) 
    cat('To write the final raster\n')
    writeRaster(x = av, filename = ou, overwrite = TRUE)  
    cat('Done ', k, '\n')
})
  
  cat('Done\n') 
}


# Apply the function -----------------------------------------------------
spStack <- map(spcs, make_average_reps)





