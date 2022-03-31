# Load libraries --------------------------------------------
library(pacman)

pacman::p_load(glue, raster, rgdal, rgeos, readxl, stringr, sf, R.filesets,
               tidyverse, terra, foreach, fs, future.apply, furrr, fst,
               stringr, glue, compiler, hrbrthemes, gtools, hrbrthemes, colorspace)

rm(list = ls())

# Load data ---------------------------------------------------------------
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)
limt <- sf::st_read('inputs/NT1_BCR6/NT1_BCR6_poly.shp') 

ecrg <- sf::st_read('inputs/ecoregions/ecoregions.shp')

targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

# Extract by mask for the ecoregions ---------------------------------------
plot(st_geometry(ecrg))
limt <- sf::st_transform(x = limt, crs = targetCRS)
ecrg <- sf::st_transform(x = ecrg, crs = targetCRS)
ecrg_limt <- sf::st_intersection(x = ecrg, y = limt)
plot(st_geometry(ecrg_limt))
plot(st_geometry(limt))


logRatio_rasters <- function(spc){
  spc <- spcs[2] # Run and comment (after)
  cat('Start ', spc, '\n')
  dir <- grep(spc, dirs, value = TRUE)
  fle <- fs::dir_ls(dir, regexp = '.qs')
  tbl <- qs::qread(file = glue('./outputs/{spc}/_yrs_{spc}.qs'))
  tbl <- dplyr::select(tbl, x, y, gc, everything())
  names(tbl)[1:2] <- c('lon', 'lat')
  tbl <- mutate(tbl, avg = rowMeans(tbl[,4:9]))
  tbl <- as_tibble(tbl)
  gcm <- unique(tbl$gc)
  
  cat('Estimating change from initial (2011) and final (2091) year\n')
  tbl <- mutate(tbl, change = y2091 - y2011 )  #change 2100 for 2091
  #tbl <- mutate(tbl, perctChange = ((y2091 - y2011) / y2011) * 100) 
  tbl <- mutate(tbl, ratio = (y2091/y2011))
  tbl <- mutate(tbl, logRatio = log2(ratio))
  tbl <- mutate(tbl, gc = as.factor(gc))
  
  qs::qsave(x = tbl, file = glue('./qs/{spc}_table_changes.qs'))
  
  cat('Making a change map for:', spc, '\n')
  ggRatio <- ggplot() +
    geom_tile(data = tbl, aes(x = lon, y = lat, fill = change)) +
    #scale_fill_binned_diverging(palette= 'Blue-Red', rev = TRUE, n.breaks = 5) +
    scale_fill_gradientn(colours = '#D73027','#f4f4f4', '#1A9870') +
    facet_wrap(. ~ gc, ncol = 3, nrow = 1) +
    #geom_tile(aes(fill = logRatio)) +
    geom_sf(data = limt, fill = NA, col = '#999999') +
    geom_sf(data = ecrg_limt, fill = NA, col = '#bfbfbf') +
    #scale_fill_manual(values = c('#D73027','#f4f4f4', '#1A9870')) + ## change green from #1A9850
    ggtitle(label = spc) +
    theme_bw() +
    theme(legend.position = 'bottom',
          legend.key.width = unit(2, 'line'),
          axis.text.y = element_text(angle = 90, vjust = 0.5)) +
    labs(x = 'Longitude', y = 'Latitude')
  
  ggsave(plot = ggRatio,filename = glue('./graphs/maps/ratio/change_{spc}.png'),
         units = 'in', width = 12, height = 9, dpi = 700)
}

# Apply the function -----------------------------------------------------
map(spcs, logRatio_rasters)