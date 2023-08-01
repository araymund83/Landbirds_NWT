# Load libraries --------------------------------------------
library(pacman)

pacman::p_load(glue, raster, rgdal, rgeos, readxl, stringr, sf, R.filesets,
               tidyverse, terra, foreach, fs, future.apply, furrr, fst,
               stringr, glue, compiler, hrbrthemes, gtools, hrbrthemes, colorspace)

rm(list = ls())

# Load data ---------------------------------------------------------------
thrs <- read_csv('./inputs/prevOcc.csv')
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)
spcs <- spcs[1:72]
dirs <- glue('{dirs}/occur')
dirs <- as.character(dirs)

limt <- sf::st_read('inputs/NT1_BCR6/NT1_BCR6_poly.shp') 
ecrg <- sf::st_read('inputs/ecoregions/NWT_ecoregions_dissolvec.shp')


targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
spName <- read.csv('./inputs/SppNames.csv') 

# Extract by mask for the ecoregions ---------------------------------------
plot(st_geometry(ecrg))
limt <- sf::st_transform(x = limt, crs = targetCRS)
ecrg <- sf::st_transform(x = ecrg, crs = targetCRS)
plot(st_geometry(ecrg))
plot(st_geometry(limt))


calcChange_rasters <- function(spc){
  spc <- spcs[1] # Run and comment (after)
  cat('Start ', spc, '\n')
  dir <- grep(spc, dirs, value = TRUE)
  fle <- fs::dir_ls(dir, regexp = '.qs')
  tbl <- qs::qread(file = glue('./outputs/{spc}/occur/occ_yrs_{spc}.qs'))
  tbl <- dplyr::select(tbl, x, y, gc, everything())
  names(tbl)[1:2] <- c('lon', 'lat')
  tbl <- mutate(tbl, avg = rowMeans(tbl[,4:9]))
  tbl <- as_tibble(tbl)
  gcm <- unique(tbl$gc)
  tbl <- tbl %>% mutate(pixelID = 1:nrow(tbl))
  #tbl <- select(tbl, -c(lon, lat))
  
  #tblLong <- tbl %>% pivot_longer( !c(gc,pixelID, lat, lon), names_to = 'year', values_to = 'value')
  
  cat('Estimating change from initial (2011) and final (2091) year\n')
  tbl <- mutate(tbl, change = y2091 - y2011 )  #change 2100 for 2091
  #tbl <- mutate(tbl, perctChange = ((y2091 - y2011) / y2011) * 100) 
  tbl <- mutate(tbl, ratio = (y2091/y2011))
  tbl <- mutate(tbl, log2Ratio = log2(ratio))
  tbl <- mutate(tbl, gc = as.factor(gc))
  
  
  #tst <- tbl %>%  filter(gc == 'CCSM4') %>% dplyr::select(lon, lat, logRatio) %>% rasterFromXYZ()
  ## para verificar que los datos se comportan de esa manera, y no hay error de indexacion.
  
  qs::qsave(x = tbl, file = glue('./qs/{spc}_occu_change.qs'))
  
  cat('Making a change map for:', spc, '\n')
  ggRatio <- ggplot() +
    geom_tile(data = tbl, (aes(x = 'lon', y = 'lat', fill = 'change'))) +
    #scale_fill_binned_diverging(palette= 'Blue-Red', rev = TRUE, n.breaks = 5) +
    scale_colour_manual(values =c('#a6611a','#dfc27d' ,'#f5f5f5', '#80cdc1', '#018571')) +
    geom_sf(data = limt, fill = NA, col = '#999999') +
    geom_sf(data = ecrg, fill = NA, col = '#bfbfbf')+
    coord_sf(lims_method = 'geometry_bbox') + 
    facet_wrap(.~gc, ncol = 3, nrow = 1 ) +
    ggtitle(label = spc) +
    theme_bw() +
    theme(legend.position = 'bottom',
          #legend.key.width = unit(2, 'line'),## aumenta la longitud de la leyenda
          axis.text.y = element_text(angle = 90, vjust = 0.5)) +
    labs(x = 'Longitude', y = 'Latitude', fill = 'Change')
  
  ggsave(plot = ggRatio,filename = glue('./graphs/maps/ratio/ocurr/occ_change91-11_{spc}.png'),
         units = 'in', width = 12, height = 9, dpi = 300)
}

# Apply the function -----------------------------------------------------
map(spcs[31:75], logRatio_rasters)
