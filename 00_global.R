
# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(raster, rgdal, rgeos, reproducible, readxl, stringr, sf, 
               tidyverse, foreach, fs, data.table, qs, glue, terra)

# Load functions ----------------------------------------------------------
source('./R/downloadPredRas.R')
source('./R/loadBirdPredictions.R')
source('./R/loadMeanRas.R')
source('./R/loadOccRas.R')


#speciesOfInterest <- c("NOWA", "FOSP", "HOLA", "OSFL", "PISI", "SWSP", "TEWA", "WEWP", 
 #                      "WWCR", "ATSP", "BOCH", "COYE", "CAWA", "REVI", "WCSP")

#complete list of 72 species from google drive
species <- c("ALFL", "AMCR", "AMRE", "AMRO", "ATSP", "ATTW", "BAWW", "BBWA", 
             "BBWO", "BCCH", "BHCO", "BHVI", "BLPW", "BOCH", "BRBL", "BRCR",
             "BTNW", "CAWA", "CCSP", "CHSP", "CONW", "CORA", "COYE", "DEJU", 
             "EAKI", "EVGR", "FOSP", "GCKI", "GCTH", "GRAJ", "HAFL", "HETH",
             "HOLA", "LCSP", "LEFL", "LEYE", "LISP", "MAWA", "NOWA", "OCWA", 
             "OSFL", "OVEN", "PAWA", "PHVI", "PISI", "PUFI", "RBGR", "RBNU", 
             "RCKI", "REVI", "RUBL", "RUGR", "RWBL", "SAVS", "SOSP", "SWSP", 
             "SWTH", "TEWA" ,"TRES", "VATH", "WAVI", "WCSP", "WETA", "WEWP", 
             "WIWA", "WIWR", "WTSP", "WWCR", "YBFL", "YBSA", "YEWA", "YRWA")

pathData <- './outputs'

birdPredictions <- downloadPredRas(folderUrl= "1O34zQIem_RUxxCDOZMGEUPIkUtCWsS_c",
                                   birdsList = paste(species, collapse = "|"),
                                   #yearAnalysis = paste(c(2011, 2100), collapse = "|"),
                                   #climateScenario = "CanESM2",
                                   rastersPath =pathData)

birdPred <- loadBirdPredictions(birdList = species,
                                pathData = pathData)
meanStack <- loadMeanRas(species = species,
                         pathData = pathData,
                         pattern = 'mean')

occStack <- loadOccRas(species = species,
                       pathData = pathData,
                       pattern = 'occu')
