if (1 == 0) {
  
#####################################################
# SCRIPT SHOWING EXAMPLE OF HOW ONE COULD USE THE EJSCREEN API TO GET BUFFER RESULTS
#####################################################

### Preliminaries - clear memory, load packages
rm(list=ls(all=TRUE))

library(jsonlite)
library(httr)
library(tidyverse) # need magrittr
library(data.table)
library(sf)

# library(ejanalysis); library(ejscreen); library(analyze.stuff)
library(proxistat); library(UScensus2010blocks)

library(EJSCREENbatch) # This is another draft buffering tool, for EJSCREENbatch::EJfunction()

# specify where to save the data
# setwd('~')

####################################
# Specify test data points
## Draw random sample of lat/longs

sample.count <- 1000
pts <- proxistat::testpoints_block2010(sample.count)

####################################################

####################################
# Use API and test it for speed 
####################################

outlist <- proxistat::bufferapi(pts$lon, lat=pts$lat, radius = 5)

#####################################################
# see how the output is organized
# str(outlist[1]) 
summary(outlist)
class(outlist[[1]])
names(outlist[[1]])

# Note many outputs are NULL, no data returned by API...  why?
table(sapply(outlist, FUN = is.null))  # 40% were NULL, for example in one test

#####################################################



#####################################################
## Data for comparison of API results with other draft batch tool's output.
#####################################################

api.datalist <- data.table::rbindlist(outlist, fill = T, idcol = 'id') %>%
  select('id', 'totalPop', 'N_D_INCOME_PER', 'N_D_MINOR_PER', 'N_D_LESSHS_PER',
         'N_D_LING_PER','N_D_UNDER5_PER','N_D_OVER64_PER',
         'N_E_CANCER_PER', 'N_E_DIESEL_PER','N_E_LEAD_PER',
         'N_E_O3_PER', 'N_E_PM25_PER','N_E_NPL_PER',
         'N_E_RMP_PER','N_E_TRAFFIC_PER','N_E_TSDF_PER',
         'N_E_NPDES_PER','N_E_RESP_PER')

#####################################################
## Run through their EJSCREENbatch function
#####################################################

# ****  file must be stored locally on disk for now: ****
raster.path <- '~/Downloads/EJ_example/US Census Grid_SF2010_TIFF'

## Keep only coordinate that returned data from api (where an api.datalist$id was not empty? ''? )
keep.pts <- pts[api.datalist$id,] %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) 

batch.datalist <- EJSCREENbatch::EJfunction(data_type = "landbased", 
                             facility_data = keep.pts, 
                             gis_option = "intersection", 
                             raster_data = raster.path, 
                             buff_dist = c(5), 
                             threshold = 80)


}
