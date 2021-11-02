if (1 == 0) {
  
  #####################################################
  # rough draft SCRIPT EXAMPLE OF HOW ONE COULD 
  # USE THE EJSCREEN API TO GET BUFFER RESULTS
  #####################################################
  
  # **** also just see the examples in  proxistat::bufferapi()   ****
  
  
  ### Preliminaries - clear memory, load packages
  #  rm(list=ls(all=TRUE))
  
  library(jsonlite)
  library(httr)
  library(tidyverse) # need magrittr
  library(data.table)
  library(dplyr)
  library(sf) # used only for EJSCREENbatch
  # 
  # packages from github ejanalysis/
  # library(ejanalysis); library(ejscreen); library(analyze.stuff)
  #
  # devtools::install_github("ejanalysis/proxistat") # https://github.com/ejanalysis/proxistat
  library(proxistat) 
  # devtools::install_github("ejanalysis/analyze.stuff") 
  library(analyze.stuff) # needed by proxistat
  # devtools::install_github("ejanalysis/UScensus2010blocks")
  library(UScensus2010blocks) # not sure it is essential. and should switch to 2020
  
  # https://github.com/USEPA/EJSCREENBatch
  # devtools::install_github('USEPA/EJSCREENBatch')
  library(EJSCREENbatch) # This is another draft buffering tool, for EJSCREENbatch::EJfunction()
  
  # specify where to save the data
  # setwd('~')
  
  ####################################
  # Specify test data points
  ## Draw random sample of lat/longs
  
  sample.count <- 100
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
  
     x <- do.call(rbind, outlist)
     x[,c(1:18,162:173)]
     t(x[1:2,]) 
  
  
  #####################################################
  
  
  
  
  
  #####################################################
  ## Data for comparison of API results with OW draft batch tool's output.
  #####################################################
  
  api.datalist <- data.table::rbindlist(outlist, fill = T, idcol = 'id') %>%
    dplyr::select('id', 'totalPop', 'N_D_INCOME_PER', 'N_D_MINOR_PER', 'N_D_LESSHS_PER',
                  'N_D_LING_PER','N_D_UNDER5_PER','N_D_OVER64_PER',
                  'N_E_CANCER_PER', 'N_E_DIESEL_PER','N_E_LEAD_PER',
                  'N_E_O3_PER', 'N_E_PM25_PER','N_E_NPL_PER',
                  'N_E_RMP_PER','N_E_TRAFFIC_PER','N_E_TSDF_PER',
                  'N_E_NPDES_PER','N_E_RESP_PER')
  
  #####################################################
  ## Run through the OW EJSCREENbatch function (from OW, on EPA's  github)
  #####################################################
  
  # ****  file must be stored locally on disk for now -: ****
  # get from SEDAC? per OW instructions on how to get it
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
