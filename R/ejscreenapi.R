#' Use EJSCREEN API to get stats on each circular buffer
#' 
#' Requests a standard EJSCREEN report for each of one or more circular buffers.
#' Specify a radius and vector of latitude longitude points,
#' and get for a buffer the population weighted mean value of each raw indicator
#' like percent low-income, and total population count, and percentiles for those
#' raw indicator scores, all from EJSCREEN, as in an EJSCREEN standard report. 
#' 
#' @param lon Longitude numeric vector
#' @param lat Latitude numeric vector
#' @param radius radius of circular buffer - Probably in miles but should confirm
#'
#' @export
#'
#' @examples  
#'  \dontrun{
#'  # Specify size of buffer circle and pick random points as example data
#'  myradius <- 3
#'  n <- 10
#'  pts <- proxistat::testpoints_block2010(n)
#'  
#'  benchmark.start <- Sys.time()
#'   out <- proxistat::ejscreenapi(pts$lon, lat=pts$lat, radius = myradius)
#'  benchmark.end <- Sys.time()
#'   
#'  # Average speed
#'  perhour <- proxistat::speedsummary(benchmark.start, benchmark.end, NROW(pts))
#'  cat('\n', perhour, 'buffers per hour \n')
#'  out[,c(1:18,162:173)]
#'  t(out[1:2,]) 
#'  }
ejscreenapi <- function(lon, lat, radius=5) {
  
  # as of 2021 - note this excludes the geometry column
  outcolnames <-  c("RAW_E_PM25", "RAW_E_O3", "RAW_E_DIESEL", "RAW_E_CANCER", "RAW_E_RESP", 
    "RAW_E_TRAFFIC", "RAW_E_LEAD", "RAW_E_NPL", "RAW_E_RMP", "RAW_E_TSDF", 
    "RAW_E_NPDES", "RAW_D_INDEX", "RAW_D_MINOR", "RAW_D_INCOME", 
    "RAW_D_LING", "RAW_D_LESSHS", "RAW_D_UNDER5", "RAW_D_OVER64", 
    "S_E_PM25", "S_E_O3", "S_E_DIESEL", "S_E_CANCER", "S_E_RESP", 
    "S_E_TRAFFIC", "S_E_LEAD", "S_E_NPL", "S_E_RMP", "S_E_TSDF", 
    "S_E_NPDES", "S_D_INDEX", "S_D_MINOR", "S_D_INCOME", "S_D_LING", 
    "S_D_LESSHS", "S_D_UNDER5", "S_D_OVER64", "S_P_PM25", "S_P_O3", 
    "S_P_DIESEL", "S_P_CANCER", "S_P_RESP", "S_P_TRAFFIC", "S_P_LEAD", 
    "S_P_NPL", "S_P_RMP", "S_P_TSDF", "S_P_NPDES", "S_E_PM25_PER", 
    "S_E_O3_PER", "S_E_DIESEL_PER", "S_E_CANCER_PER", "S_E_RESP_PER", 
    "S_E_TRAFFIC_PER", "S_E_LEAD_PER", "S_E_NPL_PER", "S_E_RMP_PER", 
    "S_E_TSDF_PER", "S_E_NPDES_PER", "S_D_INDEX_PER", "S_D_MINOR_PER", 
    "S_D_INCOME_PER", "S_D_LING_PER", "S_D_LESSHS_PER", "S_D_UNDER5_PER", 
    "S_D_OVER64_PER", "R_E_PM25", "R_E_O3", "R_E_DIESEL", "R_E_CANCER", 
    "R_E_RESP", "R_E_TRAFFIC", "R_E_LEAD", "R_E_NPL", "R_E_RMP", 
    "R_E_TSDF", "R_E_NPDES", "R_D_INDEX", "R_D_MINOR", "R_D_INCOME", 
    "R_D_LING", "R_D_LESSHS", "R_D_UNDER5", "R_D_OVER64", "R_P_PM25", 
    "R_P_O3", "R_P_DIESEL", "R_P_CANCER", "R_P_RESP", "R_P_TRAFFIC", 
    "R_P_LEAD", "R_P_NPL", "R_P_RMP", "R_P_TSDF", "R_P_NPDES", "R_E_PM25_PER", 
    "R_E_O3_PER", "R_E_DIESEL_PER", "R_E_CANCER_PER", "R_E_RESP_PER", 
    "R_E_TRAFFIC_PER", "R_E_LEAD_PER", "R_E_NPL_PER", "R_E_RMP_PER", 
    "R_E_TSDF_PER", "R_E_NPDES_PER", "R_D_INDEX_PER", "R_D_MINOR_PER", 
    "R_D_INCOME_PER", "R_D_LING_PER", "R_D_LESSHS_PER", "R_D_UNDER5_PER", 
    "R_D_OVER64_PER", "N_E_PM25", "N_E_O3", "N_E_DIESEL", "N_E_CANCER", 
    "N_E_RESP", "N_E_TRAFFIC", "N_E_LEAD", "N_E_NPL", "N_E_RMP", 
    "N_E_TSDF", "N_E_NPDES", "N_D_INDEX", "N_D_MINOR", "N_D_INCOME", 
    "N_D_LING", "N_D_LESSHS", "N_D_UNDER5", "N_D_OVER64", "N_P_PM25", 
    "N_P_O3", "N_P_DIESEL", "N_P_CANCER", "N_P_RESP", "N_P_TRAFFIC", 
    "N_P_LEAD", "N_P_NPL", "N_P_RMP", "N_P_TSDF", "N_P_NPDES", "N_E_PM25_PER", 
    "N_E_O3_PER", "N_E_DIESEL_PER", "N_E_CANCER_PER", "N_E_RESP_PER", 
    "N_E_TRAFFIC_PER", "N_E_LEAD_PER", "N_E_NPL_PER", "N_E_RMP_PER", 
    "N_E_TSDF_PER", "N_E_NPDES_PER", "N_D_INDEX_PER", "N_D_MINOR_PER", 
    "N_D_INCOME_PER", "N_D_LING_PER", "N_D_LESSHS_PER", "N_D_UNDER5_PER", 
    "N_D_OVER64_PER", "stateAbbr", "stateName", "epaRegion", "totalPop", 
    "NUM_NPL", "NUM_TSDF", "statLayerCount", "statLayerZeroPopCount", 
    "weightLayerCount", "timeSeconds", "distance", "unit", "statlevel", 
    "inputAreaMiles")
  
  benchmark.start <- Sys.time()
  cat("\n")
  cat("Buffering for", length(lon), 'points for radius of', radius, '\n')
  cat('\n')
  pts <- data.frame(lon=lon, lat=lat)
  outlist <- vector(mode = 'list', length = dim(pts)[1])
  noresults_count <- 0
  
  for (i in 1:dim(pts)[1]){
    cat(paste0('Iteration #: ', i))
    
    # Not entirely sure this GET() is the one from the httr package, so I guessed
    ej.data <- httr::GET(
      paste0('https://ejscreen.epa.gov/mapper/ejscreenRESTbroker.aspx?namestr=&',
             'geometry={"spatialReference":{"wkid":4326},"x":',
             pts$lon[[i]], ',"y":', pts$lat[[i]],
             '}&distance=', radius,'&unit=9035&areatype=&areaid=&f=pjson')
      #    '}&distance=5&unit=9035&areatype=&areaid=&f=pjson')
    )$content
    
    #
    ### Not sure which package this fromJSON() was intended to be from...  (and not sure they are identical, ie if it matters)
    # That same function name is used by RJSONIO and  jsonlite  and  rjson
    # This use of fromJSON was I think from an example provided by Victoria S maybe
    #
    ej.data <- data.table::as.data.table(jsonlite::fromJSON(rawToChar(ej.data)))
    
    
    # SHOULD HANDLE NULL RESULTS HERE 
    ###########################
    # Note: api does not return values for coords in highly nonpopulated areas.
    # it just returns this as ej.data:
    #    message                                                messageType
    # 1: Input area too small or sparsely populated for output  underLimit
    #
    # lat lon are stored in a second and third row but the rest of those rows is just duplicated info so drop them
    # lon is in   ej.data[2,'geometry']  and also  is  pts$lon[i]

    # save all but geometry column if got valid results back, i.e. got more than 100 columns instead of an error message
    if (dim(ej.data)[2] > 100){  
      # outlist[[i]] <- unique(ej.data[, geometry := NULL]) ## remove column by reference in data.table package ... not sure why this line stopped working. 
      outlist[[i]] <- unique(ej.data[, -166])# column 166 is geometry
    } else {
      # assume got error message and save as a table full of NA values
      cat(' No results - (probably because) area too small or sparsely populated for output')
      emptyresults <- data.frame(matrix(data = NA, nrow = 1, ncol = length(outcolnames))) 
      colnames(emptyresults) <- outcolnames
      outlist[[i]] <- emptyresults
      noresults_count <- noresults_count + 1
    }
    outlist[[i]][,'lon'] <- pts$lon[i]
    outlist[[i]][,'lat'] <- pts$lat[i]
    cat('\n')
    # rm(ej.data)
  } # end of loop over buffers
  
  # Format results as a single table
  
  api.out.table <- data.table::rbindlist(outlist, fill = T, idcol = 'id')
  results <- do.call(rbind, outlist)
  # does it need to be just a data.frame or is data.table OK still?
  
  # Fix numeric columns that got turned into character format
  results <- makenumericdf(results)
  
  benchmark.end <- Sys.time()
  # report time elapsed and avg speed
  total.benchmark <- difftime(benchmark.end, benchmark.start)
  total.seconds <- difftime(benchmark.end, benchmark.start, units = 'secs')
  perhour <- round((NROW(pts)/ as.numeric(total.seconds))*3600,0)
  cat('\n')
  cat(paste0(
    'Rate of ',
    format(round((NROW(pts) / as.numeric(total.seconds))*3600,0), big.mark=',', scientific=FALSE), 
    ' buffers per hour (',
    format(NROW(pts),big.mark = ',', scientific = FALSE),
    ' lat/long pairs per ',
    format(round(as.numeric(total.seconds),0), big.mark = ',', scientific = FALSE),
    ' seconds)'
  )  )
  cat('\n')
  print(round(total.benchmark, 1))
  cat('\n')
  cat('Note that results were unavailable for', noresults_count,'out of the', NROW(pts), 'buffers.')
  cat('\n')
  
  return(results)
}
