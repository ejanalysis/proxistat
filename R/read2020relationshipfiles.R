read2020relationshipfiles <- function(folder) {
  
  # read census 2020 vs 2010 block relationship files
  # that show what changes were made to blocks
  # downloaded from  https://www.census.gov/geographies/reference-files/time-series/geo/relationship-files.html 
  # documentation:
  # https://www.census.gov/programs-surveys/geography/technical-documentation/records-layout/2020-census-block-record-layout.html
  #setwd('~/Downloads/2020 vs 2010 block relationships')
  
  x <- list()
  fnames <- list.files(path = folder, pattern = 'tab2010_tab2020')
  for (i in 1:length(fnames)) {
    # fname = 'tab2010_tab2020_st01_al.txt'
    x[i] <- readr::read_delim(file = file.path(folder, fnames[i]))
    print(fnames[i])
  }
  x <- rbind(x)
  return(x)

# dim(x)
# 292,219 blocks in just Alabama 
# cbind(names(x))

# [1,] "STATE_2010"       
# [2,] "COUNTY_2010"      
# [3,] "TRACT_2010"       
# [4,] "BLK_2010"         
# [5,] "BLKSF_2010"       
# [6,] "AREALAND_2010"    
# [7,] "AREAWATER_2010"   
# [8,] "BLOCK_PART_FLAG_O"

# [9,] "STATE_2020"       
# [10,] "COUNTY_2020"      
# [11,] "TRACT_2020"       
# [12,] "BLK_2020"         
# [13,] "BLKSF_2020"       
# [14,] "AREALAND_2020"    
# [15,] "AREAWATER_2020"   
# [16,] "BLOCK_PART_FLAG_R"

# [17,] "AREALAND_INT"     
# [18,] "AREAWATER_INT" 

# 2010 Census Tabulation Block to 2020 Census Tabulation Block Relationship File Layout
# Field #	Maximum Length	Field Name	Field Description
# 1	2	  STATE_2010	2010 tabulation state FIPS code
# 2	3	  COUNTY_2010	2010 tabulation county FIPS code
# 3	6	  TRACT_2010	2010 census tract number
# 4	4	  BLK_2010	2010 tabulation block number
# 5	1	  BLKSF_2010	2010 tabulation block suffix
# 6	14	AREALAND_2010	2010 Land Area
# 7	14	AREAWATER_2010	2010 Water Area
# 8	1	  BLOCK_PART_FLAG_O	2010 tabulation block part flag; Blank = whole; P = part

# 9	2	    STATE_2020	2020 tabulation state FIPS code
# 10	3	  COUNTY_2020	2020 tabulation county FIPS code
# 11	6	  TRACT_2020	2020 census tract number
# 12	4	  BLK_2020	2020 tabulation block number
# 13	1	  BLKSF_2020	2020 tabulation block suffix
# 14	14	AREALAND_2020	2020 Land Area
# 15	14	AREAWATER_2020	2020 Water Area
# 16	1	  BLOCK_PART_FLAG_R	2020 tabulation block part flag; Blank = whole; P = part

# 17	14	AREALAND_INT	Intersection Land Area shared by the 2010 and 2020 blocks represented by the record
# 18	14	AREAWATER_INT	Intersection Water Area shared by the 2010 and 2020 blocks represented by the record

# table(new=x$BLOCK_PART_FLAG_O, old=x$BLOCK_PART_FLAG_R, useNA = 'always')
# 
# x[1:10, c("BLK_2010","BLK_2020",'BLOCK_PART_FLAG_O','BLOCK_PART_FLAG_R')]
# 
# x[x$STATE_2010 == '01' & x$COUNTY_2010 == '001' & x$TRACT_2010 == '020100' , ]
# 
# x[x$STATE_2010 == '01' & x$COUNTY_2010 == '001' & x$TRACT_2010 == '020500' & x$BLK_2020 == 1002, ]
# 
# (x[x$STATE_2010 == '01' & x$COUNTY_2010 == '001' & x$TRACT_2010 == '020100' & x$BLK_2020 %in% c(1000,1001), c(1:4,8,6,17,14,16,12)])


}
