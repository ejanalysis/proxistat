## NOTES on naming of counties and county equivalent areas in USA

# data(countiesall, package = 'proxistat')

## All countylike places have one or more of these terms in their full name:

# for (term in c('County', 'City', 'city', 'Parish', 'Municipio', 'Municipality', 'Borough', 'Island','Census Area', 'District of', 'District','Guam')) {cat('\n'); cat(sum(grepl(term, countiesall$fullname)), ' ', term)}

# 3007   County
# 7   City
# 41   city
# 64   Parish
# 78   Municipio
# 6   Municipality
# 16   Borough
# 18   Island
# 11   Census Area
# 1   District of
# 4   District
# 1   Guam

## > 3007+7+41+64+78+6+16+18+11+1+4+1
## [1] 3254  # this doublecounts the ones with 2+ terms

# length(countiesall$fullname[grepl('(County|City|city|Parish|Municipio|Municipality|Borough|Island|Census Area|District of|District,|Guam)', countiesall$fullname)])
## [1] 3235

# length(countiesall$fullname[!grepl('(County|City|city|Parish|Municipio|Municipality|Borough|Island|Census Area|District of|District,|Guam)', countiesall$fullname)])
## 0
