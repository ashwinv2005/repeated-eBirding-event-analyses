##################################### all previous GBBC data

require(lubridate)
require(tidyverse)
require(sp)
require(rgeos)
require(ggfortify)
require(rgdal)
require(sf)
require(mapview)
require(leaflet)
require(rmapshaper)

preimp = c("GLOBAL.UNIQUE.IDENTIFIER","CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
           "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","STATE.CODE","COUNTY.CODE",
           "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
           "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","LOCALITY","FIRST.NAME","LAST.NAME",
           "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")

nms = read.delim("ebd_IN_relDec-2020.txt", nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

# read data from certain columns only
data1 = read.delim("ebd_IN_relDec-2020.txt", colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ","null",NA))

## choosing important columns required for further analyses

data = data1
data = data %>% filter(OBSERVATION.DATE %in% c("2013-02-15","2013-02-16","2013-02-17","2013-02-18",
                                               "2014-02-15","2014-02-16","2014-02-17","2014-02-14",
                                               "2015-02-15","2015-02-16","2015-02-13","2015-02-14",
                                               "2016-02-15","2016-02-12","2016-02-13","2016-02-14",
                                               "2017-02-19","2017-02-20","2017-02-17","2017-02-18",
                                               "2018-02-19","2018-02-16","2018-02-17","2018-02-18",
                                               "2019-02-15","2019-02-16","2019-02-17","2019-02-18",
                                               "2020-02-15","2020-02-16","2020-02-17","2020-02-14"))

# no of days in every month, and cumulative number
days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

# create a column "group.id" which can help remove duplicate checklists
data = data %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER))

data = data %>%
  #filter(REVIEWED == 0 | VALID == 1) %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         month = month(OBSERVATION.DATE),
         day = day(OBSERVATION.DATE) + cdays[month],
         daym = day(OBSERVATION.DATE),
         #week = week(OBSERVATION.DATE),
         #fort = ceiling(day/14),
         cyear = year(OBSERVATION.DATE)) %>%
  dplyr::select(-c("OBSERVATION.DATE")) %>%
  mutate(year = ifelse(day <= 151, cyear-1, cyear))

load("maps.RData")

districtmap1 = gSimplify(districtmap, tol=0.01, topologyPreserve=TRUE)
d1 = districtmap@data
districtmap1 = sp::SpatialPolygonsDataFrame(districtmap1, d1)
statemap1 = gSimplify(statemap, tol=0.01, topologyPreserve=TRUE)
s1 = statemap@data
statemap1 = sp::SpatialPolygonsDataFrame(statemap1, s1)

load("clips.RData")

data = data %>% filter(!is.na(group.id))

# add columns with DISTRICT and ST_NM to main data 

temp = data %>% group_by(group.id) %>% slice(1) # same group ID, same grid/district/state 

rownames(temp) = temp$group.id # only to setup adding the group.id column for the future left_join
coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
proj4string(temp) = "+proj=longlat +datum=WGS84"
temp = over(temp,districtmap) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
temp = data.frame(temp) # convert into data frame for left_join
temp = temp[,1:2]
temp$group.id = rownames(temp) # add column to join with the main data
data = left_join(temp,data)

# add columns with protected area name to main data 

temp = data %>% group_by(group.id) %>% slice(1) # same group ID, same grid/district/state 

rownames(temp) = temp$group.id # only to setup adding the group.id column for the future left_join
coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
proj4string(temp) = "+proj=longlat +datum=WGS84"
temp = over(temp,pamap) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
temp = data.frame(temp) # convert into data frame for left_join
temp$group.id = rownames(temp) # add column to join with the main data
data = left_join(temp,data)


# add columns with GRID ATTRIBUTES to main data

temp = data %>% group_by(group.id) %>% slice(1)

rownames(temp) = temp$group.id
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,gridmapg1)
temp = data.frame(temp)
temp$group.id = rownames(temp)
data = left_join(temp,data)
names(data)[1] = "gridg1"

temp = data %>% group_by(group.id) %>% slice(1)

rownames(temp) = temp$group.id
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,gridmapg2)
temp = data.frame(temp)
temp$group.id = rownames(temp)
data = left_join(temp,data)
names(data)[1] = "gridg2"

temp = data %>% group_by(group.id) %>% slice(1)

rownames(temp) = temp$group.id
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,gridmapg3)
temp = data.frame(temp)
temp$group.id = rownames(temp)
data = left_join(temp,data)
names(data)[1] = "gridg3"

temp = data %>% group_by(group.id) %>% slice(1)

rownames(temp) = temp$group.id
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,gridmapg4)
temp = data.frame(temp)
temp$group.id = rownames(temp)
data = left_join(temp,data)
names(data)[1] = "gridg4"

temp = data %>% group_by(group.id) %>% slice(1)

rownames(temp) = temp$group.id
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,g2clip)
temp = data.frame(temp)
temp$group.id = rownames(temp)
data = left_join(temp,data)
names(data)[1] = "g2clip"

temp = data %>% group_by(group.id) %>% slice(1)

rownames(temp) = temp$group.id
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,g3clip)
temp = data.frame(temp)
temp$group.id = rownames(temp)
data = left_join(temp,data)
names(data)[1] = "g3clip"


############ Add first and last name

data$obs.id.num <- gsub("[[:alpha:]]", "", data$OBSERVER.ID)

eBird.users = read.delim("ebd_users_relNov-2020.txt", sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                         na.strings = c(""," ",NA))
names(eBird.users) = c("USER_ID","FIRST.NAME","LAST.NAME")
eBird.users$obs.id.num = gsub("[[:alpha:]]", "", eBird.users$USER_ID)
data = left_join(data, eBird.users)

pastdata = data


save(pastdata, file = "pastGBBCdata.RData")
