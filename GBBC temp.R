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

preimp = c("GLOBAL.UNIQUE.IDENTIFIER","category","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
          "LOCALITY.ID","LOCALITY.TYPE","reviewed","valid","STATE.CODE","COUNTY.CODE",
           "latitude","longitude","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
           "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","FIRST.NAME","LAST.NAME",
           "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")

nms = read.delim("GBBC1.tsv", nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

# read data from certain columns only
data1 = read.delim("GBBC1.tsv", colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ","null",NA))
data2 = read.delim("GBBC2.tsv", colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ","null",NA))
data3 = read.delim("GBBC3.tsv", colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ","null",NA))
data4 = read.delim("GBBC4.tsv", colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ","null",NA))
data5 = read.delim("GBBC5.tsv", colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ","null",NA))


# merge both data frames
data1 = rbind(data1,data2,data3,data4,data5)
names(data1) = c("GLOBAL.UNIQUE.IDENTIFIER","CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
                 "STATE.CODE","COUNTY.CODE","LOCALITY.ID","LOCALITY.TYPE",
                 "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
                 "FIRST.NAME","LAST.NAME","SAMPLING.EVENT.IDENTIFIER","PROTOCOL.TYPE","DURATION.MINUTES",
                 "EFFORT.DISTANCE.KM","NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER",
                 "VALID","REVIEWED")



# COTTON UNIVERSITY in Assam
#data = data1 %>% filter(STATE.CODE %in% c("IN-AS"))
#data = data %>% filter(!is.na(COUNTY.CODE)) %>%   
#  filter(grepl('Cotton',LOCALITY.ID))
data = data1 %>% filter(OBSERVATION.DATE %in% c("2022-02-18","2022-02-19","2022-02-20","2022-02-21"))

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

states = data %>%
  group_by(STATE.CODE) %>% summarize(n = n_distinct(OBSERVER.ID)) %>% arrange(desc(n))

write.csv(data, "Cotton University_GBBC_data.csv", row.names = F)
