# get a list of sampling event identifiers

library(tidyverse)
library(lubridate)
library(stringr)

pmclists = read.csv("pmc_list_data.csv")
temp = str_split_fixed(pmclists$url, 'checklist/', 2)
pmclists$samp.id = temp[,2]
write.csv(pmclists,"pmc_list_data.csv",row.names = F)



# read data and start analysis

pmclists = read.csv("pmc_list_data.csv")

preimp = c("GLOBAL.UNIQUE.IDENTIFIER","CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
           "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","STATE.CODE","COUNTY.CODE","EXOTIC.CODE",
           "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
           "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","LOCALITY","FIRST.NAME","LAST.NAME",
           "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")

nms = read.delim("ebd_IN-GJ-KA_202209_202209_relOct-2022.txt", nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

# read data from certain columns only
data1 = read.delim("ebd_IN-GJ-KA_202209_202209_relOct-2022.txt", colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ","null",NA))

## choosing important columns required for further analyses

data = data1
data = data %>% filter(OBSERVATION.DATE %in% c("2022-09-10","2022-09-11"))

# no of days in every month, and cumulative number
days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

# create a column "group.id" which can help remove duplicate checklists
data = data %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER))

data = data %>%
  filter(REVIEWED == 0 | APPROVED == 1) %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         month = month(OBSERVATION.DATE),
         day = day(OBSERVATION.DATE) + cdays[month],
         daym = day(OBSERVATION.DATE),
         #week = week(OBSERVATION.DATE),
         #fort = ceiling(day/14),
         cyear = year(OBSERVATION.DATE)) %>%
  dplyr::select(-c("OBSERVATION.DATE")) %>%
  mutate(year = ifelse(day <= 151, cyear-1, cyear))

# create and write a file with common names and scientific names of all species
# useful for mapping
temp = data %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  filter(!EXOTIC.CODE %in% c("X")) %>%
  distinct(COMMON.NAME,SCIENTIFIC.NAME)

write.csv(temp,"PMCspecieslist2022.csv", row.names=FALSE)



#########

euro = data %>% filter(COMMON.NAME == "European Roller")
euro1 = euro %>% 
  group_by(group.id) %>% slice(1)
euro2 = euro %>% 
  filter(SAMPLING.EVENT.IDENTIFIER %in% pmclists$samp.id)

spfl = data %>% filter(COMMON.NAME == "Spotted Flycatcher")
spfl1 = spfl %>% 
  group_by(group.id) %>% slice(1)
spfl2 = spfl %>% 
  filter(SAMPLING.EVENT.IDENTIFIER %in% pmclists$samp.id)

rtsr = data %>% filter(COMMON.NAME == "Rufous-tailed Scrub-Robin")
rtsr1 = rtsr %>% 
  group_by(group.id) %>% slice(1)
rtsr2 = rtsr %>% 
  filter(SAMPLING.EVENT.IDENTIFIER %in% pmclists$samp.id)


