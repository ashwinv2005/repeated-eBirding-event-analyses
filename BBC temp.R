require(tidyverse)
require(lubridate)


############ day 1

preimp = c("GLOBAL.UNIQUE.IDENTIFIER","category","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
           "LOCALITY.ID","LOCALITY.TYPE","reviewed","valid","STATE.CODE","COUNTY.CODE",
           "latitude","longitude","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
           "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","FIRST.NAME","LAST.NAME",
           "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")

nms = read.delim("India Records -2023_01_13-11_00_04_384-D12iR-10-1.tsv", nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

# read data from certain columns only
data1 = read.delim("India Records -2023_01_13-11_00_04_384-D12iR-10-1.tsv", colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ","null",NA))

names(data1) = c("GLOBAL.UNIQUE.IDENTIFIER","CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
                 "STATE.CODE","COUNTY.CODE","LOCALITY.ID","LOCALITY.TYPE",
                 "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
                 "FIRST.NAME","LAST.NAME","SAMPLING.EVENT.IDENTIFIER","PROTOCOL.TYPE","DURATION.MINUTES",
                 "EFFORT.DISTANCE.KM","NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER",
                 "VALID","REVIEWED")

data_day1 = data1 %>% filter(STATE.CODE %in% c("IN-AS"))



days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

# create a column "group.id" which can help remove duplicate checklists
data_day1 = data_day1 %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER))

data_day1 = data_day1 %>%
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

data_day1 = data_day1 %>% filter(daym == 14, month == 4, cyear == 2022)

temp = data_day1 %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  distinct(COMMON.NAME,SCIENTIFIC.NAME)

a = length(unique(data_day1$SAMPLING.EVENT.IDENTIFIER))
b = length(unique(data_day1$group.id))
c = length(unique(temp$COMMON.NAME))
d = length(unique(data_day1$COUNTY.CODE))
e = length(unique(data_day1$OBSERVER.ID))

temp = data_day1 %>%
  group_by(COUNTY.CODE) %>% summarize(n = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>%
  arrange(desc(n))




####################################################################### day 2

preimp = c("GLOBAL.UNIQUE.IDENTIFIER","category","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
           "LOCALITY.ID","LOCALITY.TYPE","reviewed","valid","STATE.CODE","COUNTY.CODE",
           "latitude","longitude","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
           "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","FIRST.NAME","LAST.NAME",
           "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")

nms = read.delim("India Records -2023_01_14-11_00_05_011-hIURb-10-1.tsv", nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

# read data from certain columns only
data1 = read.delim("India Records -2023_01_14-11_00_05_011-hIURb-10-1.tsv", colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ","null",NA))

names(data1) = c("GLOBAL.UNIQUE.IDENTIFIER","CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
                 "STATE.CODE","COUNTY.CODE","LOCALITY.ID","LOCALITY.TYPE",
                 "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
                 "FIRST.NAME","LAST.NAME","SAMPLING.EVENT.IDENTIFIER","PROTOCOL.TYPE","DURATION.MINUTES",
                 "EFFORT.DISTANCE.KM","NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER",
                 "VALID","REVIEWED")

data_day2 = data1 %>% filter(STATE.CODE %in% c("IN-AS"))


days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

# create a column "group.id" which can help remove duplicate checklists
data_day2 = data_day2 %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER))

data_day2 = data_day2 %>%
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

day1ext2 = data_day2 %>% filter(daym == 14, month == 4, cyear == 2022)
data_day2 = data_day2 %>% filter(daym == 15, month == 4, cyear == 2022)

temp = data_day2 %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  distinct(COMMON.NAME,SCIENTIFIC.NAME)

a = length(unique(data_day2$SAMPLING.EVENT.IDENTIFIER))
b = length(unique(data_day2$group.id))
c = length(unique(temp$COMMON.NAME))
d = length(unique(data_day2$COUNTY.CODE))
e = length(unique(data_day2$OBSERVER.ID))


temp = data_day2 %>%
  group_by(COUNTY.CODE) %>% summarize(n = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>%
  arrange(desc(n))



####################################################################### day 3

preimp = c("GLOBAL.UNIQUE.IDENTIFIER","category","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
           "LOCALITY.ID","LOCALITY.TYPE","reviewed","valid","STATE.CODE","COUNTY.CODE",
           "latitude","longitude","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
           "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","FIRST.NAME","LAST.NAME",
           "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")

nms = read.delim("India Records -2023_01_15-11_00_06_669-M2crI-10-1.tsv", nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

# read data from certain columns only
data1 = read.delim("India Records -2023_01_15-11_00_06_669-M2crI-10-1.tsv", colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ","null",NA))

names(data1) = c("GLOBAL.UNIQUE.IDENTIFIER","CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
                 "STATE.CODE","COUNTY.CODE","LOCALITY.ID","LOCALITY.TYPE",
                 "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
                 "FIRST.NAME","LAST.NAME","SAMPLING.EVENT.IDENTIFIER","PROTOCOL.TYPE","DURATION.MINUTES",
                 "EFFORT.DISTANCE.KM","NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER",
                 "VALID","REVIEWED")

data_day3 = data1 %>% filter(STATE.CODE %in% c("IN-AS"))


days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

# create a column "group.id" which can help remove duplicate checklists
data_day3 = data_day3 %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER))

data_day3 = data_day3 %>%
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

day1ext3 = data_day3 %>% filter(daym == 13, month == 1, cyear == 2022)
day2ext3 = data_day3 %>% filter(daym == 14, month == 1, cyear == 2022)
data_day3 = data_day3 %>% filter(daym == 15, month == 1, cyear == 2022)

temp = data_day3 %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  distinct(COMMON.NAME,SCIENTIFIC.NAME)

a = length(unique(data_day3$SAMPLING.EVENT.IDENTIFIER))
b = length(unique(data_day3$group.id))
c = length(unique(temp$COMMON.NAME))
d = length(unique(data_day3$COUNTY.CODE))
e = length(unique(data_day3$OBSERVER.ID))


temp = data_day3 %>%
  group_by(COUNTY.CODE) %>% summarize(n = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>%
  arrange(desc(n))




####################################################################### day 4

preimp = c("GLOBAL.UNIQUE.IDENTIFIER","category","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
           "LOCALITY.ID","LOCALITY.TYPE","reviewed","valid","STATE.CODE","COUNTY.CODE",
           "latitude","longitude","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
           "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","FIRST.NAME","LAST.NAME",
           "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")

nms = read.delim("India Records -2023_01_16-11_00_06_884-iDQxm-10-1.tsv", nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

# read data from certain columns only
data1 = read.delim("India Records -2022_01_16-11_00_08_567-DVEdQ-10-1.tsv", colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ","null",NA))

names(data1) = c("GLOBAL.UNIQUE.IDENTIFIER","CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
                 "STATE.CODE","COUNTY.CODE","LOCALITY.ID","LOCALITY.TYPE",
                 "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
                 "FIRST.NAME","LAST.NAME","SAMPLING.EVENT.IDENTIFIER","PROTOCOL.TYPE","DURATION.MINUTES",
                 "EFFORT.DISTANCE.KM","NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER",
                 "VALID","REVIEWED")

data_day4 = data1 %>% filter(STATE.CODE %in% c("IN-AS"))


days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

# create a column "group.id" which can help remove duplicate checklists
data_day4 = data_day4 %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER))

data_day4 = data_day4 %>%
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

day1ext4 = data_day4 %>% filter(daym == 13, month == 1, cyear == 2022)
day2ext4 = data_day4 %>% filter(daym == 14, month == 1, cyear == 2022)
day3ext4 = data_day4 %>% filter(daym == 15, month == 1, cyear == 2022)
data_day4 = data_day4 %>% filter(daym == 16, month == 1, cyear == 2022)

temp = data_day4 %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  distinct(COMMON.NAME,SCIENTIFIC.NAME)

a = length(unique(data_day4$SAMPLING.EVENT.IDENTIFIER))
b = length(unique(data_day4$group.id))
c = length(unique(temp$COMMON.NAME))
d = length(unique(data_day4$COUNTY.CODE))
e = length(unique(data_day4$OBSERVER.ID))


temp = data_day4 %>%
  group_by(COUNTY.CODE) %>% summarize(n = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>%
  arrange(desc(n))



########## combined_temp

datacomb = rbind(data_day1,data_day2,data_day3,data_day4,
                 day1ext2,day1ext3,day2ext3,day1ext4,day2ext4,day3ext4)

temp = datacomb %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  distinct(COMMON.NAME,SCIENTIFIC.NAME)

a = length(unique(datacomb$SAMPLING.EVENT.IDENTIFIER))
b = length(unique(datacomb$group.id))
c = length(unique(temp$COMMON.NAME))
d = length(unique(datacomb$COUNTY.CODE))
e = length(unique(datacomb$OBSERVER.ID))

temp = datacomb %>%
  group_by(COUNTY.CODE) %>% summarize(n = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>%
  arrange(desc(n))

temp1 = datacomb %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  group_by(COUNTY.CODE) %>% summarize(n = n_distinct(COMMON.NAME)) %>% arrange(desc(n))

temp2a = datacomb %>%
  group_by(daym) %>% summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER))

temp2b = datacomb %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  group_by(daym) %>% summarize(species = n_distinct(COMMON.NAME))

temp2 = left_join(temp2a,temp2b)
