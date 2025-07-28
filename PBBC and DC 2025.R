##################################### full data

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

load("maps_sf.RData")

preimp = c("GLOBAL.UNIQUE.IDENTIFIER","CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
           "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","STATE","COUNTY",
           "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
           "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","LOCALITY","FIRST.NAME","LAST.NAME",
           "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER","HAS.MEDIA")

nms = read.delim("ebd_IN_202503_202504_relApr-2025.txt", nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

# read data from certain columns only
data1 = read.delim("ebd_IN_202503_202504_relApr-2025.txt", colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ","null",NA))


############ Add first and last name

data1$obs.id.num <- gsub("[[:alpha:]]", "", data1$OBSERVER.ID)

eBird.users = read.delim("ebd_users_relFeb-2025.txt", sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                         na.strings = c(""," ",NA))
names(eBird.users) = c("USER_ID","FIRST.NAME","LAST.NAME")
eBird.users$obs.id.num = gsub("[[:alpha:]]", "", eBird.users$USER_ID)
data1 = left_join(data1, eBird.users) %>% filter(obs.id.num != "1927965")


data1$FIRST.NAME[data1$obs.id.num == "2847891"] = "SUBRATA"
data1$LAST.NAME[data1$obs.id.num == "2847891"] = "PAL"
data1$FIRST.NAME[data1$obs.id.num == "6919064"] = "Biswapriya"
data1$LAST.NAME[data1$obs.id.num == "6919064"] = "Ghar"
data1$FIRST.NAME[data1$obs.id.num == "1351355"] = "Arkapal"
data1$LAST.NAME[data1$obs.id.num == "1351355"] = "Banerjee"
data1$FIRST.NAME[data1$obs.id.num == "2245790"] = "Sukanta"
data1$LAST.NAME[data1$obs.id.num == "2245790"] = "Banerjee"
data1$FIRST.NAME[data1$obs.id.num == "7026821"] = "Nehal"
data1$LAST.NAME[data1$obs.id.num == "7026821"] = "Subba"
data1$FIRST.NAME[data1$obs.id.num == "1263376"] = "Subham"
data1$LAST.NAME[data1$obs.id.num == "1263376"] = "Banerjee"
data1$FIRST.NAME[data1$obs.id.num == "4564240"] = "Ashutosh"
data1$LAST.NAME[data1$obs.id.num == "4564240"] = "Ghosh"
data1$FIRST.NAME[data1$obs.id.num == "3717966"] = "Yosita"
data1$LAST.NAME[data1$obs.id.num == "3717966"] = "Laha"
data1$FIRST.NAME[data1$obs.id.num == "4699655"] = "Sourav"
data1$LAST.NAME[data1$obs.id.num == "4699655"] = "Sarkar"
data1$FIRST.NAME[data1$obs.id.num == "6818903"] = "Sandeep"
data1$LAST.NAME[data1$obs.id.num == "6818903"] = "Bhardwaj"
data1$FIRST.NAME[data1$obs.id.num == "5865746"] = "Prakash"
data1$LAST.NAME[data1$obs.id.num == "5865746"] = "Chuktay Sherpa"
data1$FIRST.NAME[data1$obs.id.num == "7045566"] = "Soma"
data1$LAST.NAME[data1$obs.id.num == "7045566"] = "Sinha"
data1$FIRST.NAME[data1$obs.id.num == "7033463"] = "Tanmoy"
data1$LAST.NAME[data1$obs.id.num == "7033463"] = "Paul"
data1$FIRST.NAME[data1$obs.id.num == "6591163"] = "Arijit"
data1$LAST.NAME[data1$obs.id.num == "6591163"] = "Roy"
data1$FIRST.NAME[data1$obs.id.num == "6818736"] = "Puja"
data1$LAST.NAME[data1$obs.id.num == "6818736"] = "Midya"
data1$FIRST.NAME[data1$obs.id.num == "6784431"] = "Rina"
data1$LAST.NAME[data1$obs.id.num == "6784431"] = "Pandey"
data1$FIRST.NAME[data1$obs.id.num == "7064645"] = "Yuhina"
data1$LAST.NAME[data1$obs.id.num == "7064645"] = "Mandal"
data1$FIRST.NAME[data1$obs.id.num == "4952478"] = "Wesley"
data1$LAST.NAME[data1$obs.id.num == "4952478"] = "Van zadelhoff"
data1$FIRST.NAME[data1$obs.id.num == "7034308"] = "Rajarshee"
data1$LAST.NAME[data1$obs.id.num == "7034308"] = "Banerjee"
data1$FIRST.NAME[data1$obs.id.num == "2772316"] = "Abhishek"
data1$LAST.NAME[data1$obs.id.num == "2772316"] = "Das"
data1$FIRST.NAME[data1$obs.id.num == "6744220"] = "Arunashankar"
data1$LAST.NAME[data1$obs.id.num == "6744220"] = "Deb"


## choosing important columns required for further analyses

# PBBC
data = data1 %>% filter(STATE == "West Bengal", OBSERVATION.DATE %in% c("2025-04-12","2025-04-13","2025-04-14","2025-04-15"))

# no of days in every month, and cumulative number
days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

# create a column "group.id" which can help remove duplicate checklists
data = data %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER))

dataPBBC = data %>%
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

# create and write a file with common names and scientific names of all species
# useful for mapping
temp = dataPBBC %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  distinct(COMMON.NAME,SCIENTIFIC.NAME)

write.csv(temp,"PBBC2025specieslist.csv", row.names=FALSE)
write.csv(dataPBBC,"dataPBBC2025.csv", row.names=FALSE)





# DC
data = data1 %>% filter(OBSERVATION.DATE %in% c("2025-03-23"))

# no of days in every month, and cumulative number
days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

# create a column "group.id" which can help remove duplicate checklists
data = data %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER))

dataDC = data %>%
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

withaudio = read.csv("ML__2025-05-18T11-05_audio_IN.csv") %>%
  select(Common.Name, Day, eBird.Checklist.ID) %>%
  rename(COMMON.NAME = Common.Name,
         daym = Day,
         SAMPLING.EVENT.IDENTIFIER = eBird.Checklist.ID) %>%
  filter(daym == 23)

dataDC = dataDC %>%
  filter(SAMPLING.EVENT.IDENTIFIER %in% withaudio$SAMPLING.EVENT.IDENTIFIER)

time_parts = strsplit(dataDC$TIME.OBSERVATIONS.STARTED,":")

# Assign the split values to three new columns
dataDC$hours <- as.numeric(sapply(time_parts, function(x) x[1]))
dataDC$minutes <- as.numeric(sapply(time_parts, function(x) x[2]))
dataDC$seconds <- as.numeric(sapply(time_parts, function(x) x[3]))

# Calculate the total time in seconds
dataDC = dataDC %>%
  mutate(total_seconds = hours * 3600 + minutes * 60 + seconds)

# Start before 8:30
dataDC = dataDC %>%
  filter(total_seconds < 3600*8+30*60)
withaudio = withaudio %>%
  filter(SAMPLING.EVENT.IDENTIFIER %in% dataDC$SAMPLING.EVENT.IDENTIFIER) %>%
  mutate(hasaudio = 1)
dataDC = dataDC %>% left_join(withaudio)

# create and write a file with common names and scientific names of all species
# useful for mapping
temp = dataDC %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  filter(hasaudio == 1) %>%
  distinct(COMMON.NAME,SCIENTIFIC.NAME)

write.csv(temp,"DC2025specieslistwithaudio.csv", row.names=FALSE)
write.csv(dataDC,"dataDC2025.csv", row.names=FALSE)



## PBBC

############ Top 10 checklist uploaders

datax = dataPBBC %>% filter(ALL.SPECIES.REPORTED == 1)
datax = datax %>%
  group_by(obs.id.num,FIRST.NAME,LAST.NAME) %>% summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% ungroup %>%
  arrange(desc(lists)) %>% slice(1:10)
write.csv(datax,"PBBCtop10uploaders2025.csv", row.names=FALSE)

############ birders per district

datax = dataPBBC %>%
  distinct(COUNTY,obs.id.num,FIRST.NAME,LAST.NAME) %>%
  arrange(COUNTY)
write.csv(datax,"PBBCbirdersperdistrict2025.csv", row.names=FALSE)

############ district level table of checklists

datax = dataPBBC %>%
  group_by(COUNTY) %>% summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>%
  arrange(desc(lists))
write.csv(datax,"PBBClistsperdistrict2025.csv", row.names=FALSE)

########### overall stats

participants = length(unique(dataPBBC$OBSERVER.ID))
checklists = length(unique(dataPBBC$SAMPLING.EVENT.IDENTIFIER))
uniquechecklists = length(unique(dataPBBC$group.id))          
species = length(unique(dataPBBC[dataPBBC$CATEGORY %in% c("species","issf"),]$COMMON.NAME))

df = data.frame(metrics = c("participants","checklists","uniquechecklists","species"),
                values = c(participants,checklists,uniquechecklists,species))
write.csv(df,"PBBCoverallstats2025.csv", row.names=FALSE)


################################### plot points on map


dataPBBC1 = dataPBBC %>% distinct(LOCALITY.ID,LATITUDE,LONGITUDE)

ggp = ggplot() +
  geom_sf(data = states_sf %>% filter(STATE.NAME == "West Bengal"), colour = 'white', fill = "black")+  
  geom_point(data = dataPBBC1, aes(x=LONGITUDE,y=LATITUDE), colour = "#dcc343", size = 2, alpha = 1) +
  #scale_x_continuous(expand = c(0,0)) +
  #scale_y_continuous(expand = c(0,0)) +
  theme_bw()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        #panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "black",colour = NA),
        panel.background = element_rect(fill = "black", colour = NA))+
  coord_sf()

n1 = "PBBCpointmap2025.jpg"

print(ggp)
ggsave(file=n1, units="in", width=8, height=11)



## DC

############ Top 10 checklist uploaders

datax = dataDC %>% filter(ALL.SPECIES.REPORTED == 1)
datax = datax %>%
  group_by(obs.id.num,FIRST.NAME,LAST.NAME) %>% summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% ungroup %>%
  arrange(desc(lists)) %>% slice(1:10)
write.csv(datax,"DCtop10uploaders2025.csv", row.names=FALSE)

########## Top audio uploaders

datax = dataDC %>% filter(!is.na(hasaudio))
datax = datax %>%
  group_by(obs.id.num,FIRST.NAME,LAST.NAME) %>% summarize(obs = n_distinct(GLOBAL.UNIQUE.IDENTIFIER)) %>% ungroup %>%
  arrange(desc(obs))
write.csv(datax,"DCtopaudiouploaders2025.csv", row.names=FALSE)

############ birders per state

datax = dataDC %>%
  distinct(STATE,obs.id.num,FIRST.NAME,LAST.NAME) %>%
  arrange(STATE)
write.csv(datax,"DCbirdersperstate2025.csv", row.names=FALSE)

############ state level table of checklists

datax = dataDC %>%
  group_by(STATE) %>% summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>%
  arrange(desc(lists))
write.csv(datax,"DClistsperstate2025.csv", row.names=FALSE)

########### overall stats

participants = length(unique(dataDC$OBSERVER.ID))
checklists = length(unique(dataDC$SAMPLING.EVENT.IDENTIFIER))
uniquechecklists = length(unique(dataDC$group.id))          
species = length(unique(dataDC[dataDC$CATEGORY %in% c("species","issf"),]$COMMON.NAME))
obswithaudio = length(dataDC[!is.na(dataDC$hasaudio),]$COMMON.NAME)
specieswithaudio = length(unique(dataDC[!is.na(dataDC$hasaudio),]$COMMON.NAME))


df = data.frame(metrics = c("participants","checklists","uniquechecklists","species","obswithaudio","specieswithaudio"),
                values = c(participants,checklists,uniquechecklists,species,obswithaudio,specieswithaudio))
write.csv(df,"DCoverallstats2025.csv", row.names=FALSE)


################################### plot points on map


dataDC1 = dataDC %>% distinct(LOCALITY.ID,LATITUDE,LONGITUDE)

ggp = ggplot() +
  geom_sf(data = states_sf, colour = 'white', fill = "black")+  
  geom_point(data = dataDC1, aes(x=LONGITUDE,y=LATITUDE), colour = "#dcc343", size = 2, alpha = 1) +
  #scale_x_continuous(expand = c(0,0)) +
  #scale_y_continuous(expand = c(0,0)) +
  theme_bw()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        #panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "black",colour = NA),
        panel.background = element_rect(fill = "black", colour = NA))+
  coord_sf()

n1 = "DCpointmap2025.jpg"

print(ggp)
ggsave(file=n1, units="in", width=8, height=11)