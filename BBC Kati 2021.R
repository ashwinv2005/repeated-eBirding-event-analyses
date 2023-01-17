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

preimp = c("GLOBAL.UNIQUE.IDENTIFIER","CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
           "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","STATE.CODE","COUNTY.CODE",
           "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
           "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","LOCALITY","FIRST.NAME","LAST.NAME",
           "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")

nms = read.delim("ebd_IN_202110_202110_relOct-2021.txt", nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

# read data from certain columns only
data1 = read.delim("ebd_IN_202110_202110_relOct-2021.txt", colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ","null",NA))

## choosing important columns required for further analyses

# ASSAM
data = data1 %>% filter(STATE.CODE %in% c("IN-AS"))
data = data %>% filter(OBSERVATION.DATE %in% c("2021-10-15","2021-10-16","2021-10-17","2021-10-18"))

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

# create and write a file with common names and scientific names of all species
# useful for mapping
temp = data %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  distinct(COMMON.NAME,SCIENTIFIC.NAME)

write.csv(temp,"BBCKatispecieslist2021.csv", row.names=FALSE)


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

eBird.users = read.delim("ebd_users_relJun-2021.txt", sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                         na.strings = c(""," ",NA))
names(eBird.users) = c("USER_ID","FIRST.NAME","LAST.NAME")
eBird.users$obs.id.num = gsub("[[:alpha:]]", "", eBird.users$USER_ID)

data = left_join(data, eBird.users)
data$FIRST.NAME[data$obs.id.num == "2801049"] = "Rahul"
data$LAST.NAME[data$obs.id.num == "2801049"] = "Sarma"
data$FIRST.NAME[data$obs.id.num == "862699"] = "Genius"
data$LAST.NAME[data$obs.id.num == "862699"] = "Teron"
data$FIRST.NAME[data$obs.id.num == "2550952"] = "Jugal"
data$LAST.NAME[data$obs.id.num == "2550952"] = "Borah"
data$FIRST.NAME[data$obs.id.num == "2781521"] = "Pompy"
data$LAST.NAME[data$obs.id.num == "2781521"] = "Basumatary"
data$FIRST.NAME[data$obs.id.num == "2799182"] = "Deepjyoti"
data$LAST.NAME[data$obs.id.num == "2799182"] = "Daa"
data$FIRST.NAME[data$obs.id.num == "2800974"] = "Preetika"
data$LAST.NAME[data$obs.id.num == "2800974"] = "Kemprai"
data$FIRST.NAME[data$obs.id.num == "2180395"] = "Nekib"
data$LAST.NAME[data$obs.id.num == "2180395"] = "Ali"
data$FIRST.NAME[data$obs.id.num == "2483994"] = "Tomal"
data$LAST.NAME[data$obs.id.num == "2483994"] = "Gogoi"
data$FIRST.NAME[data$obs.id.num == "2122347"] = "NABADEEP"
data$LAST.NAME[data$obs.id.num == "2122347"] = "DAS"
data$FIRST.NAME[data$obs.id.num == "2795774"] = "Mukta"
data$LAST.NAME[data$obs.id.num == "2795774"] = "Chandra Das"
data$FIRST.NAME[data$obs.id.num == "1208831"] = "Angad"
data$LAST.NAME[data$obs.id.num == "1208831"] = "Singh"
data$FIRST.NAME[data$obs.id.num == "1524039"] = "Pallab"
data$LAST.NAME[data$obs.id.num == "1524039"] = "Saikia"
data$FIRST.NAME[data$obs.id.num == "2642527"] = "BABLU"
data$LAST.NAME[data$obs.id.num == "2642527"] = "DEY"



############ Top 10 checklist uploaders

datax = data %>% filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14)
datax = datax %>%
  group_by(obs.id.num,FIRST.NAME,LAST.NAME) %>% summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% ungroup %>%
  arrange(desc(lists)) %>% slice(1:10)

############ birders per district

datax = data %>%
  distinct(DISTRICT,FIRST.NAME,LAST.NAME) %>%
  arrange(DISTRICT)
write.csv(datax,"BBCKatibirdersperdistrict2021.csv", row.names=FALSE)

############ district level table of checklists

datax = data %>%
  group_by(DISTRICT) %>% summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>%
  arrange(desc(lists))
write.csv(datax,"BBCKatilistsperdistrict2021.csv", row.names=FALSE)

############ daywise checklists

datax = data %>%
  group_by(daym) %>% summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER), birders = n_distinct(OBSERVER.ID)) %>%
  arrange(desc(lists))
write.csv(datax,"BBCKatilistsperday2021.csv", row.names=FALSE)

############ day and district wise checklists

datax = data %>%
  group_by(DISTRICT,daym) %>% summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER), birders = n_distinct(OBSERVER.ID)) %>%
  arrange(DISTRICT,daym)
write.csv(datax,"BBCKatilistsperdaydistrict2021.csv", row.names=FALSE)


######################################### Plot district map with info

districtmap1 = gBuffer(districtmap1, byid=TRUE, width=0)
finalmap = districtmap1[districtmap1@data$ST_NM == "ASSAM",]


temp = data %>% 
  filter(CATEGORY %in% c("species","issf")) %>%
  group_by(DISTRICT) %>%
  summarize(Species = n_distinct(COMMON.NAME))

temp1 = data %>% 
  group_by(DISTRICT) %>%
  summarize(Checklists = n_distinct(SAMPLING.EVENT.IDENTIFIER))

temp2 = data %>% 
  group_by(DISTRICT) %>%
  summarize(Participants = n_distinct(OBSERVER.ID))

temp3 = data %>% 
  group_by(DISTRICT) %>%
  summarize(Locations = n_distinct(LOCALITY.ID))

temp = left_join(temp,temp1)
temp = left_join(temp,temp2)
temp = left_join(temp,temp3)


temp$DISTRICT = as.character(temp$DISTRICT)

effortmap = merge(finalmap,temp, by =  "DISTRICT")


proj4string(finalmap) = "+proj=longlat +datum=WGS84"
proj4string(effortmap) = "+proj=longlat +datum=WGS84"

mapviewOptions(fgb = FALSE)
a = mapView(effortmap, zcol = c("Checklists"), map.types = c("Esri.WorldImagery"),
            layer.name = c("Checklists - Districts"), 
            popup = leafpop::popupTable(effortmap,c("DISTRICT","Checklists",
                                                    "Participants","Locations","Species"), 
                                        feature.id=FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,5,10,20,30,max(na.omit(effortmap$Checklists)))), alpha.regions = 0.6)
mapshot(a, "BBCKati_districts_2021.html")

############################ Common species

west = c("Dhubri","South Salmara Mancachar","Kokrajhar","Goalpara","Bongaigaon","Chirang","Baksa","Barpeta",
         "Nalbari","Kamrup")

central = c("Kamrup Metropolitan","Darrang","Udalguri","Morigaon","Nagaon","Sonitpur","Biswanath")

south = c("West Karbi Anglong","Hojai","Karbi Anglong","Dima Hasao","Cachar","Karimganj","Hailakandi","Golaghat")

east = c("Lakhimpur","Majuli","Jorhat","Sivasagar","Dhemaji","Dibrugarh","Charaideo","Tinsukia")

#North Assam Division
north = c("Udalguri", "Darrang", "Sonitpur", "Biswanath", "Lakhimpur",  "Dhemaji")
#Lower Assam Division
lower = c("Dhubri", "Kokrajhar", "Bongaigaon", "Goalpara", "Baksa", "Chirang", "Barpeta", "Nalbari", "Kamrup", 
          "Kamrup Metropolitan", "South Salmara Mankachar")
#Central Assam Division
central = c("Dima Hasao", "Karbi Anglong", "West Karbi Anglong", "Nagaon", "Morigaon", "Hojai")
#Upper Assam Division
upper = c("Dibrugarh", "Tinsukia", "Sivasagar", "Jorhat", "Golaghat", "Charaideo", "Majuli")
#Barak Valley Division
barak = c("Cachar", "Hailakandi", "Karimganj")

data2 = data %>%
  filter(!is.na(DISTRICT)) %>%
  mutate(region = "") %>%
  mutate(region = ifelse(DISTRICT %in% north, "North Assam Division", region)) %>%
  mutate(region = ifelse(DISTRICT %in% lower, "Lower Assam Division", region)) %>%
  mutate(region = ifelse(DISTRICT %in% central, "Central Assam Division", region)) %>%
  mutate(region = ifelse(DISTRICT %in% upper, "Upper Assam Division", region)) %>%
  mutate(region = ifelse(DISTRICT %in% barak, "Barak Valley Division", region)) %>%
  filter(region != "")

specs = data2 %>%
  filter(CATEGORY %in% c("species","issf")) %>%
  group_by(region) %>% summarize(nspecies = n_distinct(COMMON.NAME))

lists = data2 %>%
  group_by(region) %>% summarize(checklists = n_distinct(SAMPLING.EVENT.IDENTIFIER))

regionsummary = left_join(lists,specs)

write.csv(regionsummary,"BBCKati_REGIONsummary2021.csv",row.names = F)

cosp = data2 %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(DISTRICT) %>% mutate(distlist = n_distinct(group.id)) %>% ungroup %>%
  filter(distlist > 5) %>%
  group_by(COMMON.NAME,region,DISTRICT) %>% summarize(freq = n_distinct(group.id)/max(distlist)) %>% ungroup %>%
  group_by(region) %>% mutate(ndist = n_distinct(DISTRICT)) %>% ungroup %>%
  group_by(COMMON.NAME,region) %>% summarize(freq = sum(freq)/max(ndist)) %>% ungroup %>%
  group_by(region) %>% arrange(desc(freq), .by_group = T) %>% ungroup %>%
  group_by(region) %>% slice(1:5)

write.csv(cosp,"BBCKati_CommonSpeciesbyRegion2021.csv",row.names = F)

################################### plot points on map


data4 = data %>% distinct(LOCALITY.ID,LATITUDE,LONGITUDE)

ggp = ggplot() +
  geom_polygon(data = statemap1[statemap1@data$ST_NM == "ASSAM",], aes(x=long, y=lat, group=group), colour = 'white', fill = "black")+  
  geom_point(data = data, aes(x=LONGITUDE,y=LATITUDE), colour = "#dcc343", size = 1, alpha = 0.3) +
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
  coord_map()

n1 = "BBCKatipointmap2021.jpg"

print(ggp)
ggsave(file=n1, units="in", width=8, height=11)


################################################## plot region map

require(ggthemes)
theme_set(theme_tufte())

fdistrictmap = fortify(districtmap1[districtmap1@data$ST_NM == "ASSAM",], region = c("DISTRICT"))
data5 = data2 %>% distinct(DISTRICT,region)
alldists = c(north,lower,central,upper,barak)
diff = setdiff(alldists,data5$DISTRICT)
extra = data5[1:length(diff),]
extra$region = c("Lower Assam Division","Central Assam Division","Central Assam Division","Upper Assam Division",
                 "Upper Assam Division","Barak Valley Division")
extra$DISTRICT = diff
data5 = rbind(data5,extra)
fdistrictmap = left_join(fdistrictmap,data5,by = c('id' = "DISTRICT"))
fdistrictmap = fdistrictmap %>% filter(!is.na(region))

cols = c("#869B27", "#E49B36", "#A13E2B", "#78CAE0", "#B69AC9", "#EA5599", "#31954E", "#493F3D",
         "#CC6666", "#9999CC", "#000000", "#66CC99")

ns = 5

cols1 = cols[c(1:ns)]
bks1 = c("North Assam Division","Lower Assam Division","Central Assam Division","Upper Assam Division",
         "Barak Valley Division")

plotindiamap = ggplot() +
  #geom_polygon(data = indiamap, aes(x=long, y=lat, group=group), colour = NA, fill = NA)+  
  geom_polygon(data = fdistrictmap, aes(x=long, y=lat,group=group,fill=region,colour=region))+  
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank())+
  scale_fill_manual(breaks = bks1, values = cols1)+
  scale_colour_manual(breaks = bks1, values = cols1)+
  coord_map()

ggp = plotindiamap +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12))

n1 = "BBCKatiregionmap2021.jpg"

print(ggp)
ggsave(file=n1, units="in", width=10, height=7)


########### overall stats

participants = length(unique(data$OBSERVER.ID))
checklists = length(unique(data$SAMPLING.EVENT.IDENTIFIER))
uniquechecklists = length(unique(data$group.id))          
species = length(unique(data[data$CATEGORY %in% c("species","issf"),]$COMMON.NAME))