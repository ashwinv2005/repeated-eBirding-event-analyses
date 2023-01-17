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

nms = read.delim("BBC1.tsv", nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

# read data from certain columns only
data1 = read.delim("BBC1.tsv", colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ","null",NA))
data2 = read.delim("BBC2.tsv", colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ","null",NA))
data3 = read.delim("BBC3.tsv", colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ","null",NA))
data4 = read.delim("BBC4.tsv", colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ","null",NA))
data5 = read.delim("BBC5.tsv", colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ","null",NA))


# merge both data frames
data1 = rbind(data1,data2,data3,data4,data5)
names(data1) = c("GLOBAL.UNIQUE.IDENTIFIER","CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
                 "STATE.CODE","COUNTY.CODE","LOCALITY.ID","LOCALITY.TYPE",
                 "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
                 "FIRST.NAME","LAST.NAME","SAMPLING.EVENT.IDENTIFIER","PROTOCOL.TYPE","DURATION.MINUTES",
                 "EFFORT.DISTANCE.KM","NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER",
                 "VALID","REVIEWED")



# TAMIL NADU
data = data1 %>% filter(STATE.CODE %in% c("IN-TN","IN-PY"))
data = data %>% filter(!is.na(COUNTY.CODE)) %>% filter(!COUNTY.CODE %in% c("IN-PY-YA","IN-PY-MA"))
data = data %>% filter(OBSERVATION.DATE %in% c("2021-01-14","2021-01-15","2021-01-16","2021-01-17"))

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

write.csv(temp,"PBCspecieslist2021.csv", row.names=FALSE)


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

############ Top 10 checklist uploaders

datax = data %>% filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14.5)
datax = datax %>%
  group_by(FIRST.NAME,LAST.NAME) %>% summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% ungroup %>%
  arrange(desc(lists)) %>% slice(1:10)

############ birders per district

datax = data %>%
  distinct(DISTRICT,FIRST.NAME,LAST.NAME) %>%
  arrange(DISTRICT)
write.csv(datax,"PBCbirdersperdistrict2021.csv", row.names=FALSE)

############ district level table of checklists

datax = data %>%
  group_by(DISTRICT) %>% summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>%
  arrange(desc(lists))
write.csv(datax,"PBClistsperdistrict2021.csv", row.names=FALSE)

############ daywise checklists

datax = data %>%
  group_by(daym) %>% summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER), birders = n_distinct(OBSERVER.ID)) %>%
  arrange(desc(lists))
write.csv(datax,"PBClistsperday2021.csv", row.names=FALSE)

############ day and district wise checklists

datax = data %>%
  group_by(DISTRICT,daym) %>% summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER), birders = n_distinct(OBSERVER.ID)) %>%
  arrange(DISTRICT,daym)
write.csv(datax,"PBClistsperdaydistrict2021.csv", row.names=FALSE)

############# common species

datax = data %>%
  group_by(COMMON.NAME,group.id) %>% slice(1) %>% ungroup %>%
  mutate(lists = n_distinct(group.id)) %>%
  group_by(COMMON.NAME) %>% summarize(freq = n()/max(lists)) %>%
  arrange(desc(freq))
write.csv(datax,"PBCcommonspecies2021.csv", row.names=FALSE)



######################################### Plot district map with info

districtmap1 = gBuffer(districtmap1, byid=TRUE, width=0)
finalmap = districtmap1[districtmap1@data$ST_NM %in% c("TAMIL NADU","PUDUCHERRY") & 
                          !districtmap1@data$DISTRICT %in% c("Mahe","Yanam"),]


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

a = mapView(effortmap, zcol = c("Checklists"), map.types = c("Esri.WorldImagery"),
            layer.name = c("Checklists - Districts"), 
            popup = leafpop::popupTable(effortmap,c("DISTRICT","Checklists",
                                                    "Participants","Locations","Species"), 
                                        feature.id=FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,250,max(na.omit(effortmap$Checklists)))), alpha.regions = 0.6)
mapshot(a, "PBC_districts_2021.html")

############################ Common species

west = c("Vellore","Tiruvannamalai","Krishnagiri","Dharmapuri","Salem","Namakkal","Erode",
         "The Nilgiris","Tirupathur")

central = c("Perambalur","Karur","Dindigul","Thiruvarur","Coimbatore","Tiruppur","Tiruchirappalli")

south = c("Theni","Madurai","Sivaganga","Pudukkottai","Ramanathapuram","Virudhunagar","Thoothukkudi",
          "Tirunelveli","Kanniyakumari","Tenkasi")

east = c("Tiruvallur","Chennai","Kancheepuram","Viluppuram","Cuddalore","Nagapattinam",
         "Kallakurichi","Ariyalur","Thanjavur","Chengalpattu","Ranipet","Thiruvarur","Puducherry","Karaikal")

data2 = data %>%
  filter(!is.na(DISTRICT)) %>%
  mutate(region = "") %>%
  mutate(region = ifelse(DISTRICT %in% west, "West", region)) %>%
  mutate(region = ifelse(DISTRICT %in% central, "Central", region)) %>%
  mutate(region = ifelse(DISTRICT %in% south, "South", region)) %>%
  mutate(region = ifelse(DISTRICT %in% east, "East", region)) %>%
  filter(region != "")

specs = data2 %>%
  filter(CATEGORY %in% c("species","issf")) %>%
  group_by(region) %>% summarize(nspecies = n_distinct(COMMON.NAME))

lists = data2 %>%
  group_by(region) %>% summarize(checklists = n_distinct(SAMPLING.EVENT.IDENTIFIER))

regionsummary = left_join(lists,specs)

write.csv(regionsummary,"PBC_REGIONsummary2021.csv",row.names = F)

cosp = data2 %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(DISTRICT) %>% mutate(distlist = n_distinct(group.id)) %>% ungroup %>%
  filter(distlist > 5) %>%
  group_by(COMMON.NAME,region,DISTRICT) %>% summarize(freq = n_distinct(group.id)/max(distlist)) %>% ungroup %>%
  group_by(region) %>% mutate(ndist = n_distinct(DISTRICT)) %>% ungroup %>%
  group_by(COMMON.NAME,region) %>% summarize(freq = sum(freq)/max(ndist)) %>% ungroup %>%
  group_by(region) %>% arrange(desc(freq), .by_group = T) %>% ungroup %>%
  group_by(region) %>% slice(1:5)

write.csv(cosp,"PBC_CommonSpeciesbyRegion2021.csv",row.names = F)

################################### plot points on map


data4 = data %>% distinct(LOCALITY.ID,LATITUDE,LONGITUDE)

ggp = ggplot() +
  geom_polygon(data = statemap1[statemap1@data$ST_NM %in% c("TAMIL NADU"),], aes(x=long, y=lat, group=group), colour = 'white', fill = "black")+  
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

n1 = "PBCpointmap2021.jpg"

print(ggp)
ggsave(file=n1, units="in", width=8, height=11)


################################### plot points on district map


data4 = data %>% distinct(LOCALITY.ID,LATITUDE,LONGITUDE)

ggp = ggplot() +
  geom_polygon(data = districtmap1[districtmap1@data$ST_NM %in% c("TAMIL NADU","PUDUCHERRY") & 
                                     !districtmap1@data$DISTRICT %in% c("Mahe","Yanam"),], aes(x=long, y=lat, group=group), colour = 'black', fill = "white")+  
  geom_point(data = data, aes(x=LONGITUDE,y=LATITUDE), colour = "blue", size = 1, alpha = 0.3) +
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
        plot.background = element_rect(fill = "white",colour = NA),
        panel.background = element_rect(fill = "white", colour = NA))+
  coord_map()

n1 = "PBCpointdistrictmap2021.jpg"

print(ggp)
ggsave(file=n1, units="in", width=8, height=11)


################################################## plot region map

require(ggthemes)
theme_set(theme_tufte())

fdistrictmap = fortify(districtmap1[districtmap1@data$ST_NM == "TAMIL NADU",], region = c("DISTRICT"))
data5 = data2 %>% distinct(DISTRICT,region)
#extra = data5[1:2,]
#extra$DISTRICT = c("")
#data5 = rbind(data5,extra)
fdistrictmap = left_join(fdistrictmap,data5,by = c('id' = "DISTRICT"))
fdistrictmap = fdistrictmap %>% filter(!is.na(region))

cols = c("#869B27", "#E49B36", "#A13E2B", "#78CAE0", "#B69AC9", "#EA5599", "#31954E", "#493F3D",
         "#CC6666", "#9999CC", "#000000", "#66CC99")

ns = 4

cols1 = cols[c(1:ns)]
bks1 = c("West","Central","South","East")

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

n1 = "PBCregionmap2021.jpg"

print(ggp)
ggsave(file=n1, units="in", width=10, height=7)


########### overall stats

participants = length(unique(data$OBSERVER.ID))
checklists = length(unique(data$SAMPLING.EVENT.IDENTIFIER))
uniquechecklists = length(unique(data$group.id))          
species = length(unique(data[data$CATEGORY %in% c("species","issf"),]$COMMON.NAME))

