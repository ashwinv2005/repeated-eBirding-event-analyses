################################# setup old gbbc data

require(lubridate)
require(tidyverse)
require(stringr)
load("GBBC-India-Allyears.RData")

imp = c("PRIMARY_COM_NAME","HOW_MANY_ATLEAST",
        "LOC_ID","NAME","SUBNATIONAL1_CODE",
        "LATITUDE","LONGITUDE", 
        "P_NAME","SUB_ID",
        "DURATION_HRS","EFFORT_DISTANCE_KM",
        "NUM_OBSERVERS","ALL_OBS_REPORTED","group.id","day","time","year")

map = read.csv("Map to Other Lists - map.csv")
gbbc.all = left_join(gbbc.all,map, by = c("COMMON.NAME" = "eBird.English.Name.2018"))
gbbc.all = gbbc.all %>% select(-COMMON.NAME)
names(gbbc.all)[55] = "COMMON.NAME"

gbbc.all = gbbc.all %>% select(SAMPLING.EVENT.IDENTIFIER,COMMON.NAME,OBSERVATION.COUNT,GROUP.IDENTIFIER,
                               PROTOCOL.TYPE,LOCALITY.ID,LOCALITY,STATE.CODE,DURATION.MINUTES,ALL.SPECIES.REPORTED,
                               NUMBER.OBSERVERS,EFFORT.DISTANCE.KM,LATITUDE,LONGITUDE,TIME.OBSERVATIONS.STARTED,
                               FULL.NAME,CATEGORY,OBSERVATION.DATE)

gbbc.all = gbbc.all %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         month = month(OBSERVATION.DATE),
         day = day(OBSERVATION.DATE), 
         #week = week(OBSERVATION.DATE),
         #fort = ceiling(day/14),
         year = year(OBSERVATION.DATE)) %>%
  dplyr::select(-c("OBSERVATION.DATE"))

sp1 = str_split_fixed(gbbc.all$TIME.OBSERVATIONS.STARTED,":",3)
gbbc.all$time = sp1[,1]

gbbc.all$DURATION.MINUTES = gbbc.all$DURATION.MINUTES/60

names(gbbc.all) = c("SUB_ID","PRIMARY_COM_NAME","HOW_MANY_ATLEAST","GROUP.IDENTIFIER","PROTOCOL.TYPE",
                    "LOC_ID","NAME","SUBNATIONAL1_CODE","DURATION_HRS","ALL_OBS_REPORTED","NUM_OBSERVERS",
                    "EFFORT_DISTANCE_KM","LATITUDE","LONGITUDE","TIME.OBSERVATIONS.STARTED","P_NAME", 
                    "CATEGORY","month","dayx","year","time")
gbbc.all$dayx = 1

gbbc.all = gbbc.all %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SUB_ID, GROUP.IDENTIFIER)) %>%
  group_by(year) %>% mutate(mday = (min(dayx)-1)) %>% ungroup %>%
  mutate(day = dayx-mday) %>%
  dplyr::select(imp) %>%
  ungroup

gbbc.all$PRIMARY_COM_NAME = as.character(gbbc.all$PRIMARY_COM_NAME)
gbbc.all$HOW_MANY_ATLEAST = as.numeric(gbbc.all$HOW_MANY_ATLEAST)



########################################


require(ggfortify)
require(rgdal)
require(sp)
require(sf)
require(rgeos)
require(mapview)
require(leaflet)

data = read.csv("2020 GBBC India_9 Mar 2020.csv", stringsAsFactors=F)
data$year = 2020

## choosing important variables

sp = str_split_fixed(data$TO_CHAR.S.OBS_DT..YYYY.MM.DDHH24.MI.SS.., " ", 2)
sp1 = str_split_fixed(sp[,2],":",3)
data$TO_CHAR.S.OBS_DT..YYYY.MM.DDHH24.MI.SS.. = sp[,1]
data$time = sp1[,1]

data = data %>% mutate(P_NAME = paste(FIRST_NAME, LAST_NAME, sep = " "))
#data = data %>% mutate(DISTRICT = paste(ST_NM, DISTRICT, sep = "-"))

data = data %>%
  mutate(group.id = ifelse(GROUP_ID == "", SUB_ID, GROUP_ID)) %>%
  mutate(day = as.numeric(as.factor(TO_CHAR.S.OBS_DT..YYYY.MM.DDHH24.MI.SS..))) %>%
  dplyr::select(imp) %>%
  ungroup


data = rbind(gbbc.all,data)

  
load("maps.RData")

districtmap$ST_NM = as.character(districtmap$ST_NM)
districtmap$ST_NM[339] = "Ladakh"
#districtmap$ST_NM[districtmap$DISTRICT == "Leh (Ladakh)"] = "Ladakh"
districtmap$DISTRICT = paste(districtmap$ST_NM,districtmap$DISTRICT,sep = "-")

t = districtmap[districtmap@data$ST_NM %in% c("Ladakh"),]
t@data = t@data %>% select(2)
statemap = statemap - districtmap[districtmap@data$ST_NM %in% c("Ladakh"),]
statemap = rbind(statemap,t)


# add columns with DISTRICT and ST_NM to main data 

temp = data %>% group_by(group.id) %>% slice(1) # same group ID, same grid/district/state 

rownames(temp) = temp$group.id # only to setup adding the group.id column for the future left_join
coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
#proj4string(temp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
temp = over(temp,districtmap) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
temp = data.frame(temp) # convert into data frame for left_join
temp$group.id = rownames(temp) # add column to join with the main data
data = left_join(temp,data)

data1 = data %>% filter(!grepl("sp.",PRIMARY_COM_NAME))
data1 = data1 %>% filter(!grepl("/",PRIMARY_COM_NAME))
data1 = data1 %>% filter(!grepl("Domestic",PRIMARY_COM_NAME))
data1 = data1 %>% filter(!grepl("Hybrid",PRIMARY_COM_NAME))

temp = data1 %>%
  group_by(PRIMARY_COM_NAME) %>% slice(1) %>% ungroup()

totalgbbc = temp$PRIMARY_COM_NAME

alltill2019 = data1 %>%
  filter(year < 2020) %>%
  group_by(PRIMARY_COM_NAME) %>% slice(1) %>% ungroup()
alltill2019 = alltill2019$PRIMARY_COM_NAME

all2019 = data1 %>%
  filter(year == 2019) %>%
  group_by(PRIMARY_COM_NAME) %>% slice(1) %>% ungroup()
all2019 = all2019$PRIMARY_COM_NAME

alltill2018 = data1 %>%
  filter(year < 2019) %>%
  group_by(PRIMARY_COM_NAME) %>% slice(1) %>% ungroup()
alltill2018 = alltill2018$PRIMARY_COM_NAME

extra2020 = setdiff(totalgbbc,alltill2019)
tableextra2020 = data %>% filter(PRIMARY_COM_NAME %in% extra2020, !PRIMARY_COM_NAME %in% "Indochinese Roller") %>% 
  select(PRIMARY_COM_NAME,P_NAME,ST_NM) %>%
  distinct(PRIMARY_COM_NAME,P_NAME,ST_NM)
tableextra2020$ST_NM[is.na(tableextra2020$ST_NM)] = "Karnataka"

write.csv(tableextra2020,"newspecies2020.csv",row.names = F)


#########################################

alldata = data
alldata1 = data1

data = data %>% filter(year == 2020)
data1 = data %>% filter(year == 2020)

######################################### Plot district map with info


finalmap = districtmap

temp = data1 %>% 
  group_by(DISTRICT) %>%
  summarize(Species = n_distinct(PRIMARY_COM_NAME))

temp1 = data %>% 
  group_by(DISTRICT) %>%
  summarize(Checklists = n_distinct(SUB_ID))

temp2 = data %>% 
  group_by(DISTRICT) %>%
  summarize(Participants = n_distinct(P_NAME))

temp3 = data %>% 
  group_by(DISTRICT) %>%
  summarize(Locations = n_distinct(LOC_ID))

temp = left_join(temp,temp1)
temp = left_join(temp,temp2)
temp = left_join(temp,temp3)


temp$DISTRICT = as.character(temp$DISTRICT)

effortmap = merge(finalmap,temp, by =  "DISTRICT")


proj4string(finalmap) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(effortmap) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

a = mapView(effortmap, zcol = c("Checklists"), map.types = c("Esri.WorldImagery"),
            layer.name = c("Checklists - Districts"), 
            popup = leafpop::popupTable(effortmap,c("DISTRICT","Checklists",
                                                    "Participants","Locations","Species"), 
                                        feature.id=FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,250,1000,2500,max(na.omit(effortmap$Checklists)))), alpha.regions = 0.6)
mapshot(a, "GBBC_districts_2020.html")


######################################### Plot state map with info

finalmap = statemap

temp = data1 %>% 
  group_by(ST_NM) %>%
  summarize(Species = n_distinct(PRIMARY_COM_NAME))

temp1 = data %>% 
  group_by(ST_NM) %>%
  summarize(Checklists = n_distinct(SUB_ID))

temp2 = data %>% 
  group_by(ST_NM) %>%
  summarize(Participants = n_distinct(P_NAME))

temp3 = data %>% 
  group_by(ST_NM) %>%
  summarize(Locations = n_distinct(LOC_ID))

temp = left_join(temp,temp1)
temp = left_join(temp,temp2)
temp = left_join(temp,temp3)


temp$ST_NM = as.character(temp$ST_NM)
temp$GBBCweb = '<a href = "https://ebird.org/gbbc/region/IN/regions?yr=EBIRD_GBBC_2020&m="> Click on the State for more information </a>'

effortmap = merge(finalmap,temp, by =  "ST_NM")


proj4string(finalmap) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(effortmap) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

b = mapView(effortmap, zcol = c("Checklists"), map.types = c("Esri.WorldImagery"),
            layer.name = c("Checklists - States"), 
            popup = leafpop::popupTable(effortmap,c("ST_NM","Checklists",
                                                    "Participants","Locations","Species","GBBCweb"), 
                                        feature.id=FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,20,100,200,500,2000,5000,max(na.omit(effortmap$Checklists)))), alpha.regions = 0.6)
mapshot(b, "GBBC_states_2020.html")

c = a + b

mapshot(c, "GBBC_states_districts_2020.html")

########################## add separate items, photos, video, etc. to GBBC
########################################################

#dat1 = tibble(site = c("Bangalore", "Delhi", "Jammu"),
#             url_1 = c("https://birdcount.in/", 
#                       "https://ebird.org/india/home", 
#                       "https://gbbc.birdcount.org/"),
#             coord_y = c(12.9716, 28.7041, 32.7266),
#             coord_x = c(77.5946, 77.1025, 74.8570))

dat2 = tibble(site = c("Asian Emerald Cuckoo - P. Divya Prasad https://ebird.org/india/checklist/S64628402",
                       "Rufous-backed Redstart - Ramnarayan k https://ebird.org/india/checklist/S64735633",
                       "Long-eared Owl - Amit Sharma https://ebird.org/checklist/S64544595",
                       "Smoky Warbler - Souvik Ray https://ebird.org/checklist/S64484037",
                       "Lesser Kestrel - Dr Himanshu Gupta https://ebird.org/india/checklist/S64560206",
                       "Long-toed Stint - Manoj Karingamadathil https://ebird.org/checklist/S64554438",
                       "Orange Bullfinch - Shiv Kumar Lahaul Spiti https://ebird.org/india/checklist/S64629346",
                       "White-tailed Stonechat - Pankaj Chibber https://ebird.org/checklist/S64634253",
                       "Australasian Grass-Owl - Harish Thangaraj https://ebird.org/india/checklist/S64773707",
                       "Lesser Frigatebird - Tarun Menon https://ebird.org/india/checklist/S64486130",
                       "White-bellied Woodpecker - Ravi Naidu https://ebird.org/india/checklist/S64451379",
                       "Caspian Gull - Mike Prince https://ebird.org/india/checklist/S64479321",
                       "Crimson Sunbird - Ghanshyam Prasad Bhanware https://ebird.org/checklist/S64550780",
                       "Desert Wheatear - Ramesh Shenai https://ebird.org/india/checklist/S64556258",
                       "Eastern Orphean Warbler - Ritesh Dighe https://ebird.org/india/checklist/S64499271",
                       "Sykes's Short-toed Lark - Susanta Mukherjee https://ebird.org/checklist/S64556890",
                       "Sykes's Warbler - Subhajit Roy https://ebird.org/checklist/S64439038",
                       "Long-billed Thrush - Renuka Vijayaraghavan https://ebird.org/india/checklist/S64931859",
                       "Painted Francolin - Arpit Puranik https://ebird.org/india/checklist/S64579957",
                       "Water Rail - Gil Ewing https://ebird.org/checklist/S64506396",
                       "Grey-headed Fish-Eagle - Peter Kaestner https://ebird.org/checklist/S64478067",
                       "Eastern Orphean Warbler Vigneshwaran Subramanian https://ebird.org/india/checklist/S64552285",
                       "Red-throated Thrush Tashi Angchok Stanba https://ebird.org/india/checklist/S64487922",
                       "Lesser Cuckoo Fazal Koduvally https://ebird.org/india/checklist/S64488506",
                       "Spot-bellied Eagle-Owl Anisha Tomy https://ebird.org/checklist/S64554886",
                       "Great Thick-knee Krishna Kishore Eyunni https://ebird.org/checklist/S64503309",
                       "Slaty-breasted Rail Srinivas Daripineni https://ebird.org/checklist/S64552776",
                       "White-naped Tit Vasen Suli https://ebird.org/gbbc/checklist/S64753898",
                       "European Bee-Eater Albin Jacob https://ebird.org/checklist/S64552961",
                       "Grey-headed Lapwing Santhosh TL https://ebird.org/checklist/S65513972",
                       "Sind Sparrow Sahana M https://ebird.org/checklist/S64440386",
                       "Isabelline Shrike Ramesh Desai https://ebird.org/checklist/S64484848",
                       "Common Babbler Arulvelan Thillainayagam https://ebird.org/india/checklist/S64507994",
                       "Short-eared Owl Bhakta Patnaik https://ebird.org/checklist/S64738135",
                       "Western Yellow Wagtail (feldegg) https://ebird.org/india/checklist/S64892040",
                       "Banded Bay Cuckoo Dipak Bowalkar https://ebird.org/checklist/S64750051"),
             coord_y = c(11.6356652, 29.4821201, 28.8135265, 30.3444356, 21.0408408, 12.0278141,
                         32.7498009, 32.8354958, 24.5537592, 13.6322991, 19.014042, 32.044096, 22.30492, 
                         19.4701297, 18.3716735, 22.7143608, 21.6021359, 26.914797, 22.8363625,
                         27.1788819, 26.0129781, 13.6328386, 34.0743966, 11.2690736, 10.5318111, 17.7518747,
                         12.6847012, 11.7656428, 12.5626329, 12.9756219, 31.1331864, 16.8861851, 9.1157269,
                         23.4383142, 22.7269524, 15.2259205),
             coord_x = c(92.7170777, 79.6375322, 76.5829039, 77.9978228, 81.7859019, 75.2912077,
                         76.4676106, 74.618716, 93.7936735, 74.6692424, 82.130126, 76.0604954, 80.4815867, 
                         72.7624743, 74.098843, 87.6860046, 88.2843304, 88.403592, 76.8839765,
                         77.5170422, 76.5032959, 78.4527254, 77.6118921, 75.9308696, 76.2666768, 77.9268622,
                         80.1455426, 77.9313781, 77.4607849, 77.6878271, 74.9511493, 75.7099525, 78.4183645,
                         69.9236441, 72.0179504, 74.077785))

#dat3 = tibble(site = c("Great Beach Bird Count"),
#              coord_y = c(8.0883),
#              coord_x = c(77.5385))

#dat1 = dat1 %>% 
#  mutate(tag = paste0("Story: <a href=", url_1,">", url_1, "</a>"))

#icon.ion1 = makeAwesomeIcon(icon = 'ios-close', markerColor = 'yellow', library='ion')
icon.ion2 = makeAwesomeIcon(icon = 'ios-close', markerColor = 'green', library='ion')
#icon.ion3 = makeAwesomeIcon(icon = 'ios-close', markerColor = 'blue', library='ion')

d = b@map %>% 
  #addAwesomeMarkers(~coord_x, ~coord_y, label=~site, icon = icon.ion1,
  #                              labelOptions=labelOptions(noHide=F,direction="right",style=list(
  #                                'color'='black',
  #                                'font-family'= 'serif',
  #                                'font-style'= 'regular',
  #                                'font-size' = '20px'
  #                              )), popup = ~tag, data=dat1) %>% 
  #addAwesomeMarkers(~coord_x, ~coord_y, label=~site, icon = icon.ion3,
  #                  labelOptions=labelOptions(noHide=F,direction="right",style=list(
  #                    'color'='black',
  #                    'font-family'= 'serif',
  #                    'font-style'= 'regular',
  #                    'font-size' = '20px'
  #                  )),
  #                  popup = leafpop:::popupIframe(c("https://www.youtube.com/embed/_sg-nYOhXV0"), 
  #                                                width = 300, height = 225), data=dat3) %>%
  addAwesomeMarkers(~coord_x, ~coord_y, label=~site, icon = icon.ion2,
                    labelOptions=labelOptions(noHide=F,direction="right",style=list(
                      'color'='black',
                      'font-family'= 'serif',
                      'font-style'= 'regular',
                      'font-size' = '20px'
                    )),
                    popup = leafpop::popupImage(
                      c("https://download.ams.birds.cornell.edu/api/v1/asset/209832301/2400", 
                      "https://download.ams.birds.cornell.edu/api/v1/asset/210365721/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209824491/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209569571/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209563071/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/210778211/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209879901/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/211837461/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/210498471/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209830661/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209124901/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/214135361/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/210307071/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209808411/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209525801/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209834051/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/214115351/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/211803561/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209614701/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/212193571/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209194161/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/214107221/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209349201/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209260931/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209891781/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/211425041/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209583151/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/210374291/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209535631/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209584341/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/210495601/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209269601/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/214601091/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/210357771/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/211517411/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/210375341/2400"), 
                      src = "remote"), data=dat2)

e = c@map %>% 
  #addAwesomeMarkers(~coord_x, ~coord_y, label=~site, icon = icon.ion1,
  #                              labelOptions=labelOptions(noHide=F,direction="right",style=list(
  #                                'color'='black',
  #                                'font-family'= 'serif',
  #                                'font-style'= 'regular',
  #                                'font-size' = '20px'
  #                              )), popup = ~tag, data=dat1) %>% 
  #addAwesomeMarkers(~coord_x, ~coord_y, label=~site, icon = icon.ion3,
  #                  labelOptions=labelOptions(noHide=F,direction="right",style=list(
  #                    'color'='black',
  #                    'font-family'= 'serif',
#                    'font-style'= 'regular',
#                    'font-size' = '20px'
#                  )),
#                  popup = leafpop:::popupIframe(c("https://www.youtube.com/embed/_sg-nYOhXV0"), 
#                                                width = 300, height = 225), data=dat3) %>%
addAwesomeMarkers(~coord_x, ~coord_y, label=~site, icon = icon.ion2,
                  labelOptions=labelOptions(noHide=F,direction="right",style=list(
                    'color'='black',
                    'font-family'= 'serif',
                    'font-style'= 'regular',
                    'font-size' = '20px'
                  )),
                  popup = leafpop::popupImage(
                    c("https://download.ams.birds.cornell.edu/api/v1/asset/209832301/2400", 
                      "https://download.ams.birds.cornell.edu/api/v1/asset/210365721/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209824491/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209569571/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209563071/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/210778211/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209879901/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/211837461/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/210498471/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209830661/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209124901/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/214135361/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/210307071/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209808411/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209525801/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209834051/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/214115351/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/211803561/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209614701/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/212193571/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209194161/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/214107221/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209349201/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209260931/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209891781/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/211425041/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209583151/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/210374291/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209535631/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209584341/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/210495601/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/209269601/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/214601091/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/210357771/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/211517411/2400",
                      "https://download.ams.birds.cornell.edu/api/v1/asset/210375341/2400"), 
                    src = "remote"), data=dat2)
  
mapshot(d, "GBBC_stories_states_2020.html")
#mapshot(e, "GBBC_stories_states_districts_2020.html")


############################ Common species

north = c("Punjab","Haryana","Uttar Pradesh","NCT of Delhi","Bihar","Chandigarh")

west = c("Gujarat","Rajasthan","Dadara & Nagar Havelli")

himalayas = c("Jammu & Kashmir","Ladakh","Uttarakhand","Himachal Pradesh")

central = c("Madhya Pradesh","Chhattisgarh","Maharashtra","Jharkhand","Odisha")

south = c("Andhra Pradesh","Telangana","Karnataka","Kerala","Tamil Nadu","Goa","Puducherry")

east = c("Arunanchal Pradesh","Nagaland","Manipur","Tripura","Mizoram","Sikkim","West Bengal","Assam","Meghalaya")

an = c("Andaman & Nicobar Island")

data2 = data %>%
  filter(!is.na(ST_NM)) %>%
  mutate(region = "") %>%
  mutate(region = ifelse(ST_NM %in% north, "North", region)) %>%
  mutate(region = ifelse(ST_NM %in% west, "West", region)) %>%
  mutate(region = ifelse(ST_NM %in% himalayas, "Himalayas", region)) %>%
  mutate(region = ifelse(ST_NM %in% central, "Central", region)) %>%
  mutate(region = ifelse(ST_NM %in% south, "South", region)) %>%
  mutate(region = ifelse(ST_NM %in% east, "East", region)) %>%
  mutate(region = ifelse(ST_NM %in% an, "A&N", region)) %>%
  filter(region != "")

data3 = data1 %>%
  filter(!is.na(ST_NM)) %>%
  mutate(region = "") %>%
  mutate(region = ifelse(ST_NM %in% north, "North", region)) %>%
  mutate(region = ifelse(ST_NM %in% west, "West", region)) %>%
  mutate(region = ifelse(ST_NM %in% himalayas, "Himalayas", region)) %>%
  mutate(region = ifelse(ST_NM %in% central, "Central", region)) %>%
  mutate(region = ifelse(ST_NM %in% south, "South", region)) %>%
  mutate(region = ifelse(ST_NM %in% east, "East", region)) %>%
  mutate(region = ifelse(ST_NM %in% an, "A&N", region)) %>%
  filter(region != "")

specs = data3 %>%
  group_by(region) %>% summarize(nspecies = n_distinct(PRIMARY_COM_NAME))

lists = data2 %>%
  group_by(region) %>% summarize(nlists = n_distinct(group.id))

regionsummary = left_join(lists,specs)

write.csv(regionsummary,"REGIONsummary.csv",row.names = F)

cosp = data2 %>%
  group_by(DISTRICT) %>% mutate(distlist = n_distinct(group.id)) %>% ungroup %>%
  filter(distlist > 10) %>%
  group_by(PRIMARY_COM_NAME,region,DISTRICT) %>% summarize(freq = n_distinct(group.id)/max(distlist)) %>% ungroup %>%
  group_by(region) %>% mutate(ndist = n_distinct(DISTRICT)) %>% ungroup %>%
  group_by(PRIMARY_COM_NAME,region) %>% summarize(freq = sum(freq)/max(ndist)) %>% ungroup %>%
  group_by(region) %>% arrange(desc(freq), .by_group = T) %>% ungroup %>%
  group_by(region) %>% slice(1:5)

write.csv(cosp,"CommonSpeciesbyRegion.csv",row.names = F)



#################################### Campus summaries

cbc = read.csv("CBC_2020.csv")
locs = cbc$D..Campus.Hotspot.ID

cbcdata = data %>% filter(LOC_ID %in% locs)
cbcdata1 = data1 %>% filter(LOC_ID %in% locs)

clists = cbcdata %>%
  group_by(NAME) %>% summarize(participants = n_distinct(P_NAME), lists = n_distinct(SUB_ID), 
                               uniquelists = n_distinct(group.id))

cspecs = cbcdata1 %>%
  group_by(NAME) %>% summarize(species = n_distinct(PRIMARY_COM_NAME))

campussummary = left_join(clists,cspecs)

campussummary = campussummary %>% arrange(desc(participants))

write.csv(campussummary,"CBCsummary.csv",row.names = F)


################################### plot points on map


data4 = data %>% distinct(LOC_ID,LATITUDE,LONGITUDE)

ggp = ggplot() +
  geom_polygon(data = indiamap, aes(x=long, y=lat, group=group), colour = 'white', fill = "black")+  
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

n1 = "GBBCpointmap.png"

print(ggp)
ggsave(file=n1, units="in", width=8, height=11)


################################################## plot region map

require(ggthemes)
theme_set(theme_tufte())

fstatemap = fortify(statemap, region = c("ST_NM"))
data5 = data2 %>% distinct(ST_NM,region)
extra = data5[1:2,]
extra$ST_NM = c("Sikkim","Mizoram")
extra$region = "East"
data5 = rbind(data5,extra)
fstatemap = left_join(fstatemap,data5,by = c('id' = "ST_NM"))
fstatemap = fstatemap %>% filter(!is.na(region))

cols = c("#869B27", "#E49B36", "#A13E2B", "#78CAE0", "#B69AC9", "#EA5599", "#31954E", "#493F3D",
         "#CC6666", "#9999CC", "#000000", "#66CC99")

ns = 7

cols1 = cols[c(1:ns)]
bks1 = c("A&N","Central","East","Himalayas","North","South","West")

plotindiamap = ggplot() +
  #geom_polygon(data = indiamap, aes(x=long, y=lat, group=group), colour = NA, fill = NA)+  
  geom_polygon(data = fstatemap, aes(x=long, y=lat,group=group,fill=region), colour = NA)+  
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
  coord_map()

ggp = plotindiamap +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12))

n1 = "GBBCregionmap.png"

print(ggp)
ggsave(file=n1, units="in", width=10, height=7)


################################ year summaries

sums1 = alldata %>%
  group_by(year) %>% summarize(num = n_distinct(group.id)) %>% ungroup %>%
  mutate(type = "Checklists")

sums2 = alldata %>%
  group_by(year) %>% summarize(num = n_distinct(P_NAME)) %>% ungroup %>%
  mutate(type = "Participants")

sums3 = alldata %>%
  group_by(year) %>% summarize(num = n_distinct(DISTRICT)) %>% ungroup %>%
  mutate(type = "Districts")

sums4 = alldata %>%
  group_by(year) %>% summarize(num = sum(na.omit(DURATION_HRS))) %>% ungroup %>%
  mutate(type = "Person Hours")

sums5 = alldata1 %>%
  group_by(year) %>% summarize(num = n_distinct(PRIMARY_COM_NAME)) %>% ungroup %>%
  mutate(type = "Species")


overyears = rbind(sums1,sums2,sums3,sums4,sums5)


cols = c("#869B27", "#E49B36", "#A13E2B", "#78CAE0", "#B69AC9", "#EA5599", "#31954E", "#493F3D",
         "#CC6666", "#9999CC", "#000000", "#66CC99")

is.extrafont.installed <- function(){
  if(is.element("extrafont", installed.packages()[,1])){
    library(extrafont)
    # probably need something here to run font_import()
    return(T)
  }else{
    warning("Library extrafont installed; using system sans/serif libraries as fallback fonts. 
    To enable full font support, run: 
      install.packages('extrafont') 
      font_import()")
    return(F)
  }
}

base_font_family_tufte <- function(){
  if(is.extrafont.installed()){
    library(extrafont)
    tuftefont <- choose_font(c("Gill Sans MT", "Gill Sans", "GillSans", "Verdana", "serif"), quiet = FALSE)  
  }else{
    tuftefont <- "serif"
  }
  return(tuftefont)
}

theme_tufte_revised <- function(base_size = 11, base_family = base_font_family_tufte(), ticks = TRUE) {
  
  ret <- theme_bw(base_family = base_family, base_size = base_size) + 
    theme(
      axis.line = element_line(color = 'black'),
      axis.title.x = element_text(vjust = -0.3), 
      axis.title.y = element_text(vjust = 0.8),
      legend.background = element_blank(), 
      legend.key = element_blank(), 
      legend.title = element_text(face="plain"),
      panel.background = element_blank(), 
      panel.border = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_blank(),
      strip.background = element_blank()
    )
  
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }
  
  ret
} 

require(extrafont)

pd = position_dodge(0.2)

ggp = ggplot(data = overyears[overyears$type == "Person Hours",], aes(x = year, y = num, col = type)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  xlab("Years") +
  ylab("")+
  theme_tufte_revised()

ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        legend.position = "bottom") +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_colour_manual(breaks = c("Person Hours"), 
                      values = cols[c(1)]) +
  scale_x_continuous(breaks = c(2013,2014,2015,2016,2017,2018,2019,2020)) +
  scale_y_continuous(breaks = c(0,100000,200000,300000), labels = c("0","100,000","200,000","300,000"),
                     limits = c(0,320000))

ggp = ggplot(data = overyears[overyears$type == "Checklists",], aes(x = year, y = num, col = type)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  xlab("Years") +
  ylab("")+
  theme_tufte_revised()

ggp2 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        legend.position = "bottom") +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_colour_manual(breaks = c("Checklists"), 
                      values = cols[c(1)]) +
  scale_x_continuous(breaks = c(2013,2014,2015,2016,2017,2018,2019,2020)) +
  scale_y_continuous(breaks = c(0,5000,10000,15000), labels = c("0","5,000","10,000","15,000"))

ggp = ggplot(data = overyears[!overyears$type %in% c("Person Hours","Checklists"),], 
             aes(x = year, y = num, col = type)) +
  geom_point(size = 3, position = pd) +
  geom_line(size = 1, position = pd) +
  xlab("Years") +
  ylab("")+
  theme_tufte_revised()

ggp3 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        legend.position = "bottom") +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_colour_manual(breaks = c("Participants","Districts","Species"), 
                      values = cols[c(1:3)]) +
  scale_x_continuous(breaks = c(2013,2014,2015,2016,2017,2018,2019,2020)) +
  scale_y_continuous(breaks = seq(0,2000,200), labels = c("0","200","400","600","800","1,000","1,200",
                                                          "1,400","1,600","1,800","2,000"))

data6 = alldata %>%
  filter(!is.na(ST_NM)) %>%
  mutate(region = "") %>%
  mutate(region = ifelse(ST_NM %in% north, "North", region)) %>%
  mutate(region = ifelse(ST_NM %in% west, "West", region)) %>%
  mutate(region = ifelse(ST_NM %in% himalayas, "Himalayas", region)) %>%
  mutate(region = ifelse(ST_NM %in% central, "Central", region)) %>%
  mutate(region = ifelse(ST_NM %in% south, "South", region)) %>%
  mutate(region = ifelse(ST_NM %in% east, "East", region)) %>%
  mutate(region = ifelse(ST_NM %in% an, "A&N", region)) %>%
  filter(region != "")

freqs = data6 %>%
  filter(year > 2014) %>%
  group_by(DISTRICT) %>% mutate(distlist = n_distinct(group.id)) %>% ungroup %>%
  filter(distlist > 10) %>%
  group_by(year,PRIMARY_COM_NAME,DISTRICT) %>% summarize(freq = n_distinct(group.id)/max(distlist)) %>% ungroup %>%
  group_by(year) %>% mutate(ndist = n_distinct(DISTRICT)) %>% ungroup %>%
  group_by(PRIMARY_COM_NAME,year) %>% summarize(freq = sum(freq)/max(ndist)) %>% ungroup 

ggp = ggplot(data = freqs[freqs$PRIMARY_COM_NAME %in% c("Common Myna","Rock Pigeon","Red-vented Bulbul"),], 
             aes(x = year, y = freq*100, col = PRIMARY_COM_NAME)) +
  geom_point(size = 3, position = pd) +
  geom_line(size = 1, position = pd) +
  xlab("Years") +
  ylab("Frequency (%)")+
  theme_tufte_revised()

ggp4 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        legend.position = "bottom") +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_colour_manual(breaks = c("Common Myna","Rock Pigeon","Red-vented Bulbul"), 
                      values = cols[c(1:5)]) +
  scale_x_continuous(breaks = c(2015,2016,2017,2018,2019,2020))


png('comp1.png', units="in", width=10, height=7, res=1000)
ggp1
dev.off()

png('comp2.png', units="in", width=10, height=7, res=1000)
ggp2
dev.off()

png('comp3.png', units="in", width=10, height=7, res=1000)
ggp3
dev.off()

png('comp4.png', units="in", width=10, height=7, res=1000)
ggp4
dev.off()



