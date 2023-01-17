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

load("data.RData")
data = data %>% filter(ST_NM == "WEST BENGAL")
data1 = data


################################# load kmz

unzip("WB_10km_grid.kmz")
WB10grid = readOGR("doc.kml","WB_10Km2")
WB10grid$Name = 1:length(WB10grid$Name)
WB10grid$Description = 0

######################################### Plot district map with info

WB10grid1 = gBuffer(WB10grid, byid=TRUE, width=0)
finalmap = WB10grid1

temp = data %>% group_by(group.id) %>% slice(1) # same group ID, same grid/district/state 

rownames(temp) = temp$group.id # only to setup adding the group.id column for the future left_join
coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
proj4string(temp) = "+proj=longlat +datum=WGS84"
temp = over(temp,WB10grid1) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
temp = data.frame(temp) # convert into data frame for left_join
temp = temp[,c(1,2)]
temp$group.id = rownames(temp) # add column to join with the main data
data = left_join(temp,data)


temp = data %>% 
  filter(CATEGORY %in% c("species","issf")) %>%
  group_by(Name) %>%
  summarize(Species = n_distinct(COMMON.NAME))

temp1 = data %>% 
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(Name) %>%
  summarize(Complete.Checklists = n_distinct(SAMPLING.EVENT.IDENTIFIER))

temp2 = data %>% 
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(Name) %>%
  summarize(Complete.Unique.Checklists = n_distinct(group.id))

temp3 = data %>% 
  group_by(Name) %>%
  summarize(Observers = n_distinct(OBSERVER.ID))

temp4 = data %>% 
  group_by(Name) %>%
  summarize(Locations = n_distinct(LOCALITY.ID))

temp = left_join(temp,temp1)
temp = left_join(temp,temp2)
temp = left_join(temp,temp3)
temp = left_join(temp,temp4)



temp$Name = as.character(temp$Name)

effortmap = merge(finalmap,temp, by =  "Name")


proj4string(finalmap) = "+proj=longlat +datum=WGS84"
proj4string(effortmap) = "+proj=longlat +datum=WGS84"

mapviewOptions(fgb = FALSE)
a = mapView(effortmap, zcol = c("Complete.Checklists"), map.types = c("Esri.WorldImagery"),
            layer.name = c("Birding effort - 10x10 grids"), 
            popup = leafpop::popupTable(effortmap,c("Name","Complete.Checklists",
                                                    "Complete.Unique.Checklists",
                                                    "Observers","Locations","Species"), 
                                        feature.id=FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,5,20,50,100,500,1000,max(na.omit(effortmap$Complete.Checklists)))), alpha.regions = 0.6)
mapshot(a, "WGgridinfo10km.html")



## 5km grid

unzip("WB_5km_grid.kmz")
WB5grid = readOGR("doc.kml","WB_5Km")
WB5grid$Name = 1:length(WB5grid$Name)
WB5grid$Description = 0

data = data1

######################################### Plot district map with info

WB5grid1 = gBuffer(WB5grid, byid=TRUE, width=0)
finalmap = WB5grid1

temp = data %>% group_by(group.id) %>% slice(1) # same group ID, same grid/district/state 

rownames(temp) = temp$group.id # only to setup adding the group.id column for the future left_join
coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
proj4string(temp) = "+proj=longlat +datum=WGS84"
temp = over(temp,WB5grid1) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
temp = data.frame(temp) # convert into data frame for left_join
temp = temp[,c(1,2)]
temp$group.id = rownames(temp) # add column to join with the main data
data = left_join(temp,data)


temp = data %>% 
  filter(CATEGORY %in% c("species","issf")) %>%
  group_by(Name) %>%
  summarize(Species = n_distinct(COMMON.NAME))

temp1 = data %>% 
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(Name) %>%
  summarize(Complete.Checklists = n_distinct(SAMPLING.EVENT.IDENTIFIER))

temp2 = data %>% 
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(Name) %>%
  summarize(Complete.Unique.Checklists = n_distinct(group.id))

temp3 = data %>% 
  group_by(Name) %>%
  summarize(Observers = n_distinct(OBSERVER.ID))

temp4 = data %>% 
  group_by(Name) %>%
  summarize(Locations = n_distinct(LOCALITY.ID))

temp = left_join(temp,temp1)
temp = left_join(temp,temp2)
temp = left_join(temp,temp3)
temp = left_join(temp,temp4)



temp$Name = as.character(temp$Name)

effortmap = merge(finalmap,temp, by =  "Name")


proj4string(finalmap) = "+proj=longlat +datum=WGS84"
proj4string(effortmap) = "+proj=longlat +datum=WGS84"

mapviewOptions(fgb = FALSE)
a = mapView(effortmap, zcol = c("Complete.Checklists"), map.types = c("Esri.WorldImagery"),
            layer.name = c("Birding effort - 5x5 grids"), 
            popup = leafpop::popupTable(effortmap,c("Name","Complete.Checklists",
                                                    "Complete.Unique.Checklists",
                                                    "Observers","Locations","Species"), 
                                        feature.id=FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,5,20,50,100,500,1000,max(na.omit(effortmap$Complete.Checklists)))), alpha.regions = 0.6)
mapshot(a, "WGgridinfo5km.html")