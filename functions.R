####################################################################################

## read and clean raw data and add important columns like group id, seaonality variables
## place raw txt file (India download) in working directory 

readcleanrawdata = function(rawpath, owndata = F)
{
  require(tidyverse)
  
  data = read.csv(rawpath, stringsAsFactors=F)
  
  ## choosing important variables
  
  if (!isTRUE(owndata))
  {
    imp = c("COMMON.NAME","OBSERVATION.COUNT",
            "LOCALITY.ID","LOCALITY.TYPE","COUNTY.CODE",
            "LATITUDE","LONGITUDE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID", 
            "FIRST.NAME", "LAST.NAME","SAMPLING.EVENT.IDENTIFIER",
            "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM",
            "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","group.id","day")
    
    
    data = data %>%
      mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER)) %>%
      mutate(day = as.numeric(as.factor(OBSERVATION.DATE))) %>%
      dplyr::select(imp) %>%
      ungroup
  }
  
  if (owndata)
  {
    data = data[,c(1,2,5,9,10,11,12,13,14,15,16,18)]
    names(data) = c("group.id","COMMON.NAME","OBSERVATION.COUNT","LATITUDE",
                    "LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","PROTOCOL.TYPE",
                    "DURATION.MINUTES","ALL.SPECIES.REPORTED","EFFORT.DISTANCE.KM","NUMBER.OBSERVERS")
  }
  
  
  
  temp = data %>%
    group_by(COMMON.NAME) %>% slice(1) %>% ungroup()
  
  write.csv(temp,"specieslist.csv")
  
  assign("data",data,.GlobalEnv)
  
  rm(addmapvars, 
     plotfreqmap, 
     readcleanrawdata, pos = ".GlobalEnv")
  
  save.image("data.RData")
}



######################################################################################


## prepare data for spatial analyses, add map variables, grids
## place the 'maps' workspace in working directory

addmapvars = function(datapath = "data.RData", mappath = "maps.RData")
{
  require(tidyverse)
  require(sp)
  require(rgeos)
  
  load(datapath)
  
  ## add map details to eBird data
  
  load(mappath)
  
  # add columns with DISTRICT and ST_NM to main data 
  
  temp = data %>% group_by(group.id) %>% slice(1) # same group ID, same grid/district/state 
  
  rownames(temp) = temp$group.id # only to setup adding the group.id column for the future left_join
  coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
  #proj4string(temp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  temp = over(temp,districtmap) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
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
  #data$gridg1 = as.factor(data$gridg1)
  
  temp = data %>% group_by(group.id) %>% slice(1)
  
  rownames(temp) = temp$group.id
  coordinates(temp) = ~LONGITUDE + LATITUDE
  temp = over(temp,gridmapg2)
  temp = data.frame(temp)
  temp$group.id = rownames(temp)
  data = left_join(temp,data)
  names(data)[1] = "gridg2"
  #data$gridg2 = as.factor(data$gridg2)
  
  temp = data %>% group_by(group.id) %>% slice(1)
  
  rownames(temp) = temp$group.id
  coordinates(temp) = ~LONGITUDE + LATITUDE
  temp = over(temp,gridmapg3)
  temp = data.frame(temp)
  temp$group.id = rownames(temp)
  data = left_join(temp,data)
  names(data)[1] = "gridg3"
  #data$gridg3 = as.factor(data$gridg3)
  
  temp = data %>% group_by(group.id) %>% slice(1)
  
  rownames(temp) = temp$group.id
  coordinates(temp) = ~LONGITUDE + LATITUDE
  temp = over(temp,gridmapg4)
  temp = data.frame(temp)
  temp$group.id = rownames(temp)
  data = left_join(temp,data)
  names(data)[1] = "gridg4"
  #data$gridg4 = as.factor(data$gridg4)
  
  temp = data %>% group_by(group.id) %>% slice(1)
  
  rownames(temp) = temp$group.id
  coordinates(temp) = ~LONGITUDE + LATITUDE
  temp = over(temp,gridmapg5)
  temp = data.frame(temp)
  temp$group.id = rownames(temp)
  data = left_join(temp,data)
  names(data)[1] = "gridg5"
  #data$gridg5 = as.factor(data$gridg5)
  
  ## 
  
  assign("data",data,.GlobalEnv)
  assign("gridlevels",gridlevels,.GlobalEnv)
  rm(addmapvars, plotfreqmap, readcleanrawdata, pos = ".GlobalEnv")
  
  save.image("dataforspatialanalyses.RData")
}


#######################################################################################


## a script to plot frequencies on a map with relative ease
## input can be trivial frequencies or modeled output
## data is unsummarized data
## resolution can take 7 values; "state","district","g1","g2","g3","g4","g5"
## species common name
## path can be true or false; this is for boundaries
## add can take the values "species", "unique lists","observers","unique locations"

plotfreqmap = function(data, taxonname, resolution, level = "species", season = "year round", rich = F,
                       add = "species", smooth = F, h = 2, cutoff = 5,
                       showempty = T, states = "none",
                       mappath = "maps.RData", maskpath = "mask.RData")
{
  ## errors for wrong parameter values
  
  if (!level %in% c("species","genus","family","order"))
    return(paste("the taxonomic level",level,"doesn't exist, select any one from species, genus, family and order (in quotes)"))

  if (!resolution %in% c("state","district","g1","g2","g3","g4","g5"))
    return(paste("the resolution",resolution,"doesn't exist, select any one from state, district, g1, g2, g3, g4 and g5 (in quotes)"))
  
  if (!season %in% c("year round","summer","winter","passage"))
    return(paste("the season",season,"doesn't exist, select any one from year round, summer, winter and passage (in quotes)"))
  
  if (!is.na(as.logical(rich)))
  {
    if (rich != as.logical(rich))
      return("rich must be a logical operator")
  }else{
    return("rich must be a logical operator")
  }

  if (!is.na(as.logical(smooth)))
  {
    if (smooth != as.logical(smooth))
      return("smooth must be a logical operator")
  }else{
    return("smooth must be a logical operator")
  }
  
  if (!is.na(as.logical(showempty)))
  {
    if (showempty != as.logical(showempty))
      return("showempty must be a logical operator")
  }else{
    return("showempty must be a logical operator")
  }
  
  if (cutoff & (cutoff < 0 | cutoff != as.integer(cutoff)))
    return("cutoff (minimum no. of lists) must be a non-negative integer")
  
  if (smooth & (h < 0 | h != as.numeric(h)))
    return("h (smoothing bandwidth) must be positive")
    
  
  require(tidyverse)
  require(ggfortify)
  require(viridis)
  require(RColorBrewer)
  require(scales)
  
  data$gridg1 = as.factor(data$gridg1)
  data$gridg2 = as.factor(data$gridg2)
  data$gridg3 = as.factor(data$gridg3)
  data$gridg4 = as.factor(data$gridg4)
  data$gridg5 = as.factor(data$gridg5)
  
  load(mappath)
  
  filterstate = fortify(statemap)
  
  if (states == "none")
  {
    states = as.character(na.omit(unique(data$ST_NM)))
  }
  
  if (states[1] != "none")
  {
    if (resolution == "district" | resolution == "state")
    {
      filterstate = fortify(statemap[statemap@data$ST_NM %in% states,], region = c("ST_NM"))
      
      data = data %>%
        filter(ST_NM %in% states)
      
      filterdistrict = fortify(districtmap[districtmap@data$ST_NM %in% states,], region = c("DISTRICT"))
    }
  }
  
  
  
  plotindiamap = ggplot() +
    geom_polygon(data = filterstate, aes(x=long, y=lat, group=group), colour = 'black', fill = "white")+  
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme_bw()+
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
    coord_map()
  
  if (level == "species")
  {
    if (!taxonname %in% unique(data$COMMON.NAME))
      return(paste(taxonname,"is not a valid",level,"name"))
  }
  
  if (level != "species")
  {
    taxlevels = read.csv("speciestaxonomiclevels.csv")
    taxlevels = taxlevels[,c("COMMON.NAME",level)]
    
    if (level == "genus")
    {
      if (!taxonname %in% unique(taxlevels$genus))
        return(paste(taxonname,"is not a valid",level,"name"))
    }
    
    if (level == "family")
    {
      if (!taxonname %in% unique(taxlevels$family))
        return(paste(taxonname,"is not a valid",level,"name"))
    }
    
    if (level == "order")
    {
      if (!taxonname %in% unique(taxlevels$order))
        return(paste(taxonname,"is not a valid",level,"name"))
    }
    
    data = left_join(data,taxlevels)
    l = length(names(data))
    names(data)[l] = "TAXON.NAME"
    
    if (rich)
    {
      data = data %>% filter(TAXON.NAME == taxonname)
    }else{
      data = data %>% distinct(gridg5,group.id,gridg4,gridg3,gridg2,gridg1,DISTRICT,ST_NM,LOCALITY.ID,
                               LOCALITY.TYPE,LATITUDE,LONGITUDE,TIME.OBSERVATIONS.STARTED,
                               OBSERVER.ID,PROTOCOL.TYPE,DURATION.MINUTES,EFFORT.DISTANCE.KM,NUMBER.OBSERVERS,
                               ALL.SPECIES.REPORTED,no.sp,LOCALITY.HOTSPOT,TAXON.NAME)
    }
  }else{
    data$TAXON.NAME = data$COMMON.NAME
  }
  
  if (season == "summer")
  {
    data = data %>% 
      filter(day >= 135 & day <= 225)
  }
  
  if (season == "winter")
  {
    data = data %>% 
      filter(day < 60 | day > 300)
  }
  
  if (season == "passage")
  {
    data = data %>% 
      filter((day >= 120 & day < 135) | (day > 240 & day <= 300))
  }
  
  if (!isTRUE(rich))
  {
    data = data %>%
      filter(ALL.SPECIES.REPORTED == 1)
    data1 = data %>% filter(TAXON.NAME == taxonname)
  }
  
  if (isTRUE(rich))
  {
    if (resolution == "state")
    {
      if (add == "species"){
        temp = data %>% 
          group_by(ST_NM) %>%
          summarize(freq = n_distinct(COMMON.NAME))
      }
      if (add == "unique lists"){
        temp = data %>% 
          group_by(ST_NM) %>%
          summarize(freq = n_distinct(group.id))
      }
      if (add == "all lists"){
        temp = data %>% 
          group_by(ST_NM) %>%
          summarize(freq = n_distinct(SAMPLING.EVENT.IDENTIFIER))
      }
      if (add == "observers"){
        temp = data %>% 
          group_by(ST_NM) %>%
          summarize(freq = n_distinct(OBSERVER.ID))
      }
      if (add == "unique locations"){
        temp = data %>% 
          group_by(ST_NM) %>%
          summarize(freq = n_distinct(LOCALITY.ID))
      }
      
      min = min(temp$freq)
      max = max(temp$freq)
      
      filled = data %>%
        group_by(ST_NM) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(ST_NM,lists)
      
      fortified = filterstate
      fortind = filterstate
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))
      
      zlists = setdiff(unique(fort1$id),unique(filled$ST_NM))
      
      if(length(zlists > 0))
      {
        empty = data.frame(unique(zlists),0)
        names(empty) = names(filled)
      }
      
      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "ST_NM"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "ST_NM"))) # SPDF to plot
        switch = T
      }
      else
      {
        switch = F
      }
    }
    
    if (resolution == "district")
    {
      if (add == "species"){
        temp = data %>% 
          group_by(DISTRICT) %>%
          summarize(freq = n_distinct(COMMON.NAME))
      }
      if (add == "unique lists"){
        temp = data %>% 
          group_by(DISTRICT) %>%
          summarize(freq = n_distinct(group.id))
      }
      if (add == "all lists"){
        temp = data %>% 
          group_by(DISTRICT) %>%
          summarize(freq = n_distinct(SAMPLING.EVENT.IDENTIFIER))
      }
      if (add == "observers"){
        temp = data %>% 
          group_by(DISTRICT) %>%
          summarize(freq = n_distinct(OBSERVER.ID))
      }
      if (add == "unique locations"){
        temp = data %>% 
          group_by(DISTRICT) %>%
          summarize(freq = n_distinct(LOCALITY.ID))
      }
      
      min = min(temp$freq)
      max = max(temp$freq)
      
      filled = data %>%
        group_by(DISTRICT) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(DISTRICT,lists)
      
      fortified = filterdistrict
      fortind = filterstate
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))

      zlists = setdiff(unique(fort1$id),unique(filled$DISTRICT))
      
      if(length(zlists > 0))
      {
        empty = data.frame(unique(zlists),0)
        names(empty) = names(filled)
      }
      
      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "DISTRICT"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "DISTRICT"))) # SPDF to plot
        switch = T
      }
      else
      {
        switch = F
      }
    }
    
    if (resolution == "g1")
    {
      datar = data %>%
        group_by(gridg1,COMMON.NAME) %>% slice(1) %>% ungroup()
      
      if (add == "species"){
        temp = data %>% 
          group_by(gridg1) %>%
          summarize(freq = n_distinct(COMMON.NAME))
      }
      if (add == "unique lists"){
        temp = data %>% 
          group_by(gridg1) %>%
          summarize(freq = n_distinct(group.id))
      }
      if (add == "all lists"){
        temp = data %>% 
          group_by(gridg1) %>%
          summarize(freq = n_distinct(SAMPLING.EVENT.IDENTIFIER))
      }
      if (add == "observers"){
        temp = data %>% 
          group_by(gridg1) %>%
          summarize(freq = n_distinct(OBSERVER.ID))
      }
      if (add == "unique locations"){
        temp = data %>% 
          group_by(gridg1) %>%
          summarize(freq = n_distinct(LOCALITY.ID))
      }
      
      min = min(temp$freq)
      max = max(temp$freq)
      
      filled = data %>%
        group_by(gridg1) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(gridg1,lists)
      
      fortified = fortify(gridmapg1, region = c("id"))
      fortind = fortify(indiamap)
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))
      
      zlists = setdiff(unique(fort1$id),unique(filled$gridg1))
      empty = data.frame(unique(zlists),0)
      names(empty) = names(filled)
      
      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "gridg1"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "gridg1"))) # SPDF to plot
        switch = T
      }
      else
      {
        switch = F
      }
      
      mmplotdf = plotdf %>%
        group_by(id) %>% summarize(minlong = min(long), maxlong = max(long), minlat = min(lat),maxlat = max(lat)) %>% ungroup()
      
      datar = left_join(datar,mmplotdf,by = c("gridg1" = 'id'))
      
      datar = datar %>%
        group_by(gridg1) %>% mutate(LONGITUDE = cbind(runif(n(),max(minlong),
                                                            max(maxlong)),runif(n(),
                                                                                max(minlat),
                                                                                max(maxlat)))[,1],
                                    LATITUDE = cbind(runif(n(),max(minlong),
                                                           max(maxlong)),runif(n(),
                                                                               max(minlat),
                                                                               max(maxlat)))[,2])
    }
    
    if (resolution == "g2")
    {
      datar = data %>%
        group_by(gridg2,COMMON.NAME) %>% slice(1) %>% ungroup()
      
      if (add == "species"){
        temp = data %>% 
          group_by(gridg2) %>%
          summarize(freq = n_distinct(COMMON.NAME))
      }
      if (add == "unique lists"){
        temp = data %>% 
          group_by(gridg2) %>%
          summarize(freq = n_distinct(group.id))
      }
      if (add == "all lists"){
        temp = data %>% 
          group_by(gridg2) %>%
          summarize(freq = n_distinct(SAMPLING.EVENT.IDENTIFIER))
      }
      if (add == "observers"){
        temp = data %>% 
          group_by(gridg2) %>%
          summarize(freq = n_distinct(OBSERVER.ID))
      }
      if (add == "unique locations"){
        temp = data %>% 
          group_by(gridg2) %>%
          summarize(freq = n_distinct(LOCALITY.ID))
      }
      
      min = min(temp$freq)
      max = max(temp$freq)
      
      filled = data %>%
        group_by(gridg2) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(gridg2,lists)
      
      fortified = fortify(gridmapg2, region = c("id"))
      fortind = fortify(indiamap)
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))
      
      zlists = setdiff(unique(fort1$id),unique(filled$gridg2))
      empty = data.frame(unique(zlists),0)
      names(empty) = names(filled)
      
      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "gridg2"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "gridg2"))) # SPDF to plot
        switch = T
      }
      else
      {
        switch = F
      }
      
      mmplotdf = plotdf %>%
        group_by(id) %>% summarize(minlong = min(long), maxlong = max(long), minlat = min(lat),maxlat = max(lat)) %>% ungroup()
      
      datar = left_join(datar,mmplotdf,by = c("gridg2" = 'id'))
      
      datar = datar %>%
        group_by(gridg2) %>% mutate(LONGITUDE = cbind(runif(n(),max(minlong),
                                                            max(maxlong)),runif(n(),
                                                                                max(minlat),
                                                                                max(maxlat)))[,1],
                                    LATITUDE = cbind(runif(n(),max(minlong),
                                                           max(maxlong)),runif(n(),
                                                                               max(minlat),
                                                                               max(maxlat)))[,2])
    }
    
    if (resolution == "g3")
    {
      datar = data %>%
        group_by(gridg3,COMMON.NAME) %>% slice(1) %>% ungroup()
      
      if (add == "species"){
        temp = data %>% 
          group_by(gridg3) %>%
          summarize(freq = n_distinct(COMMON.NAME))
      }
      if (add == "unique lists"){
        temp = data %>% 
          group_by(gridg3) %>%
          summarize(freq = n_distinct(group.id))
      }
      if (add == "all lists"){
        temp = data %>% 
          group_by(gridg3) %>%
          summarize(freq = n_distinct(SAMPLING.EVENT.IDENTIFIER))
      }
      if (add == "observers"){
        temp = data %>% 
          group_by(gridg3) %>%
          summarize(freq = n_distinct(OBSERVER.ID))
      }
      if (add == "unique locations"){
        temp = data %>% 
          group_by(gridg3) %>%
          summarize(freq = n_distinct(LOCALITY.ID))
      }
      
      min = min(temp$freq)
      max = max(temp$freq)
      
      filled = data %>%
        group_by(gridg3) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(gridg3,lists)
      
      fortified = fortify(gridmapg3, region = c("id"))
      fortind = fortify(indiamap)
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))
      
      zlists = setdiff(unique(fort1$id),unique(filled$gridg3))
      empty = data.frame(unique(zlists),0)
      names(empty) = names(filled)
      
      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "gridg3"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "gridg3"))) # SPDF to plot
        switch = T
      }
      else
      {
        switch = F
      }
      
      mmplotdf = plotdf %>%
        group_by(id) %>% summarize(minlong = min(long), maxlong = max(long), minlat = min(lat),maxlat = max(lat)) %>% ungroup()
      
      datar = left_join(datar,mmplotdf,by = c("gridg3" = 'id'))
      
      datar = datar %>%
        group_by(gridg3) %>% mutate(LONGITUDE = cbind(runif(n(),max(minlong),
                                                            max(maxlong)),runif(n(),
                                                                                max(minlat),
                                                                                max(maxlat)))[,1],
                                    LATITUDE = cbind(runif(n(),max(minlong),
                                                           max(maxlong)),runif(n(),
                                                                               max(minlat),
                                                                               max(maxlat)))[,2])
    }
    
    if (resolution == "g4")
    {
      datar = data %>%
        group_by(gridg4,COMMON.NAME) %>% slice(1) %>% ungroup()
      
      if (add == "species"){
        temp = data %>% 
          group_by(gridg4) %>%
          summarize(freq = n_distinct(COMMON.NAME))
      }
      if (add == "unique lists"){
        temp = data %>% 
          group_by(gridg4) %>%
          summarize(freq = n_distinct(group.id))
      }
      if (add == "all lists"){
        temp = data %>% 
          group_by(gridg4) %>%
          summarize(freq = n_distinct(SAMPLING.EVENT.IDENTIFIER))
      }
      if (add == "observers"){
        temp = data %>% 
          group_by(gridg4) %>%
          summarize(freq = n_distinct(OBSERVER.ID))
      }
      if (add == "unique locations"){
        temp = data %>% 
          group_by(gridg4) %>%
          summarize(freq = n_distinct(LOCALITY.ID))
      }
      
      min = min(temp$freq)
      max = max(temp$freq)
      
      filled = data %>%
        group_by(gridg4) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(gridg4,lists)
      
      fortified = fortify(gridmapg4, region = c("id"))
      fortind = fortify(indiamap)
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))
      
      zlists = setdiff(unique(fort1$id),unique(filled$gridg4))
      empty = data.frame(unique(zlists),0)
      names(empty) = names(filled)
      
      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "gridg4"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "gridg4"))) # SPDF to plot
        switch = T
      }else{
        switch = F
      }
      
      mmplotdf = plotdf %>%
        group_by(id) %>% summarize(minlong = min(long), maxlong = max(long), minlat = min(lat),maxlat = max(lat)) %>% ungroup()
      
      datar = left_join(datar,mmplotdf,by = c("gridg4" = 'id'))
      
      datar = datar %>%
        group_by(gridg4) %>% mutate(LONGITUDE = cbind(runif(n(),max(minlong),
                                                            max(maxlong)),runif(n(),
                                                                                max(minlat),
                                                                                max(maxlat)))[,1],
                                    LATITUDE = cbind(runif(n(),max(minlong),
                                                           max(maxlong)),runif(n(),
                                                                               max(minlat),
                                                                               max(maxlat)))[,2])
    }
    
    if (resolution == "g5")
    {
      datar = data %>%
        group_by(gridg5,COMMON.NAME) %>% slice(1) %>% ungroup()
      
      if (add == "species"){
        temp = data %>% 
          group_by(gridg5) %>%
          summarize(freq = n_distinct(COMMON.NAME))
      }
      if (add == "unique lists"){
        temp = data %>% 
          group_by(gridg5) %>%
          summarize(freq = n_distinct(group.id))
      }
      if (add == "all lists"){
        temp = data %>% 
          group_by(gridg5) %>%
          summarize(freq = n_distinct(SAMPLING.EVENT.IDENTIFIER))
      }
      if (add == "observers"){
        temp = data %>% 
          group_by(gridg5) %>%
          summarize(freq = n_distinct(OBSERVER.ID))
      }
      if (add == "unique locations"){
        temp = data %>% 
          group_by(gridg5) %>%
          summarize(freq = n_distinct(LOCALITY.ID))
      }
      
      min = min(temp$freq)
      max = max(temp$freq)
      
      filled = data %>%
        group_by(gridg5) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(gridg5,lists)
      
      fortified = fortify(gridmapg5, region = c("id"))
      fortind = fortify(indiamap)
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))
      
      zlists = setdiff(unique(fort1$id),unique(filled$gridg5))
      empty = data.frame(unique(zlists),0)
      names(empty) = names(filled)
      
      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "gridg5"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "gridg5"))) # SPDF to plot
        switch = T
      }
      else
      {
        switch = F
      }
      
      mmplotdf = plotdf %>%
        group_by(id) %>% summarize(minlong = min(long), maxlong = max(long), minlat = min(lat),maxlat = max(lat)) %>% ungroup()
      
      datar = left_join(datar,mmplotdf,by = c("gridg5" = 'id'))
      
      datar = datar %>%
        group_by(gridg5) %>% mutate(LONGITUDE = cbind(runif(n(),max(minlong),
                                                            max(maxlong)),runif(n(),
                                                                                max(minlat),
                                                                                max(maxlat)))[,1],
                                    LATITUDE = cbind(runif(n(),max(minlong),
                                                           max(maxlong)),runif(n(),
                                                                               max(minlat),
                                                                               max(maxlat)))[,2])
    }
  }
  
  if (!isTRUE(rich))
  {
    if (resolution == "state")
    {
      temp = data %>% 
        group_by(ST_NM) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(TAXON.NAME == taxonname, lists >= cutoff) %>%
        group_by(ST_NM) %>%
        summarize(freq = n_distinct(group.id)/max(lists))
      
      min = min(temp$freq)
      max = max(temp$freq)
      
      filled = data %>%
        group_by(ST_NM) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(ST_NM,lists)
      
      fortified = filterstate
      fortind = filterstate
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))
      
      zlists = setdiff(unique(fort1$id),unique(filled$ST_NM))
      
      if(length(zlists > 0))
      {
        empty = data.frame(unique(zlists),0)
        names(empty) = names(filled)
      }
      
      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "ST_NM"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "ST_NM"))) # SPDF to plot
        switch = T
      }
      else
      {
        switch = F
      }
    }
    
    if (resolution == "district")
    {
      temp = data %>% 
        group_by(DISTRICT) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(TAXON.NAME == taxonname, lists >= cutoff) %>%
        group_by(DISTRICT) %>%
        summarize(freq = n_distinct(group.id)/max(lists))
      
      min = min(temp$freq)
      max = max(temp$freq)
      
      filled = data %>%
        group_by(DISTRICT) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(DISTRICT,lists)
      
      fortified = filterdistrict
      fortind = filterstate
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))

      zlists = setdiff(unique(fort1$id),unique(filled$DISTRICT))
      
      if(length(zlists > 0))
      {
        empty = data.frame(unique(zlists),0)
        names(empty) = names(filled)
      }
      
      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "DISTRICT"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "DISTRICT"))) # SPDF to plot
        switch = T
      }
      else
      {
        switch = F
      }
    }
    
    if (resolution == "g1")
    {
      temp = data %>% 
        group_by(gridg1) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(TAXON.NAME == taxonname, lists >= cutoff) %>%
        group_by(gridg1) %>%
        summarize(freq = n_distinct(group.id)/max(lists))
      
      min = min(temp$freq)
      max = max(temp$freq)
      
      filled = data %>%
        group_by(gridg1) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(gridg1,lists)
      
      fortified = fortify(gridmapg1, region = c("id"))
      fortind = fortify(indiamap)
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))
      
      zlists = setdiff(unique(fort1$id),unique(filled$gridg1))
      empty = data.frame(unique(zlists),0)
      names(empty) = names(filled)
      
      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "gridg1"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "gridg1"))) # SPDF to plot
        switch = T
      }
      else
      {
        switch = F
      }
      
      data1 = data1 %>%
        group_by(gridg1) %>% slice(1) %>% ungroup()
      
      mmplotdf = plotdf %>%
        group_by(id) %>% summarize(minlong = min(long), maxlong = max(long), minlat = min(lat), maxlat = max(lat), freq = max(freq)) %>% ungroup()
      
      data1 = left_join(data1,mmplotdf,by = c("gridg1" = 'id'))
      roundUp = function(x) 10^ceiling(log10(x))
      data1$freq = round(data1$freq*roundUp(1/median(na.omit(data1$freq)))*10)
      data1 = data1 %>% filter(freq > 0)
      
      
      data1 = data1[rep(row.names(data1), data1$freq),]
      
      
      data1 = data1 %>%
        group_by(gridg1) %>% mutate(LONGITUDE = cbind(runif(n(),max(minlong),
                                                            max(maxlong)),runif(n(),
                                                                                max(minlat),
                                                                                max(maxlat)))[,1],
                                    LATITUDE = cbind(runif(n(),max(minlong),
                                                           max(maxlong)),runif(n(),
                                                                               max(minlat),
                                                                               max(maxlat)))[,2])
    }
    
    if (resolution == "g2")
    {
      temp = data %>% 
        group_by(gridg2) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(TAXON.NAME == taxonname, lists >= cutoff) %>%
        group_by(gridg2) %>%
        summarize(freq = n_distinct(group.id)/max(lists))
      
      min = min(temp$freq)
      max = max(temp$freq)
      
      filled = data %>%
        group_by(gridg2) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(gridg2,lists)
      
      fortified = fortify(gridmapg2, region = c("id"))
      fortind = fortify(indiamap)
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))
      
      zlists = setdiff(unique(fort1$id),unique(filled$gridg2))
      empty = data.frame(unique(zlists),0)
      names(empty) = names(filled)
      
      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "gridg2"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "gridg2"))) # SPDF to plot
        switch = T
      }
      else
      {
        switch = F
      }
      
      data1 = data1 %>%
        group_by(gridg2) %>% slice(1) %>% ungroup()
      
      mmplotdf = plotdf %>%
        group_by(id) %>% summarize(minlong = min(long), maxlong = max(long), minlat = min(lat), maxlat = max(lat), freq = max(freq)) %>% ungroup()
      
      data1 = left_join(data1,mmplotdf,by = c("gridg2" = 'id'))
      roundUp = function(x) 10^ceiling(log10(x))
      data1$freq = round(data1$freq*roundUp(1/median(na.omit(data1$freq)))*10)
      data1 = data1 %>% filter(freq > 0)
      
      
      data1 = data1[rep(row.names(data1), data1$freq),]
      
      
      data1 = data1 %>%
        group_by(gridg2) %>% mutate(LONGITUDE = cbind(runif(n(),max(minlong),
                                                            max(maxlong)),runif(n(),
                                                                                max(minlat),
                                                                                max(maxlat)))[,1],
                                    LATITUDE = cbind(runif(n(),max(minlong),
                                                           max(maxlong)),runif(n(),
                                                                               max(minlat),
                                                                               max(maxlat)))[,2])
    }
    
    if (resolution == "g3")
    {
      temp = data %>% 
        group_by(gridg3) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(TAXON.NAME == taxonname, lists >= cutoff) %>%
        group_by(gridg3) %>%
        summarize(freq = n()/max(lists))
      
      min = min(temp$freq)
      max = max(temp$freq)
      
      filled = data %>%
        group_by(gridg3) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(gridg3,lists)
      
      fortified = fortify(gridmapg3, region = c("id"))
      fortind = fortify(indiamap)
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))
      
      zlists = setdiff(unique(fort1$id),unique(filled$gridg3))
      empty = data.frame(unique(zlists),0)
      names(empty) = names(filled)
      
      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "gridg3"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "gridg3"))) # SPDF to plot
        switch = T
      }else{
        switch = F
      }
      
      data1 = data1 %>%
        group_by(gridg3) %>% slice(1) %>% ungroup()
      
      mmplotdf = plotdf %>%
        group_by(id) %>% summarize(minlong = min(long), maxlong = max(long), minlat = min(lat), maxlat = max(lat), freq = max(freq)) %>% ungroup()
      
      data1 = left_join(data1,mmplotdf,by = c("gridg3" = 'id'))
      roundUp = function(x) 10^ceiling(log10(x))
      data1$freq = round(data1$freq*roundUp(1/median(na.omit(data1$freq)))*10)
      data1 = data1 %>% filter(freq > 0)
      
      
      data1 = data1[rep(row.names(data1), data1$freq),]
      
      
      data1 = data1 %>%
        group_by(gridg3) %>% mutate(LONGITUDE = cbind(runif(n(),max(minlong),
                                                            max(maxlong)),runif(n(),
                                                                                max(minlat),
                                                                                max(maxlat)))[,1],
                                    LATITUDE = cbind(runif(n(),max(minlong),
                                                           max(maxlong)),runif(n(),
                                                                               max(minlat),
                                                                               max(maxlat)))[,2])
    }
    
    if (resolution == "g4")
    {
      temp = data %>% 
        group_by(gridg4) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(TAXON.NAME == taxonname, lists >= cutoff) %>%
        group_by(gridg4) %>%
        summarize(freq = n_distinct(group.id)/max(lists))
      
      min = min(temp$freq)
      max = max(temp$freq)
      
      filled = data %>%
        group_by(gridg4) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(gridg4,lists)
      
      fortified = fortify(gridmapg4, region = c("id"))
      fortind = fortify(indiamap)
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))
      
      zlists = setdiff(unique(fort1$id),unique(filled$gridg4))
      empty = data.frame(unique(zlists),0)
      names(empty) = names(filled)
      
      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "gridg4"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "gridg4"))) # SPDF to plot
        switch = T
      }
      else
      {
        switch = F
      }
      
      data1 = data1 %>%
        group_by(gridg4) %>% slice(1) %>% ungroup()
      
      mmplotdf = plotdf %>%
        group_by(id) %>% summarize(minlong = min(long), maxlong = max(long), minlat = min(lat), maxlat = max(lat), freq = max(freq)) %>% ungroup()
      
      data1 = left_join(data1,mmplotdf,by = c("gridg4" = 'id'))
      roundUp = function(x) 10^ceiling(log10(x))
      data1$freq = round(data1$freq*roundUp(1/median(na.omit(data1$freq)))*10)
      data1 = data1 %>% filter(freq > 0)
      
      
      data1 = data1[rep(row.names(data1), data1$freq),]
      
      
      data1 = data1 %>%
        group_by(gridg4) %>% mutate(LONGITUDE = cbind(runif(n(),max(minlong),
                                                            max(maxlong)),runif(n(),
                                                                                max(minlat),
                                                                                max(maxlat)))[,1],
                                    LATITUDE = cbind(runif(n(),max(minlong),
                                                           max(maxlong)),runif(n(),
                                                                               max(minlat),
                                                                               max(maxlat)))[,2])
    }
    
    if (resolution == "g5")
    {
      temp = data %>% 
        group_by(gridg5) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(TAXON.NAME == taxonname, lists >= cutoff) %>%
        group_by(gridg5) %>%
        summarize(freq = n_distinct(group.id)/max(lists))
      
      min = min(temp$freq)
      max = max(temp$freq)
      
      filled = data %>%
        group_by(gridg5) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(gridg5,lists)
      
      fortified = fortify(gridmapg5, region = c("id"))
      fortind = fortify(indiamap)
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))
      
      zlists = setdiff(unique(fort1$id),unique(filled$gridg5))
      empty = data.frame(unique(zlists),0)
      names(empty) = names(filled)
      
      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "gridg5"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "gridg5"))) # SPDF to plot
        switch = T
      }
      else
      {
        switch = F
      }
      
      data1 = data1 %>%
        group_by(gridg5) %>% slice(1) %>% ungroup()
      
      mmplotdf = plotdf %>%
        group_by(id) %>% summarize(minlong = min(long), maxlong = max(long), minlat = min(lat), maxlat = max(lat), freq = max(freq)) %>% ungroup()
      
      data1 = left_join(data1,mmplotdf,by = c("gridg5" = 'id'))
      roundUp = function(x) 10^ceiling(log10(x))
      data1$freq = round(data1$freq*roundUp(1/median(na.omit(data1$freq)))*10)
      data1 = data1 %>% filter(freq > 0)
      
      
      data1 = data1[rep(row.names(data1), data1$freq),]
      
      
      data1 = data1 %>%
        group_by(gridg5) %>% mutate(LONGITUDE = cbind(runif(n(),max(minlong),
                                                            max(maxlong)),runif(n(),
                                                                                max(minlat),
                                                                                max(maxlat)))[,1],
                                    LATITUDE = cbind(runif(n(),max(minlong),
                                                           max(maxlong)),runif(n(),
                                                                               max(minlat),
                                                                               max(maxlat)))[,2])
    }
  }
  
  if(resolution != "state" & resolution != "district"){load(maskpath)}
  
  if(isTRUE(smooth) & resolution != "state" & resolution != "district")
  {
    stats = plotindiamap +
    {if(!isTRUE(rich))stat_density2d(data = data1, aes(x = LONGITUDE, y = LATITUDE, fill = stat(level)), h = h, n = 100, geom = "polygon")} +
    {if(rich)stat_density2d(data = datar, aes(x = LONGITUDE, y = LATITUDE, fill = stat(level)), h = h, n = 100, geom = "polygon")} +
      scale_fill_gradient2(low = muted("blue"),
                           high = "white", space = "Lab", na.value = "grey50", trans = 'reverse')
  }
  
  
  if(smooth & resolution != "state" & resolution != "district"){
    minstat = min(ggplot_build(stats)$data[[2]]$level)
    maxstat = max(ggplot_build(stats)$data[[2]]$level)
  }else{
    minstat = min
    maxstat = max
  }
  
  
  for (i in c(5,4,3,2,1))
  {
    breaks = seq(minstat,maxstat,length.out=i)
    labels = round(seq(min,max,length.out=i),2)
    if(rich){labels = round(seq(min,max,length.out=i))}
    if (length(unique(labels)) == i)
      break
  }
  
  cols = "grey30"
  cl = paste(" <",cutoff,"lists")
  names(cols) = cl
  
  require(mltools)
  
  plotdf$freq1 = mltools::bin_data(plotdf$freq, bins=4, binType = "quantile")
  
  sm = plotdf %>%
    group_by(freq1) %>% summarize(min = round(min(freq),2),max = round(max(freq),2))
  
  l = length(sm$freq1)
  vals = c("#99CCFF","#6699CC","#336699","#003399")
  
  data = data %>%
    filter(COMMON.NAME == taxonname) %>%
    group_by(LOCALITY.ID) %>% slice(1)
  
  plot = plotindiamap +
    {if(smooth & resolution != "state" & resolution != "district" & !isTRUE(rich))stat_density2d(data = data1, aes(x = LONGITUDE, y = LATITUDE, fill = stat(level)), h = h, n = 100, geom = "polygon")} +
    {if(smooth & resolution != "state" & resolution != "district" & rich)stat_density2d(data = datar, aes(x = LONGITUDE, y = LATITUDE, fill = stat(level)), h = h, n = 100, geom = "polygon")} +
    {if(!isTRUE(smooth) | resolution == "state" | resolution == "district")geom_polygon(data = plotdf, aes(x = long, y = lat, group = group, fill = freq1))} +
    {if(switch & showempty)geom_polygon(data = emptydf, aes(x = long, y = lat, group = group, col = cl), fill = "grey30")} +
    {if(resolution != "state" & resolution != "district")geom_polygon(data = mask, aes(x = long, y = lat, group = group), col = 'white', fill = 'white')}+
    {if(resolution == "district")geom_path(data = filterdistrict, aes(x = long, y = lat, group = group), col = 'darkolivegreen', size = 0.5)}+
    geom_path(data = filterstate, aes(x = long, y = lat, group = group), col = 'black', size = 1) +
    {if(l <= 2)scale_fill_manual(values = vals[1:l],
                                 breaks = sm$freq1, labels = sm$freq1)} +
    {if(l == 3)scale_fill_manual(values = vals[1:l],
                                 breaks = sm$freq1, labels = c(paste("<=",sm$max[1]),paste(sm$min[2]," - ",sm$max[2]),
                                 paste(">",sm$min[3])))} +
    {if(l > 3)scale_fill_manual(values = vals,breaks = sm$freq1, labels = c(paste("<=",sm$max[1]),paste(sm$min[2]," - ",sm$max[2]),
                                 paste(sm$min[3]," - ",sm$max[3]),paste(">",sm$min[4])))} +
    {if(switch)scale_colour_manual(values = cols)} +
    #theme(legend.justification=c(1,1), legend.position=c(0.99,0.99)) +
    theme(legend.text = element_text(size = 12)) +
    {if(!isTRUE(rich) | level != "species")ggtitle(taxonname)} +
    {if(!isTRUE(rich) | level != "species")theme(plot.title = element_text(hjust = 0.5, vjust = 0.1, size = 20))} +
    guides(fill = guide_legend(title = add, reverse = TRUE, override.aes = list(size=10)))
    #geom_point(data = data, aes(x = LONGITUDE, y = LATITUDE), col = 'red', size = 2)+
    #theme(legend.position = "none")+
    #ggtitle(endyear)
  
  return(plot)
}

topspecies = function(data, n = 5, cutoff = 5, district = F)
{
  require(tidyverse)
  data = data %>%
    filter(ALL.SPECIES.REPORTED == 1)
  if (district)
  {
    top = data %>%
      group_by(DISTRICT) %>%
      mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
      filter(lists >= cutoff) %>%
      group_by(DISTRICT,COMMON.NAME) %>%
      summarize(freq = n_distinct(group.id)/max(lists)) %>% ungroup() %>%
      group_by(DISTRICT) %>% arrange(desc(freq), .by_group = TRUE) %>% ungroup() %>%
      group_by(DISTRICT) %>% slice(1:n) %>% ungroup()
  }
  if (!isTRUE(district))
  {
    top = data %>%
      #group_by(ST_NM) %>%
      mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
      filter(lists >= cutoff) %>%
      group_by(COMMON.NAME) %>% ## add ST_NM when required
      summarize(freq = n_distinct(group.id)/max(lists)) %>% ungroup() %>%
      #group_by(ST_NM) %>% 
      arrange(desc(freq)) %>% ungroup() %>%
      #group_by(ST_NM) %>% 
      slice(1:n) %>% ungroup()
  }
  return(top)
}
