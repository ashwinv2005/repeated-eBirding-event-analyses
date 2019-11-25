library(tidyverse)
library(ggthemes)
theme_set(theme_tufte())

source('~/GitHub/repeated-birding-event-analysis/functions.R')
readcleanrawdata("PongalBirdCount-2019.csv")
source('~/GitHub/repeated-birding-event-analysis/functions.R')
addmapvars()
data$DISTRICT = as.character(data$DISTRICT)
data[data$COUNTY.CODE == "IN-TN-KK" & is.na(data$DISTRICT),]$DISTRICT = "Kanniyakumari"
data[data$COUNTY.CODE == "IN-TN-PU" & is.na(data$DISTRICT),]$DISTRICT = "Pudukkottai"
data[data$COUNTY.CODE == "IN-TN-RA" & is.na(data$DISTRICT),]$DISTRICT = "Ramanathapuram"

family = c("Great Hornbill","Rufous-necked Hornbill","Malabar Gray Hornbill","Indian Gray Hornbill",
           "Malabar Pied-Hornbill","Narcondam Hornbill","Oriental Pied-Hornbill","Wreathed Hornbill",
           "Brown Hornbill")
datat = data[data$COMMON.NAME %in% family,]

source('~/GitHub/repeated-birding-event-analysis/functions.R')
source('~/GitHub/repeated-birding-event-analysis/surveymapfunctions.R')


surveymaps("Yellow-browed Bulbul", gridsize = 3, smooth = T, h = 0.1, cutoff = 1, showempty = F)
surveymaps("Sri Lanka Bay-Owl", gridsize = 3, smooth = T, h = 0.05, cutoff = 3, showempty = F)

plotfreqmap(data, "Ashy Prinia", "district", level = "species", season = "year round", smooth = F, 
            rich = F, add = "all lists", h = 2, cutoff = 5, 
            showempty = T, states = "Tamil Nadu")

top = topspecies(data,district = T,n=5, cutoff = 0)

write.csv(top,"top_species_district.csv")


data.frame(top)
top[c(8,14,28,40,47,54,61,63,64,65,74),]

data1 = data %>% filter(ALL.SPECIES.REPORTED == 1)

ls = data %>%
  filter(!is.na(DURATION.MINUTES), !is.na(DISTRICT)) %>%
  group_by(DISTRICT) %>% mutate(participants = n_distinct(OBSERVER.ID)) %>% ungroup %>%
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% slice(1) %>% ungroup %>%
  group_by(DISTRICT) %>% mutate(meanhours = mean(DURATION.MINUTES/60)) %>% ungroup %>%
  group_by(DISTRICT,OBSERVER.ID) %>% mutate(birdinghours = sum(DURATION.MINUTES/60)) %>% ungroup %>%
  group_by(DISTRICT,OBSERVER.ID) %>% slice(1) %>% ungroup %>%
  group_by(DISTRICT) %>% arrange(desc(birdinghours), .by_group = T) %>%
  select(DISTRICT,participants,meanhours,FIRST.NAME,LAST.NAME,birdinghours)

write.csv(ls,"district_participant_hours.csv")


ls = data %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% slice(1) %>% ungroup %>%
  group_by(OBSERVER.ID) %>% mutate(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% ungroup %>%
  group_by(OBSERVER.ID) %>% slice(1) %>% ungroup %>%
  arrange(desc(lists)) %>%
  select(FIRST.NAME,LAST.NAME,lists)

write.csv(ls,"top_complete_lists.csv")
