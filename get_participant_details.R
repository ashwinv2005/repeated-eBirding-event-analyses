library(tidyverse)

a = read.csv("GBA_Users.csv")
b = read.csv("eBirder_details.csv")

a$obs.id.num = gsub("[[:alpha:]]", "", a$OBSERVER.ID) 
b$obs.id.num = gsub("[[:alpha:]]", "", b$OBSERVER.ID)
b = b %>%
  dplyr::select(-OBSERVER.ID)

a = a %>% left_join(b)

write.csv(a,"GBA_emails.csv",row.names = F)
