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
           "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","STATE.CODE","COUNTY.CODE","EXOTIC.CODE",
           "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
           "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","LOCALITY","FIRST.NAME","LAST.NAME",
           "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")

nms = read.delim("ebd_IN_202205_202205_relMay-2022.txt", nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

# read data from certain columns only
data1 = read.delim("ebd_IN_202205_202205_relMay-2022.txt", colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ","null",NA))
data1$COUNTRY = "India"
datab = read.delim("ebd_BT_202205_202205_relMay-2022.txt", colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ","null",NA))
datab$COUNTRY = "Bhutan"
datan = read.delim("ebd_NP_202205_202205_relMay-2022.txt", colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ","null",NA))
datan$COUNTRY = "Nepal"

data1 = rbind(data1,datab,datan)


## choosing important columns required for further analyses

data = data1
data = data %>% filter(OBSERVATION.DATE %in% c("2022-05-14"))

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

load("maps.RData")
bhutan = readOGR("btn_admbnda_bnlc_20201026_shp","btn_admbnda_adm1_bnlc_20201026")
nepal = readOGR("npl_admbnda_nd_20201117_shp","npl_admbnda_adm1_nd_20201117")
bhutans = readOGR("btn_admbnda_bnlc_20201026_shp","btn_admbnda_adm0_bnlc_20201026")
nepals = readOGR("npl_admbnda_nd_20201117_shp","npl_admbnda_adm0_nd_20201117")

districtmap1 = gSimplify(districtmap, tol=0.01, topologyPreserve=TRUE)
d1 = districtmap@data[,1:2]
d1$COUNTRY = "India"
districtmap1 = sp::SpatialPolygonsDataFrame(districtmap1, d1)
bhutandistmap = gSimplify(bhutan, tol=0.01, topologyPreserve=TRUE)
bd1 = bhutan@data[,c(3,8)]
names(bd1) = c("DISTRICT","ST_NM")
bd1$ST_NM = "BHUTAN"
bd1$COUNTRY = "Bhutan"
bhutandistmap = sp::SpatialPolygonsDataFrame(bhutandistmap, bd1)
nepaldistmap = gSimplify(nepal, tol=0.01, topologyPreserve=TRUE)
nd1 = nepal@data[,c(3,8)]
names(nd1) = c("DISTRICT","ST_NM")
nd1$ST_NM = "NEPAL"
nd1$COUNTRY = "Nepal"
nepaldistmap = sp::SpatialPolygonsDataFrame(nepaldistmap, nd1)

districtmap1 = rbind(districtmap1,bhutandistmap,nepaldistmap)



statemap1 = gSimplify(statemap, tol=0.01, topologyPreserve=TRUE)
s1 = statemap@data[,2:3]
names(s1)[1] = "ST_NM"
s1$COUNTRY = "India"
s1 = s1[,-2]
statemap1 = sp::SpatialPolygonsDataFrame(statemap1, s1)
bhutanstmap = gSimplify(bhutans, tol=0.01, topologyPreserve=TRUE)
bs1 = bhutans@data[,c(3:4)]
names(bs1) = c("ST_NM")
bs1$ST_NM = "BHUTAN"
bs1$COUNTRY = "Bhutan"
bs1 = bs1[,-2]
bhutanstmap = sp::SpatialPolygonsDataFrame(bhutanstmap, bs1)
nepalstmap = gSimplify(nepals, tol=0.01, topologyPreserve=TRUE)
ns1 = nepals@data[,3:4]
names(ns1) = c("ST_NM")
ns1$ST_NM = "NEPAL"
ns1$COUNTRY = "Nepal"
ns1 = ns1[,-2]
nepalstmap = sp::SpatialPolygonsDataFrame(nepalstmap, ns1)

statemap1 = rbind(statemap1,bhutanstmap,nepalstmap)


data = data %>% filter(!is.na(group.id))

# add columns with DISTRICT and ST_NM to main data 

temp = data %>% group_by(group.id) %>% slice(1) # same group ID, same grid/district/state 

rownames(temp) = temp$group.id # only to setup adding the group.id column for the future left_join
coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
proj4string(temp) = "+proj=longlat +datum=WGS84"
temp = over(temp,districtmap1) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
temp = data.frame(temp) # convert into data frame for left_join
temp = temp[,1:2]
temp$group.id = rownames(temp) # add column to join with the main data
data = left_join(temp,data)


############ Add first and last name

data$obs.id.num <- gsub("[[:alpha:]]", "", data$OBSERVER.ID)

eBird.users = read.delim("ebd_users_relMar-2022.txt", sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                         na.strings = c(""," ",NA))
names(eBird.users) = c("USER_ID","FIRST.NAME","LAST.NAME")
eBird.users$obs.id.num = gsub("[[:alpha:]]", "", eBird.users$USER_ID)

data = left_join(data, eBird.users)
data$FIRST.NAME[data$obs.id.num == "2965391"] = "Jekmat"
data$LAST.NAME[data$obs.id.num == "2965391"] = "Norbu"
data$FIRST.NAME[data$obs.id.num == "2383369"] = "Joho"
data$LAST.NAME[data$obs.id.num == "2383369"] = "Tayu"
data$FIRST.NAME[data$obs.id.num == "3052603"] = "Timai"
data$LAST.NAME[data$obs.id.num == "3052603"] = "Miwu"
data$FIRST.NAME[data$obs.id.num == "3141253"] = "Pronov"
data$LAST.NAME[data$obs.id.num == "3141253"] = "Mega"
data$FIRST.NAME[data$obs.id.num == "1459352"] = "Millo"
data$LAST.NAME[data$obs.id.num == "1459352"] = "Tako"
data$FIRST.NAME[data$obs.id.num == "3142798"] = "Kartik"
data$LAST.NAME[data$obs.id.num == "3142798"] = "S M Chauhan"
data$FIRST.NAME[data$obs.id.num == "2948496"] = "P"
data$LAST.NAME[data$obs.id.num == "2948496"] = "K"
data$FIRST.NAME[data$obs.id.num == "3152273"] = "Ayush"
data$LAST.NAME[data$obs.id.num == "3152273"] = "Tripathi"
data$FIRST.NAME[data$obs.id.num == "2348304"] = "Atul"
data$LAST.NAME[data$obs.id.num == "2348304"] = "Mohinder"
data$FIRST.NAME[data$obs.id.num == "3150361"] = "Devi"
data$LAST.NAME[data$obs.id.num == "3150361"] = "Ram"
data$FIRST.NAME[data$obs.id.num == "3151155"] = "Gajendra"
data$LAST.NAME[data$obs.id.num == "3151155"] = "Choudhary"
data$FIRST.NAME[data$obs.id.num == "3148702"] = "Ravikant"
data$LAST.NAME[data$obs.id.num == "3148702"] = "Choudhary"
data$FIRST.NAME[data$obs.id.num == "3152489"] = "Mohti kumar"
data$LAST.NAME[data$obs.id.num == "3152489"] = "Yadav"
data$FIRST.NAME[data$obs.id.num == "3152488"] = "Nikhil"
data$LAST.NAME[data$obs.id.num == "3152488"] = "dwivedi"
data$FIRST.NAME[data$obs.id.num == "3155263"] = "NIRBHAY"
data$LAST.NAME[data$obs.id.num == "3155263"] = "SHAHI SHAHI"
data$FIRST.NAME[data$obs.id.num == "3148843"] = "Amit"
data$LAST.NAME[data$obs.id.num == "3148843"] = "Kumar"
data$FIRST.NAME[data$obs.id.num == "3155923"] = "Rijul"
data$LAST.NAME[data$obs.id.num == "3155923"] = "Kansal"
data$FIRST.NAME[data$obs.id.num == "3149478"] = "SARVESHWAR PRATAP"
data$LAST.NAME[data$obs.id.num == "3149478"] = "SINGH"


data$FIRST.NAME[data$obs.id.num == "3150455"] = "Fahad"
data$LAST.NAME[data$obs.id.num == "3150455"] = "khan"
data$FIRST.NAME[data$obs.id.num == "3150931"] = "Sushant"
data$LAST.NAME[data$obs.id.num == "3150931"] = "Shukla"
data$FIRST.NAME[data$obs.id.num == "3145855"] = "Akhilesh"
data$LAST.NAME[data$obs.id.num == "3145855"] = "Tiwari"
data$FIRST.NAME[data$obs.id.num == "3150359"] = "Ashish"
data$LAST.NAME[data$obs.id.num == "3150359"] = "Tiwari"
data$FIRST.NAME[data$obs.id.num == "3149775"] = "SHIVAM"
data$LAST.NAME[data$obs.id.num == "3149775"] = "GUPTA"
data$FIRST.NAME[data$obs.id.num == "3153236"] = "Swati"
data$LAST.NAME[data$obs.id.num == "3153236"] = "Singh"
data$FIRST.NAME[data$obs.id.num == "3152605"] = "Shashank"
data$LAST.NAME[data$obs.id.num == "3152605"] = "Gupta"
data$FIRST.NAME[data$obs.id.num == "3149483"] = "Sharavan"
data$LAST.NAME[data$obs.id.num == "3149483"] = "Kumar"
data$FIRST.NAME[data$obs.id.num == "3149679"] = "anand"
data$LAST.NAME[data$obs.id.num == "3149679"] = "Shekhar"
data$FIRST.NAME[data$obs.id.num == "3153194"] = "PRASHANT"
data$LAST.NAME[data$obs.id.num == "3153194"] = "SHARMA"
data$FIRST.NAME[data$obs.id.num == "3152138"] = "Manisha"
data$LAST.NAME[data$obs.id.num == "3152138"] = "Kukreti"
data$FIRST.NAME[data$obs.id.num == "3150332"] = "Abhishek"
data$LAST.NAME[data$obs.id.num == "3150332"] = "Upadhyay"
data$FIRST.NAME[data$obs.id.num == "3150486"] = "Sonam"
data$LAST.NAME[data$obs.id.num == "3150486"] = "Dixit"
data$FIRST.NAME[data$obs.id.num == "3152575"] = "Pragati"
data$LAST.NAME[data$obs.id.num == "3152575"] = "Chauhan"
data$FIRST.NAME[data$obs.id.num == "3150654"] = "Vishal Singh"
data$LAST.NAME[data$obs.id.num == "3150654"] = "Rathore"
data$FIRST.NAME[data$obs.id.num == "3150766"] = "Gaurav"
data$LAST.NAME[data$obs.id.num == "3150766"] = "singh"
data$FIRST.NAME[data$obs.id.num == "3156106"] = "RISHI"
data$LAST.NAME[data$obs.id.num == "3156106"] = "KUMAR"
data$FIRST.NAME[data$obs.id.num == "3152481"] = "ankita"
data$LAST.NAME[data$obs.id.num == "3152481"] = "kishore"

data$FIRST.NAME[data$obs.id.num == "3151696"] = "Reeta"
data$LAST.NAME[data$obs.id.num == "3151696"] = "Tiwari"
data$FIRST.NAME[data$obs.id.num == "3153019"] = "Amit"
data$LAST.NAME[data$obs.id.num == "3153019"] = "Singh"
data$FIRST.NAME[data$obs.id.num == "3153043"] = "Aditya"
data$LAST.NAME[data$obs.id.num == "3153043"] = "Saxena"
data$FIRST.NAME[data$obs.id.num == "3170527"] = "aishah"
data$LAST.NAME[data$obs.id.num == "3170527"] = "naseem"
data$FIRST.NAME[data$obs.id.num == "3150243"] = "Vishal"
data$LAST.NAME[data$obs.id.num == "3150243"] = "Rawat"
data$FIRST.NAME[data$obs.id.num == "3153233"] = "Pradeep"
data$LAST.NAME[data$obs.id.num == "3153233"] = "Kumar"
data$FIRST.NAME[data$obs.id.num == "3153176"] = "Vinay Kumar"
data$LAST.NAME[data$obs.id.num == "3153176"] = "Singh"

data$FIRST.NAME[data$obs.id.num == "3107657"] = "Aiyush"
data$LAST.NAME[data$obs.id.num == "3107657"] = "Saini"
data$FIRST.NAME[data$obs.id.num == "1642105"] = "Wasim"
data$LAST.NAME[data$obs.id.num == "1642105"] = "Malik"
data$FIRST.NAME[data$obs.id.num == "2948734"] = "Mansoor"
data$LAST.NAME[data$obs.id.num == "2948734"] = "Shayir"
data$FIRST.NAME[data$obs.id.num == "3153578"] = "Mushtaq"
data$LAST.NAME[data$obs.id.num == "3153578"] = "Tabassum"
data$FIRST.NAME[data$obs.id.num == "3153344"] = "Tahir ali"
data$LAST.NAME[data$obs.id.num == "3153344"] = "Kazmi"
data$FIRST.NAME[data$obs.id.num == "3168349"] = "Ameed"
data$LAST.NAME[data$obs.id.num == "3168349"] = "Shafi"

######## 

data$FIRST.NAME[data$obs.id.num == "1563981"] = "Muddassir"
data$LAST.NAME[data$obs.id.num == "1563981"] = "Ahmad"
data$FIRST.NAME[data$obs.id.num == "2774657"] = "Rigzen"
data$LAST.NAME[data$obs.id.num == "2774657"] = "Mingyur"
data$FIRST.NAME[data$obs.id.num == "1698994"] = "GHULAM"
data$LAST.NAME[data$obs.id.num == "1698994"] = "HASSANAINE"
data$FIRST.NAME[data$obs.id.num == "3150547"] = "Stanzin"
data$LAST.NAME[data$obs.id.num == "3150547"] = "Yangzon"
data$FIRST.NAME[data$obs.id.num == "3153377"] = "Stanzin"
data$LAST.NAME[data$obs.id.num == "3153377"] = "Putith"
data$FIRST.NAME[data$obs.id.num == "3153244"] = "Stanzin"
data$LAST.NAME[data$obs.id.num == "3153244"] = "Angmo"
data$FIRST.NAME[data$obs.id.num == "3153453"] = "Mohammad Umer"
data$LAST.NAME[data$obs.id.num == "3153453"] = "Borkan"
data$FIRST.NAME[data$obs.id.num == "3150632"] = "Stanzin"
data$LAST.NAME[data$obs.id.num == "3150632"] = "Doton"
data$FIRST.NAME[data$obs.id.num == "3153343"] = "Dechen"
data$LAST.NAME[data$obs.id.num == "3153343"] = "Chuzin"
data$FIRST.NAME[data$obs.id.num == "3153465"] = "Hagerah"
data$LAST.NAME[data$obs.id.num == "3153465"] = "Banii"
data$FIRST.NAME[data$obs.id.num == "3153451"] = "Padma"
data$LAST.NAME[data$obs.id.num == "3153451"] = "Lhazes"
data$FIRST.NAME[data$obs.id.num == "3153432"] = "Diskit"
data$LAST.NAME[data$obs.id.num == "3153432"] = "Zamgmo"

data$FIRST.NAME[data$obs.id.num == "2966591"] = "Kaisang"
data$LAST.NAME[data$obs.id.num == "2966591"] = "Angail"
data$FIRST.NAME[data$obs.id.num == "1473319"] = "Kesang"
data$LAST.NAME[data$obs.id.num == "1473319"] = "Dorjey"
data$FIRST.NAME[data$obs.id.num == "1748365"] = "Lobsang"
data$LAST.NAME[data$obs.id.num == "1748365"] = "Palden"

# Sikkim

data$FIRST.NAME[data$obs.id.num == "1557878"] = "Bishal"
data$LAST.NAME[data$obs.id.num == "1557878"] = "Thakuri"
data$FIRST.NAME[data$obs.id.num == "3149214"] = "Basant"
data$LAST.NAME[data$obs.id.num == "3149214"] = "Sharma"
data$FIRST.NAME[data$obs.id.num == "3150568"] = "Aarati"
data$LAST.NAME[data$obs.id.num == "3150568"] = "Chettri"
data$FIRST.NAME[data$obs.id.num == "2917935"] = "Adi"
data$LAST.NAME[data$obs.id.num == "2917935"] = "Gurung"

# Uttarakhand

data$FIRST.NAME[data$obs.id.num == "949205"] = "Sanjay"
data$LAST.NAME[data$obs.id.num == "949205"] = "Chatterji"
data$FIRST.NAME[data$obs.id.num == "3126415"] = "Dinesh"
data$LAST.NAME[data$obs.id.num == "3126415"] = "Negi"
data$FIRST.NAME[data$obs.id.num == "3104695"] = "HEMANT"
data$LAST.NAME[data$obs.id.num == "3104695"] = "BISHT"
data$FIRST.NAME[data$obs.id.num == "2087709"] = "Rashi"
data$LAST.NAME[data$obs.id.num == "2087709"] = "Gupta"
data$FIRST.NAME[data$obs.id.num == "3149856"] = "Samir"
data$LAST.NAME[data$obs.id.num == "3149856"] = "Rana"
data$FIRST.NAME[data$obs.id.num == "3080812"] = "Lakshita"
data$LAST.NAME[data$obs.id.num == "3080812"] = "Bhati"
data$FIRST.NAME[data$obs.id.num == "2870588"] = "Anuj"
data$LAST.NAME[data$obs.id.num == "2870588"] = "Joshi"

# West Bengal

data$FIRST.NAME[data$obs.id.num == "3101335"] = "Shilpita"
data$LAST.NAME[data$obs.id.num == "3101335"] = "Mandal"
data$FIRST.NAME[data$obs.id.num == "3161301"] = "Madhu"
data$LAST.NAME[data$obs.id.num == "3161301"] = "Rai"
data$FIRST.NAME[data$obs.id.num == "3132978"] = "Robin"
data$LAST.NAME[data$obs.id.num == "3132978"] = "Rai"


EBD_totalst = unique(statemap1[statemap1@data$COUNTRY == "India",]@data$ST_NM)
EBD_totaldt = districtmap1[districtmap1@data$COUNTRY == "India",]@data$DISTRICT

######## filter for HBC

HBC_st = c("LADAKH","JAMMU & KASHMIR","HIMACHAL PRADESH","UTTARAKHAND",
           "SIKKIM","ARUNACHAL PRADESH","BHUTAN","NEPAL")
HBC_dt = c("Darjiling","Kalimpong","Alipurduar","Hoshiarpur","Rupnagar",
           "Pilibhit","Lakhimpur Kheri","Kokrajhar","Chirang","Baksa",
           "Sonitpur","Dhemaji","Lakhimpur","Panchkula","Udalguri","Biswanath",
           "Kheri","Pathankot","Una","Sahibzada Ajit Singh Nag*","Saharanpur",
           "Yamunanagar","Bahraich","Shrawasti","Balrampur","Pashchim Champaran",
           "Purba Champaran")

datae = data %>%
  filter(COUNTRY == "India") %>%
  filter(!obs.id.num %in% c(3145356,1516112,2482229,2220798,1614016,3129530,2756820))

datah = data %>%
  filter(ST_NM %in% HBC_st | DISTRICT %in% HBC_dt & 
           (ST_NM != "UTTAR PRADESH" | DISTRICT != "Hamirpur") &
           (ST_NM != "CHHATTISGARH")) %>%
  filter(!obs.id.num %in% c(3145356,1516112,2482229,2220798,1614016,3129530,2756820))

HBC_totaldt = districtmap1[districtmap1@data$ST_NM %in% HBC_st |
                             districtmap1@data$DISTRICT %in% HBC_dt,]@data$DISTRICT

HBC_totalst = unique(districtmap1[districtmap1@data$ST_NM %in% HBC_st |
                             districtmap1@data$DISTRICT %in% HBC_dt,]@data$ST_NM)

temp = datae %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  filter(!EXOTIC.CODE %in% c("X")) %>%
  distinct(COMMON.NAME,SCIENTIFIC.NAME)

write.csv(temp,"EBDspecieslist2022.csv", row.names=FALSE)

temp = datah %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  filter(!EXOTIC.CODE %in% c("X")) %>%
  distinct(COMMON.NAME,SCIENTIFIC.NAME)

write.csv(temp,"HBCspecieslist2022.csv", row.names=FALSE)


#rem = unique(data[is.na(data$FIRST.NAME),]$obs.id.num)


############ Top 30 checklist uploaders HBC

datax = datah %>% filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14)
datax = datax %>%
  group_by(FIRST.NAME,LAST.NAME,ST_NM,OBSERVER.ID) %>% 
  summarize(lists15mins = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% ungroup %>%
  arrange(desc(lists15mins)) %>% slice(1:30)

write.csv(datax,"HBCtop30birders2022.csv", row.names=FALSE)

########## Top birder in every state HBC

datax = datah %>% filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14)
datax = datax %>%
  group_by(ST_NM,FIRST.NAME,LAST.NAME,obs.id.num) %>% summarize(lists15mins = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% ungroup %>%
  group_by(ST_NM) %>% arrange(desc(lists15mins)) %>% slice(1) %>% ungroup %>% arrange(desc(lists15mins)) %>%
  filter(!is.na(ST_NM))

write.csv(datax,"HBCtopbirderperstate2022.csv", row.names=FALSE)


############ birders per state HBC

datax = datah %>%
  distinct(ST_NM,obs.id.num,FIRST.NAME,LAST.NAME) %>%
  arrange(ST_NM) %>%   filter(!is.na(ST_NM))
write.csv(datax,"HBCbirdersperstate2022.csv", row.names=FALSE) 

newbirders = c(2965391,2383369,3052603,3141253,1459352,3142798,2948496,3152273,2348304,
3150361,3151155,3148702,3152489,3152488,3155263,3148843,3155923,3149478,3150455,3150931,
3145855,3150359,3149775,3153236,3152605,3149483,3149679,3153194,3152138,3150332,3150486,
3152575,3150654,3150766,3156106,3152481,3151696,3153019,3153043,3170527,3150243,3153233,
3153176,3107657,1642105,2948734,3153578,3153344,3168349,1563981,2774657,1698994,3150547,
3153377,3153244,3153453,3150632,3153343,3153465,3153451,3153432,2966591,1473319,1748365,
1557878,3149214,3150568,2917935,949205,3126415,3104695,2087709,3149856,3080812,2870588,
3101335,3161301,3132978)

datax = datax %>% filter(obs.id.num %in% newbirders) %>% distinct(ST_NM,FIRST.NAME,LAST.NAME)

write.csv(datax,"newbirdersperstate2022.csv", row.names=FALSE) 



############ state level table of checklists EBD

datax = datae %>%
  group_by(ST_NM) %>% summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>%
  arrange(desc(lists)) %>% filter(!is.na(ST_NM))
write.csv(datax,"EBDlistsperstate2022.csv", row.names=FALSE)

############ state and district level table of checklists HBC

datax = datah %>%
  group_by(DISTRICT) %>% summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>%
  arrange(desc(lists)) %>% filter(!is.na(DISTRICT))
write.csv(datax,"HBClistsperdistrict2022.csv", row.names=FALSE)

datax = datah %>%
  group_by(ST_NM) %>% summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>%
  arrange(desc(lists)) %>% filter(!is.na(ST_NM))
write.csv(datax,"HBClistsperstate2022.csv", row.names=FALSE)


############# common species EBD

datax = datae %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(ST_NM) %>% mutate(statelist = n_distinct(group.id)) %>% ungroup %>%
  filter(statelist > 10) %>%
  group_by(COMMON.NAME,ST_NM) %>% summarize(freq = n_distinct(group.id)/max(statelist)) %>% ungroup %>%
  mutate(nstate = n_distinct(ST_NM)) %>% ungroup %>%
  group_by(COMMON.NAME) %>% summarize(freq = sum(freq)/max(nstate)) %>% ungroup %>%
  arrange(desc(freq))
write.csv(datax,"EBDcommonspecies2022.csv", row.names=FALSE)


############# common species HBC

datax = datah %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(ST_NM) %>% mutate(statelist = n_distinct(group.id)) %>% ungroup %>%
  filter(statelist > 10) %>%
  group_by(COMMON.NAME,ST_NM) %>% summarize(freq = n_distinct(group.id)/max(statelist)) %>% ungroup %>%
  mutate(nstate = n_distinct(ST_NM)) %>% ungroup %>%
  group_by(COMMON.NAME) %>% summarize(freq = sum(freq)/max(nstate)) %>% ungroup %>%
  arrange(desc(freq))

write.csv(datax,"HBCcommonspecies2022.csv", row.names=FALSE)


######################################### Plot district map with info EBD

districtmap1 = gBuffer(districtmap1, byid=TRUE, width=0)
finalmap = districtmap1[districtmap1@data$COUNTRY == "India",]
statemap1 = gBuffer(statemap1, byid=TRUE, width=0)
finalmap1 = statemap1[statemap1@data$COUNTRY == "India",]


temp = datae %>% 
  filter(CATEGORY %in% c("species","issf")) %>%
  filter(is.na(EXOTIC.CODE)) %>%
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
names(effortmap)[1:2] = c("District","State")


proj4string(finalmap) = "+proj=longlat +datum=WGS84"
proj4string(finalmap1) = "+proj=longlat +datum=WGS84"
proj4string(effortmap) = "+proj=longlat +datum=WGS84"

mapviewOptions(fgb = FALSE)
a = mapView(effortmap, zcol = c("Checklists"), map.types = c("Esri.WorldImagery"),
            layer.name = c("Checklists"), 
            popup = leafpop::popupTable(effortmap,c("District","Checklists",
                                                    "Participants","Locations","Species"), 
                                        feature.id=FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,20,50,100,200,max(na.omit(effortmap$Checklists)))), alpha.regions = 0.6)

b = mapview(as(finalmap1, "SpatialLines"), color = "black", lwd = 1)

mapshot(a+b, "EBD_districts_2022.html")


######################################### Plot state map with info EBD

statemap1 = gBuffer(statemap1, byid=TRUE, width=0)
finalmap = statemap1[statemap1@data$COUNTRY == "India",]

temp = data %>% 
  filter(CATEGORY %in% c("species","issf")) %>%
  filter(is.na(EXOTIC.CODE)) %>%
  group_by(ST_NM) %>%
  summarize(Species = n_distinct(COMMON.NAME))

temp1 = data %>% 
  group_by(ST_NM) %>%
  summarize(Checklists = n_distinct(SAMPLING.EVENT.IDENTIFIER))

temp2 = data %>% 
  group_by(ST_NM) %>%
  summarize(Participants = n_distinct(OBSERVER.ID))

temp3 = data %>% 
  group_by(ST_NM) %>%
  summarize(Locations = n_distinct(LOCALITY.ID))

temp = left_join(temp,temp1)
temp = left_join(temp,temp2)
temp = left_join(temp,temp3)


temp$ST_NM = as.character(temp$ST_NM)
temp$GBBCweb = '<a href = "https://ebird.org/region/IN?yr=BIGDAY_2022a&rank=lrec"> Click on the State for more information </a>'

effortmap = merge(finalmap,temp, by =  "ST_NM")
names(effortmap)[1] = c("State")



proj4string(finalmap) = "+proj=longlat +datum=WGS84"
proj4string(effortmap) = "+proj=longlat +datum=WGS84"

mapviewOptions(fgb = FALSE)
b = mapView(effortmap, zcol = c("Checklists"), map.types = c("Esri.WorldImagery"),
            layer.name = c("Checklists"), 
            popup = leafpop::popupTable(effortmap,c("State","Checklists",
                                                    "Participants","Locations","Species"), 
                                        feature.id=FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,20,50,100,200,500,max(na.omit(effortmap$Checklists)))), alpha.regions = 0.6)

c = mapview(as(finalmap1, "SpatialLines"), color = "black", lwd = 2)

mapshot(b+c, "EBD_states_2022.html")


######################################### Plot district map with info HBC

districtmap1 = gBuffer(districtmap1, byid=TRUE, width=0)
finalmap = districtmap1[(districtmap1@data$DISTRICT %in% HBC_totaldt) &
                          (districtmap1@data$ST_NM %in% HBC_totalst) &
                          (districtmap1@data$DISTRICT != "Hamirpur" |
                             districtmap1@data$ST_NM != "UTTAR PRADESH") &
                          (districtmap1@data$ST_NM != "CHHATTISGARH"),]
statemap1 = gBuffer(statemap1, byid=TRUE, width=0)
HBC_totalstr = HBC_totalst[!HBC_totalst %in% c("PUNJAB","HARYANA","UTTAR PRADESH",
                                               "ASSAM","WEST BENGAL","BIHAR","CHHATTISGARH")]
finalmap1 = statemap1[statemap1@data$ST_NM %in% HBC_totalstr,]

npbhmap = statemap1[statemap1@data$ST_NM %in% c("BHUTAN","NEPAL"),]


temp = datah %>% 
  filter(CATEGORY %in% c("species","issf")) %>%
  filter(is.na(EXOTIC.CODE)) %>%
  group_by(DISTRICT) %>%
  summarize(Species = n_distinct(COMMON.NAME))

temp1 = datah %>% 
  group_by(DISTRICT) %>%
  summarize(Checklists = n_distinct(SAMPLING.EVENT.IDENTIFIER))

temp2 = datah %>% 
  group_by(DISTRICT) %>%
  summarize(Participants = n_distinct(OBSERVER.ID))

temp3 = datah %>% 
  group_by(DISTRICT) %>%
  summarize(Locations = n_distinct(LOCALITY.ID))

temp = left_join(temp,temp1)
temp = left_join(temp,temp2)
temp = left_join(temp,temp3)


temp$DISTRICT = as.character(temp$DISTRICT)

effortmap = merge(finalmap,temp, by =  "DISTRICT")
names(effortmap)[1:2] = c("District","State")


proj4string(finalmap) = "+proj=longlat +datum=WGS84"
proj4string(finalmap1) = "+proj=longlat +datum=WGS84"
proj4string(effortmap) = "+proj=longlat +datum=WGS84"

mapviewOptions(fgb = FALSE)
a = mapView(effortmap, zcol = c("Checklists"), map.types = c("Esri.WorldImagery"),
            layer.name = c("Checklists"), 
            popup = leafpop::popupTable(effortmap,c("District","Checklists",
                                                    "Participants","Locations","Species"), 
                                        feature.id=FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,5,20,50,100,150,max(na.omit(effortmap$Checklists)))), alpha.regions = 0.6)

b = mapview(as(finalmap1, "SpatialLines"), color = "black", lwd = 2)

d = mapview(as(npbhmap, "SpatialLines"), color = "black", lwd = 4)


mapshot(a+b+d, "HBC_districts_2022.html")



######################################### Plot state map with info HBC

statemap1 = gBuffer(statemap1, byid=TRUE, width=0)
HBC_totalstr = HBC_totalst[!HBC_totalst %in% c("PUNJAB","HARYANA","UTTAR PRADESH",
                                               "ASSAM","WEST BENGAL","BIHAR","CHHATTISGARH")]
finalmap = statemap1[statemap1@data$ST_NM %in% HBC_totalstr,]

temp = datah %>% 
  filter(CATEGORY %in% c("species","issf")) %>%
  filter(is.na(EXOTIC.CODE)) %>%
  group_by(ST_NM) %>%
  summarize(Species = n_distinct(COMMON.NAME))

temp1 = datah %>% 
  group_by(ST_NM) %>%
  summarize(Checklists = n_distinct(SAMPLING.EVENT.IDENTIFIER))

temp2 = datah %>% 
  group_by(ST_NM) %>%
  summarize(Participants = n_distinct(OBSERVER.ID))

temp3 = datah %>% 
  group_by(ST_NM) %>%
  summarize(Locations = n_distinct(LOCALITY.ID))

temp = left_join(temp,temp1)
temp = left_join(temp,temp2)
temp = left_join(temp,temp3)


temp$ST_NM = as.character(temp$ST_NM)
temp$GBBCweb = '<a href = "https://ebird.org/gbbc/region/IN/regions?yr=EBIRD_GBBC_2022&m="> Click on the State for more information </a>'

effortmap = merge(finalmap,temp, by =  "ST_NM")
names(effortmap)[1] = c("State")



proj4string(finalmap) = "+proj=longlat +datum=WGS84"
proj4string(effortmap) = "+proj=longlat +datum=WGS84"

mapviewOptions(fgb = FALSE)
b = mapView(effortmap, zcol = c("Checklists"), map.types = c("Esri.WorldImagery"),
            layer.name = c("Checklists"), 
            popup = leafpop::popupTable(effortmap,c("State","Checklists",
                                                    "Participants","Locations","Species"), 
                                        feature.id=FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(40,60,100,150,max(na.omit(effortmap$Checklists)))), alpha.regions = 0.6)

c = mapview(as(finalmap1, "SpatialLines"), color = "black", lwd = 2)

d = mapview(as(npbhmap, "SpatialLines"), color = "black", lwd = 4)

mapshot(b+c+d, "HBC_states_2022.html")



############################ Common species EBD

north = c("PUNJAB","HARYANA","UTTAR PRADESH","DELHI","BIHAR","CHANDIGARH")

west = c("GUJARAT","RAJASTHAN","DADRA & NAGAR HAVE","DAMAN & DIU")

himalayas = c("JAMMU & KASHMIR","LADAKH","UTTARAKHAND","HIMACHAL PRADESH")

central = c("MADHYA PRADESH","CHHATTISGARH","MAHARASHTRA","JHARKHAND","ODISHA")

south = c("ANDHRA PRADESH","TELANGANA","KARNATAKA","KERALA","TAMIL NADU","GOA","PUDUCHERRY")

east = c("ARUNACHAL PRADESH","NAGALAND","MANIPUR","TRIPURA","MIZORAM","SIKKIM","WEST BENGAL","ASSAM","MEGHALAYA")

an = c("ANDAMAN & NICOBAR","LAKSHADWEEP")

data2 = datae %>%
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

specs = data2 %>%
  filter(CATEGORY %in% c("species","issf")) %>%
  filter(is.na(EXOTIC.CODE)) %>%
  group_by(region) %>% summarize(nspecies = n_distinct(COMMON.NAME))

lists = data2 %>%
  group_by(region) %>% summarize(checklists = n_distinct(SAMPLING.EVENT.IDENTIFIER))

regionsummary = left_join(lists,specs)

write.csv(regionsummary,"EBD_REGIONsummary_2022.csv",row.names = F)

cosp = data2 %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(ST_NM) %>% mutate(statelist = n_distinct(group.id)) %>% ungroup %>%
  filter(statelist > 10) %>%
  group_by(COMMON.NAME,region,ST_NM) %>% summarize(freq = n_distinct(group.id)/max(statelist)) %>% ungroup %>%
  group_by(region) %>% mutate(nstate = n_distinct(ST_NM)) %>% ungroup %>%
  group_by(COMMON.NAME,region) %>% summarize(freq = sum(freq)/max(nstate)) %>% ungroup %>%
  group_by(region) %>% arrange(desc(freq), .by_group = T) %>% ungroup %>%
  group_by(region) %>% slice(1:5)

write.csv(cosp,"EBD_CommonSpeciesbyRegion_2022.csv",row.names = F)


############################ Common species HBC

western = c("JAMMU & KASHMIR","LADAKH","HIMACHAL PRADESH","PUNJAB","HARYANA")

central = c("UTTARAKHAND","UTTAR PRADESH","NEPAL","BIHAR")

eastern = c("ARUNACHAL PRADESH","SIKKIM","WEST BENGAL","ASSAM","BHUTAN")

data3 = datah %>%
  filter(!is.na(ST_NM)) %>%
  mutate(region = "") %>%
  mutate(region = ifelse(ST_NM %in% western, "Western Region", region)) %>%
  mutate(region = ifelse(ST_NM %in% central, "Central Region", region)) %>%
  mutate(region = ifelse(ST_NM %in% eastern, "Eastern Region", region)) %>%
  filter(region != "")

specs = data3 %>%
  filter(CATEGORY %in% c("species","issf")) %>%
  filter(is.na(EXOTIC.CODE)) %>%
  group_by(region) %>% summarize(nspecies = n_distinct(COMMON.NAME))

lists = data3 %>%
  group_by(region) %>% summarize(checklists = n_distinct(SAMPLING.EVENT.IDENTIFIER))

regionsummary = left_join(lists,specs)

write.csv(regionsummary,"HBC_REGIONsummary_2022.csv",row.names = F)

cosp = data3 %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(ST_NM) %>% mutate(statelist = n_distinct(group.id)) %>% ungroup %>%
  filter(statelist > 10) %>%
  group_by(COMMON.NAME,region,ST_NM) %>% summarize(freq = n_distinct(group.id)/max(statelist)) %>% ungroup %>%
  group_by(region) %>% mutate(nstate = n_distinct(ST_NM)) %>% ungroup %>%
  group_by(COMMON.NAME,region) %>% summarize(freq = sum(freq)/max(nstate)) %>% ungroup %>%
  group_by(region) %>% arrange(desc(freq), .by_group = T) %>% ungroup %>%
  group_by(region) %>% slice(1:5)

write.csv(cosp,"HBC_CommonSpeciesbyRegion_2022.csv",row.names = F)


################################### plot points on map HBC


data4 = datah %>% distinct(LOCALITY.ID,LATITUDE,LONGITUDE)

ggp = ggplot() +
  geom_polygon(data = statemap1, aes(x=long, y=lat, group=group), colour = "white", fill = "black")+  
  geom_point(data = data4, aes(x=LONGITUDE,y=LATITUDE), colour = "#fcfa53", size = 1) +
  geom_polygon(data = npbhmap, aes(x=long, y=lat, group=group), colour = "white", fill = NA, size = 2)+  
  
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
        plot.background = element_rect(fill = "black",colour = NA),
        panel.background = element_rect(fill = "black", colour = NA),
        plot.title = element_text(hjust = 0.5))+
  coord_map()

n1 = "HBCpointmap2022.jpg"

print(ggp)
ggsave(file=n1, units="in", width=8, height=11)





################################################## plot region map EBD

require(ggthemes)
theme_set(theme_tufte())

fstatemap = fortify(statemap1, region = c("ST_NM"))
data5 = data2 %>% distinct(ST_NM,region)
setdiff(EBD_totalst,data5$ST_NM)

extra = data5[1:4,]
extra$ST_NM = c("DAMAN & DIU","LAKSHADWEEP","MANIPUR","DADRA & NAGAR HAVE")
extra$region = c("West","A&N","East","West")
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
  geom_polygon(data = fstatemap, aes(x=long, y=lat,group=group,fill=region,colour=region))+  
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

n1 = "EBDregionmap_2022.jpg"

print(ggp)
ggsave(file=n1, units="in", width=10, height=7)


################################################## plot region map HBC

require(ggthemes)
theme_set(theme_tufte())

fdistrictmap = fortify(districtmap1[(districtmap1@data$DISTRICT %in% HBC_totaldt) &
                                      (districtmap1@data$ST_NM %in% HBC_totalst) &
                                      (districtmap1@data$DISTRICT != "Hamirpur" |
                                         districtmap1@data$ST_NM != "UTTAR PRADESH") &
                                      (districtmap1@data$ST_NM != "CHHATTISGARH"),], 
                       region = c("DISTRICT"))
data5 = data3 %>% distinct(DISTRICT,region)
extra = data5[1:length(setdiff(HBC_totaldt,data5$DISTRICT)),]
extra$DISTRICT = setdiff(HBC_totaldt,data5$DISTRICT)
extra$region = c(rep("Western Region",7),
                 rep("Central Region",10),rep("Eastern Region",34),"Central Region")
data5 = rbind(data5,extra)
fdistrictmap = left_join(fdistrictmap,data5,by = c('id' = "DISTRICT"))
fdistrictmap = fdistrictmap %>% filter(!is.na(region))

cols = c("#869B27", "#E49B36", "#A13E2B", "#78CAE0", "#B69AC9", "#EA5599", "#31954E", "#493F3D",
         "#CC6666", "#9999CC", "#000000", "#66CC99")

ns = 3

cols1 = cols[c(1:ns)]
bks1 = c("Western Region","Central Region","Eastern Region")

plotindiamap = ggplot() +
  geom_polygon(data = fdistrictmap, aes(x=long, y=lat,group=group,fill=region,colour=region))+  
  geom_polygon(data = indiamap, aes(x=long, y=lat, group=group), colour = "black", fill = NA)+  
  geom_polygon(data = npbhmap, aes(x=long, y=lat, group=group), colour = "black", fill = NA, size = 2)+  
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
  

n1 = "HBCregionmap_2022.jpg"

print(ggp)
ggsave(file=n1, units="in", width=10, height=7)


## overall stats

stats = data.frame(metric = rep(c("participants","checklists","unique-checklists","species"),2),
                   event = rep(c("EBD","HBC"), each = 4), y2022 = NA, y2021 = NA)


########### overall stats EBD

stats$y2022[1] = length(unique(datae$OBSERVER.ID))
stats$y2022[2] = length(unique(datae$SAMPLING.EVENT.IDENTIFIER))
stats$y2022[3] = length(unique(datae$group.id))   
temp = datae %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  filter(is.na(EXOTIC.CODE)) %>%
  distinct(COMMON.NAME,SCIENTIFIC.NAME)
stats$y2022[4] = length(unique(temp$COMMON.NAME))



########### overall stats HBC

stats$y2022[5] = length(unique(datah$OBSERVER.ID))
stats$y2022[6] = length(unique(datah$SAMPLING.EVENT.IDENTIFIER))
stats$y2022[7] = length(unique(datah$group.id))   
temp = datah %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  filter(is.na(EXOTIC.CODE)) %>%
  distinct(COMMON.NAME,SCIENTIFIC.NAME)
stats$y2022[8] = length(unique(temp$COMMON.NAME))



###### previous year


preimp = c("GLOBAL.UNIQUE.IDENTIFIER","CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
           "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","STATE.CODE","COUNTY.CODE","EXOTIC.CODE",
           "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
           "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","LOCALITY","FIRST.NAME","LAST.NAME",
           "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")

nms = read.delim("ebd_IN_202105_202105_relMay-2022.txt", nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

# read data from certain columns only
data1 = read.delim("ebd_IN_202105_202105_relMay-2022.txt", colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ","null",NA))
data1$COUNTRY = "India"
datab = read.delim("ebd_BT_202105_202105_relMay-2022.txt", colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ","null",NA))
datab$COUNTRY = "Bhutan"
datan = read.delim("ebd_NP_202105_202105_relMay-2022.txt", colClasses = nms, sep = "\t", header = T, quote = "", 
                   stringsAsFactors = F, na.strings = c(""," ","null",NA))
datan$COUNTRY = "Nepal"

data1 = rbind(data1,datab,datan)


## choosing important columns required for further analyses

data = data1
data = data %>% filter(OBSERVATION.DATE %in% c("2021-05-08"))

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


data = data %>% filter(!is.na(group.id))

# add columns with DISTRICT and ST_NM to main data 

temp = data %>% group_by(group.id) %>% slice(1) # same group ID, same grid/district/state 

rownames(temp) = temp$group.id # only to setup adding the group.id column for the future left_join
coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
proj4string(temp) = "+proj=longlat +datum=WGS84"
temp = over(temp,districtmap1) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
temp = data.frame(temp) # convert into data frame for left_join
temp = temp[,1:2]
temp$group.id = rownames(temp) # add column to join with the main data
data = left_join(temp,data)


############ Add first and last name

data$obs.id.num <- gsub("[[:alpha:]]", "", data$OBSERVER.ID)

eBird.users = read.delim("ebd_users_relMar-2022.txt", sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                         na.strings = c(""," ",NA))
names(eBird.users) = c("USER_ID","FIRST.NAME","LAST.NAME")
eBird.users$obs.id.num = gsub("[[:alpha:]]", "", eBird.users$USER_ID)

data = left_join(data, eBird.users)

dataep = data %>%
  filter(COUNTRY == "India") %>%
  filter(!obs.id.num %in% c(3145356,1516112,2482229,2220798,1614016,3129530,2756820))

datahp = data %>%
  filter(ST_NM %in% HBC_st | DISTRICT %in% HBC_dt & 
           (ST_NM != "UTTAR PRADESH" | DISTRICT != "Hamirpur") &
           (ST_NM != "CHHATTISGARH")) %>%
  filter(!obs.id.num %in% c(3145356,1516112,2482229,2220798,1614016,3129530,2756820))

########### overall stats EBD prev. year

stats$y2021[1] = length(unique(dataep$OBSERVER.ID))
stats$y2021[2] = length(unique(dataep$SAMPLING.EVENT.IDENTIFIER))
stats$y2021[3] = length(unique(dataep$group.id))   
temp = dataep %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  filter(is.na(EXOTIC.CODE)) %>%
  distinct(COMMON.NAME,SCIENTIFIC.NAME)
stats$y2021[4] = length(unique(temp$COMMON.NAME))



########### overall stats HBC prev. year

stats$y2021[5] = length(unique(datahp$OBSERVER.ID))
stats$y2021[6] = length(unique(datahp$SAMPLING.EVENT.IDENTIFIER))
stats$y2021[7] = length(unique(datahp$group.id))   
temp = datahp %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  filter(is.na(EXOTIC.CODE)) %>%
  distinct(COMMON.NAME,SCIENTIFIC.NAME)
stats$y2021[8] = length(unique(temp$COMMON.NAME))

stats1a = stats %>% 
  select(-y2022) %>%
  pivot_wider(names_from = "event", names_prefix = "2021", values_from =  ("y2021"))
stats1b = stats %>% 
  select(-y2021) %>%
  pivot_wider(names_from = "event", names_prefix = "2022", values_from =  ("y2022"))

stats1 = left_join(stats1a,stats1b)
stats1 = stats1 %>% select(metric,`2021EBD`,`2022EBD`,`2021HBC`,`2022HBC`)
stats1$HBCEBD2021 = round(100*stats1$`2021HBC`/stats1$`2021EBD`)
stats1$HBCEBD2022 = round(100*stats1$`2022HBC`/stats1$`2022EBD`)


write.csv(stats1,"overall_stats.csv",row.names = F)


## HBC state and district



datap = rbind(datah,datahp)

stats_states = datap %>% group_by(cyear,ST_NM) %>% 
  summarise(participants = n_distinct(OBSERVER.ID), 
  checklists = n_distinct(SAMPLING.EVENT.IDENTIFIER),
  unique_checklists = n_distinct(group.id))

st_extra = datap %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  filter(is.na(EXOTIC.CODE)) %>%
  group_by(cyear,ST_NM) %>%
  summarise(species = n_distinct(COMMON.NAME))

stats_states = left_join(stats_states,st_extra)
stats_states1 = stats_states %>% 
  pivot_longer(-c(cyear,ST_NM), values_to = "values", names_to = "metrics")
stats_states2 = stats_states1 %>% 
  pivot_wider(names_from = "cyear", names_prefix = "y", values_from =  "values")
names(stats_states2) = c("state","metric","2021HBC","2022HBC")


stats_districts = datap %>% group_by(cyear,ST_NM,DISTRICT) %>% 
  summarise(participants = n_distinct(OBSERVER.ID), 
            checklists = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            unique_checklists = n_distinct(group.id))

dt_extra = datap %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  filter(is.na(EXOTIC.CODE)) %>%
  group_by(cyear,ST_NM,DISTRICT) %>%
  summarise(species = n_distinct(COMMON.NAME))

stats_districts = left_join(stats_districts,dt_extra)

stats_districts1 = stats_districts %>% 
  pivot_longer(-c(cyear,ST_NM,DISTRICT), values_to = "values", names_to = "metrics")
stats_districts2 = stats_districts1 %>% 
  pivot_wider(names_from = "cyear", names_prefix = "y", values_from =  "values")
names(stats_districts2) = c("state","district","metric","2021HBC","2022HBC")
stats_districts3 = stats_districts2 %>% arrange(state,district)


write.csv(stats_states2,"state_stats.csv",row.names = F)
write.csv(stats_districts3,"district_stats.csv",row.names = F)


## subset Nepal data for their app

nep = data %>% filter(ST_NM == "NEPAL")
nep1 = nep[,-c(2,4,10,11,25,26,27,28,30,33,34,35,36,37)]
names(nep1)[22:23] = c("day","year")
write.csv(nep1,"nepal_data.csv", row.names = F)
