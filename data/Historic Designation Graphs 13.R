

library(ggplot2)
library(ggthemes)
library(forecast)
library(sf)
library(tidycensus)
library(tidyr)
library(dplyr)


st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))


# average cost plot

hd_cost = data.frame(type =c('Conventional', "Historically Designated"), Cost=c(25,100))

ggplot()+geom_col(data=hd_cost, aes(x=type, y=Cost),width = 0.5)+xlab('')+ylab('Average Cost')+
  theme_fivethirtyeight() + scale_colour_fivethirtyeight() +
  theme(axis.text=element_text(size=16),axis.title.y=element_text(size=18),
        axis.title=element_text(size=16),axis.text.y=element_blank())


# city comparison

hd_per = data.frame(type =c("Philadelphia" ,'Boston',"Washington \nDC"), Cost=c(2,7,19))

ggplot()+geom_col(data=hd_per, aes(x=type, y=Cost),width = 0.5)+xlab('')+ylab('% Historically Designated\n')+
  theme_fivethirtyeight() + scale_colour_fivethirtyeight() +
  theme(axis.text=element_text(size=16),axis.title.y=element_text(size=18),
        axis.title=element_text(size=16))




#hd = read_sf('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/Website/Historic_Districts/Historic_Districts.shp')
#hd$aream2 = st_area(hd)
# MANUALLY EDITED THIS FILE DONT TOUCH write_sf(subset(hd,select = -geometry),'C:/Users/mmann/Google Drive/HousingLife/Historic Designation/Website/hd_boundary_data.csv')

# hd_pot = read_sf('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/Website/Potential_Districts.kml')
# hd_pot$aream2 = st_area(hd_pot)
# plot(hd_pot)
# head(hd_pot)
# write_sf(subset(hd_pot,select = -geometry),'C:/Users/mmann/Google Drive/HousingLife/Historic Designation/Website/hd_boundary_data_potential.csv')



# % area HD designated and planned   
########################################################################

data = read.csv('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/Website/hd_boundary_data.csv',stringsAsFactors = F)
data  = data[order(data$YEAR),]
data[,"cum_area"] <- cumsum(data$aream2)
data$Status = data$STATUS
data$Status = factor(data$Status,labels=c('Designated','Under Consideration'))

ggplot() + geom_line(data = data[!is.na(data$Status),], aes(x=YEAR, y=cum_area/1550000,colour=Status),size=2) + geom_point(size=2)+xlab("Year")+
  ylab('Percent of DC Land Area  \nHistorically Designnated\n') +
  theme_fivethirtyeight() + #scale_colour_fivethirtyeight() +
  theme(axis.text=element_text(size=16),axis.title.y=element_text(size=15),
        axis.title=element_text(size=16))



# median income compared to year of designation 
########################################################################

acs <- load_variables(2016,dataset = "acs5", cache = TRUE)


medinc_bg <- get_acs(geography = "block group", 
                     variables = c(medincome = "B19013_001"), 
                     state = "DC",geometry=T)


hd = read_sf('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/Website/Historic_Districts/Historic_Districts.shp')
hd = st_transform(hd, crs=4269)  
medinc_bg = st_transform(medinc_bg, crs=4269)  

ggplot() +
  geom_sf(data= medinc_bg,aes(fill = estimate, color = estimate))+geom_sf(data= hd,fill=NA,color='white')



hd2 <- aggregate(medinc_bg[medinc_bg,"estimate"], hd, FUN =  function(x) mean(x,na.rm=T))
hd2 = st_join(hd2, hd)
hd2

hd2 = hd2[!(hd2$LABEL %in% c('Gallaudet College HD',"Marine Barracks HD","Georgetown Visitation Convent and Prep School",
                             "Mt Vernon Seminary","National Arboretum","Observatory Hill","Rock Creek Park HD",
                             "T.R. Island Nat'l Mem.","Spingarn Campus HD","Armed Forces Retirement Home HD",
                             "C & O Canal NHP","Immaculata Seminary HD","National Zoological Park HD","Soldier's Home NHS",
                             "Washington Cathedral HD","Walter Reed Army Medical Center HD","St. Elizabeth's Hospital HD",
                             "National Mall HD")),]

# avoid multipart
hd2 = hd2 %>% 
  group_by(NAME) %>% 
  summarise(OBJECTID=first(OBJECTID),LABEL=first(LABEL),mn_income = mean(estimate,na.rm=T)) %>% 
  st_cast() 


# add year established
data = read.csv('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/Website/hd_boundary_data.csv')

hd2 = left_join(hd2,data[,names(data) %in% c('OBJECTID',"YEAR")],by="OBJECTID")

# ggplot() + 
#   geom_sf(data= hd2[hd2$YEAR<2018,],aes(fill = mn_income, color = mn_income)) 



ggplot(data= hd2[hd2$YEAR<2018,],aes(x=YEAR,y = mn_income)) + 
  geom_point() +geom_text(aes(label=LABEL),hjust=0, vjust=0)



ggplot() + 
  geom_point(data= hd2,aes(x=YEAR,y = mn_income)) +
  geom_smooth(data= hd2,aes(x=YEAR,y = mn_income))  


summary(lm(mn_income~YEAR,data=hd2))



##########################################
# Anacostia

medinc_bg <- get_acs(geography = "block group", 
                     variables = c(medincome = "B19013_001"), 
                     state = "DC",geometry=T)

st_write(medinc_bg, dsn = "C:/Users/mmann/Google Drive/HousingLife/Historic Designation/data/ACS_2016_MedInc/acs_2016_medincome.geojson",  driver = "GeoJSON")
st_write(medinc_bg, dsn = "C:/Users/mmann/Google Drive/HousingLife/Historic Designation/data/ACS_2016_MedInc/acs_2016_medincome.shp",   driver = "ESRI Shapefile")
st_write(medinc_bg, dsn = "C:/Users/mmann/Google Drive/HousingLife/Historic Designation/data/ACS_2016_MedInc/acs_2016_medincome.kml",   driver = "ESRI Shapefile")


hd = read_sf('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/Website/Historic_Districts/Historic_Districts.shp')
anacostia = read_sf('C:\\Users\\mmann\\Google Drive\\HousingLife\\Historic Designation\\data\\DC_Quadrants\\Anacostia.shp')

hd = st_transform(hd, crs=4269)  
medinc_bg = st_transform(medinc_bg, crs=4269)  
anacostia = st_transform(anacostia, crs=4269)  


hd_anacostia = hd[(hd$LABEL %in% c("Anacostia HD")),]


hd2 = aggregate(medinc_bg[medinc_bg,"estimate"], hd, FUN =  function(x) mean(x,na.rm=T))
medinc_bg2 = st_join(medinc_bg, hd_anacostia)

medinc_bg2 =  medinc_bg2 %>% filter(st_contains(anacostia, ., sparse = FALSE))
medinc_bg2$NAME.y[is.na(medinc_bg2$NAME.y)] = 'Other Ward 7 & 8' 


ggplot()+geom_boxplot(data=medinc_bg2,aes(y=estimate,fill=NAME.y))+
  theme_fivethirtyeight() + scale_fill_fivethirtyeight() +
  labs( y="Median Income", fill="")+theme(axis.title.x=element_blank(),
                                          axis.text.x=element_blank(),
                                          axis.ticks.x=element_blank())




# Percentage white 
##########################################################################

wht_bg <- get_acs(state = "DC", geography = "block group", year=2016,
                  variables =  c(wht_pop ="B02001_002",total_pop="B01001_001"), geometry = TRUE)    #blackpop = "B02001_003"

wht_bg = wht_bg %>% 
  group_by(GEOID) %>% 
  summarise(NAME=first(NAME),pr_wht = last(estimate)/first(estimate),geometry=first(geometry)) %>% 
  st_cast() 


quad = st_transform(read_sf('C:\\Users\\mmann\\Google Drive\\HousingLife\\Historic Designation\\data\\DC_Quadrants\\DC_Quadrants.shp'), crs=4269)  
hd = read_sf('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/Website/Historic_Districts/Historic_Districts.shp')
hd = st_transform(hd, crs=4269)  
wht_bg = st_transform(wht_bg, crs=4269)  



hd2 <- aggregate(wht_bg[wht_bg,"pr_wht"], hd, FUN =  function(x) mean(x,na.rm=T))
hd2 = st_join(hd2, hd)
hd2 = st_join(hd2, quad[,c('QUADRANT','geometry')])

hd2



hd2 = hd2[!(hd2$LABEL %in% c('Gallaudet College HD',"Marine Barracks HD","Georgetown Visitation Convent and Prep School",
                             "Mt Vernon Seminary","National Arboretum","Observatory Hill","Rock Creek Park HD",
                             "T.R. Island Nat'l Mem.","Spingarn Campus HD","Armed Forces Retirement Home HD",
                             "C & O Canal NHP","Immaculata Seminary HD","National Zoological Park HD","Soldier's Home NHS",
                             "Washington Cathedral HD","Walter Reed Army Medical Center HD","St. Elizabeth's Hospital HD",
                             "National Mall HD")),]

# avoid multipart
hd2 = hd2 %>% 
  group_by(NAME) %>% 
  summarise(OBJECTID=first(OBJECTID),LABEL=first(LABEL),pr_wht = mean(pr_wht,na.rm=T),QUADRANT=first(QUADRANT)) %>% 
  st_cast() 

# add year established
data = read.csv('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/Website/hd_boundary_data.csv')

hd2 = left_join(hd2,data[,names(data) %in% c('OBJECTID',"YEAR")],by="OBJECTID")

ggplot() + 
  geom_sf(data= hd2,aes(fill = pr_wht, color = pr_wht)) 



ggplot(data= hd2,aes(x=YEAR,y = pr_wht)) + 
  geom_point() +geom_text(aes(label=LABEL),hjust=0, vjust=0)


ggplot() + 
  geom_point(data= hd2,aes(x=YEAR,y = pr_wht)) +
  geom_smooth(data= hd2,aes(x=YEAR,y = pr_wht),method='lm')  

summary(lm(pr_wht~I(YEAR-1950),data=hd2))


ggplot() + 
  geom_point(data= hd2,aes(x=2018-YEAR,y = pr_wht)) +
  geom_smooth(data= hd2,aes(x=2018-YEAR,y = pr_wht),method='lm')+ylab('% White Population')+xlab('Years since designation')


ggplot() + 
  geom_point(data= hd2,aes(x=2018-YEAR,y = pr_wht)) +
  geom_smooth(data= hd2,aes(x=2018-YEAR,y = pr_wht),method='lm')+
  coord_cartesian(ylim = c(0, 1))+
  facet_wrap(~QUADRANT)+
  ylab('% White Population')+xlab('Years since designation')




# Percentage white by zone
##########################################################################

wht_bg <- st_transform(get_acs(state = "DC",
                               geography = "block group",
                               year=2016,
                               variables =  c(wht_pop ="B02001_002",total_pop="B01001_001"),
                               geometry = TRUE) ,
                       crs=4269)     #blackpop = "B02001_003"

wht_bg = wht_bg %>%
  group_by(GEOID) %>%
  summarise(NAME=first(NAME),
            pr_wht = last(estimate)/first(estimate),
            geometry=first(geometry)) %>%
  st_cast()


hd = st_transform(read_sf('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/Website/Historic_Districts/Historic_Districts.shp'), crs=4269)
quad = st_transform(read_sf('C:\\Users\\mmann\\Google Drive\\HousingLife\\Historic Designation\\data\\DC_Quadrants\\DC_Quadrants.shp'), crs=4269)


#hd2 = aggregate(x = wht_bg[wht_bg,"pr_wht"], by = hd, FUN =  function(x) mean(x,na.rm=T))
#hd2 = st_join(hd2, hd)
hd2 = st_join(wht_bg, hd)
hd2

hd2 = st_join(hd2, quad[,c('QUADRANT','geometry')])


hd2 = hd2[!(hd2$LABEL %in% c('Gallaudet College HD',"Marine Barracks HD","Georgetown Visitation Convent and Prep School",
                             "Mt Vernon Seminary","National Arboretum","Observatory Hill","Rock Creek Park HD",
                             "T.R. Island Nat'l Mem.","Spingarn Campus HD","Armed Forces Retirement Home HD",
                             "C & O Canal NHP","Immaculata Seminary HD","National Zoological Park HD","Soldier's Home NHS",
                             "Washington Cathedral HD","Walter Reed Army Medical Center HD","St. Elizabeth's Hospital HD",
                             "National Mall HD")),]

# avoid multipart
hd2 = hd2 %>%
  group_by(NAME.x) %>%
  summarise(OBJECTID=first(OBJECTID),LABEL=first(LABEL),QUADRANT=first(QUADRANT),pr_wht = mean(pr_wht,na.rm=T),pr_mnrt = 1-mean(pr_wht,na.rm=T)) %>%
  st_cast()

# add year established
data = read.csv('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/Website/hd_boundary_data.csv',stringsAsFactors = F)

hd2 = left_join(hd2,data[!is.na(data$OBJECTID),names(data) %in% c('OBJECTID',"YEAR",'STATUS')],by="OBJECTID")
hd2$STATUS[is.na(hd2$STATUS)] = "Normal"
hd2$YEAR[is.na(hd2$YEAR)] = 0
hd2$pr_wht = 100 * hd2$pr_wht

ggplot() +
  geom_sf(data= hd2,aes(fill = pr_wht, color = pr_wht))

ggplot(data= hd2[hd2$STATUS=="Designated",]) +
  geom_point(aes(x=2018-YEAR,y = pr_wht)) +
  geom_smooth(aes(x=2018-YEAR,y = pr_wht),method='lm')+ylab('% White Population')+xlab('Years since designation')

ggplot(data= hd2[hd2$STATUS=="Designated",]) +
  geom_point(aes(x=2018-YEAR,y = pr_wht)) +
  geom_smooth(aes(x=2018-YEAR,y = pr_wht),method='lm')+
  facet_wrap(~QUADRANT)+
  ylab('% White Population')+
  xlab('Years since designation')

ggplot(data= hd2[hd2$STATUS=="Designated",]) +
  geom_point(aes(x=2018-YEAR,y = pr_mnrt)) +
  geom_smooth(aes(x=2018-YEAR,y = pr_mnrt),method='lm')+
  facet_wrap(~QUADRANT)+
  ylab('% Minority Population')+
  xlab('Years since designation')

summary(lm(pr_wht~I(YEAR-1950),data=hd2))


ggplot(data= hd2[hd2$STATUS=="Designated",],aes(x=YEAR,y = pr_wht)) +
  geom_point() +geom_text(aes(label=LABEL),hjust=0, vjust=0)






B19101_001
Estimate!!Total
FAMILY INCOME IN THE PAST 12 MONTHS (IN 2015 INFLATION-ADJUSTED DOLLARS)
2041
B19101_002
Estimate!!Total!!Less than $10,000
FAMILY INCOME IN THE PAST 12 MONTHS (IN 2015 INFLATION-ADJUSTED DOLLARS)
2042
B19101_003
Estimate!!Total!!$10,000 to $14,999
FAMILY INCOME IN THE PAST 12 MONTHS (IN 2015 INFLATION-ADJUSTED DOLLARS)
2043
B19101_004
Estimate!!Total!!$15,000 to $19,999
FAMILY INCOME IN THE PAST 12 MONTHS (IN 2015 INFLATION-ADJUSTED DOLLARS)
2044
B19101_005
Estimate!!Total!!$20,000 to $24,999
FAMILY INCOME IN THE PAST 12 MONTHS (IN 2015 INFLATION-ADJUSTED DOLLARS)
2045


B19101_014
Estimate!!Total!!$100,000 to $124,999
FAMILY INCOME IN THE PAST 12 MONTHS (IN 2015 INFLATION-ADJUSTED DOLLARS)
2054
B19101_015
Estimate!!Total!!$125,000 to $149,999
FAMILY INCOME IN THE PAST 12 MONTHS (IN 2015 INFLATION-ADJUSTED DOLLARS)
2055
B19101_016
Estimate!!Total!!$150,000 to $199,999
FAMILY INCOME IN THE PAST 12 MONTHS (IN 2015 INFLATION-ADJUSTED DOLLARS)
2056
B19101_017
Estimate!!Total!!$200,000 or more
v15 <- load_variables(2015, "acs5", cache = TRUE)
View(v15)




# tract inc_upper quintile 
##########################################################################

wht_bg <- get_acs(state = "DC", geography = "tract", year=2016,
                  variables =  c(wht_pop ="B02001_002",total_pop="B01001_001",total_pop="B01001_001",inc_upperQ = "B19080_004"), geometry = TRUE)    #blackpop = "B02001_003"

wht_bg = wht_bg %>% 
  group_by(GEOID) %>% 
  summarise(NAME=first(NAME),pr_wht = last(estimate)/first(estimate), inc_upperQ=mean(inc_upperQ),geometry=first(geometry)) %>% 
  st_cast() 


quad = st_transform(read_sf('C:\\Users\\mmann\\Google Drive\\HousingLife\\Historic Designation\\data\\DC_Quadrants\\DC_Quadrants.shp'), crs=4269)  
hd = read_sf('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/Website/Historic_Districts/Historic_Districts.shp')
hd = st_transform(hd, crs=4269)  
wht_bg = st_transform(wht_bg, crs=4269)  



hd2 <- aggregate(wht_bg[wht_bg$variable=="inc_upperQ",], hd, FUN =  function(x) mean(x,na.rm=T)) # summarize by hd
hd2 = st_join(hd2, hd)
hd2 = st_join(hd2, quad[,c('QUADRANT','geometry')])

hd2



hd2 = hd2[!(hd2$LABEL %in% c('Gallaudet College HD',"Marine Barracks HD","Georgetown Visitation Convent and Prep School",
                             "Mt Vernon Seminary","National Arboretum","Observatory Hill","Rock Creek Park HD",
                             "T.R. Island Nat'l Mem.","Spingarn Campus HD","Armed Forces Retirement Home HD",
                             "C & O Canal NHP","Immaculata Seminary HD","National Zoological Park HD","Soldier's Home NHS",
                             "Washington Cathedral HD","Walter Reed Army Medical Center HD","St. Elizabeth's Hospital HD",
                             "National Mall HD",'Dumbarton Oaks Park')),]

# avoid multipart
hd2 = hd2 %>% 
  group_by(NAME.y) %>% 
  summarise(OBJECTID=first(OBJECTID),LABEL=first(LABEL),inc_upperQ = mean(estimate,na.rm=T),QUADRANT=first(QUADRANT)) %>% 
  st_cast() 

# add year established
data = read.csv('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/Website/hd_boundary_data.csv')

hd2 = left_join(hd2,data[,names(data) %in% c('OBJECTID',"YEAR")],by="OBJECTID")

ggplot() + 
  geom_sf(data= hd2,aes(fill = inc_upperQ, color = inc_upperQ)) 



ggplot(data= hd2,aes(x=YEAR,y = pr_wht)) + 
  geom_point() +geom_text(aes(label=LABEL),hjust=0, vjust=0)


ggplot() + 
  geom_point(data= hd2,aes(x=YEAR,y = inc_upperQ)) +
  geom_smooth(data= hd2,aes(x=YEAR,y = inc_upperQ),method='lm')  

summary(lm(inc_upperQ~I(YEAR-1950)+factor(QUADRANT),data=hd2))


ggplot() + 
  geom_point(data= hd2,aes(x=2018-YEAR,y = inc_upperQ)) +
  geom_smooth(data= hd2,aes(x=2018-YEAR,y = inc_upperQ),method='lm')+ylab('Income of upper Quantile')+xlab('Years since designation')


ggplot() + 
  geom_point(data= hd2,aes(x=2018-YEAR,y = inc_upperQ)) +
  geom_smooth(data= hd2,aes(x=2018-YEAR,y = inc_upperQ),method='lm')+
  facet_wrap(~QUADRANT)+
  ylab('Income of upper Quantile')+xlab('Years since designation')






# BG % 1br rent greater than 1500   
##########################################################################

wht_bg <- get_acs(state = "DC", geography = "block group", year=2016,
                  variables =  c(total_1br="B25068_011",total_1br_1500 = "B25068_018"), geometry = TRUE)    #blackpop = "B02001_003"

wht_bg = wht_bg %>% 
  group_by(GEOID) %>% 
  summarise(NAME=first(NAME), per_1b_1500=last(estimate)/first(estimate),geometry=first(geometry)) %>% 
  st_cast() 


quad = st_transform(read_sf('C:\\Users\\mmann\\Google Drive\\HousingLife\\Historic Designation\\data\\DC_Quadrants\\DC_Quadrants.shp'), crs=4269)  
hd = read_sf('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/Website/Historic_Districts/Historic_Districts.shp')
hd = st_transform(hd, crs=4269)  
wht_bg = st_transform(wht_bg, crs=4269)  



hd2 <- aggregate(wht_bg[,"per_1b_1500"], hd, FUN =  function(x) mean(x,na.rm=T)) # summarize by hd
hd2 = st_join(hd2, hd)
hd2 = st_join(hd2, quad[,c('QUADRANT','geometry')])

hd2



hd2 = hd2[!(hd2$LABEL %in% c('Gallaudet College HD',"Marine Barracks HD","Georgetown Visitation Convent and Prep School",
                             "Mt Vernon Seminary","National Arboretum","Observatory Hill","Rock Creek Park HD",
                             "T.R. Island Nat'l Mem.","Spingarn Campus HD","Armed Forces Retirement Home HD",
                             "C & O Canal NHP","Immaculata Seminary HD","National Zoological Park HD","Soldier's Home NHS",
                             "Washington Cathedral HD","Walter Reed Army Medical Center HD","St. Elizabeth's Hospital HD",
                             "National Mall HD",'Dumbarton Oaks Park')),]

# avoid multipart
hd2 = hd2 %>% 
  group_by(NAME) %>% 
  summarise(OBJECTID=first(OBJECTID),LABEL=first(LABEL),per_1b_1500 = mean(per_1b_1500,na.rm=T),QUADRANT=first(QUADRANT)) %>% 
  st_cast() 

# add year established
data = read.csv('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/Website/hd_boundary_data.csv')

hd2 = left_join(hd2,data[,names(data) %in% c('OBJECTID',"YEAR")],by="OBJECTID")


ggplot() +  geom_sf(data= hd2,aes(fill = per_1b_1500, color = per_1b_1500)) 


ggplot(data= hd2,aes(x=YEAR,y = per_1b_1500)) + 
  geom_point() +geom_text(aes(label=LABEL),hjust=0, vjust=0)


ggplot() + 
  geom_point(data= hd2,aes(x=YEAR,y = per_1b_1500)) +
  geom_smooth(data= hd2,aes(x=YEAR,y = per_1b_1500),method='lm')  

summary(lm(per_1b_1500~I(YEAR-1950)+factor(QUADRANT),data=hd2))


ggplot() + 
  geom_point(data= hd2,aes(x=2018-YEAR,y = per_1b_1500)) +
  geom_smooth(data= hd2,aes(x=2018-YEAR,y = per_1b_1500),method='lm')+ylab('Percent of 1br rents over $1500')+xlab('Years since designation')


ggplot() + 
  geom_point(data= hd2,aes(x=2018-YEAR,y = per_1b_1500)) +
  geom_smooth(data= hd2,aes(x=2018-YEAR,y = per_1b_1500),method='lm')+
  facet_wrap(~QUADRANT,scales = 'free')+
  ylab('Income of upper Quantile')+xlab('Years since designation')





#  TOTAL # HOUSING UNITS     B25001_001
##########################################################################
 





# BG % 1br rent greater than 1500   
##########################################################################

wht_bg <- get_acs(state = "DC", geography = "block group", year=2016,
                  variables =  c(total_1br="B25068_011",total_1br_1500 = "B25068_018"), geometry = TRUE)    #blackpop = "B02001_003"

wht_bg = wht_bg %>% 
  group_by(GEOID) %>% 
  summarise(NAME=first(NAME), per_1b_1500=last(estimate)/first(estimate),geometry=first(geometry)) %>% 
  st_cast() 


quad = st_transform(read_sf('C:\\Users\\mmann\\Google Drive\\HousingLife\\Historic Designation\\data\\DC_Quadrants\\DC_Quadrants.shp'), crs=4269)  
hd = read_sf('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/Website/Historic_Districts/Historic_Districts.shp')
hd = st_transform(hd, crs=4269)  
wht_bg = st_transform(wht_bg, crs=4269)  

hd2 = st_join(wht_bg, hd)
hd2 = st_join(hd2, quad[,c('QUADRANT','geometry')])

hd2



hd2 = hd2[!(hd2$LABEL %in% c('Gallaudet College HD',"Marine Barracks HD","Georgetown Visitation Convent and Prep School",
                             "Mt Vernon Seminary","National Arboretum","Observatory Hill","Rock Creek Park HD",
                             "T.R. Island Nat'l Mem.","Spingarn Campus HD","Armed Forces Retirement Home HD",
                             "C & O Canal NHP","Immaculata Seminary HD","National Zoological Park HD","Soldier's Home NHS",
                             "Washington Cathedral HD","Walter Reed Army Medical Center HD","St. Elizabeth's Hospital HD",
                             "National Mall HD",'Dumbarton Oaks Park')),]

# Cleanup
hd2$Status = hd2$STATUS
hd2$Status[is.na(hd2$STATUS)] = 'Not HD'
hd2$Status[hd2$STATUS=='Designated']= 'Historic \nDesignated'

hd2$quad <- factor(hd2$QUADRANT, levels = c('NW','NE','SW','SE'))


ggplot(data = hd2,aes(Status, per_1b_1500,fill=Status))+geom_boxplot( )+
  #stat_summary(geom="text", fun.y=median,  
  #           aes(label= 'Middle'),color='white',
  #           position=position_nudge(x=0,y=.05), size=3.5) + 
  facet_wrap(~QUADRANT,scales = 'fixed') +
  ylab('Percent of 1 Bedroom Apartments \n over $1500/month') +xlab('') + 
  scale_fill_manual(values = c("#ef6269", "#999999")) + 
  theme(axis.text=element_text(size=13),axis.title.y=element_text(size=15),axis.title=element_text(size=13),
        legend.position="right",legend.direction = 'vertical')+ 
  scale_y_continuous(labels = scales::percent)
  

ggplot(data = hd2,aes(Status, per_1b_1500,fill=Status))+geom_boxplot( )+
  #stat_summary(geom="text", fun.y=median,  
  #           aes(label= 'Middle'),color='white',
  #           position=position_nudge(x=0,y=.05), size=3.5) + 
  facet_wrap(~QUADRANT,scales = 'fixed') +
  ylab('Percent of 1 Bedroom Apartments \n over $1500/month') +xlab('') + 
  scale_fill_manual(values = c("#ef6269", "#999999")) + 
  theme(axis.text=element_text(size=13),axis.title.y=element_text(size=15),axis.title=element_text(size=13),
        legend.position="right",legend.direction = 'vertical')+ 
  scale_y_continuous(labels = scales::percent)



ggplot(data = hd2,aes(Status, per_1b_1500,fill=Status))+geom_boxplot( )+
  facet_wrap(~QUADRANT,scales = 'fixed') +
  ylab('% of 1 Bedroom Apartments over \n$1500/month') +xlab('') + 
  scale_fill_manual(values = c("#ef6269", "#999999")) + 
  theme(axis.text=element_text(size=13),axis.title.y=element_text(size=15),axis.title=element_text(size=13),
        legend.position="right",legend.direction = 'vertical')+ 
  scale_y_continuous(labels = scales::percent) + 
  annotate("text", x = 1, y = 0.26, label = "Exclusionary Zoning", fontface =2,color='gray40',)
  


ggplot(data = hd2,aes(Status, per_1b_1500,fill=Status))+geom_bar(  stat = "summary", fun.y = "median") +
  facet_wrap(~quad,scales = 'fixed') + 
  annotate("segment", x = 1.55, xend = 2.45, y = 0.01, yend = 0.01, colour = "grey60",size=2)+
  ylab('Percent of 1 Bedroom Apartments \n over $1500/month\n') +xlab('') + 
  scale_fill_manual(values = c("#ef6269", "#999999")) + 
  theme(axis.text=element_text(size=13),axis.title.y=element_text(size=15),axis.title=element_text(size=13),
        legend.position="right",legend.direction = 'vertical')+ 
  scale_y_continuous(labels = scales::percent)



# BG % 2br rent greater than 1500   
##########################################################################

wht_bg <- get_acs(state = "DC", geography = "block group", year=2016,
                  variables =  c(total_1br="B25068_020",total_1br_1500 = "B25068_027"), geometry = TRUE)    #blackpop = "B02001_003"

wht_bg = wht_bg %>% 
  group_by(GEOID) %>% 
  summarise(NAME=first(NAME), per_1b_1500=last(estimate)/first(estimate),geometry=first(geometry)) %>% 
  st_cast() 


quad = st_transform(read_sf('C:\\Users\\mmann\\Google Drive\\HousingLife\\Historic Designation\\data\\DC_Quadrants\\DC_Quadrants.shp'), crs=4269)  
hd = read_sf('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/Website/Historic_Districts/Historic_Districts.shp')
hd = st_transform(hd, crs=4269)  
wht_bg = st_transform(wht_bg, crs=4269)  

hd2 = st_join(wht_bg, hd)
hd2 = st_join(hd2, quad[,c('QUADRANT','geometry')])

hd2



hd2 = hd2[!(hd2$LABEL %in% c('Gallaudet College HD',"Marine Barracks HD","Georgetown Visitation Convent and Prep School",
                             "Mt Vernon Seminary","National Arboretum","Observatory Hill","Rock Creek Park HD",
                             "T.R. Island Nat'l Mem.","Spingarn Campus HD","Armed Forces Retirement Home HD",
                             "C & O Canal NHP","Immaculata Seminary HD","National Zoological Park HD","Soldier's Home NHS",
                             "Washington Cathedral HD","Walter Reed Army Medical Center HD","St. Elizabeth's Hospital HD",
                             "National Mall HD",'Dumbarton Oaks Park')),]

# Cleanup
hd2$STATUS[is.na(hd2$STATUS)] = 'Not'
hd2$Status = hd2$STATUS
hd2$Status[hd2$STATUS=='Designated']= 'Historic \nDesignated'

hd2$quad <- factor(hd2$QUADRANT, levels = c('NW','NE','SW','SE'))


ggplot()+geom_boxplot(data = hd2, aes(Status, per_1b_1500,fill=Status)) + facet_wrap(~quad,scales = 'fixed') +
  ylab('% of 2 Bedroom Apartments over \n$1500/month') +xlab('') + 
  scale_fill_manual(values = c("#e85420", "#999999")) + 
  theme(axis.text=element_text(size=13),axis.title.y=element_text(size=15),axis.title=element_text(size=13),
        legend.position="right",legend.direction = 'vertical')+ 
  scale_y_continuous(labels = scales::percent)






# % elderly 
##########################################################################

wht_bg <- get_acs(state = "DC", geography = "block group", year=2016,survey = 'acs5',
                  variables =  c(all_total_pop="B01001_001",M75 = "B01001_023",M80='B01001_024',M85='B01001_025',
                                 F75 = 'B01001_047',F80='B01001_048',F85='B01001_049'), geometry = T,cache_table = T)    #blackpop = "B02001_003"



wht_bg = wht_bg %>% 
  group_by(GEOID) %>% 
  summarise(NAME=first(NAME),all_total_pop=sum(estimate[variable=="all_total_pop"]), 
            elder_pop= sum(estimate[variable=="M75"],estimate[variable=="M80"],estimate[variable=="M85"],
                           estimate[variable=="F75"],estimate[variable=="F80"],estimate[variable=="F85"]),
            geometry=first(geometry)) %>% 
  st_cast() 


quad = st_transform(read_sf('C:\\Users\\mmann\\Google Drive\\HousingLife\\Historic Designation\\data\\DC_Quadrants\\DC_Quadrants.shp'), crs=4269)  
hd = read_sf('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/Website/Historic_Districts/Historic_Districts.shp')
hd = st_transform(hd, crs=4269)  
wht_bg = st_transform(wht_bg, crs=4269)  

hd2 = st_join(wht_bg, hd)
hd2 = st_join(hd2, quad[,c('QUADRANT','geometry')])

hd2



hd2 = hd2[!(hd2$LABEL %in% c('Gallaudet College HD',"Marine Barracks HD","Georgetown Visitation Convent and Prep School",
                             "Mt Vernon Seminary","National Arboretum","Observatory Hill","Rock Creek Park HD",
                             "T.R. Island Nat'l Mem.","Spingarn Campus HD","Armed Forces Retirement Home HD",
                             "C & O Canal NHP","Immaculata Seminary HD","National Zoological Park HD","Soldier's Home NHS",
                             "Washington Cathedral HD","Walter Reed Army Medical Center HD","St. Elizabeth's Hospital HD",
                             "National Mall HD",'Dumbarton Oaks Park')),]

# Cleanup
hd2$Status = hd2$STATUS
hd2$Status[is.na(hd2$STATUS)] = 'Not HD'
hd2$Status[hd2$STATUS=='Designated']= 'Historic \nDesignated'

hd2$quad <- factor(hd2$QUADRANT, levels = c('NW','NE','SW','SE'))


ggplot()+geom_boxplot(data = hd2, aes(Status, elder_pop/all_total_pop,fill=Status),outlier.shape = NA ) + facet_wrap(~quad,scales = 'fixed') +
  ylab('Percent Elderly Population (70+)\n') +xlab('') + 
  scale_fill_manual(values = c("#ef6269", "#999999")) + 
  theme(axis.text=element_text(size=13),axis.title.y=element_text(size=15),axis.title=element_text(size=13),
        legend.position="right",legend.direction = 'vertical')+ 
  scale_y_continuous(labels = scales::percent,limits = c(0,0.15))




ggplot(data = hd2,aes(Status, elder_pop/all_total_pop,fill=Status))+geom_bar(position = "dodge", stat = "summary", fun.y = "median") +
  facet_wrap(~quad,scales = 'fixed') +
  ylab('Percent Elderly Population (70+)\n') +xlab('') + 
  scale_fill_manual(values = c("#ef6269", "#999999")) + 
  theme(axis.text=element_text(size=13),axis.title.y=element_text(size=15),axis.title=element_text(size=13),
        legend.position="right",legend.direction = 'vertical')+ 
  scale_y_continuous(labels = scales::percent)



# % non-white 
##########################################################################

wht_bg <- get_acs(state = "DC", geography = "block group", year=2016,survey = 'acs5',
                  variables =  c(all_total_pop="B01001_001",wht_pop ="B02001_002"), geometry = T,cache_table = T)    #blackpop = "B02001_003"



wht_bg = wht_bg %>% 
  group_by(GEOID) %>% 
  summarise(NAME=first(NAME),all_total_pop=sum(estimate[variable=="all_total_pop"]), 
            white_pop= sum(estimate[variable=="wht_pop"]),
            geometry=first(geometry)) %>%   st_cast() 

wht_bg$non_white_pop = wht_bg$all_total_pop - wht_bg$white_pop

quad = st_transform(read_sf('C:\\Users\\mmann\\Google Drive\\HousingLife\\Historic Designation\\data\\DC_Quadrants\\DC_Quadrants.shp'), crs=4269)  
hd = read_sf('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/Website/Historic_Districts/Historic_Districts.shp')
hd = st_transform(hd, crs=4269)  
wht_bg = st_transform(wht_bg, crs=4269)  

hd2 = st_join(wht_bg, hd)
hd2 = st_join(hd2, quad[,c('QUADRANT','geometry')])

hd2



hd2 = hd2[!(hd2$LABEL %in% c('Gallaudet College HD',"Marine Barracks HD","Georgetown Visitation Convent and Prep School",
                             "Mt Vernon Seminary","National Arboretum","Observatory Hill","Rock Creek Park HD",
                             "T.R. Island Nat'l Mem.","Spingarn Campus HD","Armed Forces Retirement Home HD",
                             "C & O Canal NHP","Immaculata Seminary HD","National Zoological Park HD","Soldier's Home NHS",
                             "Washington Cathedral HD","Walter Reed Army Medical Center HD","St. Elizabeth's Hospital HD",
                             "National Mall HD",'Dumbarton Oaks Park')),]

# Cleanup
hd2$Status = hd2$STATUS
hd2$Status[is.na(hd2$STATUS)] = 'Not HD'
hd2$Status[hd2$STATUS=='Designated']= 'Historic \nDesignated'

hd2$quad <- factor(hd2$QUADRANT, levels = c('NW','NE','SW','SE'))


ggplot()+geom_boxplot(data = hd2, aes(Status, non_white_pop/all_total_pop,fill=Status),outlier.shape = NA ) + 
  facet_wrap(~quad,scales = 'free') +
  ylab('Percent Non-White Population \n') +xlab('') + 
  scale_fill_manual(values = c("#ef6269", "#999999")) + 
  theme(axis.text=element_text(size=13),axis.title.y=element_text(size=15),axis.title=element_text(size=13),
        legend.position="right",legend.direction = 'vertical')+ 
  scale_y_continuous(labels = scales::percent,limits = c(0,1))




ggplot(data = hd2,aes(Status, non_white_pop/all_total_pop,fill=Status))+geom_bar(position = "dodge", stat = "summary", fun.y = "median") +
  facet_wrap(~quad,scales = 'fixed') +
  ylab('Percent Non-White Population \n') +xlab('') + 
  scale_fill_manual(values = c("#ef6269", "#999999")) + 
  theme(axis.text=element_text(size=13),axis.title.y=element_text(size=15),axis.title=element_text(size=13),
        legend.position="right",legend.direction = 'vertical')+ 
  scale_y_continuous(labels = scales::percent)




 

# % middle income 35-100K
##########################################################################
#https://www.washingtonpost.com/graphics/2017/business/your-income-comparison/?noredirect=on&utm_term=.35569946cc78

wht_bg <- get_acs(state = "DC", geography = "block group", year=2016,survey = 'acs5',
                  variables =  c(all_total_all_income="B19001_001",I35 = "B19001_008",I40='B19001_009',I45='B19001_010',
                                 I50 = 'B19001_011',I60='B19001_012',I75='B19001_013',I100='B19001_014',I125='B19001_015'), geometry = T,cache_table = T)    #blackpop = "B02001_003"



wht_bg = wht_bg %>% 
  group_by(GEOID) %>% 
  summarise(NAME=first(NAME),all_total_all_income=sum(estimate[variable=="all_total_all_income"]), 
            mid_inc_pop= sum(estimate[variable=="I35"],estimate[variable=="I40"],estimate[variable=="I45"],
                             estimate[variable=="I50"],estimate[variable=="I60"],estimate[variable=="I75"] ),
            geometry=first(geometry)) %>% 
  st_cast() 


quad = st_transform(read_sf('C:\\Users\\mmann\\Google Drive\\HousingLife\\Historic Designation\\data\\DC_Quadrants\\DC_Quadrants.shp'), crs=4269)  
hd = read_sf('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/Website/Historic_Districts/Historic_Districts.shp')
hd = st_transform(hd, crs=4269)  
wht_bg = st_transform(wht_bg, crs=4269)  

hd2 = st_join(wht_bg, hd)
hd2 = st_join(hd2, quad[,c('QUADRANT','geometry')])

hd2



hd2 = hd2[!(hd2$LABEL %in% c('Gallaudet College HD',"Marine Barracks HD","Georgetown Visitation Convent and Prep School",
                             "Mt Vernon Seminary","National Arboretum","Observatory Hill","Rock Creek Park HD",
                             "T.R. Island Nat'l Mem.","Spingarn Campus HD","Armed Forces Retirement Home HD",
                             "C & O Canal NHP","Immaculata Seminary HD","National Zoological Park HD","Soldier's Home NHS",
                             "Washington Cathedral HD","Walter Reed Army Medical Center HD","St. Elizabeth's Hospital HD",
                             "National Mall HD",'Dumbarton Oaks Park')),]

# Cleanup
hd2$Status = hd2$STATUS
hd2$Status[is.na(hd2$STATUS)] = 'Not HD'
hd2$Status[hd2$STATUS=='Designated']= 'Historic \nDesignated'

hd2$quad <- factor(hd2$QUADRANT, levels = c('NW','NE','SW','SE'))


ggplot()+geom_boxplot(data = hd2, aes(Status, mid_inc_pop/all_total_all_income,fill=Status) ) + facet_wrap(~quad,scales = 'fixed') +
  ylab('% HH earning  $35-$100,000 \n(lower middle income)\n') +xlab('') + 
  scale_fill_manual(values = c("#e85420", "#999999")) + 
  theme(axis.text=element_text(size=13),axis.title.y=element_text(size=15),axis.title=element_text(size=13),
        legend.position="right",legend.direction = 'vertical')+ 
  scale_y_continuous(labels = scales::percent )




# % middle income <100K
##########################################################################
#https://www.washingtonpost.com/graphics/2017/business/your-income-comparison/?noredirect=on&utm_term=.35569946cc78

wht_bg <- get_acs(state = "DC", geography = "block group", year=2016,survey = 'acs5',
                  variables =  c(all_total_all_income="B19001_001",I0='B19001_002',I10='B19001_003',I15='B19001_004',
                                 I20='B19001_005',I25='B19001_006', I30='B19001_007',
                                 I35 = "B19001_008",I40='B19001_009',I45='B19001_010',
                                 I50 = 'B19001_011',I60='B19001_012',I75='B19001_013',I100='B19001_014',I125='B19001_015'), geometry = T,cache_table = T)    #blackpop = "B02001_003"



wht_bg = wht_bg %>% 
  group_by(GEOID) %>% 
  summarise(NAME=first(NAME),all_total_all_income=sum(estimate[variable=="all_total_all_income"]), 
            mid_inc_pop= sum(estimate[variable=="I0"],estimate[variable=="I10"],estimate[variable=="I15"],
                             estimate[variable=="I20"],estimate[variable=="I25"],estimate[variable=="I30"],
                             estimate[variable=="I35"],estimate[variable=="I40"],estimate[variable=="I45"],
                             estimate[variable=="I50"],estimate[variable=="I60"],estimate[variable=="I75"] ),
            geometry=first(geometry)) %>% 
  st_cast() 


quad = st_transform(read_sf('C:\\Users\\mmann\\Google Drive\\HousingLife\\Historic Designation\\data\\DC_Quadrants\\DC_Quadrants.shp'), crs=4269)  
hd = read_sf('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/Website/Historic_Districts/Historic_Districts.shp')
hd = st_transform(hd, crs=4269)  
wht_bg = st_transform(wht_bg, crs=4269)  

hd2 = st_join(wht_bg, hd)
hd2 = st_join(hd2, quad[,c('QUADRANT','geometry')])

hd2



hd2 = hd2[!(hd2$LABEL %in% c('Gallaudet College HD',"Marine Barracks HD","Georgetown Visitation Convent and Prep School",
                             "Mt Vernon Seminary","National Arboretum","Observatory Hill","Rock Creek Park HD",
                             "T.R. Island Nat'l Mem.","Spingarn Campus HD","Armed Forces Retirement Home HD",
                             "C & O Canal NHP","Immaculata Seminary HD","National Zoological Park HD","Soldier's Home NHS",
                             "Washington Cathedral HD","Walter Reed Army Medical Center HD","St. Elizabeth's Hospital HD",
                             "National Mall HD",'Dumbarton Oaks Park')),]

# Cleanup
hd2$Status = hd2$STATUS
hd2$Status[is.na(hd2$STATUS)] = 'Not HD'
hd2$Status[hd2$STATUS=='Designated']= 'Historic \nDesignated'

hd2$quad <- factor(hd2$QUADRANT, levels = c('NW','NE','SW','SE'))


ggplot()+geom_boxplot(data = hd2, aes(Status, mid_inc_pop/all_total_all_income,fill=Status) ) + facet_wrap(~quad,scales = 'fixed') +
  ylab('Percent Households earning \n< $100,000') +xlab('') + 
  scale_fill_manual(values = c("#ef6269", "#999999")) + 
  theme(axis.text=element_text(size=13),axis.title.y=element_text(size=15),axis.title=element_text(size=13),
        legend.position="right",legend.direction = 'vertical')+ 
  scale_y_continuous(labels = scales::percent )




ggplot(data = hd2,aes(Status, mid_inc_pop/all_total_all_income,fill=Status))+geom_bar(position = "dodge", stat = "summary", fun.y = "median") +
  facet_wrap(~quad,scales = 'fixed') +
  ylab('Percent Households earning \n< $100,000\n') +xlab('') + 
  scale_fill_manual(values = c("#ef6269", "#999999")) + 
  theme(axis.text=element_text(size=13),axis.title.y=element_text(size=15),axis.title=element_text(size=13),
        legend.position="right",legend.direction = 'vertical')+ 
  scale_y_continuous(labels = scales::percent)





# % middle income  > 100K
##########################################################################
#https://www.washingtonpost.com/graphics/2017/business/your-income-comparison/?noredirect=on&utm_term=.35569946cc78


wht_bg <- get_acs(state = "DC", geography = "block group", year=2016,survey = 'acs5',
                  variables =  c(all_total_all_income="B19001_001",I0='B19001_002',I10='B19001_003',I15='B19001_004',
                                 I20='B19001_005',I25='B19001_006', I30='B19001_007',
                                 I35 = "B19001_008",I40='B19001_009',I45='B19001_010',
                                 I50 = 'B19001_011',I60='B19001_012',I75='B19001_013',I100='B19001_014',I125='B19001_015'), geometry = T,cache_table = T)    #blackpop = "B02001_003"



wht_bg = wht_bg %>% 
  group_by(GEOID) %>% 
  summarise(NAME=first(NAME),all_total_all_income=sum(estimate[variable=="all_total_all_income"]), 
            mid_inc_pop= sum(estimate[variable=="I0"],estimate[variable=="I10"],estimate[variable=="I15"],
                             estimate[variable=="I20"],estimate[variable=="I25"],estimate[variable=="I30"],
                             estimate[variable=="I35"],estimate[variable=="I40"],estimate[variable=="I45"],
                             estimate[variable=="I50"],estimate[variable=="I60"],estimate[variable=="I75"] ),
            geometry=first(geometry)) %>% 
  st_cast() 


quad = st_transform(read_sf('C:\\Users\\mmann\\Google Drive\\HousingLife\\Historic Designation\\data\\DC_Quadrants\\DC_Quadrants.shp'), crs=4269)  
hd = read_sf('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/Website/Historic_Districts/Historic_Districts.shp')
hd = st_transform(hd, crs=4269)  
wht_bg = st_transform(wht_bg, crs=4269)  

hd2 = st_join(wht_bg, hd)
hd2 = st_join(hd2, quad[,c('QUADRANT','geometry')])

hd2



hd2 = hd2[!(hd2$LABEL %in% c('Gallaudet College HD',"Marine Barracks HD","Georgetown Visitation Convent and Prep School",
                             "Mt Vernon Seminary","National Arboretum","Observatory Hill","Rock Creek Park HD",
                             "T.R. Island Nat'l Mem.","Spingarn Campus HD","Armed Forces Retirement Home HD",
                             "C & O Canal NHP","Immaculata Seminary HD","National Zoological Park HD","Soldier's Home NHS",
                             "Washington Cathedral HD","Walter Reed Army Medical Center HD","St. Elizabeth's Hospital HD",
                             "National Mall HD",'Dumbarton Oaks Park')),]

# Cleanup
hd2$STATUS[is.na(hd2$STATUS)] = 'Not'
hd2$Status = hd2$STATUS
hd2$Status[hd2$STATUS=='Designated']= 'Historic \nDesignated'

hd2$quad <- factor(hd2$QUADRANT, levels = c('NW','NE','SW','SE'))


ggplot()+geom_boxplot(data = hd2, aes(Status, (all_total_all_income-mid_inc_pop)/all_total_all_income,fill=Status) ) + facet_wrap(~quad,scales = 'fixed') +
  ylab('% HH earning > $100,000') +xlab('') + 
  scale_fill_manual(values = c("#e85420", "#999999")) + 
  theme(axis.text=element_text(size=13),axis.title.y=element_text(size=15),axis.title=element_text(size=13),
        legend.position="right",legend.direction = 'vertical')+ 
  scale_y_continuous(labels = scales::percent )






#Historic and single family zoning affordable housing
##################################################

# water d7 pdr wr arts he cg ste

afford = read_sf('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/data/Affordable_Housing/Affordable_Housing_simple_clean.geojson')
water = read_sf('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/data/Waterbodies/Waterbodies.shp')

hd = read_sf('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/data/Historic_Districts/Historic_Districts.geojson')
hd$aream2 = st_area(hd)
hd$in_hd = 1
hd = hd[,'in_hd']
sf = read_sf('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/data/Zoning_Regulations_of_2016/R1_3.geojson')
sf$aream2 = st_area(sf)
sf$in_sf = 1
sf = sf[,'in_sf']

zoning = read_sf('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/data/Zoning_Regulations_of_2016/Zoning_Regulations_of_2016.geojson')


afford= st_join(afford, hd, join = st_intersects)
afford$in_hd[is.na(afford$in_hd)] = 0

afford= st_join(afford, sf, join = st_intersects)
afford$in_sf[is.na(afford$in_sf)] = 0
afford

afford$zone_type = 'Inclusionary \nZoning'
afford$zone_type[afford$in_hd == 1] = 'Historic \nDesignation'
afford$zone_type[afford$in_sf == 1] = 'Single \nFamily Zoning'
afford$zone_type[afford$in_sf == 1 & afford$in_hd == 1] = 'Both'
afford$zone_type = factor(afford$zone_type, levels = c('Inclusionary \nZoning','Historic \nDesignation','Single \nFamily Zoning'))


ggplot()+geom_bar(data=afford, aes(x=zone_type))+ggtitle('Number of Affordable Housing Projects Since 2015')+
  xlab('Zoning Type')+ylab('# Projects')+ theme(axis.text.x = element_text(size = 12),
                                                axis.title.y = element_text(size = 13),
                                                axis.title.x = element_text(size=13),
                                                plot.title = element_text(size=16))


total = afford %>%   group_by(zone_type) %>%  summarise(Zone_Total_Affo = sum(TOTAL_AFFO),Total_0_30AMI=sum(AFFORDABLE),Total_31_60AMI=sum(AFFORDAB_1,AFFORDAB_2),
                                                        Total_61_AMI=sum(AFFORDAB_3,AFFORDAB_4)  )

total


average = afford %>%   group_by(zone_type) %>%  summarise(mn_Zone_Total_Affo = mean(TOTAL_AFFO),mn_Total_0_30AMI=mean(AFFORDABLE),mn_Total_31_60AMI=mean(sum(AFFORDAB_1,AFFORDAB_2)),
                                                        mn_Total_61_AMI=mean(sum(AFFORDAB_3,AFFORDAB_4)) )


ggplot()+geom_col(data=total, aes(x=zone_type,y=Zone_Total_Affo))+ggtitle('Number of Affordable Units Since 2015')+
  xlab('Zoning Type')+ylab('# Units')+ theme(axis.text.x = element_text(  size = 12))+ theme(axis.text.x = element_text(size = 12),
                                                                                             axis.title.y = element_text(size = 13),
                                                                                             axis.title.x = element_text(size=13),
                                                                                             plot.title = element_text(size=16))

# average number of units 
ggplot()+geom_col(data=average, aes(x=zone_type,y=mn_Zone_Total_Affo))+ggtitle('Average # Affordable Units Since 2015')+
  xlab('Zoning Type')+ylab('# Units')+ theme(axis.text.x = element_text(  size = 12))+ theme(axis.text.x = element_text(size = 12),
                                                                                             axis.title.y = element_text(size = 13),
                                                                                             axis.title.x = element_text(size=13),
                                                                                             plot.title = element_text(size=16))

library(reshape2)

total_ami = melt(data = total, id.vars = "zone_type", measure.vars = c("Total_0_30AMI", "Total_31_60AMI",'Total_61_AMI'))
total_ami$variable  = factor(total_ami$variable,label=c('0 to 30% AMI','31 to 60% AMI','61+% AMI') )

ggplot()+geom_col(data=total_ami, aes(x=zone_type,y=value,fill =variable))+ggtitle('Number of Affordable Units by % of Average Median Income')+
  xlab('Zoning Type')+ylab('# Units')+
  theme_fivethirtyeight() + scale_fill_economist() +theme(axis.text.x = element_text( size = 12))+ 
  annotate("rect", xmin = 1.5, xmax = 3.5, ymin = 000, ymax = 10000, alpha = .15)+
  annotate("text", x = 2.5, y = 9000, label = "Exclusionary Zoning", fontface =2)+
  annotate("text", x = 2.5, y = 7000, label = "These zoning types effectively exclude affordable\n development by making them more expensive \nor prevent them outright")






#Historic and single family zoning affordable housing AREA CORRECTION
##################################################

afford = read_sf('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/data/Affordable_Housing/Affordable_Housing_simple_clean.geojson')
afford <- st_transform(afford, crs = 26918)

hd = read_sf('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/data/Historic_Districts/Historic_Districts.geojson')
hd = hd %>% st_union() %>% st_sf()# dissolve and convert back to feature collection
hd$aream2 = st_area(hd)
hd$in_hd = 1
hd <- st_transform(hd, crs = 26918)
sf = read_sf('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/data/Zoning_Regulations_of_2016/R1_3.geojson')
sf = sf %>% st_union() %>% st_sf()# dissolve and convert back to feature collection
sf$aream2 = st_area(sf)
sf$in_sf = 1
sf <- st_transform(sf, crs = 26918)

# zoning = read_sf('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/data/Zoning_Regulations_of_2016/Zoning_Regulations_of_2016.geojson')
# water = read_sf('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/data/Waterbodies/Waterbodies.shp')
# 
# # find area developable - remove commercial, and other non res types
# zoning = zoning[!(zoning$ZONING_LAB %in% c('ARTS-1','ARTS-2','ARTS-3','ARTS-4','D-7','D-8','HE-1','HE-2','HE-3','HE-4','StE-11',  
#                                            'StE-12',  'StE-13',  'StE-14',  'StE-15',  'StE-16',  'StE-17',  'StE-18',  'StE-19',   'StE-2',   'StE-3', 
#                                            'StE-4',   'StE-5',   'StE-6',   'StE-7',   'StE-8',   'StE-9','WR-1','WR-2','WR-3','WR-4',    
#                                            'WR-5','WR-6','WR-7','WR-8', 'CG-1','CG-2','CG-3','CG-4','CG-5','CG-6','CG-7', 'UNZONED' )),]
# 
# # remove water hd and single family 
# zoning = zoning %>% st_union(zoning) %>% st_sf() # dissolve to feature collection
# area_remaining = st_erase(zoning,water)
# area_remaining = st_erase(area_remaining,hd)
# area_remaining = st_erase(area_remaining,sf)

area_remaining = read_sf('C:/Users/mmann/Google Drive/HousingLife/Historic Designation/data/Zoning_Regulations_of_2016/remaining_not_sf_hd_water_commercial.geojson')
area_remaining = area_remaining %>% st_union() %>% st_sf()# dissolve and convert back to feature collection
area_remaining <- st_transform(area_remaining, crs = 26918)
plot(area_remaining)

# calc areas
area_remaining_aream2 = st_area(area_remaining)
hd_aream2 = st_area(hd)
sf_aream2 = st_area(sf)

# join affordable 
afford= st_join(afford, hd, join = st_intersects)
afford$in_hd[is.na(afford$in_hd)] = 0
afford= st_join(afford, sf, join = st_intersects)
afford$in_sf[is.na(afford$in_sf)] = 0
afford

afford$zone_type = 'Other Residential \nZoning'
afford$zone_type[afford$in_hd == 1] = 'Historic \nDesignation'
afford$zone_type[afford$in_sf == 1] = 'Single \nFamily Zoning'
afford$zone_type[afford$in_sf == 1 & afford$in_hd == 1] = 'Both'
afford$zone_type = factor(afford$zone_type, levels = c('Other Residential \nZoning','Historic \nDesignation','Single \nFamily Zoning'))




ggplot()+geom_bar(data=afford, aes(x=zone_type))+ggtitle('Number of Affordable Housing Projects Since 2015')+
  xlab('Zoning Type')+ylab('# Projects')+ theme(axis.text.x = element_text(size = 12),
                                                axis.title.y = element_text(size = 13),
                                                axis.title.x = element_text(size=13),
                                                plot.title = element_text(size=16))


total = afford %>%   group_by(zone_type) %>%  summarise(Zone_Total_Affo = sum(TOTAL_AFFO),Total_0_30AMI=sum(AFFORDABLE),Total_31_60AMI=sum(AFFORDAB_1,AFFORDAB_2),
                                                        Total_61_AMI=sum(AFFORDAB_3,AFFORDAB_4)  )

# add areas in m^2
total$area[total$zone_type == 'Other Residential \nZoning' ]=area_remaining_aream2
total$area[total$zone_type == 'Historic \nDesignation' ]=hd_aream2
total$area[total$zone_type == 'Single \nFamily Zoning' ]=sf_aream2

total

ggplot()+geom_col(data=total, aes(x=zone_type,y=Zone_Total_Affo))+ggtitle('Number of Affordable Units Since 2015')+
  xlab('Zoning Type')+ylab('# Units')+ theme(axis.text.x = element_text(  size = 12))+ theme(axis.text.x = element_text(size = 12),
                                                                                             axis.title.y = element_text(size = 13),
                                                                                             axis.title.x = element_text(size=13),
                                                                                             plot.title = element_text(size=16))


library(reshape2)

total_ami = melt(data = total, id.vars = c("zone_type",'area'), measure.vars = c("Total_0_30AMI", "Total_31_60AMI",'Total_61_AMI'))
total_ami$variable  = factor(total_ami$variable,label=c('0 to 30% AMI','31 to 60% AMI','61+% AMI') )

ggplot()+geom_col(data=total_ami, aes(x=zone_type,y=value,fill =variable))+ggtitle('Number of Affordable Units by % of Average Median Income')+
  xlab('Zoning Type')+ylab('# Units')+
  theme_fivethirtyeight() + scale_fill_economist(name='% of Median Income') +theme(axis.text.x = element_text( size = 12))+ 
  annotate("rect", xmin = 1.5, xmax = 3.5, ymin = 000, ymax = 10000, alpha = .15)+
  annotate("text", x = 2.5, y = 9000, label = "Exclusionary Zoning", fontface =2)+
  annotate("text", x = 2.5, y = 7000, label = "These zoning types effectively exclude affordable\n development by making them more expensive \nor prevent them outright")

library('latex2exp')
library('gridExtra')
ggplot()+geom_col(data=total_ami, aes(x=zone_type,y=value/(area/1000000),fill =variable))+
  ggtitle(TeX('Number of Affordable Units per $km^{2} $ by % of Average Median Income'))+
  xlab('Zoning Type')+ylab('# Units')+
  theme_fivethirtyeight() + scale_fill_economist(name='% of Median Income') +theme(axis.text.x = element_text( size = 12))
 

total_ami$variable  = factor(total_ami$variable,label=c('<$30,000','$31-60,000','$61,000+') )


a=ggplot()+geom_col(data=total_ami, aes(x=zone_type,y=value/(area/1000000),fill =variable))+
  #ggtitle(TeX('Number of Affordable Units per $km^{2} $ by Household Income'))+
  xlab('Zoning Type')+ylab('# Units')+
  annotate("text", x = 1, y = 250, label = "Very Affordable", fontface =2,color='white')+
  annotate("text", x = 1, y = 150, label = "Affordable", fontface =2,color='white')+
  annotate("text", x = 1, y = 25, label = "Semi Affordable", fontface =2,color='white')+
  theme_fivethirtyeight() + scale_fill_economist(name='Family Income') +theme(axis.text.x = element_text( size = 12))+
  annotate("rect", xmin = 1.5, xmax = 3.5, ymin = 000, ymax = 175, alpha = .15)+
  annotate("text", x = 2.5, y = 140, label = "Exclusionary Zoning", fontface =2,color='gray40')+
  annotate("text", x = 2.5, y = 90, color='gray40',label = "These zoning types effectively exclude affordable\n development by making them more expensive \nor prevent them outright")

  
 a +labs(
   title=TeX('Number of Affordable Units per $km^{2} $'),
    subtitle ='Built since 2015 by Household Income')+
   theme(plot.subtitle=element_text(size=9,  face="italic", color="black"),
         plot.title=element_text(size=22  ))
