library(sf)
library(tidyverse)
library(tmap)
library(tmaptools)
library(stringr)

'''
calcuate the average percent of science students (in all) grades per county meeting the 
required standards and produce a map to show where the Country averages are above or below the 
State of Washington average.
1.CountMetStandard
2.Count of Students Expected to Test
3.County
4.OrganizationLevel
5.GradeLevel
6.TestSubject
7.StudentGroupType

'''


mycsv <- read_csv("D:/MSc/Term1/GIS/practice/practice2/Report_Card_Assessment_Data_2018-19_School_Year.csv")
#ls(mycsv)


#select columns needed
csv1 <- mycsv %>%
  dplyr::select('County','OrganizationLevel','StudentGroupType','GradeLevel','TestSubject',
                'CountMetStandard','Count of Students Expected to Test')%>%
  print()


#filter by 
csv2 <- csv1 %>% 
  filter(OrganizationLevel == "District", 
         StudentGroupType == "All", 
         GradeLevel == "All Grades", 
         TestSubject == "Science")
  
#NULL 
csv2[csv2=="NULL"] <- '0'
csv2$CountMetStandard <- as.numeric(csv2$CountMetStandard)
csv2$`Count of Students Expected to Test` <- as.numeric(csv2$`Count of Students Expected to Test`)

csv2<-csv2 %>%
  dplyr::rename(TotalStudentTested="Count of Students Expected to Test")


#calculate by county
csv3<-csv2 %>%
  group_by(County) %>%
  summarise(CountMetStandard=sum(CountMetStandard), TotalStudentTested=sum(TotalStudentTested))%>%
  mutate(percent=CountMetStandard/TotalStudentTested)

Washing_ave=sum(csv3$CountMetStandard)/sum(csv3$TotalStudentTested)
#print()


#differ by 0/1
csv3 <- csv3 %>%
  mutate(ave_Compare=case_when(percent>=Washing_ave ~1, TRUE ~0))

#calculate differ
csv3<-csv3 %>%
  mutate(differ=percent-Washing_ave)

#read map
shape <- st_read("D:/MSc/Term1/GIS/practice/practice2/Washington_Counties_with_Natural_Shoreline___washsh_area/Washington_Counties_with_Natural_Shoreline___washsh_area.shp")

# shape%>%head(., n=10)

#merge
csv3$County<- str_to_upper(csv3$County)

shape <- shape%>%
  merge(.,
        csv3,
        by.x="COUNTY",
        by.y="County")

#draw map
tmap_mode("plot")
# change the fill to your column name if different
shape %>%
  qtm(.,fill = "differ")

tmapwashington <- shape %>%
  st_bbox(.) %>% 
  tmaptools::read_osm(., type = "osm", zoom = NULL)

tm_shape(tmapwashington)+
  tm_rgb()+
  tm_shape(shape) + 
  tm_polygons("differ", 
              style="pretty",
              palette="Blues",
              midpoint=NA,
              title="Differ of Average",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Difference in Average Percent", legend.position = c("right", "bottom"))

# hello world
         
