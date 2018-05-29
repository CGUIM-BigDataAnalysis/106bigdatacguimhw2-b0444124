library(readr)
library(dplyr)
library(knitr)
library(ggplot2)
library(choroplethr)
library(choroplethrMaps)
`103_ab103_C` <- read.csv("C:/Users/yijie/Downloads/103_ab103_C.csv")
`103_ab103_S` <- read.csv("C:/Users/yijie/Downloads/103_ab103_S.csv")
`104_ab104_C` <- read.csv("C:/Users/yijie/Downloads/104_ab104_C.csv")
`104_ab104_S` <- read.csv("C:/Users/yijie/Downloads/104_ab104_S.csv")
`105_ab105_C` <- read.csv("C:/Users/yijie/Downloads/105_ab105_C.csv")
`105_ab105_S` <- read.csv("C:/Users/yijie/Downloads/105_ab105_S.csv")
`106_ab105_C` <- read.csv("C:/Users/yijie/Downloads/106_ab105_C.csv")
`106_ab105_S` <- read.csv("C:/Users/yijie/Downloads/106_ab105_S.csv")
Student_RPT_07 <- read.csv("C:/Users/yijie/Downloads/Student_RPT_07.csv")
`105c.`        <- read.csv("C:/Users/yijie/Downloads/105c .csv")
CountriesComparisionTable <- read_csv("C:/Users/yijie/Downloads/CountriesComparisionTable.csv")

`103_ab103_C`$t103<-rowSums(`103_ab103_C`[,3:11])
`104_ab104_C`$t104<-rowSums(`104_ab104_C`[,3:11])
`105_ab105_C`$t105<-rowSums(`105_ab105_C`[,3:11])
`106_ab105_C`$t106<-rowSums(`106_ab105_C`[,3:11])


X103<-`103_ab103_C`[order(`103_ab103_C`$t103,decreasing = T),][1:11,c(2,12)]
X104<-`104_ab104_C`[order(`104_ab104_C`$t104,decreasing = T),][1:11,c(2,12)]
X105<-`105_ab105_C`[order(`105_ab105_C`$t105,decreasing = T),][1:11,c(2,12)]
X106<-`106_ab105_C`[order(`106_ab105_C`$t106,decreasing = T),][1:11,c(2,12)]

X103456<-full_join(X103,full_join(X104,full_join(X105,X106)))
X103456$t<-rowSums(X103456[,2:5])
X103456[order(X103456$t,decreasing = T),][1:10,c(1,6)]
kable(X103456[order(X103456$t,decreasing = T),][1:10,c(1,6)])


#--------------------------------------------------------------------------------

`103_ab103_S`$t103<-rowSums(`103_ab103_S`[,4:12])
`104_ab104_S`$t104<-rowSums(`104_ab104_S`[,4:12])
`105_ab105_S`$t105<-rowSums(`105_ab105_S`[,4:12])
`106_ab105_S`$t106<-rowSums(`106_ab105_S`[,4:12])

ab103<-`103_ab103_S`[order(`103_ab103_S`$t103,decreasing = T),][1:20,c(3,13)]
ab104<-`104_ab104_S`[order(`104_ab104_S`$t104,decreasing = T),][1:20,c(3,13)]
ab105<-`105_ab105_S`[order(`105_ab105_S`$t105,decreasing = T),][1:20,c(3,13)]
ab106<-`106_ab105_S`[order(`106_ab105_S`$t106,decreasing = T),][1:20,c(3,13)]

ab103456<-full_join(ab103,full_join(ab104,full_join(ab105,ab106)))
ab103456$t<-rowSums(ab103456[,2:5])
ab103456[order(ab103456$t,decreasing = T),][1:10,c(1,6)]
kable(ab103456[order(ab103456$t,decreasing = T),][1:10,c(1,6)])

#------------------------------------------------------------------------------

XX103<-`103_ab103_C`[,c(2,12)]
XX104<-`104_ab104_C`[,c(2,12)]
XX105<-`105_ab105_C`[,c(2,12)]
XX106<-`106_ab105_C`[,c(2,12)]

XX103456<-full_join(XX103,full_join(XX104,full_join(XX105,XX106)))
XX103456[is.na(XX103456)]<-0
XX103456$t<-rowSums(XX103456[,2:5])
colnames(XX103456)[1]<-"country"

ggplot()+geom_bar(data=XX103456,aes(x=country,y=t),stat = "identity")

#------------------------------------------------------------------------------
library(readr)
library(dplyr)
library(knitr)
library(ggplot2)
library(choroplethr)
library(choroplethrMaps)
`103_ab103_C` <- read.csv("C:/Users/yijie/Downloads/103_ab103_C.csv")
`104_ab104_C` <- read.csv("C:/Users/yijie/Downloads/104_ab104_C.csv")
`105_ab105_C` <- read.csv("C:/Users/yijie/Downloads/105_ab105_C.csv")
`106_ab105_C` <- read.csv("C:/Users/yijie/Downloads/106_ab105_C.csv")
CountriesComparisionTable <- read_csv("C:/Users/yijie/Downloads/CountriesComparisionTable.csv")

`103_ab103_C`$t103<-rowSums(`103_ab103_C`[,3:11])
`104_ab104_C`$t104<-rowSums(`104_ab104_C`[,3:11])
`105_ab105_C`$t105<-rowSums(`105_ab105_C`[,3:11])
`106_ab105_C`$t106<-rowSums(`106_ab105_C`[,3:11])

XXX103<-`103_ab103_C`[,c(2,12)]
XXX104<-`104_ab104_C`[,c(2,12)]
XXX105<-`105_ab105_C`[,c(2,12)]
XXX106<-`106_ab105_C`[,c(2,12)]

XXX103456<-full_join(XXX103,full_join(XXX104,full_join(XXX105,XXX106)))
XXX103456$t<-rowSums(XXX103456[,2:5])
XXX103456<-XXX103456[,c(1,6)]

colnames(CountriesComparisionTable)[3]<-"國別"
country.name<-right_join(CountriesComparisionTable,XXX103456,by="國別")
country.name<-country.name[complete.cases(country.name),]

library(rgdal)
library(rgeos)
library(maptools)
library(ggplot2)
library(RColorBrewer)
library(readr)
world <- readShapeSpatial("C:/Users/yijie/Downloads/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp")
world.df <-fortify(world, region = "ISO3")

colnames(world.df)[6]<-"ISO3"
final.plot<-merge(world.df,country.name,by="ISO3",all.x=T)
final.plot<-final.plot[order(final.plot$order),]

twcmap<-ggplot() +
  geom_polygon(data = final.plot, 
               aes(x =long, y =lat,
                   group=group,
                   fill = t), 
               color = "gray", 
               size = 0.1) + 
  coord_map()+
  scale_fill_gradientn(
    colours = brewer.pal(5,"Reds"))+
  theme_void()
twcmap
#------------------------------------------------------------------------------------
Student_RPT_07 <- read.csv("C:/Users/yijie/Downloads/Student_RPT_07.csv", stringsAsFactors=FALSE)
colnames(Student_RPT_07)[4]<-"Taiwan"
colnames(Student_RPT_07)[7]<-"total"

Student<-group_by(Student_RPT_07,Taiwan)%>%
  summarise(schoolsum=sum(total))
orderstudent<-head(order(Student$schoolsum,decreasing = T),10)
print(Student[orderstudent,])

#------------------------------------------------------------------------------------
Student_RPT_07 <- read.csv("C:/Users/yijie/Downloads/Student_RPT_07.csv", stringsAsFactors=FALSE)
colnames(Student_RPT_07)[3]<-"sname"
colnames(Student_RPT_07)[7]<-"total"
Student_RPT<-group_by(Student_RPT_07,sname)%>%
  summarise(schoolsum=sum(total))
orderschool<-head(order(Student_RPT$schoolsum,decreasing = T),10)
print(Student_RPT[orderschool,])

#------------------------------------------------------------------------------------

ggplot()+geom_bar(data=Student_RPT,aes(x=sname,y=schoolsum),stat = "identity")


#---------------------------------------------------------------------------------------------

CountriesComparisionTable <- read_csv("C:/Users/yijie/Downloads/CountriesComparisionTable.csv")
colnames(Student_RPT_07)[4]<-"Taiwan"
CountriesComparisionTable[48,3]<-"中國大陸"

schoolcountry.name<-right_join(CountriesComparisionTable,Student_RPT_07,by="Taiwan")
schoolcountry.name<-schoolcountry.name[complete.cases(schoolcountry.name),]

world <- readShapeSpatial("C:/Users/yijie/Downloads/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp")
world.df <-fortify(world, region = "ISO3")

Student<-group_by(schoolcountry.name,ISO3)%>%
  summarise(schoolsum=sum(total))

colnames(world.df)[6]<-"ISO3"
final<-merge(world.df,Student,by="ISO3",all.x=T)
final<-final[order(final$order),]

sixmap<-ggplot() +
  geom_polygon(data = final, 
               aes(x =long, y =lat,
                   group=group,
                   fill =schoolsum), 
               color = "gray", 
               size = 0.1) + 
  coord_map()+
  scale_fill_gradientn(
    colours = brewer.pal(5,"Reds"))+
  theme_void()
sixmap
#---------------------------------------------------------------------------------------------
`105c.`<- read.csv("C:/Users/yijie/Downloads/105c .csv",stringsAsFactor=F)
colnames(`105c.`)[3]<-"tot"
seven<-head(`105c.`<-`105c.`[order(`105c.`$tot,decreasing = T),],10)
seven[,2:3]


#---------------------------------------------------------------------------------------------
CountriesComparisionTable <- read_csv("C:/Users/yijie/Downloads/CountriesComparisionTable.csv")
`105c.`<- read.csv("C:/Users/yijie/Downloads/105c .csv",stringsAsFactor=F)
colnames(`105c.`)[2]<-"Taiwan"

stucountry.name<-right_join(CountriesComparisionTable,`105c.`,by="Taiwan")
stucountry.name<-stucountry.name[complete.cases(stucountry.name),]

world <- readShapeSpatial("C:/Users/yijie/Downloads/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp")
world.df <-fortify(world, region = "ISO3")

colnames(world.df)[6]<-"ISO3"
final<-merge(world.df,stucountry.name,by="ISO3",all.x=T)
final<-final[order(final$order),]
colnames(final)[11]<-"totalpo"


sevenmap<-ggplot() +
  geom_polygon(data = final, 
               aes(x =long, y =lat,
                   group=group,
                   fill =totalpo), 
               color = "gray", 
               size = 0.1) + 
  coord_map()+
  scale_fill_gradientn(
    colours = brewer.pal(5,"Reds"))+
  theme_void()
sevenmap
