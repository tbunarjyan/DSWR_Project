library(plyr)
library(dplyr) 
library(readr) 
library(reshape2) 
library(tidyr)
library(ggplot2)
library(ggthemes)

apps<-read.csv("yerevan_april_9.csv")
apps$price=apps$price/1000

#scatterplots

#price-area
q<-qplot(area,price,data=apps,color=price,main=("Dependency of price on area")) 
q+ylab("Price(in K)")+xlab("Area")
q+geom_point(size=0.5)+geom_smooth(alpha=I(0.7),method = "lm")
q+facet_wrap(~ district,ncol=3)# to see for different districts 

#boxplots

#condition-price
q<-qplot(condition,price,color=condition,data = apps,geom="boxplot")
q+xlab("Condition")+ylab("Price(in K)")
qplot(condition,price,color=condition,data = apps,geom="boxplot",outlier.colour = NA)
#district-price
k<-qplot(district,price,color=district,data = apps,geom="boxplot")
k+xlab("District")+ylab("Price(in K)")
#condition-area
q<-qplot(condition,area,color=condition,data = apps,geom="boxplot")
q+xlab("Condition")+ylab("Price(in K)")
qplot(condition,area,color=condition,data = apps,geom="boxplot",outlier.colour = NA)
#district-area
k<-qplot(district,area,color=district,data = apps,geom="boxplot")
k+xlab("District")+ylab("Area")

#histogram

#price-condition
g<-ggplot(apps,aes(x=price))+geom_histogram(binwidth = 200,aes(fill=condition))  
g+xlab("Price(in K)")+ylab("Count")+ggtitle("Histogram of Price")+scale_fill_discrete(name="Condition")
g+facet_wrap(~condition) 
#price-building type
g<-ggplot(apps,aes(x=price))+geom_histogram(binwidth = 200,aes(fill=building_type)) 
g+xlab("Price(in K)")+ylab("Count")+ggtitle("Histogram of Price")+scale_fill_discrete(name="Building_Type")
g+facet_wrap(~building_type)
#area-district
g<-ggplot(apps,aes(x=area))+geom_histogram(binwidth = 200,aes(fill=district)) 
g+xlab("Area(in m^2)")+ylab("Count")+ggtitle("Histogram of Area")+scale_fill_discrete(name="District")
g+facet_wrap(~district)

#density

#price-condition
g<-ggplot(apps,aes(x=price))+geom_density(kernel="gaussian",aes(fill=condition))
g+xlab("Price(in K)")+ylab("Density")+ggtitle("Density of Price")+scale_fill_discrete(name="Condition")
g<-g+facet_wrap(~condition)
g
#price-building_type
g<-ggplot(apps,aes(x=price))+geom_density(kernel="gaussian",aes(fill=building_type))
g+xlab("Price(in K)")+ylab("Density")+ggtitle("Density of Price")+scale_fill_discrete(name="Building_Type")
g<-g+facet_wrap(~building_type)
g
#area-building_type
g<-ggplot(apps,aes(x=area))+geom_density(kernel="gaussian",aes(fill=building_type))
g+xlab("Area(in m^2)")+ylab("Density")+ggtitle("Density of Area")+scale_fill_discrete(name="Building_Type")
g<-g+facet_wrap(~building_type)
g

#barcharts

#district-building_type
g<-ggplot(apps,aes(x=district))+geom_bar(aes(fill=building_type)) 
g+xlab("District")+ylab("Count")+ggtitle("Barchart for districts")+scale_fill_discrete(name="Building_Type")
g+scale_fill_gdocs()+theme_gdocs() 
#max_floor-condition
g<-ggplot(apps,aes(x=max_floor))+geom_bar(aes(fill=condition)) 
g+xlab("Max_floor")+ylab("Count")+ggtitle("Barchart for Max_Floor")+scale_fill_discrete(name="Condition")
#building_type-condition
g<-ggplot(apps,aes(x=building_type))+geom_bar(aes(fill=condition))
g+xlab("Building_Type")+ylab("Count")+ggtitle("Barchart for Building_Type")+scale_fill_discrete(name="Condition")
