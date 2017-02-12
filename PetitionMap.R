#load required packages
library(RColorBrewer)
library(sp)
library(maptools)
library(classInt)
library(jsonlite)
library(plyr)

#Load in shape file (shp, dbf and shx files must also be present in same location)
electmap <- readShapePoly("westminster_const_region.shp")

#load in json files for both petitions
forTrump=read_json("178844.json")
againstTrump=read_json("171928.json")

#Get the total signature counts for each petition and calculate the ratio
forTotalCount=forTrump$data$attributes$signature_count
againstTotalCount=againstTrump$data$attributes$signature_count
forAgainstOverallRatio=forTotalCount/againstTotalCount

#Create data frames of the for and against signatures by constituency
forFrame=as.data.frame(laply(forTrump$data$attributes$signatures_by_constituency,identity))
againstFrame=as.data.frame(laply(againstTrump$data$attributes$signatures_by_constituency,identity))

#check if the data is sorted correctly for comparison (data check)
#If it is create a list of the ratio of For/Against Signatories for each constituency
if(identical(forFrame$ons_code,againstFrame$ons_code)){
	FdivAVect=as.integer(forFrame$signature_count)/as.integer(againstFrame$signature_count)
	}

#Create a second list the is the difference compared with the overall ratio for each constituency
#Negative numbers indicate a constituency is more against Trump, positive numbers more in favour
FDivADiffVect=FdivAVect-forAgainstOverallRatio

#Create a dataframe containing only ONS codes and FdiVADiff numbers
DiffFrame=data.frame(laply(forFrame$ons_code,identity),FDivADiffVect)
colnames(DiffFrame)[1]<-"CODE"

#merge this with electmap
plotFrame<-merge(electmap,DiffFrame, by="CODE")

#Set colour pallette
colours<-brewer.pal(11,"RdYlBu")

#set colours and class intervals
colours<-brewer.pal(11,"RdYlBu")
colours<-rev(colours)
intervals<-classIntervals(plotFrame$FDivADiffVect,n=11,style="equal")
intervals<-intervals$brks

#plot the Chloropleth
plot(plotFrame, col=colours[findInterval(plotFrame$FDivADiffVect, intervals, all.inside=TRUE)])
#Add Title
title(paste("Trump visit petition signatures For/Against ratio vs overall ratio"))
#Add Legend
legend(x="right", legend=leglabs(signif(intervals,2)),fill=colours)
