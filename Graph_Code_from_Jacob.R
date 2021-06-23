##Author: Jacob Fuhr, Last Updated 23/3/2021

#Set working directory to location of excel file being used
setwd("/Users/jacobfuhr/Code") 

#Skip if packages are already installed
install.packages("readxl")
install.packages("ggplot2") 
install.packages("wesanderson")
install.packages("languageserver")

#Load packages
library("readxl")
library("ggplot2")
library("wesanderson")

#Pull in excel data file, change name as necessary
full_water_data <- read_excel("Table_3_Water_to_Jacob.xlsx") 

#Create smaller matrices for working with data
gen_data <- full_water_data[,c(1:2,4,6,10,18,149:151,157:160)]
gset1_data <-gen_data[,c(1,3:5,10:13)]
gset2_data <-gen_data[,c(1:9)]
gset3_data <-gen_data[,c(1:4,10:12)]

##Graph set 1

#Only correct depths
gset1_data <- gset1_data[!(gset1_data$sample_depth_m=="SW"),c(1,3:8)]

#Clean up dates
count<-1
while (count <= nrow(gset1_data)){
  gset1_data$sample_collection_date_mm_dd_yy_h_mm[count] <- as.Date(gset1_data$sample_collection_date_mm_dd_yy_h_mm[count])
  count <- count+1
}

#Start variables
start<-0
end<-0

#Proper search and plot
for (locs in unique(gset1_data$site_name[])){
  startloc<-nrow(gset1_data)
  endloc<-0
  for (val in 1:nrow(gset1_data)){
    if(locs==gset1_data[val,1] & val<startloc){
      startloc<-val
    }
    if(locs==gset1_data[val,1] & (val>endloc | val==nrow(gset1_data))){
      endloc<-val
    }
  }
  startdate<-startloc
  enddate<-startloc-1
  for (date in unique(gset1_data$sample_collection_date_mm_dd_yy_h_mm[startloc:endloc])){
    startdate<-enddate+1
    for (val in startloc:endloc){
      if(date==gset1_data$sample_collection_date_mm_dd_yy_h_mm[val] & (val>enddate | val==endloc)){
        enddate<-val
      }
    }
    #print(paste("Location: ",locs," StartL: ",startloc," EndL: ",endloc," StartD: ",startdate," EndD: ",enddate,sep=""))
    start<-startdate
    end<-enddate
    plot(gset1_data$percent_Comp1[start:end], gset1_data$sample_depth_m[start:end], 
         ylim = rev(range(0:sum(strtoi(gset1_data$sample_depth_m[enddate]),20))),
         xlim = c(0,7), col=wes_palette("Moonrise2")[1],
         type="o", main=paste("Depth profile at",locs,sep=" "), lwd = 2,
         sub = as.Date(gset1_data$sample_collection_date_mm_dd_yy_h_mm[enddate]),
         xlab="% Composition for Components or Ratio of Components for Ratios",
         ylab="Sample Depth (m)", xaxt="n")
    axis(1, at = c(0,0.25,0.5,0.75,1,2,3,4,5,6,7))
    lines(gset1_data$percent_Comp2[start:end], gset1_data$sample_depth_m[start:end], 
          col=wes_palette("Moonrise2")[2], type="o", lwd = 2)
    lines(gset1_data$percent_Comp3[start:end], gset1_data$sample_depth_m[start:end], 
          col=wes_palette("Moonrise2")[3], type="o", lwd = 2)
    lines(gset1_data$`Ratio_Comp1/Comp3`[start:end], gset1_data$sample_depth_m[start:end], 
         col=wes_palette("Moonrise3")[1], type="o", lty=2, lwd = 2)
    lines(gset1_data$percent_Comp2[start:end]/gset1_data$percent_Comp3[start:end], gset1_data$sample_depth_m[start:end], 
          col=wes_palette("Moonrise3")[2], type="o", lwd = 2, lty=2)
    lines(gset1_data$percent_Comp1[start:end]/gset1_data$percent_Comp2[start:end], gset1_data$sample_depth_m[start:end], 
          col=wes_palette("Moonrise3")[3], type="o", lwd = 2, lty=2)
    legend("bottomright", legend=c("Component 1","Component 2","Component 3","C1/C3","C2/C3","C1/C2"), 
           col = c(col=wes_palette("Moonrise2")[1], col=wes_palette("Moonrise2")[2],
                   col=wes_palette("Moonrise2")[3], col=wes_palette("Moonrise3")[1], 
                   col=wes_palette("Moonrise3")[2], col=wes_palette("Moonrise3")[3]), lty=c(1,1,1,2,2,2), cex=0.8, lwd = 2)
  }
} 


##Graph Set 2

#Only correct locations
gset2_data <- gset2_data[!(gset2_data$site_classification=="Tributary"),c(1,4,6:9)]

#Clean up dates
count<-1
while (count <= nrow(gset2_data)){
  gset2_data$sample_collection_date_mm_dd_yy_h_mm[count] <- as.Date(gset2_data$sample_collection_date_mm_dd_yy_h_mm[count])
  count <- count+1
}

#Format and plot sets
for (locs in unique(gset2_data$site_name[1:20])){
  startloc<-nrow(gset2_data)
  endloc<-0
  for (val in 1:nrow(gset2_data)){
    if(locs==gset2_data[val,1] & val<startloc){
      startloc<-val
    }
    if(locs==gset2_data[val,1] & (val>endloc | val==nrow(gset2_data))){
      endloc<-val
    }
  }
  startYear<-startloc
  endYear<-startloc-1
  for (year in unique(format(gset2_data$sample_collection_date_mm_dd_yy_h_mm[startloc:endloc], "%Y"))){
    startYear<-endYear+1
    for (val in startloc:endloc){
      if(year==format(gset2_data$sample_collection_date_mm_dd_yy_h_mm[val], "%Y") & (val>endYear | val==endloc)){
        endYear<-val
      }
    }
    
    #Make lists containing month by month here
    monthsV <- c()
    comp1V <- c()
    comp2V <- c()
    comp3V <- c()
    docV <- c()
    
    
    startMon<-startYear
    endMon<-startYear-1
    for (month in unique(format(gset2_data$sample_collection_date_mm_dd_yy_h_mm[startYear:endYear], "%B"))){
      startMon<-endMon+1
      for (val in startYear:endYear){
        if(month==format(gset2_data$sample_collection_date_mm_dd_yy_h_mm[val], "%B") & (val>endMon | val==endYear)){
          endMon<-val
        }
      }
      
      comp1sum <-0
      comp2sum <-0
      comp3sum <-0
      docsum <-0
      
      for( val in startMon:endMon){
        comp1sum <-comp1sum + gset2_data$Comp.1[val]
        comp2sum <-comp2sum + gset2_data$Comp.2[val]
        comp3sum <-comp3sum + gset2_data$Comp.3[val]
        docsum <-docsum + gset2_data$doc_boulder_mgc_per_l[val]
      }
      
      comp1avg <- comp1sum/(endMon-startMon+1)
      comp2avg <-comp2sum/(endMon-startMon+1)
      comp3avg <-comp3sum/(endMon-startMon+1)
      docsavg <-docsum/(endMon-startMon+1)
      
      monthsV[length(monthsV)+1]<-month
      comp1V[length(comp1V)+1] <- comp1avg
      comp2V[length(comp2V)+1] <- comp2avg
      comp3V[length(comp3V)+1] <- comp3avg
      docV[length(docV)+1] <- docsavg
    }
    
    gset2_data_yearlyavgs <- data.frame(monthsV, comp1V, comp2V, comp3V, docV)
    
    graph <- ggplot(data = gset2_data_yearlyavgs, aes(x=monthsV, y=comp1V, group = 1))+
              geom_line() +
              geom_point()
    print(graph)
  }
}


ggplot(data = gset2_data_yearlyavgs, aes(x=monthsV, y=comp1V))+
  geom_line()


#  plot(gset1_data$percent_Comp1[start:end], gset1_data$sample_depth_m[start:end], 
#       ylim = rev(range(0:sum(strtoi(gset1_data$sample_depth_m[enddate]),20))),
#       xlim = c(0,7), col=wes_palette("Moonrise2")[1],
#       type="o", main=paste("Depth profile at",locs,sep=" "), lwd = 2,
#       sub = as.Date(gset1_data$sample_collection_date_mm_dd_yy_h_mm[enddate]),
#       xlab="% Composition for Components or Ratio of Components for Ratios",
#       ylab="Sample Depth (m)", xaxt="n")
#  axis(1, at = c(0,0.25,0.5,0.75,1,2,3,4,5,6,7))
#  lines(gset1_data$percent_Comp2[start:end], gset1_data$sample_depth_m[start:end], 
#        col=wes_palette("Moonrise2")[2], type="o", lwd = 2)
#  lines(gset1_data$percent_Comp3[start:end], gset1_data$sample_depth_m[start:end], 
#        col=wes_palette("Moonrise2")[3], type="o", lwd = 2)
#  lines(gset1_data$`Ratio_Comp1/Comp3`[start:end], gset1_data$sample_depth_m[start:end], 
#        col=wes_palette("Moonrise3")[1], type="o", lty=2, lwd = 2)
#  lines(gset1_data$percent_Comp2[start:end]/gset1_data$percent_Comp3[start:end], gset1_data$sample_depth_m[start:end], 
#        col=wes_palette("Moonrise3")[2], type="o", lwd = 2, lty=2)
#  lines(gset1_data$percent_Comp1[start:end]/gset1_data$percent_Comp2[start:end], gset1_data$sample_depth_m[start:end], 
#        col=wes_palette("Moonrise3")[3], type="o", lwd = 2, lty=2)
#  legend("bottomright", legend=c("Component 1","Component 2","Component 3","C1/C3","C2/C3","C1/C2"), 
#         col = c(col=wes_palette("Moonrise2")[1], col=wes_palette("Moonrise2")[2],
#                 col=wes_palette("Moonrise2")[3], col=wes_palette("Moonrise3")[1], 
#                 col=wes_palette("Moonrise3")[2], col=wes_palette("Moonrise3")[3]), lty=c(1,1,1,2,2,2), cex=0.8, lwd = 2)
