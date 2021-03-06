#library(SparkR)
library(rCAT)
library(DT)
library(dplyr)
library(tibble)
library(tidyr)
library(plyr)
library(data.table)
library(stringr)

data_folder <- "C:/Users/aaron.beach/..../GitHub/Hammer Conversion/datafolder - Copy"
processed_folder <- "C:/Users/aaron.beach/..../GitHub/Hammer Conversion/Processed data/"

filenames = list.files(data_folder, pattern = "*.csv", full.names = T)
dataname = basename(filenames)
dataname <-  str_remove_all(dataname, ".csv")
testdf <-  strsplit(dataname, "_")

table1 <-  lapply(filenames, fread, header=FALSE, stringsAsFactors=FALSE)

#imported_data <- read.csv("C:/Users/aaron.beach/..../R/Hammer Conversion/sample2.csv", header=FALSE, stringsAsFactors=FALSE)

labels <-  t(data.frame(strsplit(dataname, "_")))
fullnames <-  data.frame(dataname, labels)


Collateddata = NULL
for (k in 1:length(table1)){

  fullnames1 <- fullnames[k,]
  data1 = as.data.frame(table1[[k]])
  
  Polex <- as.numeric(data1[3,3])
  Poley <- as.numeric(data1[4,3])
  CalFrontGroundx <- as.numeric(data1[7,2])
  CalFrontGroundy <- as.numeric(data1[7,3])
  CalFront1.8x <- as.numeric(data1[8,2])
  CalFront1.8y <- as.numeric(data1[8,3])
  CalBackGroundx <- as.numeric(data1[10,2])
  CalBackGroundy <- as.numeric(data1[10,3])
  CalBack1.8x <- as.numeric(data1[9,2])
  CalBack1.8y <- as.numeric(data1[9,3])
  
  Calx <- CalFrontGroundx - CalBackGroundx
  Caly <- CalFront1.8y - CalFrontGroundy
  ScaleFactorx <- Polex/Calx
  ScaleFactory <- Poley/Caly
  
  SamplingRate <-  as.numeric(data1[15,3])
  ConversionFactor <-  as.numeric(data1[18,4])
  
  
  
  
  Distance = as.character(data1[21,seq(2,(ncol(data1)-1),2)])
  FootFalls <-  data1[24:36,seq(2,(ncol(data1)-1),2)]

  Throws <-  data1[41:44,2:ncol(data1)]
  RoundCount = ncol(FootFalls)
  
  
  Footfalldata <- NULL
  for (i in 1:RoundCount){
    
    
    Footfalldata$DUTurn1[i] <- as.numeric(round((as.numeric(FootFalls[3,i]) - as.numeric(FootFalls[2,i]))*ConversionFactor,2))
    Footfalldata$DUTurn2[i] <- as.numeric(round((as.numeric(FootFalls[6,i]) - as.numeric(FootFalls[5,i]))*ConversionFactor,2))
    Footfalldata$DUTurn3[i] <- as.numeric(round((as.numeric(FootFalls[9,i]) - as.numeric(FootFalls[8,i]))*ConversionFactor,2))
    Footfalldata$DUTurn4[i] <- as.numeric(round((as.numeric(FootFalls[13,i]) - as.numeric(FootFalls[11,i]))*ConversionFactor,2))
    
    
    Footfalldata$Turn1Single[i] <- round((as.numeric(FootFalls[2,i]) - as.numeric(FootFalls[1,i]))*ConversionFactor,2)
    Footfalldata$Turn1Double[i] <- round((as.numeric(FootFalls[4,i]) - as.numeric(FootFalls[2,i]))*ConversionFactor,2)
    Footfalldata$Turn2Single[i] <- round((as.numeric(FootFalls[5,i]) - as.numeric(FootFalls[4,i]))*ConversionFactor,2)
    Footfalldata$Turn2Double[i] <- round((as.numeric(FootFalls[7,i]) - as.numeric(FootFalls[5,i]))*ConversionFactor,2)
    Footfalldata$Turn3Single[i] <- round((as.numeric(FootFalls[8,i]) - as.numeric(FootFalls[7,i]))*ConversionFactor,2)
    Footfalldata$Turn3Double[i] <- round((as.numeric(FootFalls[10,i]) - as.numeric(FootFalls[8,i]))*ConversionFactor,2)
    Footfalldata$Turn4Single[i] <- round((as.numeric(FootFalls[11,i]) - as.numeric(FootFalls[10,i]))*ConversionFactor,2)
    Footfalldata$Turn4Double[i] <- round((as.numeric(FootFalls[13,i]) - as.numeric(FootFalls[11,i]))*ConversionFactor,2)
    
    Footfalldata$Turn1[i] <-  Footfalldata$Turn1Single[i] + Footfalldata$Turn1Double[i]
    Footfalldata$Turn2[i] <-  Footfalldata$Turn2Single[i] + Footfalldata$Turn2Double[i]
    Footfalldata$Turn3[i] <-  Footfalldata$Turn3Single[i] + Footfalldata$Turn3Double[i]
    Footfalldata$Turn4[i] <-  Footfalldata$Turn4Single[i] + Footfalldata$Turn4Double[i]
    Footfalldata$TotalTime[i] <-  Footfalldata$Turn1[i] +Footfalldata$Turn2[i] + Footfalldata$Turn3[i] + Footfalldata$Turn4[i]
    
    
    Footfalldata$Turn1SinglePC[i] <-  round(Footfalldata$Turn1Single[i]/Footfalldata$TotalTime[i]*100,0)
    Footfalldata$Turn1DoublePC[i] <-  round(Footfalldata$Turn1Double[i]/Footfalldata$TotalTime[i]*100,0)
    Footfalldata$Turn2SinglePC[i] <-  round(Footfalldata$Turn2Single[i]/Footfalldata$TotalTime[i]*100,0)
    Footfalldata$Turn2DoublePC[i] <-  round(Footfalldata$Turn2Double[i]/Footfalldata$TotalTime[i]*100,0)
    Footfalldata$Turn3SinglePC[i] <-  round(Footfalldata$Turn3Single[i]/Footfalldata$TotalTime[i]*100,0)
    Footfalldata$Turn3DoublePC[i] <-  round(Footfalldata$Turn3Double[i]/Footfalldata$TotalTime[i]*100,0)
    Footfalldata$Turn4SinglePC[i] <-  round(Footfalldata$Turn4Single[i]/Footfalldata$TotalTime[i]*100,0)
    Footfalldata$Turn4DoublePC[i] <-  round(Footfalldata$Turn4Double[i]/Footfalldata$TotalTime[i]*100,0)
    
  }
  
  Footfalldata <- data.frame(Footfalldata)
  Footfalldatameans <-  as.numeric(round(numcolwise(mean)(Footfalldata),2))
  Footfalldata_withmean <- rbind.data.frame(Footfalldata, Footfalldatameans)
  
  Turn1Single_Combined <- paste0(Footfalldata_withmean$Turn1Single, " (",Footfalldata_withmean$Turn1SinglePC,"%)") 
  Turn1Double_Combined <- paste0(Footfalldata_withmean$Turn1Double, " (",Footfalldata_withmean$Turn1DoublePC,"%)") 
  Turn2Single_Combined <- paste0(Footfalldata_withmean$Turn2Single, " (",Footfalldata_withmean$Turn2SinglePC,"%)") 
  Turn2Double_Combined <- paste0(Footfalldata_withmean$Turn2Double, " (",Footfalldata_withmean$Turn2DoublePC,"%)") 
  Turn3Single_Combined <- paste0(Footfalldata_withmean$Turn3Single, " (",Footfalldata_withmean$Turn3SinglePC,"%)") 
  Turn3Double_Combined <- paste0(Footfalldata_withmean$Turn3Double, " (",Footfalldata_withmean$Turn3DoublePC,"%)") 
  Turn4Single_Combined <- paste0(Footfalldata_withmean$Turn4Single, " (",Footfalldata_withmean$Turn4SinglePC,"%)") 
  Turn4Double_Combined <- paste0(Footfalldata_withmean$Turn4Double, " (",Footfalldata_withmean$Turn4DoublePC,"%)") 
  Turn4Double_Combined <- paste0(Footfalldata_withmean$Turn4Double, " (",Footfalldata_withmean$Turn4DoublePC,"%)") 
  
  
  
  
  ############################
  Throwdata <- NULL
  ThrowsCols <-  seq(1,ncol(Throws),2)
  for(i in 1:RoundCount){
    
    
    j = i*2-1
    Throwdata$InHandx[i] <- as.numeric(Throws[1,j])
    Throwdata$InHandy[i] <- as.numeric(Throws[1,j+1])
    
    Throwdata$OutHand1x[i] <- as.numeric(Throws[2,j])
    Throwdata$OutHand1y[i] <- as.numeric(Throws[2,j+1])
    
    Throwdata$OutHand2x[i] <- as.numeric(Throws[3,j])
    Throwdata$OutHand2y[i] <- as.numeric(Throws[3,j+1])
    
    Throwdata$OutHand3x[i] <- as.numeric(Throws[4,j])
    Throwdata$OutHand3y[i] <- as.numeric(Throws[4,j+1])
    
    Throwdata$Angle1[i] <- round(rad2deg(atan2((Throwdata$OutHand1y[i]-Throwdata$InHandy[i]), (Throwdata$OutHand1x[i]-Throwdata$InHandx[i]))),2)
    Throwdata$Angle2[i] <- round(rad2deg(atan2((Throwdata$OutHand2y[i]-Throwdata$OutHand1y[i]), (Throwdata$OutHand2x[i]-Throwdata$OutHand1x[i]))),2)
    Throwdata$Angle3[i] <- round(rad2deg(atan2((Throwdata$OutHand3y[i]-Throwdata$OutHand2y[i]), (Throwdata$OutHand3x[i]-Throwdata$OutHand2x[i]))),2)
    Throwdata$AngleAvg[i] <- round(mean(c(Throwdata$Angle1[i], Throwdata$Angle2[i], Throwdata$Angle3[i])),2)
    
    Throwdata$HVel1[i] <- round((Throwdata$OutHand1x[i]-Throwdata$InHandx[i])*ScaleFactorx,2)
    Throwdata$VVel1[i] <- round((Throwdata$OutHand1y[i]-Throwdata$InHandy[i])*ScaleFactory,2)
    Throwdata$HVel2[i] <- round((Throwdata$OutHand2x[i]-Throwdata$OutHand1x[i])*ScaleFactorx,2)
    Throwdata$VVel2[i] <- round((Throwdata$OutHand2y[i]-Throwdata$OutHand1y[i])*ScaleFactory,2)
    Throwdata$HVel3[i] <- round((Throwdata$OutHand3x[i]-Throwdata$OutHand2x[i])*ScaleFactorx,2)
    Throwdata$VVel3[i] <- round((Throwdata$OutHand3y[i]-Throwdata$OutHand2y[i])*ScaleFactory,2)
    Throwdata$HVelAvg[i] <- round(mean(c(Throwdata$HVel1[i], Throwdata$HVel2[i], Throwdata$HVel3[i])),2)
    Throwdata$VVelAvg[i] <- round(mean(c(Throwdata$VVel1[i], Throwdata$VVel2[i], Throwdata$VVel3[i])),2)
    
    Throwdata$RVel1[i] <- round(Throwdata$HVel1[i]/cos(deg2rad(Throwdata$Angle1[i])),2)
    Throwdata$RVel2[i] <- round(Throwdata$HVel2[i]/cos(deg2rad(Throwdata$Angle2[i])),2)
    Throwdata$RVel3[i] <- round(Throwdata$HVel3[i]/cos(deg2rad(Throwdata$Angle3[i])),2)
    Throwdata$RVelAvg[i] <- round(mean(c(Throwdata$RVel1[i], Throwdata$RVel2[i], Throwdata$RVel3[i])),2)
    
    Throwdata$RVelfromAvg[i] <- round(Throwdata$HVelAvg[i]/cos(deg2rad(Throwdata$AngleAvg[i])),2)
  }
  Throwdata <- data.frame(Throwdata)

  RoundCount2 <- c(1:RoundCount)

  ExportData = cbind.data.frame(fullnames1, RoundCount2,Distance, Throwdata, Footfalldata, row.names = FALSE)
  ExportData[,1:3] <- sapply(ExportData[,1:3],as.character)
  ExportData <-  ExportData %>% dplyr::rename(Round = RoundCount2, Name = X1, Competition = X2)

  
  filename = paste0(processed_folder,ExportData[1,1])
  write.csv(ExportData, paste0(filename, ".csv"))
  
  Collateddata[[filename]] <- ExportData
}

Collateddatadf = bind_rows(Collateddata)
