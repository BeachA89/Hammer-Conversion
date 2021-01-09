library(SparkR)
library(rCAT)
library(DT)
library(dplyr)
library(tibble)
library(tidyr)
library(plyr)
library(data.table)
library(stringr)
library(ggplot2)
library(RColorBrewer)

data_folder <- "C:/Users/aaron.beach/OneDrive - nswis.com.au/R/Hammer Conversion/Processed data"

filenames = list.files(data_folder, pattern = "*.csv", full.names = T)
dataname = basename(filenames)
dataname <-  str_remove_all(dataname, ".csv")


table1 <-  lapply(filenames, fread, header=TRUE, stringsAsFactors=FALSE)

#imported_data <- read.csv("C:/Users/aaron.beach/OneDrive - nswis.com.au/R/Hammer Conversion/sample2.csv", header=FALSE, stringsAsFactors=FALSE)

Collateddata = rbindlist(table1, fill=TRUE)

Collateddata <- Collateddata %>%dplyr::mutate("Round&Distance" = paste0(Collateddata$Round, " ",Collateddata$Distance))

Collateddata$Distance = as.numeric(Collateddata$Distance)



Collateddata <- Collateddata %>% 
  dplyr::mutate("Turn 1 Single" = paste0(Collateddata$Turn1Single, " (",Collateddata$Turn1SinglePC,"%)")) %>% 
  dplyr::mutate("Turn 1 Double"=paste0(Collateddata$Turn1Double, " (",Collateddata$Turn1DoublePC,"%)")) %>% 
  dplyr::mutate("Turn 2 Single"=paste0(Collateddata$Turn2Single, " (",Collateddata$Turn2SinglePC,"%)")) %>% 
  dplyr::mutate("Turn 2 Double"=paste0(Collateddata$Turn2Double, " (",Collateddata$Turn2DoublePC,"%)")) %>% 
  dplyr::mutate("Turn 3 Single"=paste0(Collateddata$Turn3Single, " (",Collateddata$Turn3SinglePC,"%)")) %>% 
  dplyr::mutate("Turn 3 Double"=paste0(Collateddata$Turn3Double, " (",Collateddata$Turn3DoublePC,"%)")) %>% 
  dplyr::mutate("Turn 4 Single"=paste0(Collateddata$Turn4Single, " (",Collateddata$Turn4SinglePC,"%)")) %>% 
  dplyr::mutate("Turn 4 Double"=paste0(Collateddata$Turn4Double, " (",Collateddata$Turn4DoublePC,"%)")) %>% 
  dplyr::mutate("Turn 1" = paste0(Collateddata$Turn1, " (",Collateddata$Turn1PC,"%)")) %>% 
  dplyr::mutate("Turn 2" = paste0(Collateddata$Turn2, " (",Collateddata$Turn2PC,"%)")) %>% 
  dplyr::mutate("Turn 3" = paste0(Collateddata$Turn3, " (",Collateddata$Turn3PC,"%)")) %>% 
  dplyr::mutate("Turn 4" = paste0(Collateddata$Turn4, " (",Collateddata$Turn4PC,"%)"))

  

Collateddata <-  Collateddata %>% dplyr::filter(Name == 'Alex Hulley') %>% 
  dplyr::filter(Competition == 'SydneyTrackClassic2020')

##### Tables ########
Table1 <- Collateddata %>% select(Name, Competition, Round, Distance, AngleAvg, HVelAvg, RVelAvg, `Turn 1 Single`, `Turn 1 Double`, 
                                  `Turn 2 Single`, `Turn 2 Double`,  `Turn 3 Single`, `Turn 3 Double`,
                                  `Turn 4 Single`, `Turn 4 Double`)

Table1[is.na(Table1)] <- "X"
datatable(Table1)

Table2 <- Collateddata %>% select(Name, Competition, Round, Distance, `Turn 1`, `Turn 2`, `Turn 3`, `Turn 4`, TotalTime)

Table2[is.na(Table2)] <- "X"
datatable(Table2)


Table3 <- Collateddata %>% select(Name, Competition, Round, Distance, DUTurn1, DUTurn2, DUTurn3, DUTurn4)
Table3[is.na(Table3)] <- "X"
datatable(Table3)

##### GGPLOTS ######
Plot1 <-  Collateddata %>% select(`Round&Distance`, `Turn1Single`, `Turn1Double`, 
                            `Turn2Single`, `Turn2Double`,  `Turn3Single`, `Turn3Double`,
                            `Turn4Single`, `Turn4Double`) %>% reshape2::melt(id = c("Round&Distance"))

ggplot(data=Plot1, aes(x=variable, y=value, fill=`Round&Distance`)) + geom_bar(stat="identity",position=position_dodge()) + scale_fill_brewer(palette = "Greens")

Plot2 <-Collateddata %>% select(`Round`, `Turn1Single`, `Turn1Double`, 
                                `Turn2Single`, `Turn2Double`,  `Turn3Single`, `Turn3Double`,
                                `Turn4Single`, `Turn4Double`) %>% reshape2::melt(id = c("Round"))

ggplot(data=Plot2, aes(x=`Round`, y=value, fill=`variable`)) + geom_bar(stat="identity")  + scale_fill_manual(values=c("orange","green4", "orange","green4", "orange", "green4", "orange", "green4")) +
  scale_x_continuous(breaks = c(1:6)) + scale_y_continuous("Time(s)", breaks = seq(0, 2.4, 0.1)) + coord_flip()

Plot3 <-Collateddata %>% select(`Round&Distance`, `DUTurn1`, `DUTurn2`, `DUTurn3`, `DUTurn4`) %>% reshape2::melt(id = c("Round&Distance"))
ggplot(data=Plot3, aes(x=variable, y=value, fill=`Round&Distance`)) + geom_bar(stat="identity",position=position_dodge()) + scale_fill_brewer(palette = "Greens")

