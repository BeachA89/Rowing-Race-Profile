library(shiny)
library(shinydashboard)
library(shinythemes)
library(quantmod)
library(zoo)
library(R.utils)
library(plyr)
library(tableHTML)
library(svDialogs)
library(ggplot2)
library(dplyr)
library(stringr)
library(data.table)
library(lubridate)
library(reshape2)
library(formattable)
library(matrixStats)
library(broom)
library(tibble)
library(Rmisc)
library(DT)
library(tidyverse)
library(plotly)

sec_to_ms <- function(secs){
  
  as.character(      
    paste(
      formatC(secs %/% 60 %% 60, width = 1, format = "d", flag = "0"),
      formatC(secs %% 60, width = 5, format = "f", digits = 2, flag = "0"),
      sep = ":"
    )
  )
  
}

Prognostic.Velocities <- data.frame(fread("Prognostic Velocities.csv", header = TRUE, stringsAsFactors=FALSE))

filePaths <- list.files("data", full.names = TRUE)
table1 <-  lapply(filePaths, fread, skip = 3, header=TRUE, stringsAsFactors=FALSE)


#### identify data labels ####

#dataname  <- input$file1[['name']]
dataname <-  str_remove_all(filePaths, ".csv")
dataname <-  str_remove_all(dataname, "data/")

newdataframe = NULL
for (i in 1:length(dataname)){
  a <- data.frame(strsplit(dataname[i], "_"))
  if (nrow(a) == 7){
    newdataframe[[i]] <- data.frame(a[1,], a[2,], a[3,], NA,NA,NA, a[4,], a[5,], a[6,], a[7,])
  }else if (nrow(a) == 8){
    newdataframe[[i]] <- data.frame(a[1,], a[2,], a[3,], a[4,], NA,NA, a[5,], a[6,], a[7,], a[8,])
  }else if (nrow(a) == 10){
    newdataframe[[i]] <- data.frame(a[1,], a[2,], a[3,], a[4,], a[5,], a[6,], a[7,], a[8,], a[9,], a[10,])
  }
}
newdataframe_combine <-  rbindlist(newdataframe, use.names=FALSE)


fullnames <-  data.frame(dataname, newdataframe_combine)

zeros <-data.frame(0, 0, 0, 0, 0)
names(zeros) <- c("Time", "Dist", "AvVel", "AvStkRate", "#Strokes")

for (i in 1:length(table1)){
  table1[[i]] = rbind(zeros, table1[[i]])
}

#### Create Accumulated Time column ####


for (i in 1:length(table1)){
  table1[[i]][["AccTime"]] = cumsum(table1[[i]][["Time"]])
}


#### Create col names ####
col_names <- c(
  "ID", "Date", "Class", "Name1", "Name2", "Name3","Name4","Competition", "Location", "Age", "Phase", #1-8
  "Time_0m", #9
  "Time_50m", "Time_100m", "Time_150m", "Time_200m", 
  "Time_250m", "Time_300m", "Time_350m", "Time_400m", 
  "Time_450m", "Time_500m", "Time_550m", "Time_600m", 
  "Time_650m", "Time_700m", "Time_750m", "Time_800m", 
  "Time_850m", "Time_900m", "Time_950m", "Time_1000m", 
  "Time_1050m", "Time_1100m", "Time_1150m", "Time_1200m",
  "Time_1250m", "Time_1300m", "Time_1350m", "Time_1400m",
  "Time_1450m", "Time_1500m", "Time_1550m", "Time_1600m",
  "Time_1650m", "Time_1700m", "Time_1750m", "Time_1800m", 
  "Time_1850m", "Time_1900m", "Time_1950m", "Time_2000m", #49
  
  "AvVel_0m", #50
  "AvVel_50m", "AvVel_100m", "AvVel_150m", "AvVel_200m",  
  "AvVel_250m", "AvVel_300m", "AvVel_350m", "AvVel_400m",
  "AvVel_450m", "AvVel_500m", "AvVel_550m", "AvVel_600m",
  "AvVel_650m", "AvVel_700m", "AvVel_750m", "AvVel_800m",
  "AvVel_850m", "AvVel_900m", "AvVel_950m", "AvVel_1000m",
  "AvVel_1050m", "AvVel_1100m", "AvVel_1150m", "AvVel_1200m",
  "AvVel_1250m", "AvVel_1300m", "AvVel_1350m", "AvVel_1400m",
  "AvVel_1450m", "AvVel_1500m", "AvVel_1550m", "AvVel_1600m",
  "AvVel_1650m", "AvVel_1700m", "AvVel_1750m", "AvVel_1800m",
  "AvVel_1850m", "AvVel_1900m", "AvVel_1950m", "AvVel_2000m", # 90
  
  "AvStkRate_0m", #91
  "AvStkRate_50m", "AvStkRate_100m", "AvStkRate_150m", "AvStkRate_200m", 
  "AvStkRate_250m", "AvStkRate_300m", "AvStkRate_350m", "AvStkRate_400m",
  "AvStkRate_450m", "AvStkRate_500m", "AvStkRate_550m", "AvStkRate_600m",
  "AvStkRate_650m", "AvStkRate_700m", "AvStkRate_750m", "AvStkRate_800m",
  "AvStkRate_850m", "AvStkRate_900m", "AvStkRate_950m", "AvStkRate_1000m",
  "AvStkRate_1050m", "AvStkRate_1100m", "AvStkRate_1150m", "AvStkRate_1200m",
  "AvStkRate_1250m", "AvStkRate_1300m", "AvStkRate_1350m", "AvStkRate_1400m",
  "AvStkRate_1450m", "AvStkRate_1500m", "AvStkRate_1550m", "AvStkRate_1600m",
  "AvStkRate_1650m", "AvStkRate_1700m", "AvStkRate_1750m", "AvStkRate_1800m",
  "AvStkRate_1850m", "AvStkRate_1900m", "AvStkRate_1950m", "AvStkRate_2000m", #131
  
  "Strokes_0m", #132
  "Strokes_50m", "Strokes_100m", "Strokes_150m", "Strokes_200m", 
  "Strokes_250m", "Strokes_300m", "Strokes_350m", "Strokes_400m",
  "Strokes_450m", "Strokes_500m", "Strokes_550m", "Strokes_600m",
  "Strokes_650m", "Strokes_700m", "Strokes_750m", "Strokes_800m",
  "Strokes_850m", "Strokes_900m", "Strokes_950m", "Strokes_1000m",
  "Strokes_1050m", "Strokes_1100m", "Strokes_1150m", "Strokes_1200m",
  "Strokes_1250m", "Strokes_1300m", "Strokes_1350m", "Strokes_1400m",
  "Strokes_1450m", "Strokes_1500m", "Strokes_1550m", "Strokes_1600m",
  "Strokes_1650m", "Strokes_1700m", "Strokes_1750m", "Strokes_1800m",
  "Strokes_1850m", "Strokes_1900m", "Strokes_1950m", "Strokes_2000m", #172
  
  "AccTime_0m", #173
  "AccTime_50m", "AccTime_100m", "AccTime_150m", "AccTime_200m", 
  "AccTime_250m", "AccTime_300m", "AccTime_350m", "AccTime_400m",
  "AccTime_450m", "AccTime_500m", "AccTime_550m", "AccTime_600m",
  "AccTime_650m", "AccTime_700m", "AccTime_750m", "AccTime_800m",
  "AccTime_850m", "AccTime_900m", "AccTime_950m", "AccTime_1000m",
  "AccTime_1050m", "AccTime_1100m", "AccTime_1150m", "AccTime_1200m",
  "AccTime_1250m", "AccTime_1300m", "AccTime_1350m", "AccTime_1400m",
  "AccTime_1450m", "AccTime_1500m", "AccTime_1550m", "AccTime_1600m",
  "AccTime_1650m", "AccTime_1700m", "AccTime_1750m", "AccTime_1800m",
  "AccTime_1850m", "AccTime_1900m", "AccTime_1950m", "AccTime_2000m"#, #213
  
)


col_namesdist <- c(
  "ID", "Date", "Class", "Name1", "Name2", "Name3","Name4","Competition", "Location", "Age", "Phase",
  0, 50, 100, 150, 200, 
  250, 300, 350, 400,
  450, 500, 550, 600,
  650, 700, 750, 800,
  850, 900, 950, 1000,
  1050, 1100, 1150, 1200,
  1250, 1300, 1350, 1400,
  1450, 1500, 1550, 1600,
  1650, 1700, 1750, 1800,
  1850, 1900, 1950, 2000)




#### Transpose data to along columns of one row  and combine with labels ####

data_transposed = list()
for (i in 1:length(table1)){
  data2 = table1[[i]]
  data_transposed[[i]] <-  data.frame(t(data.frame(data2[,1])), t(data.frame(data2[,3])), t(data.frame(data2[,4])), t(data.frame(data2[,5])), t(data.frame(data2[,6])))
}

#combine all dataframes in list to one dataframe#

data1 <-  rbindlist(data_transposed, fill=TRUE)

# combine  labels and data into one dataframe #


Labelled_data <-  data.frame(fullnames, data1)


#define column and row names (data500 labels)

row_numbers <-  1:nrow(fullnames)

colnames(Labelled_data) <-  col_names


rownames(Labelled_data) <-  row_numbers


#### sort data by final time ####
Labelled_data <-  arrange(Labelled_data, AccTime_2000m)

Labelled_data_top10 <- Labelled_data[1:10,]

#### extract just time variable ####


Labelled_data_Time <-  Labelled_data[,1:52]

colnames(Labelled_data_Time) <-  col_namesdist

Labelled_data_top10_Time <-  Labelled_data_top10[,1:52]

colnames(Labelled_data_top10_Time) <-  col_namesdist

Labelled_data_Time$Class = as.character(Labelled_data_Time$Class)
Labelled_data_Time$Name1 = as.character(Labelled_data_Time$Name1)
Labelled_data_Time$Name2 = as.character(Labelled_data_Time$Name2)
Labelled_data_Time$Name3 = as.character(Labelled_data_Time$Name3)
Labelled_data_Time$Name4 = as.character(Labelled_data_Time$Name4)
Labelled_data_Time$Competition = as.character(Labelled_data_Time$Competition)
Labelled_data_Time$Date = as.character(Labelled_data_Time$Date)
Labelled_data_Time$Phase = as.character(Labelled_data_Time$Phase)

#### extract just Av vel variable ####

cols <-c(1:11, 53:93)
Labelled_data_AvVel <-  Labelled_data[cols]

colnames(Labelled_data_AvVel) <-  col_namesdist

Labelled_data_top10_AvVel <-  Labelled_data_top10[cols]

colnames(Labelled_data_top10_AvVel) <-  col_namesdist

#### extract just AvStkRate variable ####

cols <-c(1:11, 94:134)
Labelled_data_AvStkRate <-  Labelled_data[cols]

colnames(Labelled_data_AvStkRate) <-  col_namesdist

Labelled_data_top10_AvStkRate <-  Labelled_data_top10[cols]

colnames(Labelled_data_top10_AvStkRate) <-  col_namesdist

#### extract just Strokes variable ####

cols <-c(1:11, 135:175)
Labelled_data_Strokes <-  Labelled_data[cols]

colnames(Labelled_data_Strokes) <-  col_namesdist

Labelled_data_top10_Strokes <-  Labelled_data_top10[cols]

colnames(Labelled_data_top10_Strokes) <-  col_namesdist

#### extract just AccTime variable ####


cols <-c(1:11, 176:216)
Labelled_data_AccTime <-  Labelled_data[cols]

colnames(Labelled_data_AccTime) <-  col_namesdist

Labelled_data_top10_AccTime <-  Labelled_data_top10[cols]

colnames(Labelled_data_top10_AccTime) <-  col_namesdist

