#1000m

library(svDialogs)
library(ggplot2)
library(dplyr)
library(stringr)
library(tcltk)
library(data.table)
library(plyr)
library(lubridate)
library(reshape2)
library(formattable)
library(matrixStats)
library(broom)
library(tibble)
library(Rmisc)
library(DT)
library(tidyverse)

graphics.off()

#Select data folder

Sys.sleep(1) #pause just a little for dailogs
data_folder <- tk_choose.dir(getwd(), caption = "Select directory")

#data_folder = "C:/Users/aaron.beach/OneDrive - nswis.com.au/R/Canoe Race Model/data"

Prog_Speed <- 3.59

sec_to_ms <- function(secs){
  
  as.character(      
    paste(
      formatC(secs %/% 60 %% 60, width = 1, format = "d", flag = "0"),
      formatC(secs %% 60, width = 5, format = "f", digits = 2, flag = "0"),
      sep = ":"
    )
  )
  
}

#identify data labels

filenames = list.files(data_folder, pattern = "*.csv", full.names = T)
dataname = basename(filenames)
dataname <-  str_remove_all(dataname, ".csv")
labels <-  t(data.frame(strsplit(dataname, "_")))
fullnames <-  data.frame(dataname, labels)
#fullnames <-  fullnames[,c(1,2,3,4,7,8,10)]

#Read data into a list
table1 <- lapply(filenames, fread, skip = 3, sep=",", header=TRUE, stringsAsFactors=FALSE, fill=TRUE)

#### Create zero row ####

zeros <-data.frame(0, 0, 0, 0, 0)
names(zeros) <- c("Time", "Dist", "AvVel", "AvStkRate", "#Strokes")

for (i in 1:length(table1)){
  table1[[i]] = rbind(zeros, table1[[i]])
}

#### Create Accumulated Time column ####


for (i in 1:length(table1)){
  table1[[i]][["AccTime"]] = cumsum(table1[[i]][["Time"]])
}

#### Create Prognostic % Time column ####


for (i in 1:length(table1)){
  table1[[i]][["Prog Speed"]] = (table1[[i]][["AvVel"]])/Prog_Speed*100
}



#### Create col names ####
col_names <- c(
  "ID", "Name", "Competition", "Date", "Location", "Class", "Age", "Phase", #1-8
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
  "AccTime_1850m", "AccTime_1900m", "AccTime_1950m", "AccTime_2000m", #213
  
  "ProgSpeed_0m", #214
  "ProgSpeed_50m", "ProgSpeed_100m", "ProgSpeed_150m", "ProgSpeed_200m", 
  "ProgSpeed_250m", "ProgSpeed_300m", "ProgSpeed_350m", "ProgSpeed_400m",
  "ProgSpeed_450m", "ProgSpeed_500m", "ProgSpeed_550m", "ProgSpeed_600m",
  "ProgSpeed_650m", "ProgSpeed_700m", "ProgSpeed_750m", "ProgSpeed_800m",
  "ProgSpeed_850m", "ProgSpeed_900m", "ProgSpeed_950m", "ProgSpeed_1000m",
  "ProgSpeed_1050m", "ProgSpeed_1100m", "ProgSpeed_1150m", "ProgSpeed_1200m",
  "ProgSpeed_1250m", "ProgSpeed_1300m", "ProgSpeed_1350m", "ProgSpeed_1400m",
  "ProgSpeed_1450m", "ProgSpeed_1500m", "ProgSpeed_1550m", "ProgSpeed_1600m",
  "ProgSpeed_1650m", "ProgSpeed_1700m", "ProgSpeed_1750m", "ProgSpeed_1800m",
  "ProgSpeed_1850m", "ProgSpeed_1900m", "ProgSpeed_1950m", "ProgSpeed_2000m" #254
)


col_namesdist <- c(
  "ID", "Name", "Competition", "Date", "Location", "Class", "Age", "Phase",
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





#### Transpose data to along columns of one row ####

data_transposed = list()
for (i in 1:length(table1)){
  data2 = table1[[i]]
    data_transposed[[i]] <-  data.frame(t(data.frame(data2[,1])), t(data.frame(data2[,3])), t(data.frame(data2[,4])), t(data.frame(data2[,5])), t(data.frame(data2[,6])), t(data.frame(data2[,7])))
  }

#combine all dataframes in list to one dataframe

data <-  rbindlist(data_transposed, fill=TRUE)

#### combine  labels and data into one dataframe ####


Labelled_data <-  data.frame(fullnames, data)


#define column and row names (data500 labels)

row_numbers <-  1:nrow(fullnames)

colnames(Labelled_data) <-  col_names


rownames(Labelled_data) <-  row_numbers


#### sort data500combined by final time ####
Labelled_data <-  arrange(Labelled_data, AccTime_2000m)

Labelled_data_top10 <- Labelled_data[1:10,]

#### extract just time variable ####


Labelled_data_Time <-  Labelled_data[,1:49]

colnames(Labelled_data_Time) <-  col_namesdist

Labelled_data_top10_Time <-  Labelled_data_top10[,1:49]

colnames(Labelled_data_top10_Time) <-  col_namesdist

#### extract just Av vel variable ####

cols <-c(1:8, 50:90)
Labelled_data_AvVel <-  Labelled_data[cols]

colnames(Labelled_data_AvVel) <-  col_namesdist

Labelled_data_top10_AvVel <-  Labelled_data_top10[cols]

colnames(Labelled_data_top10_AvVel) <-  col_namesdist

#### extract just AvStkRate variable ####

cols <-c(1:8, 91:131)
Labelled_data_AvStkRate <-  Labelled_data[cols]

colnames(Labelled_data_AvStkRate) <-  col_namesdist

Labelled_data_top10_AvStkRate <-  Labelled_data_top10[cols]

colnames(Labelled_data_top10_AvStkRate) <-  col_namesdist

#### extract just Strokes variable ####

cols <-c(1:8, 132:172)
Labelled_data_Strokes <-  Labelled_data[cols]

colnames(Labelled_data_Strokes) <-  col_namesdist

Labelled_data_top10_Strokes <-  Labelled_data_top10[cols]

colnames(Labelled_data_top10_Strokes) <-  col_namesdist

#### extract just AccTime variable ####


cols <-c(1:8, 173:213)
Labelled_data_AccTime <-  Labelled_data[cols]

colnames(Labelled_data_AccTime) <-  col_namesdist

Labelled_data_top10_AccTime <-  Labelled_data_top10[cols]

colnames(Labelled_data_top10_AccTime) <-  col_namesdist

#### extract just Prog Speed variable ####


cols <-c(1:8, 214:254)
Labelled_data_ProgSpeed <-  Labelled_data[cols]

colnames(Labelled_data_ProgSpeed) <-  col_namesdist

Labelled_data_top10_ProgSpeed <-  Labelled_data_top10[cols]

colnames(Labelled_data_top10_ProgSpeed) <-  col_namesdist


####melt Time data grouping by ID for ggplot####


Labelled_data_Time_melted <- Labelled_data_Time %>% melt(id = c("ID", "Name", "Competition", "Date", "Location", "Class", "Age", "Phase"))


Labelled_data_top10_Time_melted <- melt(Labelled_data_top10_Time, id = c("ID", "Name", "Competition", "Date", "Location", "Class", "Age", "Phase")) 


#Make split column ("Variable") as numeric for plotting
Labelled_data_Time_melted$variable <-  as.numeric(as.character(Labelled_data_Time_melted$variable))
Labelled_data_top10_Time_melted$variable <-  as.numeric(as.character(Labelled_data_top10_Time_melted$variable))

#calculate mean and CI from melted data (group by split)
Labelled_data_top10_Time_melted_meanCI <- 
  Labelled_data_top10_Time_melted %>%
  group_by(variable)%>%
  dplyr::summarise(UpperLimit = CI(value, ci=0.95)[1],
                   Average = mean(value), 
                   LowerLimit = CI(value, ci=0.95)[3]) %>%
  mutate(variable = variable %>% as.factor())

#make sure the split variable is numeric
Labelled_data_top10_Time_melted_meanCI$variable <-  as.numeric(as.character(Labelled_data_top10_Time_melted_meanCI$variable))


#round data to 2 DPs
Labelled_data_top10_Time_melted_meanCI <-  round(Labelled_data_top10_Time_melted_meanCI, digits = 2)




####melt AvVel data grouping by ID for ggplot####


Labelled_data_AvVel_melted <- melt(Labelled_data_AvVel, id = c("ID", "Name", "Competition", "Date", "Location", "Class", "Age", "Phase"))


Labelled_data_top10_AvVel_melted <- melt(Labelled_data_top10_AvVel, id = c("ID", "Name", "Competition", "Date", "Location", "Class", "Age", "Phase")) 


#Make split column ("Variable") as numeric for plotting
Labelled_data_AvVel_melted$variable <-  as.numeric(as.character(Labelled_data_AvVel_melted$variable))
Labelled_data_top10_AvVel_melted$variable <-  as.numeric(as.character(Labelled_data_top10_AvVel_melted$variable))

#calculate mean and CI from melted data (group by split)
Labelled_data_top10_AvVel_melted_meanCI <- 
  Labelled_data_top10_AvVel_melted %>%
  group_by(variable)%>%
  dplyr::summarise(UpperLimit = CI(value, ci=0.95)[1],
                   Average = mean(value), 
                   LowerLimit = CI(value, ci=0.95)[3]) %>%
  mutate(variable = variable %>% as.factor())

#make sure the split variable is numeric
Labelled_data_top10_AvVel_melted_meanCI$variable <-  as.numeric(as.character(Labelled_data_top10_AvVel_melted_meanCI$variable))


#round data to 2 DPs
Labelled_data_top10_AvVel_melted_meanCI <-  round(Labelled_data_top10_AvVel_melted_meanCI, digits = 2)



####melt AvStkRate data grouping by ID for ggplot####


Labelled_data_AvStkRate_melted <- melt(Labelled_data_AvStkRate, id = c("ID", "Name", "Competition", "Date", "Location", "Class", "Age", "Phase"))


Labelled_data_top10_AvStkRate_melted <- melt(Labelled_data_top10_AvStkRate, id = c("ID", "Name", "Competition", "Date", "Location", "Class", "Age", "Phase")) 


#Make split column ("Variable") as numeric for plotting
Labelled_data_AvStkRate_melted$variable <-  as.numeric(as.character(Labelled_data_AvStkRate_melted$variable))
Labelled_data_top10_AvStkRate_melted$variable <-  as.numeric(as.character(Labelled_data_top10_AvStkRate_melted$variable))

#calculate mean and CI from melted data (group by split)
Labelled_data_top10_AvStkRate_melted_meanCI <- 
  Labelled_data_top10_AvStkRate_melted %>%
  group_by(variable)%>%
  dplyr::summarise(UpperLimit = CI(value, ci=0.95)[1],
                   Average = mean(value), 
                   LowerLimit = CI(value, ci=0.95)[3]) %>%
  mutate(variable = variable %>% as.factor())

#make sure the split variable is numeric
Labelled_data_top10_AvStkRate_melted_meanCI$variable <-  as.numeric(as.character(Labelled_data_top10_AvStkRate_melted_meanCI$variable))


#round data to 2 DPs
Labelled_data_top10_AvStkRate_melted_meanCI <-  round(Labelled_data_top10_AvStkRate_melted_meanCI, digits = 2)



####melt Strokes data grouping by ID for ggplot####


Labelled_data_Strokes_melted <- melt(Labelled_data_Strokes, id = c("ID", "Name", "Competition", "Date", "Location", "Class", "Age", "Phase"))


Labelled_data_top10_Strokes_melted <- melt(Labelled_data_top10_Strokes, id = c("ID", "Name", "Competition", "Date", "Location", "Class", "Age", "Phase")) 


#Make split column ("Variable") as numeric for plotting
Labelled_data_Strokes_melted$variable <-  as.numeric(as.character(Labelled_data_Strokes_melted$variable))
Labelled_data_top10_Strokes_melted$variable <-  as.numeric(as.character(Labelled_data_top10_Strokes_melted$variable))

#calculate mean and CI from melted data (group by split)
Labelled_data_top10_Strokes_melted_meanCI <- 
  Labelled_data_top10_Strokes_melted %>%
  group_by(variable)%>%
  dplyr::summarise(UpperLimit = CI(value, ci=0.95)[1],
                   Average = mean(value), 
                   LowerLimit = CI(value, ci=0.95)[3]) %>%
  mutate(variable = variable %>% as.factor())

#make sure the split variable is numeric
Labelled_data_top10_Strokes_melted_meanCI$variable <-  as.numeric(as.character(Labelled_data_top10_Strokes_melted_meanCI$variable))


#round data to 2 DPs
Labelled_data_top10_Strokes_melted_meanCI <-  round(Labelled_data_top10_Strokes_melted_meanCI, digits = 2)



####melt AccTime data grouping by ID for ggplot####


Labelled_data_AccTime_melted <- melt(Labelled_data_AccTime, id = c("ID", "Name", "Competition", "Date", "Location", "Class","Age", "Phase"))


Labelled_data_top10_AccTime_melted <- melt(Labelled_data_top10_AccTime, id = c("ID", "Name", "Competition", "Date", "Location", "Class", "Age", "Phase")) 


#Make split column ("Variable") as numeric for plotting
Labelled_data_AccTime_melted$variable <-  as.numeric(as.character(Labelled_data_AccTime_melted$variable))
Labelled_data_top10_AccTime_melted$variable <-  as.numeric(as.character(Labelled_data_top10_AccTime_melted$variable))

#calculate mean and CI from melted data (group by split)
Labelled_data_top10_AccTime_melted_meanCI <- 
  Labelled_data_top10_AccTime_melted %>%
  group_by(variable)%>%
  dplyr::summarise(UpperLimit = CI(value, ci=0.95)[1],
                   Average = mean(value), 
                   LowerLimit = CI(value, ci=0.95)[3]) %>%
  mutate(variable = variable %>% as.factor())

#make sure the split variable is numeric
Labelled_data_top10_AccTime_melted_meanCI$variable <-  as.numeric(as.character(Labelled_data_top10_AccTime_melted_meanCI$variable))


#round data to 2 DPs
Labelled_data_top10_AccTime_melted_meanCI <-  round(Labelled_data_top10_AccTime_melted_meanCI, digits = 2)





####melt AccTime data grouping by ID for ggplot####


Labelled_data_ProgSpeed_melted <- melt(Labelled_data_ProgSpeed, id = c("ID", "Name", "Competition", "Date", "Location", "Class","Age", "Phase"))


Labelled_data_top10_ProgSpeed_melted <- melt(Labelled_data_top10_ProgSpeed, id = c("ID", "Name", "Competition", "Date", "Location", "Class", "Age", "Phase")) 


#Make split column ("Variable") as numeric for plotting
Labelled_data_ProgSpeed_melted$variable <-  as.numeric(as.character(Labelled_data_ProgSpeed_melted$variable))
Labelled_data_top10_ProgSpeed_melted$variable <-  as.numeric(as.character(Labelled_data_top10_ProgSpeed_melted$variable))

#calculate mean and CI from melted data (group by split)
Labelled_data_top10_ProgSpeed_melted_meanCI <- 
  Labelled_data_top10_ProgSpeed_melted %>%
  group_by(variable)%>%
  dplyr::summarise(UpperLimit = CI(value, ci=0.95)[1],
                   Average = mean(value), 
                   LowerLimit = CI(value, ci=0.95)[3]) %>%
  mutate(variable = variable %>% as.factor())

#make sure the split variable is numeric
Labelled_data_top10_ProgSpeed_melted_meanCI$variable <-  as.numeric(as.character(Labelled_data_top10_ProgSpeed_melted_meanCI$variable))


#round data to 2 DPs
Labelled_data_top10_ProgSpeed_melted_meanCI <-  round(Labelled_data_top10_ProgSpeed_melted_meanCI, digits = 2)




#### Filtered data ####

data_Time_y <-  Labelled_data_Time_melted %>% filter(Name == "Horrie") %>% filter(Competition == "NationalChampionships-2019") %>% filter(Date=="2019.03.29") %>% filter(Phase=="Final")
data_AvVel_y <-  Labelled_data_AvVel_melted %>% filter(Name == "Horrie") %>% filter(Competition == "NationalChampionships-2019") %>% filter(Phase=="Final")
data_AvStkRate_y <-  Labelled_data_AvStkRate_melted %>% filter(Name == "Horrie") %>% filter(Competition == "NationalChampionships-2019") %>% filter(Phase=="Final")
data_Strokes_y <-  Labelled_data_Strokes_melted %>% filter(Name == "Horrie") %>% filter(Competition == "NationalChampionships-2019") %>% filter(Phase=="Final")
data_AccTime_y <-  Labelled_data_AccTime_melted %>% filter(Name == "Horrie") %>% filter(Competition == "NationalChampionships-2019") %>% filter(Phase=="Final")
data_ProgSpeed_y <-  Labelled_data_ProgSpeed_melted %>% filter(Name == "Horrie") %>% filter(Competition == "NationalChampionships-2019") %>% filter(Phase=="Final")


#### GGPLOT Time ####
windows()

ggplot(Labelled_data_top10_Time_melted_meanCI) + geom_line(aes(variable, Average), group=1) +   xlab("Split (m)") + 
  ylab("Time (sec)") + 
  scale_y_continuous(breaks = seq(0,100,len=21)) +
  geom_ribbon(aes(ymin=LowerLimit, ymax=UpperLimit, x=variable), alpha = 0.3) + geom_line(data=data_Time_y, aes(variable,value),colour = "red")


ggplot(Labelled_data_top10_AvVel_melted_meanCI) + geom_line(aes(variable, Average), group=1) +   xlab("Split (m)") + 
  ylab("Time (sec)") + 
  scale_y_continuous(breaks = seq(0,100,len=21)) +
  geom_ribbon(aes(ymin=LowerLimit, ymax=UpperLimit, x=variable), alpha = 0.3) + geom_line(data=data_AvVel_y, aes(variable,value),colour = "red")


ggplot(Labelled_data_top10_AvStkRate_melted_meanCI) + geom_line(aes(variable, Average), group=1) +   xlab("Split (m)") + 
  ylab("Time (sec)") + 
  scale_y_continuous(breaks = seq(0,100,len=21)) +
  geom_ribbon(aes(ymin=LowerLimit, ymax=UpperLimit, x=variable), alpha = 0.3) + geom_line(data=data_AvStkRate_y, aes(variable,value),colour = "red")


ggplot(Labelled_data_top10_Strokes_melted_meanCI) + geom_line(aes(variable, Average), group=1) +   xlab("Split (m)") + 
  ylab("Time (sec)") + 
  scale_y_continuous(breaks = seq(0,100,len=21)) +
  geom_ribbon(aes(ymin=LowerLimit, ymax=UpperLimit, x=variable), alpha = 0.3) + geom_line(data=data_Strokes_y, aes(variable,value),colour = "red")


ggplot(Labelled_data_top10_AccTime_melted_meanCI) + geom_line(aes(variable, Average), group=1) +   xlab("Split (m)") + 
  ylab("Time (sec)") + 
  scale_y_continuous(breaks = seq(0,100,len=21)) +
  geom_ribbon(aes(ymin=LowerLimit, ymax=UpperLimit, x=variable), alpha = 0.3) + geom_line(data=data_AccTime_y, aes(variable,value),colour = "red")

ggplot(Labelled_data_top10_ProgSpeed_melted_meanCI) + geom_line(aes(variable, Average), group=1) +   xlab("Split (m)") + 
  ylab("Prognostic(%)") + 
  geom_ribbon(aes(ymin=LowerLimit, ymax=UpperLimit, x=variable), alpha = 0.3) + geom_line(data=data_ProgSpeed_y, aes(variable,value, colour = "ProgSpeed")) +
  geom_line(data=data_AvStkRate_y, aes(variable,value*2,colour = "AvStkRate")) +
  scale_y_continuous(limits = c(60,110),sec.axis = sec_axis(trans = ~ . /2, name = "Stroke Rate (s/min)"))

par(mar = c(5, 5, 3, 5), xpd=TRUE)
plot(x = data_AvStkRate_y[,"variable"],y = data_AvStkRate_y[,"value"], type="l", ylab = "AvStkRate",
     main="Compare Plot", xlab="Distance", col = "blue")
par(new = TRUE)
plot(x = data_ProgSpeed_y[,"variable"],y = data_ProgSpeed_y[,"value"], type="l", xaxt = "n", yaxt = "n", ylab = "", xlab = "", col = "red", lty = 2)
axis(side = 4)
mtext("ProgSpeed", side = 4, line=3)
legend("bottom", c("AvStkRate", "ProgSpeed"),col=c("blue", "red"),lty=c(1,2))

library(plotly)
ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "Stroke Rate (s/min)",
  range = c(10,50)
)
by <- list(
  tickfont = list(color = "red"),
  side = "left",
  title = "Prognostic (%)", range = c(70,110)
)
fig <- plot_ly()
fig <- fig %>% add_lines(x = data_ProgSpeed_y[,"variable"], y = data_ProgSpeed_y[,"value"], name = "Prognostic %")
fig <- fig %>% add_lines(x = data_AvStkRate_y[,"variable"], y = data_AvStkRate_y[,"value"], name = "Stroke Rate (spm)", yaxis = "y2")
fig <- fig %>% layout(
  title = "Prognostic & Stroke Rate Profile", yaxis2 = ay,
  xaxis = list(title="Distance (m)"), yaxis = by, legend = list(x = 0.5, y=1), margin = list(l = 50, r = 50, b = 50, t = 50, pad = 2)
)

fig



#### Tables ####
#New xlabels
Xlabels <-  c(  "0m","25m", "50m", "75m", "100m", 
                "125m", "150m", "175m", "200m", 
                "225m", "250m", "275m", "300m", 
                "325m", "350m", "375m", "400m", 
                "425m", "450m", "475m", "500m")


##fancy table

#custom colours and new column labels
customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"

tablecolumns <-  c("ID",   "0m", "50m", "100m", "150m", "200m", 
                   "250m", "300m", "350m", "400m",
                   "450m", "500m", "550m", "600m",
                   "650m", "700m", "750m", "800m",
                   "850m", "900m", "950m", "1000m",
                   "1050m", "1100m", "1150m", "1200m",
                   "1250m", "1300m", "1350m", "1400m",
                   "1450m", "1500m", "1550m", "1600m",
                   "1650m", "1700m", "1750m", "1800m",
                   "1850m", "1900m", "1950m", "2000m")

#table


Tabledata <- column_to_rownames(Labelled_data_top10_Time_melted_meanCI,'variable')




Tabledata <-  data.frame(t(Tabledata))
Tabledata <-  rownames_to_column(Tabledata)
colnames(Tabledata) <-  tablecolumns

Filtereddata <-  data_Time_y %>% select(variable, value)
Filtereddata <- column_to_rownames(Filtereddata,'variable')
Filtereddata <-  data.frame(t(Filtereddata))
Filtereddata <-  rownames_to_column(Filtereddata)
Filtereddata[1,1] <- (levels(droplevels(data_Time_y$ID)))[1]
colnames(Filtereddata)<-  tablecolumns



Tabledata2 <-  cbind(Filtereddata, Tabledata)

Tabledata2 <-  bind_rows(Filtereddata, Tabledata)

#Tabledata <- rownames_to_column(Tabledata, var="data")

datatable(Tabledata2, rownames = NULL)
formattable(Tabledata2, align = c("l", "c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c"), 
            list("ID" = formatter("span", style = ~ style(color = "red", font.weight = "bold")), '25m' = color_tile(customRed,"white")))


### Splits Time calculations ####

Filtered_AccTime_data <-  data_AccTime_y %>% select(variable, value)
Filtered_AccTime_data <- column_to_rownames(Filtered_AccTime_data,'variable')
#250 splits

Split250_Time_250 <- Filtered_AccTime_data["250",]
Split250_Time_500 <- Filtered_AccTime_data["500",]-Filtered_AccTime_data["250",]
Split250_Time_750 <- Filtered_AccTime_data["750",]-Filtered_AccTime_data["500",]
Split250_Time_1000 <- Filtered_AccTime_data["1000",]-Filtered_AccTime_data["750",]
Split250_Time_1250 <- Filtered_AccTime_data["1250",]-Filtered_AccTime_data["1000",]
Split250_Time_1500 <- Filtered_AccTime_data["1500",]-Filtered_AccTime_data["1250",]
Split250_Time_1750 <- Filtered_AccTime_data["1750",]-Filtered_AccTime_data["1500",]
Split250_Time_2000 <- Filtered_AccTime_data["2000",]-Filtered_AccTime_data["1750",]

#500 Splits
Split500_Time_500 <- Split250_Time_250 + Split250_Time_500
Split500_Time_1000 <- Split250_Time_750 + Split250_Time_1000
Split500_Time_1500 <- Split250_Time_1250 + Split250_Time_1500
Split500_Time_2000 <- Split250_Time_1750 + Split250_Time_2000

#AvVel 250 splits

Split250_AvVel_250 <-  250/Split250_Time_250
Split250_AvVel_500 <-  250/Split250_Time_500
Split250_AvVel_750 <-  250/Split250_Time_750
Split250_AvVel_1000 <-  250/Split250_Time_1000
Split250_AvVel_1250 <-  250/Split250_Time_1250
Split250_AvVel_1500 <-  250/Split250_Time_1500
Split250_AvVel_1750 <-  250/Split250_Time_1750
Split250_AvVel_2000 <-  250/Split250_Time_2000


Split250_Time_250 <- sec_to_ms(Split250_Time_250)
Split250_Time_500 <- sec_to_ms(Split250_Time_500)
Split250_Time_750 <- sec_to_ms(Split250_Time_750)
Split250_Time_1000 <- sec_to_ms(Split250_Time_1000)
Split250_Time_1250 <- sec_to_ms(Split250_Time_1250)
Split250_Time_1500 <- sec_to_ms(Split250_Time_1500)
Split250_Time_1750 <- sec_to_ms(Split250_Time_1750)
Split250_Time_2000 <- sec_to_ms(Split250_Time_2000)

Split500_Time_500 <- sec_to_ms(Split500_Time_500)
Split500_Time_1000 <- sec_to_ms(Split500_Time_1000)
Split500_Time_1500 <- sec_to_ms(Split500_Time_1500)
Split500_Time_2000 <- sec_to_ms(Split500_Time_2000)

### Splits Stroke calculations ####
Filtered_AvStkRate_data <-  data_AvStkRate_y %>% select(variable, value)
Filtered_AvStkRate_data <- column_to_rownames(Filtered_AvStkRate_data,'variable')
#250 splits

Split250_AvStkRate_250 <- mean(Filtered_AvStkRate_data[2:6,])
Split250_AvStkRate_500 <- mean(Filtered_AvStkRate_data[7:11,])
Split250_AvStkRate_750 <- mean(Filtered_AvStkRate_data[12:16,])
Split250_AvStkRate_1000 <- mean(Filtered_AvStkRate_data[17:21,])
Split250_AvStkRate_1250 <- mean(Filtered_AvStkRate_data[22:26,])
Split250_AvStkRate_1500 <- mean(Filtered_AvStkRate_data[27:31,])
Split250_AvStkRate_1750 <- mean(Filtered_AvStkRate_data[32:36,])
Split250_AvStkRate_2000 <- mean(Filtered_AvStkRate_data[37:41,])


Filtered_AvProgSpeed_data <-  data_ProgSpeed_y %>% select(variable, value)
Filtered_AvProgSpeed_data <- column_to_rownames(Filtered_AvProgSpeed_data,'variable')

# 250 splits prog speed
Split250_AvProgSpeed_250 <- mean(Filtered_AvProgSpeed_data[2:6,])
Split250_AvProgSpeed_500 <- mean(Filtered_AvProgSpeed_data[7:11,])
Split250_AvProgSpeed_750 <- mean(Filtered_AvProgSpeed_data[12:16,])
Split250_AvProgSpeed_1000 <- mean(Filtered_AvProgSpeed_data[17:21,])
Split250_AvProgSpeed_1250 <- mean(Filtered_AvProgSpeed_data[22:26,])
Split250_AvProgSpeed_1500 <- mean(Filtered_AvProgSpeed_data[27:31,])
Split250_AvProgSpeed_1750 <- mean(Filtered_AvProgSpeed_data[32:36,])
Split250_AvProgSpeed_2000 <- mean(Filtered_AvProgSpeed_data[37:41,])


#500 splits


SummaryTable <-  data.table("Distance (m)" = c("250", "500", "750", "1000", "1250", "1500", "1750", "2000"), 
                            "250m splits (secs)" = c(Split250_Time_250, Split250_Time_500, Split250_Time_750, Split250_Time_1000, Split250_Time_1250, Split250_Time_1500, Split250_Time_1750, Split250_Time_2000),
                            "500m splits (secs)" = c("", Split500_Time_500, "", Split500_Time_1000, "", Split500_Time_1500, "", Split500_Time_2000),
                            "Avg Velocity (m/s)" = c(Split250_AvVel_250, Split250_AvVel_500, Split250_AvVel_750, Split250_AvVel_1000, Split250_AvVel_1250, Split250_AvVel_1500, Split250_AvVel_1750, Split250_AvVel_2000),
                            "Avg Stroke Rate (s/min)" = c(Split250_AvStkRate_250, Split250_AvStkRate_500, Split250_AvStkRate_750, Split250_AvStkRate_1000, Split250_AvStkRate_1250, Split250_AvStkRate_1500, Split250_AvStkRate_1750, Split250_AvStkRate_2000),
                            "Avg Prog Speed (%)" = c(Split250_AvProgSpeed_250, Split250_AvProgSpeed_500, Split250_AvProgSpeed_750, Split250_AvProgSpeed_1000, Split250_AvProgSpeed_1250, Split250_AvProgSpeed_1500, Split250_AvProgSpeed_1750, Split250_AvProgSpeed_2000))
                            