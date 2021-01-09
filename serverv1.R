server <- function(input, output) {
  #### Read data into a list if manual upload ####
  ## enable for manual file upload and delete code from Global
  #table1 <-  isolate(lapply(input$file1$datapath, fread, skip = 3, header=TRUE, stringsAsFactors=FALSE))
  
  
  # identify data labels #
  
  #dataname  <- input$file1[['name']]
  #dataname <-  str_remove_all(dataname, ".csv")
  #labels <-  t(data.frame(strsplit(dataname, "_")))
  #fullnames <-  data.frame(dataname, labels)
  
  #### Create zero row ####
  
  # zeros <-data.frame(0, 0, 0, 0, 0)
  # names(zeros) <- c("Time", "Dist", "AvVel", "AvStkRate", "#Strokes")
  # 
  # for (i in 1:length(table1)){
  #   table1[[i]] = rbind(zeros, table1[[i]])
  # }
  # 
  # #### Create Accumulated Time column ####
  # 
  # 
  # for (i in 1:length(table1)){
  #   table1[[i]][["AccTime"]] = cumsum(table1[[i]][["Time"]])
  # }
  # 
  # 
  # #### Create col names ####
  # col_names <- c(
  #   "ID", "Date", "Class", "Name", "Competition", "Location", "Age", "Phase", #1-8
  #   "Time_0m", #9
  #   "Time_50m", "Time_100m", "Time_150m", "Time_200m", 
  #   "Time_250m", "Time_300m", "Time_350m", "Time_400m", 
  #   "Time_450m", "Time_500m", "Time_550m", "Time_600m", 
  #   "Time_650m", "Time_700m", "Time_750m", "Time_800m", 
  #   "Time_850m", "Time_900m", "Time_950m", "Time_1000m", 
  #   "Time_1050m", "Time_1100m", "Time_1150m", "Time_1200m",
  #   "Time_1250m", "Time_1300m", "Time_1350m", "Time_1400m",
  #   "Time_1450m", "Time_1500m", "Time_1550m", "Time_1600m",
  #   "Time_1650m", "Time_1700m", "Time_1750m", "Time_1800m", 
  #   "Time_1850m", "Time_1900m", "Time_1950m", "Time_2000m", #49
  #   
  #   "AvVel_0m", #50
  #   "AvVel_50m", "AvVel_100m", "AvVel_150m", "AvVel_200m",  
  #   "AvVel_250m", "AvVel_300m", "AvVel_350m", "AvVel_400m",
  #   "AvVel_450m", "AvVel_500m", "AvVel_550m", "AvVel_600m",
  #   "AvVel_650m", "AvVel_700m", "AvVel_750m", "AvVel_800m",
  #   "AvVel_850m", "AvVel_900m", "AvVel_950m", "AvVel_1000m",
  #   "AvVel_1050m", "AvVel_1100m", "AvVel_1150m", "AvVel_1200m",
  #   "AvVel_1250m", "AvVel_1300m", "AvVel_1350m", "AvVel_1400m",
  #   "AvVel_1450m", "AvVel_1500m", "AvVel_1550m", "AvVel_1600m",
  #   "AvVel_1650m", "AvVel_1700m", "AvVel_1750m", "AvVel_1800m",
  #   "AvVel_1850m", "AvVel_1900m", "AvVel_1950m", "AvVel_2000m", # 90
  #   
  #   "AvStkRate_0m", #91
  #   "AvStkRate_50m", "AvStkRate_100m", "AvStkRate_150m", "AvStkRate_200m", 
  #   "AvStkRate_250m", "AvStkRate_300m", "AvStkRate_350m", "AvStkRate_400m",
  #   "AvStkRate_450m", "AvStkRate_500m", "AvStkRate_550m", "AvStkRate_600m",
  #   "AvStkRate_650m", "AvStkRate_700m", "AvStkRate_750m", "AvStkRate_800m",
  #   "AvStkRate_850m", "AvStkRate_900m", "AvStkRate_950m", "AvStkRate_1000m",
  #   "AvStkRate_1050m", "AvStkRate_1100m", "AvStkRate_1150m", "AvStkRate_1200m",
  #   "AvStkRate_1250m", "AvStkRate_1300m", "AvStkRate_1350m", "AvStkRate_1400m",
  #   "AvStkRate_1450m", "AvStkRate_1500m", "AvStkRate_1550m", "AvStkRate_1600m",
  #   "AvStkRate_1650m", "AvStkRate_1700m", "AvStkRate_1750m", "AvStkRate_1800m",
  #   "AvStkRate_1850m", "AvStkRate_1900m", "AvStkRate_1950m", "AvStkRate_2000m", #131
  #   
  #   "Strokes_0m", #132
  #   "Strokes_50m", "Strokes_100m", "Strokes_150m", "Strokes_200m", 
  #   "Strokes_250m", "Strokes_300m", "Strokes_350m", "Strokes_400m",
  #   "Strokes_450m", "Strokes_500m", "Strokes_550m", "Strokes_600m",
  #   "Strokes_650m", "Strokes_700m", "Strokes_750m", "Strokes_800m",
  #   "Strokes_850m", "Strokes_900m", "Strokes_950m", "Strokes_1000m",
  #   "Strokes_1050m", "Strokes_1100m", "Strokes_1150m", "Strokes_1200m",
  #   "Strokes_1250m", "Strokes_1300m", "Strokes_1350m", "Strokes_1400m",
  #   "Strokes_1450m", "Strokes_1500m", "Strokes_1550m", "Strokes_1600m",
  #   "Strokes_1650m", "Strokes_1700m", "Strokes_1750m", "Strokes_1800m",
  #   "Strokes_1850m", "Strokes_1900m", "Strokes_1950m", "Strokes_2000m", #172
  #   
  #   "AccTime_0m", #173
  #   "AccTime_50m", "AccTime_100m", "AccTime_150m", "AccTime_200m", 
  #   "AccTime_250m", "AccTime_300m", "AccTime_350m", "AccTime_400m",
  #   "AccTime_450m", "AccTime_500m", "AccTime_550m", "AccTime_600m",
  #   "AccTime_650m", "AccTime_700m", "AccTime_750m", "AccTime_800m",
  #   "AccTime_850m", "AccTime_900m", "AccTime_950m", "AccTime_1000m",
  #   "AccTime_1050m", "AccTime_1100m", "AccTime_1150m", "AccTime_1200m",
  #   "AccTime_1250m", "AccTime_1300m", "AccTime_1350m", "AccTime_1400m",
  #   "AccTime_1450m", "AccTime_1500m", "AccTime_1550m", "AccTime_1600m",
  #   "AccTime_1650m", "AccTime_1700m", "AccTime_1750m", "AccTime_1800m",
  #   "AccTime_1850m", "AccTime_1900m", "AccTime_1950m", "AccTime_2000m"#, #213
  #   
  # )
  # 
  # 
  # col_namesdist <- c(
  #   "ID", "Date", "Class", "Name", "Competition", "Location", "Age", "Phase",
  #   0, 50, 100, 150, 200, 
  #   250, 300, 350, 400,
  #   450, 500, 550, 600,
  #   650, 700, 750, 800,
  #   850, 900, 950, 1000,
  #   1050, 1100, 1150, 1200,
  #   1250, 1300, 1350, 1400,
  #   1450, 1500, 1550, 1600,
  #   1650, 1700, 1750, 1800,
  #   1850, 1900, 1950, 2000)
  # 
  # 
  # 
  # 
  # #### Transpose data to along columns of one row  and combine with labels ####
  # 
  # data_transposed = list()
  # for (i in 1:length(table1)){
  #   data2 = table1[[i]]
  #   data_transposed[[i]] <-  data.frame(t(data.frame(data2[,1])), t(data.frame(data2[,3])), t(data.frame(data2[,4])), t(data.frame(data2[,5])), t(data.frame(data2[,6])))
  # }
  # 
  # #combine all dataframes in list to one dataframe#
  # 
  # data1 <-  rbindlist(data_transposed, fill=TRUE)
  # 
  # # combine  labels and data into one dataframe #
  # 
  # 
  # Labelled_data <-  data.frame(fullnames, data1)
  # 
  # 
  # #define column and row names (data500 labels)
  # 
  # row_numbers <-  1:nrow(fullnames)
  # 
  # colnames(Labelled_data) <-  col_names
  # 
  # 
  # rownames(Labelled_data) <-  row_numbers
  # 
  # 
  # #### sort data by final time ####
  # Labelled_data <-  arrange(Labelled_data, AccTime_2000m)
  # 
  # Labelled_data_top10 <- Labelled_data[1:10,]
  # 
  # #### extract just time variable ####
  # 
  # 
  # Labelled_data_Time <-  Labelled_data[,1:49]
  # 
  # colnames(Labelled_data_Time) <-  col_namesdist
  # 
  # Labelled_data_top10_Time <-  Labelled_data_top10[,1:49]
  # 
  # colnames(Labelled_data_top10_Time) <-  col_namesdist
  # 
  # Labelled_data_Time$Class = as.character(Labelled_data_Time$Class)
  # Labelled_data_Time$Name = as.character(Labelled_data_Time$Name)
  # Labelled_data_Time$Competition = as.character(Labelled_data_Time$Competition)
  # Labelled_data_Time$Date = as.character(Labelled_data_Time$Date)
  # Labelled_data_Time$Phase = as.character(Labelled_data_Time$Phase)
  # 
  # #### extract just Av vel variable ####
  # 
  # cols <-c(1:8, 50:90)
  # Labelled_data_AvVel <-  Labelled_data[cols]
  # 
  # colnames(Labelled_data_AvVel) <-  col_namesdist
  # 
  # Labelled_data_top10_AvVel <-  Labelled_data_top10[cols]
  # 
  # colnames(Labelled_data_top10_AvVel) <-  col_namesdist
  # 
  # #### extract just AvStkRate variable ####
  # 
  # cols <-c(1:8, 91:131)
  # Labelled_data_AvStkRate <-  Labelled_data[cols]
  # 
  # colnames(Labelled_data_AvStkRate) <-  col_namesdist
  # 
  # Labelled_data_top10_AvStkRate <-  Labelled_data_top10[cols]
  # 
  # colnames(Labelled_data_top10_AvStkRate) <-  col_namesdist
  # 
  # #### extract just Strokes variable ####
  # 
  # cols <-c(1:8, 132:172)
  # Labelled_data_Strokes <-  Labelled_data[cols]
  # 
  # colnames(Labelled_data_Strokes) <-  col_namesdist
  # 
  # Labelled_data_top10_Strokes <-  Labelled_data_top10[cols]
  # 
  # colnames(Labelled_data_top10_Strokes) <-  col_namesdist
  # 
  # #### extract just AccTime variable ####
  # 
  # 
  # cols <-c(1:8, 173:213)
  # Labelled_data_AccTime <-  Labelled_data[cols]
  # 
  # colnames(Labelled_data_AccTime) <-  col_namesdist
  # 
  # Labelled_data_top10_AccTime <-  Labelled_data_top10[cols]
  # 
  # colnames(Labelled_data_top10_AccTime) <-  col_namesdist
  
  
  ##################################################################################################
  #### Filter data based on dropdowns - Time Race 1 ####
  
  
  
  tab_Time_R1 <-  reactive({
    
    Labelled_data_Time %>%
      melt(id = c("ID", "Date", "Class", "Name", "Competition", "Location", "Age", "Phase"))%>%
      filter(Class == input$Class) %>% 
      filter(Name == input$Name) %>% 
      filter(Competition == input$Competition) %>% 
      filter(Date == input$Date) %>% 
      filter(Phase == input$Phase) %>%
      mutate(variable = as.numeric(as.character(variable)))
  }) 
  
  
  tab2_Time <-  reactive({
    Labelled_data_Time %>%
      filter(Class == input$Class) %>%
      arrange_at(ncol(.)) %>%
      head(10) %>%
      melt(id = c("ID", "Date", "Class", "Name", "Competition", "Location", "Age", "Phase"))%>%
      group_by(variable) %>%
      dplyr::summarise(UpperLimit = CI(value, ci=0.95)[1],
                       Average = mean(value),
                       LowerLimit = CI(value, ci=0.95)[3]) %>%
      mutate(variable = as.numeric(as.character(variable))) %>%
      mutate(Average = round(Average, digits = 2)) %>%
      mutate(UpperLimit = round(UpperLimit, digits = 2))%>%
      mutate(LowerLimit = round(LowerLimit, digits = 2))
    
  })
  
  
  
  
  output$select_Class <-  renderUI({
    
    selectizeInput('Class', 'Select Class', choices = c("select" = "", unique(Labelled_data_Time$Class)))  
  }) 
  
  output$select_Name <-  renderUI({
    inputClass = as.character(input$Class)
    choice_Name <- reactive({
      Labelled_data_Time %>% 
        filter(Class == inputClass) %>% 
        pull(Name) %>% 
        as.character()
      
      
    })
    
    
    selectizeInput('Name', 'Select Race 1 Name', choices = c("select" = "", choice_Name()))  
  })         
  
  
  output$select_Competition <-  renderUI({
    inputClass = as.character(input$Class)
    inputName = as.character(input$Name)
    choice_Competition <- reactive({
      Labelled_data_Time %>% 
        filter(Class == inputClass) %>% 
        filter(Name == inputName) %>% 
        pull(Competition) %>% 
        as.character()
      
      
    })
    
    
    selectizeInput('Competition', 'Select Race 1 Competition', choices = c("select" = "", choice_Competition())) 
    
  })
  
  output$select_Date <-  renderUI({
    inputClass = as.character(input$Class)
    inputName = as.character(input$Name)
    inputCompetition = as.character(input$Competition)        
    choice_Date <- reactive({
      Labelled_data_Time %>% 
        filter(Class == inputClass) %>% 
        filter(Name == inputName) %>% 
        filter(Competition == inputCompetition) %>%             
        pull(Date) %>% 
        as.character()
      
      
    })
    
    
    selectizeInput('Date', 'Select Race 1 Date', choices = c("select" = "", choice_Date()))  
  })
  
  output$select_Phase <-  renderUI({
    inputClass = as.character(input$Class)
    inputName = as.character(input$Name)
    inputCompetition = as.character(input$Competition)
    inputDate = as.character(input$Date)
    choice_Phase <- reactive({
      Labelled_data_Time %>% 
        filter(Class == inputClass) %>% 
        filter(Name == inputName) %>% 
        filter(Competition == inputCompetition) %>% 
        filter(Date == Date) %>% 
        pull(Phase) %>% 
        as.character()
      
      
    })
    
    
    selectizeInput('Phase', 'Select Race 1 Phase', choices = c("select" = "", choice_Phase()))  
  })     
  
  
  
  #### Filtering based on dropdowns - AvVel Race 1 ####
  
  tab_AvVel_R1 <-  reactive({
    ClassProgSpeed <<-  Prognostic.Velocities[ which(Prognostic.Velocities$Boat.Class==input$Class),'Prog.Speed']
    Labelled_data_AvVel %>%
      melt(id = c("ID", "Date", "Class", "Name", "Competition", "Location", "Age", "Phase"))%>%
      filter(Class == input$Class) %>% 
      filter(Name == input$Name) %>% 
      filter(Competition == input$Competition) %>% 
      filter(Date == input$Date) %>% 
      filter(Phase == input$Phase) %>%
      mutate(variable = as.numeric(as.character(variable))) %>%
      mutate(ProgSpeed = value/ClassProgSpeed*100)
  }) 
  
  
  tab2_AvVel <-  reactive({
    ClassProgSpeed = Prognostic.Velocities[ which(Prognostic.Velocities$Boat.Class==input$Class),'Prog.Speed']
    Labelled_data_AvVel %>%
      filter(Class == input$Class) %>%
      arrange_at(ncol(.)) %>%
      head(10) %>%
      melt(id = c("ID", "Date", "Class", "Name", "Competition", "Location", "Age", "Phase"))%>%
      group_by(variable) %>%
      dplyr::summarise(UpperLimit = CI(value, ci=0.95)[1],
                       Average = mean(value),
                       LowerLimit = CI(value, ci=0.95)[3]) %>%
      mutate(variable = as.numeric(as.character(variable))) %>%
      mutate(Average = round(Average, digits = 2)) %>%
      mutate(UpperLimit = round(UpperLimit, digits = 2))%>%
      mutate(LowerLimit = round(LowerLimit, digits = 2))%>%
      mutate(ProgSpeedUpperLimit = UpperLimit/ClassProgSpeed*100) %>%
      mutate(ProgSpeedAverage = Average/ClassProgSpeed*100)%>%
      mutate(ProgSpeedLowerLimit = LowerLimit/ClassProgSpeed*100)
    
  })
  
  
  
  
  #### Filtering based on dropdowns - AvStkRate Race 1 ####
  
  tab_AvStkRate_R1 <-  reactive({
    
    Labelled_data_AvStkRate %>%
      melt(id = c("ID", "Date", "Class", "Name", "Competition", "Location", "Age", "Phase"))%>%
      filter(Class == input$Class) %>% 
      filter(Name == input$Name) %>% 
      filter(Competition == input$Competition) %>% 
      filter(Date == input$Date) %>% 
      filter(Phase == input$Phase) %>%
      mutate(variable = as.numeric(as.character(variable)))
  }) 
  
  
  tab2_AvStkRate <-  reactive({
    Labelled_data_AvStkRate %>%
      filter(Class == input$Class) %>%
      arrange_at(ncol(.)) %>%
      head(10) %>%
      melt(id = c("ID", "Date", "Class", "Name", "Competition", "Location", "Age", "Phase"))%>%
      group_by(variable) %>%
      dplyr::summarise(UpperLimit = CI(value, ci=0.95)[1],
                       Average = mean(value),
                       LowerLimit = CI(value, ci=0.95)[3]) %>%
      mutate(variable = as.numeric(as.character(variable))) %>%
      mutate(Average = round(Average, digits = 2)) %>%
      mutate(UpperLimit = round(UpperLimit, digits = 2))%>%
      mutate(LowerLimit = round(LowerLimit, digits = 2))
    
    
  })
  
  
  
  
  #### Filtering based on dropdowns - Strokes Race 1 ####
  
  tab_Strokes_R1 <-  reactive({
    
    Labelled_data_Strokes %>%
      melt(id = c("ID", "Date", "Class", "Name", "Competition", "Location", "Age", "Phase"))%>%
      filter(Class == input$Class) %>% 
      filter(Name == input$Name) %>% 
      filter(Competition == input$Competition) %>% 
      filter(Date == input$Date) %>% 
      filter(Phase == input$Phase) %>%
      mutate(variable = as.numeric(as.character(variable)))
  }) 
  
  
  tab2_Strokes <-  reactive({
    Labelled_data_Strokes %>%
      filter(Class == input$Class) %>%
      arrange_at(ncol(.)) %>%
      head(10) %>%
      melt(id = c("ID", "Date", "Class", "Name", "Competition", "Location", "Age", "Phase"))%>%
      group_by(variable) %>%
      dplyr::summarise(UpperLimit = CI(value, ci=0.95)[1],
                       Average = mean(value),
                       LowerLimit = CI(value, ci=0.95)[3]) %>%
      mutate(variable = as.numeric(as.character(variable))) %>%
      mutate(Average = round(Average, digits = 2)) %>%
      mutate(UpperLimit = round(UpperLimit, digits = 2))%>%
      mutate(LowerLimit = round(LowerLimit, digits = 2))
    
  })
  
  
  
  #### Filtering based on dropdowns - AccTime Race 1 ####
  
  tab_AccTime_R1 <-  reactive({
    
    Labelled_data_AccTime %>%
      melt(id = c("ID", "Date", "Class", "Name", "Competition", "Location", "Age", "Phase"))%>%
      filter(Class == input$Class) %>% 
      filter(Name == input$Name) %>% 
      filter(Competition == input$Competition) %>% 
      filter(Date == input$Date) %>% 
      filter(Phase == input$Phase) %>%
      mutate(variable = as.numeric(as.character(variable)))
  }) 
  
  
  tab2_AccTime <-  reactive({
    Labelled_data_AccTime %>%
      filter(Class == input$Class) %>%
      arrange_at(ncol(.)) %>%
      head(10) %>%
      melt(id = c("ID", "Date", "Class", "Name", "Competition", "Location", "Age", "Phase"))%>%
      group_by(variable) %>%
      dplyr::summarise(UpperLimit = CI(value, ci=0.95)[1],
                       Average = mean(value),
                       LowerLimit = CI(value, ci=0.95)[3]) %>%
      mutate(variable = as.numeric(as.character(variable))) %>%
      mutate(Average = round(Average, digits = 2)) %>%
      mutate(UpperLimit = round(UpperLimit, digits = 2))%>%
      mutate(LowerLimit = round(LowerLimit, digits = 2))
    
  })
  
  
  
  
  
  
  ###################################################################################################
  #### Filtering based on dropdowns - Time Race 2 ####
  
  
  
  tab_Time_R2 <-  reactive({
    
    Labelled_data_Time %>%
      melt(id = c("ID", "Date", "Class", "Name", "Competition", "Location", "Age", "Phase"))%>%
      filter(Class == input$Class) %>% 
      filter(Name == input$Name2) %>% 
      filter(Competition == input$Competition2) %>% 
      filter(Date == input$Date2) %>% 
      filter(Phase == input$Phase2) %>%
      mutate(variable = as.numeric(as.character(variable)))
  }) 
  
  
  
  output$select_Class <-  renderUI({
    selectizeInput('Class', 'Select Class', choices = c("select" = "", unique(Labelled_data_Time$Class)))
  })
  
  output$select_Name2 <-  renderUI({
    inputClass = as.character(input$Class)
    choice_Name2 <- reactive({
      Labelled_data_Time %>% 
        filter(Class == inputClass) %>% 
        pull(Name) %>% 
        as.character()
      
      
    })
    
    if (input$Report_Type == "Two Races"){
      
      selectizeInput('Name2', 'Select Race 2 Name', choices = c("select" = "", choice_Name2()))  }
  })         
  
  
  output$select_Competition2 <-  renderUI({
    inputClass = as.character(input$Class)
    inputName2 = as.character(input$Name2)
    choice_Competition2 <- reactive({
      Labelled_data_Time %>% 
        filter(Class == inputClass) %>% 
        filter(Name == inputName2) %>% 
        pull(Competition) %>% 
        as.character()
      
      
    })
    
    if (input$Report_Type == "Two Races"){
      
      selectizeInput('Competition2', 'Select Race 2 Competition', choices = c("select" = "", choice_Competition2())) }
    
  })
  
  output$select_Date2 <-  renderUI({
    inputClass = as.character(input$Class)
    inputName2 = as.character(input$Name2)
    inputCompetition2 = as.character(input$Competition2)        
    choice_Date2 <- reactive({
      Labelled_data_Time %>% 
        filter(Class == inputClass) %>% 
        filter(Name == inputName2) %>% 
        filter(Competition == inputCompetition2) %>%             
        pull(Date) %>% 
        as.character()
      
      
    })
    if (input$Report_Type == "Two Races"){
      
      
      selectizeInput('Date2', 'Select Race 2 Date', choices = c("select" = "", choice_Date2()))  }
  })
  
  output$select_Phase2 <-  renderUI({
    inputClass = as.character(input$Class)
    inputName2 = as.character(input$Name2)
    inputCompetition2 = as.character(input$Competition2)
    inputDate2 = as.character(input$Date2)
    choice_Phase2 <- reactive({
      Labelled_data_Time %>% 
        filter(Class == inputClass) %>% 
        filter(Name == inputName2) %>% 
        filter(Competition == inputCompetition2) %>% 
        filter(Date == inputDate2) %>% 
        pull(Phase) %>% 
        as.character()
      
      
    })
    
    if (input$Report_Type == "Two Races"){
      
      selectizeInput('Phase2', 'Select Race 2 Phase', choices = c("select" = "", choice_Phase2()))  }
  })     
  
  
  
  #### Filtering based on dropdowns - AvVel Race 2 ####
  
  tab_AvVel_R2 <-  reactive({
    ClassProgSpeed <-  Prognostic.Velocities[ which(Prognostic.Velocities$Boat.Class==input$Class),'Prog.Speed']
    Labelled_data_AvVel %>%
      melt(id = c("ID", "Date", "Class", "Name", "Competition", "Location", "Age", "Phase"))%>%
      filter(Class == input$Class) %>% 
      filter(Name == input$Name2) %>% 
      filter(Competition == input$Competition2) %>% 
      filter(Date == input$Date2) %>% 
      filter(Phase == input$Phase2) %>%
      mutate(variable = as.numeric(as.character(variable))) %>%
      mutate(ProgSpeed = value/ClassProgSpeed*100)
  }) 
  
  
  
  
  #### Filtering based on dropdowns - AvStkRate Race 2 ####
  
  tab_AvStkRate_R2 <-  reactive({
    
    Labelled_data_AvStkRate %>%
      melt(id = c("ID", "Date", "Class", "Name", "Competition", "Location", "Age", "Phase"))%>%
      filter(Class == input$Class) %>% 
      filter(Name == input$Name2) %>% 
      filter(Competition == input$Competition2) %>% 
      filter(Date == input$Date2) %>% 
      filter(Phase == input$Phase2) %>%
      mutate(variable = as.numeric(as.character(variable)))
  }) 
  
  
  
  
  #### Filtering based on dropdowns - Strokes Race 2 ####
  
  tab_Strokes_R2 <-  reactive({
    
    Labelled_data_Strokes %>%
      melt(id = c("ID", "Date", "Class", "Name", "Competition", "Location", "Age", "Phase"))%>%
      filter(Class == input$Class) %>% 
      filter(Name == input$Name2) %>% 
      filter(Competition == input$Competition2) %>% 
      filter(Date == input$Date2) %>% 
      filter(Phase == input$Phase2) %>%
      mutate(variable = as.numeric(as.character(variable)))
  }) 
  
  
  
  
  #### Filtering based on dropdowns - AccTime Race 2 ####
  
  tab_AccTime_R2 <-  reactive({
    
    Labelled_data_AccTime %>%
      melt(id = c("ID", "Date", "Class", "Name", "Competition", "Location", "Age", "Phase"))%>%
      filter(Class == input$Class) %>% 
      filter(Name == input$Name2) %>% 
      filter(Competition == input$Competition2) %>% 
      filter(Date == input$Date2) %>% 
      filter(Phase == input$Phase2) %>%
      mutate(variable = as.numeric(as.character(variable)))
  }) 
  
  
  
  
  #################################################################
  # #### Output GGplot - Time ####
  # output$ggplot_Time <-  renderPlot({
  #   ggplot(tab2_Time()) + geom_line(aes(variable, Average), group=1) +   xlab("Split (m)") +
  #     ylab("Time (sec)") +
  #     geom_ribbon(aes(ymin=LowerLimit, ymax=UpperLimit, x=variable), alpha = 0.3) + geom_line(data=tab_Time(), aes(variable,value),colour = "red")
  # })
  
  
  
  
  #### output ProgSpeed and Av Stk Rate ####
  output$plot_ProgSpeed_AvStkRate <-  renderPlotly({
    input$goButton
    isolate(if (input$Report_Type == "Single Race"){
      
      
      ay <- list(
        overlaying = "y",
        side = "right",
        title = "Stroke Rate (s/min)",
        range = c(10,50)
      )
      by <- list(
        side = "left",
        title = "Prognostic (%)", range = c(70,110)
      )
      
      fig <- plot_ly()
      fig <- fig %>% add_lines(x = tab_AvVel_R1()[,"variable"], y = tab_AvVel_R1()[,"ProgSpeed"], name = "Prognostic %", line = list(color='rgb(22, 96, 167)'))
      fig <- fig %>% add_lines(x = tab_AvStkRate_R1()[,"variable"], y = tab_AvStkRate_R1()[,"value"], name = "Stroke Rate (spm)", yaxis = "y2", line = list(color = 'rgb(205, 12, 24)'))
      
      fig <- fig %>% layout(
        title = "Prognostic & Stroke Rate Profile", yaxis2 = ay,
        xaxis = list(title="Distance (m)"), yaxis = by, legend = list(x = 0.5, y=1), margin = list(l = 50, r = 50, b = 50, t = 50, pad = 2)
      )
      
      fig
      
      
      
    }else if (input$Report_Type == "Two Races"){
      
      ay <- list(
        overlaying = "y",
        side = "right",
        title = "Stroke Rate (s/min)",
        range = c(10,50)
      )
      by <- list(
        side = "left",
        title = "Prognostic (%)", range = c(70,110)
      )
      
      
      fig <- plot_ly()
      fig <- fig %>% add_lines(x = tab_AvVel_R1()[,"variable"], y = tab_AvVel_R1()[,"ProgSpeed"], name = "Race 1 Prognostic %", line = list(color='rgb(22, 96, 167)'))
      fig <- fig %>% add_lines(x = tab_AvStkRate_R1()[,"variable"], y = tab_AvStkRate_R1()[,"value"], name = "Race 1 Stroke Rate (spm)", yaxis = "y2", line = list(color = 'rgb(205, 12, 24)'))
      
      fig <- fig %>% add_lines(x = tab_AvVel_R2()[,"variable"], y = tab_AvVel_R2()[,"ProgSpeed"], name = "Race 2 Prognostic %", line = list(color='rgb(22, 96, 167)', dash = "dot"))
      fig <- fig %>% add_lines(x = tab_AvStkRate_R2()[,"variable"], y = tab_AvStkRate_R2()[,"value"], name = "Race 2 Stroke Rate (spm)", yaxis = "y2", line = list(color = 'rgb(205, 12, 24)', dash = "dot"))
      
      fig <- fig %>% layout(
        title = "Prognostic & Stroke Rate Profile", yaxis2 = ay,
        xaxis = list(title="Distance (m)"), yaxis = by, legend = list(x = 0.5, y=1), margin = list(l = 50, r = 50, b = 50, t = 50, pad = 2)
      )
      
      fig
      
      
    }else if (input$Report_Type == "vs Top 10"){
      
      ay <- list(
        overlaying = "y",
        side = "right",
        title = "Stroke Rate (s/min)",
        range = c(10,50)
      )
      by <- list(
        side = "left",
        title = "Prognostic (%)", range = c(70,110)
      )
      x <-  tab2_AvVel()[["variable"]]
      y <-  tab2_AvVel() [["ProgSpeedAverage"]]
      x2 <-  tab2_AvStkRate()[["variable"]]
      y2 <-  tab2_AvStkRate()[["Average"]]
      
      fig <- plot_ly()
      fig <- fig %>% add_lines(x = tab_AvVel_R1()[,"variable"], y = tab_AvVel_R1()[,"ProgSpeed"], name = "Prognostic %", line = list(color='rgb(22, 96, 167)'))
      fig <- fig %>% add_lines(x = tab_AvStkRate_R1()[,"variable"], y = tab_AvStkRate_R1()[,"value"], name = "Stroke Rate (spm)", yaxis = "y2", line = list(color = 'rgb(205, 12, 24)'))
      
      fig <- fig %>% add_lines(x = x, y = y, name = "Top 10 Prognostic %", line = list(color = 'rgb(22, 96, 167)', dash = "dot"))
      fig <- fig %>% add_lines(x = x2, y = y2, name = "Top 10 Stroke Rate (spm)", yaxis = "y2", line = list(color = 'rgb(205, 12, 24)', dash = "dot"))
      
      fig <- fig %>% layout(
        title = "Prognostic & Stroke Rate Profile", yaxis2 = ay,
        xaxis = list(title="Distance (m)"), yaxis = by, legend = list(x = 0.5, y=1), margin = list(l = 50, r = 50, b = 50, t = 50, pad = 2)
      )
      
      fig
      
      
      
    }
    )
  })
  
  
  
  
  
  
  #### output Scatter SR vs Progspeed  ####
  output$plot_scatter_SR_Progspeed <-  renderPlot({
    
    
    input$goButton
    isolate(if (input$Report_Type == "Single Race"){
      plotdata <-  data.frame(tab_AvStkRate_R1()[["value"]], tab_AvVel_R1()[["ProgSpeed"]])
      names(plotdata)[1]<-paste("StrokeRate")
      names(plotdata)[2]<-paste("ProgSpeed")
      
      pointsA = geom_point(colour="red")
      scalex = scale_x_continuous(breaks = seq(30, 44, by = 2), limits = c(30,44))
      scaley = scale_y_continuous(breaks = seq(70, 110, by = 5), limits = c(70,110))
      lineA = geom_smooth(aes(colour = 'Race 1'), method=lm, se=FALSE, fullrange=TRUE)
      Legend = theme(legend.title = element_blank())
      ggplot(plotdata,(aes(x=StrokeRate, y=ProgSpeed))) + pointsA + scalex + scaley + lineA + Legend
      
      
    }else if(input$Report_Type == "Two Races"){
      plotdata <-  data.frame(tab_AvStkRate_R1()[["value"]], tab_AvVel_R1()[["ProgSpeed"]])
      names(plotdata)[1]<-paste("StrokeRate")
      names(plotdata)[2]<-paste("ProgSpeed")
      plotdata2 <-  data.frame(tab_AvStkRate_R2()[["value"]], tab_AvVel_R2()[["ProgSpeed"]])
      names(plotdata2)[1]<-paste("StrokeRate")
      names(plotdata2)[2]<-paste("ProgSpeed")
      
      
      pointsA = geom_point(colour="red")
      scalex = scale_x_continuous(breaks = seq(30, 44, by = 2), limits = c(30,44))
      scaley = scale_y_continuous(breaks = seq(70, 110, by = 5), limits = c(70,110))
      lineA = geom_smooth(aes(colour = 'Race 1'), method=lm, se=FALSE, fullrange=TRUE)
      pointsB = geom_point(data = plotdata2, aes(x=StrokeRate, y=ProgSpeed), colour="blue")
      lineB = geom_smooth(data = plotdata2, aes(x=StrokeRate, y=ProgSpeed, colour = 'Race 2'), method=lm, se=FALSE, fullrange=TRUE)
      Legend = theme(legend.title = element_blank())
      ggplot(plotdata,(aes(x=StrokeRate, y=ProgSpeed))) + pointsA + scalex + scaley + lineA + pointsB + lineB + Legend
      
      
      
    } else if (input$Report_Type == "vs Top 10"){
      
      plotdata <-  data.frame(tab_AvStkRate_R1()[["value"]], tab_AvVel_R1()[["ProgSpeed"]])
      names(plotdata)[1]<-paste("StrokeRate")
      names(plotdata)[2]<-paste("ProgSpeed")
      plotdata2 <-  data.frame(tab2_AvStkRate()[["Average"]], tab2_AvVel()[["ProgSpeedAverage"]])
      names(plotdata2)[1]<-paste("StrokeRate")
      names(plotdata2)[2]<-paste("ProgSpeed")
      
      
      pointsA = geom_point(colour="red")
      scalex = scale_x_continuous(breaks = seq(30, 44, by = 2), limits = c(30,44))
      scaley = scale_y_continuous(breaks = seq(70, 110, by = 5), limits = c(70,110))
      lineA = geom_smooth(aes(colour = 'Race 1'), method=lm, se=FALSE, fullrange=TRUE)
      pointsB = geom_point(data = plotdata2, aes(x=StrokeRate, y=ProgSpeed), colour="blue")
      lineB = geom_smooth(data = plotdata2, aes(x=StrokeRate, y=ProgSpeed, colour = 'Top 10'), method=lm, se=FALSE, fullrange=TRUE)
      Legend = theme(legend.title = element_blank())
      ggplot(plotdata,(aes(x=StrokeRate, y=ProgSpeed))) + pointsA + scalex + scaley + lineA + pointsB + lineB + Legend
      
      
      
    }
    )
  })
  
  
  
  
  
  
  #    ####    Output Info table ####
  output$RaceSummary <-  DT::renderDataTable({
    input$goButton
    isolate(if (input$Report_Type == "Single Race"){
      #Race 1
      Filtered_AccTime_data <-  tab_AccTime_R1() %>% select(variable, value)
      Filtered_AccTime_data <- column_to_rownames(Filtered_AccTime_data,'variable') 
      FinishTime <- Filtered_AccTime_data["2000",]
      PrognosticTime <- 2000/ClassProgSpeed
      Difference <-  round(PrognosticTime/FinishTime*100,2)
      FinishTime <- sec_to_ms(FinishTime)
      PrognosticTime <- sec_to_ms(PrognosticTime)
      AvgVel = tab_AvVel_R1() %>% summarize(Avg_Velocity = mean(value, na.rm = TRUE)) %>% round(2)
      ClassProgSpeed2 = round(ClassProgSpeed,2)
      VelDifference = round(AvgVel/ClassProgSpeed*100,2)
      
      
      Race_1 = data.frame(FinishTime, PrognosticTime, Difference, AvgVel, ClassProgSpeed2, VelDifference)
      colnames(Race_1) = c("Finish Time", "Prognostic Time", "% Difference", "Average Velocity (m/s)", "Prognostic Velocity (m/s)", "% Difference")
      
      
      ({datatable(Race_1, rownames = c("Race 1"), options = list(dom='t', ordering=F, scrollX = TRUE))})
      
    } else if (input$Report_Type == "Two Races"){
      #Race 1
      Filtered_AccTime_data <-  tab_AccTime_R1() %>% select(variable, value)
      Filtered_AccTime_data <- column_to_rownames(Filtered_AccTime_data,'variable') 
      FinishTime <- Filtered_AccTime_data["2000",]
      PrognosticTime <- 2000/ClassProgSpeed
      Difference <-  round(PrognosticTime/FinishTime*100,2)
      FinishTime <- sec_to_ms(FinishTime)
      PrognosticTime <- sec_to_ms(PrognosticTime)
      AvgVel = tab_AvVel_R1() %>% summarize(Avg_Velocity = mean(value, na.rm = TRUE)) %>% round(2)
      ClassProgSpeed2 = round(ClassProgSpeed,2)
      VelDifference = round(AvgVel/ClassProgSpeed*100,2)
      
      #Race 2
      Filtered_AccTime_data_R2 <-  tab_AccTime_R2() %>% select(variable, value)
      Filtered_AccTime_data_R2 <- column_to_rownames(Filtered_AccTime_data_R2,'variable') 
      FinishTime_R2 <- Filtered_AccTime_data_R2["2000",]
      PrognosticTime_R2 <- 2000/ClassProgSpeed
      Difference_R2 <-  round(PrognosticTime_R2/FinishTime_R2*100,2)
      FinishTime_R2 <- sec_to_ms(FinishTime_R2)
      PrognosticTime_R2 <- sec_to_ms(PrognosticTime_R2)
      AvgVel_R2 = tab_AvVel_R2() %>% summarize(Avg_Velocity_R2 = mean(value, na.rm = TRUE)) %>% round(2)
      ClassProgSpeed2_R2 = round(ClassProgSpeed,2)
      VelDifference_R2 = round(AvgVel_R2/ClassProgSpeed*100,2)
      
      
      
      Race_1 = data.frame(FinishTime, PrognosticTime, Difference, AvgVel, ClassProgSpeed2, VelDifference)
      colnames(Race_1) = c("Finish Time", "Prognostic Time", "% Difference", "Average Velocity (m/s)", "Prognostic Velocity (m/s)", "% Difference")
      
      Race_2 = data.frame(FinishTime_R2, PrognosticTime_R2, Difference_R2, AvgVel_R2, ClassProgSpeed2_R2, VelDifference_R2)
      colnames(Race_2) = c("Finish Time", "Prognostic Time", "% Difference", "Average Velocity (m/s)", "Prognostic Velocity (m/s)", "% Difference")
      
      RaceSummary <<-  rbind(Race_1, Race_2)
      ({datatable(RaceSummary, rownames = c("Race 1", "Race 2"), options = list(dom='t', ordering=F, scrollX = TRUE))})
      
    }  else if (input$Report_Type == "vs Top 10"){
      
      #Race 1
      Filtered_AccTime_data <-  tab_AccTime_R1() %>% select(variable, value)
      Filtered_AccTime_data <- column_to_rownames(Filtered_AccTime_data,'variable') 
      FinishTime <- Filtered_AccTime_data["2000",]
      PrognosticTime <- 2000/ClassProgSpeed
      Difference <-  round(PrognosticTime/FinishTime*100,2)
      FinishTime <- sec_to_ms(FinishTime)
      PrognosticTime <- sec_to_ms(PrognosticTime)
      AvgVel = tab_AvVel_R1() %>% summarize(Avg_Velocity = mean(value, na.rm = TRUE)) %>% round(2)
      ClassProgSpeed2 = round(ClassProgSpeed,2)
      VelDifference = round(AvgVel/ClassProgSpeed*100,2)
      
      #Race 2
      Filtered_AccTime_data_top10 <-  tab2_AccTime() %>% select(variable, Average)
      Filtered_AccTime_data_top10 <- column_to_rownames(Filtered_AccTime_data_top10,'variable') 
      FinishTime_top10 <- Filtered_AccTime_data_top10["2000",]
      PrognosticTime_top10 <- 2000/ClassProgSpeed
      Difference_top10 <-  round(PrognosticTime_top10/FinishTime_top10*100,2)
      FinishTime_top10 <- sec_to_ms(FinishTime_top10)
      PrognosticTime_top10 <- sec_to_ms(PrognosticTime_top10)
      AvgVel_top10 = tab2_AvVel() %>% summarize(Avg_Velocity_top10 = mean(Average, na.rm = TRUE)) %>% round(2)
      ClassProgSpeed2_top10 = round(ClassProgSpeed,2)
      VelDifference_top10 = round(AvgVel_top10/ClassProgSpeed*100,2)
      
      
      
      Race_1 = data.frame(FinishTime, PrognosticTime, Difference, AvgVel, ClassProgSpeed2, VelDifference)
      colnames(Race_1) = c("Finish Time", "Prognostic Time", "% Difference", "Average Velocity (m/s)", "Prognostic Velocity (m/s)", "% Difference")
      
      Race_2 = data.frame(FinishTime_top10, PrognosticTime_top10, Difference_top10, AvgVel_top10, ClassProgSpeed2_top10, VelDifference_top10)
      colnames(Race_2) = c("Finish Time", "Prognostic Time", "% Difference", "Average Velocity (m/s)", "Prognostic Velocity (m/s)", "% Difference")
      
      RaceSummary <<-  rbind(Race_1, Race_2)
      ({datatable(RaceSummary, rownames = c("Race 1", "Top 10"), options = list(dom='t', ordering=F, scrollX = TRUE))})
      
      
    }
    )
  })
  
  #### Output table - Time ####
  
  
  
  # output$table3 <-  DT::renderDataTable({
  #   tablecolumns <-  c("ID", "0m", "50m", "100m", "150m", "200m", 
  #                      "250m", "300m", "350m", "400m",
  #                      "450m", "500m", "550m", "600m",
  #                      "650m", "700m", "750m", "800m",
  #                      "850m", "900m", "950m", "1000m",
  #                      "1050m", "1100m", "1150m", "1200m",
  #                      "1250m", "1300m", "1350m", "1400m",
  #                      "1450m", "1500m", "1550m", "1600m",
  #                      "1650m", "1700m", "1750m", "1800m",
  #                      "1850m", "1900m", "1950m", "2000m")  
  #   
  #   Tabledata_Time <- column_to_rownames(tab2_Time(),'variable')
  #   
  #   ## 250splits
  #   
  #   Tabledata_Time <-  data.frame(t(Tabledata_Time))
  #   Tabledata_Time <-  rownames_to_column(Tabledata_Time)
  #   colnames(Tabledata_Time) <-  tablecolumns
  #   
  #   Filtereddata_Time <-  tab_Time() %>% select(variable, value)
  #   Filtereddata_Time <- column_to_rownames(Filtereddata_Time,'variable')
  #   Filtereddata_Time <-  data.frame(t(Filtereddata_Time))
  #   Filtereddata_Time <-  rownames_to_column(Filtereddata_Time)
  #   Filtereddata_Time[1,1] <- "Race"
  #   colnames(Filtereddata_Time) <-  tablecolumns
  #   
  #   Tabledata2_Time <-  data.frame(bind_rows(Filtereddata_Time, Tabledata_Time))
  #   
  #   
  #   
  #   
  #   #Tabledata2_Time <<-  Tabledata2_Time
  #   background <- "value == 'Race' ? 'orange' : value == 'Average' ? 'grey' : value != 'else' ? '' : ''"
  #   class(background) <- "JS_EVAL"
  #   
  #   
  #   
  #   
  #   ({datatable(Tabledata2_Time, rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE)) %>%
  #       formatStyle(
  #         'ID',
  #         target = 'row',
  #         backgroundColor = background
  #       )
  #   })
  # })
  # 
  # 
  
  ##################################################################################################################
  
  
  #### define Splits for Race 1 ####
  Race1 <- reactive({
    # Splits Time calculations #
    input$goButton
    isolate(
      Filtered_AccTime_data <-  tab_AccTime_R1() %>% select(variable, value))
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
    Split250_Time_Data <-  data.table(Split250_Time_250, Split250_Time_500, Split250_Time_750, Split250_Time_1000, Split250_Time_1250, Split250_Time_1500, Split250_Time_1750, Split250_Time_2000)
    Split250_Time_Avg <- sec_to_ms(mean(t(Split250_Time_Data)))
    Split250_Time_Max1 <- max(t(Split250_Time_Data))
    Split250_Time_Min1 <- min(t(Split250_Time_Data))
    Split250_Time_DO <-  round((Split250_Time_Max1-Split250_Time_Min1)/Split250_Time_Max1*100,2)
    Split250_Time_Max <-  sec_to_ms(Split250_Time_Max1)
    Split250_Time_Min <-  sec_to_ms(Split250_Time_Min1)
    
    
    #500 Splits
    Split500_Time_500 <- Split250_Time_250 + Split250_Time_500
    Split500_Time_1000 <- Split250_Time_750 + Split250_Time_1000
    Split500_Time_1500 <- Split250_Time_1250 + Split250_Time_1500
    Split500_Time_2000 <- Split250_Time_1750 + Split250_Time_2000
    Split500_Time_Data <-  data.table(Split500_Time_500, Split500_Time_1000, Split500_Time_1500, Split500_Time_2000)
    Split500_Time_Avg <- sec_to_ms(mean(t(Split500_Time_Data)))
    Split500_Time_Max1 <- max(t(Split500_Time_Data))
    Split500_Time_Min1 <- min(t(Split500_Time_Data))
    Split500_Time_DO <-  round((Split500_Time_Max1-Split500_Time_Min1)/Split500_Time_Max1*100,2)
    Split500_Time_Max <-  sec_to_ms(Split500_Time_Max1)
    Split500_Time_Min <-  sec_to_ms(Split500_Time_Min1)
    
    #AvVel 250 splits
    
    Split250_AvVel_250 <-  format(round((250/Split250_Time_250),2), nsmall = 2)
    Split250_AvVel_500 <-  format(round((250/Split250_Time_500),2), nsmall = 2)
    Split250_AvVel_750 <-  format(round((250/Split250_Time_750),2), nsmall = 2)
    Split250_AvVel_1000 <-  format(round((250/Split250_Time_1000),2), nsmall = 2)
    Split250_AvVel_1250 <-  format(round((250/Split250_Time_1250),2), nsmall = 2)
    Split250_AvVel_1500 <-  format(round((250/Split250_Time_1500),2), nsmall = 2)
    Split250_AvVel_1750 <-  format(round((250/Split250_Time_1750),2), nsmall = 2)
    Split250_AvVel_2000 <-  format(round((250/Split250_Time_2000),2), nsmall = 2)
    Split250_AvVel_Data <-  data.table(Split250_AvVel_250, Split250_AvVel_500, Split250_AvVel_750, Split250_AvVel_1000, Split250_AvVel_1250, Split250_AvVel_1500, Split250_AvVel_1750, Split250_AvVel_2000)
    Split250_AvVel_Data <-  as.numeric(Split250_AvVel_Data)
    Split250_AvVel_Avg <- round(mean(t(Split250_AvVel_Data)),2)
    Split250_AvVel_Max <- round(max(t(Split250_AvVel_Data)),2)
    Split250_AvVel_Min <- round(min(t(Split250_AvVel_Data)),2)
    Split250_AvVel_DO <-  round((Split250_AvVel_Max-Split250_AvVel_Min)/Split250_AvVel_Max*100,2)
    
    
    
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
    
    # Splits Stroke calculations #
    Filtered_AvStkRate_data <-  tab_AvStkRate_R1() %>% select(variable, value)
    Filtered_AvStkRate_data <- column_to_rownames(Filtered_AvStkRate_data,'variable')
    #250 splits
    
    Split250_AvStkRate_250 <- format(round((mean(Filtered_AvStkRate_data[2:6,])),2), nsmall = 2)
    Split250_AvStkRate_500 <- format(round((mean(Filtered_AvStkRate_data[7:11,])),2), nsmall = 2)
    Split250_AvStkRate_750 <- format(round((mean(Filtered_AvStkRate_data[12:16,])),2), nsmall = 2)
    Split250_AvStkRate_1000 <- format(round((mean(Filtered_AvStkRate_data[17:21,])),2), nsmall = 2)
    Split250_AvStkRate_1250 <- format(round((mean(Filtered_AvStkRate_data[22:26,])),2), nsmall = 2)
    Split250_AvStkRate_1500 <- format(round((mean(Filtered_AvStkRate_data[27:31,])),2), nsmall = 2)
    Split250_AvStkRate_1750 <- format(round((mean(Filtered_AvStkRate_data[32:36,])),2), nsmall = 2)
    Split250_AvStkRate_2000 <- format(round((mean(Filtered_AvStkRate_data[37:41,])),2), nsmall = 2)
    Split250_AvStkRate_Data <-  data.table(Split250_AvStkRate_250, Split250_AvStkRate_500, Split250_AvStkRate_750, Split250_AvStkRate_1000, Split250_AvStkRate_1250, Split250_AvStkRate_1500, Split250_AvStkRate_1750, Split250_AvStkRate_2000)
    Split250_AvStkRate_Data <-  as.numeric(Split250_AvStkRate_Data)
    Split250_AvStkRate_Avg <- round(mean(t(Split250_AvStkRate_Data)),2)
    Split250_AvStkRate_Max <- round(max(t(Split250_AvStkRate_Data)),2)
    Split250_AvStkRate_Min <- round(min(t(Split250_AvStkRate_Data)),2)
    Split250_AvStkRate_DO <-  round((Split250_AvStkRate_Max-Split250_AvStkRate_Min)/Split250_AvStkRate_Max*100,2)
    
    Filtered_AvProgSpeed_data <-  tab_AvVel_R1() %>% select(variable, ProgSpeed)
    Filtered_AvProgSpeed_data <- column_to_rownames(Filtered_AvProgSpeed_data,'variable')
    
    # 250 splits prog speed
    Split250_AvProgSpeed_250 <- format(round((mean(Filtered_AvProgSpeed_data[2:6,])),2), nsmall = 2)
    Split250_AvProgSpeed_500 <- format(round((mean(Filtered_AvProgSpeed_data[7:11,])),2), nsmall = 2)
    Split250_AvProgSpeed_750 <- format(round((mean(Filtered_AvProgSpeed_data[12:16,])),2), nsmall = 2)
    Split250_AvProgSpeed_1000 <- format(round((mean(Filtered_AvProgSpeed_data[17:21,])),2), nsmall = 2)
    Split250_AvProgSpeed_1250 <- format(round((mean(Filtered_AvProgSpeed_data[22:26,])),2), nsmall = 2)
    Split250_AvProgSpeed_1500 <- format(round((mean(Filtered_AvProgSpeed_data[27:31,])),2), nsmall = 2)
    Split250_AvProgSpeed_1750 <- format(round((mean(Filtered_AvProgSpeed_data[32:36,])),2), nsmall = 2)
    Split250_AvProgSpeed_2000 <- format(round((mean(Filtered_AvProgSpeed_data[37:41,])),2), nsmall = 2)
    Split250_AvProgSpeed_Data <-  data.table(Split250_AvProgSpeed_250, Split250_AvProgSpeed_500, Split250_AvProgSpeed_750, Split250_AvProgSpeed_1000, Split250_AvProgSpeed_1250, Split250_AvProgSpeed_1500, Split250_AvProgSpeed_1750, Split250_AvProgSpeed_2000)
    Split250_AvProgSpeed_Data <-  as.numeric(Split250_AvProgSpeed_Data)
    Split250_AvProgSpeed_Avg <- round(mean(t(Split250_AvProgSpeed_Data)),2)
    Split250_AvProgSpeed_Max <- round(max(t(Split250_AvProgSpeed_Data)),2)
    Split250_AvProgSpeed_Min <- round(min(t(Split250_AvProgSpeed_Data)),2)
    Split250_AvProgSpeed_DO <-  round((Split250_AvProgSpeed_Max-Split250_AvProgSpeed_Min)/Split250_AvProgSpeed_Max*100,2)
    
    Race1 <-  data.table("Distance (m)" = c("250", "500", "750", "1000", "1250", "1500", "1750", "2000", "", "Average", "Max", "Min", "% Drop Off"), 
                         "250m splits (secs)" = c(Split250_Time_250, Split250_Time_500, Split250_Time_750, Split250_Time_1000, Split250_Time_1250, Split250_Time_1500, Split250_Time_1750, Split250_Time_2000,"", Split250_Time_Avg, Split250_Time_Max, Split250_Time_Min, Split250_Time_DO),
                         "500m splits (secs)" = c("", Split500_Time_500, "", Split500_Time_1000, "", Split500_Time_1500, "", Split500_Time_2000, "", Split500_Time_Avg, Split500_Time_Max, Split500_Time_Min, Split500_Time_DO),
                         "Avg Velocity (m/s)" = c(Split250_AvVel_250, Split250_AvVel_500, Split250_AvVel_750, Split250_AvVel_1000, Split250_AvVel_1250, Split250_AvVel_1500, Split250_AvVel_1750, Split250_AvVel_2000, "", Split250_AvVel_Avg, Split250_AvVel_Max, Split250_AvVel_Min,Split250_AvVel_DO),
                         "Avg Stroke Rate (s/min)" = c(Split250_AvStkRate_250, Split250_AvStkRate_500, Split250_AvStkRate_750, Split250_AvStkRate_1000, Split250_AvStkRate_1250, Split250_AvStkRate_1500, Split250_AvStkRate_1750, Split250_AvStkRate_2000, "", Split250_AvStkRate_Avg, Split250_AvStkRate_Max, Split250_AvStkRate_Min, Split250_AvStkRate_DO),
                         "Avg Prog Speed (%)" = c(Split250_AvProgSpeed_250, Split250_AvProgSpeed_500, Split250_AvProgSpeed_750, Split250_AvProgSpeed_1000, Split250_AvProgSpeed_1250, Split250_AvProgSpeed_1500, Split250_AvProgSpeed_1750, Split250_AvProgSpeed_2000, "", Split250_AvProgSpeed_Avg, Split250_AvProgSpeed_Max, Split250_AvProgSpeed_Min, Split250_AvProgSpeed_DO))
    return(Race1)
    
  })
  




#### define Splits for Race 2 ####
Race2 <- reactive({
  # Splits Time calculations #
  input$goButton
  isolate(
  Filtered_AccTime_data <-  tab_AccTime_R2() %>% select(variable, value))
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
  Split250_Time_Data <-  data.table(Split250_Time_250, Split250_Time_500, Split250_Time_750, Split250_Time_1000, Split250_Time_1250, Split250_Time_1500, Split250_Time_1750, Split250_Time_2000)
  Split250_Time_Avg <- sec_to_ms(mean(t(Split250_Time_Data)))
  Split250_Time_Max1 <- max(t(Split250_Time_Data))
  Split250_Time_Min1 <- min(t(Split250_Time_Data))
  Split250_Time_DO <-  round((Split250_Time_Max1-Split250_Time_Min1)/Split250_Time_Max1*100,2)
  Split250_Time_Max <-  sec_to_ms(Split250_Time_Max1)
  Split250_Time_Min <-  sec_to_ms(Split250_Time_Min1)
  
  
  #500 Splits
  Split500_Time_500 <- Split250_Time_250 + Split250_Time_500
  Split500_Time_1000 <- Split250_Time_750 + Split250_Time_1000
  Split500_Time_1500 <- Split250_Time_1250 + Split250_Time_1500
  Split500_Time_2000 <- Split250_Time_1750 + Split250_Time_2000
  Split500_Time_Data <-  data.table(Split500_Time_500, Split500_Time_1000, Split500_Time_1500, Split500_Time_2000)
  Split500_Time_Avg <- sec_to_ms(mean(t(Split500_Time_Data)))
  Split500_Time_Max1 <- max(t(Split500_Time_Data))
  Split500_Time_Min1 <- min(t(Split500_Time_Data))
  Split500_Time_DO <-  round((Split500_Time_Max1-Split500_Time_Min1)/Split500_Time_Max1*100,2)
  Split500_Time_Max <-  sec_to_ms(Split500_Time_Max1)
  Split500_Time_Min <-  sec_to_ms(Split500_Time_Min1)
  
  #AvVel 250 splits
  
  Split250_AvVel_250 <-  format(round((250/Split250_Time_250),2), nsmall = 2)
  Split250_AvVel_500 <-  format(round((250/Split250_Time_500),2), nsmall = 2)
  Split250_AvVel_750 <-  format(round((250/Split250_Time_750),2), nsmall = 2)
  Split250_AvVel_1000 <-  format(round((250/Split250_Time_1000),2), nsmall = 2)
  Split250_AvVel_1250 <-  format(round((250/Split250_Time_1250),2), nsmall = 2)
  Split250_AvVel_1500 <-  format(round((250/Split250_Time_1500),2), nsmall = 2)
  Split250_AvVel_1750 <-  format(round((250/Split250_Time_1750),2), nsmall = 2)
  Split250_AvVel_2000 <-  format(round((250/Split250_Time_2000),2), nsmall = 2)
  Split250_AvVel_Data <-  data.table(Split250_AvVel_250, Split250_AvVel_500, Split250_AvVel_750, Split250_AvVel_1000, Split250_AvVel_1250, Split250_AvVel_1500, Split250_AvVel_1750, Split250_AvVel_2000)
  Split250_AvVel_Data <-  as.numeric(Split250_AvVel_Data)
  Split250_AvVel_Avg <- round(mean(t(Split250_AvVel_Data)),2)
  Split250_AvVel_Max <- round(max(t(Split250_AvVel_Data)),2)
  Split250_AvVel_Min <- round(min(t(Split250_AvVel_Data)),2)
  Split250_AvVel_DO <-  round((Split250_AvVel_Max-Split250_AvVel_Min)/Split250_AvVel_Max*100,2)
  
  
  
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
  
  # Splits Stroke calculations ##
  Filtered_AvStkRate_data <-  tab_AvStkRate_R2() %>% select(variable, value)
  Filtered_AvStkRate_data <- column_to_rownames(Filtered_AvStkRate_data,'variable')
  #250 splits
  
  Split250_AvStkRate_250 <- format(round((mean(Filtered_AvStkRate_data[2:6,])),2), nsmall = 2)
  Split250_AvStkRate_500 <- format(round((mean(Filtered_AvStkRate_data[7:11,])),2), nsmall = 2)
  Split250_AvStkRate_750 <- format(round((mean(Filtered_AvStkRate_data[12:16,])),2), nsmall = 2)
  Split250_AvStkRate_1000 <- format(round((mean(Filtered_AvStkRate_data[17:21,])),2), nsmall = 2)
  Split250_AvStkRate_1250 <- format(round((mean(Filtered_AvStkRate_data[22:26,])),2), nsmall = 2)
  Split250_AvStkRate_1500 <- format(round((mean(Filtered_AvStkRate_data[27:31,])),2), nsmall = 2)
  Split250_AvStkRate_1750 <- format(round((mean(Filtered_AvStkRate_data[32:36,])),2), nsmall = 2)
  Split250_AvStkRate_2000 <- format(round((mean(Filtered_AvStkRate_data[37:41,])),2), nsmall = 2)
  Split250_AvStkRate_Data <-  data.table(Split250_AvStkRate_250, Split250_AvStkRate_500, Split250_AvStkRate_750, Split250_AvStkRate_1000, Split250_AvStkRate_1250, Split250_AvStkRate_1500, Split250_AvStkRate_1750, Split250_AvStkRate_2000)
  Split250_AvStkRate_Data <-  as.numeric(Split250_AvStkRate_Data)
  Split250_AvStkRate_Avg <- round(mean(t(Split250_AvStkRate_Data)),2)
  Split250_AvStkRate_Max <- round(max(t(Split250_AvStkRate_Data)),2)
  Split250_AvStkRate_Min <- round(min(t(Split250_AvStkRate_Data)),2)
  Split250_AvStkRate_DO <-  round((Split250_AvStkRate_Max-Split250_AvStkRate_Min)/Split250_AvStkRate_Max*100,2)
  
  Filtered_AvProgSpeed_data <-  tab_AvVel_R2() %>% select(variable, ProgSpeed)
  Filtered_AvProgSpeed_data <- column_to_rownames(Filtered_AvProgSpeed_data,'variable')
  
  # 250 splits prog speed
  Split250_AvProgSpeed_250 <- format(round((mean(Filtered_AvProgSpeed_data[2:6,])),2), nsmall = 2)
  Split250_AvProgSpeed_500 <- format(round((mean(Filtered_AvProgSpeed_data[7:11,])),2), nsmall = 2)
  Split250_AvProgSpeed_750 <- format(round((mean(Filtered_AvProgSpeed_data[12:16,])),2), nsmall = 2)
  Split250_AvProgSpeed_1000 <- format(round((mean(Filtered_AvProgSpeed_data[17:21,])),2), nsmall = 2)
  Split250_AvProgSpeed_1250 <- format(round((mean(Filtered_AvProgSpeed_data[22:26,])),2), nsmall = 2)
  Split250_AvProgSpeed_1500 <- format(round((mean(Filtered_AvProgSpeed_data[27:31,])),2), nsmall = 2)
  Split250_AvProgSpeed_1750 <- format(round((mean(Filtered_AvProgSpeed_data[32:36,])),2), nsmall = 2)
  Split250_AvProgSpeed_2000 <- format(round((mean(Filtered_AvProgSpeed_data[37:41,])),2), nsmall = 2)
  Split250_AvProgSpeed_Data <-  data.table(Split250_AvProgSpeed_250, Split250_AvProgSpeed_500, Split250_AvProgSpeed_750, Split250_AvProgSpeed_1000, Split250_AvProgSpeed_1250, Split250_AvProgSpeed_1500, Split250_AvProgSpeed_1750, Split250_AvProgSpeed_2000)
  Split250_AvProgSpeed_Data <-  as.numeric(Split250_AvProgSpeed_Data)
  Split250_AvProgSpeed_Avg <- round(mean(t(Split250_AvProgSpeed_Data)),2)
  Split250_AvProgSpeed_Max <- round(max(t(Split250_AvProgSpeed_Data)),2)
  Split250_AvProgSpeed_Min <- round(min(t(Split250_AvProgSpeed_Data)),2)
  Split250_AvProgSpeed_DO <-  round((Split250_AvProgSpeed_Max-Split250_AvProgSpeed_Min)/Split250_AvProgSpeed_Max*100,2)
  
  Race2 <-  data.table("Distance (m)" = c("250", "500", "750", "1000", "1250", "1500", "1750", "2000", "", "Average", "Max", "Min", "% Drop Off"), 
                       "250m splits (secs)" = c(Split250_Time_250, Split250_Time_500, Split250_Time_750, Split250_Time_1000, Split250_Time_1250, Split250_Time_1500, Split250_Time_1750, Split250_Time_2000,"", Split250_Time_Avg, Split250_Time_Max, Split250_Time_Min, Split250_Time_DO),
                       "500m splits (secs)" = c("", Split500_Time_500, "", Split500_Time_1000, "", Split500_Time_1500, "", Split500_Time_2000, "", Split500_Time_Avg, Split500_Time_Max, Split500_Time_Min, Split500_Time_DO),
                       "Avg Velocity (m/s)" = c(Split250_AvVel_250, Split250_AvVel_500, Split250_AvVel_750, Split250_AvVel_1000, Split250_AvVel_1250, Split250_AvVel_1500, Split250_AvVel_1750, Split250_AvVel_2000, "", Split250_AvVel_Avg, Split250_AvVel_Max, Split250_AvVel_Min,Split250_AvVel_DO),
                       "Avg Stroke Rate (s/min)" = c(Split250_AvStkRate_250, Split250_AvStkRate_500, Split250_AvStkRate_750, Split250_AvStkRate_1000, Split250_AvStkRate_1250, Split250_AvStkRate_1500, Split250_AvStkRate_1750, Split250_AvStkRate_2000, "", Split250_AvStkRate_Avg, Split250_AvStkRate_Max, Split250_AvStkRate_Min, Split250_AvStkRate_DO),
                       "Avg Prog Speed (%)" = c(Split250_AvProgSpeed_250, Split250_AvProgSpeed_500, Split250_AvProgSpeed_750, Split250_AvProgSpeed_1000, Split250_AvProgSpeed_1250, Split250_AvProgSpeed_1500, Split250_AvProgSpeed_1750, Split250_AvProgSpeed_2000, "", Split250_AvProgSpeed_Avg, Split250_AvProgSpeed_Max, Split250_AvProgSpeed_Min, Split250_AvProgSpeed_DO))
  
  return(Race2)
  
})


#### Define splits for Top10 ####
Top10 <- reactive({
  # Splits Time calculations #
  
  input$goButton
  isolate(Filtered_AccTime_data <-  tab2_AccTime() %>% select(variable, Average))
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
  Split250_Time_Data <-  data.table(Split250_Time_250, Split250_Time_500, Split250_Time_750, Split250_Time_1000, Split250_Time_1250, Split250_Time_1500, Split250_Time_1750, Split250_Time_2000)
  Split250_Time_Avg <- sec_to_ms(mean(t(Split250_Time_Data)))
  Split250_Time_Max1 <- max(t(Split250_Time_Data))
  Split250_Time_Min1 <- min(t(Split250_Time_Data))
  Split250_Time_DO <-  round((Split250_Time_Max1-Split250_Time_Min1)/Split250_Time_Max1*100,2)
  Split250_Time_Max <-  sec_to_ms(Split250_Time_Max1)
  Split250_Time_Min <-  sec_to_ms(Split250_Time_Min1)
  
  
  #500 Splits
  Split500_Time_500 <- Split250_Time_250 + Split250_Time_500
  Split500_Time_1000 <- Split250_Time_750 + Split250_Time_1000
  Split500_Time_1500 <- Split250_Time_1250 + Split250_Time_1500
  Split500_Time_2000 <- Split250_Time_1750 + Split250_Time_2000
  Split500_Time_Data <-  data.table(Split500_Time_500, Split500_Time_1000, Split500_Time_1500, Split500_Time_2000)
  Split500_Time_Avg <- sec_to_ms(mean(t(Split500_Time_Data)))
  Split500_Time_Max1 <- max(t(Split500_Time_Data))
  Split500_Time_Min1 <- min(t(Split500_Time_Data))
  Split500_Time_DO <-  round((Split500_Time_Max1-Split500_Time_Min1)/Split500_Time_Max1*100,2)
  Split500_Time_Max <-  sec_to_ms(Split500_Time_Max1)
  Split500_Time_Min <-  sec_to_ms(Split500_Time_Min1)
  
  #AvVel 250 splits
  
  Split250_AvVel_250 <-  format(round((250/Split250_Time_250),2), nsmall = 2)
  Split250_AvVel_500 <-  format(round((250/Split250_Time_500),2), nsmall = 2)
  Split250_AvVel_750 <-  format(round((250/Split250_Time_750),2), nsmall = 2)
  Split250_AvVel_1000 <-  format(round((250/Split250_Time_1000),2), nsmall = 2)
  Split250_AvVel_1250 <-  format(round((250/Split250_Time_1250),2), nsmall = 2)
  Split250_AvVel_1500 <-  format(round((250/Split250_Time_1500),2), nsmall = 2)
  Split250_AvVel_1750 <-  format(round((250/Split250_Time_1750),2), nsmall = 2)
  Split250_AvVel_2000 <-  format(round((250/Split250_Time_2000),2), nsmall = 2)
  Split250_AvVel_Data <-  data.table(Split250_AvVel_250, Split250_AvVel_500, Split250_AvVel_750, Split250_AvVel_1000, Split250_AvVel_1250, Split250_AvVel_1500, Split250_AvVel_1750, Split250_AvVel_2000)
  Split250_AvVel_Data <-  as.numeric(Split250_AvVel_Data)
  Split250_AvVel_Avg <- round(mean(t(Split250_AvVel_Data)),2)
  Split250_AvVel_Max <- round(max(t(Split250_AvVel_Data)),2)
  Split250_AvVel_Min <- round(min(t(Split250_AvVel_Data)),2)
  Split250_AvVel_DO <-  round((Split250_AvVel_Max-Split250_AvVel_Min)/Split250_AvVel_Max*100,2)
  
  
  
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
  
  # Splits Stroke calculations ##
  Filtered_AvStkRate_data <-  tab2_AvStkRate() %>% select(variable, Average)
  Filtered_AvStkRate_data <- column_to_rownames(Filtered_AvStkRate_data,'variable')
  #250 splits
  
  Split250_AvStkRate_250 <- format(round((mean(Filtered_AvStkRate_data[2:6,])),2), nsmall = 2)
  Split250_AvStkRate_500 <- format(round((mean(Filtered_AvStkRate_data[7:11,])),2), nsmall = 2)
  Split250_AvStkRate_750 <- format(round((mean(Filtered_AvStkRate_data[12:16,])),2), nsmall = 2)
  Split250_AvStkRate_1000 <- format(round((mean(Filtered_AvStkRate_data[17:21,])),2), nsmall = 2)
  Split250_AvStkRate_1250 <- format(round((mean(Filtered_AvStkRate_data[22:26,])),2), nsmall = 2)
  Split250_AvStkRate_1500 <- format(round((mean(Filtered_AvStkRate_data[27:31,])),2), nsmall = 2)
  Split250_AvStkRate_1750 <- format(round((mean(Filtered_AvStkRate_data[32:36,])),2), nsmall = 2)
  Split250_AvStkRate_2000 <- format(round((mean(Filtered_AvStkRate_data[37:41,])),2), nsmall = 2)
  Split250_AvStkRate_Data <-  data.table(Split250_AvStkRate_250, Split250_AvStkRate_500, Split250_AvStkRate_750, Split250_AvStkRate_1000, Split250_AvStkRate_1250, Split250_AvStkRate_1500, Split250_AvStkRate_1750, Split250_AvStkRate_2000)
  Split250_AvStkRate_Data <-  as.numeric(Split250_AvStkRate_Data)
  Split250_AvStkRate_Avg <- round(mean(t(Split250_AvStkRate_Data)),2)
  Split250_AvStkRate_Max <- round(max(t(Split250_AvStkRate_Data)),2)
  Split250_AvStkRate_Min <- round(min(t(Split250_AvStkRate_Data)),2)
  Split250_AvStkRate_DO <-  round((Split250_AvStkRate_Max-Split250_AvStkRate_Min)/Split250_AvStkRate_Max*100,2)
  
  Filtered_AvProgSpeed_data <-  tab2_AvVel() %>% select(variable, ProgSpeedAverage)
  Filtered_AvProgSpeed_data <- column_to_rownames(Filtered_AvProgSpeed_data,'variable')
  
  # 250 splits prog speed
  Split250_AvProgSpeed_250 <- format(round((mean(Filtered_AvProgSpeed_data[2:6,])),2), nsmall = 2)
  Split250_AvProgSpeed_500 <- format(round((mean(Filtered_AvProgSpeed_data[7:11,])),2), nsmall = 2)
  Split250_AvProgSpeed_750 <- format(round((mean(Filtered_AvProgSpeed_data[12:16,])),2), nsmall = 2)
  Split250_AvProgSpeed_1000 <- format(round((mean(Filtered_AvProgSpeed_data[17:21,])),2), nsmall = 2)
  Split250_AvProgSpeed_1250 <- format(round((mean(Filtered_AvProgSpeed_data[22:26,])),2), nsmall = 2)
  Split250_AvProgSpeed_1500 <- format(round((mean(Filtered_AvProgSpeed_data[27:31,])),2), nsmall = 2)
  Split250_AvProgSpeed_1750 <- format(round((mean(Filtered_AvProgSpeed_data[32:36,])),2), nsmall = 2)
  Split250_AvProgSpeed_2000 <- format(round((mean(Filtered_AvProgSpeed_data[37:41,])),2), nsmall = 2)
  Split250_AvProgSpeed_Data <-  data.table(Split250_AvProgSpeed_250, Split250_AvProgSpeed_500, Split250_AvProgSpeed_750, Split250_AvProgSpeed_1000, Split250_AvProgSpeed_1250, Split250_AvProgSpeed_1500, Split250_AvProgSpeed_1750, Split250_AvProgSpeed_2000)
  Split250_AvProgSpeed_Data <-  as.numeric(Split250_AvProgSpeed_Data)
  Split250_AvProgSpeed_Avg <- round(mean(t(Split250_AvProgSpeed_Data)),2)
  Split250_AvProgSpeed_Max <- round(max(t(Split250_AvProgSpeed_Data)),2)
  Split250_AvProgSpeed_Min <- round(min(t(Split250_AvProgSpeed_Data)),2)
  Split250_AvProgSpeed_DO <-  round((Split250_AvProgSpeed_Max-Split250_AvProgSpeed_Min)/Split250_AvProgSpeed_Max*100,2)
  
  Top10 <-  data.table("Distance (m)" = c("250", "500", "750", "1000", "1250", "1500", "1750", "2000", "", "Average", "Max", "Min", "% Drop Off"), 
                       "250m splits (secs)" = c(Split250_Time_250, Split250_Time_500, Split250_Time_750, Split250_Time_1000, Split250_Time_1250, Split250_Time_1500, Split250_Time_1750, Split250_Time_2000,"", Split250_Time_Avg, Split250_Time_Max, Split250_Time_Min, Split250_Time_DO),
                       "500m splits (secs)" = c("", Split500_Time_500, "", Split500_Time_1000, "", Split500_Time_1500, "", Split500_Time_2000, "", Split500_Time_Avg, Split500_Time_Max, Split500_Time_Min, Split500_Time_DO),
                       "Avg Velocity (m/s)" = c(Split250_AvVel_250, Split250_AvVel_500, Split250_AvVel_750, Split250_AvVel_1000, Split250_AvVel_1250, Split250_AvVel_1500, Split250_AvVel_1750, Split250_AvVel_2000, "", Split250_AvVel_Avg, Split250_AvVel_Max, Split250_AvVel_Min,Split250_AvVel_DO),
                       "Avg Stroke Rate (s/min)" = c(Split250_AvStkRate_250, Split250_AvStkRate_500, Split250_AvStkRate_750, Split250_AvStkRate_1000, Split250_AvStkRate_1250, Split250_AvStkRate_1500, Split250_AvStkRate_1750, Split250_AvStkRate_2000, "", Split250_AvStkRate_Avg, Split250_AvStkRate_Max, Split250_AvStkRate_Min, Split250_AvStkRate_DO),
                       "Avg Prog Speed (%)" = c(Split250_AvProgSpeed_250, Split250_AvProgSpeed_500, Split250_AvProgSpeed_750, Split250_AvProgSpeed_1000, Split250_AvProgSpeed_1250, Split250_AvProgSpeed_1500, Split250_AvProgSpeed_1750, Split250_AvProgSpeed_2000, "", Split250_AvProgSpeed_Avg, Split250_AvProgSpeed_Max, Split250_AvProgSpeed_Min, Split250_AvProgSpeed_DO))
  
  
  return(Top10)
  
})

##################################################################################################################

#TABLE OUTPUT###
#### Output Summary Table ####

output$SummaryTable <-  DT::renderDataTable({
  input$goButton
  isolate(if (input$Report_Type == "Single Race"){
    Summarydatatable <-  data.table(Race1()[,"Distance (m)"], 
                                    "Race 1" = Race1()[,"250m splits (secs)"], 
                                    "Race 1" = Race1()[,"500m splits (secs)"], 
                                    "Race 1" = Race1()[,"Avg Velocity (m/s)"], 
                                    "Race 1" = Race1()[,"Avg Stroke Rate (s/min)"],
                                    "Race 1" = Race1()[,"Avg Prog Speed (%)"])
    
    Summarydatatable <-  setnames(Summarydatatable, old = c("Race 1.250m splits (secs)", "Race 1.500m splits (secs)", "Race 1.Avg Velocity (m/s)", "Race 1.Avg Stroke Rate (s/min)", "Race 1.Avg Prog Speed (%)"), new = c("250m splits (secs)", "500m splits (secs)", "Avg Velocity (m/s)","Avg Stroke Rate (s/min)", "Avg Prog Speed (%)"))
    
    
    ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 13)) %>% formatStyle(
      columns = c(2,4,6),
      #c('250m splits (secs)', "Race 2", "Avg Velocity (m/s)", "Avg Prog Speed (%)"),
      valueColumns = 0,
      target = 'cell',
      backgroundColor = 'lightblue',
      fontWeight = 'bold')
      
    }) 
    
    
    
  }else if(input$Report_Type  == "Two Races"){
    Summarydatatable <-  data.table(Race1()[,"Distance (m)"], 
                                    "Race 1" = Race1()[,"250m splits (secs)"], "Race 2" = Race2()[,"250m splits (secs)"],
                                    "Race 1" = Race1()[,"500m splits (secs)"], "Race 2" = Race2()[,"500m splits (secs)"],
                                    "Race 1" = Race1()[,"Avg Velocity (m/s)"], "Race 2" = Race2()[,"Avg Velocity (m/s)"],
                                    "Race 1" = Race1()[,"Avg Stroke Rate (s/min)"], "Race 2" = Race2()[,"Avg Stroke Rate (s/min)"],
                                    "Race 1" = Race1()[,"Avg Prog Speed (%)"], "Race 2" = Race2()[,"Avg Prog Speed (%)"])
    
    Summarydatatable <-  setnames(Summarydatatable, old = c("Race 1.250m splits (secs)","Race 2.250m splits (secs)", "Race 1.500m splits (secs)","Race 2.500m splits (secs)", "Race 1.Avg Velocity (m/s)","Race 2.Avg Velocity (m/s)", "Race 1.Avg Stroke Rate (s/min)","Race 2.Avg Stroke Rate (s/min)", "Race 1.Avg Prog Speed (%)","Race 2.Avg Prog Speed (%)"), new = c("250m splits (secs)","Race 2", "500m splits (secs)","Race 2", "Avg Velocity (m/s)","Race 2", "Avg Stroke Rate (s/min)","Race 2", "Avg Prog Speed (%)", "Race 2"))
    
    
    ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 13)) %>% formatStyle(
      columns = c(2,3,6,7,10,11),
      #c('250m splits (secs)', "Race 2", "Avg Velocity (m/s)", "Avg Prog Speed (%)"),
      valueColumns = 0,
      target = 'cell',
      backgroundColor = 'lightblue',
      fontWeight = 'bold')
      
    })
    
    
    
    
  }else if (input$Report_Type  == "vs Top 10"){
    Summarydatatable <-  data.table(Race1()[,"Distance (m)"], 
                                    Race1()[,"250m splits (secs)"], "Ideal" = Top10()[,"250m splits (secs)"],
                                    Race1()[,"500m splits (secs)"], "Ideal" = Top10()[,"500m splits (secs)"],
                                    Race1()[,"Avg Velocity (m/s)"], "Ideal" = Top10()[,"Avg Velocity (m/s)"],
                                    Race1()[,"Avg Stroke Rate (s/min)"], "Ideal" = Top10()[,"Avg Stroke Rate (s/min)"],
                                    Race1()[,"Avg Prog Speed (%)"], "Ideal" = Top10()[,"Avg Prog Speed (%)"])
    
    Summarydatatable <-  setnames(Summarydatatable, old = c("Ideal.250m splits (secs)","Ideal.500m splits (secs)","Ideal.Avg Velocity (m/s)","Ideal.Avg Stroke Rate (s/min)","Ideal.Avg Prog Speed (%)"), new = c("Top 10", "Top 10","Top 10","Top 10","Top 10"))
    
    
    ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 13)) %>% formatStyle(
      columns = c(2,3,6,7,10,11),
      #c('250m splits (secs)', "Race 2", "Avg Velocity (m/s)", "Avg Prog Speed (%)"),
      valueColumns = 0,
      target = 'cell',
      backgroundColor = 'lightblue',
      fontWeight = 'bold')
      
    })  
    
  }
  )
})





#### Output headers ####

output$Summarydatatable2head <- renderText({
  
  if (input$Report_Type == "Single Race"){
    paste(input$Class,input$Name, input$Competition, input$Date, input$Phase)  
    
    
  } else if  (input$Report_Type == "Two Races"){
    paste(input$Class,input$Name, input$Competition, input$Date, input$Phase, "vs", input$Name2, input$Competition2, input$Date2, input$Phase2)  
  }  else if (input$Report_Type == "vs Top 10"){
    paste(input$Class,input$Name, input$Competition, input$Date, input$Phase, "vs Top 10 Average")  
    
  }
  
})



output$Summaryhead <- renderText({paste(tab()$First_Name[1], tab()$Last_Name[1], tab()$Competition[1], tab()$Date[1], tab()$Class[1], tab()$Distance[1], tab()$Phase[1], "vs Top 10 average")
})
output$Timehead <- renderText({paste0("Time vs Top 10 average")
})

output$Summaryheadgap <- renderText({
  paste(" ")
})
output$Splithead <- renderText({paste0("Splits vs Top 10 average")
})



}

