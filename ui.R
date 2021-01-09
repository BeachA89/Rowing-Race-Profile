

ui <- dashboardPage(
  dashboardHeader (title = "Rowing Race Profile", titleWidth = 450),
  dashboardSidebar(
    # UNCOMMENT IF MANUAL UPLOAD###
    #fileInput("file1", "Choose files", multiple = TRUE,
    #          accept = c("text/csv",
    #                     "text/comma-separated-values,text/plain",
    #                     ".csv")),
    selectInput("Report_Type", "Report Type:",
                c("Single Race" = "Single Race",
                  "Two Races" = "Two Races",
                  "vs Top 10" = "vs Top 10")),    
   
    uiOutput("select_Class"),
    uiOutput("select_Name"),
    uiOutput("select_Competition"),
    uiOutput("select_Date"),
    uiOutput("select_Phase"),
    
    uiOutput("select_Name2"),
    uiOutput("select_Competition2"),
    uiOutput("select_Date2"),
    uiOutput("select_Phase2"),
    actionButton("goButton", "Go!")
  
  ),
  dashboardBody(
    
    fluidRow(
      h3(textOutput("Summarydatatable2head"),align = "center")
    ),
    fluidRow(
      column(8,
      box(title = "Race Summary", status = "primary", solidHeader = TRUE, width = 12,
          DT::dataTableOutput("RaceSummary")))
    ),
    fluidRow(
      column(8,box(title = "Summary Table", status = "primary", solidHeader = TRUE,width = 12,
          DT::dataTableOutput("SummaryTable"))),
      column(4,box(title = "Race Splits Profile", status = "primary", solidHeader = TRUE,width = 12,
          plotOutput("plot_scatter_SR_Progspeed")))
    ),
    fluidRow(
      box(title = "Prognostic and Stroke Rate Profile", status = "primary", solidHeader = TRUE,width = 12,
          plotlyOutput("plot_ProgSpeed_AvStkRate"))
    ),

   #fluidRow(
  #   box(title = "Race Splits Profile", status = "primary", solidHeader = TRUE,width = 12,
  #       collapsible = TRUE,plotOutput("ggplot_Time"))
  # ),



    
  )
)


