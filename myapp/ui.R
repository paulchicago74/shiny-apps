## app.R ##
library(shinydashboard)
library(shinyBS)
library(PerformanceAnalytics)
library(plotly)
library(ggplot2)
library(RColorBrewer)
library(qcc)
library(DT)
library(qualityTools)
library(shinyWidgets)

data(edhec)

shinyUI(dashboardPage(
  dashboardHeader(title = "DASHBOARD", titleWidth = 250),
  
  dashboardSidebar(width=250,
                   fluidPage(
                     fileInput(inputId = "fileInp", label = "Input file:",multiple = FALSE,
                               accept = c(
                                 "text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"))
                   ),
                  
  sidebarMenu(
   # Setting id makes input$tabs give the tabName of currently-selected tab
  id = "tabs",
 # menuItem("Pareto_Chart", tabName = "Pareto_Chart", selected = T, icon = icon("bar-chart-o")),
  menuItem("S_Chart",  tabName = "S_Chart", selected = T,icon = icon("bar-chart-o")),
   menuItem("R_Chart",  tabName = "R_Chart", selected = T,icon = icon("bar-chart-o")),
   menuItem("CUSUM",  tabName = "CUSUM", selected = T,icon = icon("bar-chart-o")),
   menuItem("Process_Capability Studies",tabName = "Process Capability Studies", icon = icon("bar-chart-o")),
   menuItem("X-Bar_Chart",  tabName = "X-Bar Chart", selected = T,icon = icon("bar-chart-o")),
   #menuItem("Information",  tabName = "Information", selected = T,icon = icon("bar-chart-o")),
  br(),
 conditionalPanel("input.tabs === 'S_Chart'",
                  #numericInput("stddev1", "stddev",3),
                  numericInput("LSL1", "LSL",0),
                  numericInput("USL1", "USL",17)),
 conditionalPanel("input.tabs === 'R_Chart'",
                  #numericInput("stddev2", "stddev",3),
                  numericInput("LSL2", "LSL",0),
                  numericInput("USL2", "USL",17)),
 conditionalPanel("input.tabs === 'CUSUM'",
                  numericInput("stddev3", "std.dev",3),
                  numericInput("LSL3", "LSL",0),
                  numericInput("USL3", "USL",17)),
 conditionalPanel("input.tabs === 'Process Capability Studies'",
                  numericInput("LSL4", "LSL",0),
                  numericInput("USL4", "USL",17)),
 conditionalPanel("input.tabs === 'X-Bar Chart'",
                  #numericInput("stddev5", "stddev",3),
                  numericInput("LSL5", "LSL",0),
                  numericInput("USL5", "USL",17)),
br(),
 
 fluidRow(
   column(6, align="center", offset = 3,
          
          downloadButton('downloadplot',"Save as png",style="color: #fff; background-color: teal; border-color: teal; font-size:100%;float:center"),br(),br(),
          downloadButton('downloadData',"Save as CSV",style="color: #fff; background-color: teal; border-color: teal; font-size:100%;float:center"),br(),br(),
          downloadButton('report',"Make report",style="color: #fff; background-color: teal; border-color: teal; font-size:100%;display:center-align;width=20px"),
         tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")
          )
 )
 
 
 
 
  )
 
 
),

  dashboardBody(tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
   
   #tabItems(
      tabsetPanel(
      tabPanel("Chart",
               valueBoxOutput("vbox", width = 3),
               valueBoxOutput("vbox1", width = 3),
               valueBoxOutput("vbox2", width = 3),
               valueBoxOutput("vbox3", width = 3),
               column(4,
               box(height =490,width = 600, solidHeader = FALSE, status = "success",
                            uiOutput("select_col",height = 80))
              
               ),
               
             column(8,box(height =490,width = 1000, solidHeader = FALSE, status = "success",
                          plotOutput("lep",height = 400))
                    )
                    ),
      
      tabPanel("Table",
       
               column(12,box(height =600,width = 800, solidHeader = FALSE, status = "success",
          DT::dataTableOutput("table",height=600)))
      )
      
      )
    
      )
)
)
