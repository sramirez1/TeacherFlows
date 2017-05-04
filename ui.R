#Load libraries
library(shiny)
library(dplyr)
library(googleVis)
library(sankeyD3, lib.loc = .libPaths()[2])
library(DT)
library(shinydashboard)
library(babynames)
#Use googleVis sankey?
google=TRUE
header<-dashboardHeader(title="The River of Teachers ALPHA", titleWidth = 350)

sidebar<-dashboardSidebar(
  sidebarMenu(
    
    dateInput('dateStart',
      label = paste('Start Date: '),
      value = as.character(Sys.Date()),
      min = "2012-09-01", max = "2017-09-01",
      format = "mm/dd/yyyy",
      startview = 'year', weekstart = 1, language = "de"
    ),
    
    dateInput('dateEnd',
              label = paste('End Date: '),
              value = as.character(Sys.Date()),
              min = "2012-09-01", max = "2017-09-01",
              format = "mm/dd/yyyy",
              startview = 'year', weekstart = 1, language = "de"
    ),    
    
    selectInput("yearInput", "Year of DOE Entry",choices = c(2012, 2013, 2014, 2015, 2016)),
    checkboxGroupInput("tppInput", "Teacher Preparation Program",choices = c('Columbia', 'TFA', 'CUNY', 'Other', 'NYU','Baruch','Brown','Rutgers'), selected=c('Columbia', 'TFA', 'CUNY', 'Other', 'NYU','Baruch','Brown','Rutgers')),
    selectInput("dbnInput", "Restrict to DBN", choices=c("All DBNs", "00X001", "00X002","00X003")),
    menuItem("River", tabName = "d3"),
    menuItem("d3 River", tabName = "d3", badgeLabel = "new", badgeColor="green")
    ),
  helpText(HTML("<b>DISCLAIMER</b>")),
  helpText(HTML("I contributed to this Shiny app in my own personal capacity.
                <br><br>
                The views and content expressed here do not represent the views of the NYC Department of Education.
                <br><br>
                All data presented here is <b>FAKE</b> and does not correspond to actual entry, exit, or Advance Rating of NYC Department of Education teachers.")),
  menuItem("Source code", icon = icon("file-code-o"), 
           href = "https://github.com/sramirez1/TeacherFlows")
  )

body<-dashboardBody(
  tabItems(
    tabItem(tabName="dashboard",

            fluidRow(
              valueBoxOutput("newHires"),
              valueBoxOutput("exit1"),
              valueBoxOutput("exit2")
            ),

            fluidRow(
              box(
                width = 12, status = "info", solidHeader = TRUE,
                title = "Sankey Flowchart by Entry Year",
                htmlOutput("Sankey")
              )
            ),

            fluidRow(
              box(
                width = 9, status = "info", solidHeader = TRUE,
                title = "HEDI Ratings",
                DT::dataTableOutput("results")
              )
            )
    ),
    tabItem(tabName= "d3",
            
            fluidRow(
              box(
                width = 12, status = "info", solidHeader = TRUE,
                title = "Sankey Flowchart by Entry Year",
                sankeyNetworkOutput("Sankey2", width="1600px", height="800px")
              )
            )
    )
  )
)

  
  
dashboardPage(header, sidebar, body)



# ui <- fluidPage(
#   
#   titlePanel("NYCDOE Teacher Retention Flows"),
#   
#   if (google==FALSE){
#     #Render Sankey with networkD3
#     sankeyNetworkOutput("Sankey")
#     
#   } else {
#   #Render sankey with googleVis
#   mainPanel(
#     htmlOutput("Sankey")
# 
#   )
#   },
#   
# 
#     
#   fluidRow(),
#   fluidRow(
#     column(3,
#            h4("Diamonds Explorer"),
#            sliderInput('sampleSize', 'Sample Size', 
#                        min=1, max=53000,
#                        value=min(1000, 53000), 
#                        step=500, round=0)
#     ),
#     column(9,
#            DT::dataTableOutput("results")
#     )
#   )
  
  # sidebarLayout(
  #   sidebarPanel(
  #     sliderInput("priceInput", "Price", min = 0, max = 100,
  #                 value = c(25, 40), pre = "$"),
  #     radioButtons("typeInput", "Product type",
  #                  choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
  #                  selected = "WINE"),
  #     selectInput("countryInput", "Country",
  #                 choices = c("CANADA", "FRANCE", "ITALY"))
  #   ),
  #   mainPanel(
  #     # streamgraphOutput("Streamgraph"),
  #     plotOutput("coolplot"),
  #     br(),
  #     br(),
  #     tableOutput("results")
  #     
  #   )
  # )
# )