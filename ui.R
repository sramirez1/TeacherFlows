#Load libraries
library(shiny)
library(dplyr)
library(googleVis)
library(networkD3)
library(DT)
library(shinydashboard)
library(babynames)
#Use googleVis sankey?
google=TRUE
header<-dashboardHeader(title="The River of Teachers ALPHA", titleWidth = 350)

sidebar<-dashboardSidebar(
  sidebarMenu(
    selectInput("countryInput", "Year of DOE Entry",choices = c("2012-2013", "2013-2014", "2014-2015","2015-2016","2016-2017")),
    checkboxGroupInput("tppInput", "Teacher Preparation Program",choices = c('Columbia', 'TFA', 'CUNY', 'Other', 'NYU','Baruch','Brown','Rutgers'), selected=c('Columbia', 'TFA', 'CUNY', 'Other', 'NYU','Baruch','Brown','Rutgers')),
    selectInput("dbnInput", "Restrict to DBN", choices=c("All DBNs", "00x001", "00X002","00X003"))
    )
  )

body<-dashboardBody(
  fluidRow(
    column(width=12,
           box(width = NULL, solidHeader = TRUE, status= 'info', title="Sankey Flowchart by Entry Year",
               htmlOutput("Sankey")
               ))
    ),
  fluidRow(
    column(3,
           valueBox(25, "New Hires", icon=icon("user"), width=NULL),
            valueBox(30, "Exits in 2014-2015", icon=icon("sign-out"), width=NULL, color = "purple"),
            valueBox(36, "Exits in 2015-2016", icon=icon("sign-out"), width=NULL, color = "yellow")),
  
    column(9,
           box(width=NULL, solidHeader=TRUE, status='info',
               DT::dataTableOutput("results"))
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