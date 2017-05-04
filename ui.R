#Load libraries
library(shiny)
library(dplyr)
library(googleVis)
library(sankeyD3, lib.loc = .libPaths()[2])
library(DT)
library(shinydashboard)
library(babynames)
library(lubridate)
#Use googleVis sankey?
google=TRUE
header<-dashboardHeader(title="The River of Teachers ALPHA", titleWidth = 350)

sidebar<-dashboardSidebar(
  sidebarMenu(
    
    dateInput('dateStart',
      label = paste('Start Date: '),
      value = "2012-09-01",
      min = "2012-09-01", max = as.character(Sys.Date()),
      format = "mm/dd/yyyy",
      startview = 'year', weekstart = 1
    ),
    
    dateInput('dateEnd',
              label = paste('End Date: '),
              value = as.character(Sys.Date()),
              min = "2012-09-01", max = "2017-09-01",
              format = "mm/dd/yyyy",
              startview = 'year', weekstart = 1
    ),    
    
    # selectInput("yearInput", "Year of DOE Entry",choices = c(2012, 2013, 2014, 2015, 2016)),
    checkboxGroupInput("tppInput", "Teacher Preparation Program",choices = c('Columbia', 'TFA', 'CUNY', 'Other', 'NYU','Baruch','Brown','Rutgers'), selected=c('Columbia', 'TFA', 'CUNY', 'Other', 'NYU','Baruch','Brown','Rutgers')),
    selectInput("dbnInput", "Restrict to DBN", choices=c("All DBNs", "00X001", "00X002","00X003")),
    # menuItem("River", tabName = "d3"),
    menuItem("River", tabName = "d3", badgeLabel = "d3", badgeColor="green")
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
    tabItem(tabName= "d3",
            fluidRow(
              valueBoxOutput("newHires"),
              valueBoxOutput("exit1"),
              valueBoxOutput("exit2")
            ),
            
            fluidRow(
              box(
                width = 12, status = "info", solidHeader = TRUE,
                title = "Sankey Flowchart Across Time",
                sankeyNetworkOutput("Sankey2", width="1600px", height="800px")
              )
            )
    )
  )
)

  
  
dashboardPage(header, sidebar, body)
