#Load libraries
library(shiny)
library(dplyr)
library(googleVis)
library(networkD3)
library(DT)

#Use googleVis sankey?
google=FALSE

ui <- fluidPage(
  
  titlePanel("NYCDOE Teacher Retention Flows"),
  
  if (google==FALSE){
    #Render Sankey with networkD3
    sankeyNetworkOutput("Sankey")
    
  } else {
  #Render sankey with googleVis
  mainPanel(
    htmlOutput("Sankey")

  )
  },
  

    
  fluidRow(),
  fluidRow(
    column(3,
           h4("Diamonds Explorer"),
           sliderInput('sampleSize', 'Sample Size', 
                       min=1, max=53000,
                       value=min(1000, 53000), 
                       step=500, round=0)
    ),
    column(9,
           DT::dataTableOutput("results")
    )
  )
  
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
)