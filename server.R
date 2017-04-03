library(shiny)
server <- function(input, output) {

  #Use googleVis sankey?
  google=TRUE
  
  ##Create temporary dataset for Sankey
  set.seed(1983)
  
  #Vector of rating categories
  ratings<-c('Highly Effective', 'Effective', 'Developing', 'Ineffective')
  
  #Vector of Teacher Preparation Programs
  programs<-c('Columbia', 'TFA', 'CUNY', 'Other', 'NYU','Baruch','Brown','Rutgers')
  
  #Create function to simulate flow data from origin to destination
  originDestination<-function(origin=ratings, originYear="2012-2013",
                              destination=ratings, destinationYear="2013-2014",
                              sizes=rows(), probs=c(.1,.75,.1,.05)){
    
    df<-data_frame(origins = sample(paste0(origin, originYear), size =sizes, replace = TRUE), 
               destinations = sample(paste0(destination, destinationYear), prob=probs, size =sizes, replace = TRUE))%>%
      as.data.frame()
   return(df) 
  }
  
  #Simulate Data Entry to Ratings in 2013-2014
  entry <-reactive({
    data_frame(origins = sample(programs, size = 1000, replace = TRUE),
               destinations = sample(c("DOE Entry 2012-2013"), size = 1000, replace = TRUE))%>%
    filter(origins %in% c(input$tppInput))
  })
  
  rows<-reactive({c(nrow(entry()))})
  
  entry1 <- reactive({
    originDestination(origin="DOE Entry 2012-2013", originYear = "",
                      destination = ratings, destinationYear = " 2013-2014")
    # data_frame(origins = sample(c("DOE Entry 2012-2013"), size = rows(), replace = TRUE),
    #                    destinations = sample(paste(ratings, "2013-2014"),prob=c(.1,.75,.1,.05), size =rows(), replace = TRUE))
  })

  #Flow from Ratings in 2013-2014 to Ratings in 2013-2014
  df1<- reactive({
    originDestination(originYear = " 2013-2014",
                      destination = c(ratings, "Exit"), destinationYear = " 2014-2015",
                      probs=c(.1,.65,.1,.05,.1))
    # data_frame(origins = sample(paste(ratings, "2013-2014"), size = rows(), replace = TRUE), 
    #                 destinations = sample(paste(c(ratings,"Exit"), "2014-2015"),prob=c(.1,.65,.1,.05, .1), size = rows(), replace = TRUE))
  })
  
  #Flow from Ratings in 2014-2015 to Ratings in 2015-2016
  df2 <-  reactive({
    originDestination(originYear = " 2014-2015",
                      destination = c(ratings, "Exit"), destinationYear = " 2015-2016",
                      probs=c(.1,.65,.1,.05,.1))
    # data_frame(origins = sample(paste(ratings, "2014-2015"), size = rows(), replace = TRUE), 
    #                 destinations = sample(paste(c(ratings, "Exit"), "2015-2016"), size = rows(), replace = TRUE))
  })
  
  #Flow from Exit in 2016-2016 to Exit Reason
  # df3 <-reactive({
  #   data_frame(origins = sample(c('Exit 2015-2016'), size = rows(), replace = TRUE), 
  #                   destinations = sample(c("Removal","Relocation","Retirement","Morbidity"), size = rows(), replace = TRUE))
  # })
  
  #Stack flow dataframes
  flow<-reactive({
    entry()%>%
    rbind(entry1(), df1(), df2())%>%
    as.data.frame()
  })
observe(print(flow()))
  #Calculate flow counts by origin and destination
  flowCounts <-reactive({
    flow() %>%
    group_by(origins, destinations) %>%
    summarise(counts=n()) %>%
    ungroup()
  })
    
  #Vector of unique origin and destinations
  name_vec <- reactive({
    unique(c(unique(flowCounts()$origins), unique(flowCounts()$destinations)))
  })
  
  #Map node id to each name
  nodes <- reactive({
    data.frame(name = name_vec, id = 0:(length(name_vec)-1))
  })


  #Add colors to links
  colors_link <- c('green', 'blue', 'yellow', 'brown', 'red')
  colors_link_array <- paste0("[", paste0("'", colors_link,"'", collapse = ','), "]")
  
  links <- reactive({
    flowCounts()
    # left_join(nodes(), by = c('origins' = 'name')) %>%
    # rename(origin_id = id) %>%
    # left_join(nodes(), by = c('destinations' = 'name')) %>%
    # rename(dest_id = id)%>%
  })
  

  if (google==FALSE){
    
    ##Generate sankey diagram using networkD3
    # sankey<-sankeyNetwork(Links=links, Nodes = nodes, Source = 'origin_id', Target = 'dest_id',
    #               Value = 'counts', NodeID = 'name', fontSize = 16, sinksRight=FALSE, height=1500, width=2000
    #               )
  
    output$Sankey<-renderSankeyNetwork(
      sankeyNetwork(Links=links(), Nodes = nodes(), Source = 'origin_id', Target = 'dest_id',
                            Value = 'counts', NodeID = 'name', fontSize = 16, sinksRight=FALSE, height=1500, width=2000
                    )
      )
    
  } else{
    
    opts <- paste0("{
        link: { colorMode: 'source',
                color: { fill: 'lightgray', fillOpacity: 0.1},
                node:{nodePadding: 5, label:{fontSize: 14}, interactivity: true, width: 20}}
      }" )
    
    ##Generate sankey diagram using googleVis
    # sankey<-gvisSankey(links, from="origins", to="destinations", weight="counts",
    #                 options=list(title="Hello World", height=800, width=1200,
    #                              sankey=opts))


# "{
#                                link:{color:{fill: 'lightgray', fillOpacity: 0.5}},
#                                node:{nodePadding: 5, label:{fontSize: 12}, interactivity: true, width: 20},
#                                }")
#                     )


    output$Sankey<-renderGvis({
      
      gvisSankey(links(), from="origins", to="destinations", weight="counts",
                 options=list(title="Hello World", height=800, width=1200,
                              sankey=opts))
      
      })
  }
  
  

  
  filtered<-reactive({
    df1()%>%
    mutate(total=n()) %>%
    group_by(destinations, total) %>%
    summarise(look=n())%>%
    mutate(pct=100*(look/total))%>%
    select(destinations, pct)})
  output$results <-DT::renderDataTable(filtered(), options = list(pageLength=5))
  
  # filtered <- reactive({
  #   bcl %>%
  #     filter(Price >= input$priceInput[1],
  #            Price <= input$priceInput[2],
  #            Type == input$typeInput,
  #            Country == input$countryInput
  #     )
  # })
  # 
  # output$coolplot <- renderPlot({
  #   ggplot(filtered(), aes(Alcohol_Content)) +
  #     geom_histogram()
  # })
  # 
  # output$results <- renderTable({
  #   filtered()
  # })
  # observe({print(input$priceInput)})
  # priceDiff <- reactive({
  #   diff(input$priceInput)
  # })
  # observe({print(priceDiff())})
}