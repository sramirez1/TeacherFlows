library(shiny)
server <- function(input, output) {

  #Use googleVis sankey?
  google=TRUE
  
  ##Create temporary dataset for Sankey
  set.seed(1983)
  
  #Vector of rating categories
  ratings<-c('Highly Effective', 'Effective', 'Developing', 'Ineffective')
  
  #Simulate Data Entry to Ratings in 2013-2014
  entry <- data_frame(origins = sample(c('Columbia', 'TFA', 'CUNY', 'Other'), size = 100, replace = TRUE), 
                      destinations = sample(c("DOE Entry 2012-2013"), size = 100, replace = TRUE))
  
  entry1 <- data_frame(origins = sample(c("DOE Entry 2012-2013"), size = 100, replace = TRUE), 
                       destinations = sample(paste(ratings, "2013-2014"), size = 100, replace = TRUE))
  
  #Flow from Ratings in 2013-2014 to Ratings in 2013-2014
  df1<- data_frame(origins = sample(paste(ratings, "2013-2014"), size = 100, replace = TRUE), 
                    destinations = sample(paste(c(ratings,"Exit"), "2014-2015"), size = 100, replace = TRUE))%>%
        arrange(origins)
  
  #Flow from Ratings in 2014-2015 to Ratings in 2015-2016
  df2 <- data_frame(origins = sample(paste(ratings, "2014-2015"), size = 100, replace = TRUE), 
                    destinations = sample(paste(c(ratings, "Exit"), "2015-2016"), size = 100, replace = TRUE))
  
  #Flow from Exit in 2016-2016 to Exit Reason
  df3 <- data_frame(origins = sample(c('Exit 2015-2016'), size = 100, replace = TRUE), 
                    destinations = sample(c("Removal","Relocation","Retirement","Morbidity"), size = 100, replace = TRUE))
  
  #Stack flow dataframes
  flow<-entry%>%
    rbind(entry1, df1, df2, df3)%>%
    as.data.frame()

  #Calculate flow counts by origin and destination
  flowCounts <-flow %>%
    group_by(origins, destinations) %>%
    summarise(counts=n()) %>%
    ungroup()
    
  #Vector of unique origin and destinations
  name_vec <- unique(c(unique(flowCounts$origins), unique(flowCounts$destinations)))
  
  #Map node id to each name
  nodes <- reactive({
    data.frame(name = name_vec[name_vec!=input$tppInput], id = 0:(length(name_vec[name_vec!=input$tppInput])-1))
  })


  #Add colors to links
  colors_link <- c('green', 'blue', 'yellow', 'brown', 'red')
  colors_link_array <- paste0("[", paste0("'", colors_link,"'", collapse = ','), "]")
  
  links <- reactive({
    flowCounts %>%
    left_join(nodes(), by = c('origins' = 'name')) %>%
    rename(origin_id = id) %>%
    left_join(nodes(), by = c('destinations' = 'name')) %>%
    rename(dest_id = id)%>%
    filter(origins!=input$tppInput)
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
    #                 options=list(title="Hello World", height=800, width=1600,
    #                              sankey=opts))


# "{
#                                link:{color:{fill: 'lightgray', fillOpacity: 0.5}},
#                                node:{nodePadding: 5, label:{fontSize: 12}, interactivity: true, width: 20},
#                                }")
#                     )


    output$Sankey<-renderGvis({
      
      gvisSankey(links(), from="origins", to="destinations", weight="counts",
                 options=list(title="Hello World", height=800, width=1600,
                              sankey=opts))
      
      })
  }
  
  

  
  filtered<-reactive({
    babynames %>%
    filter(name=="Jorge") %>%
    group_by(year, name) %>%
    tally(wt=n)})
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