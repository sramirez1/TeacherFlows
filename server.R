library(shiny)
server <- function(input, output) {

  ##Create temporary dataset for Sankey
  set.seed(1983)
  
  #Vector of rating categories
  ratings<-c('Highly Effective', 'Effective', 'Developing', 'Ineffective')
  
  #Simulate Data Entry to Ratings in 2013-2014
  entry <- data_frame(origins = sample(c('Columbia', 'TFA', 'CUNY', 'Other'), size = 100, replace = TRUE), 
                   destinations = sample(paste(ratings, "2013-2014"), size = 100, replace = TRUE))
  
  #Flow from Ratings in 2013-2014 to Ratings in 2013-2014
  df1<- data_frame(origins = sample(paste(ratings, "2013-2014"), size = 100, replace = TRUE), 
                    destinations = sample(paste(c(ratings,"Exit"), "2014-2015"), size = 100, replace = TRUE))
  
  #Flow from Ratings in 2014-2015 to Ratings in 2015-2016
  df2 <- data_frame(origins = sample(paste(ratings, "2014-2015"), size = 100, replace = TRUE), 
                    destinations = sample(paste(c(ratings, "Exit"), "2015-2016"), size = 100, replace = TRUE))
  #Stack flow dataframes
  flow<-entry%>%
    rbind(df1, df2)%>%
    as.data.frame()
  
  #Calculate flow counts by origin and destination
  flowCounts <- flow %>%
    group_by(origins, destinations) %>%
    summarise(counts=n()) %>%
    ungroup() %>%
    arrange(desc(counts))
  
  #Vector of unique origin and destinations
  name_vec <- unique(c(unique(flowCounts$origins), unique(flowCounts$destinations)))
  
  #Map node id to each name
  nodes <- data.frame(name = name_vec, id = 0:(length(name_vec)-1))


  links <- flowCounts %>%
    left_join(nodes, by = c('origins' = 'name')) %>%
    rename(origin_id = id) %>%
    left_join(nodes, by = c('destinations' = 'name')) %>%
    rename(dest_id = id)%>%
    arrange(origins)
  
  ##Generate sankey diagram using networkD3
  
  # sankey<-sankeyNetwork(Links = links, Nodes = nodes, Source = 'origin_id', Target = 'dest_id',
  #               Value = 'counts', NodeID = 'name', fontSize = 16, sinksRight=FALSE, height=1000, width=2000
  #               )
  # 
  # output$Sankey<-renderSankeyNetwork(sankey)
  
  
  ##Generate sankey diagram using googleVis
  sankey<-gvisSankey(links, from="origins", to="destinations", weight="counts",
                  options=list(height=800, width=850,
                               sankey="{
                             link:{color:{fill: 'lightgray', fillOpacity: 0.7}},
                             node:{nodePadding: 5, label:{fontSize: 12}, interactivity: true, width: 20},
                             }")
                  )


  output$Sankey<-renderGvis({
    sankey
    })
  
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