library(shiny)
server <- function(input, output) {

  ##############################
  ###Main Visualization Panel###
  ##############################
  #Use googleVis sankey?
  google=TRUE
  
  ##Create temporary dataset for Sankey
  set.seed(1983)
  
  #Vector of rating categories
  ratings<-c('Highly Effective', 'Effective', 'Developing', 'Ineffective')
  
  #Vector of Teacher Preparation Programs
  programs<-c('Columbia', 'TFA', 'CUNY', 'Other', 'NYU','Baruch','Brown','Rutgers')
  
  #Create function to simulate flow data from origin to destination
  originDestination<-function(origin=ratings, originYr=" 2012-2013",
                              destination=ratings, destYr=" 2013-2014",
                              sizes=rows(), probs=c(.1,.75,.1,.05), year=2012){
    
    df<-data_frame(origins = sample(paste0(origin, originYr), size =sizes, replace = TRUE), 
               destinations = sample(paste0(destination, destYr), prob=probs, size =sizes, replace = TRUE),
               entryYear=year)%>%
      as.data.frame()
   return(df) 
  }
  
  #Simulate Data Entry in 2012, 2013, 2014
  entry <-reactive({
    rbind(
    originDestination(origin = programs, originYr="", destination=c("DOE Entry 2012-2013"), destYr = "", size=1500, probs = NULL),
    originDestination(origin = programs, originYr="", destination=c("DOE Entry 2013-2014"), destYr = "", size=400, probs = NULL, year=2013),
    originDestination(origin = programs, originYr="", destination=c("DOE Entry 2014-2015"), destYr = "", size=200, probs = NULL, year=2014),
    originDestination(origin = programs, originYr="", destination=c("DOE Entry 2015-2016"), destYr = "", size=125, probs = NULL, year=2015),
    originDestination(origin = programs, originYr="", destination=c("DOE Entry 2016-2017"), destYr = "", size=125, probs = NULL, year=2016)
    )%>%
    filter(origins %in% c(input$tppInput), entryYear==input$yearInput)
  })
  
  #Count the number of rows we want to simulate based on the reactive filters
  rows<-reactive({c(nrow(entry()))})
  
  #Flow from Entry to Ratings in that year
  entry1 <- reactive({
    rbind(
    originDestination(origin="DOE Entry 2012-2013", originYr = "", destYr = " 2012-2013"),
    originDestination(origin="DOE Entry 2013-2014", originYr = "", destYr = " 2013-2014", year=2013),
    originDestination(origin="DOE Entry 2014-2015", originYr = "", destYr = " 2014-2015", year=2014),
    originDestination(origin="DOE Entry 2015-2016", originYr = "", destYr = " 2015-2016", year=2015))%>%
    filter(entryYear==input$yearInput)

    # data_frame(origins = sample(c("DOE Entry 2012-2013"), size = rows(), replace = TRUE),
    #                    destinations = sample(paste(ratings, "2013-2014"),prob=c(.1,.75,.1,.05), size =rows(), replace = TRUE))
  })
  observe(print(table(entry1()$destination)))
  #Flow from Ratings in Year 1 to Year2
  df1<- reactive({
    rbind(
    originDestination(originYr = " 2012-2013", destination = c(ratings, "Exit"), destYr = " 2013-2014", probs=NULL),
    originDestination(originYr = " 2013-2014", destination = c(ratings, "Exit"), destYr = " 2014-2015", probs=NULL, year=2013),
    originDestination(originYr = " 2014-2015", destination = c(ratings, "Exit"), destYr = " 2015-2016", probs=NULL, year=2014))%>%
    filter(entryYear==input$yearInput)
    # data_frame(origins = sample(paste(ratings, "2013-2014"), size = rows(), replace = TRUE), 
    #                 destinations = sample(paste(c(ratings,"Exit"), "2014-2015"),prob=c(.1,.65,.1,.05, .1), size = rows(), replace = TRUE))
  })
  observe(print(table(df1()$destination)))
  
  #Flow from Ratings in Year 2 to Year 3
  df2 <-  reactive({
    rbind(
    originDestination(originYr = " 2013-2014",destination = c(ratings, "Exit"), destYr = " 2014-2015",probs=c(.1,.65,.1,.05,.1)),
    originDestination(originYr = " 2014-2015",destination = c(ratings, "Exit"), destYr = " 2015-2016",probs=NULL, year=2013))%>%
    filter(entryYear==input$yearInput)
    
    # data_frame(origins = sample(paste(ratings, "2014-2015"), size = rows(), replace = TRUE), 
    #                 destinations = sample(paste(c(ratings, "Exit"), "2015-2016"), size = rows(), replace = TRUE))
  })
  
  #Flow from Ratings in Year 2 to Year 3
  df3 <-  reactive({
    rbind(
      originDestination(originYr = " 2014-2015",destination = c(ratings, "Exit"), destYr = " 2015-2016",probs=c(.15,.55,.15,.05,.1)))%>%
      filter(entryYear==input$yearInput)
    # data_frame(origins = sample(paste(ratings, "2014-2015"), size = rows(), replace = TRUE), 
    #                 destinations = sample(paste(c(ratings, "Exit"), "2015-2016"), size = rows(), replace = TRUE))
  })
  
  #Flow from Exit to Exit Reason
  # dfD <-reactive({
  #   data_frame(origins = sample(c('Exit 2016-2016'), size = 100, replace = TRUE),
  #              destinations = sample(paste(ratings, "2012-2013"), size = 100, replace = TRUE),
  #              entryYear=1000)
  # })
  
  #Stack flow dataframes
  flow<-reactive({
    entry()%>%
    rbind(entry1(), df1(), df2(), df3())%>%
    as.data.frame()
  })

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
                 options=list(title="Hello World", height=800, width=1600,
                              sankey=opts))
      
      })
  }
###############################  
### Reactive Summary Table ####
###############################
  
  filtered<-reactive({
    df1()%>%
    mutate(total=n()) %>%
    group_by(destinations, total) %>%
    summarise(look=n())%>%
    mutate(pct=100*(look/total))%>%
    select(destinations, pct)})
  output$results <-DT::renderDataTable(filtered(), options = list(pageLength=5))

############################  
### Reactive Value Boxes ###
############################
  output$newHires<- renderValueBox({
    valueBox(value=rows(),
             subtitle="New Hires",
             icon=icon("user"),
             width=NULL
    )
  })
  
  exit1<-reactive({
    df1()%>%
    filter(substr(destinations,1,4)=="Exit")%>%
    count()
  })
  
  output$exit1<- renderValueBox({
    valueBox(value=exit1(),
             subtitle="Exits after Year 1",
             icon=icon("sign-out"),
             width=NULL,
             color="purple"
    )
  })
  
  exit2<-reactive({
    df2()%>%
      filter(substr(destinations,1,4)=="Exit")%>%
      count()
  })

  output$exit2<- renderValueBox({
    valueBox(value=exit2(),
             subtitle="Exits after Year 2",
             icon=icon("sign-out"),
             width=NULL,
             color="yellow"
    )
  })
  
}