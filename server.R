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
                              sizes=1500, probs=c(.1,.75,.1,.05), year=2012){
    
    df<-data_frame(origins = sample(paste0(origin, originYr), size =sizes, replace = TRUE), 
               destinations = sample(paste0(destination, destYr), prob=probs, size =sizes, replace = TRUE),
               entryYear=year)%>%
      as.data.frame()
   return(df) 
  }

  
  #Create function to simulate flow data from origin to destination
  samp<-function(base=ratings, yr=" 2012-2013", probs=c(.1,.75,.1,.05)){
    
    df<-sample(paste0(base, yr), prob=probs, replace = TRUE)
  
    return(df) 
  }
  
  
    
  #Simulate Data Entry in 2012, 2013, 2014
  entry <-rbind(originDestination(origin = programs, originYr="", destination=c("DOE Entry 2012-2013"), destYr = "", probs = NULL),
                originDestination(origin = programs, originYr="", destination=c("DOE Entry 2013-2014"), destYr = "", size=400, probs = NULL, year=2013),
                originDestination(origin = programs, originYr="", destination=c("DOE Entry 2014-2015"), destYr = "", size=200, probs = NULL, year=2014),
                originDestination(origin = programs, originYr="", destination=c("DOE Entry 2015-2016"), destYr = "", size=125, probs = NULL, year=2015),
                originDestination(origin = programs, originYr="", destination=c("DOE Entry 2016-2017"), destYr = "", size=125, probs = NULL, year=2016))%>%
    mutate(program=origins)
  
  ratings<-sample(c(ratings, "Exit"), size=1000, prob=c(.1,.7,.1,.05,.05), replace=TRUE)
  
  #Flow from Entry to Ratings in that year
  entry1 <-entry%>%
    select(destinations, entryYear, program)%>%
    rename(origins=destinations)%>%
    group_by(entryYear)%>%
    mutate(destinations=sample(paste0(ratings, yr=paste0(" ",entryYear,"-",entryYear+1)), n(), replace=TRUE, prob=NULL))%>%
    filter(entryYear!=2016)%>%
    as.data.frame()
  
  
  #Flow from Ratings in Year 1 to Year2
  df1<-entry1%>%
    select(destinations, entryYear, program)%>%
    rename(origins=destinations)%>%
    filter(entryYear!=2015, substr(origins,1,4)!="Exit")%>%
    group_by(entryYear)%>%
    mutate(destinations=paste0(sample(ratings, n(), replace=TRUE, prob=NULL),paste0(" ",entryYear+1,"-",entryYear+2)))%>%
    as.data.frame()
  
  
  # #Flow from Ratings in Year 2 to Year 3
  df2 <-df1%>%
    select(destinations, entryYear,program)%>%
    rename(origins=destinations)%>%
    filter(entryYear!=2014, substr(origins,1,4)!="Exit")%>%
    group_by(entryYear)%>%
    mutate(destinations=paste0(sample(ratings, n(), replace=TRUE, prob=NULL),paste0(" ",entryYear+2,"-",entryYear+3)))%>%
    as.data.frame()

  # #Flow from Ratings in Year 2 to Year 3
  df3 <-df2%>%
    select(destinations, entryYear,program)%>%
    rename(origins=destinations)%>%
    filter(entryYear!=2013, substr(origins,1,4)!="Exit")%>%
    group_by(entryYear)%>%
    mutate(destinations=paste0(sample(ratings, n(), replace=TRUE, prob=NULL),paste0(" ",entryYear+3,"-",entryYear+4)))%>%
    as.data.frame()

  #Add reactivity
  entryR <-reactive({
    entry%>%
      filter(program %in% c(input$tppInput), entryYear==input$yearInput)
  })
  
  #Count the number of rows we want to simulate based on the reactive filters
  rows<-reactive({c(nrow(entryR()))})  
  
  #Add reactivity
  entry1R <-reactive({
    entry1%>%
    filter(program %in% c(input$tppInput), entryYear==input$yearInput)
  })
  
  df1R <-reactive({
    df1%>%
      filter(program %in% c(input$tppInput), entryYear==input$yearInput)
  })

  df2R <-reactive({
    df2%>%
      filter(program %in% c(input$tppInput), entryYear==input$yearInput)
  })

  df3R <-reactive({
    df3%>%
      filter(program %in% c(input$tppInput), entryYear==input$yearInput)
  })

  #Stack flow dataframes
  flow<-reactive({
    entryR()%>%
    rbind(entry1R(), df1R(),df2R(), df3R())%>%
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
        link: {colorMode:'source', color: {fillOpacity: 0.7}},
                node:{nodePadding: 50, label:{fontSize: 13}, interactivity: true, width: 20}}" )
    
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
    df1R()%>%
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
    df1R()%>%
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
    df2R()%>%
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