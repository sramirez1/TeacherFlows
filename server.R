library(shiny)
server <- function(input, output) {

  ##############################
  ###Main Visualization Panel###
  ##############################
  
  ##Create temporary dataset for Sankey
  set.seed(1983)
  
  #Vector of rating categories
  ratings <- c('1.Highly Effective', '2.Effective', '3.Developing', '4.Ineffective')
  
  #Vector of Teacher Preparation Programs
  programs <- c('Columbia', 'TFA', 'CUNY', 'Other', 'NYU','Baruch','Brown','Rutgers')
  
  #Create function to simulate flow data from origin to destination
  originDestination <- function(origin = ratings, originYr = " 2012-2013",
                              destination = ratings, destYr = " 2013-2014",
                              sizes = 1500, probs = c(.1,.75,.1,.05), year = 2012){
    
    df <- data_frame(origins = sample(paste0(origin, originYr), size = sizes, replace = TRUE), 
               destinations = sample(paste0(destination, destYr), prob = probs, size = sizes, replace = TRUE),
               entryYear = year) %>%
      as.data.frame()
   return(df) 
  }
    
  #Simulate Data Entry in 2012, 2013, 2014
  entry <-rbind(originDestination(origin = programs, originYr = "", destination = c("DOE Entry 2012-2013"), destYr = "", probs = NULL),
                originDestination(origin = programs, originYr = "", destination = c("DOE Entry 2013-2014"), destYr = "", size = 400, probs = NULL, year = 2013),
                originDestination(origin = programs, originYr = "", destination = c("DOE Entry 2014-2015"), destYr = "", size = 200, probs = NULL, year = 2014),
                originDestination(origin = programs, originYr = "", destination = c("DOE Entry 2015-2016"), destYr = "", size = 125, probs = NULL, year = 2015),
                originDestination(origin = programs, originYr = "", destination = c("DOE Entry 2016-2017"), destYr = "", size = 125, probs = NULL, year = 2016)) %>%
    mutate(program = origins,
           DBN = paste0("00","X",sprintf("%03d",sample(c(1:300), n(), replace = TRUE))))
  
  ratings <- sample(c(ratings, "5.Exit"), size = 1000, prob = c(.1,.7,.1,.05,.05), replace = TRUE)
  
  #Flow from Entry to Ratings in that year
  entry1 <- entry %>%
    select(destinations, entryYear, program, DBN) %>%
    rename(origins = destinations) %>%
    group_by(entryYear) %>%
    mutate(destinations = sample(paste0(ratings, yr = paste0(" ",entryYear,"-",entryYear + 1)), n(), replace = TRUE, prob = NULL)) %>%
    filter(entryYear != 2016) %>%
    as.data.frame()
  
  
  #Flow from Ratings in Year 1 to Year2
  df1 <- entry1 %>%
    select(destinations, entryYear, program, DBN) %>%
    rename(origins = destinations) %>%
    filter(entryYear != 2015, substr(origins,1,6) != "5.Exit") %>%
    group_by(entryYear) %>%
    mutate(destinations = paste0(sample(ratings, n(), replace = TRUE, prob = NULL),paste0(" ",entryYear + 1,"-",entryYear + 2))) %>%
    as.data.frame()
  
  
  # #Flow from Ratings in Year 2 to Year 3
  df2 <- df1 %>%
    select(destinations, entryYear,program, DBN) %>%
    rename(origins = destinations) %>%
    filter(entryYear != 2014, substr(origins,1,6) != "5.Exit") %>%
    group_by(entryYear) %>%
    mutate(destinations = paste0(sample(ratings, n(), replace = TRUE, prob = NULL),paste0(" ",entryYear + 2,"-",entryYear + 3))) %>%
    as.data.frame()

  # #Flow from Ratings in Year 2 to Year 3
  df3 <- df2 %>%
    select(destinations, entryYear,program, DBN) %>%
    rename(origins = destinations) %>%
    filter(entryYear != 2013, substr(origins,1,6) != "5.Exit") %>%
    group_by(entryYear) %>%
    mutate(destinations = paste0(sample(ratings, n(), replace = TRUE, prob = NULL),paste0(" ",entryYear + 3,"-",entryYear + 4))) %>%
    as.data.frame()

  ###############################
  # Section for non-reactive data
  ###############################
  #Stack flow dataframes
  flowd3 <- entry %>%
      rbind(entry1, df1,df2, df3) %>%
      as.data.frame()
  
  #Calculate flow counts by origin and destination
  flowCountsd3 <- flowd3 %>%
      group_by(origins, destinations) %>%
      summarise(counts = n()) %>%
      ungroup() %>%
      as.data.frame()
  
  #Vector of unique origin and destinations
  name_vecd3 <- unique(c(unique(flowCountsd3$origins), unique(flowCountsd3$destinations)))
  
  #Map node id to each name
  nodesd3 <- data.frame(name = name_vecd3, id = 0:(length(name_vecd3) - 1))
  
  linksd3 <- flowCountsd3 %>%
      left_join(nodesd3, by = c('origins' = 'name')) %>%
      rename(origin_id = id) %>%
      left_join(nodesd3, by = c('destinations' = 'name')) %>%
      rename(dest_id = id)
  
  #Vector of rating categories
  ratings <- c('1.Highly Effective', '2.Effective', '3.Developing', '4.Ineffective', '5.Exit')
  
  #Convert origins and destinations to factor
  codes <- c(paste0(ratings, " 2012-2013"),
             paste0(ratings, " 2013-2014"),
             paste0(ratings, " 2014-2015"),
             paste0(ratings, " 2015-2016"))
  
  codes<- factor(codes, levels = codes)
  
  codesJS <- paste(shQuote(codes, type = "sh"), collapse = ' , ')
  
  
  ###############
  #Add reactivity
  ###############
  entryR <- reactive({
    if (input$dbnInput != "All DBNs"){
      entry %>%
        filter(program %in% c(input$tppInput), entryYear == input$yearInput, DBN == input$dbnInput) 
    }else {
      entry %>%
        filter(program %in% c(input$tppInput), entryYear == input$yearInput) 
    }
  })
  
  #Count the number of rows we want to simulate based on the reactive filters
  rows <- reactive({c(nrow(entryR()))})  

  #Add reactivity
  entry1R <- reactive({
    if (input$dbnInput != "All DBNs"){
      entry1 %>%
        filter(program %in% c(input$tppInput), entryYear == input$yearInput, DBN == input$dbnInput) 
    }else {
      entry1 %>%
        filter(program %in% c(input$tppInput), entryYear == input$yearInput) 
    }
  })
  
  df1R <- reactive({
    if (input$dbnInput != "All DBNs"){
      df1 %>%
        filter(program %in% c(input$tppInput), entryYear == input$yearInput, DBN == input$dbnInput) 
    }else {
      df1 %>%
        filter(program %in% c(input$tppInput), entryYear == input$yearInput) 
    }
  })

  df2R <- reactive({
    if (input$dbnInput != "All DBNs"){
      df2 %>%
        filter(program %in% c(input$tppInput), entryYear == input$yearInput, DBN == input$dbnInput) 
    }else {
      df2 %>%
        filter(program %in% c(input$tppInput), entryYear == input$yearInput) 
    }
  })

  df3R <- reactive({
    if (input$dbnInput != "All DBNs"){
      df3 %>%
        filter(program %in% c(input$tppInput), entryYear == input$yearInput, DBN == input$dbnInput) 
    }else {
      df3 %>%
        filter(program %in% c(input$tppInput), entryYear == input$yearInput) 
    }
  })

  #Stack flow dataframes
  flow <- reactive({
    # if (input$dbnInput!="All DBNs"){
    #   entryR()%>%
    #     rbind(entry1R(), df1R(),df2R(), df3R())%>%
    #     as.data.frame()%>%
    #     filter(DBN==input$dbnInput)
    # }else {
    entryR() %>%
      rbind(entry1R(), df1R(),df2R(), df3R()) %>%
      as.data.frame()
    # }
  })

  #Calculate flow counts by origin and destination
  flowCounts <- reactive({
    flow() %>%
    group_by(origins, destinations) %>%
    summarise(counts = n()) %>%
    ungroup() %>%
    as.data.frame()
  })
    
  #Vector of unique origin and destinations
  name_vec <- reactive({
    unique(c(unique(flowCounts()$origins), unique(flowCounts()$destinations)))
  })
  
  #Map node id to each name
  nodes <- reactive({
    data.frame(name = name_vec, id = 0:(length(name_vec) - 1))
  })


  #Add colors to links
  colors_link <- c('green', 'blue', 'yellow', 'brown', 'red')
  colors_link_array <- paste0("[", paste0("'", colors_link,"'", collapse = ','), "]")
  
  links <- reactive({
    t <- flowCounts() %>%
    left_join(nodes(), by = c('origins' = 'name')) %>%
    rename(origin_id = id) %>%
    left_join(nodes(), by = c('destinations' = 'name')) %>%
    rename(dest_id = id)
  })
  

  ##Generate sankey diagram using networkD3
  # sankey<-sankeyNetwork(Links=links, Nodes = nodes, Source = 'origin_id', Target = 'dest_id',
  #               Value = 'counts', NodeID = 'name', fontSize = 16, sinksRight=FALSE, height=1500, width=2000
  #               )

  output$Sankey2 <- renderSankeyNetwork({
    sankeyNetwork(Links = linksd3,
                  Nodes = nodesd3,
                  Source = 'origin_id',
                  Target = 'dest_id', 
                  Value = 'counts',
                  NodeID = 'name',
                  # NodePosX = 'id',
                  LinkGroup = 'origins',
                  linkGradient = TRUE,
                  fontSize = 16,
                  fontFamily = "Helvetica",
                  highlightChildLinks = TRUE,
                  doubleclickTogglesChildren = TRUE,
                  zoom = TRUE,
                  showNodeValues = FALSE,
                  orderByPath = TRUE,
                  dragX = TRUE,
                  dragY = TRUE,
                  align = "left", 
                  curvature = .8,
                  linkOpacity = 0.25,
                  nodeLabelMargin = 5,
                  xScalingFactor = 1,
                  colourScale = JS(paste0("d3.scaleOrdinal()
                                          .domain([",codesJS,"])
                                          .range(['#78a8e1','#BAE58A','#FFC76B','#F8EE6E','#9933FF'])
                                          .unknown(['#ccc'])"))
                  )
  })
  

  opts <- paste0("{iterations : 0,
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


  output$Sankey <- renderGvis({
    req(input$tppInput)
    if (rows() <= 5){
      gvisSankey(flowCounts(), from = "origins", to = "destinations", 
                 weight = "counts", options = list(height = 200, width = 1600, sankey = opts))
    }else if(rows() <= 15){
      gvisSankey(flowCounts(), from = "origins", to = "destinations", weight = "counts",
               options = list(height = 400, width = 1600, sankey = opts))
    }else {
      gvisSankey(flowCounts(), from = "origins", to = "destinations", weight = "counts",
                 options = list(height = 800, width = 1600, sankey = opts))
    }
    })
###############################  
### Reactive Summary Table ####
###############################
  
  filtered <- reactive({
    df1R() %>%
    mutate(total = n()) %>%
    group_by(destinations, total) %>%
    summarise(look = n()) %>%
    mutate(pct = 100 * (look / total)) %>%
    select(destinations, pct)})
  output$results <- DT::renderDataTable(filtered(), options = list(pageLength = 5))

############################  
### Reactive Value Boxes ###
############################
  output$newHires <- renderValueBox({
    valueBox(value = rows(),
             subtitle = "New Hires",
             icon = icon("users"),
             width = NULL
    )
  })
  
  exit1 <- reactive({
    df1R() %>%
    filter(substr(destinations,1,4) == "Exit") %>%
    count()
  })
  
  output$exit1 <- renderValueBox({
    valueBox(value = exit1(),
             subtitle = "Exits after Year 1",
             icon = icon("sign-out"),
             width = NULL,
             color = "purple"
    )
  })
  
  exit2 <- reactive({
    df2R() %>%
      filter(substr(destinations,1,4) == "Exit") %>%
      count()
  })

  output$exit2 <- renderValueBox({
    valueBox(value = exit2(),
             subtitle = "Exits after Year 2",
             icon = icon("sign-out"),
             width = NULL,
             color = "yellow"
    )
  })
  
}