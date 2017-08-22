library(shiny)
library(solrium)
library(dplyr)
library(DT)
library(shinydashboard)

#################
# helper function to connect to SOLR
conn<-solr_connect("http://192.168.0.135:8080/solr/select", errors = 'complete', verbose=F)  #older solr 5.5
#solr_connect("http://192.168.0.123:8080/solr/select", errors = 'complete', verbose=F) # new solr 6.0
checkforsolr<-function() {checkforsolr=((Sys.getenv("SOLR_ERRORS")))}
cat(checkforsolr())

#show environment 
#sessionInfo()




create_q <- function(descr, paid = NA, coicop = NA, shop=NA){
  # finetuning the query options is essential for the estimator performance
  # see examples here: http://yonik.com/solr/query-syntax/
  words <- strsplit(descr, "\\s")[[1]]
  words <- words[nchar(words)>0]   # drop empty words
  q_descr <- paste0('(cleandescr:"',descr,'"^2)') # get points for full match in the same order
  for (i in 1:length(words))
    q_descr <-  paste0(q_descr, '(cleandescr:"', words[i], '"^', 2/(2+i),')') #get points for each matched word 
  
  if (is.na(paid)) q_paid='' else  q_paid <- paste0('(paid:', paid, ')', # get points for exact price match
                                                    '(paid:[', paid-1, ' TO ', paid+1,'])', # price within +-1
                                                    '(paid:[', floor(paid*.9), ' TO ', ceiling(paid*1.1),'])',  # price within +- 10% 
                                                    '(paid:[', floor(paid*.2), ' TO ', ceiling(paid*2),'])')  # price within +- 50% 
  
  if (is.na(coicop)) q_coicop=''  else  q_coicop = paste0('(+coicop:', coicop, ')') # require coicop if available
  
  if (is.na(shop))   q_shop=''    else  q_shop = paste0('(shop:"', shop, '"^0.5)')
  
  return(paste0(q_descr, q_paid, q_coicop, q_shop))
}

solr_weight_table <-  function(descr, paid, onlytop=T, rowmax = ROWMAX){
  q <- create_q(descr, paid) #,  shop = row$shop) # coicop = row$coicop,
  res<- solr_all(q=q, rows=rowmax, fl='score')$search
  return(res)
}

solr_coicop <-  function(descr, onlytop=T, rowmax = ROWMAX){
  q <- create_q(descr)#, paid = row$paid)#, shop = row$shop) 
  res <- solr_all(q=q, rows=rowmax, fl='score')$search
  if ((nchar(descr)>0) & (nrow(res) > 0) ){
    if (onlytop) res <- res %>% dplyr::filter(score==max(score))    # use only the best matched products
    res <- res %>% group_by(coicop) %>% summarise(s = sum(score*sqrt(count))) %>% 
      ungroup() %>% arrange(-s)
    res<- res$coicop[1]}
  else res <- NA
  return(res)
}

#################init
#inline = function (x) {tags$div(style="display:inline-block;", x)}
row_names <- c('cleandescr','coicop', 'paid', 'quantity', 'units', 'shop', 'count','score')
ROWMAX <- 12

server <- function(input, output,session) {
  values <- reactiveValues(solr_table = data.frame(row.names=row_names),
                           click_row = data.frame(row.names=row_names),
                           action_status = 'Waiting for input.')
  
  output$instructions <- renderText({
    a<- (  "This is an interactive form demonstrating functionality of real time look-up service for 'Coding' and 'Quantity' value.  
           Please enter the data as in the Blaise questionnaire.
           Instructions:
           1. enter text into the  'EXPDESC' field 
           2. click on the 'Classify' button and 'CODING' field will be automatically populated
           3. enter numerical value into the 'Paid1' field 
           4. click on the 'Look up' button and you will be presented with a look up table of similar items bellow
           5. click on a matching item from the look up table 'SOLR result' and the 'Quantity' and 'Units' field will be filled accordingly"
    )
    a
  })
  
  output$notificationMenu <- renderMenu({
    notificationItem(
      text = if (checkforsolr() != "complete" ) "error" else "OK",
      icon("cloud-upload"),
      status = if (checkforsolr() != "complete" ) "warning" else "success"
    )
  })
  
  observeEvent(input$goButton, {
    # fill in the value for 'CODING' and hide solr_table
    estimate <- solr_coicop(input$EXPDESC)
    if  (!is.na(estimate)) {  #to change with setting the field from the actual data
      updateTextInput(session, "CODING", value = estimate)  
      values$action_status <- paste0('CODING provided based on EXPDESC: "', input$EXPDESC, '"')
    } else {
      updateTextInput(session, "CODING", value = "") 
      values$action_status <- paste0('No CODING available based on EXPDESC: "', input$EXPDESC, '"')
    }
    values$solr_table = data.frame(row.names=row_names)
  })     
  
  observeEvent(input$goButton2, { 
    # populate/update the table 'Solr result' 
    values$solr_table <- solr_weight_table(descr=input$EXPDESC, paid=as.integer(input$Paid1))
    if (nrow(values$solr_table)==0){
      values$solr_table <- data.frame(row.names=row_names)
      values$action_status <- paste0('No LOOK UP table  based on EXPDESC: "', isolate(input$EXPDESC), 
                                     '" and Paid1: "', isolate(input$Paid1), '"')
    } else { values$solr_table <- values$solr_table[row_names]
    values$action_status <- paste0('LOOK UP table provided based on EXPDESC: "', isolate(input$EXPDESC), 
                                   '" and Paid1: "', isolate(input$Paid1), '"')
    }
  })   
  
  output$foodiesreactive <- renderText({ 
    paste0 ('Status: ', values$action_status)
  })
  
  output$rowselect <- renderText({
    s = input$solrresult_row_last_clicked
    if (length(s)) {
      last <-s[length(s)]
      values$clickrow <- isolate(values$solr_table[last,])
      updateTextInput(session, "Quantity", value = values$clickrow$quantity )
      updateTextInput(session, "Units"   , value = values$clickrow$units)
     
      #updateTextInput(session, "EXPDESC"   , value = values$clickrow$cleandescr)
      
      
    }
    if (length(s)) {
      paste0('Quantity and Units values retrieved from the selected row: ', last)
    }
  })
  
  output$solrresult <-  renderDataTable    ({
    DT::datatable(values$solr_table, rownames = TRUE,  selection = 'single', 
                  filter = c("none"), options = list(dom = 't', pageLength = ROWMAX))
    
    
 
    
    
    
  })
  
  output$querytable <- renderUI ({     
    DT::dataTableOutput("solrresult")   
  })
}

ui <- dashboardPage( dashboardHeader
                     (title = "LCF Diary entry prototype using Solr information retrieval",
                       titleWidth = 590, 
                           
                         
                       tags$li(a(href = '',
                                 img(src = 'ONSt.gif',
                                     title = "ONS", height = "40px"),
                                 style = "padding-top:10px; padding-bottom:5px;"),
                               class = "dropdown"),
                                 
                      #"LCF Diary entry prototype using Solr/Lucene information retrieval service", 
                       #titleWidth = 650,disable = FALSE,
                       #img(src='ONS.png', align = "right"),
                       
                       
                       
                       
                     dropdownMenuOutput("notificationMenu")),
                     dashboardSidebar(disable = TRUE),
                     dashboardBody(
                       fluidRow( 
                         tags$head(tags$style(HTML(".small-box {height: 80px}"))),
                         tags$style(type='text/css', '#foodiesreactive {background-color: rgba(255,255,0,0.40); color: green;}'), 
                         
                         #tags$head(tags$script(src = "message-handler.js")),
                         tags$style(type='text/css', '#instructions {background-color: rgba(255,255,0,0.40); color: black;}'), 
                         
                         tags$head(tags$style(HTML("table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {
                                                   background-color: #9c4242 !important;
} "))),
                         box(title = "Instructions", width=12,status = "primary", solidHeader = FALSE,
                             collapsible = TRUE,
                             verbatimTextOutput("instructions")
                         ),
                         box(title = "SOLR Query", width=12,status = "primary", solidHeader = TRUE, 
                             collapsible = FALSE,background="purple",
                             fixedRow( 
                               column(3,
                                      textInput(inputId="EXPDESC", label="EXPDESC", value = "")
                               ),
                               column(2,
                                      textInput(inputId="CODING", label="CODING", value = ""),
                                      actionButton(inputId="goButton",label= "Classify", width='100px', icon = icon("share-alt"))
                               ),
                               column(2,
                                      textInput(inputId="Paid1", label="Paid1", value = "")
                               ),
                               column(2,
                                      textInput(inputId="Quantity", label="Quantity", value = ""),
                                      actionButton(inputId="goButton2",label= "Look up", width='100px', icon = icon("server"))
                               ),
                               column(1,
                                      textInput(inputId="Units", label="Units", value = "")
                               ),
                               column(2,
                                      textInput(inputId="Shop", label="Shop", value = "")
                               ))
                         ),
                         box(title = "SOLR result",status = "primary", solidHeader = TRUE,collapsible = TRUE,
                             #collapsed = ifelse(solrboxappears,FALSE ,TRUE),
                             width=12,
                             verbatimTextOutput ("foodiesreactive"),
                             uiOutput("querytable"),
                             verbatimTextOutput("rowselect") )
                     )
                     )
                     )

shinyApp(ui = ui, server = server)