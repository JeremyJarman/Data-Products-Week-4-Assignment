library(shiny)
library(shinyjs)
library(UsingR)


ui <- fluidPage(
  titlePanel("Burst Pressure Analysis App"),
  tags$head(tags$script(src = "message-handler.js")),
  sidebarLayout(
    sidebarPanel(
      
      fileInput("file", "Choose Files", multiple = TRUE,
                accept = c(".xlsx")
      ),
      #tags$hr(),
      
      actionButton("sub","Submit!"), 
      uiOutput("selectfile"),
      textInput("BatchNum", label ="Lot Number", value =""),
      uiOutput("button"),
      textOutput("downloadFail")
    #  uiOutput("Options")
      
      
      
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Help", 
                           h3("Step 1"),
                           tags$p("Select and upload files for analysis, 
                                  note the format. The first column is time stamp, 
                                  the second is the pressure reading ", tags$br(),
                                  "p.s I included a sample data file at the bottom of the help page",
                                  tags$br(), tags$br()),
                           
                           imageOutput("Example"),
                           
                           h3(tags$br(), tags$br(),"Step 2", tags$br(), tags$br()),
                           
                           imageOutput("HitSubmit"),
                           
                           tags$p("Once the file upload is complete, hit the Submit button to start running the analysis, switch over to the Analysis tab for a better view.", tags$br(),
                                  "The select drop down menu, changes the plot displayed in the Analysis pane, but ALL traces are ploted in the PDF report."),
                           h3("Step 3"),
                           tags$p("Input a k value to generate the rated burst pressure reading."),
                           h3("Step 4"),
                           tags$p("Input the lot number for the batch of devices that were tested, then hit the Download button to generate a PDF report.", tags$br(), tags$br()),
                           
                           imageOutput("HitDownload"),
                           
                           downloadButton('downloadData', 'ExampleData')
                           
                           ),
                            
                  tabPanel("Analysis", 
                           plotOutput("newGraph"),
                           tableOutput("filedf"),
                           uiOutput("Calcs")))
                  
      
      
      
     )
  )
)