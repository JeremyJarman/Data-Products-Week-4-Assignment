library(shiny)
library(shinyjs)
library(lubridate)
library(readxl)
library(reshape)
library(ggplot2)
library(gridExtra)
library(grid)

server <- function(input, output, session) {
  
  output$Example <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
   # outfile <- tempfile("ExampleImage.PNG")    # Return a list containing the filename
    
     list(src = 'ExampleImage.PNG',
         contentType = 'png',
         width = 840,
         height = 449,
         alt = "This is alternate text")
    
  }, deleteFile = FALSE)
  
  output$HitSubmit <- renderImage({
      list(src = 'HitSubmit.PNG',
         contentType = 'png',
         width = 427,
         height = 236,
         alt = "This is alternate text")
    
  }, deleteFile = FALSE)
   
  
  output$HitDownload <- renderImage({
    list(src = 'HitDownload.PNG',
         contentType = 'png',
         width = 421,
         height = 291,
         alt = "This is alternate text")
    
  }, deleteFile = FALSE)
  
  
  
  output$downloadData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {paste("Example1", "xlsx", sep=".")
      
    },
    
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {file.copy("Example1.xlsx", file)
      }
    
  )
  
  
  
  
  
  
  
  # Extract the file paths
  output$filePaths <- renderTable({
    if(is.null(input$file)){return ()}
    input$file$datapath # the file input data frame object that contains the file attributes
  })
  
  
  #Generate UiOUtputs once data is loaded
  output$selectfile <- renderUI({
    if(is.null(input$file)) {return()}
    list(hr(),
         selectInput("Select", "Select", choices=c(input$file$name , "All")),
         numericInput("k", "Input k value", 0))
    })
  

  
  

  #Generates the plot of the selected data set, calls the data() function which forces the processing of data 
  #when submit is hit
  output$newGraph<-renderPlot({
     if(input$sub){
         if(input$Select=="All"){
            data()[[1+length(input$file$datapath)]]$Plot
          }
          else{
          tempdata <- data()[[which(input$file$name==input$Select)]]
          tempdata$Plot} 
        }})
  
    

  #Initialize the Data from Input Source and run Calcs
  data<-eventReactive(input$sub,{
    inFile <- input$file
    if(is.null(input$file)) {return()}
    else  {
      
      #inFile$plot <- NULL
      results<-list()
      testset<-list()
      withProgress(message = 'Running Calcs', value = 0,session = getDefaultReactiveDomain(),{
        i<-1
        while(i <= length(inFile$datapath))
            {
          
            file.copy(inFile$datapath[i],paste(inFile$datapath[i], ".xlsx", sep=""))
            data <- read_excel( paste(inFile$datapath[i], ".xlsx", sep=""), skip = 1)
              #col_names = c("Time", "Pressure"), col_types = c("text", "numeric", "skip")
              data <- data[,1:2]
              colnames(data)<-c("Time", "Pressure")
              data$Time <- as.numeric(data$Time)
              data$Duration<-data$Time
              
                  j<-1
                  while(j<=length(data$Time))
                  {
                    data$Duration[j]<-(data$Time[j] - data$Time[1])
                    j<- j+1
                  }
                  
            
              
              
              Plot <-ggplot(data, aes(Duration,Pressure))+ geom_line() + labs(x= "Duration(s)", y = "Pressure(Bar)")
              
              MaxPressure <- max(data$Pressure)
              
              TimeAtMax <- data$Duration[which.max(data$Pressure)]
              
              results[[i]] <- list(Plot= Plot, MaxPressure = MaxPressure, TimeAtMax = TimeAtMax)
              
              testset[[i]] <- data[,-1]
              
              incProgress(1/length(inFile$datapath))
              
              i<- i +1
          
        }
      })
      
      #Plot All
      plotdf <- cbind(cat=rep(input$file$name,sapply(testset,nrow)),do.call(rbind,testset))
      Allplot <- ggplot(plotdf, aes(Duration,Pressure, color=cat)) + geom_line() + theme(
        legend.position = c(.95, .05),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))+theme(legend.title=element_blank()) +
      labs(x= "Duration(s)", y = "Pressure(Bar)", main = "Batch Results")
               
      
      results[[1+length(inFile$datapath)]] <- list(Plot = Allplot)
      
      results
      } 
  
    
    
    })
  
  
  #Create Table of Burst pressures and time frames
  dataTable<-eventReactive(input$sub,{
  
      if(is.null(input$file)){return ()}
        
    else{
      table <- input$file[,c("name","size")]   
      k<-1
       while(k <= length(input$file$datapath))
        {
          tempdata <- data()[[k]]
          table$MaxPressure[k] <- tempdata$MaxPressure
          table$TimeAtMax[k] <- tempdata$TimeAtMax
          k<- k +1
           
        }
    table
    }
    
  })
    
  #Renders the data table as output object
  output$filedf <- renderTable({
    if(input$sub){
    dataTable()
    }
    })
  
  #Calculates the mean and sd and renders them as a text output
  
  AnalysisData <- reactive({
    if(is.null(input$file)){return ()}
    else{
    AnalysisData<-dataTable()[which(dataTable()$name %in% input$checkGroup),]
    AnalysisData
    }
  })
  

  batchMean <- eventReactive(input$sub,{paste(mean(dataTable()$MaxPressure),"sd", sd(dataTable()$MaxPressure), sep =" ")})
  output$Mean<- renderText({batchMean()})


  output$RatedBurst <- renderText({
    rated<-mean(dataTable()$MaxPressure) - (input$k*sd(dataTable()$MaxPressure))
    rated})  
 
  # Creates a do nothing button untill the Lot Number Field is filled in
  output$button <- renderUI({
   if (input$BatchNum == ""){
     actionButton("do", "Download", icon = icon("download"))
    } else {
      downloadButton("report", "Download")
    }
  })
  
  available <- eventReactive(input$do, {
    input$BatchNum != ""
  })
    
  output$downloadFail <- renderText({
    if (!(available())) {
      "No Lot Number Submitted!"
    } else {
      ""
    } 
  })
  
  
  
   output$report <- downloadHandler(
             
           
     
     
             filename = function(){paste(input$BatchNum, ".pdf", sep="")},
               #"report.pdf",
             content = function(file) { if(input$BatchNum==""){
               session$sendCustomMessage(type = 'testmessage',
                message = 'Please enter a Batch Number')
               #showNotification("Message text", action = return())
               }
               else{
           
              # Copy the report file to a temporary directory before processing it
              tempReport <- file.path(tempdir(), "report.rmd")
              file.copy("report.rmd", tempReport, overwrite = TRUE)
               
               # Set up parameters to pass to Rmd document
              AllPlot <- data()[[1+length(input$file$datapath)]]$Plot
              rated<-mean(dataTable()$MaxPressure) - (input$k*sd(dataTable()$MaxPressure))
              batchMean <- mean(dataTable()$MaxPressure)
              SdDiv <- sd(dataTable()$MaxPressure)
              TotalDevs <- length(input$file$datapath)
              Table <- dataTable()[,-2]
              BatchNum <- input$BatchNum
              Comments <- input$Comments
              names(Table) <- c("Names", "Max Pressure (Bar)", "Time (s)")
              
              params <- list(kval = input$k, Plot = AllPlot, Rated = rated, batchMean = batchMean, SdDiv = SdDiv,
                             TotalDevs = TotalDevs, Table = Table, BatchNum = BatchNum, Comments = Comments)
               
               # Knit the document, passing in the `params` list, and eval it in a
               # child of the global environment (this isolates the code in the document
               # from the code in this app).
              rmarkdown::render(tempReport, output_file = file,
                                params = params,
                                envir = new.env())
           
               }                   
            }
         )


#Generate UiOUtputs once data crunched
   output$Calcs <- renderUI({
     if(!input$sub) {return()}
     list(hr(),
          h4('Mean and Standard Deviation'), verbatimTextOutput("Mean"),
          h4('Rated Burst Pressure'), verbatimTextOutput("RatedBurst"),
          textAreaInput("Comments", label = "Comments", value = "", width = '200%', height = '200%',
                   cols = NULL, rows = NULL, placeholder = NULL, resize = "vertical")
           )
   })           
  

  
  
  
}
