library(shiny)
library(quantmod)

if (interactive()) {
  ui <- fluidPage(
    sidebarPanel(
      selectInput("Typepicked", "Type Picked",
                  c(Picked = "picked", Indexes = "index")
      ),
      # Only show this panel if the plot type is a histogram
      conditionalPanel(
        condition = "input.Typepicked == 'picked'",
        radioButtons("data",label="Sectors:",c("Online Entertainment","Online Ordering","Online Meetings",
                                               "SHOTS SHOTS SHOTS","Home Improvement")),
      ),
      conditionalPanel(
        condition = "input.Typepicked == 'index'",
        radioButtons("data1",label="Sectors:",c("Real Estate","IT","Health Care",
                                                "Discresionary","Communication","Financials",
                                                "Industrials","Consumer Staples","Utilities","Energy")),
        # Only show this panel if Custom is selected
      )
      
    ),
    mainPanel(
      textOutput("text"),
      verticalLayout( plotOutput('plt1'), plotOutput('plt2'),plotOutput('plt3'),plotOutput('plt4'),plotOutput('plt5'),plotOutput('plt6'))
    ))
  
  server<-shinyServer(function(input,output){
    output$text <- renderText({
      if (input$Typepicked == "picked") {
        if (input$data=="Online Entertainment"){
          "Online Entertainment, such as Amazon Prime, Disney Plus, HBO Max, Netflix, Apple TV"
          
        } else
          if (input$data=="Online Ordering"){
            "Online ordering services such as Uber Eats, DoorDash, Shipt"
            
          } else
            if (input$data=="Online Meetings"){
              "Online Meeting services  such as Zoom, Teams, and Webex"
              
            } else 
              if (input$data=="SHOTS SHOTS SHOTS"){
                "Companies that developed immunizations against Covid-19"
              } else
                if (input$data=="Home Improvement"){
                  "Home Improvment stores such as Lowes and Home Depot"
                }} else
                  if (input$Typepicked == "index") {
                    if (input$data1=="IT"){
                      "IT companies"
                    } else
                      if (input$data1=="Health Care"){
                        "Health Care companies"
                      } else
                        if (input$data1=="Discresionary"){
                          "Companies selling Discresionary products such as appliances, cars and entertainment"
                        } else 
                          if (input$data1=="Communication"){
                            "Communication companies"
                          } else
                            if (input$data1=="Financials"){
                              "Financial institutions"
                            } else
                              if (input$data1=="Industrials"){
                                "Companies in the Industrial sector"
                              } else
                                if (input$data1=="Consumer Staples"){
                                  "Companies selling sumsumer staples such as food products"
                                } else
                                  if (input$data1=="Utilities"){
                                    "Companies that provide utilities such as internet and water"
                                  } else
                                    if (input$data1=="Real Estate"){
                                      "Real estate funds"
                                    } else
                                      if (input$data1=="Energy"){
                                        "Companies that provide energy"
                                      }}
      
      
    })
    
    output$plt1 <- renderPlot({
      if (input$Typepicked == "picked") {
        if (input$data=="Online Entertainment"){
          getSymbols("AAPL", from = '2020-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
          chartSeries(AAPL, type="line", subset='2020',theme=chartTheme('white'))
          
          
        } else 
          if (input$data=="Online Ordering"){
            getSymbols("UBER", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
            chartSeries(UBER, type="line", subset='2020',theme=chartTheme('white'))
            
          } else 
            if (input$data=="Online Meetings"){
              getSymbols("ZM", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
              chartSeries(ZM, type="line", subset='2020',theme=chartTheme('white'))
              
            } else 
              if (input$data=="SHOTS SHOTS SHOTS"){
                getSymbols("MRNA", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
                chartSeries(MRNA, type="line", subset='2020',theme=chartTheme('white'))
                
              } else 
                if (input$data=="Home Improvement"){
                  getSymbols("LOW", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
                  chartSeries(LOW, type="line", subset='2020',theme=chartTheme('white'))
                  }}
      
      else
        if (input$Typepicked == "index") {
          if (input$data1=="IT"){
            getSymbols("VGT", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
            chartSeries(VGT, type="line", subset='2020',theme=chartTheme('white'))
          } else
            if (input$data1=="Health Care"){
              getSymbols("VHT", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
              chartSeries(VHT, type="line", subset='2020',theme=chartTheme('white'))
            } else
              if (input$data1=="Discresionary"){
                getSymbols("VCR", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
                chartSeries(VCR, type="line", subset='2020',theme=chartTheme('white'))
              } else
                if (input$data1=="Communication"){
                  getSymbols("VOX", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
                  chartSeries(VOX, type="line", subset='2020',theme=chartTheme('white'))
                } else
                  if (input$data1=="Financials"){
                    getSymbols("VFH", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
                    chartSeries(VFH, type="line", subset='2020',theme=chartTheme('white'))
                  } else
                    if (input$data1=="Industrials"){
                      getSymbols("VIS", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
                      chartSeries(VIS, type="line", subset='2020',theme=chartTheme('white'))
                    } else
                      if (input$data1=="Consumer Staples"){
                        getSymbols("VDC", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
                        chartSeries(VDC, type="line", subset='2020',theme=chartTheme('white'))
                      } else
                        if (input$data1=="Utilities"){
                          getSymbols("VPU", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
                          chartSeries(VPU, type="line", subset='2020',theme=chartTheme('white'))
                        } else
                          if (input$data1=="Real Estate"){
                            getSymbols("VNQ", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
                            chartSeries(VNQ, type="line", subset='2020',theme=chartTheme('white'))
                          } else
                            if (input$data1=="Energy"){
                              getSymbols("VDE", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
                              chartSeries(VDE, type="line", subset='2020',theme=chartTheme('white'))}}
      
    })#1
    
    output$plt2 <- renderPlot({
      if (input$Typepicked == "picked") {
        if (input$data=="Online Entertainment"){
          getSymbols("AMZN", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
          chartSeries(AMZN, type="line", subset='2020',theme=chartTheme('white'))
          
          
        } else 
          if (input$data=="Online Ordering"){
            #getSymbols("UBER", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
            getSymbols("DASH", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
            #getSymbols("GRUB", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
            #getSymbols("TGT", from = '2020-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
            
            #chartSeries(UBER, type="line", subset='2020',theme=chartTheme('white'))
            chartSeries(DASH, type="line", subset='2020',theme=chartTheme('white'))
            #chartSeries(GRUB, type="line", subset='2020',theme=chartTheme('white'))
            #chartSeries(TGT, type="line", subset='2020',theme=chartTheme('white'))
            
            
            
          } else 
            if (input$data=="Online Meetings"){
              #getSymbols("ZM", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
              getSymbols("CSCO", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
              #getSymbols("MSFT", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
              
              #chartSeries(ZM, type="line", subset='2020',theme=chartTheme('white'))
              chartSeries(CSCO, type="line", subset='2020',theme=chartTheme('white'))
              #chartSeries(MSFT, type="line", subset='2020',theme=chartTheme('white'))
              
            } else 
              if (input$data=="SHOTS SHOTS SHOTS"){
                #getSymbols("MRNA", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
                getSymbols("PFE", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
                #getSymbols("JNJ", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
                
                #chartSeries(MRNA, type="line", subset='2020',theme=chartTheme('white'))
                chartSeries(PFE, type="line", subset='2020',theme=chartTheme('white'))
                #chartSeries(JNJ, type="line", subset='2020',theme=chartTheme('white'))
                
                
              } else 
                if (input$data=="Home Improvement"){
                  #getSymbols("LOW", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
                  getSymbols("HD", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
                  
                  #chartSeries(LOW, type="line", subset='2020',theme=chartTheme('white'))
                  chartSeries(HD, type="line", subset='2020',theme=chartTheme('white'))}}
      
      else
        if (input$Typepicked == "index") {
            print('')}
      
    })#2
    
    output$plt3 <- renderPlot({
      if (input$Typepicked == "picked") {
        if (input$data=="Online Entertainment"){
          #getSymbols("AAPL", from = '2020-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
          #getSymbols("AMZN", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
          getSymbols("NFLX", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
          #getSymbols("DIS", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
          #getSymbols("T", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
          #getSymbols("GOOG", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
          #chartSeries(AAPL, type="line", subset='2020',theme=chartTheme('white'))
          #chartSeries(AMZN, type="line", subset='2020',theme=chartTheme('white'))
          chartSeries(NFLX, type="line", subset='2020',theme=chartTheme('white'))
          #chartSeries(DIS, type="line", subset='2020',theme=chartTheme('white'))
          #chartSeries(T, type="line", subset='2020',theme=chartTheme('white'))
          #chartSeries(GOOG, type="line", subset='2020',theme=chartTheme('white'))
          
          
        } else 
          if (input$data=="Online Ordering"){
            #getSymbols("UBER", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
            #getSymbols("DASH", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
            getSymbols("GRUB", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
            #getSymbols("TGT", from = '2020-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
            
            #chartSeries(UBER, type="line", subset='2020',theme=chartTheme('white'))
            #chartSeries(DASH, type="line", subset='2020',theme=chartTheme('white'))
            chartSeries(GRUB, type="line", subset='2020',theme=chartTheme('white'))
            #chartSeries(TGT, type="line", subset='2020',theme=chartTheme('white'))
            
            
            
          } else 
            if (input$data=="Online Meetings"){
              #getSymbols("ZM", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
              #getSymbols("CSCO", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
              getSymbols("MSFT", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
              
              #chartSeries(ZM, type="line", subset='2020',theme=chartTheme('white'))
              #chartSeries(CSCO, type="line", subset='2020',theme=chartTheme('white'))
              chartSeries(MSFT, type="line", subset='2020',theme=chartTheme('white'))
              
            } else 
              if (input$data=="SHOTS SHOTS SHOTS"){
                #getSymbols("MRNA", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
                #getSymbols("PFE", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
                getSymbols("JNJ", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
                
                #chartSeries(MRNA, type="line", subset='2020',theme=chartTheme('white'))
                #chartSeries(PFE, type="line", subset='2020',theme=chartTheme('white'))
                chartSeries(JNJ, type="line", subset='2020',theme=chartTheme('white'))
                
                
              } else 
                if (input$data=="Home Improvement"){
                  print('')}}
      
      else
        if (input$Typepicked == "index") {
          print('')
        }
      
    })#2
    output$plt4 <- renderPlot({
      if (input$Typepicked == "picked") {
        if (input$data=="Online Entertainment"){
          #getSymbols("AAPL", from = '2020-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
          #getSymbols("AMZN", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
          #getSymbols("NFLX", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
          getSymbols("DIS", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
          #getSymbols("T", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
          #getSymbols("GOOG", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
          #chartSeries(AAPL, type="line", subset='2020',theme=chartTheme('white'))
          #chartSeries(AMZN, type="line", subset='2020',theme=chartTheme('white'))
          #chartSeries(NFLX, type="line", subset='2020',theme=chartTheme('white'))
          chartSeries(DIS, type="line", subset='2020',theme=chartTheme('white'))
          #chartSeries(T, type="line", subset='2020',theme=chartTheme('white'))
          #chartSeries(GOOG, type="line", subset='2020',theme=chartTheme('white'))
          
          
        } else 
          if (input$data=="Online Ordering"){
            #getSymbols("UBER", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
            #getSymbols("DASH", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
            #getSymbols("GRUB", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
            getSymbols("TGT", from = '2020-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
            
            #chartSeries(UBER, type="line", subset='2020',theme=chartTheme('white'))
            #chartSeries(DASH, type="line", subset='2020',theme=chartTheme('white'))
            #chartSeries(GRUB, type="line", subset='2020',theme=chartTheme('white'))
            chartSeries(TGT, type="line", subset='2020',theme=chartTheme('white'))
            
            
            
          } else 
            if (input$data=="Online Meetings"){
               print('')
              
            } else 
              if (input$data=="SHOTS SHOTS SHOTS"){
                print('')
                
                
              } else 
                if (input$data=="Home Improvement"){
                  print('')}}
      
      else
        if (input$Typepicked == "index") {
          print('')
        }
      
    })#2
    output$plt5 <- renderPlot({
      if (input$Typepicked == "picked") {
        if (input$data=="Online Entertainment"){
          getSymbols("T", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
          chartSeries(T, type="line", subset='2020',theme=chartTheme('white'))
          #chartSeries(GOOG, type="line", subset='2020',theme=chartTheme('white'))
          
          
        } else 
          if (input$data=="Online Ordering"){
            print('')
            
            
          } else 
            if (input$data=="Online Meetings"){
              print('')
              
            } else 
              if (input$data=="SHOTS SHOTS SHOTS"){
                print('')
                
                
              } else 
                if (input$data=="Home Improvement"){
                  print('')}}
      
      else
        if (input$Typepicked == "index") {
          print('')
        }
      
    })#2
    output$plt6 <- renderPlot({
      if (input$Typepicked == "picked") {
        if (input$data=="Online Entertainment"){
          getSymbols("GOOG", from = '2017-01-01',to = "2021-10-01",warnings = FALSE,auto.assign = TRUE)
          chartSeries(GOOG, type="line", subset='2020',theme=chartTheme('white'))
          
          
        } else 
          if (input$data=="Online Ordering"){
            print('')
            
            
          } else 
            if (input$data=="Online Meetings"){
              print('')
              
            } else 
              if (input$data=="SHOTS SHOTS SHOTS"){
                print('')
                
                
              } else 
                if (input$data=="Home Improvement"){
                  print('')}}
      
      else
        if (input$Typepicked == "index") {
          print('')
        }
      
    })#2
  })#3
  
  shinyApp(ui, server)
}

