library(ggplot2)
library(shiny)
library(readr)
library(readxl)
library(lubridate)

template = read_csv("bpTemplate.csv")
ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            HTML("Upload Your Data:</br>Record time to the nearest hour in a 24-hour format."),
            downloadButton("downloadTemplate", "Download Data Template"),
            fileInput("bpdata", "Upload Your BP Data")
        ),
        mainPanel(
            h3("View Your Data"),
            plotOutput("bpplot"),
            radioButtons("timeOption", "Average across Days:", choices = c("Yes", "No"), selected = "Yes")
        )
    )
)
    
    server <- function(input, output) {
        
        output$downloadTemplate = downloadHandler(
            filename = "bpTemplate.csv",
            content = function(file){
                write.csv(template, file, row.names = FALSE)
            }
        )
        
        output$bpplot = renderPlot({
            if(!is.null(input$bpdata)){
                print(getwd())
                bpup = read.csv(input$bpdata$datapath)
                if(input$timeOption == "No"){
                    bpup$day = wday(bpup$day, label = FALSE)-1
                    bpup$time = bpup$time + 24 * bpup$day
                }
                myPlot = ggplot(data = bpup) + 
                    geom_smooth(aes(x = time, y = systolic, color = "Systolic")) + 
                    geom_smooth(aes(x = time, y = diastolic, color = "Diastolic")) + 
                    ylab("Pressure")
                if(input$timeOption == "No"){
                    myPlot = myPlot + scale_x_continuous(name = "Time", breaks = seq(0,6*24, 24),
                                                         labels = c("S", "M", "T", "W", "T", "F", "S"),
                                                         limits = c(0, 7*24))
                }
                
                
                myPlot
            }
        })
        
    }
    
    shinyApp(ui = ui, server = server)
    