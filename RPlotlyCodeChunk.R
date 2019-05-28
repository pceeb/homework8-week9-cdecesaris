library(shiny)
library(plotly)
library(knitr)
library(shinythemes)


xx<-data.frame(time1=seq(0,20))
vv<-data.frame(time1=seq(0,20))

server <- shinyServer(function(input, output) {
    
    output$testPlot3 <- renderPlotly({
        number_ticks <- function(n) {function(limits) pretty(limits, n)}
        x1<-vv[,1]
        y1<-input$p3
        df = data.frame(x1 ,y1)
        ggplot(data=df, aes(x=x1, y=y1)) +
            geom_line(color =  "lightblue") +
            scale_x_continuous(breaks = number_ticks(10)) +
            scale_y_continuous(breaks = number_ticks(10)) +
            theme_light() + xlab("time") + ylab("m/s^2")
        # plot(x, y, type = "l", pch = 1, col = 'red', ylim = c(0,75000))
    })
    output$testPlot <- renderPlotly({
        number_ticks <- function(n) {function(limits) pretty(limits, n)}
        x1<-xx[,1]
        y1<-input$p1 + input$p2*(x1)
        df = data.frame(x1 ,y1)
        ggplot(data=df, aes(x=x1, y=y1)) +
            geom_line(color =  "lightblue") +
            scale_x_continuous(breaks = number_ticks(10)) +
            scale_y_continuous(breaks = number_ticks(10)) +
            theme_light() + xlab("time") + ylab("m/s")
        # plot(x, y, type = "l", pch = 1, col = 'red', ylim = c(0,75000))
    })
    output$testPlot2 <- renderPlotly({
        number_ticks <- function(n) {function(limits) pretty(limits, n)}
        x1<-vv[,1]
        y1<-input$p1*(x1) + input$p2*(x1)^2 + input$p3
        df = data.frame(x1 ,y1)
        ggplot(data=df, aes(x=x1, y=y1)) +
            geom_line(color =  "lightblue") +
            scale_x_continuous(breaks = number_ticks(10)) +
            scale_y_continuous(breaks = number_ticks(10)) +
            theme_light() + xlab("time") + ylab("m") 
        # plot(x, y, type = "l", pch = 1, col = 'red', ylim = c(0,75000))
    })
    
    
    
})

ui <- shinyUI(fluidPage(theme = shinytheme("cerulean"),
    titlePanel("Constant Aceleration Kinematics"),
    sidebarLayout(
        sidebarPanel(
            
            numericInput("p2", 
                         "Acceleration", value = 0),
            numericInput("p1",
                         "Initial Velocity", value = 0),
            numericInput("p3",
                         "Initial Position", value = 0)
            #checkboxInput("piece", "Piece-Wise" )
        ),
        mainPanel(
            plotlyOutput("testPlot3"),
            plotlyOutput("testPlot"),
            plotlyOutput("testPlot2")
            
        )
    )
))

shiny::shinyApp(ui=ui,server=server)
