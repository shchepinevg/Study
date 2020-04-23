library(shiny)
library(shinyjs)

shinyUI(fluidPage(
  useShinyjs(),
  
  titlePanel("Bias classification"),
  
  fluidRow(
    
    column(4,
           textInput("textN", "N:", value = "1000", width = "100%")),
    
    column(4,
           selectInput("selectType", "Type:", 
                       choices = c("Central limit theorem", "Polar coords"),
                       selected = "Central limit theorem", width = "100%")),
    
    column(4,
           textInput("textK", "K:", value = "100", width = "100%"))
    
  ),
  
  fluidRow(
    
    column(12,
      sliderInput("sliderP", "P", min = 0, max = 1, value = 0.5, step = 0.01, width = "100%"))
    
  ),
  
  fluidRow(
    
    column(6,
           wellPanel(
             fluidRow(h3("First class", align="center")),
             fluidRow(column(12, textInput("textM11", "M11", value = "0"))),
             fluidRow(column(12, textInput("textM12", "M12", value = "0"))),
             fluidRow(column(12, textInput("textSD11", "SD11", value = "1"))),
             fluidRow(column(12, textInput("textSD12", "SD12", value = "1"))))
           ), 
    
    column(6,
           wellPanel(
             fluidRow(h3("Second class", align="center")),
             fluidRow(column(12, textInput("textM21", "M21", value = "3"))),
             fluidRow(column(12, textInput("textM22", "M22", value = "3"))),
             fluidRow(column(12, textInput("textSD21", "SD21", value = "1"))),
             fluidRow(column(12, textInput("textSD22", "SD22", value = "1"))))
          )
  ),
  
  fluidRow(
    
    column(12, actionButton("buttonStart", "Start", width = "12%"), align = "center")
  ),
  
  fluidRow(
    
    column(12, plotOutput(outputId="trainPlot"))
  ),
  
  fluidRow(
    
    column(12, plotOutput(outputId="testPlot"))
  ),

  fluidRow(

    column(6,
             fluidRow(column(12, hidden(textInput("textM11est", "M11^", value = "", width = "100%")))),
             fluidRow(column(12, hidden(textInput("textM12est", "M12^", value = "", width = "100%")))),
             fluidRow(column(12, hidden(textInput("textSD11est", "SD11^", value = "", width = "100%")))),
             fluidRow(column(12, hidden(textInput("textSD12est", "SD12^", value = "", width = "100%")))),
             fluidRow(column(12, hidden(textInput("textP1est", "P1^", value = "", width = "100%"))))
    ),

    column(6,
             fluidRow(column(12, hidden(textInput("textM21est", "M21^", value = "", width = "100%")))),
             fluidRow(column(12, hidden(textInput("textM22est", "M22^", value = "", width = "100%")))),
             fluidRow(column(12, hidden(textInput("textSD21est", "SD21^", value = "", width = "100%")))),
             fluidRow(column(12, hidden(textInput("textSD22est", "SD22^", value = "", width = "100%")))),
             fluidRow(column(12, hidden(textInput("textP2est", "P2^", value = "", width = "100%"))))
           )
    )
))




