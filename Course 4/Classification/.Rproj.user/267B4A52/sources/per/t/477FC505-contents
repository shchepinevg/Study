library(shiny)
library(shinyjs)

source("sampleGeneration.R")
source("classification.R")
source("sampleGeneration.R")

shinyServer(function(input, output, session) {
  observeEvent(input$selectType, {
    if(input$selectType == "Central limit theorem") {
      enable("textK")
    } else {
      disable("textK")
    }
  })
  
  observeEvent(input$buttonStart, {
    # Генерация выборок
    #####################################################################
    ntrain <- as.numeric(input$textN)
    ntest <- floor(0.25*ntrain)
    p1 <- as.numeric(input$sliderP)
    p2 <- 1 - p1

    m11 <- as.numeric(input$textM11)
    m12 <- as.numeric(input$textM12)
    m21 <- as.numeric(input$textM21)
    m22 <- as.numeric(input$textM22)
    sd11 <- as.numeric(input$textSD11)
    sd12 <- as.numeric(input$textSD12)
    sd21 <- as.numeric(input$textSD21)
    sd22 <- as.numeric(input$textSD22)
    
    k <- as.numeric(input$textK)
    typeGener <- input$selectType
    
    sample <- sampleGeneration(typeGener, ntrain, ntest, m11, m12, sd11, sd12, m21, m22, sd21, sd22, k, p1)
    
    
    x1train <- sample$x1train
    x2train <- sample$x2train
    n1train <- sample$n1train
    n2train <- sample$n2train
    
    x1test <- sample$x1test
    x2test <- sample$x2test
    n1test <- sample$n1test
    n2test <- sample$n2test
    
    #####################################################################
    
    # Расчет оценок
    #####################################################################
    m11est <- mean(x1train[,1])
    m12est <- mean(x1train[,2])
    sd11est <- (sd(x1train[,1]))^2
    sd12est <- (sd(x1train[,2]))^2
    m21est <- mean(x2train[,1])
    m22est <- mean(x2train[,2])
    sd21est <- (sd(x2train[,1]))^2
    sd22est <- (sd(x2train[,2]))^2
    p1est <- n1train/ntrain
    p2est <- n2train/ntrain
    x <- c(x1train[,1], x2train[,1])
    y <- c(x1train[,2], x2train[,2])
    sd1est <- sd(x)
    sd2est <- sd(y)
    #####################################################################
    
    # Классификация
    #####################################################################
    points <- classification(n1test, n2test, x1test, x2test, sd1est, sd2est,
                             m11est, m12est, m21est, m22est, p1est, p2est)
    
    rightPoints1 <- points$rightPoints1
    rightPoints2 <- points$rightPoints2
    errors <- points$errors
    errorPoints1 <- points$errorPoints1
    errorPoints2 <- points$errorPoints2
    unclearPoints <- points$unclearPoints
    err <- points$err
    unclear <- points$unclear
    #####################################################################
    
    # Отрисовка
    #####################################################################
    output$trainPlot <- renderPlot({
      plot(x1train, main = "Обучающая выборка", xlab = "X1", ylab = "X2", col = "red", pch = 19,
           xlim = c(min(x1train[, 1], x2train[, 1]), max(x1train[, 1], x2train[, 1])),
           ylim = c(min(x1train[, 2], x2train[, 2]), max(x1train[, 2], x2train[, 2])))
      points(x2train, col = "blue", pch = 19)
    })
    
    output$testPlot <- renderPlot({
      plot(rightPoints1, main = "Классификация на экзаменационной выборке",
           xlab = "X1", ylab = "X2", col = "red", pch = 19,
           xlim = c(min(rightPoints1[, 1], rightPoints2[, 1]), max(rightPoints1[, 1], rightPoints2[, 1])),
           ylim = c(min(rightPoints1[, 2], rightPoints2[, 2]), max(rightPoints1[, 2], rightPoints2[, 2])))
      points(rightPoints2, col = "blue", pch = 19)
      
      if (!is.null(errorPoints1)) {
        points(errorPoints1, pch = 4, col = "red")
      }
      
      if (!is.null(errorPoints2)) {
        points(errorPoints2, pch = 4, col = "blue")
      }

      if (!is.null(unclearPoints)) {
        points(unclearPoints, pch = 0, col = "grey")
      }
      
      legend("bottomright", legend=paste0("Error ", err, "%; Unclear ",
                                          unclear))
    })
    
    show("textM11est")
    show("textM12est")
    show("textSD11est")
    show("textSD12est")
    show("textM21est")
    show("textM22est")
    show("textSD21est")
    show("textSD22est")
    show("textP1est")
    show("textP2est")
    
    updateTextInput(session, "textM11est", value = m11est)
    updateTextInput(session, "textM12est", value = m12est)
    updateTextInput(session, "textSD11est", value = sd11est)
    updateTextInput(session, "textSD12est", value = sd12est)
    updateTextInput(session, "textM21est", value = m21est)
    updateTextInput(session, "textM22est", value = m22est)
    updateTextInput(session, "textSD21est", value = sd21est)
    updateTextInput(session, "textSD22est", value = sd22est)
    updateTextInput(session, "textP1est", value = p1est)
    updateTextInput(session, "textP2est", value = p2est)
    #####################################################################    
    })
})
