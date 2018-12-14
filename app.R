library(shiny)
library(tm)
library(tidyverse)
library(stringr)
library(ggplot2)
library(tidytext)
library(widyr)
source("predict_text.R")

ui <- fluidPage(
   
   # Application title
   titlePanel("Sentence completion"),
   
   sidebarLayout(
      sidebarPanel(
        textInput("text_input", "Input statement")
      ),
      
      mainPanel(
        h2("Prediction:"),
        textOutput("text_output"),
        h3("Top 5 predictions:"),
         plotOutput("distPlot")
      )
   )
)

server <- function(input, output) {
   output$text_output <- renderText({
     txt <- input$text_input
     if (is.null(txt) || txt == "") {
       c("[No input found]")
     }  else {
       predict_text(txt)
     }
  })

   output$distPlot <- renderPlot({
     txt <- input$text_input
     
     if (is.null(txt) || txt == "") {
       NULL
     }  else {
       alt <- predict_text(txt, show_alt = TRUE)
       alt <- alt %>% head(5) 
       
       alt %>%
         mutate(prediction = factor(as.character(alt$prediction), levels = rev(as.character(alt$prediction)))) %>%
         ggplot(aes(y = correlation, x = prediction, fill = prediction)) + geom_col() + coord_flip()
     }   
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

