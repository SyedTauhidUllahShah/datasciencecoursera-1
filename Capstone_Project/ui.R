library(shiny)
shinyUI(pageWithSidebar(
     # Application title
     headerPanel("Word Prediction from a Phrase"),
     sidebarPanel(
          textInput("txtInput",label="Enter a phrase", value="couple words"),
          actionButton("goButton", "Submit")
          ),
     mainPanel(
          h5('Result'),
          textOutput('answer')
          )
     ))