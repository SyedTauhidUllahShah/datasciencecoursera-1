library(shiny)
shinyUI(pageWithSidebar(
     # Application title
     headerPanel("Let's Root Root Root for the Numbers!"),
     sidebarPanel(
          numericInput("base",label="Base Number for Which Root is Desired", value=25),
          numericInput("power", "Desired (nth) Root", 2, min = 2, max = 4, step = 1),
          numericInput(inputId="epsilon", label="Desired Accuracy",value=0.001),
          actionButton("goButton", "Go!"),
          h5("Help", a("Link", href="https://github.com/lytemar/datasciencecoursera/blob/master/Developing_Data_Products/apphelp.Rmd"))
          ),
     mainPanel(
          h5('Base Number'),
          textOutput('base'),
          h5('Desired Root'),
          textOutput('power'),
          h5('Requested Accuracy'),
          textOutput('epsilon'),
          h5('Result'),
          textOutput('answer')
          )
     ))