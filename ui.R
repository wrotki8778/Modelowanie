
library(shiny)

shinyUI(navbarPage("Projekt 1",
  tabPanel("Histogram rozkladu normalnego",
                            # Application title
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "probka",
                  label = "Number of observations:",
                  min = 1,
                  max = 1000,
                  value = 50),
      numericInput(inputId="mu",
                   label='Wartosc srednia',
                   value=0),
      numericInput(inputId="sigma",
                   label='Wariancja',
                   value=1,
                   min=0.001),
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput("naszPlot"),
    )
    ),
  ),
  tabPanel("Cos innego",
           titlePanel("Hello Shiny!"),
           sidebarLayout(
             sidebarPanel(
               numericInput(inputId = "probka2",
                           label = "Number of observations:",
                           min = 1,
                           max = 1000,
                           value = 50),
               numericInput(inputId = "ziarno",
                            label = "Seed:",
                            min = 1,
                            value = 50),
               verbatimTextOutput("tabela")
             ),
             # Main panel for displaying outputs ----
             mainPanel(
               plotOutput("macierz")
             ),
           ),
  )
  )
)