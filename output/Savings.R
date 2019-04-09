#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  fluidRow(
    # Application title
    titlePanel("Three Ways to Save Money")
  ),
  fluidRow(
    column(4, 
           sliderInput("init", "Initial Amount",
                       min = 0,
                       max = 100000,
                       step = 500,
                       value = 1000),
           sliderInput("contrib",
                       "Annual Contribution",
                       min = 0,
                       max = 50000,
                       step = 500,
                       value = 2000)
           ),
    column(4,
           sliderInput("return_rate",
                       "Return Rate (in %)",
                       min = 0,
                       max = 20,
                       step = 0.1,
                       value = .05),
           sliderInput("growth_rate",
                       "Growth Rate (in %)",
                       min = 0,
                       max = 20,
                       step = 0.1,
                       value = 0.2)
           ),
    column(4,
           sliderInput("years",
                       "Years",
                       min = 0,
                       max = 50,
                       step = 1,
                       value = 20),
           selectInput("facet_choice", 
                       "Facet?",
                       c('Yes', 'No'),
                       selected = 'No',
                       multiple = FALSE,
                       selectize = TRUE)
           )
    ),
  fluidRow(
    tags$hr()
    ##<hr></hr>
  )
)


   
  
# Define server logic required to draw a histogram
server <- function(input, output) {
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

