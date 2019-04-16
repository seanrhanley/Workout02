library(shiny)
source("InvestmentFunctions.R")

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
                       value = 5),
           sliderInput("growth_rate",
                       "Growth Rate (in %)",
                       min = 0,
                       max = 20,
                       step = 0.1,
                       value = 2)
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
  ),
  fluidRow(
      h4("Balances"),
      tableOutput("modular")
  )
)


   
  
# Define server logic required to construct output graphs
server <- function(input, output) {
   
  mods <- reactive({
    
    #create initial vectors for years for each mode, including year 0
    no_contrib <- rep(0, input$years + 1)
    fixed_contrib <- rep(0, input$years + 1)
    growing_contrib <- rep(0, input$years + 1)
    
    #initial variables
    i_inv <- input$init
    rate <- input$return_rate / 100
    growth <- input$growth_rate / 100
    contrib <- input$contrib
    years <- input$years
    
    for (i in 1:years){
      
      if (i == 1){
        
        no_contrib[i] <- i_inv
        fixed_contrib[i] <- i_inv
        growing_contrib[i] <- i_inv
        
      }
      
      #mode1
      no_contrib[i+1] <- signif(future_value(amount = i_inv, rate = rate, years = i), 7)
      #mode2
      fixed_contrib[i+1] <- signif(annuity(contrib = contrib, rate = rate, years = i) + no_contrib[i+1], 7)
      #mode3
      growing_contrib[i+1] <- signif(growing_annuity(contrib = contrib, rate = rate, growth = growth, years = i) + no_contrib[i+1], 7)
      
    }
    
    #create data frame of different modalities
    mods <- data.frame(
      year = seq(0, years, 1),
      'No Contribution' = no_contrib,
      'Fixed Contribution' = fixed_contrib,
      'Growing Contribution' = growing_contrib
    )
    
    return(mods)
    
  })
   
    output$modular <- renderTable({
      
      data <- mods()
      head(data, n = isolate(input$years + 1))
      
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

