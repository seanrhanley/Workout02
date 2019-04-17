library(shiny)
library(ggplot2)
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
    h4("Timelines"),
    plotOutput("timelines")
    
  ),
  fluidRow(
    h4("Balances"),
    verbatimTextOutput("modular")
  )
)


   
  
# Define server logic required to construct output graphs
server <- function(input, output) {
   
  mods <- reactive({
    
    #initial variables
    i_inv <- input$init
    rate <- input$return_rate / 100
    growth <- input$growth_rate / 100
    contrib <- input$contrib
    years <- input$years
    
    #create initial vectors for years for each mode, including year 0
    no_contrib <- rep(0, years + 1)
    fixed_contrib <- rep(0, years + 1)
    growing_contrib <- rep(0, years + 1)
    
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
  
  mods_facet <- reactive({
    
    ################################################################################
    #initial variables
    i_inv <- input$init
    rate <- input$return_rate / 100
    growth <- input$growth_rate / 100
    contrib <- input$contrib
    years <- input$years
    
    #create initial vectors for years for each mode, including year 0
    i_mode <- rep(0, years + 1)
    balance <- rep(0, years + 1)
    no_contrib <- rep(0, years + 1)
    fixed_contrib <- rep(0, years + 1)
    growing_contrib <- rep(0, years + 1)
    
    for (i in 1:years){
      
      if (i == 1){
        
        balance[i] <- i_inv
        i_mode[i] <- 'future'
        
      }
      
      #mode1
      balance[i + 1] <- signif(future_value(amount = i_inv, rate = rate, years = i), 7)
      i_mode[i + 1] <- 'future'
    }
    
    mode1 <- data.frame(
      year = seq(0, years, 1),
      'balance' <- balance,
      'i_mode' <- i_mode
    )
    
    i_mode <- rep(0, years + 1)
    balance <- rep(0, years + 1)
    no_contrib <- rep(0, years + 1)
    fixed_contrib <- rep(0, years + 1)
    growing_contrib <- rep(0, years + 1)
    
    for (i in 1:years){
      
      if (i == 1){
        
        balance[i] <- i_inv
        i_mode[i] <- 'annuity'
        
      }
      
      #mode2
      balance[i + 1] <- signif(annuity(contrib = contrib, rate = rate, years = i) + no_contrib[i+1], 7)
      i_mode[i + 1] <- 'annuity'
    }
    
    mode2 <- data.frame(
      year = seq(0, years, 1),
      'balance' <- balance,
      'i_mode' <- i_mode
    )
    
    i_mode <- rep(0, years + 1)
    balance <- rep(0, years + 1)
    no_contrib <- rep(0, years + 1)
    fixed_contrib <- rep(0, years + 1)
    growing_contrib <- rep(0, years + 1)
    
    for (i in 1:years){
      
      if (i == 1){
        
        balance[i] <- i_inv
        i_mode[i] <- 'growing'
        
      }
      
      #mode3
      balance[i + 1] <- signif(growing_annuity(contrib = contrib, rate = rate, growth = growth, years = i) + no_contrib[i+1], 7)
      i_mode[i + 1] <- 'growing'
    }
    
    mode3 <- data.frame(
      year = seq(0, years, 1),
      'balance' <- balance,
      'i_mode' <- i_mode
    )
    
    
    #merge all data.frames into one
    mods_facet <- rbind(mode1, mode2, mode3)
    colnames(mods_facet) <- c('year', 'balance', 'savings_mode')
    
    return(mods_facet)
    ################################################################################
    
  })
   
  
    output$modular <- renderPrint({
      
      data <- mods()
      head(data, n = isolate(input$years) + 1)
      
      
    })
    output$timelines <- renderPlot({

      if (input$facet_choice == "Yes"){
        dat <- mods_facet()
        ggplot(dat, aes(x=year,y = dat$balance, fill = savings_mode)) +
          geom_line() + 
          geom_area() +
          ylab(dat$balance) +
          labs(title = "Three Types of Savings Scenarios") + theme(plot.title = element_text(hjust = 0.5)) +
          scale_color_manual(name = 'Mode', values = c('red', 'green', 'blue'), labels = c('No Contribution','Fixed Contribution', 'Growing Contribution')) + 
          facet_wrap(factor(dat$savings_mode))
      }else{
        data <- mods()
        ggplot(data) +
          geom_line(aes(x=year, y=Growing.Contribution, color = 'red')) +
          geom_line(aes(x=year, y=Fixed.Contribution, color = 'green')) +
          geom_line(aes(x=year, y=No.Contribution, color = 'blue')) +
          ylab('balance') +
          labs(title = "Three Types of Savings Scenarios") + theme(plot.title = element_text(hjust = 0.5)) +
          scale_color_manual(name = 'Modality', values = c('red', 'green', 'blue'), labels = c('No Contribution','Fixed Contribution', 'Growing Contribution'))
      }
      
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

