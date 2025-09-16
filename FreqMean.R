library(shiny)

ui <- fluidPage(
  titlePanel("Mean and SD from Frequency Table"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Enter values and their frequencies separated by commas."),
      textInput("values", "Values (x):", "0,1,2,3,4"),
      textInput("freqs", "Frequencies (f):", "20,15,16,10,5"),
      actionButton("calc", "Calculate")
    ),
    
    mainPanel(
      h4("Results"),
      verbatimTextOutput("results")
    )
  )
)

server <- function(input, output) {
  
  freq_mean_sd <- function(x, f) {
    N <- sum(f)
    mean_val <- sum(x * f) / N
    var_val <- sum(f * (x - mean_val)^2) / (N - 1)
    sd_val <- sqrt(var_val)
    return(list(mean = mean_val, sd = sd_val, n = N))
  }
  
  results <- eventReactive(input$calc, {
    x <- as.numeric(unlist(strsplit(input$values, ",")))
    f <- as.numeric(unlist(strsplit(input$freqs, ",")))
    
    if(length(x) != length(f)) {
      return("Error: Values and frequencies must have the same length!")
    }
    
    res <- freq_mean_sd(x, f)
    paste0("Mean = ", round(res$mean, 3),
           "\nSD = ", round(res$sd, 3),
           "\nN = ", res$n)
  })
  
  output$results <- renderText({
    results()
  })
}

shinyApp(ui = ui, server = server)
