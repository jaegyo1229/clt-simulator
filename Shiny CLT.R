# https://chatgpt.com/g/g-p-6a95222aded081918b1ee698a2087646/c/6a9b7803-3054-83e8-8929-54f125d5a223

library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  
  titlePanel("Central Limit Theorem Simulator"),
  

  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(
        inputId = "distribution",
        label = "Population distribution",
        choices = c("Exponential", "Uniform", "Normal"),
        selected = "Exponential"), 
      
      sliderInput(
        inputId = "n",
        label = "Sample size",
        min = 1,
        max = 1000,
        value = 1000
      ),
      
      sliderInput(
        inputId = "B",
        label = "Number of samples",
        min = 100,
        max = 1000,
        value = 1000,
        step = 100
      ), 
      
      
      conditionalPanel(
        condition = "input.distribution == 'Exponential'", 
        sliderInput(
          inputId = "rate",
          label = "Rate",
          min = 0.1,
          max = 2,
          value = 0.5,
          step = 0.1
        )), 
      
      
      conditionalPanel(
        condition = "input.distribution == 'Uniform'",
        
        sliderInput(
          inputId = "unif_range",
          label = "Range",
          min = -5,
          max = 5,
          value = c(0, 1),
          step = 0.5
        )),
        
        
      conditionalPanel(
        condition = "input.distribution == 'Normal'",
        
        sliderInput(
          inputId = "normal_mean",
          label = "Mean",
          min = -5,
          max = 5,
          value = 0,
          step = 0.5
        ),
        
        sliderInput(
          inputId = "normal_sd",
          label = "Standard deviation",
          min = 0.1,
          max = 5,
          value = 1,
          step = 0.1
        )),
      
      hr(),
      
      uiOutput("population_info"),
      
      hr(),
      
      actionButton(
        inputId = "generate",
        label = "Generate"
      )
      
    ),
    
    mainPanel(
      plotOutput("sample_plot", height = "300px"), 
      plotOutput("sampling_plot", height = "300px")
    )
  )
)
# ui end 





server <- function(input, output, session) {
  simulation <- eventReactive(input$generate, {
    
    sample_means <- numeric(input$B)
    last_sample <- NULL
    
    rate <- NULL
    unif_min <- NULL
    unif_max <- NULL
    normal_mean <- NULL
    normal_sd <- NULL
    
    if (input$distribution == "Exponential") {
      
      rate <- input$rate
      
      mu <- 1 / rate
      sigma <- 1 / rate
      
    } else if (input$distribution == "Uniform") {
      
      unif_min <- input$unif_range[1]
      unif_max <- input$unif_range[2]
      
      mu <- (unif_min + unif_max) / 2
      sigma <- (unif_max - unif_min) / sqrt(12)
      
    } else if (input$distribution == "Normal") {
      
      normal_mean <- input$normal_mean
      normal_sd <- input$normal_sd
      
      mu <- normal_mean
      sigma <- normal_sd
      
    }
    
    
    for (i in 1:input$B) {
      
      if (input$distribution == "Exponential") {
        
        sample <- rexp(
          input$n,
          rate = rate
        )
        
      } else if (input$distribution == "Uniform") {
        
        sample <- runif(
          input$n,
          min = unif_min,
          max = unif_max
        )
        
      } else if (input$distribution == "Normal") {
        
        sample <- rnorm(
          input$n,
          mean = normal_mean,
          sd = normal_sd
        )
        
      }
      
      sample_means[i] <- mean(sample)
      last_sample <- sample
    }
    
    
    list(
      sample = last_sample,
      sample_means = sample_means,
      n = input$n,
      distribution = input$distribution,
      mu = mu,
      sigma = sigma,
      
      rate = rate,
      unif_min = unif_min,
      unif_max = unif_max,
      normal_mean = normal_mean,
      normal_sd = normal_sd
    )
  })
  # reactive end 
  
  
  output$population_info <- renderUI({
    
    mu <- simulation()$mu
    sigma <- simulation()$sigma
    se <- sigma / sqrt(simulation()$n)
    
    mean_sample_means <- mean(simulation()$sample_means)
    sd_sample_means <- sd(simulation()$sample_means)
    
    
    tagList(
      paste0("Population mean: ", round(mu, 2)), br(),
      paste0("Population SD: ", round(sigma, 2)), br(),
      paste0("Standard error: ", round(se, 2)), br(), br(), 
      paste0("Mean of sample means: ", round(mean_sample_means, 2)), br(),
      paste0("SD of sample means: ", round(sd_sample_means, 2))
    )
  })
  # population_info end
  
  
  output$sample_plot <- renderPlot({
    
    p <- data.frame(x = simulation()$sample) %>%
      ggplot(aes(x = x)) +
      geom_histogram(
        aes(y = after_stat(density)),
        bins = 20,
        color = "black",
        fill = "white"
      ) +
      geom_vline(
        xintercept = simulation()$mu,
        linetype = "dashed"
      ) +
      labs(
        title = "Last sample",
        x = "x",
        y = "Density"
      )
    
    if (simulation()$distribution == "Exponential") {
      
      p <- p +
        stat_function(
          fun = dexp,
          args = list(rate = simulation()$rate),
          linewidth = 0.6
        )
      
    } else if (simulation()$distribution == "Uniform") {
      
      p <- p +
        stat_function(
          fun = dunif,
          args = list(min = simulation()$unif_min, max = simulation()$unif_max),
          linewidth = 0.6
        )
      
    } else if (simulation()$distribution == "Normal") {
      
      p <- p +
        stat_function(
          fun = dnorm,
          args = list(mean = simulation()$mu,
                      sd = simulation()$sigma),
          linewidth = 0.6)
      
    }
    
    p
  })
  
  output$sampling_plot <- renderPlot({
    
    data.frame(sample_mean = simulation()$sample_means) %>%
      ggplot(aes(x = sample_mean)) +
      geom_histogram(
        aes(y = after_stat(density)),
        bins = 30,
        color = "black",
        fill = "white"
      ) +
      stat_function(
        fun = dnorm,
        args = list(
          mean = simulation()$mu,
          sd = simulation()$sigma / sqrt(simulation()$n)
        ),
        linewidth = 0.6
      ) +
      geom_vline(
        xintercept = simulation()$mu,
        linetype = "dashed"
      ) +
      labs(
        title = "Sampling distribution of sample mean",
        x = "Sample mean",
        y = "Density"
      )
  })
  
}
# server end


shinyApp(ui = ui, server = server)
