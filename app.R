# app.R

library(shiny)
library(ggplot2)
library(mvmeta)
library(MASS)

source("R/analysis_functions.R")

# More realistic built-in example dataset than the earlier toy version
default_hist <- data.frame(
  Study = paste0("Study_", 1:10),
  logHR_OS  = c(-0.05, -0.18, -0.09, -0.25, -0.12, -0.02, -0.21, -0.14, -0.30, -0.08),
  logHR_PFS = c(-0.12, -0.34, -0.20, -0.40, -0.24, -0.10, -0.31, -0.19, -0.45, -0.16),
  SE_OS     = c(0.13, 0.11, 0.15, 0.10, 0.14, 0.16, 0.12, 0.13, 0.11, 0.15),
  SE_PFS    = c(0.11, 0.09, 0.12, 0.08, 0.10, 0.13, 0.10, 0.11, 0.09, 0.12),
  R_WITHIN  = c(0.50, 0.50, 0.45, 0.55, 0.50, 0.40, 0.60, 0.50, 0.55, 0.45)
)

ui <- fluidPage(
  titlePanel("Internal PoS App V1"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Current Study Inputs"),
      numericInput("current_pfs_loghr", "Current study PFS log(HR)", value = -0.22, step = 0.01),
      numericInput("current_os_se", "Current study OS SE", value = 0.12, min = 0.001, step = 0.01),
      numericInput("current_pfs_se", "Current study PFS SE", value = 0.10, min = 0.001, step = 0.01),
      numericInput("current_rho", "Current study within-study correlation", value = 0.50, min = -0.99, max = 0.99, step = 0.05),
      
      hr(),
      
      h4("Analysis Settings"),
      selectInput(
        "method",
        "Between-study covariance estimator",
        choices = c("REML" = "reml", "Method of Moments" = "mm"),
        selected = "reml"
      ),
      numericInput("success_hr_threshold", "OS success threshold (HR scale)", value = 1.00, min = 0.01, step = 0.01),
      
      hr(),
      
      actionButton("run_btn", "Run Analysis", class = "btn-primary"),
      br(), br(),
      
      helpText("This V1 app uses a built-in example historical dataset.")
    ),
    
    mainPanel(
      h3("Main Results"),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            h4("PoS"),
            textOutput("pos_text")
          )
        ),
        column(
          width = 4,
          wellPanel(
            h4("Predicted Current OS log(HR)"),
            textOutput("mean_text")
          )
        ),
        column(
          width = 4,
          wellPanel(
            h4("Predictive SD"),
            textOutput("sd_text")
          )
        )
      ),
      
      fluidRow(
        column(
          width = 6,
          wellPanel(
            h4("Threshold"),
            textOutput("threshold_text")
          )
        ),
        column(
          width = 6,
          wellPanel(
            h4("Estimated Historical Mean Effects"),
            tableOutput("eta_table")
          )
        )
      ),
      
      h3("Warnings"),
      uiOutput("warning_box"),
      
      h3("Predictive Distribution for Current OS"),
      plotOutput("pred_plot", height = "360px"),
      
      h3("Estimated Between-study Covariance Matrix"),
      tableOutput("psi_table"),
      
      h3("Historical Data Used"),
      tableOutput("hist_table")
    )
  )
)

server <- function(input, output, session) {
  
  hist_data <- reactive({
    default_hist
  })
  
  result <- eventReactive(input$run_btn, {
    tryCatch(
      {
        compute_pos(
          hist_dat = hist_data(),
          current_pfs_loghr = input$current_pfs_loghr,
          current_os_se = input$current_os_se,
          current_pfs_se = input$current_pfs_se,
          current_rho = input$current_rho,
          method = input$method,
          success_hr_threshold = input$success_hr_threshold
        )
      },
      error = function(e) {
        structure(list(error_message = e$message), class = "app_error")
      }
    )
  }, ignoreInit = TRUE)
  
  output$pos_text <- renderText({
    res <- result()
    if (inherits(res, "app_error")) return("N/A")
    sprintf("%.3f", res$pos)
  })
  
  output$mean_text <- renderText({
    res <- result()
    if (inherits(res, "app_error")) return("N/A")
    sprintf("%.3f", res$mean_pred)
  })
  
  output$sd_text <- renderText({
    res <- result()
    if (inherits(res, "app_error")) return("N/A")
    sprintf("%.3f", res$sd_pred)
  })
  
  output$threshold_text <- renderText({
    res <- result()
    if (inherits(res, "app_error")) return("N/A")
    sprintf(
      "HR threshold = %.3f; log(HR) threshold = %.3f",
      input$success_hr_threshold,
      res$threshold_loghr
    )
  })
  
  output$eta_table <- renderTable({
    res <- result()
    if (inherits(res, "app_error")) return(NULL)
    
    data.frame(
      Parameter = c("Mean log(HR)_OS", "Mean log(HR)_PFS"),
      Estimate = round(res$eta_hat, 4)
    )
  })
  
  output$warning_box <- renderUI({
    res <- result()
    
    if (is.null(res)) return(NULL)
    
    if (inherits(res, "app_error")) {
      return(
        div(
          style = "color: #842029; background-color: #f8d7da; padding: 12px; border-radius: 6px; border: 1px solid #f5c2c7;",
          strong("Error: "),
          res$error_message
        )
      )
    }
    
    if (length(res$warning_messages) == 0) {
      return(
        div(
          style = "color: #0f5132; background-color: #d1e7dd; padding: 12px; border-radius: 6px; border: 1px solid #badbcc;",
          "No major warnings."
        )
      )
    }
    
    tagList(
      lapply(res$warning_messages, function(msg) {
        div(
          style = "color: #664d03; background-color: #fff3cd; padding: 12px; border-radius: 6px; border: 1px solid #ffecb5; margin-bottom: 8px;",
          msg
        )
      })
    )
  })
  
  output$psi_table <- renderTable({
    res <- result()
    if (inherits(res, "app_error")) return(NULL)
    
    psi <- round(res$Sigma0_hat, 4)
    rownames(psi) <- c("logHR_OS", "logHR_PFS")
    colnames(psi) <- c("logHR_OS", "logHR_PFS")
    psi
  }, rownames = TRUE)
  
  output$hist_table <- renderTable({
    hist_data()
  })
  
  output$pred_plot <- renderPlot({
    res <- result()
    if (inherits(res, "app_error")) return(NULL)
    
    x_min <- min(res$mean_pred - 4 * res$sd_pred, res$threshold_loghr - 0.5)
    x_max <- max(res$mean_pred + 4 * res$sd_pred, res$threshold_loghr + 0.5)
    
    df <- data.frame(
      x = seq(x_min, x_max, length.out = 400)
    )
    df$dens <- dnorm(df$x, mean = res$mean_pred, sd = res$sd_pred)
    
    ggplot(df, aes(x = x, y = dens)) +
      geom_line(linewidth = 1) +
      geom_vline(xintercept = res$threshold_loghr, linetype = "dashed") +
      annotate(
        "text",
        x = res$threshold_loghr,
        y = max(df$dens) * 0.9,
        label = "Success threshold",
        angle = 90,
        vjust = -0.5,
        size = 4
      ) +
      labs(
        title = "Predictive Distribution of Current OS Effect",
        x = "Current OS log(HR)",
        y = "Density"
      ) +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui, server)