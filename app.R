# app.R

# test git pane

library(shiny)
library(ggplot2)
library(mvmeta)
library(MASS)

source("R/analysis_functions.R")

# Built-in example dataset
default_hist <- data.frame(
  Study = paste0("Study_", 1:12),
  logHR_OS  = c(-0.03, -0.22, -0.08, -0.28, -0.11,  0.02, -0.19, -0.13, -0.34, -0.06, -0.17, -0.25),
  logHR_PFS = c(-0.10, -0.37, -0.18, -0.44, -0.23, -0.02, -0.29, -0.16, -0.49, -0.11, -0.26, -0.35),
  SE_OS     = c(0.14, 0.11, 0.16, 0.10, 0.15, 0.17, 0.12, 0.14, 0.11, 0.16, 0.13, 0.12),
  SE_PFS    = c(0.12, 0.09, 0.13, 0.08, 0.11, 0.14, 0.10, 0.12, 0.09, 0.13, 0.10, 0.10),
  R_WITHIN  = c(0.45, 0.55, 0.40, 0.60, 0.50, 0.35, 0.58, 0.48, 0.62, 0.42, 0.50, 0.57)
)

ui <- fluidPage(
  titlePanel("Internal PoS App V1.4"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Historical Data Input"),
      fileInput("hist_csv", "Upload historical studies CSV", accept = c(".csv")),
      checkboxInput("use_example_if_no_upload", "Use built-in example data if no CSV uploaded", value = TRUE),
      br(),
      tags$small("Required CSV columns: Study, logHR_OS, logHR_PFS, SE_OS, SE_PFS, R_WITHIN"),
      
      hr(),
      
      h4("Current Study Inputs"),
      numericInput("current_pfs_loghr", "Current study PFS log(HR)", value = -0.22, step = 0.01),
      numericInput("current_os_se", "Current study OS SE", value = 0.12, min = 0.001, step = 0.01),
      numericInput("current_pfs_se", "Current study PFS SE", value = 0.10, min = 0.001, step = 0.01),
      numericInput("current_rho", "Current study within-study correlation", value = 0.50, min = -0.99, max = 0.99, step = 0.05),
      
      hr(),
      
      h4("Analysis Settings"),
      numericInput("success_hr_threshold", "OS success threshold (HR scale)", value = 0.80, min = 0.01, step = 0.01),
      numericInput("pos_diff_warning_threshold", "Warning threshold for |PoS(REML) - PoS(MM)|", value = 0.05, min = 0, step = 0.01),
      
      hr(),
      
      h4("Scenario Plot Settings"),
      numericInput("scenario_pfs_min", "Scenario min for current PFS log(HR)", value = -0.40, step = 0.01),
      numericInput("scenario_pfs_max", "Scenario max for current PFS log(HR)", value = 0.20, step = 0.01),
      numericInput("scenario_n", "Number of scenario points", value = 41, min = 5, step = 2),
      
      hr(),
      
      actionButton("run_btn", "Run Analysis", class = "btn-primary"),
      br(), br(),
      
      helpText("If no CSV is uploaded, the app can use the built-in example dataset.")
    ),
    
    mainPanel(
      h3("Data Source"),
      textOutput("data_source_text"),
      
      h3("Main Results"),
      tableOutput("summary_table"),
      
      h3("Method Comparison"),
      tableOutput("comparison_table"),
      
      h3("Warnings"),
      uiOutput("warning_box"),
      
      h3("Predictive Distribution for Current OS"),
      plotOutput("pred_plot", height = "380px"),
      
      h3("PFS Scenario Analysis"),
      plotOutput("scenario_plot_pos", height = "380px"),
      plotOutput("scenario_plot_diff", height = "320px"),
      
      h3("Estimated Between-study Covariance Matrix"),
      fluidRow(
        column(6, h4("REML"), tableOutput("psi_table_reml")),
        column(6, h4("Method of Moments"), tableOutput("psi_table_mm"))
      ),
      
      h3("Historical Data Used"),
      tableOutput("hist_table")
    )
  )
)

server <- function(input, output, session) {
  
  hist_data <- reactive({
    # Case 1: uploaded CSV
    if (!is.null(input$hist_csv)) {
      dat <- tryCatch(
        read.csv(input$hist_csv$datapath, stringsAsFactors = FALSE, check.names = FALSE),
        error = function(e) e
      )
      
      if (inherits(dat, "error")) {
        stop(paste("Failed to read uploaded CSV:", dat$message))
      }
      
      required_cols <- c("Study", "logHR_OS", "logHR_PFS", "SE_OS", "SE_PFS", "R_WITHIN")
      missing_cols <- setdiff(required_cols, names(dat))
      if (length(missing_cols) > 0) {
        stop(
          paste0(
            "Uploaded CSV is missing required columns: ",
            paste(missing_cols, collapse = ", ")
          )
        )
      }
      
      # Coerce numeric columns
      num_cols <- c("logHR_OS", "logHR_PFS", "SE_OS", "SE_PFS", "R_WITHIN")
      for (col in num_cols) {
        dat[[col]] <- suppressWarnings(as.numeric(dat[[col]]))
      }
      
      return(dat[, required_cols])
    }
    
    # Case 2: no upload, use example if allowed
    if (isTRUE(input$use_example_if_no_upload)) {
      return(default_hist)
    }
    
    # Case 3: no upload and example disabled
    stop("No historical dataset available. Please upload a CSV or enable the built-in example data.")
  })
  
  data_source_label <- reactive({
    if (!is.null(input$hist_csv)) {
      paste("Using uploaded CSV:", input$hist_csv$name)
    } else if (isTRUE(input$use_example_if_no_upload)) {
      "Using built-in example dataset"
    } else {
      "No dataset loaded"
    }
  })
  
  result <- eventReactive(input$run_btn, {
    compute_pos_both_methods(
      hist_dat = hist_data(),
      current_pfs_loghr = input$current_pfs_loghr,
      current_os_se = input$current_os_se,
      current_pfs_se = input$current_pfs_se,
      current_rho = input$current_rho,
      success_hr_threshold = input$success_hr_threshold
    )
  }, ignoreInit = TRUE)
  
  scenario_data <- eventReactive(input$run_btn, {
    pfs_min <- input$scenario_pfs_min
    pfs_max <- input$scenario_pfs_max
    n_pts <- input$scenario_n
    
    validate(
      need(is.numeric(pfs_min) && is.numeric(pfs_max), "Scenario range must be numeric."),
      need(pfs_min < pfs_max, "Scenario min must be smaller than scenario max."),
      need(n_pts >= 5, "Number of scenario points must be at least 5.")
    )
    
    grid <- seq(pfs_min, pfs_max, length.out = n_pts)
    
    out <- lapply(grid, function(g) {
      res <- compute_pos_both_methods(
        hist_dat = hist_data(),
        current_pfs_loghr = g,
        current_os_se = input$current_os_se,
        current_pfs_se = input$current_pfs_se,
        current_rho = input$current_rho,
        success_hr_threshold = input$success_hr_threshold
      )
      
      reml_pos <- if (!inherits(res$reml, "app_error")) res$reml$pos else NA_real_
      mm_pos   <- if (!inherits(res$mm, "app_error")) res$mm$pos else NA_real_
      
      data.frame(
        current_pfs_loghr = g,
        pos_reml = reml_pos,
        pos_mm = mm_pos,
        abs_diff = abs(reml_pos - mm_pos)
      )
    })
    
    do.call(rbind, out)
  }, ignoreInit = TRUE)
  
  output$data_source_text <- renderText({
    data_source_label()
  })
  
  output$summary_table <- renderTable({
    res <- result()
    if (is.null(res)) return(NULL)
    
    get_row <- function(x, label) {
      if (inherits(x, "app_error")) {
        return(data.frame(
          Method = label,
          PoS = NA,
          Predicted_OS_logHR = NA,
          Predictive_SD = NA,
          Status = paste("Error:", x$error_message),
          stringsAsFactors = FALSE
        ))
      }
      
      data.frame(
        Method = label,
        PoS = round(x$pos, 3),
        Predicted_OS_logHR = round(x$mean_pred, 3),
        Predictive_SD = round(x$sd_pred, 3),
        Status = "OK",
        stringsAsFactors = FALSE
      )
    }
    
    rbind(
      get_row(res$reml, "REML"),
      get_row(res$mm, "Method of Moments")
    )
  })
  
  output$comparison_table <- renderTable({
    res <- result()
    if (is.null(res)) return(NULL)
    
    if (inherits(res$reml, "app_error") || inherits(res$mm, "app_error")) {
      return(data.frame(
        Metric = "Absolute PoS Difference",
        Value = NA,
        Interpretation = "Unavailable because at least one method failed.",
        stringsAsFactors = FALSE
      ))
    }
    
    pos_diff <- abs(res$reml$pos - res$mm$pos)
    
    interpretation <- if (pos_diff > input$pos_diff_warning_threshold) {
      "Material difference across methods"
    } else {
      "Difference not material"
    }
    
    data.frame(
      Metric = c("Absolute PoS Difference", "REML PoS", "MM PoS"),
      Value = c(round(pos_diff, 3), round(res$reml$pos, 3), round(res$mm$pos, 3)),
      Interpretation = c(
        interpretation,
        "Primary analysis",
        "Sensitivity analysis"
      ),
      stringsAsFactors = FALSE
    )
  })
  
  output$warning_box <- renderUI({
    res <- result()
    if (is.null(res)) return(NULL)
    
    msgs <- character(0)
    
    collect_msgs <- function(x, label) {
      if (inherits(x, "app_error")) {
        return(paste0(label, ": ", x$error_message))
      }
      if (length(x$warning_messages) == 0) return(character(0))
      paste0(label, ": ", x$warning_messages)
    }
    
    msgs <- c(msgs, collect_msgs(res$reml, "REML"))
    msgs <- c(msgs, collect_msgs(res$mm, "Method of Moments"))
    
    if (!inherits(res$reml, "app_error") && !inherits(res$mm, "app_error")) {
      pos_diff <- abs(res$reml$pos - res$mm$pos)
      if (pos_diff > input$pos_diff_warning_threshold) {
        msgs <- c(
          msgs,
          paste0(
            "Method sensitivity warning: |PoS(REML) - PoS(MM)| = ",
            sprintf("%.3f", pos_diff),
            ", which exceeds the warning threshold of ",
            sprintf("%.3f", input$pos_diff_warning_threshold),
            ". Interpret the PoS result with caution because it is sensitive to the heterogeneity estimation method."
          )
        )
      }
    }
    
    msgs <- unique(msgs)
    
    if (length(msgs) == 0) {
      return(
        div(
          style = "color: #0f5132; background-color: #d1e7dd; padding: 12px; border-radius: 6px; border: 1px solid #badbcc;",
          "No major warnings."
        )
      )
    }
    
    tagList(
      lapply(msgs, function(msg) {
        div(
          style = "color: #664d03; background-color: #fff3cd; padding: 12px; border-radius: 6px; border: 1px solid #ffecb5; margin-bottom: 8px;",
          msg
        )
      })
    )
  })
  
  output$psi_table_reml <- renderTable({
    res <- result()
    if (is.null(res) || inherits(res$reml, "app_error")) return(NULL)
    
    psi <- round(res$reml$Sigma0_hat, 4)
    rownames(psi) <- c("logHR_OS", "logHR_PFS")
    colnames(psi) <- c("logHR_OS", "logHR_PFS")
    psi
  }, rownames = TRUE)
  
  output$psi_table_mm <- renderTable({
    res <- result()
    if (is.null(res) || inherits(res$mm, "app_error")) return(NULL)
    
    psi <- round(res$mm$Sigma0_hat, 4)
    rownames(psi) <- c("logHR_OS", "logHR_PFS")
    colnames(psi) <- c("logHR_OS", "logHR_PFS")
    psi
  }, rownames = TRUE)
  
  output$hist_table <- renderTable({
    hist_data()
  })
  
  output$pred_plot <- renderPlot({
    res <- result()
    if (is.null(res)) return(NULL)
    
    plot_res <- if (!inherits(res$reml, "app_error")) res$reml else res$mm
    if (inherits(plot_res, "app_error")) return(NULL)
    
    x_min <- min(plot_res$mean_pred - 4 * plot_res$sd_pred, plot_res$threshold_loghr - 0.5)
    x_max <- max(plot_res$mean_pred + 4 * plot_res$sd_pred, plot_res$threshold_loghr + 0.5)
    
    df <- data.frame(x = seq(x_min, x_max, length.out = 400))
    df$dens <- dnorm(df$x, mean = plot_res$mean_pred, sd = plot_res$sd_pred)
    
    ggplot(df, aes(x = x, y = dens)) +
      geom_line(linewidth = 1) +
      geom_vline(xintercept = plot_res$threshold_loghr, linetype = "dashed") +
      annotate(
        "text",
        x = plot_res$threshold_loghr,
        y = max(df$dens) * 0.9,
        label = "Success threshold",
        angle = 90,
        vjust = -0.5,
        size = 4
      ) +
      labs(
        title = "Predictive Distribution of Current OS Effect (Plot based on REML when available)",
        x = "Current OS log(HR)",
        y = "Density"
      ) +
      theme_minimal(base_size = 14)
  })
  
  output$scenario_plot_pos <- renderPlot({
    df <- scenario_data()
    if (is.null(df)) return(NULL)
    
    df_long <- rbind(
      data.frame(current_pfs_loghr = df$current_pfs_loghr, PoS = df$pos_reml, Method = "REML"),
      data.frame(current_pfs_loghr = df$current_pfs_loghr, PoS = df$pos_mm, Method = "Method of Moments")
    )
    
    ggplot(df_long, aes(x = current_pfs_loghr, y = PoS, linetype = Method)) +
      geom_line(linewidth = 1) +
      geom_vline(xintercept = input$current_pfs_loghr, linetype = "dotted") +
      annotate(
        "text",
        x = input$current_pfs_loghr,
        y = 0.05,
        label = "Current input",
        angle = 90,
        vjust = -0.5,
        size = 4
      ) +
      labs(
        title = "PoS vs Current-Study PFS log(HR)",
        x = "Current-study PFS log(HR)",
        y = "PoS"
      ) +
      coord_cartesian(ylim = c(0, 1)) +
      theme_minimal(base_size = 14)
  })
  
  output$scenario_plot_diff <- renderPlot({
    df <- scenario_data()
    if (is.null(df)) return(NULL)
    
    ggplot(df, aes(x = current_pfs_loghr, y = abs_diff)) +
      geom_line(linewidth = 1) +
      geom_hline(yintercept = input$pos_diff_warning_threshold, linetype = "dashed") +
      geom_vline(xintercept = input$current_pfs_loghr, linetype = "dotted") +
      annotate(
        "text",
        x = input$current_pfs_loghr,
        y = max(df$abs_diff, na.rm = TRUE) * 0.9,
        label = "Current input",
        angle = 90,
        vjust = -0.5,
        size = 4
      ) +
      labs(
        title = "Absolute PoS Difference: REML vs MM",
        x = "Current-study PFS log(HR)",
        y = "|PoS(REML) - PoS(MM)|"
      ) +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui, server)