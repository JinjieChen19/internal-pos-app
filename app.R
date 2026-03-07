# app.R

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

example_template_hist <- data.frame(
  Study = paste0("Study_", sprintf("%02d", 1:20)),
  logHR_OS  = c(-0.05, -0.18, -0.09, -0.25, -0.11,  0.01, -0.20, -0.13, -0.31, -0.07,
                -0.16, -0.23, -0.02, -0.21, -0.10, -0.28, -0.14,  0.03, -0.26, -0.08),
  logHR_PFS = c(-0.12, -0.30, -0.20, -0.38, -0.24, -0.05, -0.29, -0.16, -0.47, -0.11,
                -0.25, -0.34, -0.09, -0.33, -0.18, -0.41, -0.22, -0.02, -0.36, -0.15),
  SE_OS     = c(0.14, 0.11, 0.15, 0.10, 0.14, 0.17, 0.12, 0.14, 0.11, 0.16,
                0.13, 0.12, 0.15, 0.11, 0.14, 0.10, 0.13, 0.18, 0.11, 0.15),
  SE_PFS    = c(0.12, 0.09, 0.12, 0.08, 0.10, 0.14, 0.10, 0.12, 0.09, 0.13,
                0.10, 0.10, 0.13, 0.09, 0.11, 0.08, 0.10, 0.15, 0.09, 0.12),
  R_WITHIN  = c(0.45, 0.52, 0.40, 0.60, 0.48, 0.35, 0.58, 0.46, 0.63, 0.42,
                0.50, 0.57, 0.39, 0.55, 0.44, 0.61, 0.49, 0.32, 0.59, 0.43)
)

ui <- fluidPage(
  titlePanel("Internal PoS App V1.7"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Historical Data Input"),
      fileInput("hist_csv", "Upload historical studies CSV", accept = c(".csv")),
      downloadButton("download_example_template", "Download Example CSV Template"),
      checkboxInput("use_example_if_no_upload", "Use built-in example data if no CSV uploaded", value = TRUE),
      br(),
      tags$small("Required CSV columns: Study, logHR_OS, logHR_PFS, SE_OS, SE_PFS, R_WITHIN"),
      
      hr(),
      
      checkboxInput("use_os_interim", "Use OS interim data (optional)", value = FALSE),
      
      conditionalPanel(
        condition = "input.use_os_interim == true",
        numericInput("current_os_int_loghr", "Current study OS interim log(HR)", value = -0.05, step = 0.01),
        numericInput("current_os_int_se", "Current study OS interim SE", value = 0.20, step = 0.01),
        tags$small("If selected, the model updates the predicted final OS effect using both PFS and interim OS evidence.")
      ),
      br(),
      
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
      
      downloadButton("download_summary_text", "Download Summary Text"),
      br(), br(),
      downloadButton("download_summary_csv", "Download Summary CSV"),
      br(), br(),
      downloadButton("download_scenario_csv", "Download Scenario CSV"),
      
      br(), br(),
      helpText("If no CSV is uploaded, the app can use the built-in example dataset.")
    ),
    
    mainPanel(
      h3("Data Source"),
      textOutput("data_source_text"),
      
      h3("Help / Input Guide"),
      tags$div(
        style = "background-color: #f8f9fa; padding: 14px; border-radius: 8px; border: 1px solid #dee2e6; margin-bottom: 18px;",
        tags$ul(
          tags$li("Required CSV columns: Study, logHR_OS, logHR_PFS, SE_OS, SE_PFS, R_WITHIN."),
          tags$li("SE_OS and SE_PFS must be > 0; R_WITHIN must be between -1 and 1."),
          tags$li("REML is used as the primary analysis; MM (Method of Moments) is shown as a sensitivity analysis."),
          tags$li("If |PoS(REML) - PoS(MM)| exceeds the warning threshold, interpret the result with caution."),
          tags$li("More negative log(HR) generally indicates a more favorable treatment effect.")
        )
      ),
      
      h3("Auto Summary"),
      verbatimTextOutput("auto_summary"),
      
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
##############################newly added on Mar 5, 2026, add scatter plot
      h3("Historical OS vs PFS (Scatter)"),
      plotOutput("scatter_plot", height = "420px"),
############################################      
      h3("Historical Data Used"),
      tableOutput("hist_table")
    )
  )
)

server <- function(input, output, session) {
  
  get_proximity_warning <- function(df, current_value, threshold, window_n = 2) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    if (all(is.na(df$abs_diff))) return(NULL)
    
    idx <- which.min(abs(df$current_pfs_loghr - current_value))
    if (length(idx) == 0 || is.na(idx)) return(NULL)
    
    left_idx <- max(1, idx - window_n)
    right_idx <- min(nrow(df), idx + window_n)
    
    local_df <- df[left_idx:right_idx, , drop = FALSE]
    
    current_diff <- df$abs_diff[idx]
    
    if (is.na(current_diff)) return(NULL)
    
    # Only trigger proximity warning if current point itself is NOT already above threshold
    if (current_diff > threshold) return(NULL)
    
    nearby_exceed <- any(local_df$abs_diff > threshold, na.rm = TRUE)
    
    if (!nearby_exceed) return(NULL)
    
    max_local_diff <- max(local_df$abs_diff, na.rm = TRUE)
    
    paste0(
      "Scenario proximity warning: the current input does not exceed the method-sensitivity threshold at the exact point ",
      "(|PoS(REML) - PoS(MM)| = ",
      sprintf("%.4f", current_diff),
      "), but nearby PFS scenarios do exceed the threshold. ",
      "This suggests the current result is close to a method-sensitive region. ",
      "Maximum nearby absolute difference = ",
      sprintf("%.4f", max_local_diff),
      "."
    )
  }
  
  hist_data <- reactive({
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
      
      num_cols <- c("logHR_OS", "logHR_PFS", "SE_OS", "SE_PFS", "R_WITHIN")
      for (col in num_cols) {
        dat[[col]] <- suppressWarnings(as.numeric(dat[[col]]))
      }
      
      return(dat[, required_cols])
    }
    
    if (isTRUE(input$use_example_if_no_upload)) {
      return(default_hist)
    }
    
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
      success_hr_threshold = input$success_hr_threshold,
      use_os_interim = isTRUE(input$use_os_interim),
      current_os_int_loghr = input$current_os_int_loghr,
      current_os_int_se = input$current_os_int_se
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
        success_hr_threshold = input$success_hr_threshold,
        use_os_interim = isTRUE(input$use_os_interim),
        current_os_int_loghr = input$current_os_int_loghr,
        current_os_int_se = input$current_os_int_se
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
  
  auto_summary_text <- reactive({
    res <- result()
    if (is.null(res)) return("Run the analysis to generate an automatic summary.")
    
    if (inherits(res$reml, "app_error") || inherits(res$mm, "app_error")) {
      return("Automatic summary unavailable because at least one method failed. Please review the warnings and inputs.")
    }
    
    inputs_used <- if (isTRUE(input$use_os_interim)) {
      "PFS + OS interim"
    } else {
      "PFS only"
    }
    
    pos_diff <- abs(res$reml$pos - res$mm$pos)
    material_flag <- pos_diff > input$pos_diff_warning_threshold
    
    interpretation_text <- if (material_flag) {
      paste0(
        "The absolute PoS difference between REML and method of moments is ",
        sprintf("%.3f", pos_diff),
        ", which exceeds the predefined warning threshold of ",
        sprintf("%.3f", input$pos_diff_warning_threshold),
        ". This suggests material sensitivity to the heterogeneity estimation method, and the current PoS result should be interpreted with caution."
      )
    } else {
      paste0(
        "The absolute PoS difference between REML and method of moments is ",
        sprintf("%.3f", pos_diff),
        ", which does not exceed the predefined warning threshold of ",
        sprintf("%.3f", input$pos_diff_warning_threshold),
        ". This suggests no material sensitivity to the heterogeneity estimation method under the current input scenario."
      )
    }
    
    paste0(
      "Inputs used: ",inputs_used, ". ",
      "Using REML as the primary analysis, the predictive probability of success (PoS) for OS is ",
      sprintf("%.3f", res$reml$pos),
      ", with a predicted current-study OS log(HR) of ",
      sprintf("%.3f", res$reml$mean_pred),
      " and predictive SD of ",
      sprintf("%.3f", res$reml$sd_pred),
      ". ",
      "Using method of moments as a sensitivity analysis, the PoS is ",
      sprintf("%.3f", res$mm$pos),
      ". ",
      interpretation_text
    )
  })
  
  output$data_source_text <- renderText({
    data_source_label()
  })
  
  output$auto_summary <- renderText({
    auto_summary_text()
  })
  
  output$summary_table <- renderTable({
    res <- result()
    if (is.null(res)) return(NULL)
    
    inputs_used <- if (isTRUE(input$use_os_interim)) {
      "PFS + OS interim"
    } else {
      "PFS only"
    }
    
    get_row <- function(x, label) {
      if (inherits(x, "app_error")) {
        return(data.frame(
          Method = label,
          PoS = NA,
          Predicted_OS_logHR = NA,
          Predictive_SD = NA,
          Inputs_Used = inputs_used,
          Status = paste("Error:", x$error_message),
          stringsAsFactors = FALSE
        ))
      }
      
      data.frame(
        Method = label,
        PoS = round(x$pos, 3),
        Predicted_OS_logHR = round(x$mean_pred, 3),
        Predictive_SD = round(x$sd_pred, 3),
        Inputs_Used = inputs_used,
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
            sprintf("%.4f", pos_diff),
            ", which exceeds the warning threshold of ",
            sprintf("%.4f", input$pos_diff_warning_threshold),
            ". Interpret the PoS result with caution because it is sensitive to the heterogeneity estimation method."
          )
        )
      } else {
        scen_df <- scenario_data()
        prox_msg <- get_proximity_warning(
          df = scen_df,
          current_value = input$current_pfs_loghr,
          threshold = input$pos_diff_warning_threshold,
          window_n = 2
        )
        
        if (!is.null(prox_msg)) {
          msgs <- c(msgs, prox_msg)
        }
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

###############newly added, scatter plot for input data/simulated data: March 5th 2026 by Jinjie
  output$scatter_plot <- renderPlot({
  # Pull historical dataset from the reactive source
  df <- hist_data()
  # Defensive checks: ensure data exists and required columns are present
  req(df)
  req(all(c("logHR_OS", "logHR_PFS") %in% names(df)))
  # Current-study PFS input (vertical reference line)
  cur_pfs <- suppressWarnings(as.numeric(input$current_pfs_loghr))
  if (!is.finite(cur_pfs)) cur_pfs <- NA_real_
  # Build the scatter plot: historical OS vs PFS relationship
  gg <- ggplot(df, aes(x = logHR_PFS, y = logHR_OS)) +
    geom_point(size = 2, alpha = 0.80) +
    # Add a simple linear trend line for visualization (not the meta-analytic model)
    geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
    labs(
      x = "Historical logHR_PFS",
      y = "Historical logHR_OS",
      title = "Historical association between PFS and OS",
      subtitle = "Dashed line indicates current-study PFS input"
    )
  # Add the current-study PFS reference line if available
  if (is.finite(cur_pfs)) {
    gg <- gg + geom_vline(xintercept = cur_pfs, linetype = "dashed")
  }
  # Optional: if a Study identifier exists, label a few extreme points to avoid clutter
  if ("Study" %in% names(df)) {
    idx <- unique(c(
      which.min(df$logHR_PFS), which.max(df$logHR_PFS),
      which.min(df$logHR_OS),  which.max(df$logHR_OS)
    ))
    gg <- gg + geom_text(
      data = df[idx, , drop = FALSE],
      aes(label = Study),
      vjust = -0.6,
      size = 3,
      check_overlap = TRUE
    )
  }
  gg
})
  
  #########################################################################################################
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
  
  output$download_summary_text <- downloadHandler(
    filename = function() {
      paste0("pos_summary_", Sys.Date(), ".txt")
    },
    content = function(file) {
      writeLines(auto_summary_text(), con = file)
    }
  )
  
  output$download_summary_csv <- downloadHandler(
    filename = function() {
      paste0("pos_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      res <- result()
      
      if (is.null(res) || inherits(res$reml, "app_error") || inherits(res$mm, "app_error")) {
        out <- data.frame(
          Metric = c("Status"),
          Value = c("Summary unavailable because analysis did not complete successfully."),
          stringsAsFactors = FALSE
        )
      } else {
        pos_diff <- abs(res$reml$pos - res$mm$pos)
        material_flag <- ifelse(pos_diff > input$pos_diff_warning_threshold, "Yes", "No")
        inputs_used <- if (isTRUE(input$use_os_interim)) {
          "PFS + OS interim"
        } else {
          "PFS only"
        }
        
        out <- data.frame(
          Metric = c(
            "Data source",
            "Inputs used",
            "Current PFS log(HR)",
            "Current OS SE",
            "Current PFS SE",
            "Current within-study correlation",
            "Success HR threshold",
            "REML PoS",
            "REML predicted OS log(HR)",
            "REML predictive SD",
            "MM PoS",
            "MM predicted OS log(HR)",
            "MM predictive SD",
            "Absolute PoS difference",
            "Material method sensitivity"
          ),
          Value = c(
            data_source_label(),
            inputs_used,
            round(input$current_pfs_loghr, 4),
            round(input$current_os_se, 4),
            round(input$current_pfs_se, 4),
            round(input$current_rho, 4),
            round(input$success_hr_threshold, 4),
            round(res$reml$pos, 4),
            round(res$reml$mean_pred, 4),
            round(res$reml$sd_pred, 4),
            round(res$mm$pos, 4),
            round(res$mm$mean_pred, 4),
            round(res$mm$sd_pred, 4),
            round(pos_diff, 4),
            material_flag
          ),
          stringsAsFactors = FALSE
        )
      }
      
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  output$download_scenario_csv <- downloadHandler(
    filename = function() {
      paste0("pos_scenario_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- scenario_data()
      
      if (is.null(df) || nrow(df) == 0) {
        out <- data.frame(
          Status = "Scenario data unavailable. Please run the analysis first.",
          stringsAsFactors = FALSE
        )
      } else {
        out <- df
        out$current_pfs_loghr <- round(out$current_pfs_loghr, 4)
        out$pos_reml <- round(out$pos_reml, 4)
        out$pos_mm <- round(out$pos_mm, 4)
        out$abs_diff <- round(out$abs_diff, 4)
      }
      
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  output$download_example_template <- downloadHandler(
    filename = function() {
      paste0("example_historical_data_template_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(example_template_hist, file, row.names = FALSE)
    }
  )
  
  
}

shinyApp(ui, server)
