# app.R

library(shiny)
library(ggplot2)
library(mvmeta)
library(MASS)

source("R/analysis_functions.R")

# Built-in example dataset: simulated historical trials with meaningful between-study heterogeneity
simulate_default_hist <- function(seed = 20260307, n_hist = 27) {
  set.seed(seed)
  
  eta <- c(log(0.75), log(0.70))  # (OS, PFS)
  
  sd_os_between  <- 0.09
  sd_pfs_between <- 0.11
  rho_between    <- 0.75
  
  Sigma0 <- matrix(
    c(
      sd_os_between^2,
      rho_between * sd_os_between * sd_pfs_between,
      rho_between * sd_os_between * sd_pfs_between,
      sd_pfs_between^2
    ),
    nrow = 2,
    byrow = TRUE
  )
  
  theta_true <- MASS::mvrnorm(n = n_hist, mu = eta, Sigma = Sigma0)
  colnames(theta_true) <- c("theta_OS", "theta_PFS")
  
  SE_OS  <- runif(n_hist, min = 0.045, max = 0.085)
  SE_PFS <- runif(n_hist, min = 0.040, max = 0.075)
  R_WITHIN <- runif(n_hist, min = 0.35, max = 0.65)
  
  y_obs <- matrix(NA_real_, nrow = n_hist, ncol = 2)
  colnames(y_obs) <- c("logHR_OS", "logHR_PFS")
  
  for (i in seq_len(n_hist)) {
    S_i <- matrix(
      c(
        SE_OS[i]^2,
        R_WITHIN[i] * SE_OS[i] * SE_PFS[i],
        R_WITHIN[i] * SE_OS[i] * SE_PFS[i],
        SE_PFS[i]^2
      ),
      nrow = 2,
      byrow = TRUE
    )
    
    y_obs[i, ] <- MASS::mvrnorm(n = 1, mu = theta_true[i, ], Sigma = S_i)
  }
  
  data.frame(
    Study = paste0("Study_", seq_len(n_hist)),
    logHR_OS = y_obs[, "logHR_OS"],
    logHR_PFS = y_obs[, "logHR_PFS"],
    SE_OS = SE_OS,
    SE_PFS = SE_PFS,
    R_WITHIN = R_WITHIN
  )
}

default_hist <- simulate_default_hist()
example_template_hist <- default_hist

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: 'Helvetica Neue', Arial, sans-serif; background-color: #f5f7fa; }
      .navbar, .well { background-color: #ffffff; border: none; }
      .sidebar-section { margin-bottom: 6px; }
      .sidebar-section h5 {
        font-size: 13px; font-weight: 700; color: #3a3a5c;
        text-transform: uppercase; letter-spacing: 0.04em;
        margin-top: 14px; margin-bottom: 6px;
      }
      .sidebar-hr { border-top: 1px solid #e0e0e8; margin: 10px 0; }
      .main-section-title {
        font-size: 17px; font-weight: 700; color: #2c3e50;
        margin-top: 28px; margin-bottom: 10px;
        border-bottom: 2px solid #d5dce8; padding-bottom: 4px;
      }
      .help-box {
        background-color: #eef3fb; padding: 12px 16px;
        border-radius: 8px; border-left: 4px solid #3d7abf;
        margin-bottom: 18px; font-size: 13.5px;
      }
      .note-box {
        color: #0c4a7a; background-color: #ddeeff;
        padding: 10px 14px; border-radius: 6px;
        border-left: 4px solid #3d7abf; margin-bottom: 8px;
        font-size: 13.5px;
      }
      .warn-box {
        color: #664d03; background-color: #fff3cd;
        padding: 10px 14px; border-radius: 6px;
        border-left: 4px solid #f0ad4e; margin-bottom: 8px;
        font-size: 13.5px;
      }
      .ok-box {
        color: #0f5132; background-color: #d1e7dd;
        padding: 10px 14px; border-radius: 6px;
        border-left: 4px solid #2ecc71; margin-bottom: 8px;
        font-size: 13.5px;
      }
      .shiny-output-error { color: #c0392b; }
      .btn-primary { background-color: #3d7abf; border-color: #2c5f8a; width: 100%; font-weight: 600; }
      .btn-default { font-size: 13px; }
    "))
  ),
  
  titlePanel(
    div(
      style = "background: linear-gradient(90deg, #2c3e50 0%, #3d7abf 100%);
               color: white; padding: 16px 24px; border-radius: 8px; margin-bottom: 6px;",
      h2("Internal PoS App", style = "margin: 0; font-weight: 700;"),
      tags$small("Predictive probability of OS success via Daniels-Hughes surrogate model", style = "opacity: 0.85;")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      style = "background-color: #ffffff; border-radius: 8px; padding: 16px; box-shadow: 0 1px 4px rgba(0,0,0,0.08);",
      
      div(class = "sidebar-section",
        tags$h5("Historical Data"),
        fileInput("hist_csv", "Upload historical studies CSV", accept = c(".csv")),
        downloadButton("download_example_template", "Download Example CSV Template", class = "btn-default btn-sm"),
        br(), br(),
        checkboxInput("use_example_if_no_upload", "Use built-in example data if no CSV uploaded", value = TRUE),
        tags$small(style = "color: #666;", "Required columns: Study, logHR_OS, logHR_PFS, SE_OS, SE_PFS, R_WITHIN")
      ),
      
      div(class = "sidebar-hr"),
      
      div(class = "sidebar-section",
        tags$h5("OS Interim Data (Optional)"),
        checkboxInput("use_os_interim", "Include OS interim data", value = FALSE),
        conditionalPanel(
          condition = "input.use_os_interim == true",
          numericInput("current_os_int_loghr", "OS interim log(HR)", value = log(0.80), step = 0.01),
          numericInput("current_os_int_se", "OS interim SE", value = 0.20, step = 0.01),
          tags$small(style = "color: #666;", "Updates predicted final OS using both PFS and interim OS.")
        )
      ),
      
      div(class = "sidebar-hr"),
      
      div(class = "sidebar-section",
        tags$h5("Current Study Inputs"),
        numericInput("current_pfs_loghr", "PFS log(HR)", value = log(0.68), step = 0.01),
        numericInput("current_os_se", "OS SE", value = 0.12, min = 0.001, step = 0.01),
        numericInput("current_pfs_se", "PFS SE", value = 0.10, min = 0.001, step = 0.01),
        numericInput("current_rho", "Within-study correlation", value = 0.50, min = -0.99, max = 0.99, step = 0.05)
      ),
      
      div(class = "sidebar-hr"),
      
      div(class = "sidebar-section",
        tags$h5("Analysis Settings"),
        numericInput("success_loghr_threshold", "OS success threshold log(HR)", value = round(log(0.80), 4), step = 0.01),
        numericInput("pos_diff_warning_threshold", "Warning threshold for |PoS(REML) − PoS(MM)|", value = 0.05, min = 0, step = 0.01)
      ),
      
      div(class = "sidebar-hr"),
      
      div(class = "sidebar-section",
        tags$h5("Scenario Plot Settings"),
        numericInput("scenario_pfs_min", "Min PFS log(HR)", value = -0.40, step = 0.01),
        numericInput("scenario_pfs_max", "Max PFS log(HR)", value = 0.20, step = 0.01),
        numericInput("scenario_n", "Number of scenario points", value = 41, min = 5, step = 2)
      ),
      
      div(class = "sidebar-hr"),
      
      actionButton("run_btn", "Run Analysis", class = "btn-primary"),
      br(), br(),
      
      div(style = "display: flex; flex-direction: column; gap: 8px;",
        downloadButton("download_summary_text", "Download Summary Text", class = "btn-default btn-sm"),
        downloadButton("download_summary_csv", "Download Summary CSV", class = "btn-default btn-sm"),
        downloadButton("download_scenario_csv", "Download Scenario CSV", class = "btn-default btn-sm")
      )
    ),
    
    mainPanel(
      width = 9,
      style = "padding-left: 20px;",
      
      div(class = "main-section-title", "Data Source"),
      textOutput("data_source_text"),
      
      div(class = "main-section-title", "Help / Input Guide"),
      div(class = "help-box",
        tags$ul(style = "margin-bottom: 0;",
          tags$li("Required CSV columns: Study, logHR_OS, logHR_PFS, SE_OS, SE_PFS, R_WITHIN."),
          tags$li("SE_OS and SE_PFS must be > 0; R_WITHIN must be between −1 and 1."),
          tags$li("REML is the primary analysis; Method of Moments is a sensitivity check."),
          tags$li("If |PoS(REML) − PoS(MM)| exceeds the warning threshold, interpret with caution."),
          tags$li("More negative log(HR) indicates a more favourable treatment effect.")
        )
      ),
      
      div(class = "main-section-title", "Auto Summary"),
      verbatimTextOutput("auto_summary"),
      
      fluidRow(
        column(7,
          div(class = "main-section-title", "Main Results"),
          tableOutput("summary_table")
        ),
        column(5,
          div(class = "main-section-title", "Method Comparison"),
          tableOutput("comparison_table")
        )
      ),
      
      div(class = "main-section-title", "Notes & Warnings"),
      uiOutput("warning_box"),
      
      div(class = "main-section-title", "Predictive Distribution for Current OS"),
      plotOutput("pred_plot", height = "400px"),
      
      div(class = "main-section-title", "PFS Scenario Analysis"),
      fluidRow(
        column(7, plotOutput("scenario_plot_pos", height = "380px")),
        column(5, plotOutput("scenario_plot_diff", height = "380px"))
      ),
      
      div(class = "main-section-title", "Historical OS vs PFS Association"),
      plotOutput("scatter_plot", height = "420px"),
      
      div(class = "main-section-title", "Estimated Between-study Covariance Matrix"),
      fluidRow(
        column(6, tags$h5("REML", style = "font-weight: 700; color: #3a3a5c;"), tableOutput("psi_table_reml")),
        column(6, tags$h5("Method of Moments", style = "font-weight: 700; color: #3a3a5c;"), tableOutput("psi_table_mm"))
      ),
      
      div(class = "main-section-title", "Historical Data Used"),
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
      success_hr_threshold = exp(input$success_loghr_threshold),
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
        success_hr_threshold = exp(input$success_loghr_threshold),
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
    
    warn_msgs <- character(0)
    note_msgs <- character(0)
    
    collect_warns <- function(x, label) {
      if (inherits(x, "app_error")) {
        paste0(label, ": ", x$error_message)
      } else {
        if (length(x$warning_messages) > 0) paste0(label, ": ", x$warning_messages) else character(0)
      }
    }
    
    warn_msgs <- c(warn_msgs, collect_warns(res$reml, "REML"), collect_warns(res$mm, "Method of Moments"))
    
    reml_notes <- if (!inherits(res$reml, "app_error")) res$reml$note_messages else character(0)
    mm_notes   <- if (!inherits(res$mm,   "app_error")) res$mm$note_messages   else character(0)
    
    shared_notes    <- intersect(reml_notes, mm_notes)
    reml_only_notes <- setdiff(reml_notes, shared_notes)
    mm_only_notes   <- setdiff(mm_notes,   shared_notes)
    
    note_msgs <- c(
      note_msgs,
      shared_notes,
      if (length(reml_only_notes) > 0) paste0("REML: ", reml_only_notes) else character(0),
      if (length(mm_only_notes)   > 0) paste0("Method of Moments: ", mm_only_notes) else character(0)
    )
    
    if (!inherits(res$reml, "app_error") && !inherits(res$mm, "app_error")) {
      pos_diff <- abs(res$reml$pos - res$mm$pos)
      
      if (pos_diff > input$pos_diff_warning_threshold) {
        warn_msgs <- c(
          warn_msgs,
          paste0(
            "Method sensitivity warning: |PoS(REML) \u2212 PoS(MM)| = ",
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
          warn_msgs <- c(warn_msgs, prox_msg)
        }
      }
    }
    
    warn_msgs <- unique(warn_msgs)
    note_msgs <- unique(note_msgs)
    
    items <- list()
    
    if (length(note_msgs) > 0) {
      items <- c(items, lapply(note_msgs, function(msg) {
        div(class = "note-box",
          tags$span(style = "font-weight: 600; margin-right: 6px;", "\u2139\ufe0f Note:"),
          msg
        )
      }))
    }
    
    if (length(warn_msgs) > 0) {
      items <- c(items, lapply(warn_msgs, function(msg) {
        div(class = "warn-box",
          tags$span(style = "font-weight: 600; margin-right: 6px;", "\u26a0\ufe0f Warning:"),
          msg
        )
      }))
    }
    
    if (length(items) == 0) {
      return(div(class = "ok-box", "\u2705 No warnings."))
    }
    
    tagList(items)
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
      geom_smooth(method = "lm", se = TRUE, formula = y ~ x,
                  colour = "#2c5f8a", fill = "#2c5f8a", alpha = 0.15, linewidth = 1) +
      geom_point(size = 2.8, alpha = 0.85, colour = "#2c5f8a", shape = 16) +
      labs(
        x = "Historical PFS log(HR)",
        y = "Historical OS log(HR)",
        title = "Historical association between PFS and OS log(HR)",
        subtitle = "Shaded band shows 95% CI for linear trend; red dashed line marks current-study PFS input"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(colour = "#666666", size = 11),
        panel.grid.minor = element_blank()
      )
    # Add the current-study PFS reference line if available
    if (is.finite(cur_pfs)) {
      gg <- gg + geom_vline(xintercept = cur_pfs, linetype = "dashed",
                            colour = "#c0392b", linewidth = 0.9)
    }
    # Label a few extreme points to aid interpretation without cluttering
    if ("Study" %in% names(df)) {
      idx <- unique(c(
        which.min(df$logHR_PFS), which.max(df$logHR_PFS),
        which.min(df$logHR_OS),  which.max(df$logHR_OS)
      ))
      gg <- gg + geom_text(
        data = df[idx, , drop = FALSE],
        aes(label = Study),
        vjust = -0.7,
        size = 3,
        colour = "#444444",
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
    
    df <- data.frame(x = seq(x_min, x_max, length.out = 500))
    df$dens <- dnorm(df$x, mean = plot_res$mean_pred, sd = plot_res$sd_pred)
    df$region <- ifelse(df$x <= plot_res$threshold_loghr, "P(success)", "P(failure)")
    
    pos_label <- sprintf("PoS = %.1f%%", plot_res$pos * 100)
    
    ggplot(df, aes(x = x, y = dens)) +
      geom_area(aes(fill = region), alpha = 0.45) +
      geom_line(linewidth = 1.3, colour = "#2c3e50") +
      scale_fill_manual(
        values = c("P(success)" = "#2ecc71", "P(failure)" = "#e74c3c"),
        name = NULL
      ) +
      geom_vline(xintercept = plot_res$threshold_loghr, linetype = "dashed",
                 colour = "#555555", linewidth = 0.9) +
      annotate(
        "text",
        x = plot_res$threshold_loghr,
        y = max(df$dens) * 0.88,
        label = sprintf("Threshold\n(HR = %.2f)", exp(plot_res$threshold_loghr)),
        angle = 90,
        vjust = -0.4,
        size = 3.8,
        colour = "#555555"
      ) +
      annotate(
        "text",
        x = plot_res$mean_pred,
        y = max(df$dens) * 1.03,
        label = pos_label,
        size = 4.5,
        colour = "#2c3e50",
        fontface = "bold"
      ) +
      labs(
        title = "Predictive Distribution of Current-Study OS Effect",
        subtitle = "REML (primary method); shaded regions show probability of success vs failure",
        x = "Current OS log(HR)",
        y = "Density"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "top",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(colour = "#666666", size = 11),
        panel.grid.minor = element_blank()
      )
  })
  
  output$scenario_plot_pos <- renderPlot({
    df <- scenario_data()
    if (is.null(df)) return(NULL)
    
    df_long <- rbind(
      data.frame(current_pfs_loghr = df$current_pfs_loghr, PoS = df$pos_reml, Method = "REML"),
      data.frame(current_pfs_loghr = df$current_pfs_loghr, PoS = df$pos_mm, Method = "Method of Moments")
    )
    
    ggplot(df_long, aes(x = current_pfs_loghr, y = PoS, colour = Method, linetype = Method)) +
      geom_line(linewidth = 1.2) +
      geom_vline(xintercept = input$current_pfs_loghr, linetype = "dotted",
                 colour = "#555555", linewidth = 0.8) +
      annotate(
        "text",
        x = input$current_pfs_loghr,
        y = 0.06,
        label = "Current\ninput",
        angle = 90,
        vjust = -0.4,
        size = 3.5,
        colour = "#555555"
      ) +
      scale_colour_manual(
        values = c("REML" = "#2c5f8a", "Method of Moments" = "#c0392b"),
        name = "Method"
      ) +
      scale_linetype_manual(
        values = c("REML" = "solid", "Method of Moments" = "dashed"),
        name = "Method"
      ) +
      labs(
        title = "PoS vs Current-Study PFS log(HR)",
        x = "Current-study PFS log(HR)",
        y = "PoS"
      ) +
      coord_cartesian(ylim = c(0, 1)) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "top",
        plot.title = element_text(face = "bold", size = 13),
        panel.grid.minor = element_blank()
      )
  })
  
  output$scenario_plot_diff <- renderPlot({
    df <- scenario_data()
    if (is.null(df)) return(NULL)
    
    threshold_y <- input$pos_diff_warning_threshold
    
    ggplot(df, aes(x = current_pfs_loghr, y = abs_diff)) +
      geom_hline(yintercept = threshold_y, linetype = "dashed",
                 colour = "#e74c3c", linewidth = 0.9) +
      geom_area(fill = "#f4a261", alpha = 0.25) +
      geom_line(linewidth = 1.2, colour = "#e76f51") +
      geom_vline(xintercept = input$current_pfs_loghr, linetype = "dotted",
                 colour = "#555555", linewidth = 0.8) +
      annotate(
        "text",
        x = input$current_pfs_loghr,
        y = max(df$abs_diff, na.rm = TRUE) * 0.88,
        label = "Current\ninput",
        angle = 90,
        vjust = -0.4,
        size = 3.5,
        colour = "#555555"
      ) +
      annotate(
        "text",
        x = min(df$current_pfs_loghr, na.rm = TRUE),
        y = threshold_y,
        label = sprintf("Threshold (%.2f)", threshold_y),
        vjust = -0.5,
        hjust = 0,
        size = 3.5,
        colour = "#e74c3c"
      ) +
      labs(
        title = "|PoS(REML) \u2212 PoS(MM)| vs PFS log(HR)",
        x = "Current-study PFS log(HR)",
        y = "|PoS(REML) \u2212 PoS(MM)|"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 13),
        panel.grid.minor = element_blank()
      )
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
            "Success log(HR) threshold",
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
            round(input$success_loghr_threshold, 4),
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
