library(shiny)
library(DT)

source("StatTestFunctions.R")

# Run the following to export and create doc folder for GitHub Pages hosting:
# path = "C:/Users/Jordan.Aron/OneDrive - California OEHHA/Documents/PvalCalc/ExactTrendPairwiseCalculator"
# shinylive::export(appdir = "C:/Users/Jordan.Aron/OneDrive - California OEHHA/Documents/PvalCalc/ExactTrendPairwiseCalculator", destdir = "docs")
# httpuv::runStaticServer("docs")

# Link to app
# https://JordanAronOEHHA.github.io/ExactTrendPairwiseCalculator/

## This app allows users to input dose, incidence, and sample size data calculates the Cochran-Armitage trend test p-value and pairwise Fisher's exact test p-values. 
## Users can input data via text fields, an editable table, or by uploading a CSV

ui <- fluidPage(
  titlePanel("Exact C-A Trend and Fisher Pairwise P-value Calculator"),

  sidebarLayout(
    sidebarPanel(

      radioButtons(
        "input_mode",
        "Input Method:",
        choices = c("Text Input", "Table Input","CSV Upload"),
        selected = "Text Input"
      ),

      conditionalPanel(
        "input.input_mode == 'Text Input'",
        textInput("dose_text", "Dose (space‑separated):", "0 1.5 2.75 4.33"),
        textInput("inc_text", "Incidence (space‑separated):", "1 5 7 10"),
        textInput("n_text", "Sample Size (space‑separated):", "41 39 40 34")
      ),

      conditionalPanel(
        "input.input_mode == 'Table Input'",
        numericInput("num_groups", "Number of Groups:", 4, min = 2, max = 20),
        actionButton("clear_table", "Clear Table"),
        br(),
        br(),
        DTOutput("input_table")
      ),

      conditionalPanel(
        "input.input_mode == 'CSV Upload'",
        fileInput("csv_file", "Upload CSV File",
                  accept = c(".csv")),
        helpText("CSV should have 3 rows: Dose, Incidence, Sample size.")
      ),

      radioButtons(
        "trend_tail",
        "Trend Test Tail:",
        choices = c("Upper (Standard)" = "upper",
                    "Lower (Reversed)"  = "lower"),
        selected = "upper"
      ),

      br(),
      actionButton("run", "Run Tests")
    ),

    mainPanel(
      DTOutput("results"),
      br(),
      uiOutput("footnote")
    )
  )
)

server <- function(input, output, session) {

  # default values for table
  # padding with zeros allows us to easily extend to more groups if needed
  # up to 10 groups, probably not an issue
  default_dose  <- c(0, 1.5, 2.75, 4.33)
  default_inc   <- c(1, 5, 7, 10)
  default_n     <- c(41, 39, 40, 34)

  max_groups <- 10

  padded_dose <- c(default_dose, rep(0, max_groups - length(default_dose)))
  padded_inc  <- c(default_inc,  rep(0, max_groups - length(default_inc)))
  padded_n    <- c(default_n,    rep(0, max_groups - length(default_n)))


  # initiate reactive storage
  input_table_vals <- reactiveVal(NULL)   # Table for Table Input mode
  csv_table_vals   <- reactiveVal(NULL)   # Stores last uploaded CSV table


  # parse uploaded csv into reactive storage
  csv_data <- reactive({
    req(input$csv_file)

    df <- read.csv(input$csv_file$datapath,
                   header = FALSE,
                   stringsAsFactors = FALSE)

    if (nrow(df) != 3) {
      showNotification("CSV must have exactly 3 rows: Dose, Incidence, N.",
                       type = "error")
      return(NULL)
    }

    first_col <- df[,1]

    # Detect row names
    # Maybe update or get rid of this in the future 
    if (any(first_col %in% c("Dose", "Incidence", "N","dose", "incidence", "n","Sample Size","sample size","positive"))) {
      df <- df[, -1, drop = FALSE]
    }

    list(
      dose      = as.numeric(as.character(df[1, ])),
      incidence = as.numeric(as.character(df[2, ])),
      n         = as.numeric(as.character(df[3, ]))
    )
  })


  # Load CSV data into table and reactive storage when a new file is uploaded
  # Allows for multiple uploads 
  observeEvent(input$csv_file, {
    req(input$input_mode == "CSV Upload")

    csv <- csv_data()
    if (is.null(csv)) return()

    csv_tbl <- data.frame(
      Dose      = csv$dose,
      Incidence = csv$incidence,
      N         = csv$n
    )

    # Store for table mode
    csv_table_vals(csv_tbl)

    # Also display it in the CSV upload mode table (if needed)
    input_table_vals(csv_tbl)

    # now after uploading csv user can switch to table input mode and see the values in the table, make edits, etc. without needing to re-upload
    output$input_table <- DT::renderDT({
      DT::datatable(
        csv_tbl,
        editable = TRUE,
        rownames = FALSE,
        options = list(dom = "t",
                       ordering = FALSE)
      )
    })
  })



  # Table Input Mode: populate table
  # deals with padding too 
  observeEvent(list(input$input_mode, input$num_groups), {

    req(input$input_mode == "Table Input")
    ng <- input$num_groups

    uploaded_tbl <- isolate(csv_table_vals())
    old_tbl      <- isolate(input_table_vals())

    ## CASE 1: Use CSV values if they exist
    if (!is.null(uploaded_tbl)) {
      csv_ng <- nrow(uploaded_tbl)

      if (ng > csv_ng) {
        extra <- data.frame(
          Dose      = padded_dose[(csv_ng + 1):ng],
          Incidence = padded_inc[(csv_ng + 1):ng],
          N         = padded_n[(csv_ng + 1):ng]
        )
        new_tbl <- rbind(uploaded_tbl, extra)
      } else {
        new_tbl <- uploaded_tbl[1:ng, , drop = FALSE]
      }

    ## CASE 2: Use existing table values if they exist
    } else if (!is.null(old_tbl)) {

      old_ng <- nrow(old_tbl)

      if (ng > old_ng) {
        extra <- data.frame(
          Dose      = padded_dose[(old_ng + 1):ng],
          Incidence = padded_inc[(old_ng + 1):ng],
          N         = padded_n[(old_ng + 1):ng]
        )
        new_tbl <- rbind(old_tbl, extra)
      } else {
        new_tbl <- old_tbl[1:ng, , drop = FALSE]
      }

    ## CASE 3: Fall back to defaults
    } else {
      new_tbl <- data.frame(
        Dose      = padded_dose[1:ng],
        Incidence = padded_inc[1:ng],
        N         = padded_n[1:ng]
      )
    }

    input_table_vals(new_tbl)

    output$input_table <- DT::renderDT({
      DT::datatable(
        new_tbl,
        editable = TRUE,
        rownames = FALSE,
        options = list(dom = "t",
                       ordering = FALSE)
      )
    })
  })



  # Track user cell edits
  observeEvent(input$input_table_cell_edit, {
    info <- input$input_table_cell_edit
    tbl  <- isolate(input_table_vals())

    r <- info$row
    # DT is 0-based, R is 1-based
    # Need to add 1 here 
    c <- info$col + 1     

    new_val <- suppressWarnings(as.numeric(info$value))
    if (is.na(new_val) && !(info$value %in% c("0", "0.0"))) {
      showNotification("Invalid numeric entry.", type = "error")
      return()
    }

    tbl[r, c] <- new_val
    input_table_vals(tbl)
  })


  # Functionality for "Clear Table" button - resets to empty values (NA) rather than defaults, allows user to start fresh without needing to change number of groups
  observeEvent(input$clear_table, {
    req(input$input_mode == "Table Input")

    ng <- input$num_groups

    empty_tbl <- data.frame(
      Dose      = rep(NA_real_, ng),
      Incidence = rep(NA_real_, ng),
      N         = rep(NA_real_, ng)
    )

    # Update reactive storage
    input_table_vals(empty_tbl)

    # Re-render the table
    output$input_table <- DT::renderDT({
      DT::datatable(
        empty_tbl,
        editable = TRUE,
        rownames = FALSE,
        options = list(dom = "t", ordering = FALSE)
      )
    })
  })




  # Actually runs analysis when user clicks "Run Tests"
  observeEvent(input$run, {

    ## TEXT INPUT MODE
    if (input$input_mode == "Text Input") {
      #splits by spaces, converts to numeric, and unlists to get vectors
      dose_vec      <- as.numeric(strsplit(input$dose_text, "\\s+")[[1]])
      incidence_vec <- as.numeric(strsplit(input$inc_text, "\\s+")[[1]])
      n_vec         <- as.numeric(strsplit(input$n_text, "\\s+")[[1]])

    ## TABLE INPUT MODE
    } else if (input$input_mode == "Table Input") {

      tbl <- input_table_vals()
      if (is.null(tbl)) {
        showNotification("Table not initialized.", type = "error")
        return(NULL)
      }

      dose_vec      <- as.numeric(tbl$Dose)
      incidence_vec <- as.numeric(tbl$Incidence)
      n_vec         <- as.numeric(tbl$N)

    ## CSV UPLOAD MODE
    } else if (input$input_mode == "CSV Upload") {

      csv <- csv_data()
      if (is.null(csv)) return(NULL)

      dose_vec      <- csv$dose
      incidence_vec <- csv$incidence
      n_vec         <- csv$n
    }


    ############# Error Checking #############

    # length validation
    if (!(length(dose_vec) == length(incidence_vec) &&
          length(incidence_vec) == length(n_vec))) {
      showNotification("Dose, incidence, and N must have same length.",
                       type = "error")
      return(NULL)
    }

    ## Auto-sort dose if monotonicity broken
    if (is.unsorted(dose_vec)) {
      order_idx     <- order(dose_vec)
      dose_vec      <- dose_vec[order_idx]
      incidence_vec <- incidence_vec[order_idx]
      n_vec         <- n_vec[order_idx]
      showNotification("Dose vector must be monotonically increasing. Reordering inputs.",
                       type = "warning")
    }

    if (any(n_vec <= 0)) {
      showNotification("Sample size must be > 0.", type = "error")
      return(NULL)
    }

    if (any(incidence_vec < 0)) {
      showNotification("Incidence cannot be negative.", type = "error")
      return(NULL)
    }

    if (any(incidence_vec > n_vec)) {
      showNotification("Incidence cannot exceed sample size.", type = "error")
      return(NULL)
    }

    if (length(n_vec) == 2) {
      showNotification("At least 3 groups are required for trend test. Only calculating pairwise p-value", type = "warning")
    }

    ############# Stat Calculation #############

    if (length(n_vec) >= 3) {
      tail_choice <- input$trend_tail
      trend_pval <- CalculateTrendPval(incidence_vec, n_vec, dose_vec, tail = tail_choice)
    } else if (length(n_vec) == 2) {
      trend_pval <- NA
    } else {
      showNotification("At least 2 groups are required.", type = "error")
      return(NULL)
    }

    pval_vec   <- CalculatePairwisePval(incidence_vec, n_vec)
    pval_vec[1] <- trend_pval

    symbol_vec <- cut(
      pval_vec,
      breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
      labels = c("***", "**", "*", ""),
      right = FALSE
    )

    symbol_vec[is.na(pval_vec)] <- ""  # Handle NA trend p-value case

    incidence_fmt <- paste0(
      incidence_vec, "/", n_vec, symbol_vec,
      "<br>(p = ", signif(pval_vec, 3), ")"
    )

    

    ############# Output #############
    out_df <- rbind(
      Dose      = dose_vec,
      Incidence = incidence_fmt
    )

    out_df <- as.data.frame(out_df)
    colnames(out_df) <- paste0("Group ", seq_len(ncol(out_df)))

    output$results <- DT::renderDT({
      DT::datatable(
        out_df,
        rownames = TRUE,
        options = list(dom = "t", ordering = FALSE),
        escape = FALSE
      )
    })


    output$footnote <- renderUI({
    div(
      style = "margin-top: 10px; font-size: 1.3em;",
      "C-A exact trend test p-value (upper/lower dependent on user choice) shown below group 1.",
      "Corresponding Fisher exact pairwise test one-sided upper p-value below other groups.",
      "Asterisks indicate significant results:",
      "* p < 0.05, ** p < 0.01, *** p < 0.001."
    )
  })



  })
}

shinyApp(ui, server)