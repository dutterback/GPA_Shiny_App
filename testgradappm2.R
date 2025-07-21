library(shiny)
library(shinythemes)
library(ggplot2)
library(DT)
library(RColorBrewer)

# ✅ Load default model and dataset with safe logic
load("Graduate_GPA_Model.RData")  # Pre-trained default model

# ✅ Set your dataset path
dataset_path <- "Linear Regression Data Set.csv"  # Ensure this file is in the working directory

# ✅ Safe loading logic for default dataset
if (!is.null(dataset_path) && file.exists(dataset_path)) {
  default_dataset <- read.csv(dataset_path)
  if (!all(c("GGPA", "UGPA", "PGWE") %in% names(default_dataset))) {
    stop("Default dataset must include GGPA, UGPA, and PGWE columns.")
  }
  default_dataset$PGWE_Group <- cut(default_dataset$PGWE, breaks = c(-1, 12, 60, Inf),
                                    labels = c("0-12 months", "13-60 months", "61+ months"))
} else {
  warning("Default dataset not found. Using empty placeholder dataset.")
  default_dataset <- data.frame(
    GGPA = numeric(0),
    UGPA = numeric(0),
    PGWE = numeric(0),
    PGWE_Group = factor(levels = c("0-12 months", "13-60 months", "61+ months"))
  )
}

# ✅ Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Graduate GPA Prediction Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Single Applicant Prediction"),
      wellPanel(
        textInput("applicant_id", "Applicant Number:", placeholder = "e.g., Applicant 101"),
        numericInput("ugpa", "Undergraduate GPA:", min = 2.0, max = 4.0, value = 3.0, step = 0.01),
        numericInput("pgwe", "Post-Bachelor Work Experience (Months):", min = 0, max = 200, value = 12, step = 1),
        checkboxInput("cap", "Cap Prediction at 4.0 GPA?", value = TRUE),
        actionButton("predict_btn", "Generate Prediction", class = "btn-primary")
      ),
      
      h4("Batch Predictions"),
      wellPanel(
        fileInput("upload_file", "Upload CSV (UGPA, PGWE, [Applicant optional]):", accept = ".csv"),
        actionButton("batch_predict_btn", "Run Batch Prediction", class = "btn-success"),
        downloadButton("downloadTemplate", "Download Sample CSV")
      ),
      
      h4("Manage Predictions"),
      wellPanel(
        downloadButton("downloadData", "Download All Predictions"),
        br(), br(),
        actionButton("clear_btn", "Clear All Predictions", class = "btn-danger")
      )
    ),
    
    mainPanel(
      uiOutput("model_status"),  # Model status message box
      tabsetPanel(
        selected = "Instructions",
        tabPanel("Prediction Results",
                 h3("Summary"),
                 uiOutput("summary_info"),  # Dynamic summary info
                 hr(),
                 h3("Predicted Values"),
                 verbatimTextOutput("predictedGPA"),
                 verbatimTextOutput("predictionInterval"),
                 plotOutput("predPlot", height = "400px"),
                 br(),
                 h4("All Previous Predictions"),
                 DTOutput("predHistoryTable")
        ),
        tabPanel("Historical Data",
                 h3("UGPA vs GGPA (Filtered by PGWE Range)"),
                 plotOutput("historicalPlot", height = "500px")
        ),
        tabPanel("Custom Model",
                 h3("Train Custom Model"),
                 tags$p("Upload a dataset containing GGPA, UGPA, and PGWE to train a custom model. This will replace the default model until you reset it."),
                 
                 h4("Instructions:"),
                 tags$ol(
                   tags$li("Prepare your dataset as a CSV file."),
                   tags$li("Ensure the file includes the required columns: GGPA, UGPA, PGWE."),
                   tags$li("Optional column: Applicant (for reference only)."),
                   tags$li("Click 'Train Custom Model' to build the new model."),
                   tags$li("Reset to the default model anytime by clicking 'Reset to Default Model'.")
                 ),
                 
                 h4("Important Notes:"),
                 tags$ul(
                   tags$li("Graduate GPA (GGPA) refers to the final GPA of students who successfully graduated."),
                   tags$li("The default model was trained on a dataset of 200 entries."),
                   tags$li("For accuracy, use at least 30 records in your custom dataset."),
                   tags$li("Data quality matters: remove missing values and check ranges (UGPA 2.0–4.0, GGPA 2.0–4.0, PGWE 0–200 months).")
                 ),
                 
                 h4("Model Formula:"),
                 tags$p("GGPA ~ UGPA + PGWE + has_exp (binary: 1 if PGWE > 0, else 0)"),
                 
                 h4("Sample Custom Dataset:"),
                 tags$table(
                   border = 1,
                   tags$tr(tags$th("GGPA"), tags$th("UGPA"), tags$th("PGWE")),
                   tags$tr(tags$td("3.6"), tags$td("3.2"), tags$td("24")),
                   tags$tr(tags$td("3.9"), tags$td("3.8"), tags$td("48")),
                   tags$tr(tags$td("3.3"), tags$td("3.0"), tags$td("0"))
                 ),
                 
                 br(),
                 fileInput("custom_data_file", "Upload Custom Training Dataset (CSV):", accept = ".csv"),
                 actionButton("train_custom_model", "Train Custom Model", class = "btn-warning"),
                 actionButton("reset_model", "Reset to Default Model", class = "btn-info"),
                 br(), br(),
                 downloadButton("downloadCustomTemplate", "Download Custom Model Sample CSV"),
                 tags$small("Required columns: GGPA, UGPA, PGWE")
        ),
        tabPanel("Instructions",
                 h3("Welcome to the Graduate GPA Prediction Dashboard"),
                 tags$p("This tool predicts graduate GPA based on undergraduate GPA (UGPA) and post-bachelor work experience (PGWE) using a regression model."),
                 h4("How to Use:"),
                 tags$ol(
                   tags$li("Enter an applicant number (or leave blank to auto-generate)."),
                   tags$li("Enter UGPA and PGWE in months for single prediction."),
                   tags$li("Optionally, upload a CSV for batch prediction. Columns: UGPA, PGWE, Applicant(optional)."),
                   tags$li("Go to 'Custom Model' tab to train a new model with your dataset."),
                   tags$li("Reset to Default Model anytime by clicking 'Reset to Default Model'."),
                   tags$li("Compare results in 'Prediction Results' tab or view in 'Historical Data' tab."),
                   tags$li("Download or clear predictions anytime.")
                 ),
                 h4("Quick Tips:"),
                 tags$ul(
                   tags$li("Higher UGPA and work experience generally lead to higher predicted GPAs."),
                   tags$li("Confidence intervals show the likely range of actual GPA."),
                   tags$li("Use custom datasets for accurate, school-specific predictions.")
                 )
        )
      )
    )
  )
)

# ✅ Define server logic
server <- function(input, output) {
  
  # Reactive values for predictions
  predictions_history <- reactiveValues(data = data.frame(
    Applicant = character(0),
    UGPA = numeric(0),
    PGWE = numeric(0),
    Predicted_GGPA = numeric(0),
    Lower_95_PI = numeric(0),
    Upper_95_PI = numeric(0),
    Source = character(0)
  ))
  
  # Active model and dataset
  current_model <- reactiveVal(model_raw_pgwe)
  dataset <- reactiveVal(default_dataset)
  model_type <- reactiveVal("Default")
  
  # ✅ Train custom model
  observeEvent(input$train_custom_model, {
    req(input$custom_data_file)
    custom_data <- read.csv(input$custom_data_file$datapath)
    
    if (!all(c("GGPA", "UGPA", "PGWE") %in% names(custom_data))) {
      showNotification("Dataset must include GGPA, UGPA, and PGWE columns.", type = "error")
      return()
    }
    
    custom_data$has_exp <- ifelse(custom_data$PGWE > 0, 1, 0)
    new_model <- lm(GGPA ~ UGPA + PGWE + has_exp, data = custom_data)
    custom_data$PGWE_Group <- cut(custom_data$PGWE, breaks = c(-1, 12, 60, Inf),
                                  labels = c("0-12 months", "13-60 months", "61+ months"))
    
    current_model(new_model)
    dataset(custom_data)
    model_type("Custom")
    
    showNotification("Custom model trained successfully!", type = "message")
  })
  
  # ✅ Reset to default model
  observeEvent(input$reset_model, {
    current_model(model_raw_pgwe)
    dataset(default_dataset)
    model_type("Default")
    showNotification("Model reset to default successfully.", type = "message")
  })
  
  # ✅ Model status message
  output$model_status <- renderUI({
    if (model_type() == "Custom") {
      div(style = "background-color:#d4edda; color:#155724; padding:10px; border-radius:5px; margin-bottom:10px;",
          strong("Current Model: Custom"),
          br(),
          "Predictions are based on your uploaded dataset.")
    } else {
      div(style = "background-color:#cce5ff; color:#004085; padding:10px; border-radius:5px; margin-bottom:10px;",
          strong("Current Model: Default"),
          br(),
          "Predictions are based on the default dataset.")
    }
  })
  
  # ✅ Single prediction
  observeEvent(input$predict_btn, {
    has_exp_val <- ifelse(input$pgwe > 0, 1, 0)
    newdata <- data.frame(UGPA = input$ugpa, PGWE = input$pgwe, has_exp = has_exp_val)
    
    pred <- predict(current_model(), newdata = newdata, interval = "prediction")
    if (input$cap) pred <- pmin(pred, 4.0)
    
    applicant_id <- ifelse(input$applicant_id == "",
                           paste("Applicant", nrow(predictions_history$data) + 1),
                           input$applicant_id)
    
    predictions_history$data <- rbind(predictions_history$data, data.frame(
      Applicant = applicant_id,
      UGPA = newdata$UGPA,
      PGWE = newdata$PGWE,
      Predicted_GGPA = round(pred[,"fit"], 3),
      Lower_95_PI = round(pred[,"lwr"], 3),
      Upper_95_PI = round(pred[,"upr"], 3),
      Source = "Single"
    ))
  })
  
  # ✅ Batch predictions
  observeEvent(input$batch_predict_btn, {
    req(input$upload_file)
    uploaded_data <- read.csv(input$upload_file$datapath)
    
    if (!all(c("UGPA", "PGWE") %in% names(uploaded_data))) {
      showNotification("CSV must include UGPA and PGWE columns.", type = "error")
      return()
    }
    
    uploaded_data$has_exp <- ifelse(uploaded_data$PGWE > 0, 1, 0)
    preds <- predict(current_model(), newdata = uploaded_data, interval = "prediction")
    
    for (i in 1:nrow(uploaded_data)) {
      applicant_id <- if ("Applicant" %in% names(uploaded_data)) {
        as.character(uploaded_data$Applicant[i])
      } else {
        paste("Batch", nrow(predictions_history$data) + i)
      }
      
      predictions_history$data <- rbind(predictions_history$data, data.frame(
        Applicant = applicant_id,
        UGPA = uploaded_data$UGPA[i],
        PGWE = uploaded_data$PGWE[i],
        Predicted_GGPA = round(preds[i,"fit"], 3),
        Lower_95_PI = round(preds[i,"lwr"], 3),
        Upper_95_PI = round(preds[i,"upr"], 3),
        Source = "Batch"
      ))
    }
  })
  
  # ✅ Clear predictions
  observeEvent(input$clear_btn, {
    predictions_history$data <- predictions_history$data[0, ]
  })
  
  # ✅ Summary Info
  output$summary_info <- renderUI({
    total_preds <- nrow(predictions_history$data)
    single_preds <- sum(predictions_history$data$Source == "Single")
    batch_preds <- sum(predictions_history$data$Source == "Batch")
    
    div(style = "background:#f8f9fa; padding:10px; border-radius:5px;",
        strong("Summary:"),
        br(),
        paste("Total Predictions:", total_preds),
        br(),
        paste("Single Predictions:", single_preds),
        br(),
        paste("Batch Predictions:", batch_preds),
        br(),
        paste("Current Model:", model_type()))
  })
  
  # ✅ Prediction outputs
  output$predictedGPA <- renderPrint({
    if (nrow(predictions_history$data) == 0) return("No prediction yet.")
    latest <- tail(predictions_history$data, 1)
    paste("Applicant:", latest$Applicant, "| Predicted Graduate GPA:", latest$Predicted_GGPA)
  })
  
  output$predictionInterval <- renderPrint({
    if (nrow(predictions_history$data) == 0) return("")
    latest <- tail(predictions_history$data, 1)
    paste("95% Prediction Interval:", latest$Lower_95_PI, "to", latest$Upper_95_PI)
  })
  
  # ✅ Latest prediction plot
  output$predPlot <- renderPlot({
    if (nrow(predictions_history$data) == 0) return(NULL)
    latest <- tail(predictions_history$data, 1)
    
    plot(1, latest$Predicted_GGPA, pch = 19, ylim = c(2, 5), xlim = c(0.5, 1.5),
         xaxt = "n", xlab = "", ylab = "Predicted Graduate GPA",
         main = paste("Predicted GGPA for", latest$Applicant))
    segments(1, latest$Lower_95_PI, 1, latest$Upper_95_PI, col = "blue", lwd = 2)
    abline(h = 4, col = "red", lty = 2)
    text(1, latest$Predicted_GGPA, labels = latest$Predicted_GGPA, pos = 3)
  })
  
  # ✅ Prediction history table
  output$predHistoryTable <- renderDT({
    if (nrow(predictions_history$data) == 0) return(NULL)
    datatable(predictions_history$data, options = list(pageLength = 5))
  })
  
  # ✅ Historical plot
  output$historicalPlot <- renderPlot({
    plot_data <- dataset()
    if (input$pgwe <= 12) {
      filtered_dataset <- subset(plot_data, PGWE <= 12)
    } else if (input$pgwe <= 60) {
      filtered_dataset <- subset(plot_data, PGWE >= 13 & PGWE <= 60)
    } else {
      filtered_dataset <- subset(plot_data, PGWE >= 61)
    }
    
    p <- ggplot(filtered_dataset, aes(x = UGPA, y = GGPA, color = PGWE_Group)) +
      geom_point(alpha = 0.6, size = 3) +
      scale_color_brewer(palette = "Set2") +
      labs(title = "Historical UGPA vs GGPA (Filtered by PGWE Range)",
           x = "Undergraduate GPA", y = "Graduate GPA", color = "Work Experience") +
      theme_minimal(base_size = 14)
    
    if (nrow(predictions_history$data) > 0) {
      colors <- brewer.pal(max(3, nrow(predictions_history$data)), "Dark2")
      p <- p +
        geom_point(data = predictions_history$data,
                   aes(x = UGPA, y = Predicted_GGPA, color = Applicant),
                   size = 4, shape = 17) +
        geom_text(data = predictions_history$data,
                  aes(x = UGPA, y = Predicted_GGPA, label = Applicant, color = Applicant),
                  vjust = -1, size = 4) +
        scale_color_manual(values = colors)
    }
    
    p
  })
  
  # ✅ Download predictions
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("GPA_Predictions_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(predictions_history$data, file, row.names = FALSE)
    }
  )
  
  # ✅ Download sample templates
  output$downloadTemplate <- downloadHandler(
    filename = function() {
      "Sample_Template.csv"
    },
    content = function(file) {
      sample_data <- data.frame(
        Applicant = c("Applicant 101", "Applicant 102", "Applicant 103"),
        UGPA = c(3.4, 3.1, 3.7),
        PGWE = c(24, 36, 12)
      )
      write.csv(sample_data, file, row.names = FALSE)
    }
  )
  
  output$downloadCustomTemplate <- downloadHandler(
    filename = function() {
      "Custom_Model_Sample.csv"
    },
    content = function(file) {
      custom_sample <- data.frame(
        GGPA = c(3.8, 3.5, 3.9),
        UGPA = c(3.2, 3.0, 3.6),
        PGWE = c(12, 24, 36)
      )
      write.csv(custom_sample, file, row.names = FALSE)
    }
  )
}

# ✅ Run the app
shinyApp(ui = ui, server = server)
