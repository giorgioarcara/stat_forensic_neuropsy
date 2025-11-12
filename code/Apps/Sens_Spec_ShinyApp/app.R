library(shiny)
library(pROC)

ui <- fluidPage(
  titlePanel("Sensitivity, Specificity, and ROC Explorer"),
  sidebarLayout(
    sidebarPanel(
      numericInput("pat_n", "Number of Pathological", value = 50, min = 1),
      numericInput("healthy_n", "Number of Healthy", value = 50, min = 1),
      numericInput("pat_mean", "Pathological Mean", value = 20),
      numericInput("pat_sd", "Pathological SD", value = 2),
      numericInput("healthy_mean", "Healthy Mean", value = 24),
      numericInput("healthy_sd", "Healthy SD", value = 2),
      sliderInput("cutoff", "Cutoff Threshold", min = 0, max = 40, value = 15, step = 0.1),
      radioButtons("show_pathological", "Show Pathological Distribution",
                   choices = c("Yes" = TRUE, "No" = FALSE),
                   selected = TRUE, inline = TRUE),
      actionButton("regenerate", "Regenerate Data"),
      actionButton("setOptimal", "Set Optimal Threshold (Youden Index)")
    ),
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Simulated Data",
                           plotOutput("histPlot"),
                           h4("Proportions"),
                           fluidRow(
                             column(3, strong("Sensitivity:"), textOutput("sensitivity_sim")),
                             column(3, strong("Specificity:"), textOutput("specificity_sim")),
                             column(3, strong("Accuracy:"), textOutput("accuracy_sim")),
                             column(3, strong("AUC:"), textOutput("auc_sim"))
                           ),
                           fluidRow(
                             column(3, strong("True Positives:"), textOutput("TP_sim")),
                             column(3, strong("False Negatives:"), textOutput("FN_sim")),
                             column(3, strong("False Positives:"), textOutput("FP_sim")),
                             column(3, strong("True Negatives:"), textOutput("TN_sim"))
                           ),
                           hr(),
                           h4("Raw Counts"),
                           fluidRow(
                             column(3, strong("True Positives:"), textOutput("TP_count")),
                             column(3, strong("False Negatives:"), textOutput("FN_count")),
                             column(3, strong("False Positives:"), textOutput("FP_count")),
                             column(3, strong("True Negatives:"), textOutput("TN_count"))
                           )
                  ),
                  tabPanel("Density View",
                           plotOutput("densityPlot", height = "500px"),
                           fluidRow(
                             column(3, strong("Sensitivity:"), textOutput("sensitivity")),
                             column(3, strong("Specificity:"), textOutput("specificity")),
                             column(3, strong("Accuracy:"), textOutput("accuracy")),
                             column(3, strong("AUC:"), textOutput("auc"))
                           ),
                           fluidRow(
                             column(3, strong("True Positives:"), textOutput("TP")),
                             column(3, strong("False Negatives:"), textOutput("FN")),
                             column(3, strong("False Positives:"), textOutput("FP")),
                             column(3, strong("True Negatives:"), textOutput("TN"))
                           ),
                           hr(),
                           h4("What Do the Colors Mean?"),
                           tags$ul(
                             tags$li(strong("True Positive (Green):"), " Pathological subject correctly classified (score < cutoff)"),
                             tags$li(strong("False Negative (Red):"), " Pathological subject incorrectly classified as Healthy (score ≥ cutoff)"),
                             tags$li(strong("False Positive (Orange):"), " Healthy subject incorrectly classified as Pathological (score < cutoff)"),
                             tags$li(strong("True Negative (Blue):"), " Healthy subject correctly classified (score ≥ cutoff)")
                           )
                  )
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactiveVal()
  
  generateData <- function() {
    pat_n <- input$pat_n
    healthy_n <- input$healthy_n
    
    pat <- rnorm(pat_n, mean = input$pat_mean, sd = input$pat_sd)
    healthy <- rnorm(healthy_n, mean = input$healthy_mean, sd = input$healthy_sd)
    
    dat <- data.frame(
      Score = c(pat, healthy),
      Group = factor(c(rep("Pathological", pat_n), rep("Healthy", healthy_n)))
    )
    data(dat)
  }
  
  observeEvent(input$regenerate, {
    generateData()
  })
  
  observe({
    generateData()
  })
  
  rocData <- reactive({
    req(data())
    dat <- data()
    tryCatch({
      roc(dat$Group, dat$Score, ci = TRUE, percent = FALSE, direction = ">", quiet = TRUE)
    }, error = function(e) {
      NULL
    })
  })
  
  output$histPlot <- renderPlot({
    req(data())
    dat <- data()
    threshold <- input$cutoff
    roc_obj <- rocData()
    
    TP <- sum(dat$Score[dat$Group == "Pathological"] < threshold)
    TN <- sum(dat$Score[dat$Group == "Healthy"] >= threshold)
    FP <- sum(dat$Score[dat$Group == "Healthy"] < threshold)
    FN <- sum(dat$Score[dat$Group == "Pathological"] >= threshold)
    
    Sensitivity <- TP / (TP + FN)
    Specificity <- TN / (TN + FP)
    Accuracy <- (TP + TN) / (TP + TN + FP + FN)
    
    h1 <- hist(dat$Score[dat$Group == "Pathological"], plot = FALSE)
    h2 <- hist(dat$Score[dat$Group == "Healthy"], plot = FALSE)
    
    x_range <- range(c(h1$breaks, h2$breaks))
    y_range <- range(c(h1$counts, h2$counts))
    
    # Start with healthy if pathological is hidden
    if (as.logical(input$show_pathological)) {
      hist(dat$Score[dat$Group == "Pathological"], col = rgb(0, 0, 0, 0.5),
           xlim = x_range, ylim = y_range, breaks = 10,
           xlab = "Score", main = paste0("Specificity = ", round(Specificity,2),
                                         ", Sensitivity = ", round(Sensitivity,2),
                                         "\nAUC = ", round(roc_obj$auc, 2)))
      par(new = TRUE)
      hist(dat$Score[dat$Group == "Healthy"], col = rgb(0.3, 0.4, 0.8, 0.3),
           breaks = 10, xlim = x_range, ylim = y_range,
           xlab = "", ylab = "", main = "")
    } else {
      hist(dat$Score[dat$Group == "Healthy"], col = rgb(0.3, 0.4, 0.8, 0.5),
           xlim = x_range, ylim = y_range, breaks = 10,
           xlab = "Score", main = paste0("Specificity = ", round(Specificity,2),
                                         ", Sensitivity = ", round(Sensitivity,2),
                                         "\nAUC = ", round(roc_obj$auc, 2)))
    }
    
    abline(v = threshold, lwd = 2, col = "red")
    
    legend_labels <- if (as.logical(input$show_pathological)) {
      c("Pathological", "Healthy")
    } else {
      "Healthy"
    }
    legend_fills <- if (as.logical(input$show_pathological)) {
      c("black", rgb(0.3, 0.4, 0.8, 0.5))
    } else {
      rgb(0.3, 0.4, 0.8, 0.5)
    }
    legend("topright", legend = legend_labels, fill = legend_fills)
  })
  
  # Simulated Data panel outputs (proportions, matching Density View)
  output$sensitivity_sim <- renderText({
    req(data())
    dat <- data()
    threshold <- input$cutoff
    TP <- sum(dat$Score[dat$Group == "Pathological"] < threshold)
    FN <- sum(dat$Score[dat$Group == "Pathological"] >= threshold)
    round(TP / (TP + FN), 3)
  })
  
  output$specificity_sim <- renderText({
    req(data())
    dat <- data()
    threshold <- input$cutoff
    TN <- sum(dat$Score[dat$Group == "Healthy"] >= threshold)
    FP <- sum(dat$Score[dat$Group == "Healthy"] < threshold)
    round(TN / (TN + FP), 3)
  })
  
  output$accuracy_sim <- renderText({
    req(data())
    dat <- data()
    threshold <- input$cutoff
    TP <- sum(dat$Score[dat$Group == "Pathological"] < threshold)
    TN <- sum(dat$Score[dat$Group == "Healthy"] >= threshold)
    FP <- sum(dat$Score[dat$Group == "Healthy"] < threshold)
    FN <- sum(dat$Score[dat$Group == "Pathological"] >= threshold)
    round((TP + TN) / (TP + TN + FP + FN), 3)
  })
  
  output$auc_sim <- renderText({
    req(data())
    req(rocData())
    round(rocData()$auc, 3)
  })
  
  output$TP_sim <- renderText({
    req(data())
    dat <- data()
    threshold <- input$cutoff
    TP <- sum(dat$Score[dat$Group == "Pathological"] < threshold)
    total_pat <- sum(dat$Group == "Pathological")
    round(TP / total_pat, 3)
  })
  
  output$FN_sim <- renderText({
    req(data())
    dat <- data()
    threshold <- input$cutoff
    FN <- sum(dat$Score[dat$Group == "Pathological"] >= threshold)
    total_pat <- sum(dat$Group == "Pathological")
    round(FN / total_pat, 3)
  })
  
  output$FP_sim <- renderText({
    req(data())
    dat <- data()
    threshold <- input$cutoff
    FP <- sum(dat$Score[dat$Group == "Healthy"] < threshold)
    total_healthy <- sum(dat$Group == "Healthy")
    round(FP / total_healthy, 3)
  })
  
  output$TN_sim <- renderText({
    req(data())
    dat <- data()
    threshold <- input$cutoff
    TN <- sum(dat$Score[dat$Group == "Healthy"] >= threshold)
    total_healthy <- sum(dat$Group == "Healthy")
    round(TN / total_healthy, 3)
  })
  
  # Raw count outputs for Simulated Data panel
  output$TP_count <- renderText({
    req(data())
    dat <- data()
    threshold <- input$cutoff
    sum(dat$Score[dat$Group == "Pathological"] < threshold)
  })
  
  output$FN_count <- renderText({
    req(data())
    dat <- data()
    threshold <- input$cutoff
    sum(dat$Score[dat$Group == "Pathological"] >= threshold)
  })
  
  output$FP_count <- renderText({
    req(data())
    dat <- data()
    threshold <- input$cutoff
    sum(dat$Score[dat$Group == "Healthy"] < threshold)
  })
  
  output$TN_count <- renderText({
    req(data())
    dat <- data()
    threshold <- input$cutoff
    sum(dat$Score[dat$Group == "Healthy"] >= threshold)
  })
  
  output$densityPlot <- renderPlot({
    threshold <- input$cutoff
    mean_pat <- input$pat_mean
    sd_pat <- input$pat_sd
    mean_healthy <- input$healthy_mean
    sd_healthy <- input$healthy_sd
    
    x_vals <- seq(0, 40, by = 0.1)
    d_pat <- dnorm(x_vals, mean = mean_pat, sd = sd_pat)
    d_healthy <- dnorm(x_vals, mean = mean_healthy, sd = sd_healthy)
    
    sens <- pnorm(threshold, mean = mean_pat, sd = sd_pat)
    spec <- 1 - pnorm(threshold, mean = mean_healthy, sd = sd_healthy)
    accuracy <- (sens + spec) / 2
    auc <- pnorm((mean_healthy - mean_pat) / sqrt(sd_pat^2 + sd_healthy^2))
    
    plot(x_vals, d_pat, type = "n", ylim = c(0, max(c(d_pat, d_healthy)) * 1.1),
         xlab = "Score", ylab = "Density",
         main = paste0("Specificity = ", round(spec, 2),
                       ", Sensitivity = ", round(sens, 2),
                       "\nAUC = ", round(auc, 2)))
    
    if (as.logical(input$show_pathological)) {
      polygon(c(x_vals[x_vals < threshold], rev(x_vals[x_vals < threshold])),
              c(d_pat[x_vals < threshold], rep(0, sum(x_vals < threshold))),
              col = rgb(0, 0.7, 0, 0.4), border = NA)  # TP
      polygon(c(x_vals[x_vals >= threshold], rev(x_vals[x_vals >= threshold])),
              c(d_pat[x_vals >= threshold], rep(0, sum(x_vals >= threshold))),
              col = rgb(1, 0, 0, 0.4), border = NA)  # FN
    }
    
    polygon(c(x_vals[x_vals < threshold], rev(x_vals[x_vals < threshold])),
            c(d_healthy[x_vals < threshold], rep(0, sum(x_vals < threshold))),
            col = rgb(1, 0.5, 0, 0.4), border = NA)  # FP
    polygon(c(x_vals[x_vals >= threshold], rev(x_vals[x_vals >= threshold])),
            c(d_healthy[x_vals >= threshold], rep(0, sum(x_vals >= threshold))),
            col = rgb(0, 0, 1, 0.4), border = NA)  # TN
    
    if (as.logical(input$show_pathological)) {
      lines(x_vals, d_pat, col = "black", lwd = 2)
    }
    lines(x_vals, d_healthy, col = "blue", lwd = 2)
    abline(v = threshold, lwd = 2, lty = 2)
  })
  
  # Theoretical metric outputs (Density View panel)
  output$sensitivity <- renderText({
    round(pnorm(input$cutoff, mean = input$pat_mean, sd = input$pat_sd), 3)
  })
  output$specificity <- renderText({
    round(1 - pnorm(input$cutoff, mean = input$healthy_mean, sd = input$healthy_sd), 3)
  })
  output$accuracy <- renderText({
    s <- pnorm(input$cutoff, input$pat_mean, input$pat_sd)
    sp <- 1 - pnorm(input$cutoff, input$healthy_mean, input$healthy_sd)
    round((s + sp) / 2, 3)
  })
  output$auc <- renderText({
    d <- sqrt(input$pat_sd^2 + input$healthy_sd^2)
    auc <- pnorm((input$healthy_mean - input$pat_mean) / d)
    round(auc, 3)
  })
  
  output$TP <- renderText({
    round(pnorm(input$cutoff, input$pat_mean, input$pat_sd), 3)
  })
  output$FN <- renderText({
    round(1 - pnorm(input$cutoff, input$pat_mean, input$pat_sd), 3)
  })
  output$FP <- renderText({
    round(pnorm(input$cutoff, input$healthy_mean, input$healthy_sd), 3)
  })
  output$TN <- renderText({
    round(1 - pnorm(input$cutoff, input$healthy_mean, input$healthy_sd), 3)
  })
  
  observeEvent(input$setOptimal, {
    tab <- input$tabs
    if (tab == "Simulated Data") {
      req(data())
      req(rocData())
      roc_obj <- rocData()
      if (!is.null(roc_obj)) {
        tryCatch({
          threshold <- as.numeric(coords(roc_obj, "best", ret = "threshold", best.method = "youden"))
          updateSliderInput(session, "cutoff", value = round(threshold, 2))
        }, error = function(e) {
          showNotification("Error calculating optimal threshold. Please try regenerating data.", type = "error")
        })
      }
    } else {
      thresholds <- seq(0, 40, by = 0.1)
      youden <- sapply(thresholds, function(t) {
        sens <- pnorm(t, mean = input$pat_mean, sd = input$pat_sd)
        spec <- 1 - pnorm(t, mean = input$healthy_mean, sd = input$healthy_sd)
        sens + spec - 1
      })
      threshold <- thresholds[which.max(youden)]
      updateSliderInput(session, "cutoff", value = round(threshold, 2))
    }
  })
}

shinyApp(ui, server)