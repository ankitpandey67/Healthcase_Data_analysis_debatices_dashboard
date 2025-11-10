# -----------------------------------------
# Healthcare Data Insights - Enhanced Shiny Dashboard
# -----------------------------------------

# 📦 Load Libraries
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(ggplot2)
library(tidyverse)
library(caret)
library(corrplot)
library(ROCR)
library(caTools)
library(randomForest)
library(pheatmap)

# 🩺 Load Dataset
url <- "https://raw.githubusercontent.com/jbrownlee/Datasets/master/pima-indians-diabetes.data.csv"
colnames <- c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin",
              "BMI", "DiabetesPedigreeFunction", "Age", "Outcome")
data <- read.csv(url, header = FALSE, col.names = colnames)

# 🧹 Data Cleaning
data$Glucose[data$Glucose == 0] <- NA
data$BloodPressure[data$BloodPressure == 0] <- NA
data$BMI[data$BMI == 0] <- NA
data <- na.omit(data)

# 📊 Train/Test Split
set.seed(123)
split <- sample.split(data$Outcome, SplitRatio = 0.7)
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)

# 🤖 Logistic Regression Model
model <- glm(Outcome ~ ., data = train, family = binomial)
pred <- predict(model, newdata = test, type = "response")
pred_class <- ifelse(pred > 0.5, 1, 0)
accuracy <- round(mean(pred_class == test$Outcome) * 100, 2)

# ✅ Random Forest Model
rf_model <- randomForest(as.factor(Outcome) ~ ., data = train, ntree = 200, importance = TRUE)
rf_pred <- predict(rf_model, newdata = test)
rf_accuracy <- round(mean(rf_pred == test$Outcome) * 100, 2)

# ✅ Feature Importance
importance_df <- data.frame(
  Feature = rownames(importance(rf_model)),
  Importance = importance(rf_model)[, 1]
)

# ✅ Confusion Matrix
conf_matrix <- table(Predicted = rf_pred, Actual = test$Outcome)

# -----------------------------------------
# 🎨 CUSTOM CSS STYLING
# -----------------------------------------
customCSS <- "
body {
  background: linear-gradient(135deg, #e3f2fd, #fce4ec);
  font-family: 'Poppins', sans-serif;
  color: #333;
}
.box {
  border-radius: 20px !important;
  box-shadow: 0 4px 20px rgba(0,0,0,0.1);
}
h2, h3, h4 {
  font-weight: 600;
  color: #37474F;
}
.sidebar {
  background-color: #004D98 !important;
}
.skin-blue .main-header .logo {
  background-color: #004D98;
  color: white;
  font-weight: bold;
  letter-spacing: 1px;
}
.skin-blue .main-header .navbar {
  background-color: #1976D2;
}
.skin-blue .sidebar a {
  color: white !important;
}
.skin-blue .sidebar a:hover {
  background-color: #1565C0 !important;
}
#predictBtn {
  background-color: #FF4081 !important;
  border: none;
  font-weight: bold;
  transition: 0.3s;
}
#predictBtn:hover {
  background-color: #F50057 !important;
  transform: scale(1.05);
}
"

# -----------------------------------------
# 🧱 UI
# -----------------------------------------
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Healthcare Data Insights"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("info-circle")),
      menuItem("Data Visualization", tabName = "visuals", icon = icon("chart-area")),
      menuItem("Model Insights", tabName = "model", icon = icon("microscope")),
      menuItem("Advanced Analytics", tabName = "advanced", icon = icon("chart-line")),
      menuItem("Predict Diabetes", tabName = "predict", icon = icon("heartbeat"))
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML(customCSS))),
    
    tabItems(
      
      # ✅ UPDATED BEAUTIFUL OVERVIEW
      tabItem(tabName = "overview",
              
              tags$style(HTML("
                .overview-title {
                  font-size: 32px;
                  font-weight: 700;
                  color: #1A237E;
                  text-align: center;
                  margin-bottom: 20px;
                }
                .info-card {
                  background: #ffffff;
                  border-radius: 18px;
                  padding: 20px;
                  box-shadow: 0 6px 15px rgba(0,0,0,0.1);
                  border-left: 7px solid #1976D2;
                  transition: 0.3s;
                }
                .info-card:hover {
                  transform: translateY(-5px);
                  box-shadow: 0 10px 22px rgba(0,0,0,0.15);
                }
                .summary-card {
                  background: linear-gradient(135deg, #00BCD4, #2196F3);
                  color: white;
                  border-radius: 18px;
                  padding: 20px;
                  box-shadow: 0 6px 15px rgba(0,0,0,0.15);
                  border-left: 7px solid white;
                }
              ")),
              
              div(class = "overview-title", icon("stethoscope"), " Diabetes Prediction Dashboard"),
              
              fluidRow(
                column(6,
                       div(class = "info-card",
                           h3(icon("info-circle"), " About Project"),
                           p("This dashboard analyzes patient health data including BMI, Glucose, Age, and more,"),
                           p("to predict the likelihood of diabetes using advanced Machine Learning models."),
                           br(),
                           tags$ul(
                             tags$li("✅ Logistic Regression"),
                             tags$li("✅ Random Forest with Feature Importance"),
                             tags$li("✅ Real-time prediction for new patients")
                           ),
                           br(),
                           p(strong("Dataset Source:"), " Pima Indians Diabetes — UCI Machine Learning Repository")
                       )
                ),
                
                column(6,
                       div(class = "summary-card",
                           h3(icon("database"), " Dataset Summary"),
                           withSpinner(verbatimTextOutput("summary"), color = "#FFFFFF")
                       )
                )
              )
      ),
      
      # ---------------- DATA VISUALIZATION ----------------
      tabItem(tabName = "visuals",
              h3("📊 Data Visualization"),
              
              fluidRow(
                box(title = "Correlation Heatmap", width = 6, solidHeader = TRUE, status = "warning",
                    withSpinner(plotOutput("corrplot"), color="#FF9800")),
                box(title = "Outcome Distribution", width = 6, solidHeader = TRUE, status = "success",
                    withSpinner(plotOutput("outcomePlot"), color="#4CAF50"))
              ),
              
              fluidRow(
                box(title = "Age vs BMI", width = 12, solidHeader = TRUE, status = "primary",
                    withSpinner(plotOutput("ageBmiPlot"), color="#2196F3"))
              ),
              
              fluidRow(
                box(title = "Glucose Level Distribution", width = 12, solidHeader = TRUE, status = "danger",
                    withSpinner(plotOutput("glucoseHist"), color="#D32F2F"))
              ),
              
              fluidRow(
                box(title = "BMI vs Diabetes Outcome", width = 12, solidHeader = TRUE, status = "danger",
                    withSpinner(plotOutput("bmiBox"), color="#E91E63"))
              ),
              
              fluidRow(
                box(title = "Age Density by Outcome", width = 12, solidHeader = TRUE, status = "info",
                    withSpinner(plotOutput("ageDensity"), color="#2196F3"))
              ),
              
              fluidRow(
                box(title = "Pregnancies vs Outcome", width = 12, solidHeader = TRUE, status = "success",
                    withSpinner(plotOutput("pregPlot"), color="#4CAF50"))
              ),
              
              fluidRow(
                box(title = "Glucose vs BMI", width = 12, solidHeader = TRUE, status = "warning",
                    withSpinner(plotOutput("glucoseBmi"), color="#FFC107"))
              )
      ),
      
      # ---------------- MODEL INSIGHTS ----------------
      tabItem(tabName = "model",
              h3("🧠 Model Insights (Logistic Regression)"),
              fluidRow(
                box(title = "Model Summary", width = 6, solidHeader = TRUE, status = "primary",
                    withSpinner(verbatimTextOutput("modelSummary"), color="#2196F3")),
                box(title = "Model Accuracy", width = 6, solidHeader = TRUE, status = "success",
                    h3(textOutput("accuracyText")))
              ),
              fluidRow(
                box(title = "ROC Curve", width = 12, solidHeader = TRUE, status = "warning",
                    withSpinner(plotOutput("rocPlot"), color="#E91E63"))
              )
      ),
      
      # ---------------- ADVANCED ANALYTICS ----------------
      tabItem(tabName = "advanced",
              h3("🔬 Advanced Analytics (Random Forest)"),
              
              fluidRow(
                box(title = "Random Forest Accuracy", width = 6, solidHeader = TRUE, status = "success",
                    h3(textOutput("rfAccuracyText"))),
                box(title = "Accuracy Comparison", width = 6, solidHeader = TRUE, status = "info",
                    h3(textOutput("compareText")))
              ),
              
              fluidRow(
                box(title = "Feature Importance Plot", width = 12, solidHeader = TRUE, status = "primary",
                    withSpinner(plotOutput("importancePlot"), color="#1565C0"))
              ),
              
              fluidRow(
                box(title = "Confusion Matrix Heatmap", width = 12, solidHeader = TRUE, status = "danger",
                    withSpinner(plotOutput("confMatrixPlot"), color="#D32F2F"))
              )
      ),
      
      # ---------------- PREDICT DIABETES ----------------
      tabItem(tabName = "predict",
              h3("🧍 Predict Diabetes for a New Patient"),
              fluidRow(
                box(title = "Input Patient Data", width = 4, solidHeader = TRUE, status = "primary",
                    sliderInput("Pregnancies", "Pregnancies:", min = 0, max = 15, value = 2),
                    sliderInput("Glucose", "Glucose Level:", min = 50, max = 200, value = 120),
                    sliderInput("BloodPressure", "Blood Pressure:", min = 40, max = 130, value = 70),
                    sliderInput("BMI", "BMI:", min = 15, max = 50, value = 25),
                    sliderInput("Age", "Age:", min = 20, max = 80, value = 35),
                    actionButton("predictBtn", "Predict Diabetes", icon = icon("heartbeat"))
                ),
                box(title = "Prediction Result", width = 8, solidHeader = TRUE, status = "success",
                    h2(textOutput("predictionResult")),
                    h4(textOutput("probabilityText"))
                )
              )
      )
    )
  )
)

# -----------------------------------------
# ⚙️ SERVER
# -----------------------------------------
server <- function(input, output) {
  
  output$summary <- renderPrint({ summary(data) })
  
  output$corrplot <- renderPlot({
    corrplot(cor(data[,1:8]), method = "color", tl.cex = 0.8)
  })
  
  output$outcomePlot <- renderPlot({
    ggplot(data, aes(x = factor(Outcome), fill = factor(Outcome))) +
      geom_bar() +
      labs(title = "Diabetes Outcome Distribution", x = "Outcome") +
      scale_fill_manual(values = c("#00BCD4", "#E91E63")) +
      theme_minimal(base_size = 14)
  })
  
  output$ageBmiPlot <- renderPlot({
    ggplot(data, aes(x = Age, y = BMI, color = factor(Outcome))) +
      geom_point(size = 3, alpha = 0.7) + theme_minimal(base_size = 14)
  })
  
  output$glucoseHist <- renderPlot({
    ggplot(data, aes(x = Glucose)) +
      geom_histogram(binwidth = 10, fill = "#D32F2F", color = "white") + theme_minimal()
  })
  
  output$bmiBox <- renderPlot({
    ggplot(data, aes(x = factor(Outcome), y = BMI, fill = factor(Outcome))) +
      geom_boxplot() + theme_minimal()
  })
  
  output$ageDensity <- renderPlot({
    ggplot(data, aes(x = Age, fill = factor(Outcome))) +
      geom_density(alpha = 0.6) + theme_minimal()
  })
  
  output$pregPlot <- renderPlot({
    ggplot(data, aes(x = Pregnancies, fill = factor(Outcome))) +
      geom_bar(position = "dodge") + theme_minimal()
  })
  
  output$glucoseBmi <- renderPlot({
    ggplot(data, aes(x = Glucose, y = BMI, color = factor(Outcome))) +
      geom_point(size = 3, alpha = 0.7) + theme_minimal()
  })
  
  output$modelSummary <- renderPrint({ summary(model) })
  output$accuracyText <- renderText({ paste("✅ Logistic Regression Accuracy:", accuracy, "%") })
  
  output$rocPlot <- renderPlot({
    pred_obj <- prediction(pred, test$Outcome)
    perf <- performance(pred_obj, "tpr", "fpr")
    plot(perf, col = "#2196F3", lwd = 3, main = "ROC Curve")
    abline(a = 0, b = 1, lty = 2, col = "gray")
  })
  
  output$rfAccuracyText <- renderText({
    paste("🌳 Random Forest Accuracy:", rf_accuracy, "%")
  })
  
  output$compareText <- renderText({
    paste("✅ Logistic =", accuracy, "%  |  🌳 Random Forest =", rf_accuracy, "%")
  })
  
  output$importancePlot <- renderPlot({
    ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
      geom_bar(stat = "identity", fill = "#1565C0") +
      coord_flip() +
      labs(title = "Random Forest Feature Importance", x = "Feature", y = "Importance") +
      theme_minimal(base_size = 14)
  })
  
  output$confMatrixPlot <- renderPlot({
    pheatmap(conf_matrix, color = colorRampPalette(c("white", "red"))(50),
             main = "Confusion Matrix Heatmap")
  })
  
  observeEvent(input$predictBtn, {
    new_patient <- data.frame(
      Pregnancies = input$Pregnancies,
      Glucose = input$Glucose,
      BloodPressure = input$BloodPressure,
      SkinThickness = mean(data$SkinThickness),
      Insulin = mean(data$Insulin),
      BMI = input$BMI,
      DiabetesPedigreeFunction = mean(data$DiabetesPedigreeFunction),
      Age = input$Age
    )
    
    prob <- predict(model, new_patient, type = "response")
    class <- ifelse(prob > 0.5, "🩸 Likely Diabetic", "✅ Not Diabetic")
    
    output$predictionResult <- renderText({ class })
    output$probabilityText <- renderText({
      paste("Predicted Probability:", round(prob * 100, 2), "%")
    })
  })
}

# -----------------------------------------
# 🚀 RUN APP
# -----------------------------------------
shinyApp(ui, server)
