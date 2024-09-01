
library(shiny)
library(glmnet)
library(rpart)
library(rpart.plot)


ui <- fluidPage(
  titlePanel("BIAS-VARIANCE TRADEOFF VISUALIZATION"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Dropdown menu to select between apps
      selectInput("app_choice", "Task",
                  choices = c("Regression", "Classification"),
                  selected = "Regression"),
      
      uiOutput("app_ui")  # Placeholder for app-specific UI
    ),
    
    mainPanel(
      uiOutput("app_main")  # Placeholder for app-specific main content
    )
  )
)
server <- function(input, output, session) {
  
  # Reactive for the selected app
  observe({
    if (input$app_choice == "Regression") {
      # Logic for Regression UI
      output$app_ui <- renderUI({
        tagList(
          selectInput("dataset", "Dataset",
                      choices = c("Data set 1", "Data set 2", "Data set 3"),
                      selected = "Data set 2"),
          
          sliderInput("num_ob", "Number of observations",
                      min = 50, max = 250, value = 100, step = 100),
          
          uiOutput("epsilon_slider")  # Dynamic slider for epsilon
        )
      })
      
      output$app_main <- renderUI({
        tabsetPanel(
          id = "tabs",
          tabPanel("Non-Linear",
                   sliderInput("flexibility", "Flexibility", 
                               min = 1, max = 5, value = 1.5, step = 0.5),
                   checkboxInput("show_tab1_plot5", "Show True Model vs. Prediction Graphs", value = FALSE),
                   fluidRow(
                     column(3, plotOutput("tab1_plot1")),
                     column(3, plotOutput("tab1_plot2")),
                     column(3, plotOutput("tab1_plot3")),
                     column(3, plotOutput("tab1_plot4"))
                   )
                   ,
                   fluidRow(
                     column(3, conditionalPanel(
                       condition = "input.show_tab1_plot5 == true",
                       plotOutput("tab1_plot5")
                     )),
                     column(3, conditionalPanel(
                       condition = "input.show_tab1_plot5 == true",
                       plotOutput("tab1_plot6")
                     )),
                     column(3, conditionalPanel(
                       condition = "input.show_tab1_plot5 == true",
                       plotOutput("tab1_plot7")
                     )),
                     column(3, conditionalPanel(
                       condition = "input.show_tab1_plot5 == true",
                       plotOutput("tab1_plot8")
                     ))
                   )
          ),
          tabPanel("Polynomial",
                   sliderInput("degree", "Polynomial degree:", 
                               min = 1, max = 5, value = 2, step = 1),
                   checkboxInput("show_tab2_plot5", "Show True Model vs. Prediction Graphs", value = FALSE),
                   fluidRow(
                     column(3, plotOutput("tab2_plot1")),
                     column(3, plotOutput("tab2_plot2")),
                     column(3, plotOutput("tab2_plot3")),
                     column(3, plotOutput("tab2_plot4")))
                   ,
                   fluidRow(
                     column(3, conditionalPanel(
                       condition = "input.show_tab2_plot5 == true",
                       plotOutput("tab2_plot5")
                     )),
                     column(3, conditionalPanel(
                       condition = "input.show_tab2_plot5 == true",
                       plotOutput("tab2_plot6")
                     )),
                     column(3, conditionalPanel(
                       condition = "input.show_tab2_plot5 == true",
                       plotOutput("tab2_plot7")
                     )),
                     column(3, conditionalPanel(
                       condition = "input.show_tab2_plot5 == true",
                       plotOutput("tab2_plot8")
                     ))
                   )
          ),
          tabPanel("KNN",
                   sliderInput("k_value", "K-value", 
                               min = 3, max = 15, value = 5, step = 1),
                   checkboxInput("show_tab3_plot5", "Show True Model vs. Prediction Graphs", value = FALSE),
                   fluidRow(
                     column(3, plotOutput("tab3_plot1")),
                     column(3, plotOutput("tab3_plot2")),
                     column(3, plotOutput("tab3_plot3")),
                     column(3, plotOutput("tab3_plot4")))
                   ,
                   fluidRow(
                     column(3, conditionalPanel(
                       condition = "input.show_tab3_plot5 == true",
                       plotOutput("tab3_plot5")
                     )),
                     column(3, conditionalPanel(
                       condition = "input.show_tab3_plot5 == true",
                       plotOutput("tab3_plot6")
                     )),
                     column(3, conditionalPanel(
                       condition = "input.show_tab3_plot5 == true",
                       plotOutput("tab3_plot7")
                     )),
                     column(3, conditionalPanel(
                       condition = "input.show_tab3_plot5 == true",
                       plotOutput("tab3_plot8")
                     ))
                   )
          ),
          tabPanel("Regression Tree",
                   sliderInput("tree_size", "Tree Depth", 
                               min = 2, max = 7, value = 3, step = 1),
                   checkboxInput("show_tab4_plot5", "Show True Model vs. Prediction Graphs", value = FALSE),
                   fluidRow(
                     column(3, plotOutput("tab4_plot1")),
                     column(3, plotOutput("tab4_plot2")),
                     column(3, plotOutput("tab4_plot3")),
                     column(3, plotOutput("tab4_plot4")))
                   ,
                   fluidRow(
                     column(3, conditionalPanel(
                       condition = "input.show_tab4_plot5 == true",
                       plotOutput("tab4_plot5")
                     )),
                     column(3, conditionalPanel(
                       condition = "input.show_tab4_plot5 == true",
                       plotOutput("tab4_plot6")
                     )),
                     column(3, conditionalPanel(
                       condition = "input.show_tab4_plot5 == true",
                       plotOutput("tab4_plot7")
                     )),
                     column(3, conditionalPanel(
                       condition = "input.show_tab4_plot5 == true",
                       plotOutput("tab4_plot8")
                     ))
                   )
          )
          ,
          tabPanel("LASSO Regression",
                   
                   sliderInput("lambda", "Lambda", 
                               min = 0, max = 0.14, value = 0.04, step = 0.02),
                   checkboxInput("show_tab5_plot5", "Show True Model vs. Prediction Graphs", value = FALSE),
                   fluidRow(
                     column(3, plotOutput("tab5_plot1")),
                     column(3, plotOutput("tab5_plot2")),
                     column(3, plotOutput("tab5_plot3")),
                     column(3, plotOutput("tab5_plot4")))
                   ,
                   fluidRow(
                     column(3, conditionalPanel(
                       condition = "input.show_tab5_plot5 == true",
                       plotOutput("tab5_plot5")
                     )),
                     column(3, conditionalPanel(
                       condition = "input.show_tab5_plot5 == true",
                       plotOutput("tab5_plot6")
                     )),
                     column(3, conditionalPanel(
                       condition = "input.show_tab5_plot5 == true",
                       plotOutput("tab5_plot7")
                     )),
                     column(3, conditionalPanel(
                       condition = "input.show_tab5_plot5 == true",
                       plotOutput("tab5_plot8")
                     ))
                   )
          )
        )
      })
      # Reactive data generation
      df <- reactive({
        
        # Constants
        a <- 3
        b <- 0.87
        c <- 0.5
        
        # Functions to generate data
        generate_data <- function(x, fx, epsilon, num_responses = 20) {
          n <- length(x)
          responses <- data.frame(matrix(ncol = num_responses, nrow = n))
          colnames(responses) <- paste0("response", 1:num_responses)
          
          for (i in 1:num_responses) {
            e <- rnorm(n, mean = 0, sd = epsilon)
            responses[, i] <- fx + e
          }
          
          data.frame(inp = x, true_form = fx, responses)
        }
        
        generate_test_data <- function(x, fx, epsilon) {
          n <- length(x)
          e <- rnorm(n, mean = 0, sd = epsilon)
          y <- fx + e
          data.frame(inp = x, true_form = fx, observed = y)
        }
        
        toy_data <- NULL
        test_data <- NULL
        
        if (input$dataset == "Data set 1") {
          set.seed(2024)
          x <- runif(n = input$num_ob, min = -5, max = 15)
          fx <- ifelse(0.1*(x^2)<3, 0.1*(x^2), 3)
          toy_data <- generate_data(x, fx, input$epsilon)
          set.seed(2025)
          x_test <- runif(n = 1000, min = -5, max = 15)
          fx_test <- ifelse(0.1*(x_test^2)<3, 0.1*(x_test^2), 3)
          test_data <- generate_test_data(x_test, fx_test, input$epsilon)
        } else if (input$dataset == "Data set 2") {
          set.seed(2024)
          x <- runif(n = input$num_ob, min = 20, max = 40)
          fx <- a + (b * sqrt(x)) + (c * sin(x))
          toy_data <- generate_data(x, fx, input$epsilon)
          set.seed(2025)
          x_test <- runif(n = 1000, min = 20, max = 40)
          fx_test <- a + (b * sqrt(x_test)) + (c * sin(x_test))
          test_data <- generate_test_data(x_test, fx_test, input$epsilon)
        } else if (input$dataset == "Data set 3") {
          set.seed(2024)
          x <- runif(n = input$num_ob, min = -5, max = 5)
          fx <- a + (b * x^2) + (c * x^3)
          toy_data <- generate_data(x, fx, input$epsilon)
          set.seed(2025)
          x_test <- runif(n = 1000, min = -5, max = 5)
          fx_test <- a + (b * x_test^2) + (c * x_test^3)
          test_data <- generate_test_data(x_test, fx_test, input$epsilon)
        }
        
        return(list(toy_data = toy_data, test_data = test_data))
      })
      
      # Reactive model type based on active tab
      model_type <- reactive({
        switch(input$tabs,
               "Non-Linear" = "Non-Linear",
               "Polynomial" = "Polynomial",
               "KNN" = "KNN",
               "Regression Tree" = "Regression Tree",
               "LASSO Regression" = "LASSO Regression")  
      })
      
      # Logic for Regression server
      output$epsilon_slider <- renderUI({
        if (input$dataset == "Data set 2") {
          sliderInput("epsilon", "Variability", 
                      min = 0, max = 1, value = 0.3, step = 0.1)
        } else if (input$dataset == "Data set 3") {
          sliderInput("epsilon", "Variability", 
                      min = 3, max = 10, value = 8, step = 1)
        } else {
          sliderInput("epsilon", "Variability", 
                      min = 0, max = 0.6, value = 0.2, step = 0.1)
        }
      })
      
      # Add Regression server logic here
      # ...
      
      # Non-Linear Tab Plots
      output$tab1_plot1 <- renderPlot({
        df_data <- df()$toy_data
        
        
        
        p <- ggplot(data = df_data, aes(x = inp, y = response1)) + 
          geom_point() +
          labs(title = "Training Data", y = "y", x = "x")
        
        if (input$dataset == "Data set 1") {
          p <- p + scale_y_continuous(limits = c(0, 5)) +
            scale_x_continuous(limits = c(-5, 15))
        } else if (input$dataset == "Data set 2") {
          p <- p + scale_y_continuous(limits = c(3, 13)) +
            scale_x_continuous(limits = c(20, 40))
        } else if (input$dataset == "Data set 3") {
          p <- p + scale_y_continuous(limits = c(-30, 30)) +
            scale_x_continuous(limits = c(-6, 6))
        }
        
        # Use the reactive model_type
        if (model_type() == "Non-Linear") {
          p <- p + geom_smooth(span = 1/input$flexibility, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE)
        } else if (model_type() == "Polynomial") {
          p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, input$degree), se = FALSE, aes(color = "polynomial model"), show.legend = FALSE)
        } else if (model_type() == "KNN") {
          if (!is.null(input$k_value) && input$k_value > 0) {
            knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_data$inp), y = df_data$response1, k = input$k_value)
            df_knn <- data.frame(inp = df_data$inp, response1 = knn_fit$pred)
            p <- p + geom_step(data = df_knn, aes(x = inp, y = response1), size = 1)
          } else {
            p <- p + ggtitle("Invalid KNN Parameters")
          }
        } 
        
        print(p)
      })
      
      
      output$tab1_plot2 <- renderPlot({
        
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        
        
        # Initialize a data frame to store bias and variance for different complexities/k values
        metrics <- data.frame(
          Complexity = numeric(),
          Metric = character(),
          Value = numeric()
        )
        
        # Define the range of complexities or k values
        if (model_type() == "Non-Linear") {
          complexities <- seq(1, 5, by = 1)
        } else if (model_type() == "Polynomial") {
          complexities <- 1:5
        } else if (model_type() == "KNN") {
          complexities <- 3:15  # Use k values for KNN
        } else {
          complexities <- numeric()  # No complexities for unknown model types
        }
        
        # Loop over each complexity/k value
        for (complexity in complexities) {
          # Store predictions for each complexity/k value
          predictions <- matrix(nrow = nrow(df_testdata), ncol = 20)
          
          # Fit models and obtain predictions
          if (model_type() == "Non-Linear") {
            for (i in 1:20) {
              model <- loess(df_data[, i + 2] ~ inp, span = 1/complexity, data = df_data)
              predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
            }
          } else if (model_type() == "Polynomial") {
            for (i in 1:20) {
              model <- lm(df_data[, i + 2] ~ poly(inp, complexity), data = df_data)
              predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
            }
          } else if (model_type() == "KNN") {
            for (i in 1:20) {
              knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[, i + 2], k = complexity)
              predictions[, i] <- knn_fit$pred
            }
          }
          
          # Calculate average predictions
          avg_predictions <- rowMeans(predictions, na.rm = TRUE)
          
          # Ensure true_form is correctly aligned with the predictions
          true_form <- df_testdata$true_form
          
          # Calculate bias
          bias <- avg_predictions - true_form
          
          # Calculate squared bias
          squared_bias <- bias^2
          
          # Calculate overall squared bias
          overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
          
          # Calculate variances of predictions
          prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
          
          # Calculate overall variance
          overall_variance <- mean(prediction_vars, na.rm = TRUE)
          
          # Store metrics for current complexity/k value
          metrics <- rbind(
            metrics,
            data.frame(Complexity = complexity, Metric = "Bias", Value = overall_squared_bias),
            data.frame(Complexity = complexity, Metric = "Variance", Value = overall_variance)
          )
        }
        
        # Plot Bias
        plot_bias <- ggplot(metrics[metrics$Metric == "Bias", ], aes(x = as.factor(Complexity), y = Value)) +
          geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = 0.8, fill = "blue") +
          labs(
            title = "Bias",
            x = ifelse(model_type() == "KNN", "k Value", "Complexity"),  # Adjust x-axis label
            y = "Bias"  # Provide a label for y-axis
          ) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "none")  # Remove legend
        
        print(plot_bias)
      })
      
      
      output$tab1_plot3 <- renderPlot({
        
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        
        
        # Initialize a data frame to store bias and variance for different complexities/k values
        metrics <- data.frame(
          Complexity = numeric(),
          Metric = character(),
          Value = numeric()
        )
        
        # Define the range of complexities or k values
        if (model_type() == "Linear") {
          complexities <- 1  # Linear model has no complexity parameter
        } else if (model_type() == "Non-Linear") {
          complexities <- seq(1, 5, by = 1)
        } else if (model_type() == "Polynomial") {
          complexities <- 1:5
        } else if (model_type() == "KNN") {
          complexities <- 3:15  # Use k values for KNN
        } else {
          complexities <- numeric()  # No complexities for unknown model types
        }
        
        # Loop over each complexity/k value
        for (complexity in complexities) {
          # Store predictions for each complexity/k value
          predictions <- matrix(nrow = nrow(df_testdata), ncol = 20)
          
          # Fit models and obtain predictions
          if (model_type() == "Linear") {
            for (i in 1:20) {
              model <- lm(df_data[, i + 2] ~ inp, data = df_data)
              predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
            }
          } else if (model_type() == "Non-Linear") {
            for (i in 1:20) {
              model <- loess(df_data[, i + 2] ~ inp, span = 1/complexity, data = df_data)
              predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
            }
          } else if (model_type() == "Polynomial") {
            for (i in 1:20) {
              model <- lm(df_data[, i + 2] ~ poly(inp, complexity), data = df_data)
              predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
            }
          } else if (model_type() == "KNN") {
            for (i in 1:20) {
              knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[, i + 2], k = complexity)
              predictions[, i] <- knn_fit$pred
            }
          }
          
          # Calculate average predictions
          avg_predictions <- rowMeans(predictions, na.rm = TRUE)
          
          # Ensure true_form is correctly aligned with the predictions
          true_form <- df_testdata$true_form
          
          # Calculate bias
          bias <- avg_predictions - true_form
          
          # Calculate squared bias
          squared_bias <- bias^2
          
          # Calculate overall squared bias
          overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
          
          # Calculate variances of predictions
          prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
          
          # Calculate overall variance
          overall_variance <- mean(prediction_vars, na.rm = TRUE)
          
          # Store metrics for current complexity/k value
          metrics <- rbind(
            metrics,
            data.frame(Complexity = complexity, Metric = "Bias", Value = overall_squared_bias),
            data.frame(Complexity = complexity, Metric = "Variance", Value = overall_variance)
          )
        }
        
        # Plot Variance
        plot_variance <- ggplot(metrics[metrics$Metric == "Variance", ], aes(x = as.factor(Complexity), y = Value)) +
          geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = 0.8, fill = "red") +
          labs(
            title = "Variance",
            x = ifelse(model_type() == "KNN", "k Value", "Complexity"),  # Adjust x-axis label
            y = "Variance"  # Provide a label for y-axis
          ) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "none")  # Remove legend
        
        print(plot_variance)
      })
      
      
      output$tab1_plot4 <- renderPlot({
        
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        # Initialize a data frame to store bias and variance for different complexities/k values
        metrics <- data.frame(
          Complexity = numeric(),
          Metric = character(),
          Value = numeric()
        )
        
        # Define complexities or k values
        complexities <- switch(model_type(),  # Use model_type() instead of input$model_name
                               "Linear" = 1,  # Linear model has no complexity parameter
                               "Non-Linear" = seq(1, 5, by = 1),
                               "Polynomial" = 1:5,
                               "KNN" = 3:15)  # Add k values for KNN
        
        for (complexity in complexities) {
          predictions <- matrix(nrow = 1000, ncol = 20)
          training_mse_list <- numeric(20)
          
          for (i in 1:20) {
            response_col <- paste0("response", i)
            
            if (model_type() == "Linear") {
              model <- lm(df_data[[response_col]] ~ inp, data = df_data)
            } else if (model_type() == "Non-Linear") {
              model <- loess(df_data[[response_col]] ~ inp, span = 1/complexity, data = df_data)
            } else if (model_type() == "Polynomial") {
              model <- lm(df_data[[response_col]] ~ poly(inp, complexity), data = df_data)
            } else if (model_type() == "KNN") {
              knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[[response_col]], k = complexity)
              predictions[, i] <- knn_fit$pred
              training_preds <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_data$inp), y = df_data[[response_col]], k = complexity)$pred
              training_mse_list[i] <- mean((df_data[[response_col]] - training_preds)^2)
              next
            }
            
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
            training_preds <- predict(model, newdata = data.frame(inp = df_data$inp))
            training_mse_list[i] <- mean((df_data[[response_col]] - training_preds)^2)
          }
          
          avg_predictions <- rowMeans(predictions, na.rm = TRUE)
          true_form <- df_testdata$true_form[1:1000]
          bias <- avg_predictions - true_form
          squared_bias <- bias^2
          overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
          prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
          overall_variance <- mean(prediction_vars, na.rm = TRUE)
          test_MSE <- overall_variance + overall_squared_bias + (input$epsilon)^2
          training_MSE <- mean(training_mse_list, na.rm = TRUE)
          
          metrics <- rbind(
            metrics,
            data.frame(Complexity = complexity, Metric = "test MSE", Value = test_MSE),
            data.frame(Complexity = complexity, Metric = "training MSE", Value = training_MSE)
          )
        }
        
        # Plot training and test MSE
        plot_mse <- ggplot(metrics, aes(x = as.factor(Complexity), y = Value, color = Metric, group = Metric)) +
          geom_line(size = 1) +
          geom_point(size = 2) +
          labs(
            title = "Training MSE vs. Test MSE",
            x = ifelse(model_type() == "KNN", "k Value", "Complexity"),  # Adjust x-axis label
            y = "Mean Squared Error"
          ) +
          theme_minimal() +
          scale_color_manual(values = c("test MSE" = "purple", "training MSE" = "green")) +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top"  # Move legend to the top
          )
        
        print(plot_mse)
      })
      
      output$tab1_plot5 <- renderPlot({
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        # Get complexity from input$degree
        complexity <- 2
        
        # Create a data frame to store true values and predictions
        plot_data <- data.frame(
          inp = rep(df_testdata$inp, 9),  # True form + 7 models + mean model
          Value = c(df_testdata$true_form, rep(NA, 8 * nrow(df_testdata))),
          Type = rep(c("True", rep("Model", 7), "Mean_Model"), each = nrow(df_testdata))
        )
        
        # Initialize a matrix to store predictions
        predictions <- matrix(nrow = nrow(df_testdata), ncol = 7)
        
        for (i in 1:7) {
          response_col <- paste0("response", i)
          
          if (model_type() == "Linear") {
            model <- lm(df_data[[response_col]] ~ inp, data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "Non-Linear") {
            model <- loess(df_data[[response_col]] ~ inp, span = 1/complexity, data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "Polynomial") {
            model <- lm(df_data[[response_col]] ~ poly(inp, complexity), data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "KNN") {
            knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[[response_col]], k = complexity)
            predictions[, i] <- knn_fit$pred
          }
        }
        
        # Convert predictions to long format for plotting
        predictions_long <- data.frame(
          inp = rep(df_testdata$inp, 7),
          Value = as.vector(predictions[, 1:7]),
          Model = rep(paste0("Model", 1:7), each = nrow(df_testdata))
        )
        
        # Calculate the mean of the predictions for each point
        mean_predictions <- rowMeans(predictions[, 1:7])
        
        # Update plot_data with mean predictions
        plot_data$Value[plot_data$Type == "Mean_Model"] <- mean_predictions
        
        # Plot true form, predictions for the first 7 models, and the mean model
        plot_predictions <- ggplot() +
          geom_line(data = plot_data[plot_data$Type == "True", ], aes(x = inp, y = Value, color = "True Form"), size = 1) +
          geom_line(data = predictions_long, aes(x = inp, y = Value, group = Model, color = "Predictions"), size = 0.5) +
          geom_line(data = plot_data[plot_data$Type == "Mean_Model", ], aes(x = inp, y = Value, color = "Average Prediction"), size = 1.5) +
          scale_color_manual(name = NULL, values = c("True Form" = "black", "Predictions" = "pink", "Average Prediction" = "orange")) +
          labs(
            title = "Flexibility = 2",
            x = "x",
            y = "y"
          ) +
          theme_minimal() +
          theme(
            legend.position = "top",  # Position the legend to the top
            legend.title = element_blank(),  # Remove the legend title
            legend.text = element_text(size = 8),  # Make legend text smaller
            legend.key.size = unit(0.5, "lines")  # Make legend keys smaller
          )
        
        print(plot_predictions)
      })
      
      
      output$tab1_plot6 <- renderPlot({
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        # Get complexity from input$degree
        complexity <- 3
        
        # Create a data frame to store true values and predictions
        plot_data <- data.frame(
          inp = rep(df_testdata$inp, 9),  # True form + 7 models + mean model
          Value = c(df_testdata$true_form, rep(NA, 8 * nrow(df_testdata))),
          Type = rep(c("True", rep("Model", 7), "Mean_Model"), each = nrow(df_testdata))
        )
        
        # Initialize a matrix to store predictions
        predictions <- matrix(nrow = nrow(df_testdata), ncol = 7)
        
        for (i in 1:7) {
          response_col <- paste0("response", i)
          
          if (model_type() == "Linear") {
            model <- lm(df_data[[response_col]] ~ inp, data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "Non-Linear") {
            model <- loess(df_data[[response_col]] ~ inp, span = 1/complexity, data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "Polynomial") {
            model <- lm(df_data[[response_col]] ~ poly(inp, complexity), data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "KNN") {
            knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[[response_col]], k = complexity)
            predictions[, i] <- knn_fit$pred
          }
        }
        
        # Convert predictions to long format for plotting
        predictions_long <- data.frame(
          inp = rep(df_testdata$inp, 7),
          Value = as.vector(predictions[, 1:7]),
          Model = rep(paste0("Model", 1:7), each = nrow(df_testdata))
        )
        
        # Calculate the mean of the predictions for each point
        mean_predictions <- rowMeans(predictions[, 1:7])
        
        # Update plot_data with mean predictions
        plot_data$Value[plot_data$Type == "Mean_Model"] <- mean_predictions
        
        # Plot true form, predictions for the first 7 models, and the mean model
        plot_predictions <- ggplot() +
          geom_line(data = plot_data[plot_data$Type == "True", ], aes(x = inp, y = Value, color = "True Form"), size = 1) +
          geom_line(data = predictions_long, aes(x = inp, y = Value, group = Model, color = "Predictions"), size = 0.5) +
          geom_line(data = plot_data[plot_data$Type == "Mean_Model", ], aes(x = inp, y = Value, color = "Average Prediction"), size = 1.5) +
          scale_color_manual(name = NULL, values = c("True Form" = "black", "Predictions" = "pink", "Average Prediction" = "orange")) +
          labs(
            title = "Flexibility = 3",
            x = "x",
            y = "y"
          ) +
          theme_minimal() +
          theme(
            legend.position = "top",  # Position the legend to the top
            legend.title = element_blank(),  # Remove the legend title
            legend.text = element_text(size = 8),  # Make legend text smaller
            legend.key.size = unit(0.5, "lines")  # Make legend keys smaller
          )
        
        print(plot_predictions)
      })
      
      
      output$tab1_plot7 <- renderPlot({
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        # Get complexity from input$degree
        complexity <- 4
        
        # Create a data frame to store true values and predictions
        plot_data <- data.frame(
          inp = rep(df_testdata$inp, 9),  # True form + 7 models + mean model
          Value = c(df_testdata$true_form, rep(NA, 8 * nrow(df_testdata))),
          Type = rep(c("True", rep("Model", 7), "Mean_Model"), each = nrow(df_testdata))
        )
        
        # Initialize a matrix to store predictions
        predictions <- matrix(nrow = nrow(df_testdata), ncol = 7)
        
        for (i in 1:7) {
          response_col <- paste0("response", i)
          
          if (model_type() == "Linear") {
            model <- lm(df_data[[response_col]] ~ inp, data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "Non-Linear") {
            model <- loess(df_data[[response_col]] ~ inp, span = 1/complexity, data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "Polynomial") {
            model <- lm(df_data[[response_col]] ~ poly(inp, complexity), data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "KNN") {
            knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[[response_col]], k = complexity)
            predictions[, i] <- knn_fit$pred
          }
        }
        
        # Convert predictions to long format for plotting
        predictions_long <- data.frame(
          inp = rep(df_testdata$inp, 7),
          Value = as.vector(predictions[, 1:7]),
          Model = rep(paste0("Model", 1:7), each = nrow(df_testdata))
        )
        
        # Calculate the mean of the predictions for each point
        mean_predictions <- rowMeans(predictions[, 1:7])
        
        # Update plot_data with mean predictions
        plot_data$Value[plot_data$Type == "Mean_Model"] <- mean_predictions
        
        # Plot true form, predictions for the first 7 models, and the mean model
        plot_predictions <- ggplot() +
          geom_line(data = plot_data[plot_data$Type == "True", ], aes(x = inp, y = Value, color = "True Form"), size = 1) +
          geom_line(data = predictions_long, aes(x = inp, y = Value, group = Model, color = "Predictions"), size = 0.5) +
          geom_line(data = plot_data[plot_data$Type == "Mean_Model", ], aes(x = inp, y = Value, color = "Average Prediction"), size = 1.5) +
          scale_color_manual(name = NULL, values = c("True Form" = "black", "Predictions" = "pink", "Average Prediction" = "orange")) +
          labs(
            title = "Flexibility = 4",
            x = "x",
            y = "y"
          ) +
          theme_minimal() +
          theme(
            legend.position = "top",  # Position the legend to the top
            legend.title = element_blank(),  # Remove the legend title
            legend.text = element_text(size = 8),  # Make legend text smaller
            legend.key.size = unit(0.5, "lines")  # Make legend keys smaller
          )
        
        print(plot_predictions)
      })
      
      
      
      output$tab1_plot8 <- renderPlot({
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        # Get complexity from input$degree
        complexity <- 5
        
        # Create a data frame to store true values and predictions
        plot_data <- data.frame(
          inp = rep(df_testdata$inp, 9),  # True form + 7 models + mean model
          Value = c(df_testdata$true_form, rep(NA, 8 * nrow(df_testdata))),
          Type = rep(c("True", rep("Model", 7), "Mean_Model"), each = nrow(df_testdata))
        )
        
        # Initialize a matrix to store predictions
        predictions <- matrix(nrow = nrow(df_testdata), ncol = 7)
        
        for (i in 1:7) {
          response_col <- paste0("response", i)
          
          if (model_type() == "Linear") {
            model <- lm(df_data[[response_col]] ~ inp, data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "Non-Linear") {
            model <- loess(df_data[[response_col]] ~ inp, span = 1/complexity, data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "Polynomial") {
            model <- lm(df_data[[response_col]] ~ poly(inp, complexity), data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "KNN") {
            knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[[response_col]], k = complexity)
            predictions[, i] <- knn_fit$pred
          }
        }
        
        # Convert predictions to long format for plotting
        predictions_long <- data.frame(
          inp = rep(df_testdata$inp, 7),
          Value = as.vector(predictions[, 1:7]),
          Model = rep(paste0("Model", 1:7), each = nrow(df_testdata))
        )
        
        # Calculate the mean of the predictions for each point
        mean_predictions <- rowMeans(predictions[, 1:7])
        
        # Update plot_data with mean predictions
        plot_data$Value[plot_data$Type == "Mean_Model"] <- mean_predictions
        
        # Plot true form, predictions for the first 7 models, and the mean model
        plot_predictions <- ggplot() +
          geom_line(data = plot_data[plot_data$Type == "True", ], aes(x = inp, y = Value, color = "True Form"), size = 1) +
          geom_line(data = predictions_long, aes(x = inp, y = Value, group = Model, color = "Predictions"), size = 0.5) +
          geom_line(data = plot_data[plot_data$Type == "Mean_Model", ], aes(x = inp, y = Value, color = "Average Prediction"), size = 1.5) +
          scale_color_manual(name = NULL, values = c("True Form" = "black", "Predictions" = "pink", "Average Prediction" = "orange")) +
          labs(
            title = "Flexibility = 5",
            x = "x",
            y = "y"
          ) +
          theme_minimal() +
          theme(
            legend.position = "top",  # Position the legend to the top
            legend.title = element_blank(),  # Remove the legend title
            legend.text = element_text(size = 8),  # Make legend text smaller
            legend.key.size = unit(0.5, "lines")  # Make legend keys smaller
          )
        
        print(plot_predictions)
      })
      
      
      ###################
      output$tab2_plot1 <- renderPlot({
        df_data <- df()$toy_data
        
        
        
        p <- ggplot(data = df_data, aes(x = inp, y = response1)) + 
          geom_point() +
          labs(title = "Training Data", y = "y", x = "x")
        
        if (input$dataset == "Data set 1") {
          p <- p + scale_y_continuous(limits = c(0, 5)) +
            scale_x_continuous(limits = c(-5, 10))
        } else if (input$dataset == "Data set 2") {
          p <- p + scale_y_continuous(limits = c(3, 13)) +
            scale_x_continuous(limits = c(20, 40))
        } else if (input$dataset == "Data set 3") {
          p <- p + scale_y_continuous(limits = c(-30, 30)) +
            scale_x_continuous(limits = c(-6, 6))
        }
        
        # Use the reactive model_type
        if (model_type() == "Non-Linear") {
          p <- p + geom_smooth(span = 1/input$flexibility, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE)
        } else if (model_type() == "Polynomial") {
          p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, input$degree), se = FALSE, aes(color = "polynomial model"), show.legend = FALSE)
        } else if (model_type() == "KNN") {
          if (!is.null(input$k_value) && input$k_value > 0) {
            knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_data$inp), y = df_data$response1, k = input$k_value)
            df_knn <- data.frame(inp = df_data$inp, response1 = knn_fit$pred)
            p <- p + geom_step(data = df_knn, aes(x = inp, y = response1), size = 1)
          } else {
            p <- p + ggtitle("Invalid KNN Parameters")
          }
        } else {
          p <- p + ggtitle("Invalid Model Name")
        }
        
        print(p)
      })
      
      
      output$tab2_plot2 <- renderPlot({
        
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        
        
        # Initialize a data frame to store bias and variance for different complexities/k values
        metrics <- data.frame(
          Complexity = numeric(),
          Metric = character(),
          Value = numeric()
        )
        
        # Define the range of complexities or k values
        if (model_type() == "Non-Linear") {
          complexities <- seq(1, 5, by = 1)
        } else if (model_type() == "Polynomial") {
          complexities <- 1:5
        } else if (model_type() == "KNN") {
          complexities <- 3:15  # Use k values for KNN
        } else {
          complexities <- numeric()  # No complexities for unknown model types
        }
        
        # Loop over each complexity/k value
        for (complexity in complexities) {
          # Store predictions for each complexity/k value
          predictions <- matrix(nrow = nrow(df_testdata), ncol = 20)
          
          # Fit models and obtain predictions
          if (model_type() == "Non-Linear") {
            for (i in 1:20) {
              model <- loess(df_data[, i + 2] ~ inp, span = 1/complexity, data = df_data)
              predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
            }
          } else if (model_type() == "Polynomial") {
            for (i in 1:20) {
              model <- lm(df_data[, i + 2] ~ poly(inp, complexity), data = df_data)
              predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
            }
          } else if (model_type() == "KNN") {
            for (i in 1:20) {
              knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[, i + 2], k = complexity)
              predictions[, i] <- knn_fit$pred
            }
          }
          
          # Calculate average predictions
          avg_predictions <- rowMeans(predictions, na.rm = TRUE)
          
          # Ensure true_form is correctly aligned with the predictions
          true_form <- df_testdata$true_form
          
          # Calculate bias
          bias <- avg_predictions - true_form
          
          # Calculate squared bias
          squared_bias <- bias^2
          
          # Calculate overall squared bias
          overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
          
          # Calculate variances of predictions
          prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
          
          # Calculate overall variance
          overall_variance <- mean(prediction_vars, na.rm = TRUE)
          
          # Store metrics for current complexity/k value
          metrics <- rbind(
            metrics,
            data.frame(Complexity = complexity, Metric = "Bias", Value = overall_squared_bias),
            data.frame(Complexity = complexity, Metric = "Variance", Value = overall_variance)
          )
        }
        
        # Plot Bias
        plot_bias <- ggplot(metrics[metrics$Metric == "Bias", ], aes(x = as.factor(Complexity), y = Value)) +
          geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = 0.8, fill = "blue") +
          labs(
            title = "Bias",
            x = ifelse(model_type() == "KNN", "k Value", "Complexity"),  # Adjust x-axis label
            y = "Bias"  # Provide a label for y-axis
          ) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "none")  # Remove legend
        
        print(plot_bias)
      })
      
      
      output$tab2_plot3 <- renderPlot({
        
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        
        
        # Initialize a data frame to store bias and variance for different complexities/k values
        metrics <- data.frame(
          Complexity = numeric(),
          Metric = character(),
          Value = numeric()
        )
        
        # Define the range of complexities or k values
        if (model_type() == "Linear") {
          complexities <- 1  # Linear model has no complexity parameter
        } else if (model_type() == "Non-Linear") {
          complexities <- seq(0.1, 5, by = 1)
        } else if (model_type() == "Polynomial") {
          complexities <- 1:5
        } else if (model_type() == "KNN") {
          complexities <- 3:15  # Use k values for KNN
        } else {
          complexities <- numeric()  # No complexities for unknown model types
        }
        
        # Loop over each complexity/k value
        for (complexity in complexities) {
          # Store predictions for each complexity/k value
          predictions <- matrix(nrow = nrow(df_testdata), ncol = 20)
          
          # Fit models and obtain predictions
          if (model_type() == "Linear") {
            for (i in 1:20) {
              model <- lm(df_data[, i + 2] ~ inp, data = df_data)
              predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
            }
          } else if (model_type() == "Non-Linear") {
            for (i in 1:20) {
              model <- loess(df_data[, i + 2] ~ inp, span = 1/complexity, data = df_data)
              predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
            }
          } else if (model_type() == "Polynomial") {
            for (i in 1:20) {
              model <- lm(df_data[, i + 2] ~ poly(inp, complexity), data = df_data)
              predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
            }
          } else if (model_type() == "KNN") {
            for (i in 1:20) {
              knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[, i + 2], k = complexity)
              predictions[, i] <- knn_fit$pred
            }
          }
          
          # Calculate average predictions
          avg_predictions <- rowMeans(predictions, na.rm = TRUE)
          
          # Ensure true_form is correctly aligned with the predictions
          true_form <- df_testdata$true_form
          
          # Calculate bias
          bias <- avg_predictions - true_form
          
          # Calculate squared bias
          squared_bias <- bias^2
          
          # Calculate overall squared bias
          overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
          
          # Calculate variances of predictions
          prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
          
          # Calculate overall variance
          overall_variance <- mean(prediction_vars, na.rm = TRUE)
          
          # Store metrics for current complexity/k value
          metrics <- rbind(
            metrics,
            data.frame(Complexity = complexity, Metric = "Bias", Value = overall_squared_bias),
            data.frame(Complexity = complexity, Metric = "Variance", Value = overall_variance)
          )
        }
        
        # Plot Variance
        plot_variance <- ggplot(metrics[metrics$Metric == "Variance", ], aes(x = as.factor(Complexity), y = Value)) +
          geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = 0.8, fill = "red") +
          labs(
            title = "Variance",
            x = ifelse(model_type() == "KNN", "k Value", "Complexity"),  # Adjust x-axis label
            y = "Variance"  # Provide a label for y-axis
          ) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "none")  # Remove legend
        
        print(plot_variance)
      })
      
      
      output$tab2_plot4 <-renderPlot({
        
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        # Initialize a data frame to store bias and variance for different complexities/k values
        metrics <- data.frame(
          Complexity = numeric(),
          Metric = character(),
          Value = numeric()
        )
        
        # Define complexities or k values
        complexities <- switch(model_type(),  # Use model_type() instead of input$model_name
                               "Linear" = 1,  # Linear model has no complexity parameter
                               "Non-Linear" = seq(0.1, 5, by = 1),
                               "Polynomial" = 1:5,
                               "KNN" = 3:15)  # Add k values for KNN
        
        for (complexity in complexities) {
          predictions <- matrix(nrow = 1000, ncol = 20)
          training_mse_list <- numeric(20)
          
          for (i in 1:20) {
            response_col <- paste0("response", i)
            
            if (model_type() == "Linear") {
              model <- lm(df_data[[response_col]] ~ inp, data = df_data)
            } else if (model_type() == "Non-Linear") {
              model <- loess(df_data[[response_col]] ~ inp, span = 1/complexity, data = df_data)
            } else if (model_type() == "Polynomial") {
              model <- lm(df_data[[response_col]] ~ poly(inp, complexity), data = df_data)
            } else if (model_type() == "KNN") {
              knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[[response_col]], k = complexity)
              predictions[, i] <- knn_fit$pred
              training_preds <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_data$inp), y = df_data[[response_col]], k = complexity)$pred
              training_mse_list[i] <- mean((df_data[[response_col]] - training_preds)^2)
              next
            }
            
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
            training_preds <- predict(model, newdata = data.frame(inp = df_data$inp))
            training_mse_list[i] <- mean((df_data[[response_col]] - training_preds)^2)
          }
          
          avg_predictions <- rowMeans(predictions, na.rm = TRUE)
          true_form <- df_testdata$true_form[1:1000]
          bias <- avg_predictions - true_form
          squared_bias <- bias^2
          overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
          prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
          overall_variance <- mean(prediction_vars, na.rm = TRUE)
          test_MSE <- overall_variance + overall_squared_bias + (input$epsilon)^2
          training_MSE <- mean(training_mse_list, na.rm = TRUE)
          
          metrics <- rbind(
            metrics,
            data.frame(Complexity = complexity, Metric = "test MSE", Value = test_MSE),
            data.frame(Complexity = complexity, Metric = "training MSE", Value = training_MSE)
          )
        }
        
        # Plot training and test MSE
        plot_mse <- ggplot(metrics, aes(x = as.factor(Complexity), y = Value, color = Metric, group = Metric)) +
          geom_line(size = 1) +
          geom_point(size = 2) +
          labs(
            title = "Training MSE vs. Test MSE",
            x = ifelse(model_type() == "KNN", "k Value", "Complexity"),  # Adjust x-axis label
            y = "Mean Squared Error"
          ) +
          theme_minimal() +
          scale_color_manual(values = c("test MSE" = "purple", "training MSE" = "green")) +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top"  # Move legend to the top
          )
        
        print(plot_mse)
      })
      
      output$tab2_plot5 <- renderPlot({
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        # Get complexity from input$degree
        complexity <- 1
        
        # Create a data frame to store true values and predictions
        plot_data <- data.frame(
          inp = rep(df_testdata$inp, 9),  # True form + 7 models + mean model
          Value = c(df_testdata$true_form, rep(NA, 8 * nrow(df_testdata))),
          Type = rep(c("True", rep("Model", 7), "Mean_Model"), each = nrow(df_testdata))
        )
        
        # Initialize a matrix to store predictions
        predictions <- matrix(nrow = nrow(df_testdata), ncol = 7)
        
        for (i in 1:7) {
          response_col <- paste0("response", i)
          
          if (model_type() == "Linear") {
            model <- lm(df_data[[response_col]] ~ inp, data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "Non-Linear") {
            model <- loess(df_data[[response_col]] ~ inp, span = 1/complexity, data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "Polynomial") {
            model <- lm(df_data[[response_col]] ~ poly(inp, complexity), data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "KNN") {
            knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[[response_col]], k = complexity)
            predictions[, i] <- knn_fit$pred
          }
        }
        
        # Convert predictions to long format for plotting
        predictions_long <- data.frame(
          inp = rep(df_testdata$inp, 7),
          Value = as.vector(predictions[, 1:7]),
          Model = rep(paste0("Model", 1:7), each = nrow(df_testdata))
        )
        
        # Calculate the mean of the predictions for each point
        mean_predictions <- rowMeans(predictions[, 1:7])
        
        # Update plot_data with mean predictions
        plot_data$Value[plot_data$Type == "Mean_Model"] <- mean_predictions
        
        # Plot true form, predictions for the first 7 models, and the mean model
        plot_predictions <- ggplot() +
          geom_line(data = plot_data[plot_data$Type == "True", ], aes(x = inp, y = Value, color = "True Form"), size = 1) +
          geom_line(data = predictions_long, aes(x = inp, y = Value, group = Model, color = "Predictions"), size = 0.5) +
          geom_line(data = plot_data[plot_data$Type == "Mean_Model", ], aes(x = inp, y = Value, color = "Average Prediction"), size = 1.5) +
          scale_color_manual(name = NULL, values = c("True Form" = "black", "Predictions" = "pink", "Average Prediction" = "orange")) +
          labs(
            title = "Degree = 1",
            x = "x",
            y = "y"
          ) +
          theme_minimal() +
          theme(
            legend.position = "top",  # Position the legend to the top
            legend.title = element_blank(),  # Remove the legend title
            legend.text = element_text(size = 8),  # Make legend text smaller
            legend.key.size = unit(0.5, "lines")  # Make legend keys smaller
          )
        
        print(plot_predictions)
      })
      
      
      output$tab2_plot6 <- renderPlot({
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        # Get complexity from input$degree
        complexity <- 3
        
        # Create a data frame to store true values and predictions
        plot_data <- data.frame(
          inp = rep(df_testdata$inp, 9),  # True form + 7 models + mean model
          Value = c(df_testdata$true_form, rep(NA, 8 * nrow(df_testdata))),
          Type = rep(c("True", rep("Model", 7), "Mean_Model"), each = nrow(df_testdata))
        )
        
        # Initialize a matrix to store predictions
        predictions <- matrix(nrow = nrow(df_testdata), ncol = 7)
        
        for (i in 1:7) {
          response_col <- paste0("response", i)
          
          if (model_type() == "Linear") {
            model <- lm(df_data[[response_col]] ~ inp, data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "Non-Linear") {
            model <- loess(df_data[[response_col]] ~ inp, span = 1/complexity, data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "Polynomial") {
            model <- lm(df_data[[response_col]] ~ poly(inp, complexity), data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "KNN") {
            knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[[response_col]], k = complexity)
            predictions[, i] <- knn_fit$pred
          }
        }
        
        # Convert predictions to long format for plotting
        predictions_long <- data.frame(
          inp = rep(df_testdata$inp, 7),
          Value = as.vector(predictions[, 1:7]),
          Model = rep(paste0("Model", 1:7), each = nrow(df_testdata))
        )
        
        # Calculate the mean of the predictions for each point
        mean_predictions <- rowMeans(predictions[, 1:7])
        
        # Update plot_data with mean predictions
        plot_data$Value[plot_data$Type == "Mean_Model"] <- mean_predictions
        
        # Plot true form, predictions for the first 7 models, and the mean model
        plot_predictions <- ggplot() +
          geom_line(data = plot_data[plot_data$Type == "True", ], aes(x = inp, y = Value, color = "True Form"), size = 1) +
          geom_line(data = predictions_long, aes(x = inp, y = Value, group = Model, color = "Predictions"), size = 0.5) +
          geom_line(data = plot_data[plot_data$Type == "Mean_Model", ], aes(x = inp, y = Value, color = "Average Prediction"), size = 1.5) +
          scale_color_manual(name = NULL, values = c("True Form" = "black", "Predictions" = "pink", "Average Prediction" = "orange")) +
          labs(
            title = "Degree = 3",
            x = "x",
            y = "y"
          ) +
          theme_minimal() +
          theme(
            legend.position = "top",  # Position the legend to the top
            legend.title = element_blank(),  # Remove the legend title
            legend.text = element_text(size = 8),  # Make legend text smaller
            legend.key.size = unit(0.5, "lines")  # Make legend keys smaller
          )
        
        print(plot_predictions)
      })
      
      
      output$tab2_plot7 <- renderPlot({
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        # Get complexity from input$degree
        complexity <- 3.5
        
        # Create a data frame to store true values and predictions
        plot_data <- data.frame(
          inp = rep(df_testdata$inp, 9),  # True form + 7 models + mean model
          Value = c(df_testdata$true_form, rep(NA, 8 * nrow(df_testdata))),
          Type = rep(c("True", rep("Model", 7), "Mean_Model"), each = nrow(df_testdata))
        )
        
        # Initialize a matrix to store predictions
        predictions <- matrix(nrow = nrow(df_testdata), ncol = 7)
        
        for (i in 1:7) {
          response_col <- paste0("response", i)
          
          if (model_type() == "Linear") {
            model <- lm(df_data[[response_col]] ~ inp, data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "Non-Linear") {
            model <- loess(df_data[[response_col]] ~ inp, span = 1/complexity, data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "Polynomial") {
            model <- lm(df_data[[response_col]] ~ poly(inp, complexity), data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "KNN") {
            knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[[response_col]], k = complexity)
            predictions[, i] <- knn_fit$pred
          }
        }
        
        # Convert predictions to long format for plotting
        predictions_long <- data.frame(
          inp = rep(df_testdata$inp, 7),
          Value = as.vector(predictions[, 1:7]),
          Model = rep(paste0("Model", 1:7), each = nrow(df_testdata))
        )
        
        # Calculate the mean of the predictions for each point
        mean_predictions <- rowMeans(predictions[, 1:7])
        
        # Update plot_data with mean predictions
        plot_data$Value[plot_data$Type == "Mean_Model"] <- mean_predictions
        
        # Plot true form, predictions for the first 7 models, and the mean model
        plot_predictions <- ggplot() +
          geom_line(data = plot_data[plot_data$Type == "True", ], aes(x = inp, y = Value, color = "True Form"), size = 1) +
          geom_line(data = predictions_long, aes(x = inp, y = Value, group = Model, color = "Predictions"), size = 0.5) +
          geom_line(data = plot_data[plot_data$Type == "Mean_Model", ], aes(x = inp, y = Value, color = "Average Prediction"), size = 1.5) +
          scale_color_manual(name = NULL, values = c("True Form" = "black", "Predictions" = "pink", "Average Prediction" = "orange")) +
          labs(
            title = "Degree = 3.5",
            x = "x",
            y = "y"
          ) +
          theme_minimal() +
          theme(
            legend.position = "top",  # Position the legend to the top
            legend.title = element_blank(),  # Remove the legend title
            legend.text = element_text(size = 8),  # Make legend text smaller
            legend.key.size = unit(0.5, "lines")  # Make legend keys smaller
          )
        
        print(plot_predictions)
      })
      
      output$tab2_plot8 <- renderPlot({
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        # Get complexity from input$degree
        complexity <- 5
        
        # Create a data frame to store true values and predictions
        plot_data <- data.frame(
          inp = rep(df_testdata$inp, 9),  # True form + 7 models + mean model
          Value = c(df_testdata$true_form, rep(NA, 8 * nrow(df_testdata))),
          Type = rep(c("True", rep("Model", 7), "Mean_Model"), each = nrow(df_testdata))
        )
        
        # Initialize a matrix to store predictions
        predictions <- matrix(nrow = nrow(df_testdata), ncol = 7)
        
        for (i in 1:7) {
          response_col <- paste0("response", i)
          
          if (model_type() == "Linear") {
            model <- lm(df_data[[response_col]] ~ inp, data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "Non-Linear") {
            model <- loess(df_data[[response_col]] ~ inp, span = 1/complexity, data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "Polynomial") {
            model <- lm(df_data[[response_col]] ~ poly(inp, complexity), data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "KNN") {
            knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[[response_col]], k = complexity)
            predictions[, i] <- knn_fit$pred
          }
        }
        
        # Convert predictions to long format for plotting
        predictions_long <- data.frame(
          inp = rep(df_testdata$inp, 7),
          Value = as.vector(predictions[, 1:7]),
          Model = rep(paste0("Model", 1:7), each = nrow(df_testdata))
        )
        
        # Calculate the mean of the predictions for each point
        mean_predictions <- rowMeans(predictions[, 1:7])
        
        # Update plot_data with mean predictions
        plot_data$Value[plot_data$Type == "Mean_Model"] <- mean_predictions
        
        # Plot true form, predictions for the first 7 models, and the mean model
        plot_predictions <- ggplot() +
          geom_line(data = plot_data[plot_data$Type == "True", ], aes(x = inp, y = Value, color = "True Form"), size = 1) +
          geom_line(data = predictions_long, aes(x = inp, y = Value, group = Model, color = "Predictions"), size = 0.5) +
          geom_line(data = plot_data[plot_data$Type == "Mean_Model", ], aes(x = inp, y = Value, color = "Average Prediction"), size = 1.5) +
          scale_color_manual(name = NULL, values = c("True Form" = "black", "Predictions" = "pink", "Average Prediction" = "orange")) +
          labs(
            title = "Degree = 5",
            x = "x",
            y = "y"
          ) +
          theme_minimal() +
          theme(
            legend.position = "top",  # Position the legend to the top
            legend.title = element_blank(),  # Remove the legend title
            legend.text = element_text(size = 8),  # Make legend text smaller
            legend.key.size = unit(0.5, "lines")  # Make legend keys smaller
          )
        
        print(plot_predictions)
      })
      
      
      
      
      
      ###################
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      output$tab3_plot1 <- renderPlot({
        df_data <- df()$toy_data
        
        
        
        p <- ggplot(data = df_data, aes(x = inp, y = response1)) + 
          geom_point() +
          labs(title = "Training Data", y = "y", x = "x")
        
        # Conditional limits based on the dataset
        if (input$dataset == "Data set 1") {
          p <- p + scale_y_continuous(limits = c(0, 5)) +
            scale_x_continuous(limits = c(5, 10))
        } else if (input$dataset == "Data set 2") {
          p <- p + scale_y_continuous(limits = c(3, 13)) +
            scale_x_continuous(limits = c(20, 40))
        } else if (input$dataset == "Data set 3") {
          p <- p + scale_y_continuous(limits = c(-30, 30)) +
            scale_x_continuous(limits = c(-6, 6))
        }
        
        # Use the reactive model_type
        if (model_type() == "Non-Linear") {
          p <- p + geom_smooth(span = 1/input$flexibility, se = FALSE, color = "red", show.legend = FALSE)
        } else if (model_type() == "Polynomial") {
          p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, input$degree), se = FALSE, color = "red", show.legend = FALSE)
        } else if (model_type() == "KNN") {
          if (!is.null(input$k_value) && input$k_value > 0) {
            knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_data$inp), y = df_data$response1, k = input$k_value)
            df_knn <- data.frame(inp = df_data$inp, response1 = knn_fit$pred)
            p <- p + geom_step(data = df_knn, aes(x = inp, y = response1), size = 1, color = "red")
          } else {
            p <- p + ggtitle("Invalid KNN Parameters")
          }
        } 
        print(p)
      })
      
      
      output$tab3_plot2 <- renderPlot({
        
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        
        
        # Initialize a data frame to store bias and variance for different complexities/k values
        metrics <- data.frame(
          Complexity = numeric(),
          Metric = character(),
          Value = numeric()
        )
        
        # Define the range of complexities or k values
        if (model_type() == "Non-Linear") {
          complexities <- seq(0.1, 5, by = 1)
        } else if (model_type() == "Polynomial") {
          complexities <- 1:5
        } else if (model_type() == "KNN") {
          complexities <- 3:15  # Use k values for KNN
        } else {
          complexities <- numeric()  # No complexities for unknown model types
        }
        
        # Loop over each complexity/k value
        for (complexity in complexities) {
          # Store predictions for each complexity/k value
          predictions <- matrix(nrow = nrow(df_testdata), ncol = 20)
          
          # Fit models and obtain predictions
          if (model_type() == "Non-Linear") {
            for (i in 1:20) {
              model <- loess(df_data[, i + 2] ~ inp, span = 1/complexity, data = df_data)
              predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
            }
          } else if (model_type() == "Polynomial") {
            for (i in 1:20) {
              model <- lm(df_data[, i + 2] ~ poly(inp, complexity), data = df_data)
              predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
            }
          } else if (model_type() == "KNN") {
            for (i in 1:20) {
              knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[, i + 2], k = complexity)
              predictions[, i] <- knn_fit$pred
            }
          }
          
          # Calculate average predictions
          avg_predictions <- rowMeans(predictions, na.rm = TRUE)
          
          # Ensure true_form is correctly aligned with the predictions
          true_form <- df_testdata$true_form
          
          # Calculate bias
          bias <- avg_predictions - true_form
          
          # Calculate squared bias
          squared_bias <- bias^2
          
          # Calculate overall squared bias
          overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
          
          # Calculate variances of predictions
          prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
          
          # Calculate overall variance
          overall_variance <- mean(prediction_vars, na.rm = TRUE)
          
          # Store metrics for current complexity/k value
          metrics <- rbind(
            metrics,
            data.frame(Complexity = complexity, Metric = "Bias", Value = overall_squared_bias),
            data.frame(Complexity = complexity, Metric = "Variance", Value = overall_variance)
          )
        }
        
        # Plot Bias
        plot_bias <- ggplot(metrics[metrics$Metric == "Bias", ], aes(x = as.factor(Complexity), y = Value)) +
          geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = 0.8, fill = "blue") +
          labs(
            title = "Bias",
            x = ifelse(model_type() == "KNN", "k Value", "Complexity"),  # Adjust x-axis label
            y = "Bias"  # Provide a label for y-axis
          ) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "none")  # Remove legend
        
        print(plot_bias)
      })
      
      
      output$tab3_plot3 <- renderPlot({
        
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        
        
        # Initialize a data frame to store bias and variance for different complexities/k values
        metrics <- data.frame(
          Complexity = numeric(),
          Metric = character(),
          Value = numeric()
        )
        
        # Define the range of complexities or k values
        if (model_type() == "Linear") {
          complexities <- 1  # Linear model has no complexity parameter
        } else if (model_type() == "Non-Linear") {
          complexities <- seq(0.1, 5, by = 1)
        } else if (model_type() == "Polynomial") {
          complexities <- 1:5
        } else if (model_type() == "KNN") {
          complexities <- 3:15  # Use k values for KNN
        } else {
          complexities <- numeric()  # No complexities for unknown model types
        }
        
        # Loop over each complexity/k value
        for (complexity in complexities) {
          # Store predictions for each complexity/k value
          predictions <- matrix(nrow = nrow(df_testdata), ncol = 20)
          
          # Fit models and obtain predictions
          if (model_type() == "Linear") {
            for (i in 1:20) {
              model <- lm(df_data[, i + 2] ~ inp, data = df_data)
              predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
            }
          } else if (model_type() == "Non-Linear") {
            for (i in 1:20) {
              model <- loess(df_data[, i + 2] ~ inp, span = 1/complexity, data = df_data)
              predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
            }
          } else if (model_type() == "Polynomial") {
            for (i in 1:20) {
              model <- lm(df_data[, i + 2] ~ poly(inp, complexity), data = df_data)
              predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
            }
          } else if (model_type() == "KNN") {
            for (i in 1:20) {
              knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[, i + 2], k = complexity)
              predictions[, i] <- knn_fit$pred
            }
          }
          
          # Calculate average predictions
          avg_predictions <- rowMeans(predictions, na.rm = TRUE)
          
          # Ensure true_form is correctly aligned with the predictions
          true_form <- df_testdata$true_form
          
          # Calculate bias
          bias <- avg_predictions - true_form
          
          # Calculate squared bias
          squared_bias <- bias^2
          
          # Calculate overall squared bias
          overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
          
          # Calculate variances of predictions
          prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
          
          # Calculate overall variance
          overall_variance <- mean(prediction_vars, na.rm = TRUE)
          
          # Store metrics for current complexity/k value
          metrics <- rbind(
            metrics,
            data.frame(Complexity = complexity, Metric = "Bias", Value = overall_squared_bias),
            data.frame(Complexity = complexity, Metric = "Variance", Value = overall_variance)
          )
        }
        
        # Plot Variance
        plot_variance <- ggplot(metrics[metrics$Metric == "Variance", ], aes(x = as.factor(Complexity), y = Value)) +
          geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = 0.8, fill = "red") +
          labs(
            title = "Variance",
            x = ifelse(model_type() == "KNN", "k Value", "Complexity"),  # Adjust x-axis label
            y = "Variance"  # Provide a label for y-axis
          ) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "none")  # Remove legend
        
        print(plot_variance)
      })
      
      output$tab3_plot4 <-renderPlot({
        
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        # Initialize a data frame to store bias and variance for different complexities/k values
        metrics <- data.frame(
          Complexity = numeric(),
          Metric = character(),
          Value = numeric()
        )
        
        # Define complexities or k values
        complexities <- switch(model_type(),  # Use model_type() instead of input$model_name
                               "Linear" = 1,  # Linear model has no complexity parameter
                               "Non-Linear" = seq(0.1, 5, by = 1),
                               "Polynomial" = 1:5,
                               "KNN" = 3:15)  # Add k values for KNN
        
        for (complexity in complexities) {
          predictions <- matrix(nrow = 1000, ncol = 20)
          training_mse_list <- numeric(20)
          
          for (i in 1:20) {
            response_col <- paste0("response", i)
            
            if (model_type() == "Linear") {
              model <- lm(df_data[[response_col]] ~ inp, data = df_data)
            } else if (model_type() == "Non-Linear") {
              model <- loess(df_data[[response_col]] ~ inp, span = 1/complexity, data = df_data)
            } else if (model_type() == "Polynomial") {
              model <- lm(df_data[[response_col]] ~ poly(inp, complexity), data = df_data)
            } else if (model_type() == "KNN") {
              knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[[response_col]], k = complexity)
              predictions[, i] <- knn_fit$pred
              training_preds <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_data$inp), y = df_data[[response_col]], k = complexity)$pred
              training_mse_list[i] <- mean((df_data[[response_col]] - training_preds)^2)
              next
            }
            
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
            training_preds <- predict(model, newdata = data.frame(inp = df_data$inp))
            training_mse_list[i] <- mean((df_data[[response_col]] - training_preds)^2)
          }
          
          avg_predictions <- rowMeans(predictions, na.rm = TRUE)
          true_form <- df_testdata$true_form[1:1000]
          bias <- avg_predictions - true_form
          squared_bias <- bias^2
          overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
          prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
          overall_variance <- mean(prediction_vars, na.rm = TRUE)
          test_MSE <- overall_variance + overall_squared_bias + (input$epsilon)^2
          training_MSE <- mean(training_mse_list, na.rm = TRUE)
          
          metrics <- rbind(
            metrics,
            data.frame(Complexity = complexity, Metric = "test MSE", Value = test_MSE),
            data.frame(Complexity = complexity, Metric = "training MSE", Value = training_MSE)
          )
        }
        
        # Plot training and test MSE
        plot_mse <- ggplot(metrics, aes(x = as.factor(Complexity), y = Value, color = Metric, group = Metric)) +
          geom_line(size = 1) +
          geom_point(size = 2) +
          labs(
            title = "Training MSE vs. Test MSE",
            x = ifelse(model_type() == "KNN", "k Value", "Complexity"),  # Adjust x-axis label
            y = "Mean Squared Error"
          ) +
          theme_minimal() +
          scale_color_manual(values = c("test MSE" = "purple", "training MSE" = "green")) +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top"  # Move legend to the top
          )
        
        print(plot_mse)
      })
      
      output$tab3_plot5 <- renderPlot({
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        # Get complexity from input$k_value
        complexity <- 5
        
        # Create a data frame to store true values and predictions
        plot_data <- data.frame(
          inp = rep(df_testdata$inp, 9),  # True form + 7 models + mean model
          Value = c(df_testdata$true_form, rep(NA, 8 * nrow(df_testdata))),
          Type = rep(c("True", rep("Model", 7), "Mean_Model"), each = nrow(df_testdata))
        )
        
        # Initialize a matrix to store predictions
        predictions <- matrix(nrow = nrow(df_testdata), ncol = 7)
        
        for (i in 1:7) {
          response_col <- paste0("response", i)
          
          if (model_type() == "Linear") {
            model <- lm(df_data[[response_col]] ~ inp, data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "Non-Linear") {
            model <- loess(df_data[[response_col]] ~ inp, span = 1/complexity, data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "Polynomial") {
            model <- lm(df_data[[response_col]] ~ poly(inp, complexity), data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "KNN") {
            knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[[response_col]], k = complexity)
            predictions[, i] <- knn_fit$pred
          }
        }
        
        # Convert predictions to long format for plotting
        predictions_long <- data.frame(
          inp = rep(df_testdata$inp, 7),
          Value = as.vector(predictions[, 1:7]),
          Model = rep(paste0("Model", 1:7), each = nrow(df_testdata))
        )
        
        # Calculate the mean of the predictions for each point
        mean_predictions <- rowMeans(predictions[, 1:7])
        
        # Update plot_data with mean predictions
        plot_data$Value[plot_data$Type == "Mean_Model"] <- mean_predictions
        
        # Plot true form, predictions for the first 7 models, and the mean model
        plot_predictions <- ggplot() +
          geom_line(data = plot_data[plot_data$Type == "True", ], aes(x = inp, y = Value), color = "black", size = 1) +
          geom_line(data = predictions_long, aes(x = inp, y = Value, group = Model), color = "pink", size = 0.5) +
          geom_line(data = plot_data[plot_data$Type == "Mean_Model", ], aes(x = inp, y = Value), color = "orange", size = 1.5) +
          labs(
            title = "K = 5",
            x = "x",
            y = "y",
            color = NULL  # Remove the legend title
          ) +
          theme_minimal() +
          theme(
            legend.position = "top",  # Position the legend to the top
            legend.title = element_blank(),  # Remove the legend title
            legend.text = element_text(size = 8),  # Make legend text smaller
            legend.key.size = unit(0.5, "lines")  # Make legend keys smaller
          )
        
        print(plot_predictions)
      })
      
      
      output$tab3_plot6 <- renderPlot({
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        # Get complexity from input$k_value
        complexity <- 7
        
        # Create a data frame to store true values and predictions
        plot_data <- data.frame(
          inp = rep(df_testdata$inp, 9),  # True form + 7 models + mean model
          Value = c(df_testdata$true_form, rep(NA, 8 * nrow(df_testdata))),
          Type = rep(c("True", rep("Model", 7), "Mean_Model"), each = nrow(df_testdata))
        )
        
        # Initialize a matrix to store predictions
        predictions <- matrix(nrow = nrow(df_testdata), ncol = 7)
        
        for (i in 1:7) {
          response_col <- paste0("response", i)
          
          if (model_type() == "Linear") {
            model <- lm(df_data[[response_col]] ~ inp, data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "Non-Linear") {
            model <- loess(df_data[[response_col]] ~ inp, span = 1/complexity, data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "Polynomial") {
            model <- lm(df_data[[response_col]] ~ poly(inp, complexity), data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "KNN") {
            knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[[response_col]], k = complexity)
            predictions[, i] <- knn_fit$pred
          }
        }
        
        # Convert predictions to long format for plotting
        predictions_long <- data.frame(
          inp = rep(df_testdata$inp, 7),
          Value = as.vector(predictions[, 1:7]),
          Model = rep(paste0("Model", 1:7), each = nrow(df_testdata))
        )
        
        # Calculate the mean of the predictions for each point
        mean_predictions <- rowMeans(predictions[, 1:7])
        
        # Update plot_data with mean predictions
        plot_data$Value[plot_data$Type == "Mean_Model"] <- mean_predictions
        
        # Plot true form, predictions for the first 7 models, and the mean model
        plot_predictions <- ggplot() +
          geom_line(data = plot_data[plot_data$Type == "True", ], aes(x = inp, y = Value), color = "black", size = 1) +
          geom_line(data = predictions_long, aes(x = inp, y = Value, group = Model), color = "pink", size = 0.5) +
          geom_line(data = plot_data[plot_data$Type == "Mean_Model", ], aes(x = inp, y = Value), color = "orange", size = 1.5) +
          labs(
            title = "K = 7",
            x = "x",
            y = "y",
            color = NULL  # Remove the legend title
          ) +
          theme_minimal() +
          theme(
            legend.position = "top",  # Position the legend to the top
            legend.title = element_blank(),  # Remove the legend title
            legend.text = element_text(size = 8),  # Make legend text smaller
            legend.key.size = unit(0.5, "lines")  # Make legend keys smaller
          )
        
        print(plot_predictions)
      })
      
      
      output$tab3_plot7 <- renderPlot({
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        # Get complexity from input$k_value
        complexity <- 11
        
        # Create a data frame to store true values and predictions
        plot_data <- data.frame(
          inp = rep(df_testdata$inp, 9),  # True form + 7 models + mean model
          Value = c(df_testdata$true_form, rep(NA, 8 * nrow(df_testdata))),
          Type = rep(c("True", rep("Model", 7), "Mean_Model"), each = nrow(df_testdata))
        )
        
        # Initialize a matrix to store predictions
        predictions <- matrix(nrow = nrow(df_testdata), ncol = 7)
        
        for (i in 1:7) {
          response_col <- paste0("response", i)
          
          if (model_type() == "Linear") {
            model <- lm(df_data[[response_col]] ~ inp, data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "Non-Linear") {
            model <- loess(df_data[[response_col]] ~ inp, span = 1/complexity, data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "Polynomial") {
            model <- lm(df_data[[response_col]] ~ poly(inp, complexity), data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "KNN") {
            knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[[response_col]], k = complexity)
            predictions[, i] <- knn_fit$pred
          }
        }
        
        # Convert predictions to long format for plotting
        predictions_long <- data.frame(
          inp = rep(df_testdata$inp, 7),
          Value = as.vector(predictions[, 1:7]),
          Model = rep(paste0("Model", 1:7), each = nrow(df_testdata))
        )
        
        # Calculate the mean of the predictions for each point
        mean_predictions <- rowMeans(predictions[, 1:7])
        
        # Update plot_data with mean predictions
        plot_data$Value[plot_data$Type == "Mean_Model"] <- mean_predictions
        
        # Plot true form, predictions for the first 7 models, and the mean model
        plot_predictions <- ggplot() +
          geom_line(data = plot_data[plot_data$Type == "True", ], aes(x = inp, y = Value), color = "black", size = 1) +
          geom_line(data = predictions_long, aes(x = inp, y = Value, group = Model), color = "pink", size = 0.5) +
          geom_line(data = plot_data[plot_data$Type == "Mean_Model", ], aes(x = inp, y = Value), color = "orange", size = 1.5) +
          labs(
            title = "K = 11",
            x = "x",
            y = "y",
            color = NULL  # Remove the legend title
          ) +
          theme_minimal() +
          theme(
            legend.position = "top",  # Position the legend to the top
            legend.title = element_blank(),  # Remove the legend title
            legend.text = element_text(size = 8),  # Make legend text smaller
            legend.key.size = unit(0.5, "lines")  # Make legend keys smaller
          )
        
        print(plot_predictions)
      })
      
      
      output$tab3_plot8 <- renderPlot({
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        # Get complexity from input$k_value
        complexity <- 13
        
        # Create a data frame to store true values and predictions
        plot_data <- data.frame(
          inp = rep(df_testdata$inp, 9),  # True form + 7 models + mean model
          Value = c(df_testdata$true_form, rep(NA, 8 * nrow(df_testdata))),
          Type = rep(c("True", rep("Model", 7), "Mean_Model"), each = nrow(df_testdata))
        )
        
        # Initialize a matrix to store predictions
        predictions <- matrix(nrow = nrow(df_testdata), ncol = 7)
        
        for (i in 1:7) {
          response_col <- paste0("response", i)
          
          if (model_type() == "Linear") {
            model <- lm(df_data[[response_col]] ~ inp, data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "Non-Linear") {
            model <- loess(df_data[[response_col]] ~ inp, span = 1/complexity, data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "Polynomial") {
            model <- lm(df_data[[response_col]] ~ poly(inp, complexity), data = df_data)
            predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          } else if (model_type() == "KNN") {
            knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[[response_col]], k = complexity)
            predictions[, i] <- knn_fit$pred
          }
        }
        
        # Convert predictions to long format for plotting
        predictions_long <- data.frame(
          inp = rep(df_testdata$inp, 7),
          Value = as.vector(predictions[, 1:7]),
          Model = rep(paste0("Model", 1:7), each = nrow(df_testdata))
        )
        
        # Calculate the mean of the predictions for each point
        mean_predictions <- rowMeans(predictions[, 1:7])
        
        # Update plot_data with mean predictions
        plot_data$Value[plot_data$Type == "Mean_Model"] <- mean_predictions
        
        # Plot true form, predictions for the first 7 models, and the mean model
        plot_predictions <- ggplot() +
          geom_line(data = plot_data[plot_data$Type == "True", ], aes(x = inp, y = Value), color = "black", size = 1) +
          geom_line(data = predictions_long, aes(x = inp, y = Value, group = Model), color = "pink", size = 0.5) +
          geom_line(data = plot_data[plot_data$Type == "Mean_Model", ], aes(x = inp, y = Value), color = "orange", size = 1.5) +
          labs(
            title = "K = 13",
            x = "x",
            y = "y",
            color = NULL  # Remove the legend title
          ) +
          theme_minimal() +
          theme(
            legend.position = "top",  # Position the legend to the top
            legend.title = element_blank(),  # Remove the legend title
            legend.text = element_text(size = 8),  # Make legend text smaller
            legend.key.size = unit(0.5, "lines")  # Make legend keys smaller
          )
        
        print(plot_predictions)
      })
      
      
      #####
      
      
      
      output$tab4_plot1 <- renderPlot({
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        
        # Initialize the plot with training data
        p <- ggplot(data = df_data, aes(x = inp, y = response1)) + 
          geom_point() +
          labs(title = "Training Data", y = "y", x = "x")
        
        # Conditional limits based on the dataset
        if (input$dataset == "Data set 1") {
          p <- p + scale_y_continuous(limits = c(0, 5)) +
            scale_x_continuous(limits = c(-5, 10))
        } else if (input$dataset == "Data set 2") {
          p <- p + scale_y_continuous(limits = c(3, 13)) +
            scale_x_continuous(limits = c(20, 40))
        } else if (input$dataset == "Data set 3") {
          p <- p + scale_y_continuous(limits = c(-30, 30)) +
            scale_x_continuous(limits = c(-6, 6))
        }
        
        # Use the reactive model_type
        if (model_type() == "Regression Tree") {
          # Fit the regression tree model
          tree_model <- rpart(response1 ~ inp, data = df_data, 
                              control = rpart.control(maxdepth = input$tree_size, cp = 0, xval = 0, minbucket = 5))
          
          # Predict using the regression tree
          df_data$tree_pred <- predict(tree_model, newdata = df_data)
          
          # Add the regression tree prediction to the plot
          p <- p + geom_line(data = df_data, aes(x = inp, y = tree_pred), size = 1, color = "red")
        }
        
        print(p)
      })
      
      
      output$tab4_plot2 <- renderPlot({
        
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        
        
        # Initialize a data frame to store bias and variance for different complexities/k values
        metrics <- data.frame(
          Complexity = numeric(),
          Metric = character(),
          Value = numeric()
        )
        
        # Define the range of complexities or k values
        if (model_type() == "Regression Tree") {
          complexities <- 2:7
        } else if (model_type() == "Polynomial") {
          complexities <- 1:5
        } else if (model_type() == "KNN") {
          complexities <- 3:15  # Use k values for KNN
        } else {
          complexities <- numeric()  # No complexities for unknown model types
        }
        
        # Loop over each complexity/k value
        for (complexity in complexities) {
          # Store predictions for each complexity/k value
          predictions <- matrix(nrow = nrow(df_testdata), ncol = 20)
          
          if (model_type() == "Regression Tree") {
            for (i in 1:20) {
              response_col <- colnames(df_data)[i + 2]
              formula <- as.formula(paste(response_col, "~ inp"))
              
              # Fit the regression tree model with the specified max depth (complexity)
              tree_model <- rpart(formula, data = df_data, 
                                  control = rpart.control(maxdepth = complexity, cp = 0, xval = 0, minbucket = 5))
              
              # Predict using the tree model
              predictions[, i] <- predict(tree_model, newdata = df_testdata)
            }
          }
          
          # Calculate average predictions
          avg_predictions <- rowMeans(predictions, na.rm = TRUE)
          
          # Ensure true_form is correctly aligned with the predictions
          true_form <- df_testdata$true_form
          
          # Calculate bias
          bias <- avg_predictions - true_form
          
          # Calculate squared bias
          squared_bias <- bias^2
          
          # Calculate overall squared bias
          overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
          
          # Calculate variances of predictions
          prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
          
          # Calculate overall variance
          overall_variance <- mean(prediction_vars, na.rm = TRUE)
          
          # Store metrics for current complexity/k value
          metrics <- rbind(
            metrics,
            data.frame(Complexity = complexity, Metric = "Bias", Value = overall_squared_bias),
            data.frame(Complexity = complexity, Metric = "Variance", Value = overall_variance)
          )
        }
        
        # Plot Bias
        plot_bias <- ggplot(metrics[metrics$Metric == "Bias", ], aes(x = as.factor(Complexity), y = Value)) +
          geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = 0.8, fill = "blue") +
          labs(
            title = "Bias",
            x = ifelse(model_type() == "KNN", "k Value", "Complexity"),  # Adjust x-axis label
            y = "Bias"  # Provide a label for y-axis
          ) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "none")  # Remove legend
        
        print(plot_bias)
      })
      
      
      output$tab4_plot3 <- renderPlot({
        
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        
        
        # Initialize a data frame to store bias and variance for different complexities/k values
        metrics <- data.frame(
          Complexity = numeric(),
          Metric = character(),
          Value = numeric()
        )
        
        # Define the range of complexities or k values
        if (model_type() == "Regression Tree") {
          complexities <- 2:7
        } else if (model_type() == "Polynomial") {
          complexities <- 1:5
        } else if (model_type() == "KNN") {
          complexities <- 3:15  # Use k values for KNN
        } else {
          complexities <- numeric()  # No complexities for unknown model types
        }
        
        # Loop over each complexity/k value
        for (complexity in complexities) {
          # Store predictions for each complexity/k value
          predictions <- matrix(nrow = nrow(df_testdata), ncol = 20)
          
          if (model_type() == "Regression Tree") {
            for (i in 1:20) {
              response_col <- colnames(df_data)[i + 2]
              formula <- as.formula(paste(response_col, "~ inp"))
              
              # Fit the regression tree model with the specified max depth (complexity)
              tree_model <- rpart(formula, data = df_data, 
                                  control = rpart.control(maxdepth = complexity, cp = 0, xval = 0, minbucket = 5))
              
              # Predict using the tree model
              predictions[, i] <- predict(tree_model, newdata = df_testdata)
            }
          }
          
          # Calculate average predictions
          avg_predictions <- rowMeans(predictions, na.rm = TRUE)
          
          # Ensure true_form is correctly aligned with the predictions
          true_form <- df_testdata$true_form
          
          # Calculate bias
          bias <- avg_predictions - true_form
          
          # Calculate squared bias
          squared_bias <- bias^2
          
          # Calculate overall squared bias
          overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
          
          # Calculate variances of predictions
          prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
          
          # Calculate overall variance
          overall_variance <- mean(prediction_vars, na.rm = TRUE)
          
          # Store metrics for current complexity/k value
          metrics <- rbind(
            metrics,
            data.frame(Complexity = complexity, Metric = "Bias", Value = overall_squared_bias),
            data.frame(Complexity = complexity, Metric = "Variance", Value = overall_variance)
          )
        }
        
        
        
        # Plot Variance
        plot_variance <- ggplot(metrics[metrics$Metric == "Variance", ], aes(x = as.factor(Complexity), y = Value)) +
          geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = 0.8, fill = "red") +
          labs(
            title = "Variance",
            x = ifelse(model_type() == "KNN", "k Value", "Complexity"),  # Adjust x-axis label
            y = "Variance"  # Provide a label for y-axis
          ) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "none")  # Remove legend
        
        print(plot_variance)
      })
      
      library(rpart)
      library(FNN)
      library(ggplot2)
      
      output$tab4_plot4 <- renderPlot({
        
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        # Initialize a data frame to store bias and variance for different complexities/k values
        metrics <- data.frame(
          Complexity = numeric(),
          Metric = character(),
          Value = numeric()
        )
        
        # Define complexities or k values
        complexities <- switch(model_type(),  # Use model_type() instead of input$model_name
                               "Regression Tree" = 2:7,
                               "Polynomial" = 1:5,
                               "KNN" = 3:15)  # Add k values for KNN
        
        for (complexity in complexities) {
          predictions <- matrix(nrow = 1000, ncol = 20)
          training_mse_list <- numeric(20)
          
          for (i in 1:20) {
            response_col <- paste0("response", i)
            
            if (model_type() == "Regression Tree") {
              # Construct the formula for the response variable
              formula <- as.formula(paste(response_col, "~ inp"))
              
              # Fit the regression tree model with the specified max depth (complexity)
              tree_model <- rpart(formula, data = df_data, 
                                  control = rpart.control(maxdepth = complexity, cp = 0, xval = 0, minbucket = 5))
              
              # Predict using the tree model
              predictions[, i] <- predict(tree_model, newdata = df_testdata)
              
              # Predict on training data for MSE calculation
              training_preds <- predict(tree_model, newdata = df_data)
              training_mse_list[i] <- mean((df_data[[response_col]] - training_preds)^2)
              
            }
          }
          
          # Calculate metrics (e.g., bias, variance) and store in metrics data frame
          avg_predictions <- rowMeans(predictions, na.rm = TRUE)
          true_form <- df_testdata$true_form[1:1000]
          bias <- avg_predictions - true_form
          squared_bias <- bias^2
          overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
          prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
          overall_variance <- mean(prediction_vars, na.rm = TRUE)
          test_MSE <- overall_variance + overall_squared_bias + (input$epsilon)^2
          training_MSE <- mean(training_mse_list, na.rm = TRUE)
          
          metrics <- rbind(
            metrics,
            data.frame(Complexity = complexity, Metric = "test MSE", Value = test_MSE),
            data.frame(Complexity = complexity, Metric = "training MSE", Value = training_MSE)
          )
        }
        
        # Plot training and test MSE
        plot_mse <- ggplot(metrics, aes(x = as.factor(Complexity), y = Value, color = Metric, group = Metric)) +
          geom_line(size = 1) +
          geom_point(size = 2) +
          labs(
            title = "Training MSE vs. Test MSE",
            x = ifelse(model_type() == "KNN", "k Value", "Complexity"),  # Adjust x-axis label
            y = "Mean Squared Error"
          ) +
          theme_minimal() +
          scale_color_manual(values = c("test MSE" = "purple", "training MSE" = "green")) +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top"  # Move legend to the top
          )
        
        print(plot_mse)
      })
      
      
      output$tab4_plot5 <- renderPlot({
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        # Get complexity from input$tree_size
        complexity <- 2
        
        # Create a data frame to store true values and predictions
        plot_data <- data.frame(
          inp = rep(df_testdata$inp, 9),  # True form + 7 models + mean model
          Value = c(df_testdata$true_form, rep(NA, 8 * nrow(df_testdata))),
          Type = rep(c("True", rep("Model", 7), "Mean_Model"), each = nrow(df_testdata))
        )
        
        # Initialize a matrix to store predictions
        predictions <- matrix(nrow = nrow(df_testdata), ncol = 7)
        
        # Fit the models and store predictions
        for (i in 1:7) {
          response_col <- paste0("response", i)
          
          if (model_type() == "Regression Tree") {
            # Construct the formula for the response variable
            formula <- as.formula(paste(response_col, "~ inp"))
            
            # Fit the regression tree model with the specified max depth (complexity)
            tree_model <- rpart(formula, data = df_data, 
                                control = rpart.control(maxdepth = complexity, cp = 0, xval = 0, minbucket = 5))
            
            # Predict using the tree model
            predictions[, i] <- predict(tree_model, newdata = data.frame(inp = df_testdata$inp))
          }
        }
        
        # Convert predictions to long format for plotting
        predictions_long <- data.frame(
          inp = rep(df_testdata$inp, 7),
          Value = as.vector(predictions),
          Model = rep(paste0("Model", 1:7), each = nrow(df_testdata))
        )
        
        # Update plot_data with mean predictions
        mean_predictions <- rowMeans(predictions)
        plot_data$Value[plot_data$Type == "Mean_Model"] <- mean_predictions
        
        # Plot true form, predictions for the first 7 models, and the mean model
        plot_predictions <- ggplot() +
          geom_line(data = plot_data[plot_data$Type == "True", ], aes(x = inp, y = Value), color = "black", size = 1) +
          geom_line(data = predictions_long, aes(x = inp, y = Value, group = Model), color = "pink", size = 0.5) +
          geom_line(data = plot_data[plot_data$Type == "Mean_Model", ], aes(x = inp, y = Value), color = "orange", size = 1.5) +
          labs(
            title = "Tree Depth = 2",
            x = "x",
            y = "y",
            color = NULL  # Remove the legend title
          ) +
          theme_minimal() +
          theme(
            legend.position = "top",  # Position the legend to the right
            legend.title = element_blank(),  # Remove the legend title
            legend.text = element_text(size = 8),  # Make legend text smaller
            legend.key.size = unit(0.5, "lines")  # Make legend keys smaller
          )
        
        print(plot_predictions)
      })
      
      output$tab4_plot6 <- renderPlot({
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        # Get complexity from input$tree_size
        complexity <- 4
        
        # Create a data frame to store true values and predictions
        plot_data <- data.frame(
          inp = rep(df_testdata$inp, 9),  # True form + 7 models + mean model
          Value = c(df_testdata$true_form, rep(NA, 8 * nrow(df_testdata))),
          Type = rep(c("True", rep("Model", 7), "Mean_Model"), each = nrow(df_testdata))
        )
        
        # Initialize a matrix to store predictions
        predictions <- matrix(nrow = nrow(df_testdata), ncol = 7)
        
        # Fit the models and store predictions
        for (i in 1:7) {
          response_col <- paste0("response", i)
          
          if (model_type() == "Regression Tree") {
            # Construct the formula for the response variable
            formula <- as.formula(paste(response_col, "~ inp"))
            
            # Fit the regression tree model with the specified max depth (complexity)
            tree_model <- rpart(formula, data = df_data, 
                                control = rpart.control(maxdepth = complexity, cp = 0, xval = 0, minbucket = 5))
            
            # Predict using the tree model
            predictions[, i] <- predict(tree_model, newdata = data.frame(inp = df_testdata$inp))
          }
        }
        
        # Convert predictions to long format for plotting
        predictions_long <- data.frame(
          inp = rep(df_testdata$inp, 7),
          Value = as.vector(predictions),
          Model = rep(paste0("Model", 1:7), each = nrow(df_testdata))
        )
        
        # Update plot_data with mean predictions
        mean_predictions <- rowMeans(predictions)
        plot_data$Value[plot_data$Type == "Mean_Model"] <- mean_predictions
        
        # Plot true form, predictions for the first 7 models, and the mean model
        plot_predictions <- ggplot() +
          geom_line(data = plot_data[plot_data$Type == "True", ], aes(x = inp, y = Value), color = "black", size = 1) +
          geom_line(data = predictions_long, aes(x = inp, y = Value, group = Model), color = "pink", size = 0.5) +
          geom_line(data = plot_data[plot_data$Type == "Mean_Model", ], aes(x = inp, y = Value), color = "orange", size = 1.5) +
          labs(
            title = "Tree Depth = 4",
            x = "x",
            y = "y",
            color = NULL  # Remove the legend title
          ) +
          theme_minimal() +
          theme(
            legend.position = "top",  # Position the legend to the right
            legend.title = element_blank(),  # Remove the legend title
            legend.text = element_text(size = 8),  # Make legend text smaller
            legend.key.size = unit(0.5, "lines")  # Make legend keys smaller
          )
        
        print(plot_predictions)
      })
      
      
      output$tab4_plot7 <- renderPlot({
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        # Get complexity from input$tree_size
        complexity <- 5
        
        # Create a data frame to store true values and predictions
        plot_data <- data.frame(
          inp = rep(df_testdata$inp, 9),  # True form + 7 models + mean model
          Value = c(df_testdata$true_form, rep(NA, 8 * nrow(df_testdata))),
          Type = rep(c("True", rep("Model", 7), "Mean_Model"), each = nrow(df_testdata))
        )
        
        # Initialize a matrix to store predictions
        predictions <- matrix(nrow = nrow(df_testdata), ncol = 7)
        
        # Fit the models and store predictions
        for (i in 1:7) {
          response_col <- paste0("response", i)
          
          if (model_type() == "Regression Tree") {
            # Construct the formula for the response variable
            formula <- as.formula(paste(response_col, "~ inp"))
            
            # Fit the regression tree model with the specified max depth (complexity)
            tree_model <- rpart(formula, data = df_data, 
                                control = rpart.control(maxdepth = complexity, cp = 0, xval = 0, minbucket = 5))
            
            # Predict using the tree model
            predictions[, i] <- predict(tree_model, newdata = data.frame(inp = df_testdata$inp))
          }
        }
        
        # Convert predictions to long format for plotting
        predictions_long <- data.frame(
          inp = rep(df_testdata$inp, 7),
          Value = as.vector(predictions),
          Model = rep(paste0("Model", 1:7), each = nrow(df_testdata))
        )
        
        # Update plot_data with mean predictions
        mean_predictions <- rowMeans(predictions)
        plot_data$Value[plot_data$Type == "Mean_Model"] <- mean_predictions
        
        # Plot true form, predictions for the first 7 models, and the mean model
        plot_predictions <- ggplot() +
          geom_line(data = plot_data[plot_data$Type == "True", ], aes(x = inp, y = Value), color = "black", size = 1) +
          geom_line(data = predictions_long, aes(x = inp, y = Value, group = Model), color = "pink", size = 0.5) +
          geom_line(data = plot_data[plot_data$Type == "Mean_Model", ], aes(x = inp, y = Value), color = "orange", size = 1.5) +
          labs(
            title = "Tree Depth = 5",
            x = "x",
            y = "y",
            color = NULL  # Remove the legend title
          ) +
          theme_minimal() +
          theme(
            legend.position = "top",  # Position the legend to the right
            legend.title = element_blank(),  # Remove the legend title
            legend.text = element_text(size = 8),  # Make legend text smaller
            legend.key.size = unit(0.5, "lines")  # Make legend keys smaller
          )
        
        print(plot_predictions)
      })
      
      
      output$tab4_plot8 <- renderPlot({
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        # Get complexity from input$tree_size
        complexity <- 7
        
        # Create a data frame to store true values and predictions
        plot_data <- data.frame(
          inp = rep(df_testdata$inp, 9),  # True form + 7 models + mean model
          Value = c(df_testdata$true_form, rep(NA, 8 * nrow(df_testdata))),
          Type = rep(c("True", rep("Model", 7), "Mean_Model"), each = nrow(df_testdata))
        )
        
        # Initialize a matrix to store predictions
        predictions <- matrix(nrow = nrow(df_testdata), ncol = 7)
        
        # Fit the models and store predictions
        for (i in 1:7) {
          response_col <- paste0("response", i)
          
          if (model_type() == "Regression Tree") {
            formula <- as.formula(paste(response_col, "~ inp"))
            
            # Fit the regression tree model with the specified max depth (complexity)
            tree_model <- rpart(formula, data = df_data, 
                                control = rpart.control(maxdepth = complexity, cp = 0, xval = 0, minbucket = 5))
            
            # Predict using the tree model
            predictions[, i] <- predict(tree_model, newdata = data.frame(inp = df_testdata$inp))
          }
        }
        
        # Convert predictions to long format for plotting
        predictions_long <- data.frame(
          inp = rep(df_testdata$inp, 7),
          Value = as.vector(predictions),
          Model = rep(paste0("Model", 1:7), each = nrow(df_testdata))
        )
        
        # Update plot_data with mean predictions
        mean_predictions <- rowMeans(predictions)
        plot_data$Value[plot_data$Type == "Mean_Model"] <- mean_predictions
        
        # Plot true form, predictions for the first 7 models, and the mean model
        plot_predictions <- ggplot() +
          geom_line(data = plot_data[plot_data$Type == "True", ], aes(x = inp, y = Value), color = "black", size = 1) +
          geom_line(data = predictions_long, aes(x = inp, y = Value, group = Model), color = "pink", size = 0.5) +
          geom_line(data = plot_data[plot_data$Type == "Mean_Model", ], aes(x = inp, y = Value), color = "orange", size = 1.5) +
          labs(
            title = "Tree Depth = 7",
            x = "x",
            y = "y",
            color = NULL  # Remove the legend title
          ) +
          theme_minimal() +
          theme(
            legend.position = "top",  # Position the legend to the right
            legend.title = element_blank(),  # Remove the legend title
            legend.text = element_text(size = 8),  # Make legend text smaller
            legend.key.size = unit(0.5, "lines")  # Make legend keys smaller
          )
        
        print(plot_predictions)
      })
      
      output$tab5_plot1 <- renderPlot({
        df_data <- df()$toy_data
        
        p <- ggplot(data = df_data, aes(x = inp, y = response1)) + 
          geom_point() +
          labs(title = "Training Data", y = "y", x = "x")
        
        if (input$dataset == "Data set 1") {
          p <- p + scale_y_continuous(limits = c(0, 5)) +
            scale_x_continuous(limits = c(-5, 10))
        } else if (input$dataset == "Data set 2") {
          p <- p + scale_y_continuous(limits = c(3, 13)) +
            scale_x_continuous(limits = c(20, 40))
        } else if (input$dataset == "Data set 3") {
          p <- p + scale_y_continuous(limits = c(-30, 30)) +
            scale_x_continuous(limits = c(-6, 6))
        }
        
        if (model_type() == "LASSO Regression") {
          
          # Polynomial degree fixed at 5
          degreeL <- 5
          
          # Prepare the polynomial features
          X_poly <- poly(df_data$inp, degreeL, raw = TRUE)
          
          # Fit the Lasso model using the specified lambda from input$lambda
          lasso_model <- glmnet(X_poly, df_data$response1, alpha = 1, lambda = input$lambda)
          
          # Predict using the Lasso model with the specified lambda
          lasso_predictions <- predict(lasso_model, s = input$lambda, newx = X_poly)
          
          # Create a data frame for Lasso predictions
          df_lasso <- data.frame(inp = df_data$inp, response1 = as.vector(lasso_predictions))
          
          # Add the Lasso regression line to the plot
          p <- p + geom_line(data = df_lasso, aes(x = inp, y = response1, color = "Lasso model"), size = 1, show.legend = FALSE)
          
        }
        
        print(p)
      })
      
      
      
      
      
      output$tab5_plot2 <- renderPlot({
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        # Initialize a data frame to store bias and variance for different lambda values
        metrics <- data.frame(
          Complexity = numeric(),
          Metric = character(),
          Value = numeric()
        )
        
        # Define a sequence of lambda values
        lambda_values <- seq(0, 0.14, by = 0.02)
        
        # Loop over each lambda value
        for (lambda in lambda_values) {
          # Store predictions for each lambda value
          predictions <- matrix(nrow = nrow(df_testdata), ncol = 20)
          
          # Loop over each sample to fit a Lasso model and obtain predictions
          for (i in 1:20) {
            # Extract the ith sample
            sample_data <- df_data[[i + 2]]  # Adjust column index if needed
            
            # Prepare the data for Lasso regression (polynomial degree 5)
            X_poly <- model.matrix(~ poly(df_data$inp, 5), data = df_data)[, -1]  # Design matrix
            y <- sample_data  # Response variable
            newx <- model.matrix(~ poly(df_testdata$inp, 5), data = df_testdata)[, -1]  # Test design matrix
            
            # Fit the Lasso regression model
            lasso_model <- glmnet(X_poly, y, alpha = 1, lambda = lambda)
            
            # Predict using the Lasso model
            predictions[, i] <- predict(lasso_model, newx = newx)[, 1]  # Assuming predictions for the first lambda
          }
          
          # Calculate average predictions
          avg_predictions <- rowMeans(predictions, na.rm = TRUE)
          
          # Ensure true_form is correctly aligned with the predictions
          true_form <- df_testdata$true_form
          
          # Calculate bias
          bias <- avg_predictions - true_form
          
          # Calculate squared bias
          squared_bias <- bias^2
          
          # Calculate overall squared bias
          overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
          
          # Calculate variances of predictions
          prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
          
          # Calculate overall variance
          overall_variance <- mean(prediction_vars, na.rm = TRUE)
          
          # Store metrics for current lambda value
          metrics <- rbind(
            metrics,
            data.frame(Complexity = lambda, Metric = "Bias", Value = overall_squared_bias),
            data.frame(Complexity = lambda, Metric = "Variance", Value = overall_variance)
          )
        }
        
        # Plot Bias
        plot_bias <- ggplot(metrics[metrics$Metric == "Bias", ], aes(x = as.factor(Complexity), y = Value)) +
          geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = 0.8, fill = "blue") +
          labs(
            title = "Bias",
            x = "Lambda",
            y = "Bias"
          ) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "none")
        
        print(plot_bias)
      })
      
      
      output$tab5_plot3 <- renderPlot({
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        # Initialize a data frame to store bias and variance for different lambda values
        metrics <- data.frame(
          Complexity = numeric(),
          Metric = character(),
          Value = numeric()
        )
        
        # Define a sequence of lambda values
        lambda_values <- seq(0, 0.14, by = 0.02)
        
        # Loop over each lambda value
        for (lambda in lambda_values) {
          # Store predictions for each lambda value
          predictions <- matrix(nrow = nrow(df_testdata), ncol = 20)
          
          # Loop over each sample to fit a Lasso model and obtain predictions
          for (i in 1:20) {
            # Extract the ith sample
            sample_data <- df_data[[i + 2]]  # Adjust column index if needed
            
            # Prepare the data for Lasso regression (polynomial degree 5)
            X_poly <- model.matrix(~ poly(df_data$inp, 5), data = df_data)[, -1]  # Design matrix
            y <- sample_data  # Response variable
            newx <- model.matrix(~ poly(df_testdata$inp, 5), data = df_testdata)[, -1]  # Test design matrix
            
            # Fit the Lasso regression model
            lasso_model <- glmnet(X_poly, y, alpha = 1, lambda = lambda)
            
            # Predict using the Lasso model
            predictions[, i] <- predict(lasso_model, newx = newx)[, 1]  # Assuming predictions for the first lambda
          }
          
          # Calculate average predictions
          avg_predictions <- rowMeans(predictions, na.rm = TRUE)
          
          # Ensure true_form is correctly aligned with the predictions
          true_form <- df_testdata$true_form
          
          # Calculate bias
          bias <- avg_predictions - true_form
          
          # Calculate squared bias
          squared_bias <- bias^2
          
          # Calculate overall squared bias
          overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
          
          # Calculate variances of predictions
          prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
          
          # Calculate overall variance
          overall_variance <- mean(prediction_vars, na.rm = TRUE)
          
          # Store metrics for current lambda value
          metrics <- rbind(
            metrics,
            data.frame(Complexity = lambda, Metric = "Bias", Value = overall_squared_bias),
            data.frame(Complexity = lambda, Metric = "Variance", Value = overall_variance)
          )
        }
        
        # Plot Variance
        plot_Variance <- ggplot(metrics[metrics$Metric == "Variance", ], aes(x = as.factor(Complexity), y = Value)) +
          geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = 0.8, fill = "red") +
          labs(
            title = "Variance",
            x = "Lambda",
            y = "Variance"
          ) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "none")
        
        print(plot_Variance)
      })
      
      
      
      
      output$tab5_plot4 <- renderPlot({
        
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        # Initialize a data frame to store bias and variance for different lambda values
        metrics <- data.frame(
          Complexity = numeric(),
          Metric = character(),
          Value = numeric()
        )
        
        # Define the range of lambda values for Lasso regression
        lambda_values <- seq(0, 0.14, by = 0.02)
        
        for (lambda in lambda_values) {
          # Store predictions and training MSEs for each lambda
          predictions <- matrix(nrow = nrow(df_testdata), ncol = 20)
          training_mse_list <- numeric(20)
          
          for (i in 1:20) {
            response_col <- paste0("response", i)
            
            # Prepare the data for Lasso regression (polynomial degree 5)
            X_poly <- model.matrix(~ poly(df_data$inp, 5, raw = TRUE), data = df_data)[, -1]  # Design matrix
            y <- df_data[[response_col]]  # Response variable
            
            newx <- model.matrix(~ poly(df_testdata$inp, 5, raw = TRUE), data = df_testdata)[, -1]  # Test design matrix
            
            # Fit the Lasso regression model using the specified lambda
            lasso_model <- glmnet(X_poly, y, alpha = 1, lambda = lambda)
            
            # Predict using the Lasso model
            predictions[, i] <- predict(lasso_model, newx = newx)[, 1]
            
            # Predict on training data for MSE calculation
            training_preds <- predict(lasso_model, newx = X_poly)[, 1]
            training_mse_list[i] <- mean((y - training_preds)^2)
          }
          
          # Calculate average predictions
          avg_predictions <- rowMeans(predictions, na.rm = TRUE)
          
          # Ensure true_form is correctly aligned with the predictions
          true_form <- df_testdata$true_form
          
          # Calculate bias
          bias <- avg_predictions - true_form
          squared_bias <- bias^2
          overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
          
          # Calculate variances of predictions
          prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
          overall_variance <- mean(prediction_vars, na.rm = TRUE)
          
          # Calculate MSE
          test_MSE <- overall_variance + overall_squared_bias + (input$epsilon)^2
          training_MSE <- mean(training_mse_list, na.rm = TRUE)
          
          # Store metrics for current lambda value
          metrics <- rbind(
            metrics,
            data.frame(Complexity = lambda, Metric = "test MSE", Value = test_MSE),
            data.frame(Complexity = lambda, Metric = "training MSE", Value = training_MSE)
          )
        }
        
        # Plot training and test MSE
        plot_mse <- ggplot(metrics, aes(x = as.factor(Complexity), y = Value, color = Metric, group = Metric)) +
          geom_line(size = 1) +
          geom_point(size = 2) +
          labs(
            title = "Training MSE vs. Test MSE",
            x = "Lambda",
            y = "Mean Squared Error"
          ) +
          theme_minimal() +
          scale_color_manual(values = c("test MSE" = "purple", "training MSE" = "green")) +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top"  # Move legend to the top
          )
        
        print(plot_mse)
      })
      
      
      output$tab5_plot6 <- renderPlot({
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        # Set lambda to 0 for this plot
        lambda <- 0.02
        
        # Define the polynomial degree for Lasso regression
        complexity <- 5
        
        # Prepare data for Lasso regression (polynomial degree 5)
        X_poly_test <- model.matrix(~ poly(df_testdata$inp, complexity, raw = TRUE), data = df_testdata)[, -1]  # Test design matrix
        
        # Create a data frame to store true values and predictions
        plot_data <- data.frame(
          inp = rep(df_testdata$inp, 9),  # True form + 7 models + mean model
          Value = c(df_testdata$true_form, rep(NA, 8 * nrow(df_testdata))),
          Type = rep(c("True", rep("Model", 7), "Mean_Model"), each = nrow(df_testdata))
        )
        
        # Initialize a matrix to store predictions
        predictions <- matrix(nrow = nrow(df_testdata), ncol = 7)
        
        for (i in 1:7) {
          # Define the response column
          response_col <- paste0("response", i)
          
          # Get the training response variable for this sample
          y <- df_data[[response_col]]
          
          # Prepare the training design matrix for this sample
          X_poly_train <- model.matrix(~ poly(df_data$inp, complexity, raw = TRUE), data = df_data)[, -1]  # Training design matrix
          
          # Fit Lasso model with lambda = 0
          lasso_model <- glmnet(X_poly_train, y, alpha = 1, lambda = lambda)
          
          # Predict using the Lasso model
          predictions[, i] <- predict(lasso_model, newx = X_poly_test)[, 1]
        }
        
        # Convert predictions to long format for plotting
        predictions_long <- data.frame(
          inp = rep(df_testdata$inp, 7),
          Value = as.vector(predictions[, 1:7]),
          Model = rep(paste0("Model", 1:7), each = nrow(df_testdata))
        )
        
        # Calculate the mean of the predictions
        mean_predictions <- rowMeans(predictions, na.rm = TRUE)
        
        # Update plot_data with mean predictions
        plot_data$Value[plot_data$Type == "Mean_Model"] <- mean_predictions
        
        # Plot true form, Lasso model predictions, and the mean model
        plot_predictions <- ggplot() +
          geom_line(data = plot_data[plot_data$Type == "True", ], aes(x = inp, y = Value, color = "True Form"), size = 1) +
          geom_line(data = predictions_long, aes(x = inp, y = Value, group = Model, color = "Predictions"), size = 0.5) +
          geom_line(data = plot_data[plot_data$Type == "Mean_Model", ], aes(x = inp, y = Value, color = "Average Prediction"), size = 1.5) +
          labs(
            title = "Lasso Regression, Lambda = 0.02",
            x = "x",
            y = "y",
            color = NULL  # Remove the legend title
          ) +
          theme_minimal() +
          theme(
            legend.position = "top",  # Position the legend to the top
            legend.title = element_blank(),  # Remove the legend title
            legend.text = element_text(size = 8),  # Make legend text smaller
            legend.key.size = unit(0.5, "lines")  # Make legend keys smaller
          ) +
          scale_color_manual(values = c("Average Prediction" = "orange", "Predictions" = "pink", "True Form" = "black"))
        
        print(plot_predictions)
      })
      
      output$tab5_plot5 <- renderPlot({
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        # Set lambda to 0 for this plot
        lambda <- 0
        
        # Define the polynomial degree for Lasso regression
        complexity <- 5
        
        # Prepare data for Lasso regression (polynomial degree 5)
        X_poly_test <- model.matrix(~ poly(df_testdata$inp, complexity, raw = TRUE), data = df_testdata)[, -1]  # Test design matrix
        
        # Create a data frame to store true values and predictions
        plot_data <- data.frame(
          inp = rep(df_testdata$inp, 9),  # True form + 7 models + mean model
          Value = c(df_testdata$true_form, rep(NA, 8 * nrow(df_testdata))),
          Type = rep(c("True", rep("Model", 7), "Mean_Model"), each = nrow(df_testdata))
        )
        
        # Initialize a matrix to store predictions
        predictions <- matrix(nrow = nrow(df_testdata), ncol = 7)
        
        for (i in 1:7) {
          # Define the response column
          response_col <- paste0("response", i)
          
          # Get the training response variable for this sample
          y <- df_data[[response_col]]
          
          # Prepare the training design matrix for this sample
          X_poly_train <- model.matrix(~ poly(df_data$inp, complexity, raw = TRUE), data = df_data)[, -1]  # Training design matrix
          
          # Fit Lasso model with lambda = 0
          lasso_model <- glmnet(X_poly_train, y, alpha = 1, lambda = lambda)
          
          # Predict using the Lasso model
          predictions[, i] <- predict(lasso_model, newx = X_poly_test)[, 1]
        }
        
        # Convert predictions to long format for plotting
        predictions_long <- data.frame(
          inp = rep(df_testdata$inp, 7),
          Value = as.vector(predictions[, 1:7]),
          Model = rep(paste0("Model", 1:7), each = nrow(df_testdata))
        )
        
        # Calculate the mean of the predictions
        mean_predictions <- rowMeans(predictions, na.rm = TRUE)
        
        # Update plot_data with mean predictions
        plot_data$Value[plot_data$Type == "Mean_Model"] <- mean_predictions
        
        # Plot true form, Lasso model predictions, and the mean model
        plot_predictions <- ggplot() +
          geom_line(data = plot_data[plot_data$Type == "True", ], aes(x = inp, y = Value, color = "True Form"), size = 1) +
          geom_line(data = predictions_long, aes(x = inp, y = Value, group = Model, color = "Predictions"), size = 0.5) +
          geom_line(data = plot_data[plot_data$Type == "Mean_Model", ], aes(x = inp, y = Value, color = "Average Prediction"), size = 1.5) +
          labs(
            title = "Lasso Regression, Lambda = 0",
            x = "x",
            y = "y",
            color = NULL  # Remove the legend title
          ) +
          theme_minimal() +
          theme(
            legend.position = "top",  # Position the legend to the top
            legend.title = element_blank(),  # Remove the legend title
            legend.text = element_text(size = 8),  # Make legend text smaller
            legend.key.size = unit(0.5, "lines")  # Make legend keys smaller
          ) +
          scale_color_manual(values = c("Average Prediction" = "orange", "Predictions" = "pink", "True Form" = "black"))
        
        print(plot_predictions)
      })
      
      
      
      
      output$tab5_plot7 <- renderPlot({
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        # Set lambda to 0 for this plot
        lambda <- 0.04
        
        # Define the polynomial degree for Lasso regression
        complexity <- 5
        
        # Prepare data for Lasso regression (polynomial degree 5)
        X_poly_test <- model.matrix(~ poly(df_testdata$inp, complexity, raw = TRUE), data = df_testdata)[, -1]  # Test design matrix
        
        # Create a data frame to store true values and predictions
        plot_data <- data.frame(
          inp = rep(df_testdata$inp, 9),  # True form + 7 models + mean model
          Value = c(df_testdata$true_form, rep(NA, 8 * nrow(df_testdata))),
          Type = rep(c("True", rep("Model", 7), "Mean_Model"), each = nrow(df_testdata))
        )
        
        # Initialize a matrix to store predictions
        predictions <- matrix(nrow = nrow(df_testdata), ncol = 7)
        
        for (i in 1:7) {
          # Define the response column
          response_col <- paste0("response", i)
          
          # Get the training response variable for this sample
          y <- df_data[[response_col]]
          
          # Prepare the training design matrix for this sample
          X_poly_train <- model.matrix(~ poly(df_data$inp, complexity, raw = TRUE), data = df_data)[, -1]  # Training design matrix
          
          # Fit Lasso model with lambda = 0
          lasso_model <- glmnet(X_poly_train, y, alpha = 1, lambda = lambda)
          
          # Predict using the Lasso model
          predictions[, i] <- predict(lasso_model, newx = X_poly_test)[, 1]
        }
        
        # Convert predictions to long format for plotting
        predictions_long <- data.frame(
          inp = rep(df_testdata$inp, 7),
          Value = as.vector(predictions[, 1:7]),
          Model = rep(paste0("Model", 1:7), each = nrow(df_testdata))
        )
        
        # Calculate the mean of the predictions
        mean_predictions <- rowMeans(predictions, na.rm = TRUE)
        
        # Update plot_data with mean predictions
        plot_data$Value[plot_data$Type == "Mean_Model"] <- mean_predictions
        
        # Plot true form, Lasso model predictions, and the mean model
        plot_predictions <- ggplot() +
          geom_line(data = plot_data[plot_data$Type == "True", ], aes(x = inp, y = Value, color = "True Form"), size = 1) +
          geom_line(data = predictions_long, aes(x = inp, y = Value, group = Model, color = "Predictions"), size = 0.5) +
          geom_line(data = plot_data[plot_data$Type == "Mean_Model", ], aes(x = inp, y = Value, color = "Average Prediction"), size = 1.5) +
          labs(
            title = "Lasso Regression, Lambda = 0.04",
            x = "x",
            y = "y",
            color = NULL  # Remove the legend title
          ) +
          theme_minimal() +
          theme(
            legend.position = "top",  # Position the legend to the top
            legend.title = element_blank(),  # Remove the legend title
            legend.text = element_text(size = 8),  # Make legend text smaller
            legend.key.size = unit(0.5, "lines")  # Make legend keys smaller
          ) +
          scale_color_manual(values = c("Average Prediction" = "orange", "Predictions" = "pink", "True Form" = "black"))
        
        print(plot_predictions)
      })
      
      
      output$tab5_plot8 <- renderPlot({
        # Get the current value of reactive data frame df
        df_data <- df()$toy_data
        df_testdata <- df()$test_data
        
        # Set lambda to 0 for this plot
        lambda <- 0.08
        
        # Define the polynomial degree for Lasso regression
        complexity <- 5
        
        # Prepare data for Lasso regression (polynomial degree 5)
        X_poly_test <- model.matrix(~ poly(df_testdata$inp, complexity, raw = TRUE), data = df_testdata)[, -1]  # Test design matrix
        
        # Create a data frame to store true values and predictions
        plot_data <- data.frame(
          inp = rep(df_testdata$inp, 9),  # True form + 7 models + mean model
          Value = c(df_testdata$true_form, rep(NA, 8 * nrow(df_testdata))),
          Type = rep(c("True", rep("Model", 7), "Mean_Model"), each = nrow(df_testdata))
        )
        
        # Initialize a matrix to store predictions
        predictions <- matrix(nrow = nrow(df_testdata), ncol = 7)
        
        for (i in 1:7) {
          # Define the response column
          response_col <- paste0("response", i)
          
          # Get the training response variable for this sample
          y <- df_data[[response_col]]
          
          # Prepare the training design matrix for this sample
          X_poly_train <- model.matrix(~ poly(df_data$inp, complexity, raw = TRUE), data = df_data)[, -1]  # Training design matrix
          
          # Fit Lasso model with lambda = 0
          lasso_model <- glmnet(X_poly_train, y, alpha = 1, lambda = lambda)
          
          # Predict using the Lasso model
          predictions[, i] <- predict(lasso_model, newx = X_poly_test)[, 1]
        }
        
        # Convert predictions to long format for plotting
        predictions_long <- data.frame(
          inp = rep(df_testdata$inp, 7),
          Value = as.vector(predictions[, 1:7]),
          Model = rep(paste0("Model", 1:7), each = nrow(df_testdata))
        )
        
        # Calculate the mean of the predictions
        mean_predictions <- rowMeans(predictions, na.rm = TRUE)
        
        # Update plot_data with mean predictions
        plot_data$Value[plot_data$Type == "Mean_Model"] <- mean_predictions
        
        # Plot true form, Lasso model predictions, and the mean model
        plot_predictions <- ggplot() +
          geom_line(data = plot_data[plot_data$Type == "True", ], aes(x = inp, y = Value, color = "True Form"), size = 1) +
          geom_line(data = predictions_long, aes(x = inp, y = Value, group = Model, color = "Predictions"), size = 0.5) +
          geom_line(data = plot_data[plot_data$Type == "Mean_Model", ], aes(x = inp, y = Value, color = "Average Prediction"), size = 1.5) +
          labs(
            title = "Lasso Regression, Lambda = 0.08",
            x = "x",
            y = "y",
            color = NULL  # Remove the legend title
          ) +
          theme_minimal() +
          theme(
            legend.position = "top",  # Position the legend to the top
            legend.title = element_blank(),  # Remove the legend title
            legend.text = element_text(size = 8),  # Make legend text smaller
            legend.key.size = unit(0.5, "lines")  # Make legend keys smaller
          ) +
          scale_color_manual(values = c("Average Prediction" = "orange", "Predictions" = "pink", "True Form" = "black"))
        
        print(plot_predictions)
      })
      #####
      #
      #
      #
      #
      #
      #
      #
      
      
      
      
      
    } else if (input$app_choice == "Classification") {
      # Logic for Classification UI
      output$app_ui <- renderUI({
        tagList(
          sliderInput("num_ob", "Number of observations",
                      min = 100, max = 200, value = 100, step = 50),
          
          sliderInput("epsilon", "Variability",
                      min = 0.2, max = 0.8, value = 0.4, step = 0.2)
        )
      })
      
      output$app_main <- renderUI({
        tabsetPanel(
          id = "tabs",
          tabPanel("KNN",
                   sliderInput("k_value", "K-value", 
                               min = 1, max = 15, value = 5, step = 1),
                   fluidRow(
                     column(5, plotOutput("tab11_plot1")),
                     column(5, plotOutput("tab11_plot2"))
                   )
          ),
          tabPanel("Decision Tree",
                   sliderInput("depth", "Tree-depth", 
                               min = 2, max = 6, value = 3, step = 1),
                   fluidRow(
                     column(5, plotOutput("tab22_plot1")),
                     column(5, plotOutput("tab22_plot3"))
                   ),
                   fluidRow(
                     column(10, plotOutput("tab22_plot2"))
                   )
          ),
          tabPanel("Logistic Regression",
                   
                   fluidRow(
                     column(7, plotOutput("tab31_plot1"))
                   )
          ),
          tabPanel("Neural Network",
                   sliderInput("node", "Number of Nodes in Hidden Layer", 
                               min = 1, max = 10, value = 5, step = 1),
                   fluidRow(
                     column(5, plotOutput("tab44_plot1")),
                     column(5, plotOutput("tab44_plot2"))
                   )
          ),
          tabPanel("Support Vector Machine (SVM)",
                   sliderInput("c_param", "Regularization Parameter (C)",
                               min = 0.01, max = 3, value = 1, step = 0.01),
                   
                   fluidRow(
                     column(7, plotOutput("tab51_plot1"))
                   )
          )
        )
      })
      
      # Add Classification server logic here
      # ...
      
      library(mvtnorm)
      library(nnet)
      
      # Reactive data generation
      df <- reactive({
        set.seed(123)  # Set a fixed seed for reproducibility
        
        num_training_points <-  input$num_ob
        num_test_points <- 500
        noise <- input$epsilon
        num_rep_sets <- 50
        cost <- input$cost_value
        
        
        
        # Generating bivariate normal meta-means and 10 component means for each class
        mu_class0 <- c(1, -1)
        mu_class1 <- c(-1, 1)
        cov_mat <- matrix(c(1,0,0,1), nrow=2, ncol=2)
        means_class0 <- rmvnorm(n = 10, mean = mu_class0, sigma = cov_mat)
        means_class1 <- rmvnorm(n = 10, mean = mu_class1, sigma = cov_mat)
        
        # Generating test data
        test_obs_class0 <- matrix(nrow=num_test_points/2, ncol=2)
        test_obs_class1 <- matrix(nrow=num_test_points/2, ncol=2)
        for(i in 1:(num_test_points/2)) {
          s <- sample(1:10, size = 1, prob = rep(1/10, 10))
          test_obs_class0[i,] <- rmvnorm(n = 1, mean = means_class0[s,],
                                         sigma = matrix(c(noise,0,0,noise), nrow=2, ncol=2))
          
          s <- sample(1:10, size = 1, prob = rep(1/10, 10))
          test_obs_class1[i,] <- rmvnorm(n = 1, mean = means_class1[s,],
                                         sigma = matrix(c(noise,0,0,noise), nrow=2, ncol=2))
        }
        test_data <- data.frame(rbind(test_obs_class0, test_obs_class1), 
                                y_orig = factor(rep(c("0", "1"), each=num_test_points/2), 
                                                levels = c("0", "1")))
        names(test_data) <- c("x1", "x2", "y_orig")
        set.seed(321)  # Set a fixed seed for reproducibility
        
        # Generating replicated training datasets
        x1_matrix <- matrix(nrow = num_training_points, ncol = num_rep_sets)
        x2_matrix <- matrix(nrow = num_training_points, ncol = num_rep_sets)
        y_matrix <- matrix(nrow = num_training_points, ncol = num_rep_sets)
        
        for(j in 1:num_rep_sets) {
          rep_training_obs_class0 <- matrix(nrow=num_training_points/2, ncol=2)
          rep_training_obs_class1 <- matrix(nrow=num_training_points/2, ncol=2)
          for(i in 1:(num_training_points/2)) {
            s <- sample(1:10, size = 1, prob = rep(1/10, 10))
            rep_training_obs_class0[i,] <- rmvnorm(n = 1, mean = means_class0[s,],
                                                   sigma = matrix(c(noise,0,0,noise), nrow=2, ncol=2))
            
            s <- sample(1:10, size = 1, prob = rep(1/10, 10))
            rep_training_obs_class1[i,] <- rmvnorm(n = 1, mean = means_class1[s,],
                                                   sigma = matrix(c(noise,0,0,noise), nrow=2, ncol=2))
          }
          x1_matrix[, j] <- c(rep_training_obs_class0[,1], rep_training_obs_class1[,1])
          x2_matrix[, j] <- c(rep_training_obs_class0[,2], rep_training_obs_class1[,2])
          y_matrix[, j] <- rep(c("0", "1"), each=num_training_points/2)
        }
        
        list(
          training_data = list(x1 = x1_matrix, x2 = x2_matrix, y = y_matrix),
          test_data = test_data
        )
      })
      
      
      
      output$tab11_plot2 <- renderPlot({
        data_list <- df()
        test_data <- data_list$test_data
        x_test <- test_data[, 1:2]
        y_test <- test_data$y_orig
        
        # Range of k values from 5 to 33 with an increment of 4
        k_values <- seq(5, 33, by = 4)
        avg_test_errors <- numeric(length(k_values))
        avg_train_errors <- numeric(length(k_values))
        
        # Loop through each k value
        for (k in k_values) {
          test_accuracies <- numeric(ncol(data_list$training_data$x1))
          train_accuracies <- numeric(ncol(data_list$training_data$x1))
          
          # Loop through each replicated training set
          for (i in 1:ncol(data_list$training_data$x1)) {
            x_train <- cbind(data_list$training_data$x1[, i], data_list$training_data$x2[, i])
            y_train <- data_list$training_data$y[, i]
            
            # Fit KNN model and predict on test data
            knn_test_pred <- knn(train = x_train, test = x_test, cl = y_train, k = k)
            knn_train_pred <- knn(train = x_train, test = x_train, cl = y_train, k = k)
            
            # Calculate test and training accuracies
            test_accuracies[i] <- mean(knn_test_pred == y_test)
            train_accuracies[i] <- mean(knn_train_pred == y_train)
          }
          
          # Compute average accuracy and error rate for current k value
          avg_test_accuracy <- mean(test_accuracies)
          avg_train_accuracy <- mean(train_accuracies)
          avg_test_error <- 1 - avg_test_accuracy
          avg_train_error <- 1 - avg_train_accuracy
          
          avg_test_errors[which(k_values == k)] <- avg_test_error
          avg_train_errors[which(k_values == k)] <- avg_train_error
        }
        
        # Create data frame for plotting
        error_df <- data.frame(
          k_value = rep(k_values, 2),
          avg_error = c(avg_test_errors, avg_train_errors),
          error_type = rep(c("Test Error", "Training Error"), each = length(k_values))
        )
        
        # Plot average error vs k value using a line graph
        ggplot(error_df, aes(x = k_value, y = avg_error, color = error_type)) +
          geom_line(size = 1) +  # Line graph for both error types
          geom_point(size = 2) +  # Points for both error types
          labs(title = "Test and Training Error vs K-value",
               x = "K-value",
               y = "Error Rate") +
          theme_minimal() +
          scale_color_manual(values = c("blue", "red"))+  
          theme(legend.position = "top", legend.title = element_blank())  # Remove the legend title
      })
      
      
      
      
      output$tab11_plot1 <- renderPlot({
        data_list <- df()
        test_data <- data_list$test_data
        x_test <- test_data[, 1:2]
        y_test <- test_data$y_orig
        
        # Fixed K value
        k_fixed <- input$k_value
        
        # Get the first replicated training set
        x_train <- cbind(data_list$training_data$x1[, 1], data_list$training_data$x2[, 1])
        y_train <- data_list$training_data$y[, 1]
        
        # Generate a grid of values for x1 and x2
        x1_range <- seq(min(c(x_train[, 1], x_test[, 1])) - 1, max(c(x_train[, 1], x_test[, 1])) + 1, length.out = 100)
        x2_range <- seq(min(c(x_train[, 2], x_test[, 2])) - 1, max(c(x_train[, 2], x_test[, 2])) + 1, length.out = 100)
        grid <- expand.grid(x1 = x1_range, x2 = x2_range)
        
        # Predict on the grid
        knn_pred_grid <- knn(train = x_train, test = grid, cl = y_train, k = k_fixed)
        
        # Convert grid predictions to a matrix for plotting
        grid$prediction <- as.factor(knn_pred_grid)
        
        # Plot
        ggplot() +
          geom_tile(data = grid, aes(x = x1, y = x2, fill = prediction), alpha = 0.3) +
          geom_point(data = data.frame(x1 = x_train[, 1], x2 = x_train[, 2], y_orig = y_train),
                     aes(x = x1, y = x2, color = y_orig), size = 2) +
          labs(title = paste("Decision Boundary with K =", k_fixed),
               x = "X1", y = "X2") +
          scale_fill_manual(values = c("blue", "red"), name = "Prediction") +
          scale_color_manual(values = c("blue", "red"), name = "Actual") +
          theme_minimal() +  
          theme(legend.position = "top")
      })
      
      
      #############
      output$tab22_plot1 <- renderPlot({
        data_list <- df()
        test_data <- data_list$test_data
        x_test <- test_data[, 1:2]
        y_test <- test_data$y_orig
        
        # Get the first replicated training set
        x_train <- cbind(data_list$training_data$x1[, 1], data_list$training_data$x2[, 1])
        y_train <- data_list$training_data$y[, 1]
        
        # Generate a grid of values for x1 and x2
        x1_range <- seq(min(c(x_train[, 1], x_test[, 1])) - 1, max(c(x_train[, 1], x_test[, 1])) + 1, length.out = 100)
        x2_range <- seq(min(c(x_train[, 2], x_test[, 2])) - 1, max(c(x_train[, 2], x_test[, 2])) + 1, length.out = 100)
        grid <- expand.grid(x1 = x1_range, x2 = x2_range)
        
        # Create a data frame for the current training set
        train_data <- data.frame(x1 = x_train[, 1], x2 = x_train[, 2], y = y_train)
        
        # Fit the decision tree model with a fixed depth
        tree_model <- rpart(
          y ~ x1 + x2,
          data = train_data,
          control = rpart.control(maxdepth = input$depth, minbucket = 1, cp = 0, xval = 0)
        )
        
        # Predict on the grid
        tree_pred_grid <- predict(tree_model, newdata = grid, type = "class")
        
        # Convert grid predictions to a factor for plotting
        grid$prediction <- as.factor(tree_pred_grid)
        
        # Plot
        ggplot() +
          geom_tile(data = grid, aes(x = x1, y = x2, fill = prediction), alpha = 0.3) +
          geom_point(data = data.frame(x1 = x_train[, 1], x2 = x_train[, 2], y_orig = y_train),
                     aes(x = x1, y = x2, color = y_orig), size = 2) +
          labs(title = paste("Decision Boundary with Tree Depth =", input$depth),
               x = "X1", y = "X2") +
          scale_fill_manual(values = c("blue", "red"), name = "Prediction") +
          scale_color_manual(values = c("blue", "red"), name = "Actual") +
          theme_minimal() +  
          theme(legend.position = "top")
      })
      
      
      
      output$tab22_plot2 <- renderPlot({
        
        
        # Get the reactive data frame
        data_list <- df()
        test_data <- data_list$test_data
        x_test <- test_data[, 1:2]
        y_test <- test_data$y_orig
        
        # Get depth from input
        depth <- input$depth
        minbucket_value <- 1  # A small number for minbucket
        
        # Get the first replicated training set
        x_train <- cbind(data_list$training_data$x1[, 1], data_list$training_data$x2[, 1])
        y_train <- data_list$training_data$y[, 1]
        
        # Create a data frame for training
        train_data <- data.frame(x1 = x_train[, 1], x2 = x_train[, 2], y = y_train)
        
        # Fit a decision tree model with the specified depth
        tree_model <- rpart(
          formula = y ~ x1 + x2,       # Target variable y and predictors x1, x2
          data = train_data,
          control = rpart.control(maxdepth = depth, minbucket = minbucket_value, cp = 0, xval = 0), # No pruning, no cross-validation
          method = "class"             # For classification
        )
        
        
        # Plot the decision tree
        rpart.plot(tree_model, main = paste("Decision Tree with Depth =", depth))
      })
      
      
      output$tab22_plot3 <- renderPlot({
        
        data_list <- df()
        test_data <- data_list$test_data
        x_test <- test_data[, 1:2]
        y_test <- test_data$y_orig
        
        # Range of depth values (you can adjust this range as needed)
        depth_values <- seq(2, 6, by = 1)
        avg_test_errors <- numeric(length(depth_values))
        avg_train_errors <- numeric(length(depth_values))
        
        # Loop through each depth value
        for (depth in depth_values) {
          test_accuracies <- numeric(ncol(data_list$training_data$x1))
          train_accuracies <- numeric(ncol(data_list$training_data$x1))
          
          # Loop through each replicated training set
          for (i in 1:ncol(data_list$training_data$x1)) {
            x_train <- cbind(data_list$training_data$x1[, i], data_list$training_data$x2[, i])
            y_train <- data_list$training_data$y[, i]
            
            # Create a data frame for the current training set
            train_data <- data.frame(x1 = x_train[, 1], x2 = x_train[, 2], y = y_train)
            
            # Fit decision tree model with controlled depth
            tree_model <- rpart(
              y ~ x1 + x2,
              data = train_data,
              control = rpart.control(maxdepth = depth, minbucket = 1, cp = 0, xval = 0)
            )
            
            # Predict on test data
            tree_test_pred <- predict(tree_model, newdata = data.frame(x1 = x_test[, 1], x2 = x_test[, 2]), type = "class")
            test_accuracies[i] <- mean(tree_test_pred == y_test)
            
            # Predict on training data
            tree_train_pred <- predict(tree_model, newdata = train_data, type = "class")
            train_accuracies[i] <- mean(tree_train_pred == y_train)
          }
          
          # Compute average accuracy and error rate for current depth value
          avg_test_accuracy <- mean(test_accuracies)
          avg_train_accuracy <- mean(train_accuracies)
          avg_test_error <- 1 - avg_test_accuracy
          avg_train_error <- 1 - avg_train_accuracy
          
          avg_test_errors[which(depth_values == depth)] <- avg_test_error
          avg_train_errors[which(depth_values == depth)] <- avg_train_error
        }
        
        # Create data frame for plotting
        error_df <- data.frame(
          depth = rep(depth_values, 2),
          avg_error = c(avg_test_errors, avg_train_errors),
          error_type = rep(c("Test Error", "Training Error"), each = length(depth_values))
        )
        
        # Plot average error vs depth using a line graph
        ggplot(error_df, aes(x = depth, y = avg_error, color = error_type)) +
          geom_line(size = 1) +  # Line graph for both error types
          geom_point(size = 2) +  # Points for both error types
          labs(title = "Test and Training Error vs Tree Depth",
               x = "Tree Depth",
               y = "Error Rate") +
          theme_minimal() +
          scale_color_manual(values = c("blue", "red")) +  
          theme(legend.position = "top", legend.title = element_blank())  # Remove the legend title
        
      })
      
      output$tab31_plot1 <- renderPlot({
        data_list <- df()
        test_data <- data_list$test_data
        x_test <- test_data[, 1:2]
        y_test <- test_data$y_orig
        
        # Get the first replicated training set
        x_train <- cbind(data_list$training_data$x1[, 1], data_list$training_data$x2[, 1])
        y_train <- as.numeric(as.character(data_list$training_data$y[, 1]))  # Ensure y is numeric (0 and 1)
        
        # Generate a grid of values for x1 and x2
        x1_range <- seq(min(c(x_train[, 1], x_test[, 1])) - 1, max(c(x_train[, 1], x_test[, 1])) + 1, length.out = 100)
        x2_range <- seq(min(c(x_train[, 2], x_test[, 2])) - 1, max(c(x_train[, 2], x_test[, 2])) + 1, length.out = 100)
        grid <- expand.grid(x1 = x1_range, x2 = x2_range)
        
        # Create a data frame for the current training set
        train_data <- data.frame(x1 = x_train[, 1], x2 = x_train[, 2], y = y_train)
        
        # Fit the logistic regression model
        logit_model <- glm(y ~ x1 + x2, data = train_data, family = binomial)
        
        # Predict on the grid
        grid$pred <- predict(logit_model, newdata = grid, type = "response")
        
        # Convert predictions to class labels for plotting
        grid$prediction <- as.factor(ifelse(grid$pred > 0.5, "1", "0"))
        
        # Plot
        ggplot() +
          geom_tile(data = grid, aes(x = x1, y = x2, fill = prediction), alpha = 0.3) +
          geom_point(data = data.frame(x1 = x_train[, 1], x2 = x_train[, 2], y_orig = as.factor(y_train)),
                     aes(x = x1, y = x2, color = y_orig), size = 2) +
          labs(title = "Logistic Regression Decision Boundary",
               x = "X1", y = "X2") +
          scale_fill_manual(values = c("blue", "red"), name = "Prediction") +
          scale_color_manual(values = c("blue", "red"), name = "Actual") +
          theme_minimal() +  
          theme(legend.position = "top")
      })
      
      
      
      output$tab44_plot1 <- renderPlot({
        data_list <- df()
        train_data <- data_list$training_data
        
        # Selecting the first replicated training set for simplicity
        x_train <- data.frame(x1 = train_data$x1[,1], x2 = train_data$x2[,1], y = train_data$y[,1])
        x_train$y <- as.numeric(as.character(x_train$y))  # Ensure y is numeric (0 and 1)
        
        # Generate a grid of values for x1 and x2
        x1_range <- seq(min(x_train$x1) - 1, max(x_train$x1) + 1, length.out = 100)
        x2_range <- seq(min(x_train$x2) - 1, max(x_train$x2) + 1, length.out = 100)
        grid <- expand.grid(x1 = x1_range, x2 = x2_range)
        
        model <- nnet(y ~ x1 + x2, data = x_train, size = input$node, 
                      linout = FALSE, decay = 0.01, maxit = 1000)
        
        # Predict on the grid
        grid$prob <- predict(model, newdata = grid)
        
        # Convert predictions to class labels for plotting
        grid$prediction <- as.factor(ifelse(grid$prob > 0.5, "1", "0"))
        
        # Plot
        ggplot() +
          geom_tile(data = grid, aes(x = x1, y = x2, fill = prediction), alpha = 0.3) +
          geom_point(data = x_train, aes(x = x1, y = x2, color = as.factor(y)), size = 2) +
          labs(title = "Neural Network Decision Boundary",
               x = "X1", y = "X2") +
          scale_fill_manual(values = c("blue", "red"), name = "Prediction") +
          scale_color_manual(values = c("blue", "red"), name = "Actual") +
          theme_minimal() +  
          theme(legend.position = "top")
      })
      
      output$tab44_plot2 <- renderPlot({
        data_list <- df()
        test_data <- data_list$test_data
        x_test <- test_data[, c("x1", "x2")]
        y_test <- test_data$y_orig
        
        # Range of number of nodes
        node_values <- seq(1, 10, by = 1)
        avg_test_errors <- numeric(length(node_values))
        avg_train_errors <- numeric(length(node_values))
        
        # Loop through each number of nodes
        for (nodes in node_values) {
          test_errors <- numeric(ncol(data_list$training_data$x1))
          train_errors <- numeric(ncol(data_list$training_data$x1))
          
          # Loop through each replicated training set
          for (i in 1:ncol(data_list$training_data$x1)) {
            # Extract training data for the current replication
            x_train <- data.frame(x1 = data_list$training_data$x1[, i], x2 = data_list$training_data$x2[, i])
            y_train <- factor(data_list$training_data$y[, i])  # Ensure y_train is a factor
            
            # Fit the neural network model using the 'nnet' package
            model <- nnet(y_train ~ x1 + x2, data = x_train, size = nodes, 
                          linout = FALSE, decay = 0.01, maxit = 1000)
            
            # Predict on test data
            grid <- data.frame(x1 = x_test[, "x1"], x2 = x_test[, "x2"])
            test_pred_prob <- predict(model, newdata = grid, type = "raw")
            test_pred <- ifelse(test_pred_prob > 0.5, "1", "0")
            test_errors[i] <- mean(test_pred != y_test, na.rm = TRUE)
            
            # Predict on training data
            train_pred_prob <- predict(model, newdata = x_train, type = "raw")
            train_pred <- ifelse(train_pred_prob > 0.5, "1", "0")
            train_errors[i] <- mean(train_pred != y_train, na.rm = TRUE)
          }
          
          # Compute average errors for current number of nodes
          avg_test_errors[which(node_values == nodes)] <- mean(test_errors, na.rm = TRUE)
          avg_train_errors[which(node_values == nodes)] <- mean(train_errors, na.rm = TRUE)
        }
        
        # Create data frame for plotting
        error_df <- data.frame(
          nodes = rep(node_values, 2),
          avg_error = c(avg_test_errors, avg_train_errors),
          error_type = rep(c("Test Error", "Training Error"), each = length(node_values))
        )
        
        # Plot average error vs number of nodes using a line graph
        ggplot(error_df, aes(x = nodes, y = avg_error, color = error_type)) +
          geom_line(size = 1) +  # Line graph for both error types
          geom_point(size = 2) +  # Points for both error types
          labs(title = "Test and Training Error vs Number of Nodes",
               x = "Number of Nodes",
               y = "Error Rate") +
          theme_minimal() +
          scale_color_manual(values = c("blue", "red")) +
          theme(legend.position = "top", legend.title = element_blank())  # Remove the legend title
      })
      
      library(ggplot2)
      library(e1071)
      
      output$tab51_plot1 <- renderPlot({
        data_list <- df()
        test_data <- data_list$test_data
        x_test <- test_data[, 1:2]
        y_test <- test_data$y_orig
        
        # Get the first replicated training set
        x_train <- cbind(data_list$training_data$x1[, 1], data_list$training_data$x2[, 1])
        y_train <- as.factor(data_list$training_data$y[, 1])
        
        # Set a default cost value if input$c_param is NULL
        c_param <- ifelse(is.null(input$c_param), 1, input$c_param)
        
        # Fit the SVM model with a linear kernel and specified cost
        svm_model <- svm(y_train ~ x1 + x2, data = data.frame(x1 = x_train[, 1], x2 = x_train[, 2], y_train),
                         kernel = "linear", cost = c_param, scale = FALSE)
        
        # Generate a grid of values for x1 and x2
        x1_range <- seq(min(c(x_train[, 1], x_test[, 1])) - 1, max(c(x_train[, 1], x_test[, 1])) + 1, length.out = 100)
        x2_range <- seq(min(c(x_train[, 2], x_test[, 2])) - 1, max(c(x_train[, 2], x_test[, 2])) + 1, length.out = 100)
        grid <- expand.grid(x1 = x1_range, x2 = x2_range)
        
        # Predict on the grid
        grid$decision_boundary <- predict(svm_model, grid, decision.values = TRUE)
        grid$decision_boundary <- attr(grid$decision_boundary, "decision.values")
        
        # Plot decision boundary with margins and fill
        ggplot() +
          geom_tile(data = grid, aes(x = x1, y = x2, fill = decision_boundary > 0), alpha = 0.4) +
          geom_contour(data = grid, aes(x = x1, y = x2, z = decision_boundary), breaks = 0, color = "black", size = 1.5) +
          geom_contour(data = grid, aes(x = x1, y = x2, z = decision_boundary), breaks = c(-1, 1), linetype = "dashed", color = "black", size = 0.7) +
          geom_point(data = data.frame(x1 = x_train[, 1], x2 = x_train[, 2], y_orig = y_train),
                     aes(x = x1, y = x2, color = y_orig), size = 2) +
          geom_point(data = data.frame(x1 = x_train[svm_model$index, 1], x2 = x_train[svm_model$index, 2]), 
                     aes(x = x1, y = x2), shape = 21, size = 3, fill = "black") +
          labs(title = paste("SVM Decision Boundary (Linear Kernel) with C =", c_param),
               x = "X1", y = "X2") +
          scale_fill_manual(values = c("lightblue", "lightpink"), name = "Prediction", labels = c("FALSE", "TRUE")) +
          scale_color_manual(values = c("blue", "red"), name = "Actual") +
          theme_minimal() +
          theme(legend.position = "top")
      })
      
      
      
      
      
      
    }
  })
}

shinyApp(ui = ui, server = server)