# importing training and testing data
train_cbb <- read.csv("C:\\Users\\camer\\OneDrive\\Documents\\WPI School\\2022-2023\\Spring 23\\DS 501\\HW6\\train_cbb.csv")
test_cbb <- read.csv("C:\\Users\\camer\\OneDrive\\Documents\\WPI School\\2022-2023\\Spring 23\\DS 501\\HW6\\test_cbb.csv")

class(train_cbb)

# log regression training data
log_reg_train <- function(x_value1, x_value2 = NULL, x_value3 = NULL){
  if(missing(x_value2)) {
    formula <- MakeTournament ~ x_value1
  } else if(missing(x_value3)) {
    formula <- MakeTournament ~ x_value1 + x_value2
  } else {
    formula <- MakeTournament ~ x_value1 + x_value2 + x_value3
  }
  logistic_model_student <- glm(formula = formula, data = train_cbb,
                                family = binomial())
  train_cbb$logistic_predictions <- predict(logistic_model_student, type = "response")
  train_cbb$prediction <- ifelse(train_cbb$logistic_predictions < .50, 0, 1)
  accuracy <- accuracy(train_cbb$MakeTournament, train_cbb$prediction )
  return(accuracy) 
}          

log_reg_train(train_cbb$Games, train_cbb$Adjusted_Offensive_Efficiency)


# log regression testing data
log_reg_test <- function(x_value1, x_value2 = NULL, x_value3 = NULL){
  if(missing(x_value2)) {
    formula <- MakeTournament ~ x_value1
  } else if(missing(x_value3)) {
    formula <- MakeTournament ~ x_value1 + x_value2
  } else {
    formula <- MakeTournament ~ x_value1 + x_value2 + x_value3
  }
  logistic_model_student <- glm(formula = formula, data = test_cbb,
                                family = binomial())
  test_cbb$logistic_predictions <- predict(logistic_model_student, type = "response")
  test_cbb$prediction <- ifelse(test_cbb$logistic_predictions < .50, 0, 1)
  accuracy <- accuracy(test_cbb$MakeTournament, test_cbb$prediction )
  
  return(accuracy)
}

log_reg_test(test_cbb$Wins)

# train data create figure 2
fig_log_reg_train <- function(x_value1, x_value2 = NULL, x_value3 = NULL){
  library(ggplot2)
  library(dplyr)
  if(missing(x_value2)) {
    formula1 <- MakeTournament ~ x_value1
    
    simple_logistic_model <- glm(data = train_cbb,
                                 formula = formula1,
                                 family = binomial())
    train_cbb$logistic_predictions <- predict(simple_logistic_model, type = "response")
    fig <- train_cbb %>%
      ggplot(aes(x = x_value1,y = MakeTournament)) +
      geom_point() +
      labs(y = "Make the Tournament") +
      theme_minimal()
  } 
  else if(missing(x_value3)) {
    formula2 <- MakeTournament ~ x_value1 + x_value2
    logistic_model_student <- glm(formula = formula2, data = train_cbb,
                                  family = binomial())
    train_cbb$logistic_predictions <- predict(logistic_model_student, type = "response")
    train_cbb$prediction_new_for_plot = ifelse(train_cbb$logistic_predictions < .50,
                                               "No", "Yes")
    color_labels <- c("No" = "blue", "Yes" = "green")
    fig <- train_cbb %>%
      ggplot(aes(x = x_value1,y = x_value2, color = prediction_new_for_plot)) +
      geom_point(alpha = .6) +
      scale_color_manual(values = color_labels)+
      labs(y = "x_value2", x = "x_value1", color = "Made the Tournament") +
      theme_minimal()
  } 
  else {
    library(plotly)
    formula3 <- MakeTournament ~ x_value1 + x_value2 + x_value3
    logistic_model_student <- glm(formula = formula3, data = train_cbb,
                                  family = binomial())
    train_cbb$logistic_predictions <- predict(logistic_model_student, type = 
                                                "response")
    train_cbb$prediction_new_for_plot = ifelse(train_cbb$logistic_predictions < .50,"No", "Yes")
    fig <- plot_ly(data = train_cbb, x = x_value1, y = x_value2, z = x_value3,color = ~prediction_new_for_plot, mode = 'markers') %>%
      layout(title ="3D Scatter Plot of College Basketball Teams", legend = list(title=list(text='<b> Made the Tournament </b>')))
    
  }
  return(fig) 
}

# test data create figure 2 
fig_log_reg_test <- function(x_value1, x_value2 = NULL, x_value3 = NULL){
  library(ggplot2)
  library(dplyr)
  if(missing(x_value2)) {
    formula1 <- MakeTournament ~ x_value1
    
    simple_logistic_model <- glm(data = test_cbb,
                                 formula = formula1,
                                 family = binomial())
    test_cbb$logistic_predictions <- predict(simple_logistic_model, type = "response")
    fig <- test_cbb %>%
      ggplot(aes(x = x_value1,y = MakeTournament)) +
      geom_point() +
      labs(y = "Make the Tournament", x = "x_value1") +
      theme_minimal()
  } 
  else if(missing(x_value3)) {
    formula2 <- MakeTournament ~ x_value1 + x_value2
    logistic_model_student <- glm(formula = formula2, data = test_cbb,
                                  family = binomial())
    test_cbb$logistic_predictions <- predict(logistic_model_student, type = "response")
    test_cbb$prediction_new_for_plot = ifelse(test_cbb$logistic_predictions < .50,
                                              "No", "Yes")
    color_labels <- c("No" = "blue", "Yes" = "green")
    fig <- test_cbb %>%
      ggplot(aes(x = x_value1,y = x_value2, color = prediction_new_for_plot)) +
      geom_point(alpha = .6) +
      scale_color_manual(values = color_labels)+
      labs(y = "x_value2", x = "x_value1", color = "Made the Tournament") +
      theme_minimal()
  } 
  else {
    library(plotly)
    formula3 <- MakeTournament ~ x_value1 + x_value2 + x_value3
    logistic_model_student <- glm(formula = formula3, data = test_cbb,
                                  family = binomial())
    test_cbb$logistic_predictions <- predict(logistic_model_student, type = 
                                               "response")
    test_cbb$prediction_new_for_plot = ifelse(test_cbb$logistic_predictions < .50,"No", "Yes")
    fig <- plot_ly(data = test_cbb, x = x_value1, y = x_value2, z = x_value3,color = ~prediction_new_for_plot, mode = 'markers') %>%
      layout(title ="3D Scatter Plot of College Basketball Teams", legend = list(title=list(text='<b> Made the Tournament </b>')))
    
  }
  return(fig) 
}

