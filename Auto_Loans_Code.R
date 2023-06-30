  # Loading Packages
pacman::p_load(dplyr, forcats, janitor, lubridate, mosaic, purrr,
               tidyverse, ggplot2, ggpubr, lubridate, broom, caret, Hmisc, reshape2)


  # Loading Data
Internship_Project_AUTOLOANS_05182023 <- read_csv("Internship Project_AUTOLOANS_05182023.csv")
 

  # Creating age variable and shortening df title
AUTOLOANS <- Internship_Project_AUTOLOANS_05182023 


  # Descriptive statistics
summary(AUTOLOANS)
Summarytable <-summary(AUTOLOANS)
Summarytable
View(Summarytable)

  ## Cleaning Data
    # Removing Spaces
    colnames(AUTOLOANS) <- gsub(" ", "", colnames(AUTOLOANS))
    colnames(AUTOLOANS)

    # Removing "pct_single_returns_20"
    AUTOLOANS <- subset(AUTOLOANS, select = -pct_single_returns_20)

    # Removing count row (8016)
      ### Either one works ###
    AUTOLOANS <- AUTOLOANS[!is.na(AUTOLOANS$`LiquidationRatio%`),]
    AUTOLOANS <- AUTOLOANS[AUTOLOANS$`LiquidationRatio%` != 8016,]
    
    # Replacing negatives with zeroes
    AUTOLOANS$`TotalPaid`[AUTOLOANS$`TotalPaid` < 0] <- 0
    
    AUTOLOANS$`LiquidationRatio%`[AUTOLOANS$`LiquidationRatio%` < 0] <- 0
    
    # Removing zeroes from "collateral_year_model"
    AUTOLOANS$collateral_year_model[AUTOLOANS$collateral_year_model == 0] <- NA
    
    # Creating age variable
    AUTOLOANS <- AUTOLOANS %>%
      mutate(model_age = 2023 - collateral_year_model)
    
    # Filling in `NA`s for `Total Paid`
  AUTOLOANS <- AUTOLOANS %>%
      mutate(`TotalPaid` = ifelse(is.na(`TotalPaid`), 
                                   pmax(origination_balance, 0, na.rm = TRUE) -
                                     pmax(account_balance_at_closure, 0, na.rm = TRUE), 
                                   `TotalPaid`))
    
    # cLEARING UP THE NEGATIVES ATTAINED
    AUTOLOANS$`TotalPaid`[AUTOLOANS$`TotalPaid` < 0] <- 0
    
    
  # Running descriptive statistics again
summary(AUTOLOANS)
Summarytable <-summary(AUTOLOANS)
View(Summarytable)
    
  ## Time between purchase date and last payment
    AUTOLOANS <- AUTOLOANS %>%
      mutate(
        `PurchaseDate` = mdy(`PurchaseDate`),
        Last_PMT_DATE = mdy(Last_PMT_DATE),
        time_paying = as.numeric(Last_PMT_DATE - `PurchaseDate`)
      )

  ## Ratio of capital sunk
    AUTOLOANS <- AUTOLOANS %>%
      mutate(capital_sunkratio = (`TotalPaid` + coalesce(down_payment, 0)) / origination_balance)
    
    AUTOLOANS <- AUTOLOANS %>%
      mutate(capital_leftratio = 1 - capital_sunkratio)
    
  ## Finding top 3 most frequent states and creating binary
    
    # Frequency Table   
    frequencytable <- table(AUTOLOANS$state)
    View(frequencytable)
    
    # Creating Binary Identification for FL,GA,TX
    AUTOLOANS <- AUTOLOANS %>%
      mutate(FL = ifelse(state == "FL", 1, 0))
    AUTOLOANS <- AUTOLOANS %>%
      mutate(GA = ifelse(state == "GA", 1, 0))
    AUTOLOANS <- AUTOLOANS %>%
      mutate(TX = ifelse(state == "TX", 1, 0))

################# Binning (creating categorical bins)

    # Creating Quartiles
    
    # creating a function that gives quartiles
    
x = AUTOLOANS # define x

    # creating a function that gives quartiles
quartilefct <- function(x){
  quartiles <- quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  categories <- cut(x, breaks = quartiles, labels = FALSE, include.lowest = TRUE)
  categories
}

    # for loop that creates new columns from `pct_single_returns_20` onwards
cols <- colnames(AUTOLOANS)[which(colnames(AUTOLOANS) == "pct_joint_returns_20"):ncol(AUTOLOANS)]
for (col in cols) {
  AUTOLOANS <- AUTOLOANS %>% mutate(!!paste0(col, "_quartiles") := as.numeric(quartilefct(as.numeric(!!sym(col)))))
}

 
    # for loop for range: "origination_balance":"LiquidationRatio%"
 
 start_col <- "origination_balance"
 end_col <- "LiquidationRatio%"
 
 cols <- colnames(AUTOLOANS)[which(colnames(AUTOLOANS) == start_col):which(colnames(AUTOLOANS) == end_col)]
 for (col in cols) {
   AUTOLOANS <- AUTOLOANS %>% mutate(!!paste0(col, "_quartiles") := as.numeric(quartilefct(as.numeric(!!sym(col)))))
 }
 
 
 
 cols <- colnames(AUTOLOANS)[which(colnames(AUTOLOANS) == start_col):which(colnames(AUTOLOANS) == end_col)]
 for (col in cols) {
   AUTOLOANS <- AUTOLOANS %>% mutate(!!paste0(col, "_quartiles") := quartilefct(as.numeric(!!sym(col))))
 }
 
     #adding missing variable quartiles
 AUTOLOANS <- AUTOLOANS %>%
   mutate(collateral_mileage_quartiles = quartilefct(collateral_mileage))
 AUTOLOANS <- AUTOLOANS %>%
   mutate(account_balance_at_closure_quartiles = quartilefct(account_balance_at_closure))
 

  
################     *Spearman's test* 
    # Find correlation between variables and Liquidation Ratio
    
     target_variable <- "LiquidationRatio%"
    correlation_table <- data.frame(Variable = character(), Correlation = numeric(), p_value = numeric(), stringsAsFactors = FALSE)
    
    # Loop over each variable in the dataframe
    for (variable in colnames(AUTOLOANS)) {
      if (variable != target_variable && is.numeric(AUTOLOANS[[variable]])) {
        # Perform Spearman's rank correlation test
        cor_test <- cor.test(AUTOLOANS[[variable]], AUTOLOANS[[target_variable]], method = "spearman")
        
        # Extract correlation coefficient and p-value
        correlation <- cor_test$estimate
        p_value <- cor_test$p.value
        
        # Create a new row in the correlation table
        new_row <- data.frame(Variable = variable, Correlation = correlation, p_value = p_value)
        
        # Append the new row to the correlation table
        correlation_table <- rbind(correlation_table, new_row)
      }
    }
    
    # Print the correlation table
    View(correlation_table)
#### >>>> Top 4: "Time Paying", "capital_sunkratio", "TX", "Account Balance at Closure"
    
  
    
################    Binning the Top 5 variables
    
  ### Plotting Total Paid and running ANOVA ###

    # Total Paid (every 2.5k)
    bin_width <- 2500
    breaks <- c(seq(0, 27500, by = bin_width), Inf)
    labels <- c(paste(seq(0, 27500 - bin_width, by = bin_width), seq(bin_width, 27500, by = bin_width), sep = "-"), "13+")
    
    # Adjust the labels vector to match the number of intervals
    labels <- labels[1:(length(breaks) - 1)]
    
    AUTOLOANS$`TotalPaid_2500` <- cut(AUTOLOANS$`TotalPaid`, breaks = breaks, labels = FALSE, right = FALSE)
    AUTOLOANS$TotalPaid_2500 <- as.integer(AUTOLOANS$TotalPaid_2500)
    
    # Plotting bins
    ggplot(AUTOLOANS, aes(
      x =`TotalPaid_2500`, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  

    
    # Total Paid (every 5k)
    bin_width <- 5000
    breaks <- c(seq(0, 25000, by = bin_width), Inf)
    labels <- c(paste(seq(0, 25000 - bin_width, by = bin_width), seq(bin_width, 25000, by = bin_width), sep = "-"), "25000+")
    
    # Adjust the labels vector to match the number of intervals
    labels <- labels[1:(length(breaks) - 1)]
    
    AUTOLOANS$`TotalPaid_5000` <- cut(AUTOLOANS$`TotalPaid`, breaks = breaks, labels = FALSE, right = FALSE)
    AUTOLOANS$TotalPaid_5000 <- as.integer(AUTOLOANS$TotalPaid_5000)
    
    # Plotting bins
    ggplot(AUTOLOANS, aes(
      x =`TotalPaid_5000`, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))      
  
    
    # Total Paid (every 10k)
    bin_width <- 10000
    breaks <- seq(0, 50000, by = bin_width)
    labels <- paste(breaks[-length(breaks)], breaks[-1], sep = "-")
    
    AUTOLOANS$`TotalPaid_10000` <- cut(AUTOLOANS$`TotalPaid`, breaks = breaks, labels = FALSE, right = FALSE)
    AUTOLOANS$TotalPaid_10000 <- as.integer(AUTOLOANS$TotalPaid_10000)
    
    # Plotting bins
    ggplot(AUTOLOANS, aes(x =`TotalPaid_10000`, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))         
    
    
    # Total Paid (quartiles)    
    ggplot(AUTOLOANS, aes(
      x = `TotalPaid_quartiles`, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
  ### Plotting Time Paying (days) and running ANOVA
    # Time Paying (every 100 days)
    bin_width <- 100
    breaks <- c(seq(0, 1200, by = bin_width), Inf)
    labels <- c(paste(seq(0, 1200 - bin_width, by = bin_width), seq(bin_width, 1200, by = bin_width), sep = "-"), "1200+")
    
    # Adjust the labels vector to match the number of intervals
    labels <- labels[1:(length(breaks) - 1)]
    
    AUTOLOANS$time_paying_100 <- cut(AUTOLOANS$time_paying, breaks = breaks, labels = FALSE, right = FALSE)
    AUTOLOANS$time_paying_100 <- as.integer(AUTOLOANS$time_paying_100)
    
    # Plotting bins
    ggplot(AUTOLOANS, aes(
      x =time_paying_100, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  

    
    # Time Paying (every 250 days)
    bin_width <- 250
    breaks <- c(seq(0, 1500, by = bin_width), Inf)
    labels <- c(paste(seq(0, 1500 - bin_width, by = bin_width), seq(bin_width, 1500, by = bin_width), sep = "-"), "1500+")
    
    # Adjust the labels vector to match the number of intervals
    labels <- labels[1:(length(breaks) - 1)]
    
    AUTOLOANS$time_paying_250 <- cut(AUTOLOANS$time_paying, breaks = breaks, labels = FALSE, right = FALSE)
    AUTOLOANS$time_paying_250 <- as.integer(AUTOLOANS$time_paying_250)
    
    # Plotting bins
    ggplot(AUTOLOANS, aes(
      x =time_paying_250, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  

    
    # Time Paying (every 500 days)
    bin_width <- 500
    breaks <- c(seq(0, 2000, by = bin_width), Inf)
    labels <- c(paste(seq(0, 2000 - bin_width, by = bin_width), seq(bin_width, 2000, by = bin_width), sep = "-"), "1200+")
    
    # Adjust the labels vector to match the number of intervals
    labels <- labels[1:(length(breaks) - 1)]
    
    AUTOLOANS$time_paying_500 <- cut(AUTOLOANS$time_paying, breaks = breaks, labels = FALSE, right = FALSE)
    AUTOLOANS$time_paying_500 <- as.integer(AUTOLOANS$time_paying_500)
    
    # Plotting bins
    ggplot(AUTOLOANS, aes(
      x =time_paying_500, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  

    
    # Time Paying (Quartiles)
    ggplot(AUTOLOANS, aes(
      x = time_paying_quartiles, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    
    # Time Paying (Deciles)
    
    # Function to assign decile bins
    decile_bins <- function(x) {
      n <- length(x)
      deciles <- quantile(x, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
      bins <- cut(x, breaks = deciles, labels = 1:10, include.lowest = TRUE)
      as.numeric(as.character(bins))
    }
    
    # Apply decile_bins function to create new column
    AUTOLOANS <- AUTOLOANS %>%
      mutate(time_paying_deciles = decile_bins(time_paying))
    
    ggplot(AUTOLOANS, aes(
      x = time_paying_deciles, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
  ### Plotting Capital Sunk Ratio and running ANOVA
    # Capital Sunk (every 10%)
    bin_width <- 0.1
    breaks <- c(seq(0, 2.7, by = bin_width), Inf)
    labels <- c(paste(seq(0, 2.7 - bin_width, by = bin_width), seq(bin_width, 2.7, by = bin_width), sep = "-"), "2.7+")
    
    # Adjust the labels vector to match the number of intervals
    labels <- labels[1:(length(breaks) - 1)]
    
    AUTOLOANS$capital_sunkratio_0.1 <- cut(AUTOLOANS$capital_sunkratio, breaks = breaks, labels = FALSE, right = FALSE)
    AUTOLOANS$capital_sunkratio_0.1 <- as.integer(AUTOLOANS$capital_sunkratio_0.1)
    
    # Plotting bins
    ggplot(AUTOLOANS, aes(
      x =capital_sunkratio_0.1, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  

    
    # Capital Sunk (every 25%)
    bin_width <- 0.25
    breaks <- c(seq(0, 2.75, by = bin_width), Inf)
    labels <- c(paste(seq(0, 2.75 - bin_width, by = bin_width), seq(bin_width, 2.75, by = bin_width), sep = "-"), "2.75+")
    
    # Adjust the labels vector to match the number of intervals
    labels <- labels[1:(length(breaks) - 1)]
    
    AUTOLOANS$capital_sunkratio_0.25 <- cut(AUTOLOANS$capital_sunkratio, breaks = breaks, labels = FALSE, right = FALSE)
    AUTOLOANS$capital_sunkratio_0.25 <- as.integer(AUTOLOANS$capital_sunkratio_0.25)
    
    # Plotting bins
    ggplot(AUTOLOANS, aes(
      x =capital_sunkratio_0.25, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  

    
    # Capital Sunk (every 50%)
    bin_width <- 0.5
    breaks <- c(seq(0, 3, by = bin_width), Inf)
    labels <- c(paste(seq(0, 3 - bin_width, by = bin_width), seq(bin_width, 3, by = bin_width), sep = "-"), "3+")
    
    # Adjust the labels vector to match the number of intervals
    labels <- labels[1:(length(breaks) - 1)]
    
    AUTOLOANS$capital_sunkratio_0.5 <- cut(AUTOLOANS$capital_sunkratio, breaks = breaks, labels = FALSE, right = FALSE)
    AUTOLOANS$capital_sunkratio_0.5 <- as.integer(AUTOLOANS$capital_sunkratio_0.5)
    
    # Plotting bins
    ggplot(AUTOLOANS, aes(
      x =capital_sunkratio_0.5, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  

    
    # Capital Sunk Quartile
    ggplot(AUTOLOANS, aes(
      x = capital_sunkratio_quartiles, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
    # Capital Sunk (Deciles)
    
    # Apply decile_bins function to create new column
    AUTOLOANS <- AUTOLOANS %>%
      mutate(capital_sunkratio_deciles = decile_bins(capital_sunkratio))
    
    ggplot(AUTOLOANS, aes(
      x = capital_sunkratio_deciles, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  ### Plotting TX and running t-test ###
    
    ggplot(AUTOLOANS, aes(
      x = TX, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Running T-test #
    # Split the data into groups
    TX <- AUTOLOANS$`LiquidationRatio%`[AUTOLOANS$TX == 1]
    NotTX <- AUTOLOANS$`LiquidationRatio%`[AUTOLOANS$TX == 0]
    
    # Perform t-test
    resultTX <- t.test(TX, NotTX)
    
    # Print test results
    print(resultTX)
    
    
  ### Plotting Account Balance at Closure and Running ANOVA ###
    
    # Account Balance at Closure (every 2.5k)
    bin_width <- 2500
    breaks <- c(seq(0, 27500, by = bin_width), Inf)
    labels <- c(paste(seq(0, 27500 - bin_width, by = bin_width), seq(bin_width, 27500, by = bin_width), sep = "-"), "27500+")
    
    # Adjust the labels vector to match the number of intervals
    labels <- labels[1:(length(breaks) - 1)]
    
    AUTOLOANS$account_balance_at_closure_2500 <- cut(AUTOLOANS$account_balance_at_closure, breaks = breaks, labels = FALSE, right = FALSE)
    AUTOLOANS$account_balance_at_closure_2500 <- as.integer(AUTOLOANS$account_balance_at_closure_2500)
    
    # Plotting bins
    ggplot(AUTOLOANS, aes(
      x = account_balance_at_closure_2500, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
    # Account Balance at Closure (every 5k)
    bin_width <- 5000
    breaks <- c(seq(0, 30000, by = bin_width), Inf)
    labels <- c(paste(seq(0, 30000 - bin_width, by = bin_width), seq(bin_width, 30000, by = bin_width), sep = "-"), "30000+")
    
    # Adjust the labels vector to match the number of intervals
    labels <- labels[1:(length(breaks) - 1)]
    
    AUTOLOANS$account_balance_at_closure_5000 <- cut(AUTOLOANS$account_balance_at_closure, breaks = breaks, labels = FALSE, right = FALSE)
    AUTOLOANS$account_balance_at_closure_5000 <- as.integer(AUTOLOANS$account_balance_at_closure_5000)
    
    # Plotting bins
    ggplot(AUTOLOANS, aes(
      x = account_balance_at_closure_5000, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
    # Account Balance at Closure (every 10k with 30k+ collapsed)
    bin_width <- 10000
    breaks <- c(seq(0, 30000, by = bin_width), Inf)
    labels <- c(paste(seq(0, 30000 - bin_width, by = bin_width), seq(bin_width, 30000, by = bin_width), sep = "-"), "30000+")
    
    # Adjust the labels vector to match the number of intervals
    labels <- labels[1:(length(breaks) - 1)]
    
    AUTOLOANS$account_balance_at_closure_10000 <- cut(AUTOLOANS$account_balance_at_closure, breaks = breaks, labels = FALSE, right = FALSE)
    AUTOLOANS$account_balance_at_closure_10000 <- as.integer(AUTOLOANS$account_balance_at_closure_10000)
    
    # Plotting bins
    ggplot(AUTOLOANS, aes(
      x = account_balance_at_closure_10000, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
    # Plotting Quartiles
    ggplot(AUTOLOANS, aes(
      x = account_balance_at_closure_quartiles, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # account balance at closure (Deciles)
                        
    # Apply decile_bins function to create new column
    AUTOLOANS <- AUTOLOANS %>%
      mutate(account_balance_at_closure_deciles = ifelse(decile_bins(account_balance_at_closure) %in% c(8, 9, 10),
                                                8, as.numeric(decile_bins(account_balance_at_closure))))
    
    ggplot(AUTOLOANS, aes(
      x = account_balance_at_closure_deciles, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
################ Creating Table of ANOVA Results

    # Selecting the columns for ANOVA analysis 
    start_column <- "TotalPaid_2500"
    selected_columns <- colnames(AUTOLOANS)[grep(start_column, colnames(AUTOLOANS)):ncol(AUTOLOANS)]
    
    # Create an empty table to store the ANOVA results
    anova_table <- data.frame()
    
    # Perform ANOVA for each column and add to the table
    for (column in selected_columns) {
      formula <- as.formula(paste("`LiquidationRatio%` ~", column))
      anova_result <- anova(lm(formula, data = AUTOLOANS, na.action = na.exclude))
      anova_table <- rbind(anova_table, anova_result)
    }
    
    # Print the ANOVA table
    View(anova_table)
    
    # Select the additional columns to be added to the table
    additional_columns <- c("TX", "TotalPaid_quartiles", "time_paying_quartiles",
                            "capital_sunkratio_quartiles", "account_balance_at_closure_quartiles")
    
    # Perform ANOVA for each additional column and add to the table
    for (column in additional_columns) {
      formula <- as.formula(paste("`LiquidationRatio%` ~", column))
      anova_result <- anova(lm(formula, data = AUTOLOANS, na.action = na.exclude))
      anova_table <- rbind(anova_table, anova_result)
    }
    
    # Print the updated ANOVA table
    View(anova_table)
    
  
################  Mathematical Model 1 Prepping
    
    #### Splitting dataset into training and validation data
    #### This process was redone in model 2 to ensure both models were trained and validated
    #### by the same data, this code remains here as a way of demonstrating the flow of my work
    #### additionally the model section can be used as a testing grounds
    
    # Set the seed for reproducibility
    set.seed(123)
    
    # Create an index for random sampling
    index <- createDataPartition(AUTOLOANS$`LiquidationRatio%`, p = 0.75, list = FALSE)
    
    # Create the training dataset
    training_data <- AUTOLOANS[index, ]
    
    # Create the validation dataset
    validation_data <- AUTOLOANS[-index, ]
    
   ## Filling NAs with the medians

    # Specify the range of columns
    start_column <- "time_paying_100"
    end_column <- "account_balance_at_closure_deciles"
    
    # Replace NAs with median value for the columns in the range
    validation_data <- validation_data %>%
      mutate(across(starts_with(start_column):ends_with(end_column), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))    
  
    # Replacing NAs in quartile columns
    
    # Specify the columns to replace missing values
    columns_to_replace <- c("time_paying_quartiles","capital_sunkratio_quartiles",
                   "account_balance_at_closure_quartiles")
    
    # Replace NAs with median value for specified columns
    validation_data <- validation_data %>%
      mutate(across(columns_to_replace, ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
    
   
    ########################## SAVING AUTOLOANS AS CSV ###########################
    ##### for personal reasons/to prevent losing data
    ##### No need to run this code
    
    write.csv(AUTOLOANS, "C:\\Users\\AlixMorales\\OneDrive - Flock Specialty Finance\\Documents\\Desktop\\AUTOLOANS_saved.csv" )
    
    
    
################  Mathematical Model 1    
    
    # First Level Model
    summary(training_data)
    summary(validation_data)
    
    finalmodel <- lm(`LiquidationRatio%` ~ time_paying_100+capital_sunkratio_0.25+
                TX+account_balance_at_closure_5000 , data = training_data)
    summary(finalmodel)
 
    # Predict the Liquidation Ratio using the validation data
    predictions_final <- predict(finalmodel, newdata = validation_data)
    
    # Compare predicted values with actual values in the validation data
    comparison_final <- data.frame(Actual = validation_data$`LiquidationRatio%`, Predicted = predictions_final)
    
    # creating column of predicted values
    validation_data$finalmodel_predictions <- predictions_final
    
    # Create the deciles column in validation_data
    validation_data$finalmodel_predictions_deciles <- cut(predictions_final, breaks = quantile(predictions_final, probs = seq(0, 1, 0.1), na.rm = TRUE), labels = FALSE, include.lowest = TRUE)
    
    
    # Second Level Model
    model2 <- lm(`LiquidationRatio%` ~ time_paying_250+capital_sunkratio_0.5+
                   TX+account_balance_at_closure_10000 , data = training_data)
    summary(model2)
    
    # Predict the Liquidation Ratio using the validation data
    predictions2 <- predict(model2, newdata = validation_data)
    
    # Compare predicted values with actual values in the validation data
    comparison2 <- data.frame(Actual = validation_data$`LiquidationRatio%`, Predicted = predictions2)
    
    # Create the deciles column in validation_data
    validation_data$model2_predictions_deciles <- cut(predictions2, breaks = quantile(predictions1, probs = seq(0, 1, 0.1), na.rm = TRUE), labels = FALSE, include.lowest = TRUE)
    
    
    # Quartile Model
    model3 <- lm(`LiquidationRatio%` ~ time_paying_quartiles+capital_sunkratio_quartiles+
                   TX+account_balance_at_closure_quartiles , data = training_data)
    summary(model3)
    
    # Predict the Liquidation Ratio using the validation data
    predictions3 <- predict(model3, newdata = validation_data)
    
    # Compare predicted values with actual values in the validation data
    comparison3 <- data.frame(Actual = validation_data$`LiquidationRatio%`, Predicted = predictions3)
    
    validation_data <- validation_data %>%
      mutate(model3_predictions_deciles = cut(predictions3, breaks = quantile(predictions4,
                                   probs = seq(0, 1, 0.1), na.rm = TRUE), labels = FALSE, include.lowest = TRUE))
    
    
    # Decile model
    model4 <- lm(`LiquidationRatio%` ~ time_paying_deciles+capital_sunkratio_deciles+
                   TX+account_balance_at_closure_deciles , data = training_data)
    summary(model4)
    
    # Predict the Liquidation Ratio using the validation data
    predictions4 <- predict(model4, newdata = validation_data)
    
    # Compare predicted values with actual values in the validation data
    comparison4 <- data.frame(Actual = validation_data$`LiquidationRatio%`, Predicted = predictions4)
    
    # Create deciles based on predictions4
    validation_data <- validation_data %>%
      mutate(model4_predictions_deciles = cut(predictions4, breaks = quantile(predictions4,
                                      probs = seq(0, 1, 0.1), na.rm = TRUE), labels = FALSE, include.lowest = TRUE))

    # First level model chosen and named finalmodel
    # will be reused later with model 2 to create final dataset as mentioned above
    
    
   
################SAVING Validation and Training data - for personal reasons/to prevent losing data
    ##### No need to run this code
write.csv(validation_data, "C:\\Users\\AlixMorales\\OneDrive - Flock Specialty Finance\\Documents\\Desktop\\validation_saved.csv" )
    
write.csv(training_data, "C:\\Users\\AlixMorales\\OneDrive - Flock Specialty Finance\\Documents\\Desktop\\training_saved.csv" )
    


################  Model 2 preparation, cleaning, and modeling ##################   



  ######################## Binning Mileage and Model Age  
    
  ## Collateral Mileage (every 10k)
    bin_width <- 10000
    breaks <- c(0, 40000, 50000, 60000, 70000, 80000, Inf)
    labels <- c("0-40000", "40001-50000", "50001-60000","60001-70000", "70001-80000", "80001+")
    
    # Adjust the labels vector to match the number of intervals
    labels <- labels[1:(length(breaks)-1)]
    
    AUTOLOANS$collateral_mileage_10000 <- cut(AUTOLOANS$collateral_mileage, breaks = breaks,
                                               labels = FALSE, right = FALSE, include.lowest = TRUE)
    AUTOLOANS$collateral_mileage_10000 <- as.integer(AUTOLOANS$collateral_mileage_10000)
    
    # Plotting bins
    ggplot(AUTOLOANS, aes(x = collateral_mileage_10000, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))       
    
  ## Collateral Mileage (every 20k)
    bin_width <- 20000
    breaks <- c(seq(0, 100000, by = bin_width), Inf)
    labels <- c(paste(seq(0, 100000 - bin_width, by = bin_width), seq(bin_width, 100000, by = bin_width), sep = "-"), "100000+")
    
    # Adjust the labels vector to match the number of intervals
    labels <- labels[1:(length(breaks) - 1)]
    
    AUTOLOANS$collateral_mileage_20000 <- cut(AUTOLOANS$collateral_mileage, breaks = breaks, labels = FALSE, right = FALSE)
    AUTOLOANS$collateral_mileage_20000 <- as.integer(AUTOLOANS$collateral_mileage_20000)
    
    # Plotting bins
    ggplot(AUTOLOANS, aes(x =collateral_mileage_20000, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))         

  ## Collateral Mileage (every 50k)
    bin_width <- 50000
    breaks <- c(0, 50000, 100000, Inf)
    labels <- c("0-50000","50001-100000", "100001+")
    
    # Adjust the labels vector to match the number of intervals
    labels <- labels[1:(length(breaks) - 1)]
    
    AUTOLOANS$collateral_mileage_50000 <- cut(AUTOLOANS$collateral_mileage, breaks = breaks, labels = FALSE, right = FALSE)
    AUTOLOANS$collateral_mileage_50000 <- as.integer(AUTOLOANS$collateral_mileage_50000)
    
    # Plotting bins
    ggplot(AUTOLOANS, aes(x =collateral_mileage_50000, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))         
    
    # testing
    sum(is.na(AUTOLOANS$collateral_mileage_50000))
    
    
  ## Collateral Mileage (quartiles)    
    ggplot(AUTOLOANS, aes(
      x = collateral_mileage_quartiles, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  ## Collateral Mileage (deciles)  
    AUTOLOANS <- AUTOLOANS %>%
      mutate(collateral_mileage_deciles = ifelse(decile_bins(collateral_mileage) %in% c(4,5,6,7,8, 9, 10),
                                        4, as.numeric(decile_bins(collateral_mileage))))
    
    ggplot(AUTOLOANS, aes(
      x = collateral_mileage_deciles, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  ## Model Age (Every 2 years)
    bin_width <- 2
    breaks <- c(seq(0, 14, by = bin_width), Inf)
    labels <- c(paste(seq(0, 14 - bin_width, by = bin_width), seq(bin_width, 14, by = bin_width), sep = "-"), "18+")
    
    # Adjust the labels vector to match the number of intervals
    labels <- labels[1:(length(breaks) - 1)]
    
    AUTOLOANS$model_age_2 <- cut(AUTOLOANS$model_age, breaks = breaks, labels = FALSE, right = FALSE)
    AUTOLOANS$model_age_2 <- as.integer(AUTOLOANS$model_age_2)
    
    # Plotting bins
    ggplot(AUTOLOANS, aes(x = model_age_2, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ## Model Age (Every 5 years)
    bin_width <- 5
    breaks <- c(seq(0, 15, by = bin_width), Inf)
    labels <- c(paste(seq(0, 15 - bin_width, by = bin_width), seq(bin_width, 15, by = bin_width), sep = "-"), "20+")
    
    # Adjust the labels vector to match the number of intervals
    labels <- labels[1:(length(breaks) - 1)]
    
    AUTOLOANS$model_age_5 <- cut(AUTOLOANS$model_age, breaks = breaks, labels = FALSE, right = FALSE)
    AUTOLOANS$model_age_5 <- as.integer(AUTOLOANS$model_age_5)
    
    # Plotting bins
    ggplot(AUTOLOANS, aes(x = model_age_5, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ## Model Age (deciles)  

    breaks <- quantile(AUTOLOANS$model_age, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
    breaks <- unique(breaks)  # Remove any duplicate breaks
    breaks_custom <- c(breaks[1:2], breaks[3], breaks[7], Inf)
  
    AUTOLOANS$model_age_deciles <- cut(AUTOLOANS$model_age, breaks_custom, labels = FALSE, include.lowest = TRUE)

    
    ggplot(AUTOLOANS, aes(
      x = model_age_deciles, y = `LiquidationRatio%`)) +
      stat_summary(fun = "mean", geom = "point") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ##### Saving AUTOLOANS - for personal reasons/to prevent losing data
    ##### No need to run this code
write.csv(AUTOLOANS, "C:\\Users\\AlixMorales\\OneDrive - Flock Specialty Finance\\Documents\\Desktop\\AUTOLOANS.csv" )
    
  ################## Creating dummy for work vehicles ######

    # checking models in data
    unique_models <- table(AUTOLOANS$collateral_model)
    View(unique_models)
    
    
    # creating work vehicle dummy
    AUTOLOANS$work_vehicle <- ifelse(AUTOLOANS$collateral_model %in% c("E250", "E250 Vans", "Ram 3500", "ctp713",
                   "F250", "SAVANA", "TITAN", "Silverado 2500", "F-550", "F350SD", "G2500", "ECONOLINE",
                   "F250SD", "F150", "Sierra", "Ram 1500", "Silverado 1500", "1500", "2500", "3500","Sierra 1500",
                  "Express Cargo", "F550", "Sierra 2500", "EXPRESS G1500", "Ram 2500", "Silverado 3500",
                  "Silverado", "SILVERADO 3500", "TUNDRA", "RAM", "SILVERADO 2500","E150","1500 Crew Cab",
                  "1500 Quad Cab", "1500 RAM PICKUB 2WD V8", "Astro Vans", "B3000", "Canyon", "CANYON",
                  "Caravan", "Colorado", "COLORADO", "Colorado Extended Cab", "E350", "Econoline", 
                  "F-150", "F-150 XLT; LARIAT; K", "F-350", "F 150", "F150 FX2", "F150 KING RANCh", "F150 LARIAT",
                  "F150 PICKUP 2WD V6", "F150 PICKUP 4WD V8", "F150 PLATINUM", "F150 Regular Cab", "F150 Super Cab",
                  "F150 SUPER CREW", "F150 SuperCrew Cab", "F250 LARIAT", "F350", "F350 LARIAT", "F350 PLATINUM",
                  "Frontier", "FRONTIER", "Frontier Crew Cab", "G1500", "G2500", "G2500 Vans", "G3500", "G3500 Vans",
                  "GMT-400", "NV 1500", "NV 200", "Ram", "RAM 1500", "Ram 1500 Crew Cab", "RAM 2500", "RAM 3500",
                  "Ranger", "RANGER", "RANGER PICKUP 2WD V6", "S10", "SIERRA", "SIERRA 1500", "Sierrra 1500 Crew Cab",
                  "SIERRA 1500 PICKUP SLE", "SIERRA 3500", "SILVERADO", "SILVERADO 1500", "Silverado 1500 Clsc", 
                  "Silverado 1500 Crew Cab", "Silverado 1500 Double Cab", "Silverado 1500 Extended Cab", "SILVERADO 2500HD",
                  "SILVERADO 3500", "SILVERADO K1500", "T150", "T350 Vans", "Tacoma", "TACOMA", "Tacoma Access Cab", 
                  "Titan", "Titan Crew Cab", "TITAN XD", "VAN"), 1, 0)
   
    
     ###### Saving AUTOLOANS - for personal reasons/to prevent losing data
    ##### No need to run this code
    
write.csv(AUTOLOANS, "C:\\Users\\AlixMorales\\OneDrive - Flock Specialty Finance\\Documents\\Desktop\\AUTOLOANS_saved.csv" )
    
  ################ Importing reliability/pricing data 

    ######### Elementary Cleaning  
    kaggle_vehicles <- read_csv("C:\\Users\\AlixMorales\\OneDrive - Flock Specialty Finance\\Documents\\kaggle_vehicles.csv")
    
    
    # working with kaggle_vehicles
    summary(kaggle_vehicles)
   
    # eliminating variables
    kaggle_vehicles <- select(kaggle_vehicles, -url, -region_url, -cylinders, -fuel,
                              -transmission, -image_url, -county, -long, -lat, -id,
                              -region, -VIN, -drive, -paint_color, -description)
    
    # checking for missing values
    summary(kaggle_vehicles)
    
    # eliminating rows with NAs in Price, Year, Manufacturer, Model
    kaggle_vehicles <- kaggle_vehicles %>%
      filter(!is.na(price))
    
    kaggle_vehicles <- kaggle_vehicles %>%
      filter(!is.na(year))
    
    kaggle_vehicles <- kaggle_vehicles %>%
      filter(!is.na(model))
    
    kaggle_vehicles <- kaggle_vehicles %>%
      filter(!is.na(odometer))
    
    # Count the number of duplicates
    num_duplicates <- sum(duplicated(kaggle_vehicles))
    
    # Print the number of duplicates
    print(num_duplicates)
    
    # dropping all duplicates
    kaggle_vehicles_unique <- distinct(kaggle_vehicles)
    
    # eliminating any listings of years pre-1992, 93,95,97
    kaggle_vehicles_unique <- subset(kaggle_vehicles_unique, year >= 1992)
    kaggle_vehicles_unique <- subset(kaggle_vehicles_unique, !(year %in% c(1993, 1995, 1997)))
    
 ############ Cleaning Variables
  ## Manufacturers
    # Get the unique manufacturers
    manufacturers <- unique(kaggle_vehicles_unique$manufacturer)
    
    # Print the list of manufacturers
    View(table(manufacturers))
    
    # Eliminating manufacturers not in autoloans
    kaggle_vehicles_unique <- subset(kaggle_vehicles_unique, !(manufacturer %in%
        c("aston-martin", "tesla", "alfa-romeo", "harley-davidson", "datsun", "ferrari")))
    
  ## Eliminating Outliers (creates vehicles)
    price_percentile25 <- quantile(kaggle_vehicles_unique$price, 0.25)
    price_percentile75 <- quantile(kaggle_vehicles_unique$price, 0.75)
    price_iqr <- price_percentile75 - price_percentile25
    price_upper_limit <- price_percentile75 + 1.5 * price_iqr
    price_lower_limit <- quantile(kaggle_vehicles_unique$price, 0.15)
    vehicles <- kaggle_vehicles_unique[kaggle_vehicles_unique$price < price_upper_limit & kaggle_vehicles_unique$price > price_lower_limit, ]
    
    odometer_upper_limit <- quantile(vehicles$odometer, 0.75) + 1.5 * IQR(vehicles$odometer)
    vehicles <- vehicles[vehicles$odometer < odometer_upper_limit, ]

    # eliminating weird fonts
    vehicles <- vehicles[-grep("🔥GMC Sierra 1500 SLE🔥 4X4 🔥", vehicles$model), ]
    vehicles <- vehicles[-grep("𝓜𝓮𝓻𝓬𝓮𝓭𝓮𝓼 𝓫𝓮𝓷𝔃 𝓶𝓵 350", vehicles$model), ]
    vehicles <- vehicles[-grep("- 4 DOOR ACCORD - SAFETY SENSING", vehicles$model), ]
    vehicles <- vehicles[-grep("- 4WD - 5 SEAT - DOUBLE CAB", vehicles$model), ]
    vehicles <- vehicles[-grep("- 1500 Promaster Vans", vehicles$model), ]
    vehicles <- vehicles[-grep("- PEARL WHITE HYBRID - 49 MPG", vehicles$model), ]
    vehicles <- vehicles[!grepl("^\\(", vehicles$model), ]
    vehicles <- vehicles[!grepl("^\\$", vehicles$model), ]
    vehicles <- vehicles[-grep("^\\-", vehicles$model), ]
    vehicles <- vehicles[-grep("^\\*", vehicles$model), ]
    vehicles <- vehicles[-grep("^\\#", vehicles$model), ]
    vehicles <- vehicles[-grep("^\\.", vehicles$model), ]
    vehicles <- vehicles[!grepl(":", vehicles$model), ]
    vehicles <- vehicles[!grepl("♿", vehicles$model), ]
    vehicles <- vehicles[-grep("^\\/", vehicles$model), ]
    
    # eliminating years in models
    vehicles1$model <- gsub("\\b\\d{4}\\b", "", vehicles1$model)
    # Remove leading and trailing whitespace
    vehicles1$model <- trimws(vehicles1$model)
    
    # making all models lowercase in both datasets
    vehicles$model <- tolower(vehicles$model)
    AUTOLOANS_SCORED$collateral_model <- tolower(AUTOLOANS_SCORED$collateral_model)
    AUTOLOANS_SCORED$collateral_make <- tolower(AUTOLOANS_SCORED$collateral_make)
    
    # summary of work done 
    summary(vehicles)
    
    # Group the data by model and year, calculate the average price for each group, and create a new dataset
    #### This was simply to test whether i could find average values for each vehicle
    
    average_prices1 <- vehicles1 %>%
      group_by(manufacturer, model, year) %>%
      summarize(average_price = mean(price, na.rm = TRUE)) %>%
      ungroup()
    average_prices <- vehicles %>%
      group_by(manufacturer, model, year) %>%
      summarize(average_price = mean(price, na.rm = TRUE)) %>%
      ungroup()
    
    # Print the new dataset
    View(average_prices)
    View(average_prices1)  # we need to do additional cleaning
    
    ##### Additional Cleaning #####
    
  ### Seperating rows with mack
    mack_rows <- vehicles[grepl("Mack", vehicles$model, ignore.case = TRUE), ]
    mack_rows
    # turning all mack vehicles into simply mack model
    vehicles$model <- gsub(".*mack.*", "mack", vehicles$model)
    
    # trimming models down (Acura)
    vehicles$model[vehicles$manufacturer == "acura" & grepl(".*tl.*", vehicles$model)] <- "tl"
    vehicles$model[vehicles$manufacturer == "acura" & grepl(".*rsx.*", vehicles$model)] <- "rsx"
    vehicles$model <- gsub(".*mdx.*", "mdx", vehicles$model)
    vehicles$model <- gsub(".*rdx.*", "rdx", vehicles$model)
    vehicles$model <- gsub(".*ilx.*", "ilx", vehicles$model)
    vehicles$model[vehicles$manufacturer == "acura" & grepl(".*rl.*", vehicles$model)] <- "rl"
    vehicles$model[vehicles$manufacturer == "acura" & grepl(".*cl.*", vehicles$model)] <- "cl"
    vehicles$model <- gsub(".*integra.*", "integra", vehicles$model)
    vehicles$model[vehicles$manufacturer == "acura" & grepl(".*legend.*", vehicles$model)] <- "legend"
    vehicles$model <- gsub(".*legend.*", "legend", vehicles$model)
    vehicles$model <- gsub(".*mkx.*", "mkx", vehicles$model)
    vehicles$model[vehicles$manufacturer == "acura" & grepl(".*tsx.*", vehicles$model)] <- "tsx"

    
    # trimming models down (Audi)
    vehicles <- vehicles[-grep("^1.8t", vehicles$model), ]
    vehicles$model[vehicles$manufacturer == "audi" & grepl(".*a3.*", vehicles$model)] <- "a3"
    vehicles$model[vehicles$manufacturer == "audi" & grepl(".*a4.*", vehicles$model)] <- "a4"
    
    #### TRIMMING DOWN CONTINUED IN EXCEL #####
      #### Model names shortened, make names corrected, 

  ##### saving as csv
    ##### This will later become vehicles_cleaned after the excel cleaning
    
    write.csv(vehicles, "C:\\Users\\AlixMorales\\OneDrive - Flock Specialty Finance\\Documents\\Desktop\\vehicles.csv" )
    
    
#################### joining Reliability Score - from ritter ###################
    
    PerformingAuto_Data_WithReliabilityScores <- read_csv("Copy of PerformingAuto_Data_WithReliabilityScores.csv")
    View(PerformingAuto_Data_WithReliabilityScores)
    
    # Replacing NAs with 0s
    PerformingAuto_Data_WithReliabilityScores$`Reliability Score`[is.na(PerformingAuto_Data_WithReliabilityScores$`Reliability Score`)] <- 0
    
    # eliminating row 1 from Reliabity Scores Data
    PerformingAuto_Data_WithReliabilityScores <- PerformingAuto_Data_WithReliabilityScores %>%
      filter(portfolio_id != 1)
    
    # Converting Oirginal ACCT NO. to a double (numeric)
    PerformingAuto_Data_WithReliabilityScores$original_creditor_account_number<- as.numeric(PerformingAuto_Data_WithReliabilityScores$original_creditor_account_number)
    
    # Data was joined in excel using vlookup joining scores by portfolio id
    #### Saving as csv so that data can be joined in excel
write.csv(PerformingAuto_Data_WithReliabilityScores, "C:\\Users\\AlixMorales\\Documents\\PerformingAuto_Data_WithReliabilityScores.csv" )
    
    # Joined Dataset - data is reuploaded due to the dataset being very large
    #### n_max = 8015 is present to trim this data
AUTOLOANS_SCORED <- read_csv("AUTOLOANS_saved.csv", n_max = 8015)

    # Saving Dataset
    #### THIS is the data that will be used in excel along with Ritter's data
write.csv(AUTOLOANS_SCORED, "C:\\Users\\AlixMorales\\Documents\\Final Cleaned Data\\AUTOLOANS_scored.csv")

#################### Cleaning and summarizing for joining values   
    # cleaned and prepped data in excel
      # the following is loading and cleaning vehicles data along with the average prices
        vehicles_cleaned <- read_csv("C:\\Users\\AlixMorales\\Documents\\Final Cleaned Data\\vehicles_cleaned.csv")
    
        # Calculating averages 
    average_prices <- vehicles_cleaned %>%
    group_by(manufacturer, model, year) %>%
    summarize(average_price = mean(price, na.rm = TRUE)) %>%
  ungroup()
    
      View(average_prices)
      
      # Saving average prices so that I can join in excel
      write.csv(average_prices, "C:\\Users\\AlixMorales\\Documents\\Final Cleaned Data\\vehicles_averages.csv")

  ###### Loading AUTOLOANS_final (after excel joining)
      #### Loading
      AUTOLOANS_FINAL <- read_csv("C:\\Users\\AlixMorales\\Documents\\Final Cleaned Data\\AUTOLOANS_final.csv")
      
      summary(AUTOLOANS_FINAL)
      
      #### Creating new variables
      
        # loan-to-value
      AUTOLOANS_FINAL <- AUTOLOANS_FINAL %>%
        mutate(loan_to_value = (origination_balance / value))
      
        # sunkcost-to-value
      AUTOLOANS_FINAL <- AUTOLOANS_FINAL %>%
        mutate(sunkcost_to_value = (`TotalPaid` + coalesce(down_payment, 0)) / value)
      
      #### Binning
      
        # Quartiles #
      AUTOLOANS_FINAL <- AUTOLOANS_FINAL %>%
        mutate(reliability_score_quartiles = ntile(reliability_score, 4)) %>%
        mutate(reliability_score_quartiles = ifelse(reliability_score_quartiles <= 3, 1L, 2L))
      
      ggplot(AUTOLOANS_FINAL, aes(x = reliability_score_quartiles, y = `LiquidationRatio%`)) +
        stat_summary(fun = "mean", geom = "point") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      
      AUTOLOANS_FINAL <- AUTOLOANS_FINAL %>%
        mutate(loan_to_value_quartiles = quartilefct(loan_to_value))
      
      ggplot(AUTOLOANS_FINAL, aes(
        x = loan_to_value_quartiles, y = `LiquidationRatio%`)) +
        stat_summary(fun = "mean", geom = "point") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      
      AUTOLOANS_FINAL <- AUTOLOANS_FINAL %>%
        mutate(sunkcost_to_value_quartiles = quartilefct(sunkcost_to_value))

      ggplot(AUTOLOANS_FINAL, aes(
        x = sunkcost_to_value_quartiles, y = `LiquidationRatio%`)) +
        stat_summary(fun = "mean", geom = "point") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

          #### Creating bin for LiquidationRatio%
      AUTOLOANS_FINAL <- AUTOLOANS_FINAL %>%
        mutate(`Liquidationratio%_quartiles` = quartilefct(`LiquidationRatio%`))

      
      
        # Deciles #
      AUTOLOANS_FINAL <- AUTOLOANS_FINAL %>%
        mutate(reliability_score_deciles = ntile(reliability_score, 10)) %>%
        mutate(reliability_score_deciles = ifelse(reliability_score_deciles <= 8, 1L, 2L))
      
      ggplot(AUTOLOANS_FINAL, aes(x = reliability_score_deciles, y = `LiquidationRatio%`)) +
        stat_summary(fun = "mean", geom = "point") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      
      # Create deciles column
      AUTOLOANS_FINAL <- AUTOLOANS_FINAL %>%
        mutate(
          loan_to_value_deciles = ntile(loan_to_value, 10),  # Create deciles column
          loan_to_value_deciles = ifelse(loan_to_value_deciles %in% c(4, 5), 4,
                                         ifelse(loan_to_value_deciles %in% c(9, 10), 9,
                                                loan_to_value_deciles)),  # Combine deciles 4 and 5 into 4, and 9 and 10 into 9
          loan_to_value_deciles = loan_to_value_deciles - as.integer(loan_to_value_deciles > 5)  # Adjust deciles greater than 5
        )
      
      # Plotting modified deciles
      ggplot(AUTOLOANS_FINAL, aes(x = loan_to_value_deciles, y = `LiquidationRatio%`)) +
        stat_summary(fun = "mean", geom = "point") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_x_continuous(breaks = 1:8) +
        labs(x = "Loan-to-Value Deciles")
      
      
      AUTOLOANS_FINAL <- AUTOLOANS_FINAL %>%
        mutate(sunkcost_to_value_deciles = ntile(sunkcost_to_value, 10))
      
      ggplot(AUTOLOANS_FINAL, aes(
        x = sunkcost_to_value_deciles, y = `LiquidationRatio%`)) +
        stat_summary(fun = "mean", geom = "point") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
          #### Creating bin for LiquidationRatio%
      AUTOLOANS_FINAL <- AUTOLOANS_FINAL %>%
        mutate(`LiquidationRatio%_deciles` = ntile(`LiquidationRatio%`, 10))
      
      ggplot(AUTOLOANS_FINAL, aes(
        x = `LiquidationRatio%_deciles`, y = `LiquidationRatio%`)) +
        stat_summary(fun = "mean", geom = "point") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      
        # Chosen Bins #
      
          #loan_to_value (every 0.5)
      
      AUTOLOANS_FINAL <- AUTOLOANS_FINAL %>%
        mutate(
          loan_to_value = as.numeric(loan_to_value),
          loan_to_value_0.5 = ifelse(
            loan_to_value <= 1.5,
            cut(loan_to_value, breaks = seq(0, 1.5, by = 0.5), labels = FALSE),
            4
          )
        )
      
      # Convert loan_to_value_0.5 to numeric
      AUTOLOANS_FINAL$loan_to_value_0.5 <- as.numeric(AUTOLOANS_FINAL$loan_to_value_0.5)
      
      ggplot(AUTOLOANS_FINAL, aes(
        x = loan_to_value_0.5, y = `LiquidationRatio%`)) +
        stat_summary(fun = "mean", geom = "point") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      
            #sunkcost_to_value (every 0.5)

      # Define the breaks for binning
      breaks <- c(-Inf, seq(0, max(AUTOLOANS_FINAL$sunkcost_to_value, na.rm = TRUE), by = 0.5), Inf)
      
      # Create a new variable for binning
      AUTOLOANS_FINAL$sunkcost_to_value_0.5 <- cut(AUTOLOANS_FINAL$sunkcost_to_value, breaks = breaks, labels = FALSE, include.lowest = TRUE)
      
      # Combine bins 6, 7, 8, 9, and 10 into a single bin
      AUTOLOANS_FINAL$sunkcost_to_value_0.5[AUTOLOANS_FINAL$sunkcost_to_value_0.5 %in% 6:10] <- 6
      
      # Convert the bin variable to numeric
      AUTOLOANS_FINAL$sunkcost_to_value_0.5 <- as.numeric(AUTOLOANS_FINAL$sunkcost_to_value_0.5)
      
       
      ggplot(AUTOLOANS_FINAL, aes(
        x = sunkcost_to_value_0.5, y = `LiquidationRatio%`)) +
        stat_summary(fun = "mean", geom = "point") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      
      # creating dummys for the top 3 makes + significant makes #
      
      
        ### Finding the lowest and highest liquidation ratios
      LiquidationRatios <- AUTOLOANS_FINAL %>%
        group_by(collateral_make) %>%
        summarise(mean_LiquidationRatio = mean(`LiquidationRatio%`))
      
      View(LiquidationRatios)
          ## 3 Lowest: fiat, subaru, mini
          ## 3 highest: hummer, porsche, saturn
      
          # creating the dummys
            #chevy
      AUTOLOANS_FINAL <- AUTOLOANS_FINAL %>%
        mutate(chevrolet = ifelse(collateral_make == "chevrolet", 1, 0))
      
      ggplot(AUTOLOANS_FINAL, aes(
        x = chevrolet, y = `LiquidationRatio%`)) +
        stat_summary(fun = "mean", geom = "point") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
            #ford
      AUTOLOANS_FINAL <- AUTOLOANS_FINAL %>%
        mutate(ford = ifelse(collateral_make == "ford", 1, 0))
      
      ggplot(AUTOLOANS_FINAL, aes(
        x = ford, y = `LiquidationRatio%`)) +
        stat_summary(fun = "mean", geom = "point") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
            #nissan
      AUTOLOANS_FINAL <- AUTOLOANS_FINAL %>%
        mutate(nissan = ifelse(collateral_make == "nissan", 1, 0))
      
      ggplot(AUTOLOANS_FINAL, aes(
        x = nissan, y = `LiquidationRatio%`)) +
        stat_summary(fun = "mean", geom = "point") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
            # fiat
      AUTOLOANS_FINAL <- AUTOLOANS_FINAL %>%
        mutate(fiat = ifelse(collateral_make == "fiat", 1, 0))
      
      ggplot(AUTOLOANS_FINAL, aes(
        x = fiat, y = `LiquidationRatio%`)) +
        stat_summary(fun = "mean", geom = "point") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
            # mini 
      AUTOLOANS_FINAL <- AUTOLOANS_FINAL %>%
        mutate(mini = ifelse(collateral_make == "mini", 1, 0))
      
      ggplot(AUTOLOANS_FINAL, aes(
        x = mini, y = `LiquidationRatio%`)) +
        stat_summary(fun = "mean", geom = "point") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
            # subaru
      AUTOLOANS_FINAL <- AUTOLOANS_FINAL %>%
        mutate(subaru = ifelse(collateral_make == "subaru", 1, 0))
      
      ggplot(AUTOLOANS_FINAL, aes(
        x = subaru, y = `LiquidationRatio%`)) +
        stat_summary(fun = "mean", geom = "point") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
            #hummer
      AUTOLOANS_FINAL <- AUTOLOANS_FINAL %>%
        mutate(hummer = ifelse(collateral_make == "hummer", 1, 0))
      
      ggplot(AUTOLOANS_FINAL, aes(
        x = hummer, y = `LiquidationRatio%`)) +
        stat_summary(fun = "mean", geom = "point") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
            #porsche
      AUTOLOANS_FINAL <- AUTOLOANS_FINAL %>%
        mutate(porsche = ifelse(collateral_make == "porsche", 1, 0))
      
      ggplot(AUTOLOANS_FINAL, aes(
        x = porsche, y = `LiquidationRatio%`)) +
        stat_summary(fun = "mean", geom = "point") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
              
            #saturn
      AUTOLOANS_FINAL <- AUTOLOANS_FINAL %>%
        mutate(saturn = ifelse(collateral_make == "saturn", 1, 0))
      
      ggplot(AUTOLOANS_FINAL, aes(
        x = saturn, y = `LiquidationRatio%`)) +
        stat_summary(fun = "mean", geom = "point") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
      
        # creating dummys for chevy trucks, ford trucks, etc. #
      
        ## Ford Truck
      AUTOLOANS_FINAL <- AUTOLOANS_FINAL %>%
        mutate(ford_truck = ifelse(collateral_make == "ford" & work_vehicle == 1, 1, 0))
      
      ggplot(AUTOLOANS_FINAL, aes(
        x = ford_truck, y = `LiquidationRatio%`)) +
        stat_summary(fun = "mean", geom = "point") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
        ## Chevy Trucks
      AUTOLOANS_FINAL <- AUTOLOANS_FINAL %>%
        mutate(chevrolet_truck = ifelse(collateral_make == "chevrolet" & work_vehicle == 1, 1, 0))
      
      ggplot(AUTOLOANS_FINAL, aes(
        x = chevrolet_truck, y = `LiquidationRatio%`)) +
        stat_summary(fun = "mean", geom = "point") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
        ## Dodge Trucks
      AUTOLOANS_FINAL <- AUTOLOANS_FINAL %>%
        mutate(dodge_truck = ifelse(collateral_make == "dodge" & work_vehicle == 1, 1, 0))
      
      ggplot(AUTOLOANS_FINAL, aes(
        x = dodge_truck, y = `LiquidationRatio%`)) +
        stat_summary(fun = "mean", geom = "point") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
        ## Nissan Trucks
      AUTOLOANS_FINAL <- AUTOLOANS_FINAL %>%
        mutate(nissan_truck = ifelse(collateral_make == "nissan" & work_vehicle == 1, 1, 0))
      
      ggplot(AUTOLOANS_FINAL, aes(
        x = nissan_truck, y = `LiquidationRatio%`)) +
        stat_summary(fun = "mean", geom = "point") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
     
      
########### Spearman's Testing     
      #### The top code will return all variables so you can see how the new variables stack up
      #### The bottom code will only focus on the new variables, use to identify the variables
      
      ### For all variables
      target_variable <- "LiquidationRatio%"
      correlation_table2 <- data.frame(Variable = character(), Correlation = numeric(), p_value = numeric(), stringsAsFactors = FALSE)
      
      # Loop over each variable in the dataframe
      for (variable in colnames(AUTOLOANS_FINAL)) {
        if (variable != target_variable && is.numeric(AUTOLOANS_FINAL[[variable]])) {
          # Perform Spearman's rank correlation test
          cor_test <- cor.test(AUTOLOANS_FINAL[[variable]], AUTOLOANS_FINAL[[target_variable]], method = "spearman")
          
          # Extract correlation coefficient and p-value
          correlation <- cor_test$estimate
          p_value <- cor_test$p.value
          
          # Create a new row in the correlation table
          new_row <- data.frame(Variable = variable, Correlation = correlation, p_value = p_value)
          
          # Append the new row to the correlation table
          correlation_table2 <- rbind(correlation_table2, new_row)
        }
      }
      
      # Print the correlation table
      View(correlation_table2)    
      
      
      
      #### For only new model 2 variables
      
      target_variable <- "LiquidationRatio%"
      correlation_tablenew <- data.frame(Variable = character(), Correlation = numeric(), p_value = numeric(), stringsAsFactors = FALSE)
      
      # Define the range of columns to loop over
      start_column <- 83  # Starting column index
      end_column <- 117    # Ending column index
      
      # Loop over each variable in the specified range of columns
      for (i in start_column:end_column) {
        variable <- colnames(AUTOLOANS_FINAL)[i]
        if (variable != target_variable && is.numeric(AUTOLOANS_FINAL[[variable]])) {
          # Perform Spearman's rank correlation test
          cor_test <- cor.test(AUTOLOANS_FINAL[[variable]], AUTOLOANS_FINAL[[target_variable]], method = "spearman")
          
          # Extract correlation coefficient and p-value
          correlation <- cor_test$estimate
          p_value <- cor_test$p.value
          
          # Create a new row in the correlation table
          new_row <- data.frame(Variable = variable, Correlation = correlation, p_value = p_value)
          
          # Append the new row to the correlation table
          correlation_tablenew <- rbind(correlation_tablenew, new_row)
        }
      }
      
      # Print the correlation table
      View(correlation_tablenew)
      

      
###################### Model 2 Prep #######################################
      ##### As mentioned above model 1 was reurn through this data along with model 2
      ##### in an effort to have both run through the same data
      ##### This will be the final model for both model 1 and 2!
      
      
      # Create the training dataset
      training_data2 <- AUTOLOANS_FINAL[index, ]
      training_datam1 <- AUTOLOANS_FINAL[index, ]
      
      # Create the validation dataset
      validation_data2 <- AUTOLOANS_FINAL[-index, ]
      validation_datam1 <- AUTOLOANS_FINAL[-index, ]
      
      ## Filling NAs with the medians
      
      # Specify the range of columns
      start_column <- "time_paying_100"
      end_column <- "loan_to_value_bin"
      
      # Replace NAs with median value for the columns in the range
      validation_datam1 <- validation_datam1 %>%
        mutate(across(starts_with(start_column):ends_with(end_column), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))    
      
      # Replacing NAs in quartile columns
      
      # Specify the columns to replace missing values
      columns_to_replace <- c("time_paying_quartiles","capital_sunkratio_quartiles",
                              "account_balance_at_closure_quartiles")
      
      # Replace NAs with median value for specified columns
      validation_datam1 <- validation_datam1 %>%
        mutate(across(all_of(columns_to_replace), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))
      
      
###################### TEST MODELS ###################################
      
      ################### model 1
      finalmodelm1 <- lm(`LiquidationRatio%` ~ time_paying_100+capital_sunkratio_0.25+
                         TX+account_balance_at_closure_5000 , data = training_datam1)
      summary(finalmodelm1)
      plot(finalmodelm1)
      
      # Predict the Liquidation Ratio using the validation data
      predictions_final <- predict(finalmodelm1, newdata = validation_datam1)
      
      # Compare predicted values with actual values in the validation data
      comparison_final <- data.frame(Actual = validation_datam1$`LiquidationRatio%`, Predicted = predictions_final)
      
      # creating column of predicted values
      validation_datam1$model1_predictions <- predictions_final
      
      # loading separate package for deciles
      
      validation_datam1$model1_predictions_deciles <- ntile(predictions_final, 10)
      
  
      # Create the deciles column in validation_data
      validation_datam1$model1_predictions_deciles <- cut(predictions_final, breaks = quantile(predictions_final, probs = seq(0, 1, 0.1), na.rm = TRUE), labels = FALSE, include.lowest = TRUE)
      
      
      #################### model 2
      finalmodel2 <- lm(`LiquidationRatio%` ~ loan_to_value_deciles +
                      collateral_mileage + value + model_age_2 +
                      work_vehicle + reliability_score_quartiles + hummer + luxury_car +
                      dodge_truck + subaru + fiat
                    , data = training_datam1)
      summary(finalmodel2)
      plot(finalmodel2)
      
      # Predict the Liquidation Ratio using the validation data
      predictions_final <- predict(finalmodel2, newdata = validation_datam1)
      
      # Compare predicted values with actual values in the validation data
      comparison_final <- data.frame(Actual = validation_datam1$`LiquidationRatio%`, Predicted = predictions_final)
      
      # creating column of predicted values
      validation_datam1$model2_predictions <- predictions_final
      
      # Create the deciles column in validation_data
      validation_datam1$model2_predictions_deciles <- cut(predictions_final, breaks = quantile(predictions_final, probs = seq(0, 1, 0.1), na.rm = TRUE), labels = FALSE, include.lowest = TRUE)
      
  ###### Assuring the distributions are fair and reasonable 
      
      # Count the number of observations in each decile bin
      count_deciles <- validation_datam1 %>%
        group_by(model1_predictions_deciles) %>%
        summarise(count = n())
      
      # Calculate the average predicted value for each decile
      average_deciles <- validation_datam1 %>%
        group_by(model1_predictions_deciles) %>%
        summarise(average_predicted_value = mean(model1_predictions))
      
      # Merge the count and average data frames based on the decile column
      decile_summary <- merge(count_deciles, average_deciles, by = "model1_predictions_deciles")
      
      # View the decile summary
      View(decile_summary)
      
      
      
      
      # Count the number of observations in each decile bin
      count_deciles2 <- validation_datam1 %>%
        group_by(model2_predictions_deciles) %>%
        summarize(count = n())
      
      # Calculate the average predicted value for each decile
      average_deciles2 <- validation_datam1 %>%
        group_by(model2_predictions_deciles) %>%
        summarize(average_predicted_value = mean(model2_predictions))
      
      # Merge the count and average data frames based on the decile column
      decile_summary2 <- merge(count_deciles2, average_deciles2, by = "model2_predictions_deciles")
      
      # View the decile summary
      View(decile_summary2)
      
      

      
############### Creating the matrix to test distribution ###############
    ##### The following code was simply to test how the matrix would look like in excel
      #### You can ignore and skip to the excel step
      
      
      # Group by deciles from both models and calculate the average predicted value
      average_matrix <- validation_datam1 %>%
        group_by(model1_predictions_deciles, model2_predictions_deciles) %>%
        summarize(average_prediction = mean(model1_predictions), .groups = "drop")
      
      # Pivot the data to create the matrix
      matrix <- average_matrix %>%
        pivot_wider(names_from = model2_predictions_deciles, values_from = average_prediction)
      
      # View the resulting matrix
      View(matrix)
      
      
      # Group by deciles from both models and calculate the net value
      average_matrix <- validation_datam1 %>%
        group_by(model1_predictions_deciles, model2_predictions_deciles) %>%
        summarize(net_value = sum(model1_predictions * TotalPaid), .groups = "drop")
      
      # Pivot the data to create the matrix
      matrix <- average_matrix %>%
        pivot_wider(names_from = model2_predictions_deciles, values_from = net_value)
      
      # View the resulting matrix
      View(matrix)

      
      # Group by deciles from both models and calculate the average predicted value
      average_matrix <- validation_datam1 %>%
        group_by(model1_predictions_deciles, model2_predictions_deciles) %>%
        summarize(average_prediction = mean(model1_predictions), .groups = "drop")
      
      # Create the matrix using dcast
      average_matrix <- dcast(average_matrix, model1_predictions_deciles ~ model2_predictions_deciles, value.var = "average_prediction")
      
      # View the resulting matrix
      View(average_matrix)
      
      
      #### Finding counts within the cells
      # Group by deciles from both models and calculate the average predicted value and count
      count_matrix <- validation_datam1 %>%
        group_by(model1_predictions_deciles, model2_predictions_deciles) %>%
        summarize(
          count = n()
        )
      
      # Pivot the data to create the matrix
      matrix_count <- count_matrix %>%
        pivot_wider(names_from = model2_predictions_deciles, values_from = count)
      
      # View the resulting matrix
      View(matrix_count)
      
      
################### Exporting this data into excel for matrix creation ##########
      
      write.csv(validation_datam1, "C:\\Users\\AlixMorales\\Documents\\matrix_validation.csv")
