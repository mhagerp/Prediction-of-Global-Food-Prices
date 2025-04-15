library(shiny)
library(tidyverse)
library(tsibble)
library(fpp3)
library(forecast)
library(ggplot2)
library(randomForest)
library(quantmod)
library(vars)

# Load data
food_prices <- read_csv("file.csv", show_col_types = FALSE) %>%
  mutate(Month = yearmonth(Label)) %>%
  dplyr::select(Month, Value) %>%
  as_tsibble(index = Month)

us_economy <- read_csv("CPIAUCNS.csv", show_col_types = FALSE) %>%
  mutate(Month = yearmonth(observation_date)) %>%
  dplyr::select(CPIAUCNS, Month) %>%
  as_tsibble(index = Month)

food_prices_adj <- food_prices %>%
  left_join(us_economy, by = "Month") %>%
  mutate(Adjusted_Value = Value / CPIAUCNS * 100) %>%
  dplyr::select(Month, Adjusted_Value) %>%
  na.locf()

stock_xts <- getSymbols("^SP500-302020",
                        src = "yahoo",
                        from = "2000-01-07",
                        to = "2025-01-31",
                        auto.assign = FALSE)

stock_close <- Cl(stock_xts)

# Fill missing dates
all_dates <- seq.Date(from = as.Date("2000-01-07"), to = as.Date("2025-01-31"), by = "day")
stock_data_full <- merge(stock_close, xts(, all_dates))
colnames(stock_data_full) <- "Close"
stock_data_filled <- na.locf(stock_data_full)

# Convert to tibble and then tsibble
stock_df <- stock_data_filled %>%
  fortify.zoo() %>%
  as_tibble() %>%
  rename(Date = Index) %>%
  mutate(Date = as_date(Date)) %>%
  as_tsibble(index = Date)

ui <- navbarPage("Global Food Price Dashboard",
                 
                 # 1. Food Price Index
                 tabPanel("USA Average Food Price Index",
                          sidebarLayout(
                            sidebarPanel(
                              style = "position: -webkit-sticky; position: sticky; top: 0;",  # Sticky sidebar style
                              dateRangeInput("food_date_range", "Select date range:",
                                             start = as.Date("2000-01-01"),
                                             end = as.Date("2025-01-01"),
                                             min = as.Date("2000-01-01"),
                                             max = as.Date("2025-01-01")),
                              br(),  # Adds some space between elements
                              h4("Dataset Description:"),
                              p("This dataset contains the monthly food price index data adjusted for inflation using the Consumer Price Index (CPI) from 2000 to 2025. The food prices are represented as adjusted values to provide a clearer picture of price trends over time."),
                              
                              br(),  # Adds space between the description and insights
                              h4("Dataset Insights:"),
                              p("1. The US Food and Beverage dataset shows a clear upward trend in the cost of food and beverages from 2000 to present."),
                              p("2. In looking at the unadjusted data the trend appears significant and constant; however, when data is adjusted for inflation the trend is still upward but appears to have more cyclical movement."),
                              p("3. While the decomposition graphs of both the unadjusted and adjusted data show what appears to be significant seasonality in the data, the seasonality is somewhat minor."),
                              p("4. When the seasonal subseries plot is looked at in the unadjusted data, there appears to be almost no variation in monthly costs even though you would expect to see that with seasonally impacted data."),
                              p("5. The seasonality is a bit more noticeable when the seasonal subseries plot is shown for the adjusted for inflation data. Here it becomes apparent that food costs are lower in the summer months and higher in the winter months.")
                            ),
                            mainPanel(
                              plotOutput("food_ts_plot"),
                              plotOutput("food_decomp_plot"),
                              plotOutput("food_seasonal_plot")
                            )
                          )
                 ),
                 
                 # 2. Stock Market Index
                 tabPanel("S&P500 Food and Beverage Index",
                          sidebarLayout(
                            sidebarPanel(
                              dateRangeInput("stock_date_range", "Select date range:",
                                             start = as.Date("2000-01-01"),
                                             end = as.Date("2025-01-01"),
                                             min = as.Date("2000-01-01"),
                                             max = as.Date("2025-01-01")),
                              br(),  # Adds space
                              h4("Dataset Description:"),
                              p("This dataset contains the historical daily closing prices of the S&P 500 Food and Beverage Index. The data is sourced from Yahoo Finance and spans from 2000 to 2025. It offers insights into the stock market performance of companies involved in the food sector."),
                              
                              br(),  # Adds space between description and insights
                              h4("Dataset Insights:"),
                              p("1. As we would expect from data on a stock index, there is a clear upward trend over the last 25 years. This would be seen by almost all sectors of the S&P500, where there is fluctuation within date ranges, but over a long time horizon (i.e. 20+ years) you will almost certainly see an increasing trend."),
                              p("2. The decomposition showed that there is some seasonality to the stock index which isn’t as pronounced when looking at the seasonality chart."),
                              p("3. There is some seasonality in the data seen by downturns around July and increase in December."),
                              p("4. Volatility is evident in the data, particularly during financial crises, showing how external factors like economic shocks and global events can heavily impact stock prices.")
                            ),
                            mainPanel(
                              plotOutput("stock_ts_plot"),
                              plotOutput("stock_decomp_plot"),
                              plotOutput("stock_seasonal_plot")
                            )
                          )
                 ),
                 
                 # 3. Forecasting
                 tabPanel("Forecasting",
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("n_months", "Months to predict:", value = 12, min = 1, max = 60),
                              
                              br(),  # Adds space between description and insights
                              h2("Model Results Summary"),
                              p("Here's a summary of the performance metrics from each forecasting model:"),
                              tags$ul(
                                tags$li("ARIMA – RMSE: 0.35, MAE: 0.26, MAPE: 333.6"),
                                tags$li("ETS – RMSE: 0.42, MAE: 0.32, MAPE: 530.5"),
                                tags$li("Random Forest – RMSE: 1.62, MAE: 1.29, MAPE: 1.26"),
                                tags$li("VAR – RMSE: 0.37, MAE: 0.33, MAPE: 0.32")
                              ),
                              p("Based on performance and practical applicability, the VAR model offers the best balance between accuracy and interpretability."),
                              p("Random Forest appears to be a decent option, and we may be able to improve upon it by using more predictor variables or tuning it. But we would have to develop a model that accurately predicts the future stock data and then use those as values to input to forecast into the future."),
                              p("ETS was by far the worst."),
                              p("ARIMA had the best RMSE and MAE but the MAPE was really bad.")
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("ARIMA", plotOutput("arima_forecast")),
                                tabPanel("ETS", plotOutput("ets_forecast")),
                                tabPanel("VAR", plotOutput("var_forecast")),
                                tabPanel("Random Forest", plotOutput("rf_forecast"))
                              )
                            )
                          )
                 ),
                 
                 # 4. About
                 tabPanel("About",
                          fluidPage(
                            h2("About the Project"),
                            h3("Project Objectives"),
                            p("The fluctuation of food prices can have a toll on a lot of people in the USA and around the world. Except for housing, the cost of food is one of the biggest expenses for most people. When we look outside of the middle-class in the USA – who can afford small fluctuations in food prices – we see that even a small fluctuation can cause difficulties for the underprivileged as well as food banks. 
In this project we want to find a way to predict the food prices around the world using stock markets. We have found a dataset that shows average monthly food prices in the USA from 2000 to 2025. We plan to overlay this with the S&P500 Food and Beverage Stock Index to explore if there is any way to predict the price of food based on changes in the stock market. 
")
                            ),
                            hr(),
                            h3("Datasets"),
                            tags$ul(
                              tags$li("US average Food and Beverage Cost - Provided by the US Bureau of Labor Statistics"),
                              tags$li("S&P Food & Beverage Index - Retrieved through Yahoo Finance")
                            ),
                            hr(),
                            h3("Team Members"),
                            tags$ul(
                              tags$li(strong("Jaimie Cairns")),
                              tags$li(strong("Morgan Hager-Perry")),
                              tags$li(strong("Dustin Cole"))
                            ),
                            hr(),
                            p(em("Last updated: March 26, 2025"))
                          )
                 
)


server <- function(input, output, session) {
  
  # --- Prepare Stock and Food Data ---
  # Merge stock and food price data
  merged_df <- reactive({
    stock_data_filled_df <- data.frame( 
      Month = index(stock_data_filled), 
      Close = as.vector(stock_data_filled$Close)
    )
    
    stock_data_filled_df$Month <- as.Date(stock_data_filled_df$Month)
    
    # Get last day of each month for stock data 
    stock_data_filled_df <- stock_data_filled_df %>%
      group_by(YearMonth = format(Month, "%Y %b")) %>%
      filter(Month == max(Month)) %>%
      ungroup() %>%
      dplyr::select(Month = YearMonth, Close)
    
    # Convert food_prices_adj from tibble to a data frame
    food_prices_adj_df <- as.data.frame(food_prices_adj)
    food_prices_adj_df$Month <- as.character(food_prices_adj_df$Month)
    
    # Merge the datasets
    merged_df <- merge(stock_data_filled_df, food_prices_adj_df, by = "Month")
    
    # Create a merged tibble
    merged_df <- merged_df %>%
      mutate(Month = yearmonth(Month)) %>%
      as_tsibble(index = Month)
    
    return(merged_df)
  })
  
  
  # --- Food Price Index ---
  filtered_food_data <- reactive({
    food_prices_adj %>%
      filter(Month >= yearmonth(input$food_date_range[1]) &
               Month <= yearmonth(input$food_date_range[2]))
  })
  
  output$food_ts_plot <- renderPlot({
    filtered_food_data() %>%
      autoplot(Adjusted_Value) +
      labs(title = "US Average Food Prices", y = "USD", x = "Month") +
      theme(plot.title = element_text(size = 18, face = "bold"))
  })
  
  output$food_seasonal_plot <- renderPlot({
    filtered_food_data() %>%
      gg_season(Adjusted_Value, labels = "both") +
      labs(title = "Food Price Seasonality - Inflation Adjusted", y="USD") +
      theme(plot.title = element_text(size = 18, face = "bold"))
  })
  
  output$food_decomp_plot <- renderPlot({
    ts_data <- filtered_food_data()
    
    ts_data %>%
      model(decomp = classical_decomposition(type = "additive")) %>%
      components() %>%
      autoplot() +
      labs(title = "Classical additive decomposition of US Food & Beverage Prices") +
      theme(plot.title = element_text(size = 18, face = "bold"))
  })
  
  # --- Stock Market Index ---
  filtered_stock_data <- reactive({
    stock_df %>%
      filter(Date >= input$stock_date_range[1] &
               Date <= input$stock_date_range[2])
  })
  
  output$stock_ts_plot <- renderPlot({
    filtered_stock_data() %>%
      autoplot(Close) +
      labs(title = "S&P500 Food and Beverage Stock Index Closing Prices", y = "USD", x = "Date") + 
      theme(plot.title = element_text(size = 18, face = "bold"))
  })
  
  output$stock_seasonal_plot <- renderPlot({
    filtered_stock_data() %>%
      gg_season(Close, labels = "both") +
      labs(title = "Seasonal Plot: Stock Index", y= "USD") + 
      theme(plot.title = element_text(size = 18, face = "bold"))
  })
  
  output$stock_decomp_plot <- renderPlot({
    # Filter and prepare the stock data for decomposition
    stock_data_for_decomp <- filtered_stock_data()
    
    # Ensure that stock data is in 'ts' (time series) format
    stock_ts <- ts(stock_data_for_decomp$Close, frequency = 365, start = c(year(min(stock_data_for_decomp$Date)), month(min(stock_data_for_decomp$Date))))
    
    # Perform the decomposition using decompose
    decomposed <- decompose(stock_ts, type = "additive")
    
    # Plot the decomposition directly
    plot(decomposed)
  })
  
  # --- Forecast Models ---
  
  output$arima_forecast <- renderPlot({
    model <- auto.arima(merged_df()$Adjusted_Value)
    fcast <- forecast(model, h = input$n_months)
    autoplot(fcast) +
      labs(title = paste("ARIMA Forecast for", input$n_months, "Months"))
  })
  
  output$ets_forecast <- renderPlot({
    ts_data <- ts(merged_df()$Adjusted_Value, frequency = 12) 
    model <- ets(ts_data)
    fcast <- forecast(model, h = input$n_months)
    autoplot(fcast) +
      labs(title = paste("ETS Forecast for", input$n_months, "Months"))
  })
  
  output$var_forecast <- renderPlot({
    df <- merged_df() %>%
      as_tibble() %>%
      drop_na()
    
    # Create time series with monthly frequency
    df_ts <- ts(df[, c("Adjusted_Value", "Close")], frequency = 12, start = c(year(min(df$Month)), month(min(df$Month))))
    
    # Fit VAR model
    var_model <- vars::VAR(df_ts, p = 1)
    
    # Forecast
    fcast <- predict(var_model, n.ahead = input$n_months)
    
    # Extract actuals and forecast
    actuals <- as.data.frame(df_ts)
    actuals$Date <- yearmonth(min(df$Month)) + 0:(nrow(actuals) - 1)
    
    predicted <- as.data.frame(fcast$fcst$Adjusted_Value[, 1])
    predicted$Date <- yearmonth(max(actuals$Date)) + 1:input$n_months
    
    actuals$Type <- "Actual"
    predicted$Type <- "Forecast"
    colnames(predicted)[1] <- "Adjusted_Value"
    
    plotdf <- rbind(
      actuals[, c("Date", "Adjusted_Value", "Type")],
      predicted[, c("Date", "Adjusted_Value", "Type")]
    )
    
    # Plot
    ggplot(plotdf, aes(x = Date, y = Adjusted_Value, color = Type)) +
      geom_line(size = 1) +
      labs(
        x = "Month",
        y = "Adjusted Value",
        title = paste("VAR Forecast -", input$n_months, "Months Ahead")
      ) +
      theme_minimal() +
      scale_color_manual(values = c("Actual" = "black", "Forecast" = "red"))
  })
  
  output$rf_forecast <- renderPlot({
    df <- merged_df()
    df$Close <- lag(df$Adjusted_Value, 1)
    df <- na.omit(df)
    
    set.seed(123)
    train_idx <- sample(seq_len(nrow(df)), 0.8 * nrow(df))
    train <- df[train_idx, ]
    test <- df[-train_idx, ]
    
    model <- randomForest(Adjusted_Value ~ Close, data = train)
    pred <- predict(model, test)
    
    ggplot() +
      geom_point(aes(x = test$Adjusted_Value, y = pred), color = "blue") +
      geom_abline(slope = 1, intercept = 0, color = "red") +
      labs(x = "Actual", y = "Predicted", title = "Random Forest: Actual vs Predicted")
  })
  
}

shinyApp(ui, server)