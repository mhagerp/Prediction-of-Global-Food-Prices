#install.packages("quantmod")
library(quantmod)

getSymbols("^SP500-302020",
           from = "2010-01-01",
           to = "2023-12-31")

stock_database <- `SP500-302020`

plot(stock_database$`SP500-302020.Close`)
