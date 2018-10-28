library(qlcMatrix)
library(plotly)
source('R/member_function.R')
source('R/inference.R')
source('R/defuzzy.R')
source('R/plot.R')

# Read input data
data <- read.csv("data/DataTugas2.csv")
# Extract each column
earning.column <- c(data$Pendapatan)
debt.column <- c(data$Hutang)
no.column <- c(data$No)

# Calculate fuzzy input
fuzzy.earning <- calculate.earning(earning.column)
fuzzy.debt <- calculate.debt(debt.column)

fuzzy.input <- c(fuzzy.earning, fuzzy.debt)
# Calculate fuzzy output
fuzzy.output <- infer(fuzzy.input)

# Calculate acceptance
acceptance <- calc.acc(fuzzy.output)

# Put fuzzy result in a data.frame
result <- data.frame(
  No = no.column,
  Earning = earning.column,
  Debt = debt.column,
  Acceptance = acceptance
)

# Sort the result by its acceptance value
result.sorted <- result[order(-result$Accept),]
result.accepted <- result.sorted[c(1:20),]
print(result.accepted)

# Calculate accuracy
predict.acc <- data.frame(
  Debt = debt.column,
  Accuracy = debt.column / earning.column
) 
result.predict <- predict.acc[order(-predict.acc$Accuracy),]

res.acc <- result.predict$Debt %in% result.accepted$Debt
res.acc <- res.acc[c(1:20)]
cat("Accuracy: ", length(res.acc[res.acc==TRUE])/20 * 100)

# Export .csv file
write.csv(result.accepted$No, "TebakanTugas2.csv")
 
# Plot
plot.earning.mf()
plot.debt.mf()
plot.result(result.sorted)