# Earning have 5 fuzzy sets : 
# low, ,low-average, average, high-average, high
earning.low <- function(x){
  ifelse(x < 0.2, 1, 
         ifelse((x >= 0.2 & x <= 0.6), (0.6 - x)/0.4, 
                0)) # x > 0.6
}

earning.lowaverage <- function(x){
  ifelse((x < 0.2 | x > 1), 0,
         ifelse((x >= 0.2 & x <= 0.6), (x - 0.2)/0.4,
                (1 - x)/0.4)) # x > 0.6 and x <= 1
}

earning.average <- function(x){
  ifelse((x < 0.6 | x > 1.4), 0,
         ifelse((x >= 0.6 & x < 1), (x - 0.6)/0.4,
                (1.4 - x)/0.4)) #x >= 1.4 and x <= 1
}

earning.highaverage <- function(x){
  ifelse((x < 1 | x > 1.8), 0,
         ifelse((x >= 1 & x <= 1.4), (x - 1)/0.4,
                (1.8 - x)/0.4)) # x > 1.4 and x <= 1.8
}

earning.high <- function(x){
  ifelse(x < 1.4, 0,
         ifelse((x >= 1.4 & x <= 1.8), (x - 1.4)/0.4,
                1)) # x > 1.8
}


# Debt have 5 fuzzy sets : 
# fewer, few, moderate, plenty, plentier
debt.fewer <- function(x){
  ifelse(x < 10, 1,
         ifelse((x >= 10 & x <= 30), (30 - x)/20,
                0)) # x > 30
}

debt.few <- function(x){
  ifelse((x < 10 | x > 50), 0,
         ifelse((x >= 10 & x < 30), (x - 10)/20,
                (50 - x)/20)) # x >= 30 & x <= 50
}

debt.moderate <- function(x){
  ifelse((x < 30 | x > 70), 0,
         ifelse((x >= 30 & x < 50), (x - 30)/20,
                (70 - x)/20)) # x >= 50 & x <= 70
}

debt.plenty <- function(x){
  ifelse(x < 50 | x > 90, 0,
         ifelse((x >= 50 & x <= 70), (x - 50)/20,      
                (90 - x)/20)) # x > 70 & x <= 90
}

debt.plentier <- function(x){
  ifelse(x < 70, 0, 
         ifelse((x >= 70 & x < 85), (x - 70)/15,
                1)) # x >= 80
}

calculate.earning <- function(column){
  list <- list("el" =earning.low(column), "ela" =earning.lowaverage(column),
                "ea" =earning.average(column), "eha" =earning.highaverage(column),
                "eh" =earning.high(column))
  return(list)
}

calculate.debt <- function(column){
  list <- list("df" = debt.few(column), "dfr" = debt.fewer(column),
            "dm" = debt.moderate(column), "dp" = debt.plenty(column),
            "dpr" = debt.plentier(column))
  return(list)
}