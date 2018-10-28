plot.debt.mf <- function(){
  # Debt Membership Function
  data.debt <- data.frame(
    Nilai <- debt.fewer(seq(0, 100, by=0.1)),
    b <- debt.few(seq(0, 100, by=0.1)),
    c <- debt.moderate(seq(0, 100, by=0.1)),
    d <- debt.plenty(seq(0, 100, by=0.1)),
    e <- debt.plentier(seq(0, 100, by=0.1)),
    Hutang <- seq(0, 100, by=0.1)
  )
  
  debt.plot <- plot_ly(data.debt, y = ~Nilai, x = ~Hutang, name = 'debt fewer', type='scatter', mode = 'lines')%>%
    add_trace(y = ~b, name = 'debt few', mode = 'lines')%>%
    add_trace(y = ~c, name = 'debt moderate', mode = 'lines')%>%
    add_trace(y = ~d, name = 'debt plenty', mode = 'lines')%>%
    add_trace(y = ~e, name = 'debt plentier', mode = 'lines')
  
  debt.plot
}

plot.earning.mf <- function(){
  # Earning Membership Function
  data.earning <- data.frame(
    Nilai <- earning.low(seq(0, 2, by=0.1)),
    b <- earning.lowaverage(seq(0, 2, by=0.1)),
    c <- earning.average(seq(0, 2, by=0.1)),
    d <- earning.highaverage(seq(0, 2, by=0.1)),
    e <- earning.high(seq(0, 2, by =0.1)),
    Pendapatan <- seq(0, 2, by=0.1))
  
  earning.plot <- plot_ly(data.earning, y = ~Nilai, x = ~Pendapatan, name = 'earning low', type='scatter', mode = 'lines') %>%
    add_trace(y = ~b, name = 'earning low-avg', mode = 'lines')%>%
    add_trace(y = ~c, name = 'earning avg', mode = 'lines')%>%
    add_trace(y = ~d, name = 'earning high-avg', mode = 'lines')%>%
    add_trace(y = ~e, name = 'earning high', mode = 'lines')
    
  earning.plot
}

plot.result <- function(res){
  data.res <- data.frame(
    x <- res$Earning,
    y <- res$Debt
  )
  
  cols <- c(rep("red", 20), rep("blue", 80))
  
  p <- plot_ly(data.res, y = ~x, x = ~y, mode = 'markers', type = 'scatter',
               marker = list(color = cols))
  # Start plot
  p
}
