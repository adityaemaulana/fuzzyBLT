# Inference
## Calculate membership degrees for each 
## fuzzy sets of solution membership using MIN-MAX Method

## In this case the solution membership is ACCEPTANCE with fuzzy sets YES, MAYBE, and NO
infer <- function(input){
  degree.y <- matrix(c(pmin(input$el, input$df), pmin(input$el, input$dm), 
                       pmin(input$el, input$dp), pmin(input$el, input$dpr), 
                       pmin(input$ela, input$dm), pmin(input$ela, input$dp), 
                       pmin(input$ela, input$dpr),
                       pmin(input$ea, input$dpr), pmin(input$eha, input$dpr)), 
                     ncol = 9)

  degree.m <- matrix(c(pmin(input$el, input$dfr), pmin(input$ela, input$dfr), 
                       pmin(input$ela, input$df), pmin(input$ea, input$dm), 
                       pmin(input$eha, input$dp),
                       pmin(input$ea, input$dm), pmin(input$ea, input$dp)), 
                     ncol = 7)
  
  degree.n <- matrix(c(pmin(input$ea, input$dfr), pmin(input$ea, input$df),
                       pmin(input$eha, input$dfr), pmin(input$eha, input$df),
                       pmin(input$eha, input$dm), pmin(input$eh, input$dfr), 
                       pmin(input$eh, input$dm), pmin(input$eh, input$df), 
                       pmin(input$eh, input$dp)), 
                     ncol = 9)
  
  
  ## Calculate MAX predicates for each fuzzy sets
  predicate.y <- c(rowMax(degree.y))
  predicate.m <- c(rowMax(degree.m))
  predicate.n <- c(rowMax(degree.n))

  return(list("predy" = predicate.y, "predm" = predicate.m, "predn" = predicate.n))
}
