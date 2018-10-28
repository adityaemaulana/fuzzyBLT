# Defuzzyfication using Sugeno Model
calc.acc <- function(output){
  # Generate 3 randoms number for 3 sets, will be used for calculating acceptance
  # NO, MAYBE, YES
  acceptance <- 
    ((output$predy*70) + (output$predm*50) + (output$predn*30))/
    (output$predy + output$predm + output$predn)
  
  return(acceptance)
}
