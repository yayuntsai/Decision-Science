
dataset <- data.frame(item = c("pocketknife", "beans", "potatoes", "unions", 
                               "sleeping bag", "rope", "compass"), 
                      survivalpoints = c(10, 20, 15, 2, 30, 10, 30),
                      weight = c(1, 5, 10, 1, 7, 5, 1))

weightlimit <- 20

evalFunc <- function(x) {
  current_solution_survivalpoints <- x %*% dataset$survivalpoints
  current_solution_weight <- x %*% dataset$weight
  if (current_solution_weight > weightlimit) #constraint
    return(0) else return(current_solution_survivalpoints)
}

library(GA)
ga_model = ga(fitness = evalFunc, type = "binary", popSize = 200,
              maxiter = 100, pmutation = 0.01,nBits = 7, names = dataset$item)

ga_model@solution
