# Install packages if missing
list.of.packages <- c("parallel", "doParallel", "GA", "MASS")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load libraries

library(GA)
library(MASS)

Boston

#split data into train, valid, test, with prob 0.6, 0.2, 0.2
set.seed(20)
idx_split = sample(1:3, nrow(Boston), prob = c(0.6,0.2,0.2), replace = TRUE)

train_data = Boston[idx_split == 1, ]
valid_data = Boston[idx_split == 2, ]
test_data = Boston[idx_split == 3, ]

custom_fitness <- function(vars, train_data, valid_data){
  
  q_vars=sum(vars) #number of selected variables
  
  #get all columns names
  names = colnames(train_data)
  names = names[names != "medv"]
  # keep only vars from current solution
  if(q_vars != 0){ 
    # get the columns of the current solution
    names = names[vars==1]
  }else{
    names = "."
  }
  
  #train linear model
  formu = as.formula(paste("medv", "~", paste(names, collapse = "+")))
  model = lm(formu, data = as.data.frame(train_data))
  
  pred_valid = predict(model, newdata = as.data.frame(valid_data))
  
  mse = mean((pred_valid - valid_data$medv)^2)
  
  fitness_value = -mse
  
  return(fitness_value)
}

# GA parameters
param_nBits=ncol(Boston)-1
col_names= colnames(Boston)[colnames(Boston) != "medv"]
# Executing the GA 
ga_solver = ga(fitness = function(vars) custom_fitness(vars = vars, 
                                                       train_data = train_data,
                                                       valid_data = valid_data), # custom fitness function
             type = "binary", # optimization data type
             elitism = 3, # number of best ind. to pass to next iteration
             pcrossover = 0.8, # crossover rate prob(default)
             pmutation = 0.1, # mutation rate prob(default)
             popSize = 50, # the number of indivduals/solutions
             nBits = param_nBits, # total number of variables
             names=col_names, # variable name
             run = 100, #the number of consecutive generations without any improvement
             monitor= TRUE, # plot the result at each iteration
             parallel = T, # allow parallel proccesing
             seed = 1)
?ga
summary(ga_solver)

best_vars_ga = col_names[ga_solver@solution[1,]==1]

best_vars_ga

#fit a lm with selected variables
formu = as.formula(paste("medv~", paste(best_vars_ga, collapse = "+")))

model = lm(formula = formu, data = as.data.frame(train_data))

#get the MSE of test set
pred_test = predict(model, newdata = as.data.frame(test_data))
test_mse = mean((pred_test - test_data$medv)^2)
test_mse
