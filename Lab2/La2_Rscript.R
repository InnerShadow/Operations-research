library(lpSolve)

# Coefficients from min F(x)
Fun <- c(10, 4, 8, 6, 2, 3, 8, 2, 6, 6)

# Boarders (coefficients near limitations)
A <- rbind(c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0), # 1'st claster
           c(0, 0, 0, 0, 1, 1, 0, 0, 0, 0), # 2'nd claster
           c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1), # 3'th claster
           c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), # total sum = 1000
           c(10, 4, 8, 6, -2, -3, 0, 0, 0, 0), # сriterion of simultaneous completion of work
           c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), # sum data proceeding >= 700
           c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0), # x1 <= 700
           c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0), # x2 >= 700
           c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0), # x3 >= 700
           c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0), # x4 >= 700
           c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0), # x5 <= 700
           c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0), # x6 <= 700
           c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0), # x7 <= 700
           c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0), # x8 <= 700
           c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0), # x9 <= 700
           c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1)) # x10 <= 700

# The right sides

B <- c(400, 800, 600, 1000, 0, 700,
       700, 700, 700, 700, 700, 700, 700, 700, 700, 700)

# Signs

CD <- c("<=", "<=", "<=", "=", "=", ">=",
        "<=", "<=", "<=", "<=", "<=", "<=", "<=", "<=", "<=", "<=")

optimum <- lp(
  direction = "min",
  objective.in = Fun,
  const.mat = A,
  const.dir = CD,
  const.rhs = B,
  compute.sens = TRUE)
optimum

optimum$solution

library(lpSolveAPI)

# Create function with 10 features & 0 limitations
lprob <- make.lp(0, 10)

# Try to minimaze resualt
stub <- lp.control(lprob, sense = "min")
rm(stub)

add.constraint(lprob, c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0), "<=", 400)
add.constraint(lprob, c(0, 0, 0, 0, 1, 1, 0, 0, 0, 0), "<=", 800)
add.constraint(lprob, c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1), "<=", 600)
add.constraint(lprob, c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), "=", 1000)
add.constraint(lprob, c(10, 4, 8, 6, -2, -3, 0, 0, 0, 0), "=", 0)
add.constraint(lprob, c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), ">=", 700)
add.constraint(lprob, c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0), "<=", 700)
add.constraint(lprob, c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0), "<=", 700)
add.constraint(lprob, c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0), "<=", 700)
add.constraint(lprob, c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0), "<=", 700)
add.constraint(lprob, c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0), "<=", 700)
add.constraint(lprob, c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0), "<=", 700)
add.constraint(lprob, c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0), "<=", 700)
add.constraint(lprob, c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0), "<=", 700)
add.constraint(lprob, c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0), "<=", 700)
add.constraint(lprob, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1), "<=", 700)

set.objfn(lprob, c(10, 4, 8, 6, 2, 3, 8, 2, 6, 6))
lprob

# Solve 
status_code <- solve(lprob)
time_request_target <- get.objective(lprob)
time_request_target

time_request_variables <- get.variables(lprob)
time_request_variables

# Sensitive
original_options <- options(digits = 3, scipen = 30)

time_request_sensetive <- sensitivity_results <- get.sensitivity.obj(lprob)

for (i in 1:length(sensitivity_results$objfrom)) {
  cat(paste("x", i, "\t| [", sensitivity_results$objfrom[i], ", ", sensitivity_results$objtill[i], "]\n", sep = ""))
}

get.sensitivity.rhs(lprob)

optimum$sens.coef.from
optimum$sens.coef.to

# Dismiss сriterion of simultaneous completion of work
library(lpSolveAPI)

# Create function with 10 features & 0 limitations
lprob <- make.lp(0, 10)

# Try to minimaze resualt
stub <- lp.control(lprob, sense = "min")
rm(stub)

add.constraint(lprob, c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0), "<=", 400)
add.constraint(lprob, c(0, 0, 0, 0, 1, 1, 0, 0, 0, 0), "<=", 800)
add.constraint(lprob, c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1), "<=", 600)
add.constraint(lprob, c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), "=", 1000)
# add.constraint(lprob, c(10, 4, 8, 6, -2, -3, 0, 0, 0, 0), "=", 0)
add.constraint(lprob, c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), ">=", 700)
add.constraint(lprob, c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0), "<=", 700)
add.constraint(lprob, c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0), "<=", 700)
add.constraint(lprob, c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0), "<=", 700)
add.constraint(lprob, c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0), "<=", 700)
add.constraint(lprob, c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0), "<=", 700)
add.constraint(lprob, c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0), "<=", 700)
add.constraint(lprob, c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0), "<=", 700)
add.constraint(lprob, c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0), "<=", 700)
add.constraint(lprob, c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0), "<=", 700)
add.constraint(lprob, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1), "<=", 700)

set.objfn(lprob, c(10, 4, 8, 6, 2, 3, 8, 2, 6, 6))
lprob

# Solve 
status_code <- solve(lprob)
non_time_request_target <- get.objective(lprob)
non_time_request_target

non_time_request_varibles <- get.variables(lprob)
non_time_request_varibles

# Sensitive
original_options <- options(digits = 3, scipen = 30)

non_time_request_sensetive <- sensitivity_results <- get.sensitivity.obj(lprob)

for (i in 1:length(sensitivity_results$objfrom)) {
  cat(paste("x", i, "\t| [", sensitivity_results$objfrom[i], ", ", sensitivity_results$objtill[i], "]\n", sep = ""))
}

optimum$sens.coef.from
optimum$sens.coef.to


# n computer out of work
n <- 5

q_coefs <- c(10, 4, 8, 6, 2, 3, 8, 2, 6, 6)
cluster_1 <- c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
cluster_2 <- c(0, 0, 0, 0, 1, 1, 0, 0, 0, 0)
cluster_3 <- c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1)
whole_sum <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

cluster_1[n] <- 0
cluster_2[n] <- 0
cluster_3[n] <- 0
whole_sum[n] <- 0

# Create function with 10 features & 0 limitations
lprob <- make.lp(0, 10)

# Try to minimaze resualt
stub <- lp.control(lprob, sense = "min")
rm(stub)

add.constraint(lprob, cluster_1, "<=", 400)
add.constraint(lprob, cluster_2, "<=", 800)
add.constraint(lprob, cluster_3, "<=", 600)
add.constraint(lprob, whole_sum, "=", 1000)
add.constraint(lprob, whole_sum, ">=", 700)

for (i in 1:10) {
  this_computer <- rep(0, 10)
  this_computer[i] <- 1
  
  if (i != n){
    add.constraint(lprob, this_computer, "<=", 700)
  } else {
    add.constraint(lprob, this_computer, "=", 0)
  }
}

set.objfn(lprob, q_coefs)
lprob

# Solve 
status_code <- solve(lprob)
n_out_of_work_target <- get.objective(lprob)
n_out_of_work_target

n_out_of_work_varibles <- get.variables(lprob)
n_out_of_work_varibles

# Sensitive
original_options <- options(digits = 3, scipen = 30)

n_out_of_work_sensetive <- sensitivity_results <- get.sensitivity.obj(lprob)

for (i in 1:length(sensitivity_results$objfrom)) {
  cat(paste("x", i, "\t| [", sensitivity_results$objfrom[i], ", ", sensitivity_results$objtill[i], "]\n", sep = ""))
}

optimum$sens.coef.from
optimum$sens.coef.to