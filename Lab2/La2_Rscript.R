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
  direction = "max",
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

# Try to maximize resualt
stub <- lp.control(lprob, sense = "max")
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

status_code <- solve(lprob)
get.objective(lprob)

get.variables(lprob)

# Sensitive
original_options <- options(digits = 3, scipen = 30)

sensitivity_results <- get.sensitivity.obj(lprob)

for (i in 1:length(sensitivity_results$objfrom)) {
  cat(paste("x", i, "\t| [", sensitivity_results$objfrom[i], ", ", sensitivity_results$objtill[i], "]\n", sep = ""))
}

get.sensitivity.rhs(lprob)

optimum$sens.coef.from
optimum$sens.coef.to