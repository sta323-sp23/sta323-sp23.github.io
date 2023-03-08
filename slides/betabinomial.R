library(tidyverse)
########
# data #
########

# 6115 families have 12 children each
# N = 6115 binomial experiments
# n = 12 for each binomial experiment
# p constant across experiments
# m = # males

x = data.frame(families = c(3, 24, 104, 286, 670, 1033, 1343, 1112,
                            829, 478, 181, 45, 7),
               m = seq(0, 12))

y = rep(x$m, x$families) # data as we will use it
N = sum(x$families)
N = length(y)
n = 12

############################# #### #############################
############################# NOTE #############################
# from this point on, x will be the iterate of Newton's method #
##########---------- and y will be the data ---------###########
############################# #### #############################

###############################
# multivariate Newton-Raphson #
###############################
newton = function(f, Jinverse, x, tol) {
  for (i in 1:500) {
    x = x - Jinverse(x) %*% f(x)
    if (sum(abs(f(x))) < tol) {
      return(x)
    }
  }
  return(x)
}

########################## 
# objective and Jacobian #
##########################
f = function(x) {
  stopifnot(length(x) == 2)
  alpha = x[1]
  beta = x[2]
  
  gradAlpha = N * (digamma(alpha + beta) - digamma(alpha) - 
                     digamma(n + alpha + beta)) +
    sum(digamma(y + alpha))
  gradBeta = N * (digamma(alpha + beta) - digamma(beta) - 
                    digamma(n + alpha + beta)) + 
    sum(digamma(n - y + beta))

  x = c(gradAlpha, gradBeta)
  
  return(x)
}

Jinverse = function(x) {
  stopifnot(length(x) == 2)
  alpha = x[1]
  beta = x[2]
  
  j11 = N * (trigamma(alpha + beta) - trigamma(alpha) - 
                     trigamma(n + alpha + beta)) +
    sum(trigamma(y + alpha))
  j22 = N * (trigamma(alpha + beta) - trigamma(beta) - 
                    trigamma(n + alpha + beta)) + 
    sum(trigamma(n - y + beta))
  
  j12 = N * (trigamma(alpha + beta) - trigamma(n + alpha + beta))
  
  J = matrix(data = c(j11, j12, j12, j22), nrow = 2, ncol = 2)
  return(solve(J))
}

# need to start pretty close
newton(f, Jinverse, x = c(28, 30), tol = 1e-10)

######################
# Method of Moments ##
### starting point ###
######################

m1 = mean(y)
m2 = sum(y ^ 2) / N

alpha0 = ((n * m1) - m2) / (n * ((m2 / m1) - m1 - 1 ) + m1)
beta0  = ((n - m1) * (n - (m2/m1))) / ((n * ((m2/m1) - m1 - 1)) + m1)


mle = newton(f, Jinverse, x = c(alpha0, beta0), tol = 1e-10)

cat("Method of moments:", 
    "\n alpha_MM = ", alpha0,
    "\n beta_MM = ", beta0,
    "\nMaximum Likelihood Estimates:\n",
    mle)

alpha = mle[1]
beta = mle[2]

############
#Visualize #
############

bb = function(x, n, a, b) {
  N * choose(n, x) * beta(x + a, n - x + b) / beta(a, b)
}

BetaBinomial = bb(0:12, n, alpha, beta)
# notes #
# bb is the pmf of the beta-binomial, i.e. p(x_i | alpha, beta)
# notice bb returns probability of individual x_i
# we scale by number of observations to get a bar graph of mass
# the smooth density line drawn by "geom_line" connects
# the points.

phat = mean(y) / 12 # for binomial
Binomial = N * dbinom(0:12, size = 12, prob = phat)

df = data.frame(y) %>%
  count(y) %>%
  mutate(BetaBinomial = BetaBinomial,
         Binomial = Binomial) %>%
  mutate(data = n) %>%
  pivot_longer(cols = 3:4, names_to = "type", values_to = "points") %>%
  select(y, type, points)

df2 = data.frame(y)

df %>%
  ggplot() +
  geom_line(aes(x = y, y = points, color = type)) +
  geom_histogram(data = df2, aes(x = y), bins = 25, alpha = 0.5) +
  labs(y = "Count",
       title = "Beta binomial and binomial models") +
  theme_bw() +
  scale_x_continuous(breaks =
                       seq(from = 0, to = 12, by = 1))

