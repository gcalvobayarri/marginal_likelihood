# 1. MODEL SAMPLING----------------------
# Y_i ~ N(mu, sigma2)

n <- 50
y <- rnorm(n,5,sd=1)

# Prior distributions: mu ~ N(0, 10^2), tau ~ gamma((alpha)shape=0.01, rate=0.01)

mu_0 <- 0; tau_0 <- 1/100; alpha <- 0.01; beta <- 0.01

# Complete conditional densities
#  1. (mu| tau, y) ~ N( (tau_0 * mu_0 + tau * sum(y)) / (tau_0 + n * tau) ,
#  (tau_0 + n * tau) ) # aqui en forma de precisión
#  2. (tau|mu, t) ~ Gamma(alpha + n/2, beta + sum((y-mu)^2)/2)


# 2. GIBBS SAMPLING--------------------
mu <- c(); mu[1] <- 1
tau <- c(); tau[1] <- 5 # valores iniciales de los parámetros

iter <- 1500
for(i in 2 : iter){
  tau[i] <- rgamma(1, shape = alpha + n/2, rate = beta + sum((y-mu[i-1])^2)/2)
  mu[i] <- rnorm(1, (tau_0 * mu_0 + tau[i] * sum(y)) / (tau_0 + n * tau[i]), 
                 sd = 1/sqrt(tau_0 + n * tau[i]))
}


plot(mu)
plot(tau)
