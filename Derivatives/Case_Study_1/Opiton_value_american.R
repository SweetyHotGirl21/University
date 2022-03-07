### American put Pricing

S0 <- 38586.18  #Nikkei 19/12/1989 
K <- 38586.18   #Strike price
r1 <- 0.06375   #risk-free rate year 1
r2 <- 0.0585    #risk-free rate year 2 & 3
Dy <- 0.0049    #dividend yields
s <- 0.1360     #implied volatility 
t <- 3          #maturity
N <- 10000     #fractional periods
share <- 0.2    #fraction of the put
FX <- 144.28    #exchange rate

t.frac <- t/N         #100000 fractions of time per year
mu <- 1-(Dy*t.frac)   #continuous dividend yield

###approximation of the intrinsic value with binomial tree

u <- exp(s * sqrt(t.frac))  #up factor
d <- 1 / u                  #down factor

#construction of the binomial tree
tree <- c(rep(0,N+1))
tree2 <- c()
for(j in seq(0,N)){
  tree2[j+1] <- (S0*mu*u**j) * mu*d**(N-j)
}
tree3 <- c(rep(K, N+1))
for(i in 0:N+1){
  tree[i] <- max((tree3[i]-tree2[i]),0)
}

#calculating the intrinsic value backwards, beginning t+3 to t: 
for(i in seq(N-1,0,-1)){
  if (i < N/t){
    R <- exp(r1*t.frac)    #continuous compounded discount factor
    p <- (R-d)/(u-d)      #risk neutral probability of up-state
    q <- 1-p              #risk neutral probability of down-state
    tree[1:N] <- exp(-r1*t.frac)*exp(Dy*t.frac)*(p*tree[1:N+1]+q*tree[1:N])
    tree2 <- tree2*u*mu
    #allowing for early exercise 
    for(j in seq(1,N+1)){
      tree[j] <- max(tree[j],-tree2[j]+tree3[j])
    }
  }else{
    R <- exp(r2*t.frac)  #continuous compounded discount factor
    p <- (R-d)/(u-d)    #risk neutral probability of up-state
    q <- 1-p            #risk neutral probability of down-state
    tree[1:N] <- exp(-r2*t.frac)*exp(Dy*t.frac)*(p*tree[1:N+1]+q*tree[1:N])
    tree2 <- tree2*u*mu
    #allowing for early exercise 
    for(j in seq(1,N+1)){
      tree[j] <- max(tree[j],-tree2[j]+tree3[j])
    }
  }
}

value.am <- tree[1]*share / FX    #adjustment with exchange rate and share
cat("The intrisic value of the american put option: ", round(value.am,4), "$")

