###European put pricing

S0 <- 38586.18  #Nikkei 19/12/1989 
K <- 38586.18   #Strike price
r1 <- 0.06375   #risk-free rate year 1
r2 <- 0.0585    #risk-free rate year 2 & 3
Dy <- 0.0049    #dividend yields
s <- 0.1360     #implied volatility 
t <- 3          #maturity
N <- 100000     #fractional periods
share <- 0.2    #fraction of the put
FX <- 144.28    #exchange rate


D1 <- (log(S0/K) + ((r2-Dy + (s**2 / 2)) * t)) / (s * sqrt(t))
D2 <- (log(S0/K) + ((r2-Dy - (s**2 / 2)) * t)) / (s * sqrt(t))

value.eu <- (K * exp(-r2*t)*pnorm(-D2,0,1) - S0*exp(-Dy*t)*pnorm(-D1,0,1))*share/FX

cat("The intrinsic value of the european put option:", round(value.eu,4),"$")


