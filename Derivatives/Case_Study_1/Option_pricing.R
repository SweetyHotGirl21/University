value.am <- 2.5788
value.eu <- 1.679053

n.contract = 9500000

fee.legal <- 350000
commission <- 3000000
fee.denmark <- 1300000

fee.fixed <- (fee.legal + commission + fee.denmark)

fee.fixed.contract <- fee.fixed/n.contract
cat("The fixed costs per contract is :",round(fee.fixed.contract,4), "$")

### Hedging costs
cost.hedge.all <- 1
cost.hedge.80 <- 0.5

### All costs
cost.all.100 <- (fee.fixed.contract + cost.hedge.all)
cost.all.80 <- (fee.fixed.contract + cost.hedge.80)
cat("The costs per contract with 100% excahnge rate risk hedge:", round(cost.all.100,4))
cat("The costs per contract with 80% excahnge rate risk hedge:", round(cost.all.80,4))

min.price.am.100 <- (cost.all.100 + value.am)
min.price.am.80 <- (cost.all.80 + value.am)
cat("The minimum price per contract with 100% excahnge rate risk hedge:",min.price.am.100,"$")

min.price.eu.100 <- (cost.all.100 + value.eu)
min.price.eu.80 <- (cost.all.80 + value.eu)
cat("The minimum price per contract with 100% excahnge rate risk hedge:",min.price.eu.100,"$")

### Initial selling price and profit
price <- 5
profit.am <- price - min.price.am.100
cat("The profit of each ameriacen put with an price of 5$ is:",round(profit.am,4),"$")
margin.am <- profit.am/5
cat("The margin of each ameriacen put with an price of 5$ is:",round(margin.am*100,2),"%")

profit.eu <- price - min.price.eu.100
cat("The profit of each european put with an price of 5$ is:",round(profit.eu,4),"$")
margin.eu <- profit.eu/5
cat("The margin of each european put with an price of 5$ is:",round(margin.eu*100,2),"%")

