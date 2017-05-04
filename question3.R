p <- 0.25
n <- 5000
ordo <- seq(1/n,1,by=1/n)
absc <- sort(rgeom(n,p))

h <- function(x) {
  return(log(1-x))
}

i <- 1
while (i < n) {
  ordo[i] <- h(ordo[i])
  i = i + 1
}

plot(absc, ordo, main="Graphe de probabilité de la loi géométrique", xlab="xi*", ylab="ln(1-i/n)")
print(lm(ordo ~ absc))
print("pente : ")
print((1-exp(lm(ordo~absc)$coefficients[2])))