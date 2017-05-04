ordo <- c()
absc <- c()
n <- 20000
i <- 1
while (i < n) {
  ordo[i] = i/n
  i = i + 1
}

h <- function(x) {
  return(log(1-x))
}

i <- 1
while (i < n) {
  absc[i] <- h(ordo[i])
  i = i + 1
}
plot(ordo, main="Graphe de probabilité de la loi géométrique", absc, xlab="ln(1 - i/n)", ylab="i/n")
print(lm(ordo ~ absc))