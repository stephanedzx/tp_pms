p <- 0.25
n <- 5000
m <- 1000 
moyenne_empirique <- c()

for (i in 1:m) {
  sample <- sort(rgeom(n,p))
  moyenne_empirique[i] <- mean(sample)
}
x <- seq(1,m)
# laisser les 2 prochaines lignes décommentée pour avoir l'histogramme
hist(moyenne_empirique, proba = TRUE)
curve(dnorm(x, mean(moyenne_empirique), sd(moyenne_empirique)), add=TRUE, col='red')
# décommenter les 2 prochaines lignes pour avoir le graphe de probabilité
# qqnorm(moyenne_empirique)
# qqline(moyenne_empirique, col='red')