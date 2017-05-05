r <- 3
p <- 0.25
m <- 100
Rn <- c()
Pn <- c()

liste_n <- seq(100, 100100, 1000)
B_rn <- c()
B_pn <- c()
EQM_rn <- c()
EQM_pn <- c()
index <- 1
for (n in liste_n) {
  for (i in 1:m) {
    sample <- rnbinom(n,r,p)
    esti_rn <- (mean(sample)^2)/(mean(sample) + sd(sample)^2)
    esti_pn <- mean(sample)/(mean(sample) + sd(sample)^2)
    Rn[i] <- esti_rn
    Pn[i] <- esti_pn
  }
  biais_rn <- mean(Rn) - r
  biais_pn <- mean(Pn) - p
  eqm_rn <- sd(Rn)^2 + biais_rn^2
  eqm_pn <- sd(Pn)^2 + biais_pn^2
  B_rn[index] <- biais_rn
  B_pn[index] <- biais_pn
  EQM_rn[index] <- eqm_rn
  EQM_pn[index] <- eqm_pn
  index <- index + 1
  #print("biais rn : ")
  #print(biais_rn)
  #print("biais pn : ")
  #print(biais_pn)
  #print("erreur quadratique moyenne rn : ")
  #print(eqm_rn)
  #print("erreur quadratique moyenne pn : ")
  #print(eqm_pn)
}
par(mfrow=c(2,2))
plot(liste_n, B_rn, main="Biais de rn en fonction de n")
plot(liste_n, B_pn, main="Biais de pn en fonction de n")
plot(liste_n, EQM_rn, main="Erreur quadratique moyenne de rn en fonction de n")
plot(liste_n, EQM_pn, main="Erreur quadratique moyenne de pn en fonction de n")