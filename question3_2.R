p <- 0.25
n <- 10000
m <- 1000 
eps <- 0.1
N <- 0
esperance <- 1/p

for (i in 1:m) {
  sample <- sort(rgeom(n,p))
  moy <- mean(sample)
  if (abs(moy - esperance) >= eps) {
    N <- N + 1
  }
}
print("nb de fois où |moy - esperance| >= epsilon = ")
print(N)