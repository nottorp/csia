# PR 1
f1_a <- 2
f1_b <- 12
f1 <- function(x) {
  return (2^x)
}
f1_label = sprintf("f(x)=2^x, x in [%d, %d]", f1_a, f1_b)
plot(f1, main=f1_label, from=f1_a, to=f1_b)

# PR 2
for(p in seq(from=0.1, to=0.9, by=0.1)) {
  distro = dbinom(1:20, prob=p, size=20)
  label = sprintf("Binomial distro, prob=%f", p)
  fname = sprintf("binomial_prob_0_%d.pdf", as.integer(p * 10))
  pdf(fname, 8.27, 5.83) # A5
  plot(distro, main=label)
  curve(splinefun(seq(from=1, to=20), distro)(x), from=1, to=20, add=T, col="blue")
  dev.off()
}

# PR 3
# numsamples <- 3
numsamples <- 10000

#CLT <- function(n) {
#  sums = rep(0, numsamples)
#  for(i in seq(n)) {
#    current = runif(numsamples, min=-10, max=10)
#    print(current)
#    sums <- sums + current
#  }
#  
#  print(sums)
#  avgs = sapply(sums, function(x) return (x/n))
#  return (avgs)
#}

CLT <- function(n) {
  avgs <- rep(0, n)
  for(i in seq(n)) {
    curavg = mean(runif(numsamples, min=-10, max=10))
    avgs[i] <- curavg
  }
  return (avgs)
}

# print (CLT(1))
# print (CLT(5))
# print(CLT(10))
# print (CLT(100))
# values <- c(1, 5, 10, 100, 1000, 10000)
values <- c(1, 5, 10, 100, 1000, 10000)
col = 0.1
for(i in values) {
  label = sprintf("CLT test, %d samples", i)
  hist(CLT(i), main=label, col=rgb(1, 1 - col, 0, 1))
  col = col + 0.1
}
