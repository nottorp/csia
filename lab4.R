victim = read.table("house.dat", header=TRUE)
#print (victim)
data = as.matrix(victim[,-1])
y = victim$PRICE
n = ncol(data)
m = nrow(data)

rssfull = sum(resid(lm(y ~ data))^2)
s2full = rssfull / (m - n - 1)

oneRegression <- function(y, data, colidx, n, m, p)
{
  features = data[, colidx]
  reg = lm(y ~ features)
  #print(summary(reg))
  rss = sum(resid(reg) ^ 2)
  tss = sum((y - mean(y))^2)
  detcoef = 1 - rss/tss
  adjdetcoef = 1 - (rss / (m - n - 1)) / (tss / (m - 1))
  cp = ((rss / s2full) - (m - 2 * (p + 1)))
  return (c(rss, detcoef, adjdetcoef, cp))
}

brss <- c()
br2 <- c()
br2a <- c()
bcp <- c()

bbrss = NA
bbr2 = NA
bbr2a = NA
bbcp = NA

for (p in 1:n)
{
  colidxlist = combn(n, p)
  bestrss = NA
  bestdetcoef = NA
  bestadjcoef = NA
  bestcp = NA
  for (i in 1:ncol(colidxlist))
  {
    colidx = c(colidxlist[, i])
    #print (colidx)
    res = oneRegression(y, data, colidx, n, m, p)
    #print(res)
    #print(bestrss)
    #print(res[1])
    if (is.na(bestrss) || (bestrss > res[1]))
    {
      bestrss = res[1]
      bestrssidx = colidx
    }
    if (is.na(bestdetcoef) || (bestdetcoef < res[2]))
    {
      bestdetcoef = res[2]
      bestdetcoefidx = colidx
    }
    if (is.na(bestadjcoef) || (bestadjcoef < res[3]))
    {
      bestadjcoef = res[3]
      bestadjcoefidx = colidx
    }
    
    if (is.na(bestcp) || (abs(bestcp - (p+1)) > abs(res[4] - (p + 1))))
    {
      bestcp = res[4]
      bestcpidx = colidx
    }

  }
  print("===============================================================================")
  cat("For ", p, " features:\n")
  cat("Best RSS is ", bestrss, " for columns ", colnames(data)[bestrssidx], "\n")
  cat("Best R2 is ", bestdetcoef, " for columns ", colnames(data)[bestdetcoefidx], "\n")
  cat("Best R2 adjusted is ", bestadjcoef,  " for columns ", colnames(data)[bestadjcoefidx], "\n")
  cat("Best CP is ", bestcp, " for columns ", colnames(data)[bestcpidx], "\n")
  print("*******************************************************************************")
  
  brss <- append(brss, bestrss)
  br2 <- append(br2, bestdetcoef)
  br2a <- append(br2a, bestadjcoef)
  bcp <- append(bcp, bestcp)
  
  if (is.na(bbrss) || (bbrss > brss))
  {
    bbrss = brss
    bbrss_cols = bestrssidx
  }
  if (is.na(bbr2) || (bbr2 < bestdetcoef))
  {
    bbr2 = bestdetcoef
    bbr2_cols = bestdetcoefidx
  }
  #if (is.na(bbr2a) || (bbr2a < bestadjcoef))
  #{
  #  bbr2a = bestadjcoef
  #  bbr2a_cols = co
  #}
  
  #if (is.na(bestcp) || (abs(bestcp - (p+1)) > abs(res[4] - (p + 1))))
  #{
  #  bestcp = res[4]
  #  bestcpidx = colidx
  #}
  
}

normalize <- function(x)
{
  return((x - min(x)) / (max(x) - min(x)))
}

brssnorm <- normalize(brss)
bcpnorm <- normalize(bcp)

print (brss)
print (brssnorm)

#plot((1:13), brssnorm, type="b", xlab="")
#par(new=TRUE)
#plot((1:13), br2, type="b")

matplot((1:13), cbind(brss, br2, br2a, bcp), type="b", col=1:4)
# matplot((1:13), cbind(brssnorm, br2, br2a, bcpnorm), type="b", col=1:4)
legend("right", c("RSS (normalized)", "R2", "R2adj", "CP"), pch=1, col=1:4)
