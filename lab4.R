victim = read.table("house.dat", header=TRUE)
print (victim)
data = as.matrix(victim[,-1])
y = victim$PRICE
n = ncol(data)
m = nrow(data)

oneRegression <- function(y, data, colidx, n, m)
{
  features = data[, colidx]
  reg = lm(y ~ features)
  #print(summary(reg))
  rss = sum(resid(reg) ^ 2)
  tss = sum((y - mean(y))^2)
  detcoef = 1 - rss/tss
  adjdetcoef = 1 - (rss / (m - n - 1)) / (tss / (m - 1))
  cp = (rss / (rss / (m - n - 1))) - (m - 2 * (p + 1))
  return (c(rss, detcoef, adjdetcoef, cp))
}

for (p in 1:1)
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
    res = oneRegression(y, data, colidx, n, m)
    print(res)
    print(bestrss)
    print(res[1])
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
    
    if (is.na(bestcp) || (abs(bestcp - (p+1)) > abs(r[4] - (p + 1))))
    {
      bestcp = res[4]
      bestcpidx = colidx
    }

  }
  print("===============================================================================")
  cat("For ", p, " features:\n")
  cat("Best RSS is ", bestrss, " for columns ", bestrssidx, "\n")
  cat("Best R2 is ", bestdetcoef, " for columns ", bestdetcoefidx, "\n")
  cat("Best R2 adjusted is ", bestadjcoef,  " for columns ", bestadjcoefidx, "\n")
  cat("Best CP is ", bestcp, " for columns ", bestcpidx, "\n")
  print("*******************************************************************************")
}
