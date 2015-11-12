# PR 2
victim <- c(1, 8, 2, 6, 3, 8, 5, 5, 5, 5)
cat("Original: ", victim, "\n")
cat("sum/10 = ", sum(victim)/10, "\n")
cat("log10 of each: ", log10(victim), "\n")
cat("max - min = ", max(victim) - min(victim), "\n")

func <- function(x) {
  return (x - 4.8)/2.299758
}

y = sapply(victim, func)
cat("processed vector: ", y, "\n")
cat("mean: ", mean(y), "\n")
cat("std deviation: ", sd(y), "\n")

# PR 3
bill <- c(46, 33, 39, 37, 46, 30, 48, 32, 49, 35, 30, 48)
cat("monthly bills: ", bill, "\n")
cat("yearly expense: ", sum(bill), "\n")
billorder = order(bill)
cat("min monthly bill was: ", billorder[1], " with cost: ", bill[billorder[1]], "\n")
cat("max monthly bill was: ", billorder[length(billorder)], " with cost: ", bill[billorder[length(billorder)]], "\n")

expensivecount = sum(bill > 40)
expensiveper = expensivecount / length(bill) * 100

cat("You paid more than 40 for ", expensivecount, " months, i.e. ", expensiveper, "% of them\n")

# PR 4
input <- scan(nmax = 7)
imax = max(input)
imin = min(input)
imean = mean(input)
imedian = median(input)
isd = sd(input)

isorted = sort(input)
inormalized = scale(input)

cat("max: ", imax, "\n")
cat("min: ", imin, "\n")
cat("mean: ", imean, "\n")
cat("median: ", imedian, "\n")
cat("std deviation: ", isd, "\n")

cat("sorted: ", isorted, "\n")
cat("normalized: ", inormalized, "\n")
