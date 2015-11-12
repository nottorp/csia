# PR 1
gensrobs <- function(m, a, b, xmin, xmax, sigma)
{
  xes = runif(m, xmin, xmax)
  # sigma patrat ?!?
  epsilons = rnorm(m, mean=0, sd=sigma)
  #print (xes)
  #print (epsilons)
  obs = mapply(
    function(x, eps) a + b * x + eps,
    xes, epsilons
  )
#  print (cbind(obs, xes))
  return (cbind(xes, obs))
}

# PR 2

confint_manual <- function(d, m)
{
  print("BEGIN confint_manual")
  justobs = d[,2]
  print (justobs)
  obs_mean = mean(justobs)
  print (obs_mean)
  print("END confint_manual")
}

simpleregression <- function(counter, m, a, b, xmin, xmax, sigma)
{
  d = gensrobs(m, a, b, xmin, xmax, sigma)
  
  fname = sprintf("simple_regression_%d.pdf", counter)
  pdf(fname, 8.27, 5.83) # A5
  fit = lm(obs ~ xes, as.data.frame(d))
  ci_b = confint(fit, 'xes', 0.95)
  ci_a = confint(fit, '(Intercept)', 0.95)
  confint_manual(d, m)
  print(ci_b[1])
  print(ci_b[2])
  
  title2 = sprintf("m=%d, a=%.2f, b=%.2f, xmin=%.2f, xmax=%.2f, sigma=%.2f\n%.2f <= fit a=%.2f <= %.2f\n%.2f <= fit b=%.2f <= %.2f",
                   m, a, b, xmin, xmax, sigma,
                   ci_a[1], fit$coefficients[1], ci_a[2],
                   ci_b[1], fit$coefficients[2], ci_b[2])
  plot(main=title2, d)
  abline(coef(fit), col="red")
  abline(a, b, col="blue")
  legend("bottomright", # places a legend at the appropriate place
         c("Original","Regression"), # puts text in the legend
         lty=c(1,1), # gives the legend appropriate symbols (lines)
         lwd=c(2.5,2.5),col=c("blue","red")) # gives the legend lines the correct color and width
  dev.off()
  print("===========================================")
  print(sprintf("m=%d, a=%.2f, b=%.2f",
                   m, a, b))
  print(sprintf("xmin=%.2f, xmax=%.2f",
                   xmin, xmax))
  print(sprintf("sigma=%.2f",
                   sigma))
  print(sprintf("fit a=%.2f, fit b=%.2f",
                   fit$coefficients[1], fit$coefficients[2]))
# b = fit b +/- T 2.5% (m-2) . standard error (fit b), same for a
# P(b fit - * <= b <= b fit + *) = 95%
# Calculat confidence interval manual - std error etc
# SE(fit b) = sigma (original) / sqrt(m) * Sx  - std deviation for x = (sum(xi) - e(x))^2 / m
# S = estimator pt sigma = (sum (yi - e(y)))^2 / (m - 2), in loc de sigma (original)
# m - 2 == gradul de libertate, nr de observatii - nr de variabile - 1
# SE(fit b) = S / (sqrt(sum (xi = e(x))^2))

# var(b fit) = S^2 / (m * Sx^2)
  print(confint(fit, 'xes', 0.95))
  print(confint(fit, '(Intercept)', 0.95))
}

# Counter for file name, no of samples, a, b, minx, maxx, sigma
#simpleregression(1, 100, 3, 5, -200, 200, 1.5)
simpleregression(2, 10, 3, 5, -5, 5, 1)
#simpleregression(3, 10000, 3, 5, -5, 5, 1)
#simpleregression(4, 10, 3, 5, 5, 5.2, 1)
#simpleregression(5, 10000, 3, 5, 5, 5.2, 1)
#simpleregression(6, 10, 3, 5, 5, 5.2, 0.01)

