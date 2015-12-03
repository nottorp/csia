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

# b = fit b +/- T 2.5% (m-2) . standard error (fit b), same for a
# P(b fit - * <= b <= b fit + *) = 95%
# Calculat confidence interval manual - std error etc
# SE(fit b) = sigma (original) / sqrt(m) * Sx  - std deviation for x = (sum(xi) - e(x))^2 / m
# S = estimator pt sigma = (sum (yi - e(y)))^2 / (m - 2), in loc de sigma (original)
# m - 2 == gradul de libertate, nr de observatii - nr de variabile - 1
# SE(fit b) = S / (sqrt(sum (xi = e(x))^2))
# var(b fit) = S^2 / (m * Sx^2)

confint_manual <- function(d, found_slope, found_intercept)
{
  print("BEGIN confint_manual")
  xes = d[,1]
  justobs = d[,2]
  print (found_slope)
  print (found_intercept)
  fitted_obs = sapply(xes, function(x) x * found_slope + found_intercept)
  sse_dumb = sum(mapply(function(x, y) (x - y)**2, justobs, fitted_obs))
  sse_r_style = sum((justobs - fitted_obs)^2)
  sse = sse_r_style
  freedom = length(justobs) - 2
  sigma_est_curs = sqrt(sse / freedom)
  se_slope = sigma_est_curs^2
  ss_xes = sum(xes^2)
  #t25n2 = qt(.975, df=(length(justobs) - 2))
  t25n2 = qt(c(.025, .975), df=freedom)
  cat("sigma estimated:", sigma_est_curs, "\n")
  cat("t 2.5%", t25n2[1], " degrees of freedom: ", freedom, "\n")
  #print(xes)
  cat("sum of squares for x:", ss_xes, "\n")
  cat("sigma/sqrt(sum of squares x)", sigma_est_curs / sqrt(ss_xes), "\n")
  ci_slope_min = found_slope + t25n2[1] * sigma_est_curs / sqrt(ss_xes)
  ci_slope_max = found_slope + t25n2[2] * sigma_est_curs / sqrt(ss_xes)
  cat("ci_slope_min:", ci_slope_min, "\n")
  cat("ci_slope_max:", ci_slope_max, "\n")
  
  se_intercept = sigma_est_curs * sqrt(ss_xes / length(justobs))
  ci_int_min = found_intercept + t25n2[1] * se_intercept
  ci_int_max = found_intercept + t25n2[2] * se_intercept
  cat("ci_int_min:", ci_int_min, "\n")
  cat("ci_int_max:", ci_int_max, "\n")  
  return (c(ci_slope_min, ci_slope_max))
  print("END confint_manual")
}

simpleregression <- function(counter, m, a, b, xmin, xmax, sigma)
{
  d = gensrobs(m, a, b, xmin, xmax, sigma)
  
  fname = sprintf("simple_regression_%d.pdf", counter)
  pdf(fname, 8.27, 5.83) # A5
  fit = lm(obs ~ xes, as.data.frame(d))
  ci_b = confint(fit, 'xes', 0.975)
  ci_a = confint(fit, '(Intercept)', 0.975)
  ci_manual = confint_manual(d, unname(fit$coefficients[2]), unname(fit$coefficients[1]))
  print(ci_b[1])
  print(ci_b[2])
  
  title2 = sprintf("m=%d, a=%.2f, b=%.2f, xmin=%.2f, xmax=%.2f, sigma=%.2f\n%.4f <= fit a=%.4f <= %.4f\n%.4f (%.4f) <= fit b=%.4f <= (%.4f) %.4f",
                   m, a, b, xmin, xmax, sigma,
                   ci_a[1], fit$coefficients[1], ci_a[2],
                   ci_b[1], ci_manual[1], fit$coefficients[2], ci_manual[2], ci_b[2])
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
simpleregression(1, 100, 3, 5, -200, 200, 1.5)
simpleregression(2, 10, 3, 5, -5, 5, 1)
simpleregression(3, 10000, 3, 5, -5, 5, 1)
simpleregression(4, 10, 3, 5, 5, 5.2, 1)
simpleregression(5, 10000, 3, 5, 5, 5.2, 1)
simpleregression(6, 10, 3, 5, 5, 5.2, 0.01)

#d = gensrobs(10000, 3, 5, -5, 5, 1)
#fit = lm(obs ~ xes, as.data.frame(d))