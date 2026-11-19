# Source from the project root. Course plots are generated reproducibly here.
source("R/plot-theme.R")

plot_concave_utility <- function() {
  op <- insurance_par(mar = c(3.8, 5.6, 1.8, 1))
  on.exit(par(op))
  co <- insurance_colours
  x <- seq(0.05, 4.2, length.out = 600)
  x0 <- 0.55; x1 <- 3.6; theta <- 0.45
  xm <- theta * x0 + (1 - theta) * x1
  u0 <- sqrt(x0); u1 <- sqrt(x1); um <- sqrt(xm)
  uc <- theta * u0 + (1 - theta) * u1
  plot(x, sqrt(x), type = "n", xlim = c(0, 4.2), ylim = c(0, 2.3),
       axes = FALSE, xlab = "Wealth", ylab = "", xaxs = "i", yaxs = "i")
  segments(c(x0, xm, x1), 0, c(x0, xm, x1), c(u0, um, u1),
           col = co[["grid"]], lty = 3)
  segments(0, c(u0, uc, um, u1), c(x0, xm, xm, x1), c(u0, uc, um, u1),
           col = co[["grid"]], lty = 3)
  lines(x, sqrt(x), lwd = 2.6, col = co[["curve"]])
  segments(x0, u0, x1, u1, lwd = 1.8, lty = 2, col = co[["accent"]])
  segments(xm, uc, xm, um, lwd = 2.2, col = co[["accent"]])
  points(c(x0, xm, x1), c(u0, um, u1), pch = 16, col = co[["curve"]])
  points(xm, uc, pch = 21, bg = co[["background"]], col = co[["accent"]], lwd = 1.6)
  axis(1, at = c(x0, xm, x1), labels = expression(x[0], bar(x), x[1]))
  axis(2, at = c(u0, uc, um, u1),
       labels = expression(u(x[0]), bar(u), u(bar(x)), u(x[1])), cex.axis = 0.86)
  box()
  legend("topleft", c("Concave utility", "Chord"), lty = c(1, 2),
         lwd = c(2.6, 1.8), col = co[c("curve", "accent")],
         text.col = co[["text"]], bty = "n", cex = 0.9)
}

plot_proportional_demand <- function() {
  op <- insurance_par(mfrow = c(3, 1), mar = c(3.3, 4.5, 2.6, 1.1))
  on.exit(par(op))
  par(cex = 1)
  co <- insurance_colours
  alpha <- seq(0, 1, length.out = 601)
  w <- 100; S <- 40; p <- 0.2; m <- p * S
  loads <- c(1, 1.2, 1.6)
  objective <- function(a, lam) (1-p)*log(w-lam*a*m) +
    p*log(w-S+a*S-lam*a*m)
  all_values <- vapply(loads, function(lam) objective(alpha, lam)-objective(0, lam),
                       numeric(length(alpha)))
  yrange <- range(all_values) + c(-0.006, 0.007)
  titles <- c("Fair pricing: full coverage", "Moderate loading: partial coverage",
              "High loading: no coverage")
  for (j in seq_along(loads)) {
    lam <- loads[j]
    candidates <- c(0, optimize(function(a) objective(a, lam), c(0, 1),
                                maximum = TRUE)$maximum, 1)
    best <- candidates[which.max(objective(candidates, lam))]
    plot(alpha, all_values[,j], type = "n", ylim = yrange,
         xlab = expression(alpha), ylab = expression(f(alpha)-f(0)),
         main = titles[j])
    abline(h = 0, col = co[["axis"]], lty = 3)
    lines(alpha, all_values[,j], lwd = 2.5, col = co[["curve"]])
    abline(v = best, col = co[["accent"]], lty = 2)
    points(best, objective(best, lam)-objective(0, lam), pch = 16, col = co[["accent"]])
    legend("bottomleft", legend = bquote(lambda == .(lam) ~ ";" ~ alpha^"*" == .(round(best, 3))),
           bty = "n", text.col = co[["text"]], cex = 0.93)
  }
}

plot_indemnities <- function() {
  op <- insurance_par(mfrow = c(6, 1), mar = c(3.5, 3.9, 2.4, 0.8),
                      mgp = c(2.3, 0.65, 0))
  on.exit(par(op))
  par(cex = 0.95)
  co <- insurance_colours
  x <- seq(0, 100, length.out = 1001)
  indemnities <- list(function(x) 0.6*x, function(x) pmax(x-30, 0),
    function(x) pmin(x, 40), function(x) pmin(pmax(x-20, 0), 35),
    function(x) x*(0.5+0.35*sin(pi*x/25)),
    function(x) ifelse(x >= 25 & x <= 65, 0.7*x, 0))
  titles <- c("Proportional", "Deductible", "Capped", "Capped deductible",
              "Oscillating", "Band coverage")
  for (j in seq_along(indemnities)) {
    y <- indemnities[[j]](x)
    stopifnot(all(y >= 0), all(y <= x))
    plot(x, y, type = "n", xlim = c(0, 100), ylim = c(0, 100),
         xlab = "Loss x", ylab = "Indemnity I(x)", main = titles[j])
    polygon(c(0, 100, 100), c(0, 0, 100), col = co[["shade"]], border = NA)
    abline(a = 0, b = 1, col = co[["axis"]], lty = 2)
    if (j < 6) lines(x, y, col = co[["curve"]], lwd = 2.5) else {
      segments(c(0, 25, 65), c(0, 17.5, 0), c(25, 65, 100), c(0, 45.5, 0),
               col = co[["curve"]], lwd = 2.5)
      points(c(25, 65), c(17.5, 45.5), pch = 16, col = co[["curve"]])
      points(c(25, 65), c(0, 0), pch = 21, bg = co[["background"]], col = co[["curve"]])
    }
    box()
  }
}

plot_convergence <- function() {
  op <- insurance_par(mar = rep(0.25, 4), xpd = NA)
  on.exit(par(op))
  co <- insurance_colours
  plot.new(); plot.window(xlim = c(0, 10), ylim = c(0, 6), xaxs = "i", yaxs = "i")
  node <- function(x, y, label) {
    rect(x-1.4, y-0.52, x+1.4, y+0.52, col = co[["shade"]], border = co[["axis"]])
    text(x, y, label, col = co[["title"]], cex = 1.05)
  }
  node(2, 4.9, "Almost sure")
  node(8, 4.9, expression(L^1))
  node(5, 1.1, "In probability")
  # Solid arrows hold for the whole sequence; dashed ones carry extra conditions.
  arrows(3.5, 4.9, 6.5, 4.9, col = co[["accent"]], lty = 2, length = 0.10, lwd = 1.7)
  text(5, 5.5, "Uniform integrability", col = co[["text"]], cex = 0.9)
  arrows(2.35, 4.25, 4.1, 1.78, col = co[["curve"]], length = 0.10, lwd = 2)
  arrows(7.65, 4.25, 5.9, 1.78, col = co[["curve"]], length = 0.10, lwd = 2)
  arrows(3.6, 1.6, 1.75, 4.2, col = co[["accent"]], lty = 3, length = 0.10, lwd = 1.7)
  arrows(6.4, 1.6, 8.25, 4.2, col = co[["accent"]], lty = 2, length = 0.10, lwd = 1.7)
  text(1.1, 2.75, "Along a\nsubsequence", col = co[["text"]], cex = 0.9)
  text(8.95, 2.75, "Uniform\nintegrability", col = co[["text"]], cex = 0.9)
  # Direct implication L1 -> a.s. along a subsequence (reverse horizontal arrow).
  arrows(6.5, 4.35, 3.5, 4.35, col = co[["accent"]], lty = 3,
         length = 0.10, lwd = 1.8)
  text(5, 3.95, "Along a subsequence", col = co[["text"]], cex = 0.88)
  text(5, 0.18, "All convergences are towards the same X.", col = co[["text"]], cex = 0.85)
}

plot_lognormal_bounds <- function() {
  op <- insurance_par(mar = c(4, 4.2, 1.1, 1.3))
  on.exit(par(op))
  co <- insurance_colours
  sigma <- seq(0.05, 5, length.out = 800)
  den <- sqrt(expm1(1) * expm1(sigma^2))
  hi <- expm1(sigma) / den
  lo <- expm1(-sigma) / den
  plot(sigma, hi, type = "n", ylim = c(-0.8, 1.08), xlim = c(0, 5),
       xlab = expression(sigma), ylab = "Pearson correlation")
  polygon(c(sigma, rev(sigma)), c(lo, rev(hi)),
          col = co[["shade"]], border = NA)
  abline(h = 0, col = co[["axis"]], lty = 3)
  lines(sigma, hi, col = co[["curve"]], lwd = 2.5)
  lines(sigma, lo, col = co[["accent"]], lwd = 2, lty = 2)
  points(c(1, 2, 3, 4, 5), expm1(1:5)/sqrt(expm1(1)*expm1((1:5)^2)),
         pch = 16, col = co[["curve"]])
  legend("topright", c("Maximum correlation", "Minimum correlation"),
         col = co[c("curve", "accent")], lty = c(1, 2), lwd = 2,
         text.col = co[["text"]], bty = "n", cex = 0.86)
}

plot_dependence_simulation <- function() {
  op <- insurance_par(mfrow = c(3, 1), pty = "s", mar = c(3.6, 3.6, 2.4, 0.8),
                      mgp = c(2.1, 0.5, 0))
  on.exit(par(op))
  par(cex = 1)
  co <- insurance_colours
  set.seed(2026)
  n <- 900; r <- 0.75
  a <- runif(n); b <- runif(n)
  z1 <- sqrt(-2*log(a))*cos(2*pi*b)
  z2 <- sqrt(-2*log(a))*sin(2*pi*b)
  g1 <- z1; g2 <- r*z1+sqrt(1-r*r)*z2
  u <- pnorm(g1); v <- pnorm(g2)
  t1 <- -pnorm(g1, lower.tail = FALSE, log.p = TRUE)
  t2 <- -pnorm(g2, lower.tail = FALSE, log.p = TRUE)
  dot <- adjustcolor(co[["curve"]], alpha.f = 0.45)
  plot(g1, g2, pch = 16, cex = 0.4, col = dot,
       xlab = expression(G[1]), ylab = expression(G[2]), main = "Gaussian pair")
  plot(u, v, pch = 16, cex = 0.4, col = dot, xlim = c(0, 1), ylim = c(0, 1),
       xlab = "U", ylab = "V", main = "Uniform marginals")
  plot(t1, t2, pch = 16, cex = 0.4, col = dot,
       xlab = expression(T[1]), ylab = expression(T[2]), main = "Exponential lifetimes")
}

plot_conditional_projection <- function() {
  op <- insurance_par(mar = rep(0.3, 4), xpd = NA)
  on.exit(par(op))
  co <- insurance_colours
  plot.new(); plot.window(xlim = c(0, 10), ylim = c(0, 6), asp = 1)
  polygon(c(0.7, 7.8, 9.4, 2.3), c(0.8, 0.8, 2.7, 2.7),
          col = co[["shade"]], border = co[["axis"]])
  H <- c(2.8, 1.65); M <- c(6.1, 1.65); X <- c(6.1, 5.1)
  segments(H[1], H[2], X[1], X[2], col = co[["axis"]], lty = 2, lwd = 1.7)
  segments(H[1], H[2], M[1], M[2], col = co[["curve"]], lwd = 2.6)
  segments(M[1], M[2], X[1], X[2], col = co[["accent"]], lwd = 2.6)
  lines(c(M[1]-0.25, M[1]-0.25, M[1]), c(M[2], M[2]+0.25, M[2]+0.25),
        col = co[["text"]], lwd = 1.3)
  points(c(H[1], M[1], X[1]), c(H[2], M[2], X[2]), pch = 16, col = co[["curve"]])
  text(H[1]-0.35, H[2], "H", col = co[["title"]])
  text(M[1]+0.25, M[2]-0.4, expression(M == E*group("[", X~"|"~G, "]")), adj = 0, col = co[["title"]])
  text(X[1]+0.3, X[2], "X", adj = 0, col = co[["title"]])
  text(6.4, 3.65, "Residual X - M", adj = 0, col = co[["accent"]], cex = 0.95)
  text(4.4, 1.32, "M - H", col = co[["curve"]], cex = 0.95)
  text(3.4, 0.33, expression(L^2*group("(", G, ")")~": admissible predictions"), col = co[["text"]])
}

plot_var_cases <- function(horizontal = FALSE) {
  op <- insurance_par(mfrow = if (horizontal) c(1, 3) else c(3, 1),
                      mar = c(3.7, 4.5, 2.8, 1))
  on.exit(par(op))
  par(cex = 1)
  co <- insurance_colours
  frame <- function(xlim, alpha, ticks, ticklabels, heading) {
    plot(NA, xlim = xlim, ylim = c(-0.02, 1.06), xaxs = "i", yaxs = "i",
         axes = FALSE, xlab = "Loss x", ylab = expression(F[X](x)), main = heading)
    abline(h = c(0, 0.5, 1), col = co[["grid"]])
    abline(h = alpha, col = co[["accent"]], lty = 2, lwd = 1.3)
    axis(1, at = ticks, labels = ticklabels)
    axis(2, at = c(0, 0.5, 1), labels = c("0", "0.5", "1"))
    box()
    text(xlim[1]+0.05*diff(xlim), alpha+0.07, bquote(alpha == .(alpha)),
         adj = 0, col = co[["accent"]], cex = 0.95)
  }
  # Regular case: the standard normal CDF is strictly increasing.
  q <- qnorm(0.9); x <- seq(-3.5, 3.5, length.out = 1000)
  frame(c(-3.5, 3.5), 0.9, c(-3, 0, q, 3), c("-3", "0", "VaR", "3"),
        "Continuous and strictly increasing")
  segments(q, 0, q, 0.9, col = co[["text"]], lty = 3)
  lines(x, pnorm(x), col = co[["curve"]], lwd = 2.6)
  points(q, 0.9, pch = 16, col = co[["curve"]])
  text(2.1, 0.20, "VaR = 1.2816", col = co[["text"]], cex = 0.95)
  # Discrete case: P(X=0)=0.6 and P(X=2)=0.4.
  frame(c(-1, 3.3), 0.8, c(0, 2, 3), c("0", "VaR = 2", "3"),
        "A jump crosses the level")
  segments(2, 0, 2, 1, col = co[["text"]], lty = 3)
  segments(c(-1, 0, 2), c(0, 0.6, 1), c(0, 2, 3.3), c(0, 0.6, 1),
           col = co[["curve"]], lwd = 2.6)
  points(c(0, 2), c(0, 0.6), pch = 21, bg = co[["background"]],
         col = co[["curve"]], lwd = 1.6)
  points(c(0, 2), c(0.6, 1), pch = 16, col = co[["curve"]])
  text(0.15, 0.26, "F(2-) = 0.6 < 0.8 < F(2) = 1", adj = 0,
       col = co[["text"]], cex = if (horizontal) 0.8 else 0.95)
  # Continuous mixture: 1/2 Unif(0,1) + 1/2 Unif(2,3).
  frame(c(-0.6, 3.7), 0.5, c(0, 1, 2, 3), c("0", "VaR = 1", "2", "3"),
        "A plateau at the level")
  segments(1, 0, 1, 0.5, col = co[["text"]], lty = 3)
  lines(c(-0.6, 0, 1, 2, 3, 3.7), c(0, 0, 0.5, 0.5, 1, 1),
        col = co[["curve"]], lwd = 2.6)
  points(1, 0.5, pch = 16, col = co[["curve"]])
  arrows(1.35, 0.81, 1.03, 0.54, col = co[["text"]], length = 0.08)
  text(1.4, 0.88, "First point at the level", col = co[["text"]], cex = 0.95)
}

# Chapter 5: individual model versus the two compound Poisson approximations.
# Twelve independent policies; policy i pays size[i] units with probability q[i].
collective_comparison_data <- function(M = 60) {
  q <- c(rep(0.10, 4), rep(0.20, 4), rep(0.30, 4))
  size <- c(rep(1, 4), rep(2, 4), rep(3, 4))
  # Individual model: exact convolution, one policy at a time.
  ind <- c(1, rep(0, M))
  for (i in seq_along(q)) {
    shifted <- c(rep(0, size[i]), ind[1:(M + 1 - size[i])])
    ind <- (1 - q[i]) * ind + q[i] * shifted
  }
  # Compound Poisson laws by Panjer's recursion (a = 0, b = intensity).
  compound_poisson <- function(weights) {
    lambda <- sum(weights)
    f <- tapply(weights, factor(size, levels = 1:max(size)), sum) / lambda
    prob <- numeric(M + 1)
    prob[1] <- exp(-lambda)
    for (m in 1:M) {
      k <- 1:min(m, length(f))
      prob[m + 1] <- lambda / m * sum(k * f[k] * prob[m - k + 1])
    }
    prob
  }
  list(x = 0:M,
       ind = cumsum(ind),
       mean_matching = cumsum(compound_poisson(q)),
       zero_matching = cumsum(compound_poisson(-log(1 - q))))
}

plot_collective_comparison <- function(xmax = 20) {
  op <- insurance_par(mfrow = c(2, 1), mar = c(3.8, 4.6, 2.6, 1))
  on.exit(par(op))
  par(cex = 1)
  co <- insurance_colours
  d <- collective_comparison_data()
  keep <- d$x <= xmax
  x <- d$x[keep]
  cols <- c(co[["title"]], co[["curve"]], co[["accent"]])
  ltys <- c(1, 5, 2)
  labels <- c("Individual model", "Mean preserved",
              "Probability of no payment preserved")
  plot(NA, xlim = c(0, xmax), ylim = c(0, 1), xlab = "Aggregate payment x (units)",
       ylab = expression(P(S <= x)), main = "Cumulative distribution functions")
  abline(h = c(0, 1), col = co[["grid"]])
  lines(x, d$ind[keep], type = "s", col = cols[1], lwd = 1.8)
  lines(x, d$mean_matching[keep], type = "s", col = cols[2], lwd = 2.4, lty = ltys[2])
  lines(x, d$zero_matching[keep], type = "s", col = cols[3], lwd = 2.4, lty = ltys[3])
  legend("bottomright", labels, col = cols, lty = ltys, lwd = c(1.8, 2.4, 2.4),
         text.col = co[["text"]], bty = "n", cex = 0.85)
  diff1 <- d$mean_matching[keep] - d$ind[keep]
  diff2 <- d$zero_matching[keep] - d$ind[keep]
  plot(NA, xlim = c(0, xmax), ylim = range(diff1, diff2, 0) * 1.08,
       xlab = "Aggregate payment x (units)",
       ylab = "Difference of CDFs", main = "Approximation minus individual model")
  abline(h = 0, col = co[["axis"]], lty = 3)
  lines(x, diff1, type = "s", col = cols[2], lwd = 2.4, lty = ltys[2])
  lines(x, diff2, type = "s", col = cols[3], lwd = 2.4, lty = ltys[3])
}
