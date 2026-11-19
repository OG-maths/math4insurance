# Numerical checks against independent calculations, not plots.
options(warn = 2)
sigma <- c(.5, 1, 2, 3, 4, 5)
rho_hi <- expm1(sigma)/sqrt(expm1(1)*expm1(sigma^2))
rho_lo <- expm1(-sigma)/sqrt(expm1(1)*expm1(sigma^2))
stopifnot(all(abs(rho_hi-c(.928608,1,.665755,.161755,.013717,.000419))<5e-7))
stopifnot(all(abs(rho_lo-c(-.563229,-.367879,-.090100,-.008053,-.000251,-.000003))<5e-7))
# Independently integrate normal moments for several moderate sigmas.
for (s in c(.5, 1, 2, 3)) {
  cov_integral <- integrate(function(z)
    (exp(z)-exp(.5))*(exp(s*z)-exp(s*s/2))*dnorm(z),
    lower=-12, upper=12, rel.tol=1e-9)$value
  from_integral <- cov_integral/sqrt(exp(1)*expm1(1)*exp(s*s)*expm1(s*s))
  closed <- expm1(s)/sqrt(expm1(1)*expm1(s*s))
  stopifnot(abs(from_integral-closed)<1e-7)
}
x <- c(0,1,2,3,100); y <- c(0,97,98,99,100)
stopifnot(abs(cor(x,y)-.28804655714065)<1e-12,
          abs(cor(x,y,method="spearman")-1)<1e-12)
# Copula area identities: use one-dimensional antiderivatives in one margin.
integral_M <- integrate(function(u) u-u^2/2,0,1)$value
integral_W <- integrate(function(u) u^2/2,0,1)$value
stopifnot(abs(12*integral_M-3-1)<1e-12,
          abs(12*integral_W-3+1)<1e-12)
theta <- 2; qlo <- 1e-8; qhi <- 1-1e-7
clayton_diag <- function(q) (2*q^(-theta)-1)^(-1/theta)
gumbel_diag <- function(q) q^(2^(1/theta))
stopifnot(abs(clayton_diag(qlo)/qlo-2^(-1/theta))<1e-7)
stopifnot(abs((1-2*qhi+gumbel_diag(qhi))/(1-qhi)-(2-2^(1/theta)))<1e-6)
tail_student <- 2*pt(-sqrt(5*(1-.75)/(1+.75)),df=5)
stopifnot(abs(tail_student-.4365880610203819)<1e-12)
cat("Chapter 6: numerical integration, table, rank and tail checks passed.\n")
