# Independent numerical checks of the examples appearing in the supplied chapters.
options(warn = 2)
ce <- function(w) w - w^0.9 * (w-20)^0.1
cat("CRRA maximum premiums:", ce(100), ce(200), "\n")
stopifnot(round(ce(100),2)==2.21, round(ce(200),2)==2.10)
H <- function(d) (1-d)^2/2
psi <- function(d) integrate(function(x) log(2 - x - 1.2*H(d)),
                             0, d, subdivisions=1000, rel.tol=1e-12)$value +
                             (1-d)*log(2-d-1.2*H(d))
opt <- optimize(psi,c(0,1),maximum=TRUE,tol=1e-9)
cat("Optimal deductible:", opt$maximum, "premium:", 1.2*H(opt$maximum),"\n")
cat("Expected utilities:",psi(0),opt$objective,psi(1),"\n")
stopifnot(abs(opt$maximum-.751167)<1e-6,abs(opt$objective-.387697)<1e-6)
n <- c(100,1000,10000); K <- 400*sqrt(n)*qnorm(.99)
p <- pbinom(floor((200*n+K)/1000),n,.2,lower.tail=FALSE)
print(data.frame(n,K,per_policy=K/n,exceedance_percent=100*p))
stopifnot(identical(round(100*p,3),c(1.125,1.074,.999)))
# Compare the closed convolution tail with a direct numerical integral.
for(s in c(.1,1,5,20,198)) {
 direct <- 1/(1+s)+integrate(function(x) 1/((1+x)^2*(1+s-x)),0,s)$value
 closed <- 2/(s+2)+2*log1p(s)/(s+2)^2
 stopifnot(abs(direct-closed)<1e-8)
}
# ES computed by integrating the empirical quantile, including fractional atoms.
es_discrete <- function(x,p,a) {
 idx<-order(x);x<-x[idx];p<-p[idx];F<-cumsum(p);prev<-c(0,head(F,-1))
 sum(x*pmax(0,F-pmax(a,prev)))/(1-a)
}
stopifnot(abs(es_discrete(c(0,100),c(.96,.04),.95)-80)<1e-9)
stopifnot(abs(es_discrete(c(0,100),c(.92,.08),.95)-100)<1e-9)
stopifnot(abs(es_discrete(c(0,100,200),c(.9216,.0768,.0016),.95)-103.2)<1e-9)
# Chapter 5. Independent checks, without Panjer's recursion: convolution
# powers of the severity for compound Poisson laws, and enumeration of all
# 2^12 outcomes for the individual model.
conv <- function(a, b, M) sapply(0:M, function(m) sum(a[1:(m + 1)] * rev(b[1:(m + 1)])))
compound_poisson_direct <- function(lambda, f, M) {
  sev <- c(0, f, rep(0, M + 1 - length(f) - 1))[1:(M + 1)]
  power <- c(1, rep(0, M)); out <- dpois(0, lambda) * power
  for (n in 1:M) { power <- conv(power, sev, M); out <- out + dpois(n, lambda) * power }
  out
}
M <- 60
direct <- compound_poisson_direct(3, c(.5, .3, .2), M)
v <- which(cumsum(direct) >= .99)[1] - 1
es <- v + sum(pmax(0:M - v, 0) * direct) / .01
stopifnot(v == 14, abs(es - 15.909) < 5e-4)
q <- rep(c(.1, .2, .3), each = 4); size <- rep(1:3, each = 4)
outcomes <- as.matrix(expand.grid(rep(list(0:1), 12)))
p_out <- apply(outcomes, 1, function(b) prod(ifelse(b == 1, q, 1 - q)))
total <- outcomes %*% size
ind <- sapply(0:M, function(m) sum(p_out[total == m]))
mix <- function(w) as.numeric(tapply(w, size, sum)) / sum(w)
cp1 <- compound_poisson_direct(sum(q), mix(q), M)
cp2 <- compound_poisson_direct(sum(-log(1 - q)), mix(-log(1 - q)), M)
d1 <- cumsum(cp1) - cumsum(ind); d2 <- cumsum(cp2) - cumsum(ind)
stopifnot(all(d1[1:7] > 0), all(d1[8:41] < 0), all(d2 <= 1e-12),
          abs(d2[1]) < 1e-12, abs(sum(ind * 0:M) - 5.6) < 1e-12)
var99 <- function(p) which(cumsum(p) >= .99)[1] - 1
stopifnot(var99(ind) == 14, var99(cp1) == 16, var99(cp2) == 18)
source("R/figures.R")
d <- collective_comparison_data(M)
stopifnot(max(abs(d$ind - cumsum(ind))) < 1e-12,
          max(abs(d$mean_matching - cumsum(cp1))) < 1e-12,
          max(abs(d$zero_matching - cumsum(cp2))) < 1e-12)
cat("Chapter 5: Panjer example and compound Poisson comparison checks passed.\n")
cat("All numerical assertions passed.\n")
