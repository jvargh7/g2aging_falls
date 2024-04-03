library(longpower)

n <- 3
u <- list(u1 = rep(1,n), u2 = rep(0,n))
v <- list(v1 = rep(1,n),
          v2 = rep(1,n))
rho <- c(0.2, 0.5, 0.8)
delta <- seq(2,24,by=2)/100
exp_delta <- exp(delta)
tab <- outer(rho, delta,
             Vectorize(function(rho, delta){
               ceiling(liu.liang.linear.power(
                 delta=delta, u=u, v=v,
                 sigma2=1,
                 R=rho, alternative="two.sided",
                 power=0.90)$n[1])}))
colnames(tab) <- paste("exp_delta =", delta)
rownames(tab) <- paste("rho =", rho)
tab
