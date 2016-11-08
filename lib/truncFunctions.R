2# Functions for truncated distributions
# From Nadarajah & Kotz (2006). R Programs for Computing Truncated Distributions. Journal of Statistical Software, 16: Code Snippet 2

dtrunc <- function(x, spec, a = -Inf, b = Inf, ...)
{
  tt <- rep(0, length(x))
  g <- get(paste("d", spec, sep = ""), mode = "function")
  G <- get(paste("p", spec, sep = ""), mode = "function")
  tt[x>=a & x<=b] <- g(x[x>=a&x<=b], ...)/(G(b, ...) - G(a, ...))
  return(tt)
}
extrunc <- function(spec, a = -Inf, b = Inf,...)
{
  f <- function(x) x * dtrunc(x, spec, a = a, b = b, ...)
  return(integrate(f, lower = a, upper = b)$value)
}

vartrunc <- function(spec, a = -Inf, b = Inf, ...)
{
  ex <- extrunc(spec, a = a, b = b, ...)
  f <- function(x) (x - ex)^2 * dtrunc(x, spec, a = a, b = b, ...)
  tt <- integrate(f, lower = a, upper = b)$value
  return(tt)
}

ptrunc <- function(x, spec, a = -Inf, b = Inf, ...)
{
  tt <- x
  aa <- rep(a, length(x))
  bb <- rep(b, length(x))
  G <- get(paste("p", spec, sep = ""), mode = "function")
  tt <- G(apply(cbind(apply(cbind(x, bb), 1, min), aa), 1, max), ...)
  tt <- tt - G(aa, ...)
  tt <- tt/(G(bb, ...) - G(aa, ...))
  return(tt)
}

qtrunc <- function(p, spec, a = -Inf, b = Inf, ...)
{
  tt <- p
  G <- get(paste("p", spec, sep = ""), mode = "function")
  Gin <- get(paste("q", spec, sep = ""), mode = "function")
  tt <- Gin(G(a, ...) + p*(G(b, ...) - G(a, ...)), ...)
  return(tt)
}

rtrunc <- function(n, spec, a = -Inf, b = Inf, ...)
{
  x <- u <- runif(n, min = 0, max = 1)
  x <- qtrunc(u, spec, a = a, b = b,...)
  return(x)
}
