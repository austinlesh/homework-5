## ECHO is on.

n = 15
x = rnorm(n)
y = rnorm(x + rnorm(n))
z = seq(-1, 1, length.out = 100)
omega = 1

llr = function(x, y, z, omega) {
  fits = sapply(z, compute_f_hat, x, y, omega)
  return(fits)
}

compute_f_hat = function(z, x, y, omega) {
  r = abs(x - z) / omega
  Wz = sapply(r, W)
  X = make_predictor_matrix(x)
  f_hat = c(1, z) %*% solve(t(X) %*% sweep(X, 1, Wz, "*")) %*% t(X) %*% (Wz*y)
  return(f_hat)
}

make_weight_matrix = function(z, x, omega) {
  r = abs(x - z) / omega
  W_vec = sapply(r, W)
  Wz = diag(W_vec)
  return(Wz)
}

W = function(r) {
  if (abs(r) < 1) {
    return((1 - (abs(r))^3)^3)
  }
  else {
    return(0)
  }
}
  
make_predictor_matrix = function(x) {
  X = cbind(rep(1, length(x)), x)
  return(X)
}