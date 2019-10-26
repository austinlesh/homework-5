## ECHO is on.

source("llr_functions.R")

run_time <- microbenchmark::microbenchmark(llr(x, y, z, omega), times = 100)

cat("llr function runs in", mean(run_time$time), "nanoseconds on average")