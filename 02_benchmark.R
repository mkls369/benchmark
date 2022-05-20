library(microbenchmark)
times_n = 100
x <- 1e6:1e7

nthread <- 1
RhpcBLASctl::blas_set_num_threads(1)
RhpcBLASctl::omp_set_num_threads(1)

mb1 <- microbenchmark(x_sqrt = sqrt(x), times = times_n, unit = 'milliseconds')
df_mb1 <- as.data.frame(summary(mb1))
df_mb1$benchmark = 'mb1'

set.seed(369)
dta <- data.frame(close = rnorm(1e6))
dta_ttr <- TTR::BBands(dta)

mb2 <- microbenchmark(bband = TTR::BBands(dta), times = times_n, unit = 'milliseconds')
df_mb2 <- as.data.frame(summary(mb2))
df_mb2$benchmark = 'mb2'

set.seed(369)
dt_mat <- as.data.frame(cbind(rnorm(1e5), sapply(rep(1e5, 100), rnorm)))
colnames(dt_mat) <- c("Y", paste0("X_", 1:100))
mb3 <- microbenchmark(lm = lm(Y ~ ., data=dt_mat ), times = times_n, unit = 'milliseconds')

df_mb3 <- as.data.frame(summary(mb3))
df_mb3$benchmark = 'mb3'

df_all <- rbind(df_mb1, df_mb2, df_mb3)

write.csv(df_all, "02_benchmark.csv", row.names = FALSE)
print("02_benchmark.R done!")
