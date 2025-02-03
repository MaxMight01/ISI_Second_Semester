# With X_bar_k defined as $1/k * \sum_{i=0}^{k} X_{k}$ with $X_{k}$ are Bernoulli random variables, ...

N <- 10000

P <- 0.9

plot_cumulative_mean <- function(n, p) {
  X <- rbinom(n, 1, p)
  cum_mean <- cumsum(X) / (1:n)
  return(cum_mean)
}

cum_mean <- plot_cumulative_mean(N, P)
plot(1:N, cum_mean, type = "l", xlab = "k", ylab = "X_bar_k", 
     main = "Plot of k against X_bar_k")

replicate(4, lines(1:N, plot_cumulative_mean(N, P), col = rgb(runif(1), runif(1), runif(1))))