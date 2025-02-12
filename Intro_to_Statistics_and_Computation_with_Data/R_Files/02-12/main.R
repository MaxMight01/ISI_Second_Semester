# Generate a sample of size 500 from Exp(2), taking them as iid datapoints.

num_intervals <- 60
n <- 500
rate <- 2
alpha <- 0.005
mu <- 1 / rate

intervals <- matrix(NA, nrow = num_intervals, ncol = 2)

for (i in 1:num_intervals) {
    X <- rexp(n, rate = rate)
    
    X_bar <- mean(X)
    s <- sd(X)
    
    z <- qnorm(1 - alpha / 2)
    
    margin_error <- z * (s / sqrt(n))
    
    l_bound <- X_bar - margin_error
    u_bound <- X_bar + margin_error
    
    intervals[i, ] <- c(l_bound, u_bound)
}


pdf("exponential_confidence_intervals.pdf")
plot(1:num_intervals, intervals[, 1], type = "n", ylim = range(intervals), xlim = c(1, num_intervals),
     xlab = "Interval Number",
     ylab = "Confidence Interval", main = "Multiple Confidence Intervals")
abline(h = mu)

for (i in 1:num_intervals) {
    l_bound <- intervals[i, 1]
    u_bound <- intervals[i, 2]

    color <- "red"
    
    if (l_bound <= mu && u_bound >= mu) {
        color <- "black"
    }

    segments(x0 = i, y0 = l_bound, y1 = u_bound, col = color)
}

sample_means <- apply(intervals, 1, mean)
points(1:num_intervals, sample_means)