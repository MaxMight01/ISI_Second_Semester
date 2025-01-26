# A fair coin is flipped three times. Find the probaility that the second flip is a tails, given that the number of tails that have occured is at most one. # nolint

n <- 10000
c <- 0
for (i in 1:n) {
  a <- sample(c(1, 0), 3, replace = TRUE)
  while (sum(a) > 1) {
    a <- sample(c(1, 0), 3, replace = TRUE)
  }
  b <- a[2]
  c <- c + b
}
c / n