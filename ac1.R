#O(N log N)
log_factorial <- function (n) {
  # Return the log of factorial(n) for any integer n > 0
  if (n <= 1)
    return (0)
  return (log(n) + log_factorial(n - 1))
}

#install.packages("microbenchmark")
library(microbenchmark)
n <- 1000
runtimes <- c()

for(i in seq(1, n, 10)) { 
  benchtimes <- microbenchmark(log_factorial(i))[,2]
  runtimes[i] <- median(benchtimes)
}

png(filename="~/gatech/classes/6242_DVA/DVA_2018Summer/ac1plot1.png")
plot(runtimes, xlab = "n", ylab = "Time in Nanoseconds", main = "log_factorial")
dev.off()


#O(N^2 log N)
sum_log_factorial <- function (n) {
  # Return the sum of log_factorial(i) for i in 1..n
  sum <- 0
  for(i in seq(1, n, 1)) {
    sum <- sum + log_factorial(i)
  }
  return (sum)
}

n <- 1000
runtimes <- c()
for (i in seq(1, n)) {
  runtimes[i] <- as.numeric(system.time(sum_log_factorial(i)))[1]
}

png(filename="~/gatech/classes/6242_DVA/DVA_2018Summer/ac1plot2.png")
plot(runtimes, xlab = "n", ylab = "Time in Seconds", main = "sum_log_factorial")
dev.off()



#O(2^N)
fibonacci <- function(n) {
  # Return nth Fibonacci number
  if (n <= 1)
    return (n)
  return (fibonacci(n - 1) + fibonacci(n - 2))
}

n <- 100
runtimes <- c()
for (i in seq(1, n, 1)) {
  runtimes[i] = as.numeric(system.time(fibonacci(i)))[1]
}

png(filename="~/gatech/classes/6242_DVA/DVA_2018Summer/ac1plot3.png")
plot(runtimes, xlab = "n", ylab = "Time in Seconds", main = "fibonacci")
dev.off()