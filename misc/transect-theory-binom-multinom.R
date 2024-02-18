library(DescTools)
library(SOILmilaR)

# 30-point transect
x30 <- BinomCI(1:30, 30, conf.level = 0.8)
plot(1:30, x30[, 1], xlim = c(0, 30), ylim = c(0, 1))
lines(1:30, x30[, 2])
lines(1:30, x30[, 3])

BinomVarNorm(15, x30[,1])

# 10-point transect
x10 <- BinomCI(1:10, 10, conf.level = 0.8)
points(1:10, x10[, 1], xlim = c(0, 10), ylim = c(0, 1))
lines(1:10, x10[, 2])
lines(1:10, x10[, 3])

# 5-point transect
x5 <- BinomCI(1:5, 5, conf.level = 0.8)
points(1:5, x5[, 1], xlim = c(0, 5), ylim = c(0, 1))
lines(1:5, x5[, 2])
lines(1:5, x5[, 3])

# compare CI-width for 5, 10, 30 point transect
plot(1:30, x30[, 3] - x30[, 2],
     xlim = c(0, 30), ylim = c(0, 1),
     xlab = "Number of Stops",
     ylab = "80% Confidence Interval Width")
points(1:10, x10[, 3] - x10[, 2], pch = 2)
points(1:5, x5[, 3] - x5[, 2], pch = 3)
legend("topright",
       legend = c("n=30", "n=10", "n=5"),
       pch = 1:3,)

## multinomial (>2 classes) vs binomial

# complex 2 major + 2 minor
MultinomCI(c(5, 3, 1, 1), conf.level = 0.8)
BinomCI(c(5, 3, 1, 1), 10, conf.level = 0.8)

# complex 3 major
MultinomCI(c(4, 3, 2, 1), conf.level = 0.8)
BinomCI(c(4, 3, 2, 1), 10, conf.level = 0.8)

# consociation
MultinomCI(c(8, 1, 1), conf.level = 0.8)
BinomCI(c(8,1,1), 10, conf.level = 0.8)

# binom diff
BinomRatioCI(8, 10, 2, 10)

# worst case: number of samples needed to have composition CI width of 10% w/ 80% confidence
BinomCIn(p = 0.5, 0.10, conf.level = 0.8)
