# Grafico iniziale
set.seed(123)
n <- 10
x = 1:n
y = 1:n + runif(n, -2, 2)
df <- data.frame(x = x, y = y)

B <- (sum(x * y) - n * mean(x) * mean(y)) /
    (sum(x^2) - n * mean(x)^2)
A <- mean(y) - B * mean(x)

library(ggplot2)
ggplot(df, aes(x = x, y = y)) +
    geom_segment(aes(x = x, xend = x, y = A + B*x, yend = y), 
                 color = "cyan4") +
    geom_point(color = "brown3") +
    geom_abline(aes(slope = B, intercept = A)) + 
    labs(title = "Estimated regression line",
         subtitle = paste0("A = ", round(A, 2), 
                           ", B = ", round(B, 2))) +
    geom_hline(aes(yintercept = mean(y)), linetype = "dotted")

model <- lm(y ~ x, data = df)
model

names(model)
coef(model)

summary(model)

# Grafici ispezione dati + retta
set.seed(123)
n <- 50
x = 1:n
y1 = 1:n + runif(n, -5, 5)
y2 = seq(n, 1) + runif(n, -5, 5)
y3 = 1:n + runif(n, -30, 30)
y4 = seq(n, 1) + runif(n, -30, 30)
y5 = runif(n, 25-5, 25+5)
y6 = runif(n, 25-30, 25+30)
y7 = (1:n)^2 + runif(n, -30, 30)
y8 = (1:n)^3 + runif(n, -10000, 10000)
df <- data.frame(x = x, y1 = y1, y2 = y2, y3 = y3, y4 = y4, y5 = y5, y6 = y6,
                 y7 = y7, y8 = y8)

m1 <- lm(y1 ~ x, data = df)
m2 <- lm(y2 ~ x, data = df)
m3 <- lm(y3 ~ x, data = df)
m4 <- lm(y4 ~ x, data = df)
m5 <- lm(y5 ~ x, data = df)
m6 <- lm(y6 ~ x, data = df)
m7 <- lm(y7 ~ x, data = df)
m8 <- lm(y8 ~ x, data = df)

g1 <- ggplot(df, aes(x = x, y = y1)) +
    geom_point(color = "brown3") +
    geom_abline(aes(slope = coef(m1)[2], intercept = coef(m1)[1])) +
    labs(y = "y") +
    scale_y_continuous(limits = c(-10, 80))
g2 <- ggplot(df, aes(x = x, y = y2)) +
    geom_point(color = "brown3") +
    geom_abline(aes(slope = coef(m2)[2], intercept = coef(m2)[1])) +
    labs(y = "y") +
    scale_y_continuous(limits = c(-10, 80))

g3 <- ggplot(df, aes(x = x, y = y3)) +
    geom_point(color = "brown3") +
    geom_abline(aes(slope = coef(m3)[2], intercept = coef(m3)[1])) +
    labs(y = "y") +
    scale_y_continuous(limits = c(-10, 80))

g4 <- ggplot(df, aes(x = x, y = y4)) +
    geom_point(color = "brown3") +
    geom_abline(aes(slope = coef(m4)[2], intercept = coef(m4)[1])) +
    labs(y = "y") +
    scale_y_continuous(limits = c(-10, 80))

g5 <- ggplot(df, aes(x = x, y = y5)) +
    geom_point(color = "brown3") +
    geom_abline(aes(slope = coef(m5)[2], intercept = coef(m5)[1])) +
    labs(y = "y") +
    scale_y_continuous(limits = c(-10, 80))

g6 <- ggplot(df, aes(x = x, y = y6)) +
    geom_point(color = "brown3") +
    geom_abline(aes(slope = coef(m6)[2], intercept = coef(m6)[1])) +
    labs(y = "y") +
    scale_y_continuous(limits = c(-10, 80))

g7 <- ggplot(df, aes(x = x, y = y7)) +
    geom_point(color = "brown3") +
    geom_abline(aes(slope = coef(m7)[2], intercept = coef(m7)[1])) +
    labs(y = "y")

g8 <- ggplot(df, aes(x = x, y = y8)) +
    geom_point(color = "brown3") +
    geom_abline(aes(slope = coef(m8)[2], intercept = coef(m8)[1])) +
    labs(y = "y")

library(patchwork)
(g1/g2) | (g3/g4) | (g5/g6) | (g7/g8)

# Grafici ispezione retta dati + residui standardizzati
set.seed(123)
n <- 50
x = 1:n
y1 = 1:n + runif(n, -5, 5)
y2 = 1:n + runif(n, -5, 5) * (1:n)^1/4
dfr <- data.frame(x = x, y1 = y1, y2 = y2)

m1r <- lm(y1 ~ x, data = dfr)
m2r <- lm(y2 ~ x, data = dfr)

dfr$res1 <- residuals(m1r)
dfr$res2 <- residuals(m2r)
dfr$fitted1 <- fitted(m1r)
dfr$fitted2 <- fitted(m2r)
dfr$stdres1 <- residuals(m1r) / sqrt(sum(residuals(m1r)^2)/m1r$df.residual)
dfr$stdres2 <- residuals(m2r) / sqrt(sum(residuals(m2r)^2)/m2r$df.residual)

g1r <- ggplot(dfr, aes(x = x, y = y1)) +
    geom_point(color = "brown3") +
    geom_abline(aes(slope = coef(m1r)[2], intercept = coef(m1r)[1])) +
    labs(y = "y")
g2r <- ggplot(dfr, aes(x = x, y = y2)) +
    geom_point(color = "brown3") +
    geom_abline(aes(slope = coef(m2r)[2], intercept = coef(m2r)[1])) +
    labs(y = "y")

g1rs <- ggplot(dfr, aes(x = fitted1, y = stdres1)) +
    geom_point(color = "brown3") +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    labs(x = "Fitted values", y = "Standardized Residuals")

g2rs <- ggplot(dfr, aes(x = fitted2, y = stdres2)) +
    geom_point(color = "brown3") +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    labs(x = "Fitted values", y = "Standardized Residuals")

(g1r | g1rs) / (g2r | g2rs)

# Residuals 

set.seed(123)
n <- 50
x = 1:n
y1 = runif(n, -1, 1)
y2 = runif(n, -1, 1) * c(1:n)
y3 = runif(n, -1, 1) * c(1:(n/2), seq(n/2+1, 2))
y4 = ((seq(-25, 24))^2)^1/4 + runif(n, -20, 20)
y4 = (y4-mean(y4))/12
df_res <- data.frame(x = x, y1 = y1, y2 = y2, y3 = y3, y4 = y4)

r1 <- ggplot(df_res, aes(x = x, y = y1)) +
    geom_point(color = "brown3") +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    labs(y = "residuals")

r2 <- ggplot(df_res, aes(x = x, y = y2)) +
    geom_point(color = "brown3") +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    labs(y = "residuals")

r3 <- ggplot(df_res, aes(x = x, y = y3)) +
    geom_point(color = "brown3") +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    labs(y = "residuals")

r4 <- ggplot(df_res, aes(x = x, y = y4)) +
    geom_point(color = "brown3") +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    labs(y = "residuals")

(r1 | r2) / (r3 | r4)

# QQ plots

set.seed(123)
y1 <- rnorm(100)
y2 <- rt(100, 6)
y3 <- c(rnorm(70), rnorm(30, 3))

probs <- seq(0, 1, by= 1/length(y1)) 
observed_q <- quantile(stdres, probs)

dfqq <- data.frame("x" = qnorm(probs), "y1" = quantile(y1, probs),
                   "y2" = quantile(y2, probs), "y3" = quantile(y3, probs))

qq1 <- ggplot(data = dfqq, aes(x = x, y = y1)) + 
    geom_point(color = "brown3") +
    geom_abline(aes(intercept = 0, slope = 1)) +
    labs(x = "Theoretical quantiles", 
         y = "Observed quantiles",
         title = "")

qq2 <- ggplot(data = dfqq, aes(x = x, y = y2)) + 
    geom_point(color = "brown3") +
    geom_abline(aes(intercept = 0, slope = 1)) +
    labs(x = "Theoretical quantiles", 
         y = "Observed quantiles")

qq3 <- ggplot(data = dfqq, aes(x = x, y = y3)) + 
    geom_point(color = "brown3") +
    geom_abline(aes(intercept = 0, slope = 1)) +
    labs(x = "Theoretical quantiles", 
         y = "Observed quantiles")

hist1 <- ggplot(data = dfqq, aes(x = y1)) + 
    geom_histogram(aes(y = after_stat(density)), binwidth = 1) +
    geom_density(data = data.frame(x = rnorm(10000)), aes(x = x)) +
    labs(x = "Observed values", 
         y = "Density", title = "Normal distribution")

hist2 <- ggplot(data = dfqq, aes(x = y2)) + 
    geom_histogram(aes(y = after_stat(density)), binwidth = 1) +
    geom_density(data = data.frame(x = rnorm(10000)), aes(x = x)) +
    labs(x = "Observed values", 
         y = "Density", title = "Heavy tails")

hist3 <- ggplot(data = dfqq, aes(x = y3)) + 
    geom_histogram(aes(y = after_stat(density)), binwidth = 1) +
    geom_density() +
    labs(x = "Observed values", 
         y = "Density", title = "Skewed distribution")

(hist1 + qq1) / (hist2 + qq2) / (hist3 + qq3)

# FAQ qq-plot
# Istruzioni qq-plot

stdres <- c(0.8,  0.4, -0.1,  0.0,  0.3,  
            0.1,  1.5, -0.2, -0.5, -0.4)

observed_quantiles <- quantile(x = stdres, 
                               probs = seq(0.1, 0.9, by = 0.1), type = 2)
theoretical_quantiles <- round(qnorm(seq(0.1, 0.9, by = 0.1)), 2)

plot(theoretical_quantiles, observed_quantiles,
     ylab = "Quantili osservati", xlab = "Quantili teorici",
     main = "Normal Q-Q plot")
abline(0,1)

# Esempi qq-plot
set.seed(4)
x <- rnorm(100)
y1 <- rnorm(100)
set.seed(51)
y2 <- rt(100, 3)
y3 <- c(rnorm(70), rnorm(30, 2))
probs <- seq(0.01, 0.99, by = 0.01)
dfqq <- data.frame("x" = qnorm(probs), "y1" = quantile(y1, probs),
                   "y2" = quantile(y2, probs), "y3" = quantile(y3, probs))

qq1 <- ggplot(data = dfqq, aes(x = x, y = y1)) + 
    geom_point(shape = 1) +
    geom_abline(aes(intercept = 0, slope = 1)) +
    labs(x = "Theoretical quantiles", 
         y = "Observed quantiles",
         title = "Normal Q-Q plot",
         subtitle = "Satisfactory")

qq2 <- ggplot(data = dfqq, aes(x = x, y = y2)) + 
    geom_point(shape = 1) +
    geom_abline(aes(intercept = 0, slope = 1)) +
    labs(x = "Theoretical quantiles", 
         y = "Observed quantiles",
         title = "Normal Q-Q plot",
         subtitle = "Heavy tails")

qq3 <- ggplot(data = dfqq, aes(x = x, y = y3)) + 
    geom_point(shape = 1) +
    geom_abline(aes(intercept = 0, slope = 1)) +
    labs(x = "Theoretical quantiles", 
         y = "Observed quantiles",
         title = "Normal Q-Q plot",
         subtitle = "Mean is higher than zero")

qq1 + qq2 + qq3

