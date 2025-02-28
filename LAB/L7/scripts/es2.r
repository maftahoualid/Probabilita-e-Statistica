# Intervalli di confidenza per la media di una distribuzione normale

# A. Costruzione di un intervallo di confidenza al 95% (two-sided) per la media

# Supponiamo che il valore ricevuto in una trasmissione segua una distribuzione normale 
# con varianza nota (sigma^2 = 4). Lo stesso segnale è stato inviato 9 volte e i valori ricevuti sono:

data <- c(5, 8.5, 12, 15, 7, 9, 7.5, 6.5, 10.5)  # Campione di dati
n <- length(data)  # Numero di osservazioni
sigma <- 2  # Deviazione standard nota
alpha <- 0.05  # Livello di significatività

# Calcoliamo il valore critico della distribuzione normale standardizzata per un IC al 95%
z <- qnorm(alpha/2, lower.tail = FALSE)

# Calcoliamo la media campionaria
x_bar <- mean(data)

# Calcoliamo l'intervallo di confidenza al 95% (two-sided)
CI_95_two = c(x_bar - z * sigma / sqrt(n), x_bar + z * sigma / sqrt(n))
CI_95_two  # Stampa del# A researcher wants to determine if the average weight of a certain product is 
# significantly different from the claimed weight of 500 grams. A sample of 10 
# elements have been collected and each element was weighted. Conduct a 
# hypothesis test to test this claim following the instructions below.

weights <- c(488, 492, 498, 500, 502, 501, 495, 490, 503, 499)

# A. Find a bilateral confidence interval for the mean (95% confidence). 

alpha = 0.05 # Set the confidence level: 1-alpha = 0.95
n = length(weights) # Determine the sample size
avg_weight <- mean(weights) # Determine the sample mean
s_weight <- sd(weights) # Determine the sample standard deviation
# Determine the Confidence Interval
confidence_interval <- c(
    avg_weight - qt(alpha/2, df = n-1, lower.tail = FALSE) * s_weight/sqrt(n),
    avg_weight + qt(alpha/2, df = n-1, lower.tail = FALSE) * s_weight/sqrt(n))

# B. Write the system of hypotheses, then compute the p-value.

# H0 : weight = 500
# H1 : weight != 500
stat_test <- (avg_weight - 500) / (s_weight / sqrt(n))
p_value <- 2 * pt(q = abs(stat_test), df = n-1, lower.tail = FALSE)

# The p-value is border line. It is close to 0.05 and 0.1 which are two commonly
# used threshold to determine significance. If we set the significance level at
# 0.05, we accept the null hypothesis. 

# C. What if the researcher wants to determine if the average weight is 
#    significantly lower than 500g? Compute the p-value corresponding to this 
#    hypothesis test.

# H0 : weight >= 500
# H1 : weight < 500

p_value <- pt(q = stat_test, df = n-1, lower.tail = TRUE)

# We reject the null hypothesis.  
# The p-value is lower than the threshold.

# D. Repeat A., B., and C. using the function t.test() function.

t.test(x = weights, alternative = "two.sided", mu = 500, conf.level = 0.95)
t.test(x = weights, alternative = "less", mu = 500, conf.level = 0.95)

# Summarizing, we accept the hypothesis that the sample was extracted from a 
# population with mu = 500. However, we reject the hypothesis that the
# observed weights are extracted from a population with mu equals to 500 or 
# greater (which is a stronger statement).l'intervallo di confidenza

# B. Calcolo dell'intervallo di confidenza inferiore al 95% (one-sided)

# Per un IC inferiore, utilizziamo un unico valore critico di z per la coda superiore
z <- qnorm(alpha, lower.tail = FALSE)

# Costruiamo l'intervallo inferiore
CI_95_low = c(-Inf, x_bar + z * sigma / sqrt(n))
CI_95_low  # Stampa dell'intervallo inferiore

# C. Costruzione di un intervallo di confidenza al 99% (two-sided) per la media

alpha = 0.01  # Nuovo livello di significatività
z <- qnorm(alpha/2, lower.tail = FALSE)  # Valore critico per un IC al 99%

# Calcoliamo l'intervallo di confidenza al 99%
CI_99_two = c(x_bar - z * sigma / sqrt(n), x_bar + z * sigma / sqrt(n))
CI_99_two  # Stampa dell'intervallo di confidenza

# D. Intervallo di confidenza al 95% con deviazione standard sconosciuta

# Supponiamo ora che la deviazione standard della popolazione non sia nota e sia stimata dai dati.
# In questo caso, utilizziamo la distribuzione t di Student al posto della distribuzione normale.

s <- sd(data)  # Deviazione standard campionaria
alpha = 0.05

# Valore critico della distribuzione t di Student
# Il numero di gradi di libertà è dato da n - 1
t <- qt(alpha/2, df = n - 1, lower.tail = FALSE)

# Calcoliamo l'intervallo di confidenza al 95% con deviazione standard sconosciuta
CI_95_two_unk = c(x_bar - t * s / sqrt(n), x_bar + t * s / sqrt(n))
CI_95_two_unk  # Stampa dell'intervallo di confidenza con deviazione standard stimata
