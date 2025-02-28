# Analisi di regressione lineare nel dataset palmerpenguins

# A. Analisi della relazione tra lunghezza della pinna e peso dei pinguini

library(palmerpenguins)
library(ggplot2)

# Caricamento del dataset e rimozione dei valori mancanti
df <- na.omit(penguins[, c("body_mass_g", "flipper_length_mm")])

# Creazione del modello di regressione lineare
m1 <- lm(body_mass_g ~ flipper_length_mm, data = df)

# Creazione del grafico di dispersione con la retta di regressione
# - geom_point(): rappresenta i dati campionari
# - geom_abline(): traccia la retta di regressione usando i coefficienti stimati
# - La formula della retta di regressione è: y = beta_0 + beta_1 * x
#   dove beta_0 (intercetta) e beta_1 (coefficiente angolare) sono stimati dal modello

ggplot(data = df, aes(x = flipper_length_mm, y = body_mass_g)) + 
    geom_point() + 
    geom_abline(aes(slope = coef(m1)[2], intercept = coef(m1)[1]), color = "red") +
    labs(x = "Lunghezza pinna (mm)", y = "Peso (g)",
         title = "Palmer Penguins",
         subtitle = "Relazione tra lunghezza della pinna e peso")

# B. Valutazione della qualità del modello di regressione

# Calcolo dell'indice di determinazione (R^2)
summary(m1)  # Output automatico con R^2

# Calcolo manuale dell'R^2
# Syy: somma dei quadrati totale
Syy <- sum((df$body_mass_g - mean(df$body_mass_g))^2)
# SSe: somma dei quadrati spiegata dal modello
SSe <- sum((fitted(m1) - mean(df$body_mass_g))^2)
# SSr: somma dei quadrati residua
SSr <- sum(residuals(m1)^2)
# R2: percentuale della variabilità spiegata dal modello
R2 <- SSe / Syy  

# C. Analisi dei residui

# Calcolo dei residui standardizzati
df$stdres <- residuals(m1) / sqrt(sum(residuals(m1)^2)/(nrow(df) - 2))
df$fitted <- fitted.values(m1)

# Creazione del grafico residui vs valori predetti
# - geom_point(): mostra i residui rispetto ai valori predetti dal modello
# - I residui dovrebbero distribuirsi casualmente attorno allo zero senza pattern visibili

ggplot(df, aes(x = fitted, y = stdres)) + 
    geom_point() +
    labs(x = "Valori Predetti", y = "Residui Standardizzati",
         title = "Analisi dei residui della regressione")

# D. Analisi della normalità dei residui con il Q-Q plot

# Creazione del grafico Q-Q per verificare se i residui seguono una distribuzione normale
# - geom_abline(intercept = 0, slope = 1): traccia la diagonale di riferimento che rappresenta la normalità
# - Se i punti si allineano lungo la diagonale, i residui sono distribuiti normalmente

probs <- seq(0, 1, length.out = nrow(df))
df$observed_q <- quantile(df$stdres, probs = probs)
df$theoretical_q <- qnorm(probs)

ggplot(df, aes(x = theoretical_q, y = observed_q)) + 
    geom_point() +
    geom_abline(aes(intercept = 0, slope = 1)) + 
    labs(x = "Quantili Teorici", y = "Quantili Osservati",
         title = "Q-Q Plot dei residui")

# E. Ripetizione dell'analisi con la lunghezza del becco come variabile indipendente

# Sostituiamo la lunghezza della pinna con la lunghezza del becco come predittore del peso
df <- na.omit(penguins[, c("body_mass_g", "bill_length_mm")])
m2 <- lm(body_mass_g ~ bill_length_mm, data = df)

# Creazione del grafico di regressione con la nuova variabile predittiva
# - geom_abline(): traccia la retta di regressione usando i nuovi coefficienti
# - La nuova equazione sarà: y = beta_0 + beta_1 * x

ggplot(data = df, aes(x = bill_length_mm, y = body_mass_g)) + 
    geom_point() + 
    geom_abline(aes(slope = coef(m2)[2], intercept = coef(m2)[1]), color = "red") +
    labs(x = "Lunghezza becco (mm)", y = "Peso (g)",
         title = "Palmer Penguins",
         subtitle = "Relazione tra lunghezza del becco e peso")

# Valutazione della qualità del nuovo modello
summary(m2)
