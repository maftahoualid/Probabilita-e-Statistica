# Esempi di distribuzioni uniformi, esponenziali e normali con visualizzazione grafica

library(ggplot2)

# A. Distribuzione Uniforme

# Creazione di un dataframe per la distribuzione uniforme U(0,1)
# - x: sequenza di valori tra -0.5 e 1.5
# - PDF: densità di probabilità calcolata con dunif()
# - CDF: distribuzione cumulativa calcolata con punif()

df_0_1 <- data.frame(x = seq(-0.5, 1.5, 0.01),
                     PDF = dunif(x = seq(-0.5, 1.5, 0.01), min = 0, max = 1),
                     CDF = punif(seq(-0.5, 1.5, 0.01), min = 0, max = 1))

# Creazione del grafico con PDF e CDF
# - geom_line(): disegna le curve per la PDF e la CDF
# - scale_color_manual(): assegna colori distinti per migliorare la leggibilità

g_0_1 <- ggplot(df_0_1, aes(x = x)) + 
    geom_line(aes(y = PDF, color = "PDF")) +
    geom_line(aes(y = CDF, color = "CDF")) +
    scale_color_manual(values = c("cyan4", "brown3")) +
    scale_y_continuous(limits = c(0,2)) +
    labs(color = "Funzione", x = "Valori", 
         y = "Densità di probabilità",
         title = "Distribuzione Uniforme", 
         subtitle = "a = 0, b = 1")

# B. Distribuzione Esponenziale

# Creazione della sequenza di valori e dataframe per la distribuzione esponenziale
x <- seq(0, 4, 0.01)
rates <- c(0.5, 1, 2)

df_to_plot <- plyr::ldply(rates, function(rate){
    PDF = dexp(x = x, rate = rate)  # Calcola la densità di probabilità
    CDF = pexp(q = x, rate = rate)  # Calcola la distribuzione cumulativa
    return(data.frame(rate, x, PDF, CDF))
})

# Creazione del grafico della distribuzione esponenziale
# - geom_line(): traccia le curve per PDF e CDF per diversi valori di lambda

ggplot(df_to_plot, aes(x = x)) + 
    geom_line(aes(y = PDF, color = as.factor(rate), linetype = "PDF")) +
    geom_line(aes(y = CDF, color = as.factor(rate), linetype = "CDF")) +
    labs(color = "Tasso", x = "Valori", 
         y = "Densità di probabilità",
         title = "Distribuzione Esponenziale", 
         linetype = "Funzione")

# C. Distribuzione Normale

# Creazione di un dataframe per la distribuzione normale con media 0 e varianza 1
df_0_1 <- data.frame(x = seq(-5,5,0.01),
    PDF = dnorm(x = seq(-5,5,0.01), mean = 0, sd = 1),
    CDF = pnorm(q = seq(-5,5,0.01), mean = 0, sd = 1))

# Creazione del grafico della distribuzione normale
# - geom_line(): traccia le curve per PDF e CDF

g_0_1 <- ggplot(df_0_1, aes(x = x)) + 
    geom_line(aes(y = PDF, color = "PDF")) +
    geom_line(aes(y = CDF, color = "CDF")) +
    scale_color_manual(values = c("cyan4", "brown3")) +
    labs(color = "Funzione", x = "Valori", 
         y = "Densità di probabilità",
         title = "Distribuzione Normale",
         subtitle = "Media = 0, Varianza = 1")
