# Visualizzazione di distribuzioni statistiche con aree critiche

# A. Creazione di un dataframe per la distribuzione normale standardizzata e t di Student

# Creiamo una sequenza di valori z tra -4 e 4 per rappresentare la distribuzione normale e t di Student
z <- seq(-4, 4, length.out = 1000)

# Calcoliamo la densità della distribuzione normale standardizzata
y <- dnorm(z)  # PDF della normale

# Calcoliamo la densità della distribuzione t di Student con 5 gradi di libertà
t <- dt(z, 5)  # PDF della t di Student

df <- data.frame(z = z, y = y, t = t)  # Creazione del dataframe con entrambe le distribuzioni

library(ggplot2)

# B. Grafici della distribuzione normale con aree critiche

# Creazione del grafico della normale con intervallo critico z[alpha/2]
ggplot(df, aes(x = z, y = y)) +
    geom_line()  +  # Traccia la curva della PDF
    geom_segment(aes(y = 0, yend = y, x = 0, xend = 0), linetype = "dashed", color = "grey70") +  # Linea verticale al centro
    scale_x_continuous(breaks = c(-1.96, 0, 1.96), labels = c(expression(-z[alpha/2]), 0, expression(z[alpha/2]))) +  # Etichette asse x
    geom_ribbon(data = df[df$z < -1.96,], aes(ymin = 0, ymax = y), fill = "grey70") +  # Area critica sinistra
    geom_ribbon(data = df[df$z > 1.96,], aes(ymin = 0, ymax = y), fill = "grey70") +  # Area critica destra
    theme_minimal() +
    labs(x = "z values", y = "Standardized Normal PDF")

# Creazione del grafico per il test unilaterale con z[alpha]
ggplot(df, aes(x = z, y = y)) +
    geom_line()  +
    geom_segment(aes(y = 0, yend = y, x = 0, xend = 0), linetype = "dashed", color = "grey70") +
    scale_x_continuous(breaks = c(-1.64, 0, 1.64), labels = c("", 0, expression(z[alpha]))) +
    geom_ribbon(data = df[df$z > 1.64,], aes(ymin = 0, ymax = y), fill = "grey70") +  # Area critica a destra
    theme_minimal() +
    labs(x = "z values", y = "Standardized Normal PDF")

# C. Confronto tra distribuzione normale e distribuzione t di Student

ggplot(df, aes(x = z, y = t)) +
    geom_line(aes(linetype = "Student's T"))  +  # Curva della distribuzione t di Student
    geom_segment(aes(y = 0, yend = t, x = 0, xend = 0), linetype = "dashed", color = "grey70") +
    scale_x_continuous(breaks = c(-1.96, 0, 1.96), 
                       labels = c(expression(-t[alpha/2(n-1)]), 0, expression(t[alpha/2(n-1)]))) +
    geom_ribbon(data = df[df$z < -1.96,], aes(ymin = 0, ymax = t), fill = "grey70") +
    geom_ribbon(data = df[df$z > 1.96,], aes(ymin = 0, ymax = t), fill = "grey70") +
    geom_line(aes(y = y, linetype = "Normal"))  +  # Curva della distribuzione normale
    theme_minimal() +
    scale_linetype_manual(values = c("dotted", "solid"), name = "PDFs") +
    labs(x = "t values", y = "Student's T (n-1) PDF")

# D. Distribuzione Chi-quadrato con evidenziazione delle aree critiche

# Creiamo una sequenza di valori per la distribuzione chi-quadrato
x <- seq(0, 20, length.out = 1000)

# Calcoliamo la densità della distribuzione chi-quadrato con 5 gradi di libertà
q <- dchisq(x, df = 5)
df_q <- data.frame(x = x, q = q)

ggplot(df_q, aes(x = x, y = q)) +
    geom_line()  +  # Curva della distribuzione chi-quadrato
    geom_segment(aes(y = 0, yend = q, x = 0, xend = 0), linetype = "dashed", color = "grey70") +
    scale_x_continuous(breaks = c(qchisq(0.025, 5), qchisq(1 - 0.025, 5)), 
                       labels = c(expression(chi[1-alpha/2(n-1)]^2), expression(chi[alpha/2(n-1)]^2))) +  # Etichette degli intervalli critici
    geom_ribbon(data = df_q[df_q$x < qchisq(0.025, 5),], aes(ymin = 0, ymax = q), fill = "grey70") +  # Area critica sinistra
    geom_ribbon(data = df_q[df_q$x > qchisq(1 - 0.025, 5),], aes(ymin = 0, ymax = q), fill = "grey70") +  # Area critica destra
    theme_minimal() +
    labs(x = "Chi-Squared values", y = "Chi-Square PDF")
