# Confronto tra PDF e CDF di variabili casuali Uniformi continue

# A. Generazione delle funzioni di densità di probabilità (PDF) e delle funzioni
#    di distribuzione cumulativa (CDF) per variabili casuali uniformi.

# Definiamo una sequenza di valori x da -3.5 a 4.5 per coprire tutti gli intervalli possibili.
x <- seq(-3.5, 4.5, length.out = 1000)

# Creazione di un dataframe con i valori di densità (PDF) e distribuzione cumulativa (CDF)
# per quattro distribuzioni Uniformi con diversi intervalli.
df_unif <- data.frame(x = x,
    x1_pdf = dunif(x, 0, 1),   # PDF di U(0,1)
    x1_cdf = punif(x, 0, 1),   # CDF di U(0,1)
    x2_pdf = dunif(x, -3, 2),  # PDF di U(-3,2)
    x2_cdf = punif(x, -3, 2),  # CDF di U(-3,2)
    x3_pdf = dunif(x, 2, 4),   # PDF di U(2,4)
    x3_cdf = punif(x, 2, 4),   # CDF di U(2,4)
    x4_pdf = dunif(x, 0.8, 2.5),  # PDF di U(0.8,2.5)
    x4_cdf = punif(x, 0.8, 2.5))  # CDF di U(0.8,2.5)

# Convertiamo il dataframe in formato long per facilitare la creazione dei grafici con ggplot2.
df_to_plot <- reshape2::melt(df_unif, id = "x")

# B. Creazione di un grafico con due pannelli verticali: la prima riga mostra le PDF,
#    la seconda riga mostra le CDF.

library(ggplot2)

# Creiamo un filtro per selezionare solo le righe con la PDF (quelle con "pdf" nel nome della colonna)
only_PDFs <- grepl(pattern = "pdf", x = df_to_plot$variable)

# Creazione del grafico delle PDF
# - geom_line(): traccia la curva della funzione di densità di probabilità (PDF)
# - scale_color_manual(): assegna colori personalizzati per distinguere le diverse distribuzioni

gPDF <- ggplot(df_to_plot[only_PDFs, ], aes(x = x, y = value)) + 
    geom_line(aes(color = variable)) +  # Aggiunge linee per rappresentare la PDF di ciascuna distribuzione
    labs(y = "PDF", title = "Variabili casuali uniformi",
         subtitle = "Funzioni di densità di probabilità") + 
    scale_color_manual(labels = c("U(0,1)", "U(-3,2)", 
                                  "U(2,4)", "U(0.8,2.5)"),
                       values = RColorBrewer::brewer.pal(4, "Set1"))

# Creazione del grafico delle CDF
# - geom_line(): traccia la curva della funzione di distribuzione cumulativa (CDF)
# - linetype: differenzia le curve per maggiore leggibilità

gCDF <- ggplot(df_to_plot[!only_PDFs, ], aes(x = x, y = value)) + 
    geom_line(aes(color = variable, linetype = variable)) +  # Differenzia le curve con tipi di linee diverse
    labs(y = "CDF", title = "Variabili casuali uniformi",
         subtitle = "Funzioni di distribuzione cumulativa") + 
    scale_color_manual(labels = c("U(0,1)", "U(-3,2)", 
                                  "U(2,4)", "U(0.8,2.5)"),
                       values = RColorBrewer::brewer.pal(4, "Set1"))

# Uniamo i due grafici in un unico layout con due pannelli utilizzando il pacchetto patchwork
library(patchwork)
gPDF / gCDF  # Dividiamo verticalmente i due grafici per una migliore visualizzazione comparativa
