# Calcolo della probabilità che una persona abbia una malattia dato un test positivo (Teorema di Bayes)

# Definizione delle probabilità note
P_EgivenD <- 0.99 # Probabilità che il test sia positivo se la persona ha la malattia P(E|D)
P_EgivenDc <- 0.01 # Probabilità di un falso positivo, ovvero che il test sia positivo se la persona è sana P(E|Dc)
P_D <- 0.005 # Probabilità a priori che una persona abbia la malattia P(D)

# Calcolo della probabilità congiunta di avere la malattia e un test positivo
P_DE <- P_D * P_EgivenD # P(DE) = Probabilità di un vero positivo (TP : true positive)

# Calcolo della probabilità complementare (persone sane)
P_Dc <- (1 - P_D) # P(Dc) = Probabilità che la persona sia sana (TN : true negative)

# Applicazione del Teorema di Bayes per calcolare la probabilità a posteriori
# che una persona abbia la malattia dato che il test è positivo
P_DgivenE <- P_DE / (P_DE + P_EgivenDc * P_Dc) 

# Risultato: probabilità che una persona abbia la malattia se il test è positivo
P_DgivenE
