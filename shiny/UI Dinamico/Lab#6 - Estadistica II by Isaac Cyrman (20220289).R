# Problema 1

# Datos del problema
antes <- c(140, 150, 160, 170, 180, 190, 155, 165, 175, 185)
despues <- c(135, 145, 155, 168, 175, 185, 150, 160, 170, 180)

# 1. Prueba T pareada
t_pareada <- t.test(antes, despues, paired = TRUE)
print("Prueba T pareada:")
print(t_pareada)

# 2. Prueba de signos
library(BSDA)
prueba_signos <- SIGN.test(antes, despues)
print("Prueba de signos:")
print(prueba_signos)

# 3. Prueba de rangos con signos de Wilcoxon
wilcoxon_pareada <- wilcox.test(antes, despues, paired = TRUE, exact = FALSE)
print("Prueba de rangos con signos de Wilcoxon:")
print(wilcoxon_pareada)


# Problema 2

# Crear los datos del problema
antes <- c("Aprobado", "No aprobado", "No aprobado", "Aprobado", 
           "Aprobado", "No aprobado", "Aprobado", "No aprobado", 
           "Aprobado", "No aprobado", "Aprobado", "No aprobado")
despues <- c("Aprobado", "Aprobado", "No aprobado", "No aprobado", 
             "Aprobado", "Aprobado", "Aprobado", "No aprobado", 
             "No aprobado", "Aprobado", "Aprobado", "No aprobado")

# Crear los datos en formato binario (0 = No aprobado, 1 = Aprobado)
antes_bin <- ifelse(antes == "Aprobado", 1, 0)
despues_bin <- ifelse(despues == "Aprobado", 1, 0)

# Prueba de signos pareada para los datos binarios
prueba_signos_bin <- SIGN.test(antes_bin, despues_bin)
print("Resultado de la prueba de signos pareada con datos binarios:")
print(prueba_signos_bin)


# Tabla de comparación de pares
tabla_pareada <- table(antes_bin, despues_bin)
print("Tabla pareada:")
print(tabla_pareada)

# Prueba de signos pareada
library(BSDA)
prueba_signos <- SIGN.test(antes_bin, despues_bin)
print("Resultado de la prueba de signos pareada:")
print(prueba_signos)
