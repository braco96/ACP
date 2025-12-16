# ==============================================================================
# PROYECTO: ACP (Análisis de Componentes Principales)
# AUTOR: Luis Bravo Collado (Braco96)
# ==============================================================================

if(!require(pacman)) install.packages("pacman")
pacman::p_load(ggplot2, GGally, ggfortify, install = FALSE)

# Función auxiliar para carga robusta
cargar_datos <- function(nombre) {
  rutas <- c(nombre, file.path("Datos", nombre), file.path("..", nombre))
  for(r in rutas) { if(file.exists(r)) { load(r, .GlobalEnv); return(TRUE) } }
  return(FALSE)
}

print("=== PARTE 1: CASO PRÁCTICO (Datos Reales) ===")

# Problema 1: Sintéticos
set.seed(21121)
dat.t1 <- data.frame(x1=rnorm(100), x2=rnorm(100, sd=2), x3=rnorm(100, sd=0.5))
pairs(dat.t1, main="Pares Problema 1")
print("Varianzas:"); print(apply(dat.t1, 2, var))
acp1 <- princomp(dat.t1, cor=FALSE)
print("Resumen ACP1:"); print(summary(acp1))

# Problema 2: Mortalidad
if(cargar_datos("datos_prac2.RData") && exists("dat.t2")) {
  colnames(dat.t2)[5:20] <- c("todoH", "todoM", "digH", "digM", "respH", "respM", "cardvH", "cardvM", "maligH", "maligM", "cerebvH", "cerebvM", "isquemcH", "isquemcM", "diabH", "diabM")
  acp2 <- princomp(dat.t2[, 5:20], cor=TRUE)
  print("Resumen Mortalidad:"); print(summary(acp2))
  print(acp2$loadings[, 1:2])
  plot(acp2$scores[, 1:2], xlab="Comp 1", ylab="Comp 2", type="n")
  text(acp2$scores[, 1], acp2$scores[, 2], substr(dat.t2[, 4], 1, 4), cex=0.7)
}

# Problema 3: EPF
if(cargar_datos("EPF2.RData") && exists("EPF")) {
  acp3 <- princomp(EPF[, -1], cor=FALSE)
  print("Resumen EPF:"); print(summary(acp3))
  try(EPF[, 1] <- iconv(EPF[, 1], from="latin1", to="UTF-8"), silent=TRUE)
  plot(acp3$scores[, 1:2], xlab="Comp 1", ylab="Comp 2", type="n", main="Gasto Familiar")
  text(acp3$scores[, 1], acp3$scores[, 2], substr(EPF[, 1], 1, 4), cex=0.7)
}

print("=== PARTE 2: ANEXO TEÓRICO (Proteínas Europa) ===")
paises <- c("Albania", "Austria", "Belgica", "Bulgaria", "Checoslovaquia", "Dinamarca", "E_Alemania", "Finlandia", "Francia")
set.seed(100)
datos <- matrix(rnorm(length(paises)*9, 10, 5), nrow=length(paises))
rownames(datos) <- paises
colnames(datos) <- c("CR", "CB", "H", "L", "P", "C", "F", "N", "FV")
pca_res <- prcomp(datos, scale.=TRUE)
autoplot(pca_res, data=datos, label=TRUE, loadings=TRUE, main="Biplot ACP Simulado")
