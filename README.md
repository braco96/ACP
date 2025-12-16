# Proyecto: ACP

## 👤 Autoría
* **Refactorización:** Luis Bravo Collado
* **Base Teórica:** Inma Garcés Andrés (Estadística II)

## 📚 Fundamentación Teórica: Análisis de Componentes Principales
El ACP es una técnica de reducción de dimensionalidad que transforma variables correlacionadas en variables ortogonales.

### 1. Obtención de las Componentes
Se obtienen mediante la **descomposición espectral** de la matriz de varianzas-covarianzas (o correlaciones) muestral $S$.
* Los vectores propios $e_j$ definen los ejes de las nuevas componentes.
* Los valores propios $\lambda_j$ cuantifican la cantidad de varianza explicada por cada eje.

### 2. Criterio de Selección
La proporción de varianza total explicada por la $j$-ésima componente es:

$$ \frac{\lambda_j}{\text{Traza}(S)} $$

Se suelen retener las $k$ primeras componentes que expliquen un porcentaje acumulado suficiente (ej. 80%).

## 💻 Sobre el Código
El script `ACP.R` utiliza las funciones `princomp` y `prcomp` para realizar la descomposición y visualiza los resultados mediante **Scree Plots** y **Biplots**.
