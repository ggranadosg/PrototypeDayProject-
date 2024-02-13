#Este archivo contiene el código de análisis del archivo de carrosusados.csv para el PD
#El archivo de shiny precisa que este haya corrido previamente
# Cargar paquetes necesarios
library(tidyverse)
library(shiny)
library(ggplot2)

# Leer el archivo CSV
carros_usados <- read.csv("D:/BajadoEnD/20231218Módulo05Estadística&ProgramaciónConR/PrototypeDay/carrosusados.csv")

# Limpieza de datos
carros_usados_limpio <- carros_usados %>%
  # Eliminar filas duplicadas
  distinct() %>%
  # Eliminar filas con valores NA en columnas clave
  drop_na(c("manufacturer", "model", "year", "mileage", "price")) %>%
  # Convertir año a formato numérico y filtrar años razonables
  mutate(year = as.numeric(year),
         year = ifelse(year >= 1980 & year <= as.numeric(format(Sys.Date(), "%Y")), year, NA)) %>%
  drop_na(year) %>%
  # Ajustar tipos de datos
  mutate(mileage = as.numeric(mileage),
         price = as.numeric(price),
         accidents_or_damage = as.logical(accidents_or_damage),
         one_owner = as.logical(one_owner),
         personal_use_only = as.logical(personal_use_only)) %>%
  # Lidiar con valores faltantes o NA en 'seller_rating' y 'driver_rating'
  mutate(seller_rating = ifelse(is.na(seller_rating), mean(seller_rating, na.rm = TRUE), seller_rating),
         driver_rating = ifelse(is.na(driver_rating), mean(driver_rating, na.rm = TRUE), driver_rating))

# Limpiar datos: eliminar columnas innecesarias
carros_usados_limpio <- carros_usados_limpio %>%
  select(-seller_name, -driver_rating, -driver_reviews_num)

# Convertir columnas categóricas a tipo factor si es necesario
carros_usados_limpio <- carros_usados_limpio %>%
  mutate_if(is.character, as.factor)

#uso solo 10,000 registros para no perder tanto tiempo en las gráficas
carros_usados_limpio <- head(carros_usados_limpio,1000)

# Análisis de correlación (Solo para variables numéricas)
# Asumiendo que todas las variables necesarias son numéricas; de lo contrario, ajusta según sea necesario.
correlaciones <- cor(carros_usados_limpio %>% select_if(is.numeric), use = "complete.obs")
correlaciones_con_precio <- correlaciones["price",]
print(correlaciones_con_precio)

# Identificar las variables con la correlación más fuerte con 'price'
# Esto imprimirá las correlaciones; puedes inspeccionarlas para determinar las más fuertes.
print(sort(abs(correlaciones_con_precio), decreasing = TRUE))


#####
#Análisis del conjunto de datos
# Visualizar las primeras filas de los datos
head(carros_usados_limpio)

# Resumen de las características numéricas
summary(carros_usados_limpio)

# Histogramas de las variables numéricas
par(mfrow = c(2, 2))
hist(carros$precio, main = "Distribución del precio", xlab = "Precio")
hist(carros$edad_del_modelo, main = "Distribución de la edad del modelo", xlab = "Edad del modelo")
hist(carros$km_por_anio, main = "Distribución de los kilómetros por año", xlab = "Kilómetros por año")

"Este código ajusta 4 modelos diferentes (regresión lineal, regresión polinómica de grado 2, 
árbol de decisión y bosque aleatorio) al conjunto de datos de entrenamiento y evalua su rendimiento 
en el conjunto de prueba utilizando el error cuadrático medio (MSE)"

# Dividir los datos en conjunto de entrenamiento y prueba (80% train, 20% test)
set.seed(123)  # Para reproducibilidad
train_index <- sample(1:nrow(carros), 0.8 * nrow(carros))
train_data <- carros[train_index, ]
test_data <- carros[-train_index, ]

# Modelo de regresión lineal
lm_model <- lm(precio ~ edad_del_modelo + km_por_anio, data = train_data)
lm_pred <- predict(lm_model, newdata = test_data)
lm_mse <- mean((lm_pred - test_data$precio)^2)
cat("Regresión Lineal MSE:", lm_mse, "\n")

# Modelo de regresión polinómica de grado 2
poly_model <- lm(precio ~ poly(edad_del_modelo, 2) + poly(km_por_anio, 2), data = train_data)
poly_pred <- predict(poly_model, newdata = test_data)
poly_mse <- mean((poly_pred - test_data$precio)^2)
cat("Regresión Polinómica de Grado 2 MSE:", poly_mse, "\n")

# Modelo de árbol de decisión
library(rpart) 
tree_model <- rpart(precio ~ edad_del_modelo + km_por_anio, data = train_data)
tree_pred <- predict(tree_model, newdata = test_data)
tree_mse <- mean((tree_pred - test_data$precio)^2)
cat("Árbol de Decisión MSE:", tree_mse, "\n")

# Modelo de bosque aleatorio
library(randomForest)
rf_model <- randomForest(precio ~ edad_del_modelo + km_por_anio, data = train_data)
rf_pred <- predict(rf_model, newdata = test_data)
rf_mse <- mean((rf_pred - test_data$precio)^2)
cat("Bosque Aleatorio MSE:", rf_mse, "\n")

"Basándonos en estos resultados, podemos determinar que la regresión polinómica de grado 2 tiene el MSE (Error Cuadrático Medio) 
más bajo, lo que indica que tiene un mejor rendimiento en la predicción del precio en comparación con los otros 
modelos probados. 
Por lo tanto, podemos concluir que el mejor modelo de machine learning para predecir el precio 
utilizando las variables edad_del_modelo y km_por_anio en este conjunto de datos es la regresión polinómica 
de grado 2."
#####





poly_model <- lm(price ~ poly(year, 2) + poly(mileage, 2), data = carros_usados_limpio)

predict_car <- function(year, mileage) {
  prediction <- predict(poly_model, newdata = data.frame(year = year, mileage = mileage))
  return(prediction)
}
predict_car

# Generar gráficas para las variables con mayor correlación con 'price'
# Reemplaza 'variable_con_fuerte_correlacion' con el nombre de la variable identificada
# Puedes repetir este bloque para las principales variables correlacionadas con 'price'
ggplot(data = carros_usados_limpio, aes(x = mileage, y = price)) +
  geom_point(aes(color = factor(accidents_or_damage))) + # Ejemplo de cómo añadir una tercera variable
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relación entre Precio y Mileagen",
       x = "Mileage",
       y = "Precio") +
  theme_minimal()

ggplot(data = carros_usados_limpio, aes(x = mileage, y = price)) +
  geom_point(aes(color = factor(accidents_or_damage))) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relación entre Precio y Kilometraje",
       x = "Kilometraje",
       y = "Precio") +
  theme_minimal() +
  coord_cartesian(ylim = c(500, 100000)) # Ajusta la vista del eje Y sin eliminar datos


# Gráfica de dispersión para Precio vs Kilometraje
ggplot(carros_usados_limpio, aes(x = mileage, y = price)) +
  #geom_point(aes(color = exterior_color), alpha = 0.5) + # Añadimos color para diferenciar por color exterior
  geom_smooth(method = "lm", color = "blue", se = FALSE) + # Línea de tendencia
  labs(title = "Precio vs Kilometraje",
       x = "Kilometraje",
       y = "Precio") +
  theme_minimal()

# Gráfica de dispersión para Precio vs Año del Modelo
ggplot(carros_usados_limpio, aes(x = year, y = price)) +
  geom_point(aes(color = fuel_type), alpha = 0.5) + # Diferenciamos por tipo de combustible
  geom_smooth(method = "lm", color = "red", se = FALSE) + # Línea de tendencia
  labs(title = "Precio vs Año del Modelo",
       x = "Año del Modelo",
       y = "Precio") +
  theme_minimal()