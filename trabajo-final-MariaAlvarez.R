# Voy a cargar las librerias que quiero utilizar
library(tidyr)
library(ggplot2)
library(openxlsx)
library(dplyr)


#Cargamos los datos en formato RData desde el repositorio de Github

load(url("https://github.com/malvarezdelvayo/cursoR/raw/main/EPSH_2022.RData"))


# Voy a explorar los datos

datostotales <- as_tibble(Microdatos)
class(datostotales)
summary(datostotales)
nrow(datostotales)
datostotales$NIDENT <- as.numeric(datostotales$NIDENT)
length(datostotales$NIDENT)
datostotales

#Edad media de los encuestados

edad <- datostotales$Edad
mean(edad)
quantile(edad)
min(edad)
max(edad)

#Sexo de los encuestados

datostotales <- datostotales %>%
  mutate(B1 = case_when(
  B1 == 1 ~ "hombre",
  B1 == 6 ~ "mujer",
  TRUE ~ as.character(B1)
  ))

porcentaje_hombres <- prop.table(tabla_sexos)["hombre"] * 100
print(porcentaje_hombres)

#Vamos a ver la edad media de los encuestados por sexo

edadmedia <- datostotales %>% 
  select(Edad, B1) %>%
  group_by(B1) %>%
  summarise(avg = mean(Edad))

print(edadmedia)

#Cuantos encuestados por CCAA

nombresCCAA <- read.csv(url("https://raw.githubusercontent.com/malvarezdelvayo/cursoR/main/tccaa.csv"))

#Pasamos las columnas por las que queremos unir las tablas al mismo formato:

datostotales$CCAA <- as.numeric(datostotales$CCAA)
nombresCCAA$codigo <- as.numeric(nombresCCAA$codigo)

#Renombramos las columnas para que tengan el mismo nombre y el left joint sea fácil

datostotales <- datostotales %>% 
  rename(codCCAA = CCAA)
nombresCCAA <- nombresCCAA %>%
  rename(codCCAA = codigo)

#Hacemos un left joint para que aparezcan los nombres de las CCAA equivalentes a cada código en la bbdd
datostotales <- left_join(datostotales, nombresCCAA, by = 'codCCAA') 

#Lo mostramos en un gráfico
encuestadosCCAA <- data.frame(table(datostotales$CCAA))

ggplot(data = encuestadosCCAA, aes(x = reorder(Var1, Freq), y = Freq)) + 
  geom_bar(stat = "identity", fill ='#AF1964') +
  labs(y = "CCAA", x = "Nº de encuestados") +
  coord_flip() + # Invertir ejes
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) + # Rotar texto del eje y
  theme_minimal()
                                                  

# Vamos a ver la nacionalidad de los encuestados. Al ser tres variables, las metemos a mano:
datostotales <- datostotales %>%
  mutate(B3 = case_when(
    B3 == 1 ~ "española",
    B3 == 2 ~ "extranjera",
    B3 == 3 ~ "ambas",
    TRUE ~ as.character(B3)
  ))

resultadosNAC <- table(datostotales$B3)
porcentajeextranjeros <- (resultadosNAC["extranjera"]/sum(resultadosNAC)) * 100
porcentajeespañoles <- (resultadosNAC["española"]/sum(resultadosNAC)) * 100
porcentajeambos <- (resultadosNAC["ambas"]/sum(resultadosNAC)) * 100
porcentajesNAC <- c(print(porcentajeextranjeros), print(porcentajeespañoles), print(porcentajeambos))


#Vamos a ver la correspondencia entre edad, nacionalidad y sexo

# Calcular la edad media para hombres y mujeres dentro de cada nacionalidad
edad_media_por_nacionalidad <- datostotales %>%
  group_by(B3, B1) %>%
  summarise(edad_media = mean(Edad, na.rm = TRUE))

print(edad_media_por_nacionalidad)

# Vamos a ver la región del mundo del que viene el encuestado extranjero

datostotales <- datostotales %>%
  mutate(B3_LIT_COD = case_when(
    B3_LIT_COD == 1 ~ "Unión Europea",
    B3_LIT_COD == 2 ~ "Resto de Europa",
    B3_LIT_COD == 3 ~ "África",
    B3_LIT_COD == 4 ~ "América",
    B3_LIT_COD == 5 ~ "Asia",
    B3_LIT_COD == 6 ~ "Oceanía",
    B3_LIT_COD == 9 ~ "NS/NR",
    TRUE ~ as.character(B3_LIT_COD)
  ))


# Filtrar los datos por B3 igual a "extranjera"
regionEXT <- datostotales %>%
  filter(B3 == "extranjera") %>%
  select(B3_LIT_COD)

# Crear una tabla de frecuencias para la variable B3_LIT_COD
tabla_region <- table(regionEXT$B3_LIT_COD)

# Convertir la tabla en un data frame
df_tabla_region <- as.data.frame(tabla_region)

# Renombrar las columnas del data frame
names(df_tabla_region) <- c("Region", "Frecuencia")

# Crear el gráfico de barras
ggplot(df_tabla_region, aes(x = "", y = Frecuencia, fill = Region)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#541030", "#D1B3C1", "#39222D", "#A9386E","#CB2574")) +
  theme_void()



# Vamos a hacer un loop para que cambie todas las variables de las columnas H1 - H3_17 de códigos a "Sí"/ "No"


columnas_a_transformar <- c(
  paste0("H1_", 1:3),
  paste0("H2_", 4:12),
  paste0("H3_", 13:17)
)

for (col in columnas_a_transformar) {
  datostotales <- datostotales %>%
    mutate(!!sym(col) := case_when(
      .data[[col]] == 1 ~ "Sí",
      .data[[col]] == 6 ~ "No",
      .data[[col]] == 9 ~ "NS/NR",
      TRUE ~ as.character(.data[[col]])
    ))
}



#Vamos a ver cuánto dinero ingresos han recibido en total


mean(datostotales$H5)
quantile(datostotales$H5)
min(datostotales$H5)
max(datostotales$H5)


# Visualización ingresos:

rangos <- seq(0, 10000, by = 500)
etiquetas <- paste0(rangos[-length(rangos)], "-", rangos[-1])

datostotalesrangos <- cut(datostotales$H5, breaks = rangos, labels = etiquetas, include.lowest = TRUE)

datostotalesrangos <- as.data.frame(datostotalesrangos)
datostotalesrangos <- cbind(datostotalesrangos, H5 = datostotales$H5)
datostotalesrangos <- datostotalesrangos %>%
  rename(rangos = datostotalesrangos)

datostotalesrangos <- datostotalesrangos %>%
  group_by(rangos) %>%
  summarise(Cantidad = n()) %>%
  arrange(desc(Cantidad))

ggplot(datostotalesrangos, aes(x = rangos, y = Cantidad)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Rango de Ingresos", y = "Cantidad") +
  theme_minimal()
  


