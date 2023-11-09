# 1. Load library
# ----------------------------------------

#install.packages('devtools') 
#devtools::install_github("CEProduccionXXI/DatosAbiertosCEP")

library(DatosAbiertosCEP)
library(dplyr)
library(lubridate)
library(ggplot2)
library(grid)
library(stringr)
library(extrafont)
library(tidyr)
library(treemap)

#font_import()
loadfonts(device = "win")


# 2. Load data
# ----------------------------------------

# visualizar tablas existentes
View(DA_cruces_existentes())

# Descripción de los cinco parámetros: 
# tipo: refiere al dato que se quiere descargar; 
# genero: indica si se quiere la base desagregada por sexo biológico o no;
# sector: indica el nivel de desagregación de actividad económica; 
# jurisdiccion: indica los diferentes niveles de desagregación geográfica que tiene el dato;
# universo: refiere al universo de empleadores con el que se quiere trabajar. Se puede escoger por empresas privadas, 
#           empresas públicas y privadas, empleadores públicos, total del empleo y NO (para los casos en los que no se indica el universo)

# Cargar datos
# La función cuenta con el parámetro show_info_DA para facilitar el acceso a la metodología de la base. Este parámetro por default es verdadero, 
# pero eventualmente puede desactivarse. 
datos_priv <- descarga_DA(tipo = "Puestos depto", universo = "Privado", sector = "Letra", jurisdiccion = "Departamento vivienda", genero = "NO") 
datos_total <- descarga_DA(tipo = "Puestos depto", universo = "Total empleo", sector = "Letra", jurisdiccion = "Departamento vivienda", genero = "NO") 

# Añadir sector 
datos_priv <- diccionario_sectores(datos_priv, # Nombre de la base a la que se quiere añadir la información
                              agregacion = "Letra", # Nivel de agregación que se quiere sumar datos, opciones: "CLAE6","CLAE3","CLAE2","letra" 
                              descripciones = T) # T si se quiere sumar la descripción / F si no

datos_total <- diccionario_sectores(datos_total, # Nombre de la base a la que se quiere añadir la información
                                   agregacion = "Letra", # Nivel de agregación que se quiere sumar datos, opciones: "CLAE6","CLAE3","CLAE2","letra" 
                                   descripciones = T) # T si se quiere sumar la descripción / F si no


# Añadir departamento 
datos_priv <- dicc_depto_prov(datos_priv) # Nombre de la base a la que se quiere añadir la información
datos_total <- dicc_depto_prov(datos_total) # Nombre de la base a la que se quiere añadir la información


# 3. Data wrangling
# ----------------------------------------

# departamento objetivo
depto_obj <- "Trenque Lauquen"
depto_provincia <- "Buenos Aires"

datos_priv_obj <- datos_priv |> filter(nombre_departamento_indec == depto_obj, nombre_provincia_indec == depto_provincia) |> 
  rename(sector = letra_desc) |> 
  mutate(anio = year(fecha),
         mes = month(fecha))

datos_total_obj <- datos_total |> filter(nombre_departamento_indec == depto_obj, nombre_provincia_indec == depto_provincia) |> 
  rename(sector = letra_desc) |> 
  mutate(anio = year(fecha),
         mes = month(fecha))

# corregimos sector NA como desconocido en vez de NA
datos_priv_obj <- datos_priv_obj |> mutate(sector = ifelse(is.na(sector), "DESCONOCIDO", sector)) # sector desconocido reemplazamos por leyenda
datos_total_obj <- datos_total_obj |> mutate(sector = ifelse(is.na(sector), "DESCONOCIDO", sector)) # sector desconocido reemplazamos por leyenda

# corregimos puestos desconocidos como NA en vez de -99
datos_priv_obj <- datos_priv_obj |> mutate(puestos = ifelse(puestos == -99, NA, puestos))  |> select(anio, mes, sector, puestos)
datos_total_obj <- datos_total_obj |> mutate(puestos = ifelse(puestos == -99, NA, puestos))  |> select(anio, mes, sector, puestos)

datos_obj <- left_join(datos_total_obj, datos_priv_obj, by = c("anio", "mes", "sector"))

datos_obj <- datos_obj |> rename(puestos_total = puestos.x, puestos_priv = puestos.y)
datos_obj <- datos_obj |> mutate(puestos_priv = ifelse(sector == "ADMINISTRACION PUBLICA, DEFENSA Y SEGURIDAD SOCIAL OBLIGATORIA", 0, puestos_priv)) |> 
  filter(sector != "DESCONOCIDO") |> 
  mutate(puestos_pub = puestos_total - puestos_priv)

tabla <- datos_obj |> 
  filter(mes == 12 | (anio == 2023 & mes == 7)) |> 
  select(anio, sector, puestos_total, puestos_priv, puestos_pub) |> 
  mutate(sector = str_to_title(sector))

# principales sectores por puesto
tabla |> 
  group_by(sector) |> 
  summarise(promedio = mean(puestos_total), .groups = "drop") |> 
  arrange(-promedio)

# acortar descripcion sector
tabla <- tabla |> 
  mutate(sector = ifelse(sector == "Administracion Publica, Defensa Y Seguridad Social Obligatoria", 
                         "Administración Pública", sector))

tabla <- tabla |> 
  mutate(sector = ifelse(sector == "Comercio Al Por Mayor Y Al Por Menor; Reparación De Vehículos Automotores Y Motocicletas", 
                         "Comercio y Reparación Vehículos", sector))

tabla <- tabla |> 
  mutate(sector = ifelse(sector == "Agricultura, Ganadería, Caza, Silvicultura Y Pesca", 
                         "Agricultura, Ganadería y Pesca", sector))

tabla <- tabla |> 
  mutate(sector = ifelse(sector == "Servicio De Transporte Y Almacenamiento", 
                         "Transporte y Almacenamiento", sector))

tabla <- tabla |> 
  mutate(sector = ifelse(sector == "Servicios De Alojamiento Y Servicios De Comida", 
                         "Serv. de Alojamiento y de Comida", sector))

tabla <- tabla |> 
  mutate(sector = ifelse(sector == "Actividades Administrativas Y Servicios De Apoyo", 
                         "Actividades Administrativas y de Apoyo", sector))

tabla <- tabla |> 
  mutate(sector = ifelse(sector == "Servicios Profesionales, Científicos Y Técnicos", 
                         "Serv. Prof., Científ. y Técnicos", sector))

tabla <- tabla |> 
  mutate(sector = ifelse(sector == "Intermediación Financiera Y Servicios De Seguros", 
                         "Serv. Financieros y de Seguros", sector))

tabla <- tabla |> 
  mutate(sector = ifelse(sector == "Construcción", 
                         "Construcción\n ", sector))

tabla <- tabla |> 
  mutate(sector = ifelse(sector == "Salud Humana Y Servicios Sociales", 
                         "Salud Humana y Serv. Sociales", sector))

# comparamos si hay sorpresas en la evolución de los que más contribuyen
sector_puestos_promedio <- tabla |> 
  group_by(sector) |> 
  summarise(promedio = mean(puestos_total), .groups = "drop") |> 
  arrange(-promedio) |>
  select(sector, promedio) |> 
  head(10)

sector_puestos_actual <- tabla |> filter(anio == 2023) |> arrange(-puestos_total) |> select(sector, puestos_total, puestos_priv, puestos_pub) |> head(11)

sectores_top_1_11 <- tabla |> filter(anio == 2023) |> arrange(-puestos_total) |> select(sector) |> slice(1:11) |> pull(sector)

# 4. Plot
# ----------------------------------------

# gama de colores a utilizar
GRAY1   <- "#231F20"
GRAY2   <- "#414040"
GRAY3   <- "#555655"
GRAY4   <- "#646369"
GRAY5   <- "#76787B"
GRAY6   <- "#828282"
GRAY7   <- "#929497"
GRAY8   <- "#A6A6A5"
GRAY9   <- "#BFBEBE"
BLUE1   <- "#174A7E"
BLUE2   <- "#4A81BF"
BLUE3   <- "#94B2D7"
BLUE4   <- "#94AFC5"
BLUE5   <- "#22435e"
BLUE6   <- "#95B3D7"
BLUE7   <- "steelblue"
RED1    <- "#C3514E"
RED2    <- "#E6BAB7"
RED3    <- "#800000"
RED4    <- "firebrick"
GREEN1  <- "#0C8040"
GREEN2  <- "#9ABB59"
GREEN3  <- "#31859C"
GREEN4  <- "#4BACC6"
GREEN5  <- "#93CDDD"
ORANGE1 <- "#F79747"
ORANGE2 <- "#FAC090"


# tabla filtrada por sectores que más contribuyeron
tabla_plot <- tabla |> filter(sector %in% sectores_top_1_11, anio == 2023) |> select(sector, puestos_priv, puestos_pub)

tabla_plot <- tabla_plot %>%
  pivot_longer(
    cols = starts_with("puestos_"),
    names_to = "tipo",
    values_to = "puestos"
  )

# Reemplazar "priv" y "pub" en la columna "tipo" por los valores deseados
tabla_plot$tipo <- ifelse(tabla_plot$tipo == "puestos_priv", "priv", "pub")

png("proporcion_puestos_x_sector_priv_pub.png", width = 800, height = 800, units = "px")

treemap(dtf = tabla_plot, index = c("sector","tipo"), vSize = "puestos", type = "index",
        title = "Proporción de puestos para los primeros 11 sectores públicos y privados",
        fontsize.title = 18,
        fontsize.labels = c(16,14),          # tamaño de los labels en orden: grupo, subgrupo, etc.
        fontcolor.labels = c(GRAY2, "white"),# color de los labels
        fontface.labels = c(2, 1),           # font de los labels: 1,2,3,4 para normal, bold, italic, bold-italic
        bg.labels = c("transparent"),        # color de fondo de los labels
        align.labels = list(
          c("center", "top"), 
          c("right", "bottom")
          ),
        overlap.labels = 0.5,         # número entre 0 y 1 que determina la tolerancia de solapamiento entre labels.
                                      #   0 implica que label pequeños no se visualizan si uno más grande se solapa sobre él
                                      #   1 implica que siempre los labels se visualizan
                                      #   valores intermedios implican que los labels menores se visualizan si no hay un solapamiento superior a ese valor intermedio
        inflate.labels = FALSE,       # si es TRUE, los labels son mayores cuando mayor es el rectángulo.
        aspRatio = 1
        )

dev.off()