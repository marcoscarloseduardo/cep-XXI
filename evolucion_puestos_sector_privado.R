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
datos <- descarga_DA(tipo = "Puestos depto", universo = "Privado", sector = "Letra", jurisdiccion = "Departamento vivienda", genero = "NO") 

# Añadir sector 
datos <- diccionario_sectores(datos, # Nombre de la base a la que se quiere añadir la información
                              agregacion = "Letra", # Nivel de agregación que se quiere sumar datos, opciones: "CLAE6","CLAE3","CLAE2","letra" 
                              descripciones = T) # T si se quiere sumar la descripción / F si no

# Añadir departamento 
datos <- dicc_depto_prov(datos) # Nombre de la base a la que se quiere añadir la información
head(datos)


# 3. Data wrangling
# ----------------------------------------

# departamento objetivo
depto_obj <- "Trenque Lauquen"
depto_provincia <- "Buenos Aires"

datos_obj <- datos |> filter(nombre_departamento_indec == depto_obj, nombre_provincia_indec == depto_provincia) |> 
  rename(sector = letra_desc) |> 
  mutate(anio = year(fecha),
         mes = month(fecha))

datos_obj |> head(10)

# corregimos sector NA como desconocido en vez de NA
datos_obj <- datos_obj |> mutate(sector = ifelse(is.na(sector), "DESCONOCIDO", sector)) # sector desconocido reemplazamos por leyenda

# corregimos puestos desconocidos como NA en vez de -99
summary(datos_obj$puestos)
datos_obj <- datos_obj |> mutate(puestos = ifelse(puestos == -99, NA, puestos)) # puestos desconocidos reemplazamos por NA

# tomamos idea de cómo se distribuyen los puestos entre sectores en la actividad privada
hist(datos_obj$puestos)

# mes del último dato anual
datos_obj |> 
  group_by(anio) |> 
  summarise(mes_max = max(mes))

tabla <- datos_obj |> 
  filter(mes == 12 | (anio == 2023 & mes == 7)) |> 
  select(anio, sector, puestos) |> 
  mutate(sector = str_to_title(sector))

# principales sectores por puesto
tabla |> 
  group_by(sector) |> 
  summarise(promedio = mean(puestos), .groups = "drop") |> 
  arrange(-promedio)


# acortar descripcion sector
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
  summarise(promedio = mean(puestos), .groups = "drop") |> 
  arrange(-promedio) |>
  select(sector, promedio) |> 
  head(10)

sector_puestos_actual <- tabla |> filter(anio == 2023) |> arrange(-puestos) |> select(sector, puestos) |> head(10)

cbind(promedio = sector_puestos_promedio, actual = sector_puestos_actual)
# en el 2023 surge alojamiento superando a actividades adminsitrativas.

sectores_top_5 <- tabla |> filter(anio == 2023) |> arrange(-puestos) |> select(sector) |> head(5) |> pull(sector)

sectores_top_6_11 <- tabla |> filter(anio == 2023) |> arrange(-puestos) |> select(sector) |> slice(6:11) |> pull(sector)

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

# Top 5
# tabla filtrada por sectores que más contribuyeron
tabla_plot <- tabla |> filter(sector %in% sectores_top_5)

tabla_plot <- tabla_plot |> 
  mutate(color = case_when(sector == "Comercio y Reparación Vehículos" ~ ORANGE1, T ~ GRAY9))

# valores máximos y mínimos para posicionar texto
anio_min = min(tabla_plot$anio)
anio_max = max(tabla_plot$anio)
puestos_min = min(tabla_plot$puestos)
puestos_max = max(tabla_plot$puestos)

# theme Story Telling with Data (SWD) - best practices
theme_set(theme_minimal() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  axis.title = element_blank(),
                                  axis.line.x = element_line(color = GRAY9),
                                  axis.ticks.x = element_line(color = GRAY9),
                                  axis.text.x = element_text(size = 12, GRAY9),
                                  axis.line.y = element_blank(),
                                  axis.text.y = element_blank(),
                                  plot.margin = unit(c(2,1,4,7.5), "cm"),
                                  plot.subtitle = element_text(color = GRAY4),
                                  plot.title = element_text(hjust = -3),
                                  plot.caption = element_text(size = 8, color = GRAY7, hjust = 0, margin = margin(15,0,0,0, "pt"))
))

# leyendas informativas
grob_title <- grobTree(textGrob("Top 5 Sectores privados con Mayor Empleo en la Última Década", x = -0.7,  y = 1.12, hjust = 0, gp = gpar( fontsize = 16)))
grob_subtitle <- grobTree(textGrob("Distrito de Trenque Lauquen, Prov. Buenos Aires, Argentina", x = -0.7,  y = 1.05, hjust = 0, gp = gpar(col = GRAY6, fontsize = 14)))
grob_caption <- grobTree(
  textGrob("Nota: Evolución de enero 2014 a septiembre 2023.\nFuente: Elaboración propia en base a Datos Abiertos del CEP XXI.\n©2023 Carlos Marcos (https://github.com/marcoscarloseduardo)",
           x = -0.7, y = -0.3, hjust = 0, gp = gpar(col = GRAY6, fontsize = 11))
)

fig <- ggplot(tabla_plot, aes(x = anio, y = puestos, group = sector, color = color )) + 
  geom_line(linewidth = 3) +
  
  # puntos en año inicial y final
  geom_point(data = tabla_plot %>% filter(anio == anio_min | anio == anio_max), size = 5) + 
  
  # sector en año inicial
  geom_text(data = tabla_plot %>% filter(anio == anio_min), aes(label = sector), vjust = 0.5, nudge_y = 10, hjust = 1, nudge_x = -1.8, size = 5) + 
  
  # valores en año inicial
  geom_text(data = tabla_plot %>% filter(anio == anio_min), aes(label = paste0("bold(",round(puestos),")")), nudge_x = -1, parse = T, size = 5) +
  
  # valores en año final
  geom_text(data = tabla_plot %>% filter(anio == anio_max), aes(label = paste0("bold(",round(puestos),")")), nudge_x = 1, parse = T, size = 5) +
  
  # evitamos leyendas adicionales
  scale_color_identity() +
  
  # flexibilizar ubicación de información en el gráfico
  coord_cartesian(clip = "off") + 
  
  ylim(c(0, puestos_max)) + 
  
  # geom_blank para modificar visualización del eje x
  geom_blank(data = data.frame(anio = c(2014, 2023), puestos = rep(puestos_max, 2), sector = rep("", 2), color = "black")) +
  
  # marcas de los años intermedios y leyendas en los extremos
  scale_x_continuous(breaks = c(2014:2023), labels = c(2014, sector = rep("", 8), 2023)) +
  
  annotation_custom(grob_title) + 
  annotation_custom(grob_subtitle) + 
  annotation_custom(grob_caption)

ggsave(
  filename = "top_puestos_x_sector.jpg",
  width = 7.5, height = 6, dpi = 1200,
  device = "jpg", fig
)


# Top 6-11
# tabla filtrada por sectores que más contribuyeron
tabla_plot <- tabla |> filter(sector %in% sectores_top_6_11)

tabla_plot <- tabla_plot |> 
  mutate(color = case_when(
    sector == "Serv. de Alojamiento y de Comida" ~ GREEN1,
    sector == "Salud Humana y Serv. Sociales" ~ GREEN3,
    sector == "Serv. Prof., Científ. y Técnicos" ~ GREEN4,
    sector == "Serv. Financieros y de Seguros" ~ RED4,
    sector == "Actividades Administrativas y de Apoyo" ~ RED3,
    T ~ GRAY9))

# valores máximos y mínimos para posicionar texto
anio_min = min(tabla_plot$anio)
anio_max = max(tabla_plot$anio)
puestos_min = min(tabla_plot$puestos)
puestos_max = max(tabla_plot$puestos)

# theme Story Telling with Data (SWD) - best practices
theme_set(theme_minimal() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  axis.title = element_blank(),
                                  axis.line.x = element_line(color = GRAY9),
                                  axis.ticks.x = element_line(color = GRAY9),
                                  axis.text.x = element_text(size = 12, GRAY9),
                                  axis.line.y = element_blank(),
                                  axis.text.y = element_blank(),
                                  plot.margin = unit(c(2,1,4,7.5), "cm"),
                                  plot.subtitle = element_text(color = GRAY4),
                                  plot.title = element_text(hjust = -3),
                                  plot.caption = element_text(size = 8, color = GRAY7, hjust = 0, margin = margin(15,0,0,0, "pt"))
))

# leyendas informativas
grob_title <- grobTree(textGrob("Top 6-11 Sectores privados con Mayor Empleo en la Última Década", x = -0.7,  y = 1.12, hjust = 0, gp = gpar( fontsize = 16)))
grob_subtitle <- grobTree(textGrob("Distrito de Trenque Lauquen, Prov. Buenos Aires, Argentina", x = -0.7,  y = 1.07, hjust = 0, gp = gpar(col = GRAY6, fontsize = 14)))
grob_caption <- grobTree(
  textGrob("Nota: Evolución de enero 2014 a septiembre 2023.\nFuente: Elaboración propia en base a Datos Abiertos del CEP XXI.\n©2023 Carlos Marcos (https://github.com/marcoscarloseduardo)",
           x = -0.7, y = 0, hjust = 0, gp = gpar(col = GRAY6, fontsize = 9))
)

fig <- ggplot(tabla_plot, aes(x = anio, y = puestos, group = sector, color = color )) + 
  geom_smooth(method = "loess", se = FALSE, size = 2, span = 0.3) +
  
  # puntos en año inicial y final
  geom_point(data = tabla_plot %>% filter(anio == anio_min | anio == anio_max), size = 3) + 
  
  # sector en año inicial
  geom_text(data = tabla_plot %>% filter(anio == anio_min), aes(label = sector), vjust = 0.5, nudge_y = 0, hjust = 1, nudge_x = -1.8, size = 4) + 
  
  # valores en año inicial
  geom_text(data = tabla_plot %>% filter(anio == anio_min), aes(label = paste0("bold(",round(puestos),")")), nudge_x = -1, parse = T, size = 4) +
  
  # valores en año final
  geom_text(data = tabla_plot %>% filter(anio == anio_max), aes(label = paste0("bold(",round(puestos),")")), nudge_x = 1, parse = T, size = 4) +
  
  # evitamos leyendas adicionales
  scale_color_identity() +
  
  # flexibilizar ubicación de información en el gráfico
  coord_cartesian(clip = "off") +
  
  ylim(c(puestos_min, puestos_max)) + 
  
  # geom_blank para modificar visualización del eje x
  geom_blank(data = data.frame(anio = c(2014, 2023), puestos = rep(puestos_max, 2), sector = rep("", 2), color = "black")) +
  
  # marcas de los años intermedios y leyendas en los extremos
  scale_x_continuous(breaks = c(2014:2023), labels = c(2014, rep("", 8), 2023), position = "top") +
  
  annotation_custom(grob_title) + 
  annotation_custom(grob_subtitle) + 
  annotation_custom(grob_caption)

ggsave(
  filename = "top_6_11_puestos_x_sector.jpg",
  width = 7.5, height = 9, dpi = 1200,
  device = "jpg", fig
)

