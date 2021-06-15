################################################################################
# Archivo: extraccionCoberturasDescendSIOSE14.R
# Autor: Juan Carlos VelAzquez Melero 
# Fecha: 30/03/2021 - 17/04/2021
# DescripciOn: 
# ** Problema **:
# La tabla t_valores de SIOSE contiene tanto coberturas "parentales" como
# "descendientes" con sus porcentajes de ocupaciOn correspondientes (campo SUPERF_POR).
# Para que la suma de los porcentajes de ocupaciOn de todas las coberturas de un 
# polIgono sea 100 hay que descartar los porcentajes de ocupaciOn de los parentales
# Ej.: 100A(60PST_40ZEV) --> El registro 100A hay que descartarlo y contar solo
# con 60PST y 40ZEV. Si se sumasen todos los % de ocupaciOn saldría 200%.
# ** ¿QuE hace este script? **:
# En la tabla t_valores, marca los registros "descencientes" como vAlidos (V) y 
# los "parentales" como No VAlidos (NV). De tal forma que si se suman los porcentajes
# de ocupaciOn de los registros VAlidos de cada polIgono, suman el 100%
# 
# ** Entradas: Geodatabase de SIOSE (SIOSE_Madrid_2014.gdb)
# ** Salidas: 
#   - tabla t_valores modificada ("t_valores_coberturasClasificadas.csv")
#   - coberturas descendientes sin duplicados ("coberturasDescendientesUnicas.csv")
# NOTA: (1) Este script solo ha sido testeado con la GDB de SIOSE 2014 de Madrid
################################################################################

rm(list = ls(all = TRUE)) # Eliminar objetos previos, si los hubiera
# InstalaciOn de paquetes tomada de Antoine Soetewey (https://statsandr.com)
packages <- c("sf", "dplyr")
# IntalaciOn de paquetes
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Carga de paquetes
invisible(lapply(packages, library, character.only = TRUE))

################################################################################
##################____CARPETAS DE ENTRADA Y SALIDA_____#########################
################################################################################
# Introduce aquI la carpeta donde estA la contenida la GDB con SIOSE 2014 Madrid
setwd("D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/0_datos_brutos_entrada/siose2014/SIOSE_Madrid_2014_GDB")

# Introduce aquI un directorio en el que guardar las tablas de salida
exportPath <- "D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/1_procesado_datos/0_extraccionCoberturasDescendSIOSE14"
# Nombre de la carpeta con las exportaciones, (dejarlo asI por defecto)
newFolderName <- c("exportacionesCoberturasDescendSIOSE14") # Nombre de la carpeta con las exportaciones
# Path completo para crear la nueva carpeta con las exportaciones
exportPathComplete <- file.path(exportPath, newFolderName)
# Crear carpeta en el exportPathComplete, donde se guardarAn las tablas
dir.create(exportPathComplete)

################################################################################
################____PROCESADO DE LA TABLA T_VALORES_____########################
################################################################################
# Abrir tabla de datos "T_VALORES" 
t_valores <- st_read(dsn = "SIOSE_Madrid_2014.gdb", layer = "T_VALORES")

# Abrir tabla de datos "TC_SIOSE_COBERTURAS" 
coberturas <- st_read(dsn = "SIOSE_Madrid_2014.gdb", layer = "TC_SIOSE_COBERTURAS")

# Unir con las coberturas de la tabla t_valores y la tabla TC_SIOSE_COBERTURAS
t_valores <- merge(t_valores, coberturas, by = "ID_COBERTURAS", all.x = T)
rm(coberturas)

#  ANadir un campo que concatene todos los INTER_ANCESTROS de cada polIgono (INTER_ANCESTROS_concatenado)
t_valores <- t_valores %>% 
  group_by(ID_POLYGON) %>% 
  mutate(INTER_ANCESTROS_concatenado = paste0(INTER_ANCESTROS, collapse = ","))

# ANadir un campo que comprueba si INTER_ID estA en INTER_ANCESTROS_concatenado,
# Siendo TRUE si estA contenido y FALSE si no lo estA

INTER_ANCESTROS_DESconcatenado <- c() # Se crea vector vacio
t_valores$INTER_ID_tipo <- "" # se crea el campo vacio
for (row in 1:nrow(t_valores)){
  # Se desconcatena el campo para pasarlo a vector numErico. Este proceso es mucho 
  # menos eficiente que chechear directamente en el campo como si fueran cadenas. 
  # Sin embargo, si se hace asI se generan falsos TRUES. Por ejemplo: si "3" %in% "13" sale TRUE por ser cadena
  # Por eso se hace este bucle
  INTER_ANCESTROS_DESconcatenado <- strsplit(t_valores$INTER_ANCESTROS_concatenado[row],
                                             ",")
  INTER_ANCESTROS_DESconcatenado <- as.numeric(unlist(INTER_ANCESTROS_DESconcatenado))
  
  # Se comprueba si el entero de INTER_ID estA en el vector numErico desconcatenado y se asigna T/F 
  t_valores$INTER_ID_tipo[row] <- ifelse(t_valores$INTER_ID[row] %in% INTER_ANCESTROS_DESconcatenado,
                                      "parental", "descendiente")
}

# Ordenar el df final 
ordenColumnas <- c("ID_POLYGON", "ID_COBERTURAS", "DESCRIPCION_COBERTURAS", 
                   "CODE_ABREVIADO", "SUPERF_POR", "INTER_ANCESTROS",
                   "ID_ANCESTROS", "INTER_ID", "INTER_ANCESTROS_concatenado",
                   "INTER_ID_tipo")

t_valores <- t_valores[,ordenColumnas]
t_valores <- t_valores[with(t_valores, order(ID_POLYGON, INTER_ID)), ]

# Seleccionar Unicamente las coberturas descendientes sin duplicados
t_valores_descend <- t_valores[t_valores$INTER_ID_tipo == "descendiente", ]
t_valores_descend <- t_valores_descend[, c("ID_COBERTURAS", "DESCRIPCION_COBERTURAS",
                                           "CODE_ABREVIADO")]
t_valores_descend_unicos <- unique(t_valores_descend)
rm(t_valores_descend)

################################################################################
###################____EXPORTACIoN DE LAS TABLAS____############################
################################################################################

# ExportaciOn de la tabla t_valores modificada
path_t_valores_cobClasif <- file.path(exportPathComplete, "t_valores_coberturasClasificadas.csv")
write.table(t_valores, path_t_valores_cobClasif, row.names = F, sep = ",")

# Exportacion de las coberturas descendientes sin duplicados
pathCoberturasDescendUnicas <- file.path(exportPathComplete, "coberturasDescendientesUnicas.csv")
write.table(t_valores_descend_unicos, pathCoberturasDescendUnicas, row.names = F, sep = ",")
