packages <- c("readxl","RStata","reticulate", "shiny", "bslib", "ggthemes", "RColorBrewer", "sf", "shinythemes", "lubridate", "jsonlite",
              "stringr", "readr", "dplyr", "tidyverse", "shinyjs", "plotly", "ggplot2", "DT", "shinyWidgets",
              "shinydashboard", "shinycssloaders", "cowplot", "ggmap", "ggspatial", "rmarkdown", "rgdal", "RStata",
              "fontawesome", "haven", "readxl", "gridExtra", "scales", "writexl", "openxlsx", "kableExtra", "rlang", "formattable"
)


install_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]

if (length(install_packages) > 0) {
  install.packages(install_packages)
}


# Load packages

library(shiny)
library(rlang)
library(RStata)
library(bslib)
library(ggthemes)
library(RColorBrewer)
library(sf)
library(shinythemes)
library(lubridate)
library(jsonlite)
library(stringr)
library(readr)
library(dplyr)
library(tidyverse)
library(shinyjs)
library(plotly)
library(ggplot2)
library(DT)
library(shinyWidgets)
library(shinydashboard)
library(bslib) 
library(shinycssloaders)
library(ggmap)
library(ggspatial)
library(rmarkdown) 
# library(rgdal)
library(fontawesome)
library(haven)
library(readxl)
library(scales)
library(writexl)
library(openxlsx)
library(gridExtra)
library(cowplot)
library(kableExtra)
library(jsonlite)


##importr dados financeiros 
dados_ficticios <- read_excel("dados_ficticios_com_lucro.xlsx")

#importar dados de Pegada de carbono
dados_pegadas <- read_excel("Pegada de Carbono Report.xlsx")

 

### Alterar nome das variaveis
dados_pegadas <- dados_pegadas %>% rename(
  compra_fornecedor_local= `Compras os produtos para seu negócio de fornecedor local?`,
  Quantas_vezes = `Quantas vezes na semana ou no mês compras os produtos para desempenhar vosso negócio?`,
  Quando_compras = `Quando compras os produtos para desempenhar teu negócio, priorizas aqueles que tenham menor pegada de carbono?`,
  Quando_compras_as_embalagens =  `Na Aquisição, quando compras as embalagens para entregar teus produtos, priorizas aqueles que tenham menor pegada de carbono?`,
  Quando_vais_a_comprar = `Quando vais a comprar os produtos para desempenhar teu negócio, utilizas meios de deslocação de baixa emissão de carbono?`,
  levas_embora_embalados = `Quando compras os produtos para desempenhar teu negócio, os levas embora embalados em plásticos?`,
  entregar_produto = `Quando vais entregar teus produtos/serviços, utilizas meios de deslocação de baixa emissão de carbono?`,
  Quando_entrega_compras_as_embalagens = `Quando compras as embalagens para entregar teus produtos, priorizas aqueles que tenham menor pegada de carbono?`,
  separar_lixoOrganico = `Separas o lixo orgânico do não orgânico?`,
  ulizarlixoorganico = `Utilizas o lixo orgânico do teu negócio para compostagem?`,
  vender_lixo_compostagem = `Vendes o lixo orgânico do teu negócio para compostagem?`,
  reutilizar_lixoOrganico = `Reutilizas o lixo não orgânico do teu negócio (ex. embalagem)?`,  
  vender_lixo_negocio = `Vendes o lixo não orgânico do teu negócio (ex. embalagem)?`,  
  pouparAgua = `Quando desempenhas actividades do teu negócio, poupas água?`,  
  ReutilizarAgua = `Quando desempenhas actividades do teu negócio, reutilizas água?`,  
  utiliza_carvao = `Utilizas madeira carvão ou querosene no seu negócio?`,
  Desliga_os_equipamentos = `Desliga os equipamentos do vosso negócio quando não estás a utilizá-los?`,
  Utilizas_energia_solar = `Utilizas energia solar no seu negócio?`
  
  )


 
p8transporte <- 0
p7embalagens <- 0
p6pegada <- 0
p4compras <- 0
p5compra <- 0
p9embalados <- 0
Pontuacao_aquisicao <- 0

# Compra de fornecedor local
if (compra_fornecedor_local == "Sempre compra de fornecedor local") {
  p4compras <- 0
} else if (compra_fornecedor_local == "As vezes compra de fornecedor local") {
  p4compras <- 1
} else if (compra_fornecedor_local == "Nunca compra de fornecedor local") {
  p4compras <- 2
}

# Quantas vezes compra a grosso
if (Quantas_vezes == "Compra a grosso 1-2 vezes na semana/mês (a depender do negócio)") {
  p5compra <- 0
} else if (Quantas_vezes == "Compra a grosso 3-4 vezes na semana/mês (a depender do negócio)") {
  p5compra <- 1
} else if (Quantas_vezes == "Compra sempre que precisa sem se preocupar com quantas deslocações está a fazer") {
  p5compra <- 2
}

# Priorização de produtos de menor pegada de carbono
if (Quando_compras == "Sempre prioriza produtos de menor pegada de carbono (ex. papel ao invés de plástico)") {
  p6pegada <- 0
} else if (Quando_compras == "Às vezes prioriza produtos de menor pegada de carbono (ex. papel ao invés de plástico)") {
  p6pegada <- 1
} else if (Quando_compras == "Nunca prioriza produtos de menor pegada de carbono (ex. papel ao invés de plástico)") {
  p6pegada <- 2
}

# Priorização de embalagens de menor pegada de carbono
if (Quando_compras_as_embalagens == "Sempre prioriza produtos de menor pegada de carbono (ex. papel ao invés de plástico)") {
  p7embalagens <- 0
} else if (Quando_compras_as_embalagens == "Às vezes prioriza produtos de menor pegada de carbono (ex. papel ao invés de plástico)") {
  p7embalagens <- 1
} else if (Quando_compras_as_embalagens == "Nunca prioriza produtos de menor pegada de carbono (ex. papel ao invés de plástico)") {
  p7embalagens <- 2
}

# Meios de deslocação de baixa emissão de carbono
if (Quando_vais_a_comprar == "Sempre utiliza meios de deslocação que têm baixa emissão de carbono (ex. bicicleta, ir de chapa ou ir a pé)") {
  p8transporte <- 0
} else if (Quando_vais_a_comprar == "Às vezes utiliza meios de deslocação que têm baixa emissão de carbono (ex. bicicleta, ir de chapa ou ir a pé)") {
  p8transporte <- 1
} else if (Quando_vais_a_comprar == "Nunca utiliza meios de deslocação que têm baixa emissão de carbono (ex. bicicleta, ir de chapa ou ir a pé)") {
  p8transporte <- 2
}

# Produtos vendidos em embalagens plásticas
if (levas_embora_embalados == "Nunca adquire produtos vendidos em embalagens plásticas") {
  p9embalados <- 0
} else if (levas_embora_embalados == "Às vezes adquire produtos vendidos em embalagens plásticas") {
  p9embalados <- 1
} else if (levas_embora_embalados == "Sempre adquire produtos vendidos em embalagens plásticas") {
  p9embalados <- 2
}

# Cálculo do resultado
Pontuacao_aquisicao <- p4compras + p5compra + p6pegada + p7embalagens + p8transporte + p9embalados

 

