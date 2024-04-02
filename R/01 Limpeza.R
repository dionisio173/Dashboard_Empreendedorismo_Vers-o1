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
  compra_fornecedor_local = `Compras os produtos para seu negócio de fornecedor local?`,
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
  Utilizas_energia_solar = `Utilizas energia solar no seu negócio?`,
  Pontuacao_Gestao_de_residuos=`Pontuação Gestão de residuos`,
  ano_projeto=`Ano do Projecto`,
  nome_projeto=`Nome Projecto`,
  cidade=Cidade,
  Tipo_de_Avaliacao = `Tipo de Avaliação`,
  ciclo= Ciclo
  )


###########################AQUISICAO########################################################
# Inicialização das variáveis de pontuação
dados_pegadas$p8transporte <- 0
dados_pegadas$p7embalagens <- 0
dados_pegadas$p6pegada <- 0
dados_pegadas$p4compras <- 0
dados_pegadas$p5compra <- 0
dados_pegadas$p9embalados <- 0
dados_pegadas$Pontuacao_aquisicao <- 0

# Compra de fornecedor local
dados_pegadas$p4compras <- ifelse(dados_pegadas$compra_fornecedor_local == "Sempre compra de fornecedor local", 0,
                                  ifelse(dados_pegadas$compra_fornecedor_local == "As vezes compra de fornecedor local", 1, 2))

# Quantas vezes compra a grosso
dados_pegadas$p5compra <- ifelse(dados_pegadas$Quantas_vezes == "Compra a grosso 1-2 vezes na semana/mês (a depender do negócio)", 0,
                                 ifelse(dados_pegadas$Quantas_vezes == "Compra a grosso 3-4 vezes na semana/mês (a depender do negócio)", 1, 2))

# Priorização de produtos de menor pegada de carbono
dados_pegadas$p6pegada <- ifelse(dados_pegadas$Quando_compras == "Sempre prioriza produtos de menor pegada de carbono (ex. papel ao invés de plástico)", 0,
                                 ifelse(dados_pegadas$Quando_compras == "Às vezes prioriza produtos de menor pegada de carbono (ex. papel ao invés de plástico)", 1, 2))

# Priorização de embalagens de menor pegada de carbono
dados_pegadas$p7embalagens <- ifelse(dados_pegadas$Quando_compras_as_embalagens == "Sempre prioriza produtos de menor pegada de carbono (ex. papel ao invés de plástico)", 0,
                                     ifelse(dados_pegadas$Quando_compras_as_embalagens == "Às vezes prioriza produtos de menor pegada de carbono (ex. papel ao invés de plástico)", 1, 2))

# Meios de deslocação de baixa emissão de carbono
dados_pegadas$p8transporte <- ifelse(dados_pegadas$Quando_vais_a_comprar == "Sempre utiliza meios de deslocação que têm baixa emissão de carbono (ex. bicicleta, ir de chapa ou ir a pé)", 0,
                                     ifelse(dados_pegadas$Quando_vais_a_comprar == "Às vezes utiliza meios de deslocação que têm baixa emissão de carbono (ex. bicicleta, ir de chapa ou ir a pé)", 1, 2))

# Produtos vendidos em embalagens plásticas
dados_pegadas$p9embalados <- ifelse(dados_pegadas$levas_embora_embalados == "Nunca adquire produtos vendidos em embalagens plásticas", 0,
                                    ifelse(dados_pegadas$levas_embora_embalados == "Às vezes adquire produtos vendidos em embalagens plásticas", 1, 2))

# Cálculo do resultado
dados_pegadas$Pontuacao_aquisicao <- dados_pegadas$p4compras + dados_pegadas$p5compra + dados_pegadas$p6pegada + dados_pegadas$p7embalagens + dados_pegadas$p8transporte + dados_pegadas$p9embalados


######################ENTREGAS#####################################################
# Inicialização das variáveis
dados_pegadas$P1local <- 0
dados_pegadas$P2local <- 0
dados_pegadas$PontuacaoEntregas <- 0

# Entrega de produtos
dados_pegadas$P1local <- ifelse(dados_pegadas$entregar_produto == "Sempre utiliza meios de deslocação que têm baixa emissão de gases (ex. bicicleta, ir de chapa ou ir a pé)", 0,
                  ifelse(dados_pegadas$entregar_produto == "Às vezes utiliza meios de deslocação que têm baixa emissão de gases (ex. bicicleta, ir de chapa ou ir a pé)", 1, 2))

# Quando entrega compras as embalagens
dados_pegadas$P2local <- ifelse(dados_pegadas$Quando_entrega_compras_as_embalagens == "Sempre utiliza embalagens reutilizáveis ou mais amigas do meio ambiente (ex. papel ao invés de plástico)", 0,
                  ifelse(dados_pegadas$Quando_entrega_compras_as_embalagens == "Às vezes utiliza embalagens reutilizáveis ou mais amigas do meio ambiente (ex. papel ao invés de plástico)", 1, 2))

# Calculando a pontuação das entregas
dados_pegadas$PontuacaoEntregas <- dados_pegadas$P1local + dados_pegadas$P2local

#######################CONSUMO ENERGIA################################################
# Inicialização das variáveis
dados_pegadas$p1energia <- 0
dados_pegadas$p2energia <- 0
dados_pegadas$p3energia <- 0
dados_pegadas$Pontuacao_Consumo_energia <- 0

# Controlando a utilização de carvão
dados_pegadas$p1energia <- ifelse(dados_pegadas$utiliza_carvao == "Nunca utiliza madeira, carvão ou querosene", 0,
                                  ifelse(dados_pegadas$utiliza_carvao == "As vezes utiliza madeira, carvão ou querosene", 1, 2))

# Desliga os equipamentos
dados_pegadas$p2energia <- ifelse(dados_pegadas$Desliga_os_equipamentos == "Sempre desliga os equipamentos quando não estão em uso", 0,
                                  ifelse(dados_pegadas$Desliga_os_equipamentos == "Às vezes desliga os equipamentos quando não estão em uso", 1, 2))

# Utilizas energia solar
dados_pegadas$p3energia <- ifelse(dados_pegadas$Utilizas_energia_solar == "Sempre utiliza energia solar", 0,
                                  ifelse(dados_pegadas$Utilizas_energia_solar == "Às vezes utiliza energia solar", 1, 2))

# Calculando a pontuação de consumo de energia
dados_pegadas$Pontuacao_Consumo_energia <- dados_pegadas$p1energia + dados_pegadas$p2energia + dados_pegadas$p3energia

########################GESTAO DE RESIDUOS###################################################
# Inicialização das variáveis de pontuação
dados_pegadas$P1LIXO <- 0
dados_pegadas$P2LIXO <- 0
dados_pegadas$P3LIXO <- 0
dados_pegadas$P4LIXO <- 0
dados_pegadas$P5LIXO <- 0
dados_pegadas$P6LIXO <- 0
dados_pegadas$P7LIXO <- 0
dados_pegadas$Pontuacao_Gestao_de_residuos <- 0

# Separar lixo orgânico
dados_pegadas$P1LIXO <- ifelse(dados_pegadas$separar_lixoOrganico == "Sempre separa o lixo organico do lixo não organico", 0,
                               ifelse(dados_pegadas$separar_lixoOrganico == "As vezes separa o lixo organico do lixo não organico", 1, 2))

# Utilizar lixo orgânico para compostagem
dados_pegadas$P2LIXO <- ifelse(dados_pegadas$ulizarlixoorganico == "Sempre utiliza o lixo organico para compostagem", 0,
                               ifelse(dados_pegadas$ulizarlixoorganico == "As vezes utiliza o lixo organico para compostagem", 1, 2))

# Vender lixo para compostagem
dados_pegadas$P3LIXO <- ifelse(dados_pegadas$vender_lixo_compostagem == "Sempre vende o lixo orgânico para compostagem", 0,
                               ifelse(dados_pegadas$vender_lixo_compostagem == "Às vezes vende o lixo orgânico para compostagem", 1, 2))

# Reutilizar lixo não orgânico
dados_pegadas$P4LIXO <- ifelse(dados_pegadas$reutilizar_lixoOrganico == "Sempre reutiliza o lixo não orgânico dentro da empresa", 0,
                               ifelse(dados_pegadas$reutilizar_lixoOrganico == "Às vezes reutiliza o lixo não orgânico dentro da empresa", 1, 2))

# Vender lixo não orgânico
dados_pegadas$P5LIXO <- ifelse(dados_pegadas$vender_lixo_negocio == "Sempre vende o lixo não organico que a empresa produz", 0,
                               ifelse(dados_pegadas$vender_lixo_negocio == "Às vezes vende o lixo não orgânico que a empresa produz", 1, 2))

# Poupar água
dados_pegadas$P6LIXO <- ifelse(dados_pegadas$pouparAgua == "Poupa água sempre quando desempenha atividades do negócio", 0,
                               ifelse(dados_pegadas$pouparAgua == "Poupa água quando é possível quando desempenha atividades do negócio", 1, 2))

# Reutilizar água
dados_pegadas$P7LIXO <- ifelse(dados_pegadas$ReutilizarAgua == "Sempre reutiliza água para desempenhar atividades do negócio", 0,
                               ifelse(dados_pegadas$ReutilizarAgua == "Às vezes reutiliza água para desempenhar atividades do negócio", 1, 2))

# Cálculo da pontuação total de gestão de resíduos
dados_pegadas$Pontuacao_Gestao_de_residuos <- dados_pegadas$P1LIXO + dados_pegadas$P2LIXO + dados_pegadas$P3LIXO + dados_pegadas$P4LIXO + dados_pegadas$P5LIXO + dados_pegadas$P6LIXO + dados_pegadas$P7LIXO


###RESULTADO FINAL

dados_pegadas$Pontuacao_de_Pegada_de_Carbono <- dados_pegadas$Pontuacao_aquisicao + dados_pegadas$Pontuacao_Gestao_de_residuos + dados_pegadas$PontuacaoEntregas + dados_pegadas$Pontuacao_Consumo_energia

# Se a pegada do negócio estiver entre 0 e 12, significa que sua pegada é baixa.
# Se a pegada do negócio estiver entre 13 e 24, significa que sua pegada é média
# Se a pegada do negócio estiver entre 25 e 36, significa que sua pegada é alta.
dados_pegadas$Status <- ifelse(
  dados_pegadas$Pontuacao_de_Pegada_de_Carbono >= 0 & dados_pegadas$Pontuacao_de_Pegada_de_Carbono <= 12,
  "PEGADA DE CARBONO BAIXA",
  ifelse(
    dados_pegadas$Pontuacao_de_Pegada_de_Carbono >= 13 & dados_pegadas$Pontuacao_de_Pegada_de_Carbono <= 24,
    "PEGADA DE CARBONO MÉDIA",
    ifelse(
      dados_pegadas$Pontuacao_de_Pegada_de_Carbono >= 25 & dados_pegadas$Pontuacao_de_Pegada_de_Carbono <= 36,
      "PEGADA DE CARBONO ALTA",
      NA  # Valor padrão caso nenhuma condição seja atendida
    )
  )
)

##apagar variveis 
dados_pegadas$P1LIXO <- NULL
dados_pegadas$P2LIXO <- NULL
dados_pegadas$P3LIXO <- NULL
dados_pegadas$P4LIXO <- NULL
dados_pegadas$P5LIXO <- NULL
dados_pegadas$P6LIXO <- NULL
dados_pegadas$P7LIXO <- NULL
dados_pegadas$p1energia <- NULL
dados_pegadas$p2energia <- NULL
dados_pegadas$p3energia <- NULL
dados_pegadas$P1local <- NULL
dados_pegadas$P2local <- NULL
dados_pegadas$p8transporte <- NULL
dados_pegadas$p7embalagens <- NULL
dados_pegadas$p6pegada <- NULL
dados_pegadas$p4compras <- NULL
dados_pegadas$p5compra <- NULL
dados_pegadas$p9embalados <- NULL


###ALTERAR status_pegada_carbono 
dados_pegadas <- dados_pegadas %>% rename(
  status_pegada_carbono= Status
)
