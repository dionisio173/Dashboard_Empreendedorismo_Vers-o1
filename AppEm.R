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
dados_empreendedoras <- read_excel("Pegada de Carbono Report.xlsx")

dados_pegadas <- dados_empreendedoras



# Definir a interface do usuário
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Dashboard Empreendedorismo"),
  navbarPage("Navegação",
             tabPanel("PAM_VERDE",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("projeto", "Projeto:", choices = c("Todos", unique(dados_ficticios$Projeto))),
                          selectInput("cidade", "Cidade:", choices = c("Todas", unique(dados_ficticios$Cidade))),
                          selectInput("ano", "Ano do Projeto:", choices = c("Todos", unique(dados_ficticios$Ano_Projeto))),
                          selectInput("ciclo", "Ciclo:", choices = c("Todos", unique(dados_ficticios$Ciclo))),
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Visão Geral", 
                                     wellPanel(
                                       withSpinner(plotOutput("resumo")),
                                       downloadButton("downloadData", "Baixar Inscritas")
                                     )
                            ),
                            tabPanel("Análise Dos Ciclos",
                                     column(12,
                                            wellPanel(((" ")),
                                                      withSpinner(plotOutput("grafico_lucro_mes")),
                                                      downloadButton("downloadLucro", "Baixar Dados de Lucro")
                                            ),
                                            column(12,
                                                   # wellPanel(("Grafico de linha")),
                                                   # (withSpinner(plotOutput("grafico_linha"), color = "black"))
                                            )
                                     )
                            ),
                            
                            tabPanel("Tabela de Dados", 
                                     wellPanel(
                                       withSpinner(dataTableOutput("tabela_dados"))
                                     )
                            )
                          )
                        )
                      )
             ),
             tabPanel("PEGADA DE CARBONO",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("projeto_pegada", "Projeto:", choices = c("Todos", unique(dados_pegadas$nome_projeto))),
                          selectInput("cidade_pegada", "Cidade:", choices = c("Todas", unique(dados_pegadas$cidade))),
                          selectInput("ano_pegada", "Ano do Projeto:", choices = c("Todos", unique(dados_pegadas$ano_projeto))),
                          selectInput("ciclo_pegada", "Ciclo:", choices = c("Todos", unique(dados_pegadas$ciclo))),
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Visão Geral", 
                                     wellPanel(
                                       withSpinner(plotOutput("Grafico_barras_pegada")),
                                       downloadButton("downloadVisaoGeral_pegada", "Baixar Visão Geral")
                                     )
                            ),
                            tabPanel("Análise das Pontuações",
                                     wellPanel(
                                       plotOutput("graficoPontuacao"),
                                       downloadButton("downloadPontuacao", "Baixar Pontuações")
                                     )
                            )
                          )
                        )
                      )
             )
  )
)

# Definir o servidor
server <- function(input, output) {
  
  # Função para download dos dados Financeiros
  output$downloadData <- downloadHandler(
      filename = function() {
        "Dados_ficticios.xlsx"
      },
      content = function(file) {
        # Escreve os dados em um arquivo Excel
        write.xlsx(dados_ficticios, path = file)
    }
  )  
 
  # Subconjunto dos dados com base nos filtros selecionados
  dados_filtrados <- reactive({
    df <- dados_ficticios
    if (input$projeto != "Todos") {
      df <- df[df$Projeto == input$projeto, ]
    }
    if (input$cidade != "Todas") {
      df <- df[df$Cidade == input$cidade, ]
    }
    if (input$ano != "Todos") {
      df <- df[df$Ano_Projeto == as.numeric(input$ano), ]
    }
    if (input$ciclo != "Todos") {
      df <- df[df$Ciclo == input$ciclo, ]
    }
    df
  })
  
  output$resumo <- renderPlot({
    # Calcular valores e percentagens
    dados_resumo <- dados_filtrados() %>%
      group_by(Projeto) %>%
      summarise(n = n()) %>%
      mutate(Percentagem = n / sum(n) * 100)
    
    ggplot(dados_resumo, aes(x = Projeto, y = n, fill = Projeto)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(n, " (", round(Percentagem, 1), "%)")),
                position = position_stack(vjust = 0.5), size = 9) +
      labs(x = "Projeto", y = "Número de Inscritas", title = "Número de Empreendedoras Inscritas por Projeto") +
      theme(legend.position = "none")
  })
 ########################LUCRO MES########################################################
  
  output$grafico_lucro_mes <- renderPlot({
    dados_filtrados() %>%
      group_by(Periodo_Mes, Ciclo) %>%
      summarise(Media_Lucro = mean(Lucro)) %>%
      mutate(Periodo_Mes = factor(Periodo_Mes, levels = c("Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho",
                                                          "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"))) %>%
      ggplot(aes(x = Periodo_Mes, y = Media_Lucro, fill = Ciclo)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(Media_Lucro, 2)), vjust = -0.5) +
      scale_fill_manual(values = c("#9442D4", "#F77333", "#008080")) +
      labs(x = "Mês", y = "Média de Lucro", title = "Média de Lucro por Mês VS Ciclo")
  })
  
  output$downloadLucro <- downloadHandler(
    filename = function() {
      "dados_lucro.xlsx"
    },
    content = function(file) {
      # Escreva os dados de lucro em um arquivo Excel
      write.xlsx(dados_lucro, path = file)
    }
  )
  
  ############################################
  # output$grafico_linha <- renderPlot({
  #   dados_filtrados() %>%
  #     group_by(Periodo_Mes, Ciclo) %>%
  #     summarise(Media_Lucro = mean(Lucro)) %>%
  #     mutate(Periodo_Mes = factor(Periodo_Mes, levels = c("Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho",
  #                                                         "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"))) %>%
  #     ggplot(aes(x = Periodo_Mes, y = Media_Lucro, color = Ciclo)) +
  #     geom_line() +
  #     geom_point() +
  #     geom_text(aes(label = round(Media_Lucro, 2)), vjust = -0.5) +
  #     scale_color_manual(values = c("red", "blue", "green")) +
  #     labs(x = "Mês", y = "Média de Lucro", title = "Média de Lucro por Mês vs. Ciclos")
  # })
  
  #############################TABELA GERAL#########################################
  output$tabela_dados <- renderDataTable({
    dados_filtrados()
  })
##########################PEGADA DE CARBONO######################
  # Subconjunto dos dados com base nos filtros selecionados para a Pegada de Carbono
  dados_pegada_filtrados <- reactive({
    df <- dados_pegadas
    if (input$projeto_pegada != "Todos") {
      df <- df[df$nome_projeto == input$projeto_pegada, ]
    }
    if (input$cidade_pegada != "Todas") {
      df <- df[df$cidade == input$cidade_pegada, ]
    }
    if (input$ano_pegada != "Todos") {
      df <- df[df$ano_projeto == as.numeric(input$ano_pegada), ]
    }
    if (input$ciclo_pegada != "Todos") {
      df <- df[df$ciclo == input$ciclo_pegada, ]
    }
    df
  })
  
  # Renderização do gráfico de barras para o número de inscritas na Pegada de Carbono
  output$Grafico_barras_pegada <- renderPlot({
    # Contagem de inscritas por projeto
    dados_contagem <- dados_pegada_filtrados() %>%
      group_by(nome_projeto) %>%
      summarise(num_inscritas = n())
    
    # Adicionando percentagens
    dados_contagem <- dados_contagem %>%
      mutate(Percentagem = num_inscritas / sum(num_inscritas) * 100)
    
    # Gráfico de barras com cores diferentes para cada projeto
    ggplot(dados_contagem, aes(x = nome_projeto, y = num_inscritas, fill = nome_projeto)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(num_inscritas, " (", round(Percentagem, 1), "%)")),
                position = position_stack(vjust = 0.5), size = 9) +
      labs(x = "Projeto", y = "Número de Inscritas", title = "Número de Inscritas na Pegada de Carbono por Projeto") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  
  # Função para download dos dados de visão geral da Pegada de Carbono em formato Excel
  output$downloadVisaoGeral_pegada <- downloadHandler(
    filename = function() {
      "VisaoGeral_pegada.xlsx"
    },
    content = function(file) {
      # Escreve os dados de visão geral em um arquivo Excel
      write.xlsx(dados_pegada_filtrados(), path = file)
    }
  )
  ####################Pontuacões##############
  output$graficoPontuacao <- renderPlot({
    # Subconjunto dos dados com base nos filtros selecionados
    dados_filtrados <- dados_empreendedoras
    if (input$projeto_pegada != "Todos") {
      dados_filtrados <- dados_filtrados[dados_filtrados$nome_projeto == input$projeto_pegada, ]
    }
    if (input$cidade_pegada != "Todas") {
      dados_filtrados <- dados_filtrados[dados_filtrados$cidade == input$cidade_pegada, ]
    }
    if (input$ano_pegada != "Todos") {
      dados_filtrados <- dados_filtrados[dados_filtrados$ano_projeto == as.numeric(input$ano_pegada), ]
    }
    if (input$ciclo_pegada != "Todos") {
      dados_filtrados <- dados_filtrados[dados_filtrados$ciclo == input$ciclo_pegada, ]
    }
    
    # Contagem de participantes por categoria de pegada de carbono
    dados_contagem <- dados_filtrados %>%
      group_by(status_pegada_carbono) %>%
      summarise(num_participantes = n())
    
    # Adicionando percentagens
    dados_contagem <- dados_contagem %>%
      mutate(Percentagem = num_participantes / sum(num_participantes) * 100)
    
    # Gráfico de barras com cores diferentes para cada categoria
    ggplot(dados_contagem, aes(x = status_pegada_carbono, y = num_participantes, fill = status_pegada_carbono)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(num_participantes, " (", round(Percentagem, 1), "%)")),
                position = position_stack(vjust = 0.5), size = 9) +
      labs(x = "Status Pegada de Carbono", y = "Número de Participantes", title = "Número de Participantes por Categoria de Pegada de Carbono") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
}
  
# Executar o aplicativo
shinyApp(ui = ui, server = server)