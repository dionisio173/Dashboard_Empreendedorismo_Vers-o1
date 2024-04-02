 
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



ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Dashboard Empreendedorismo"),
  navbarPage("Navegação",
             
             tabPanel("PAM_VERDE",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("cidade", "Cidade:", choices = c("Todas", unique(dados_ficticios$Cidade))),
                          selectInput("ano", "Ano do Projeto:", choices = c("Todos", unique(dados_ficticios$Ano_Projeto))),
                          selectInput("ciclo", "Ciclo:", choices = c("Todos", unique(dados_ficticios$Ciclo))),
                        ),
                        mainPanel(
                          dropdownMenu(type = "tabs",
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
                          selectInput("cidade_pegada", "Cidade:", choices = c("Todas", unique(dados_pegadas$cidade))),
                          selectInput("ano_pegada", "Ano do Projeto:", choices = c("Todos", unique(dados_pegadas$ano_projeto))),
                          selectInput("ciclo_pegada", "Ciclo:", choices = c("Todos", unique(dados_pegadas$ciclo))),
                        ),
                        
                        mainPanel(
                          dropdownMenu(type = "tabs",
                                       tabPanel("Visão Geral", 
                                                wellPanel(
                                                  withSpinner(plotOutput("Grafico_barras_pegada")),
                                                  downloadButton("downloadVisaoGeral_pegada", "Baixar Visão Geral")
                                                ),
                                                column(12,
                                                       wellPanel((("Grafico de Distribuicão dos Sectores ")),
                                                                 withSpinner(plotOutput("graficoSectores"))
                                                       )
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
      theme(legend.position = "none", panel.grid = element_blank())
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
      labs(x = "Mês", y = "Média de Lucro", title = "Média de Lucro por Mês VS Ciclo") +
      theme(legend.position = "none", panel.grid = element_blank())
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
    if (input$ano_pegada != "Todos") {
      df <- df[df$ano_projeto == as.numeric(input$ano_pegada), ]
    }
    if (input$ciclo_pegada != "Todos") {
      df <- df[df$ciclo == input$ciclo_pegada, ]
    }
    df
  })
  
  # Renderização do gráfico de barras para o número de inscritas na Pegada de Carbono por cidade
  output$Grafico_barras_pegada <- renderPlot({
    # Contagem de inscritas por cidade
    dados_contagem <- dados_pegada_filtrados() %>%
      group_by(cidade) %>%
      summarise(num_inscritas = n())
    
    # Adicionando percentagens
    dados_contagem <- dados_contagem %>%
      mutate(Percentagem = num_inscritas / sum(num_inscritas) * 100)
    
    # Gráfico de barras com cores diferentes para cada cidade
    ggplot(dados_contagem, aes(x = cidade, y = num_inscritas, fill = cidade)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(num_inscritas, " (", round(Percentagem, 1), "%)")),
                position = position_stack(vjust = 0.5), size = 9) +
      labs(x = "Cidade", y = "Número de Inscritas", title = "Número de Inscritas por Cidade") +
      theme_minimal() +
      theme(legend.position = "none", panel.grid = element_blank())
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

  ##############SECTOR DAS EMPREENDEDORAS############
  output$graficoSectores <- renderPlot({
    filtered_data <- dados_pegadas %>%
      filter(
        if (input$ano_pegada != "Todos") ano_projeto == input$ano_pegada else TRUE,
        if (input$ciclo_pegada != "Todos") ciclo == input$ciclo_pegada else TRUE
      )
    
    city_totals <- filtered_data %>%
      group_by(cidade) %>%
      summarise(total = n())
    
    percentage_data <- filtered_data %>%
      group_by(`SECTOR DA EMPREENDEDORA`, cidade) %>%
      summarise(count = n()) %>%
      left_join(city_totals, by = "cidade") %>%
      group_by(`SECTOR DA EMPREENDEDORA`) %>%
      mutate(percentage = count / total * 100)
    
    # Definindo uma paleta de cores alternativa
    cores_alternativas <- c("#F77333", "#69C7BE", "#8054A2", "#ADD8E6", "#90EE90", "#E6E6FA", "#FFFF00", "#FFA500")
    
    # Gráfico de barras faceteado por cidade
    ggplot(percentage_data) +
      aes(x = `SECTOR DA EMPREENDEDORA`, y = percentage, fill = `SECTOR DA EMPREENDEDORA`) +
      geom_col() +
      theme_minimal() +
      facet_wrap(vars(cidade)) +
      scale_fill_manual(values = cores_alternativas) +  # Usando a paleta de cores alternativa
      geom_text(aes(label = paste0(count, " (", round(percentage, 1), "%)")), 
                position = position_stack(vjust = 0.5)) +
      scale_x_discrete(guide = "none") +  # Remover a legenda no eixo x
      theme(legend.position = "right", 
            legend.box.background = element_blank(),
            legend.key = element_blank(),
            legend.title = element_blank(),
            guides(fill = guide_legend(override.aes = list(alpha = 0))),
        panel.grid = element_blank())
  })
  
  
  
  
  ####################Pontuacões##############
  output$graficoPontuacao <- renderPlot({
    # Subconjunto dos dados com base nos filtros selecionados
    dados_filtrados <- dados_pegadas
    
    if (input$cidade_pegada != "Todas") {
      dados_filtrados <- dados_filtrados[dados_filtrados$cidade == input$cidade_pegada, ]
    }
    if (input$ano_pegada != "Todos") {
      dados_filtrados <- dados_filtrados[dados_filtrados$ano_projeto == as.numeric(input$ano_pegada), ]
    }
    if (input$ciclo_pegada != "Todos") {
      dados_filtrados <- dados_filtrados[dados_filtrados$ciclo == input$ciclo_pegada, ]
    }
    
    # Contagem de participantes por categoria de pegada de carbono e tipo de avaliação
    dados_contagem <- dados_filtrados %>%
      group_by(status_pegada_carbono, Tipo_de_Avaliacao) %>%
      summarise(num_participantes = n())
    
    # Adicionando percentagens
    dados_contagem <- dados_contagem %>%
      group_by(Tipo_de_Avaliacao) %>%
      mutate(Percentagem = num_participantes / sum(num_participantes) * 100)
    
    # Gráfico de barras com cores diferentes para cada categoria
    ggplot(dados_contagem, aes(x = status_pegada_carbono, y = num_participantes, fill = status_pegada_carbono)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(num_participantes, " (", round(Percentagem, 1), "%)")),
                position = position_stack(vjust = 0.5), size = 7) +
      labs(x = "Status Pegada de Carbono", y = "Número de Participantes", title = "Número de Participantes por Categoria de Pegada de Carbono") +
      facet_wrap(~Tipo_de_Avaliacao, scales = "free") +
      theme_minimal() +
      theme(legend.position = "none", panel.grid = element_blank()) +
      scale_fill_manual(values = c("#F37238", "#69C7BE", "#8054A2"))
  })
}
   
# Executar o aplicativo
shinyApp(ui = ui, server = server)