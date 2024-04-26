# Carregar bibliotecas necessárias
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(openxlsx)

# Definir a interface do usuário
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Dashboard Empreendedorismo"),
  navbarPage("Navegação",
             tabPanel("DADOS FINANCEIROS",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("cidade", "Cidade:", choices = c("Todas", unique(dados_ficticios$Cidade))),
                          selectInput("ano", "Ano do Projeto:", choices = c("Todos", sort(unique(dados_ficticios$Ano_Projeto)))),
                          selectInput("nome", "Nome:", choices = c("Todos", unique(dados_ficticios$Nome)), selected = unique(dados_ficticios$Nome)[1])
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Visão Geral", 
                                     selectInput("cidade12", "Cidade:", choices = c("Todas", unique(dados_ficticios$Cidade))),
                                     wellPanel(
                                       withSpinner(plotOutput("resumo")),
                                       withSpinner(plotOutput("distribuicaoSetor", width = "115%")),  # Definindo a largura do gráfico como 100%         downloadButton("downloadData", "Baixar Inscritas")
                                     )
                            ),
                            tabPanel("Análise Dos Ciclos",
                                     column(12,
                                            wellPanel(
                                              withSpinner(plotOutput("grafico_lucro_mes")),
                                              downloadButton("downloadLucro", "Baixar Dados de Lucro")
                                            ),
                                            wellPanel(
                                              withSpinner(plotOutput("grafico_barras"), color = "black"),
                                              downloadButton("downloadBarras", "Baixar Dados do Gráfico de Barras")
                                            )
                                     )
                            ),
                            
                            tabPanel("Desempenho Individual",
                                     wellPanel(
                                       withSpinner(plotOutput("grafico"))
                                     )
                            )
                          )
                        )
                      )
             ),
             tabPanel("PEGADA DE CARBONO",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("cidade_pegada", "Cidade:", choices = c("Todas", unique(dados_pegadas$cidade)), width = "200px"),  # Ajustando a largura do selectInput
                          selectInput("ano_pegada", "Ano do Projeto:", choices = c("Todos", unique(dados_pegadas$ano_projeto)), width = "200px"),  # Ajustando a largura do selectInput
                          selectInput("ciclo_pegada", "Ciclo:", choices = c("Todos", unique(dados_pegadas$ciclo)), width = "200px")  # Ajustando a largura do selectInput
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Visão Geral", 
                                     wellPanel(
                                       withSpinner(plotOutput("Grafico_barras_pegada", width = "90%")),  # Ajusta a largura para 100%
                                       downloadButton("downloadVisaoGeral_pegada", "Baixar Visão Geral")
                                     )
                            ),
                            
                            tabPanel("Análise das Pontuações",
                                     wellPanel(
                                       plotOutput("graficoPontuacao", width = "100%"),  # Ajusta a largura para 100%
                                       downloadButton("downloadPontuacao", "Baixar Pontuações")
                                     )
                            ),
                            
                            tabPanel("Pegada Por Sector",
                                     wellPanel(
                                       plotOutput("graficoBaseline", width = "100%"),  # Ajusta a largura para 100%
                                       downloadButton("downloadBaseline", "Baixar Baseline")
                                     ),
                                     wellPanel(
                                       plotOutput("graficoEndline", width = "100%"),  # Ajusta a largura para 100%
                                       downloadButton("downloadEndline", "Baixar Endline")
                                     )
                            )
                          ),
                          width = 11.5  # Define a largura do mainPanel
                        )
                        
                      )
             )
  ))

# Definir o servidor
server <- function(input, output, session) {
  # Atualiza as opções do filtro "Ano do Projeto" com base na cidade selecionada
  observe({
    cidades <- unique(dados_ficticios$Cidade)
    if (input$cidade != "Todas") {
      anos <- unique(dados_ficticios$Ano_Projeto[dados_ficticios$Cidade == input$cidade])
    } else {
      anos <- unique(dados_ficticios$Ano_Projeto)
    }
    updateSelectInput(session, "ano", choices = c("Todos", sort(anos)))
  })
  
  # Atualiza as opções do filtro "Nome" com base na cidade e ano selecionados
  observe({
    cidades <- unique(dados_ficticios$Cidade)
    anos <- unique(dados_ficticios$Ano_Projeto)
    
    if (input$cidade != "Todas") {
      if (input$ano != "Todos") {
        nomes <- unique(dados_ficticios$Nome[dados_ficticios$Cidade == input$cidade & dados_ficticios$Ano_Projeto == as.numeric(input$ano)])
      } else {
        nomes <- unique(dados_ficticios$Nome[dados_ficticios$Cidade == input$cidade])
      }
    } else {
      if (input$ano != "Todos") {
        nomes <- unique(dados_ficticios$Nome[dados_ficticios$Ano_Projeto == as.numeric(input$ano)])
      } else {
        nomes <- unique(dados_ficticios$Nome)
      }
    }
    
    updateSelectInput(session, "nome", choices = c("Todos", nomes), selected = nomes[1])
  })
  
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
      df <- df[grep(input$ano, df$Ano_Projeto), ]
    }
    df
  })
  
  output$resumo <- renderPlot({
    # Calcular valores e percentagens
    dados_resumo <- dados_filtrados() %>%
      distinct(Nome) %>%
      group_by(Projeto) %>%
      summarise(n = n()) %>%
      mutate(Percentagem = n / sum(n) * 100)
    
    # Gráfico de barras
    ggplot(dados_resumo, aes(x = Projeto, y = n, fill = Projeto)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(n, " (", round(Percentagem, 1), "%)")),
                position = position_stack(vjust = 0.5), size = 9) +
      labs(x = "Projeto", y = "Número de Inscritas", title = "Número de Empreendedoras Inscritas por Projeto") +
      theme(legend.position = "none", panel.grid = element_blank())  +
      scale_y_continuous(labels = NULL) +  # Retirar valores do eixo vertical
      scale_fill_brewer(palette = "Set3")  # Escolher uma paleta de cores
  })
  
  output$distribuicaoSetor <- renderPlot({
    # Calcular valores e percentagens
    dados_resumo <- dados_filtrados() %>%
      distinct(Nome, .keep_all = TRUE) %>%
      group_by(Sector_Negocio) %>%
      summarise(n = n()) %>%
      mutate(Percentagem = n / sum(n) * 100)
    
    ggplot(dados_resumo, aes(x = Sector_Negocio, y = n, fill = Sector_Negocio)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(n, " (", round(Percentagem, 1), "%)")),
                position = position_stack(vjust = 0.5), size = 9) +
      labs(x = "Setor de Negócio", y = "Número de Inscritas", title = "Número de Inscritas por Setor de Negócio") +
      theme(legend.position = "none", panel.grid = element_blank()) +
      scale_y_continuous(labels = NULL) # Retirar valores do eixo vertical
  })
  
  ########################LUCRO MES########################################################
  
  output$grafico_lucro_mes <- renderPlot({
    dados_filtrados() %>%
      group_by(Periodo_Mes, Ano_Projeto) %>%
      summarise(Media_Lucro = mean(Lucro)) %>%
      mutate(Periodo_Mes = factor(Periodo_Mes, levels = c("Primeiro Mes de Implementação","Segundo Mes de Implementação", "Terceiro Mes de Implementação", "Quarto Mes de Implementação", "Quinto Mês de Implementação"))) %>%
      ggplot(aes(x = Periodo_Mes, y = Media_Lucro, color = Ano_Projeto, group = Ano_Projeto)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      labs(x = "Mês", y = "Média de Lucro", title = "Média de Lucro por Mês VS Ciclo") +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 10),
            legend.position = "bottom") # Coloca a legenda na parte inferior
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
  
  ########################LUCRO SECTORES######################### 
  output$grafico_barras <- renderPlot({ 
    # Filtro dos dados
    dados_filtrados <- dados_ficticios
    
    if (input$cidade != "Todas") {
      dados_filtrados <- dados_filtrados[dados_filtrados$Cidade == input$cidade, ]
    }
    
    if (input$ano != "Todos") {
      dados_filtrados <- dados_filtrados[dados_filtrados$Ano_Projeto == as.numeric(input$ano), ]
    }
    
    # Calcular o lucro total por setor de negócio
    lucro_por_setor <- dados_filtrados %>%
      group_by(Sector_Negocio) %>%
      summarise(Lucro_total = sum(Lucro, na.rm = TRUE)) %>%
      arrange(desc(Lucro_total))  # Ordenar os setores pelo lucro total em ordem decrescente
    
    # Criar o gráfico de barras horizontal
    ggplot(lucro_por_setor, aes(x = Lucro_total, y = reorder(Sector_Negocio, Lucro_total))) +
      geom_bar(stat = "identity", fill = "skyblue") +
      geom_text(aes(label = round(Lucro_total, 2)), hjust = 1, size = 4, color = "black") +  # Adiciona os valores ao gráfico
      labs(y = "Setor de Negócio", x = "Lucro Total", title = "Lucro Total por Setor de Negócio") +
      theme_minimal() +
      theme(axis.text.y = element_text(hjust = 0))  # Ajusta a posição do texto do eixo y
  })
  
  
  
  
  
  
  
  
  #############################GRAFICO DE METRICAS#########################################
  output$grafico <- renderPlot({
    dados_filtrados <- reactive({
      df <- dados_ficticios
      if (input$cidade != "Todas") {
        df <- df[df$Cidade == input$cidade, ]
      }
      if (input$ano != "Todos") {
        df <- df[df$Ano_Projeto == as.numeric(input$ano), ]
      }
      if (input$nome != "" && input$nome != "Todos") {
        df <- df[df$Nome == input$nome, ]
      }
      df$Periodo_Mes <- factor(df$Periodo_Mes, levels = c("Primeiro Mes de Implementação",
                                                          "Segundo Mes de Implementação",
                                                          "Terceiro Mes de Implementação",
                                                          "Quarto Mes de Implementação",
                                                          "Quinto Mês de Implementação"))
      df
    })
    
    ggplot(dados_filtrados(), aes(x = Periodo_Mes)) +
      geom_line(aes(y = Lucro, color = "Lucro", group = 1)) +
      geom_line(aes(y = Custo_Operacional, color = "Custo Operacional", group = 1)) +
      geom_line(aes(y = Custo_Produto, color = "Custo Produto", group = 1)) +
      geom_line(aes(y = Rendimento, color = "Rendimento", group = 1)) +
      geom_point(aes(y = Lucro, color = "Lucro"), size = 3) +
      geom_point(aes(y = Custo_Operacional, color = "Custo Operacional"), size = 3) +
      geom_point(aes(y = Custo_Produto, color = "Custo Produto"), size = 3) +
      geom_point(aes(y = Rendimento, color = "Rendimento"), size = 3) +
      geom_text(aes(y = Lucro, label = Lucro), vjust = -1.5, color = "blue") +
      geom_text(aes(y = Custo_Operacional, label = Custo_Operacional), vjust = -1.5, color = "red") +
      geom_text(aes(y = Custo_Produto, label = Custo_Produto), vjust = 1.5, color = "green") +
      geom_text(aes(y = Rendimento, label = Rendimento), vjust = 1.5, color = "orange") +
      labs(x = "Mês", y = "Total", title = "Métricas Financeiras por Mês") +
      scale_color_manual(name = "Variável",
                         values = c("Lucro" = "blue", "Custo Operacional" = "red", "Custo Produto" = "green", "Rendimento" = "orange"),
                         labels = c("Custo Operacional", "Custo Produto", "Lucro", "Rendimento")) +
      theme_minimal()
  })
  
  
  ##########################PEGADA DE CARBONO######################
  observe({
    cidades <- unique(dados_pegadas$cidade)
    if (input$cidade_pegada != "Todas") {
      anos <- unique(dados_pegadas$ano_projeto[dados_pegadas$cidade == input$cidade_pegada])
    } else {
      anos <- unique(dados_pegadas$ano_projeto)
    }
    updateSelectInput(session, "ano_pegada", choices = c("Todos", sort(anos)))
  })
  
  observe({
    cidades <- unique(dados_pegadas$cidade)
    anos <- unique(dados_pegadas$ano_projeto)
    if (input$cidade_pegada != "Todas") {
      if (input$ano_pegada != "Todos") {
        ciclos <- unique(dados_pegadas$ciclo[dados_pegadas$cidade == input$cidade_pegada & dados_pegadas$ano_projeto == as.numeric(input$ano_pegada)])
      } else {
        ciclos <- unique(dados_pegadas$ciclo[dados_pegadas$cidade == input$cidade_pegada])
      }
    } else {
      if (input$ano_pegada != "Todos") {
        ciclos <- unique(dados_pegadas$ciclo[dados_pegadas$ano_projeto == as.numeric(input$ano_pegada)])
      } else {
        ciclos <- unique(dados_pegadas$ciclo)
      }
    }
    updateSelectInput(session, "ciclo_pegada", choices = c("Todos", sort(ciclos)))
  })
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
      group_by(cidade, `Nome empreendedoras`) %>%
      summarise(num_inscritas = n_distinct(`Nome empreendedoras`)) %>%
      group_by(cidade) %>%
      summarise(total_inscritas = sum(num_inscritas))
    
    # Adicionando percentagens
    dados_contagem <- dados_contagem %>%
      mutate(Percentagem = total_inscritas / sum(total_inscritas) * 100)
    
    # Gráfico de barras com cores diferentes para cada cidade
    ggplot(dados_contagem, aes(x = cidade, y = total_inscritas, fill = cidade)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(total_inscritas, " (", round(Percentagem, 1), "%)")),
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
    p <- ggplot(dados_contagem, aes(x = status_pegada_carbono, y = num_participantes, fill = status_pegada_carbono)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(num_participantes, " (", round(Percentagem, 1), "%)")),
                position = position_stack(vjust = 0.5), size = 7) +
      labs(x = "Status Pegada de Carbono", y = "Número de Participantes", title = "") +
      facet_wrap(~Tipo_de_Avaliacao, scales = "free") +
      theme_minimal() +
      theme(legend.position = "bottom",  # Coloca a legenda na parte inferior
            panel.grid = element_blank(),
            strip.text = element_text(size = 15)) +
      scale_fill_manual(values = c("#F37238", "#8054A2", "#69C7BE")) +
      scale_y_continuous(labels = NULL)
    
    # Adicionar linha vertical
    p <- p + geom_vline(xintercept = 3.6, linetype = "dashed", color = "black")
    print(p)
  })
  
  ################Sector#####################
  # Renderização do gráfico de linha para o baseline da Pegada de Carbono
  output$graficoBaseline <- renderPlot({
    # Subconjunto dos dados com base nos filtros selecionados e apenas para Baseline
    dados_filtrados <- dados_pegadas[dados_pegadas$Tipo_de_Avaliacao == "Baseline", ]
    
    if (input$cidade_pegada != "Todas") {
      dados_filtrados <- dados_filtrados[dados_filtrados$cidade == input$cidade_pegada, ]
    }
    if (input$ano_pegada != "Todos") {
      dados_filtrados <- dados_filtrados[dados_filtrados$ano_projeto == as.numeric(input$ano_pegada), ]
    }
    if (input$ciclo_pegada != "Todos") {
      dados_filtrados <- dados_filtrados[dados_filtrados$ciclo == input$ciclo_pegada, ]
    }
    
    # Contagem de participantes por categoria de pegada de carbono e setor
    dados_contagem <- dados_filtrados %>%
      group_by(status_pegada_carbono, `SECTOR DA EMPREENDEDORA`) %>%
      summarise(num_participantes = n())
    
    # Adicionando percentagens
    dados_contagem <- dados_contagem %>%
      group_by(`SECTOR DA EMPREENDEDORA`) %>%
      mutate(Percentagem = num_participantes / sum(num_participantes) * 100)
    
    # Gráfico de barras empilhadas com cores diferentes para cada categoria de pegada de carbono
    p <- ggplot(dados_contagem, aes(x = `SECTOR DA EMPREENDEDORA`, y = num_participantes, fill = status_pegada_carbono)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(num_participantes, " (", round(Percentagem, 1), "%)")),
                position = position_stack(vjust = 0.5), size = 7) +
      labs(x = "Setor", y = "Número de Participantes", title = "") +
      theme_minimal() +
      theme(panel.grid = element_blank()) +
      scale_fill_manual(values = c("#F37238", "#8054A2", "#69C7BE"),
                        name = "Pegada de Carbono") +  # Adicionando legenda
      scale_y_continuous(labels = NULL) +
      theme(axis.text.x = element_text(size = 10),  # Aumentando o tamanho das letras dos setores
            legend.text = element_text(size = 8))  # Aumentando o tamanho das letras da legenda
    
    print(p)
  })
  
  output$graficoEndline <- renderPlot({
    # Subconjunto dos dados com base nos filtros selecionados e apenas para Endline
    dados_filtrados <- dados_pegadas[dados_pegadas$Tipo_de_Avaliacao == "EndLine", ]
    
    if (input$cidade_pegada != "Todas") {
      dados_filtrados <- dados_filtrados[dados_filtrados$cidade == input$cidade_pegada, ]
    }
    if (input$ano_pegada != "Todos") {
      dados_filtrados <- dados_filtrados[dados_filtrados$ano_projeto == as.numeric(input$ano_pegada), ]
    }
    if (input$ciclo_pegada != "Todos") {
      dados_filtrados <- dados_filtrados[dados_filtrados$ciclo == input$ciclo_pegada, ]
    }
    
    # Contagem de participantes por categoria de pegada de carbono e setor
    dados_contagem <- dados_filtrados %>%
      group_by(status_pegada_carbono, `SECTOR DA EMPREENDEDORA`) %>%
      summarise(num_participantes = n())
    
    # Adicionando percentagens
    dados_contagem <- dados_contagem %>%
      group_by(`SECTOR DA EMPREENDEDORA`) %>%
      mutate(Percentagem = num_participantes / sum(num_participantes) * 100)
    
    # Gráfico de barras empilhadas com cores diferentes para cada categoria de pegada de carbono
    p <- ggplot(dados_contagem, aes(x = `SECTOR DA EMPREENDEDORA`, y = num_participantes, fill = status_pegada_carbono)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(num_participantes, " (", round(Percentagem, 1), "%)")),
                position = position_stack(vjust = 0.5), size = 7) +
      labs(x = "Setor", y = "Número de Participantes", title = "") +
      theme_minimal() +
      theme(legend.position = "none", panel.grid = element_blank()) +
      scale_fill_manual(values = c("#F37238", "#8054A2", "#69C7BE")) +
      scale_y_continuous(labels = NULL)
    
    print(p)
  }) 
  
}

# Executar o aplicativo
shinyApp(ui = ui, server = server) 
