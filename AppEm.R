# Carregar bibliotecas necessárias
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(openxlsx)

# Definir a interface do usuário
ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  theme = shinytheme("flatly"),
  titlePanel("Dashboard Empreendedorismo"),
  navbarPage("Navegação",
             tabPanel("DADOS FINANCEIROS",
                      sidebarPanel(
                        selectInput("cidade", "Cidade:", choices = c("Todas", unique(dados_ficticios$Cidade))),
                        selectInput("ano", "Ano do Projeto:", choices = c("Todos", sort(unique(dados_ficticios$Ano_Projeto))))
                      ),
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Visão Geral",
                                   fluidRow(
                                     column(6,
                                            wellPanel(
                                              uiOutput("total_inscritas"),
                                              downloadButton("downloadInscritas", "Baixar Dados de Inscritas")
                                            )
                                     ),
                                     column(6,
                                            wellPanel(
                                              uiOutput("legenda_dinamica")
                                            )
                                     )
                                   ),
                                   wellPanel(
                                     withSpinner(plotOutput("distribuicaoSetor", width = "100%"))
                                   )
                          ),
                          tabPanel("Análise Geral",
                                   column(12,
                                          wellPanel(
                                            absolutePanel(
                                              top = 10,
                                              right = "80%",
                                              # downloadButton("downloadLucro", "Baixar Dados de Lucro")
                                            ),
                                            withSpinner(plotOutput("grafico_lucro_mes"))
                                          ),
                                          wellPanel(
                                            withSpinner(plotOutput("grafico_barras"), color = "black"),
                                            # downloadButton("downloadBarras", "Baixar Dados do Gráfico de Barras")
                                          )
                                   )
                          ),
                          tabPanel("Desempenho Semanal",
                                   selectInput("nome", "Nome:", choices = c("Todos", unique(Financeiro_Report$`Nome empreendedoras`)), selected = unique(Financeiro_Report$`Nome empreendedoras`)[1]),
                                   selectInput("Periodo", "Mês:", 
                                               choices = c("Todos", sort(unique(Financeiro_Report$Periodo), decreasing = FALSE)), 
                                               selected = "Primeiro Mês"),
          
                                   wellPanel(
                                     withSpinner(plotOutput("grafico_semanal"))
                                   )
                          ),
                          tabPanel("Desempenho Mensal",
                                   selectInput("nome", "Nome:", choices = c("Todos", unique(dados_ficticios$Nome)), selected = unique(dados_ficticios$Nome)[1]),
                                   wellPanel(
                                     withSpinner(plotOutput("grafico"))
                                   )
                          )
                        )
                      )
             ),
             tabPanel("PEGADA DE CARBONO",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("cidade_pegada", "Cidade:", choices = c("Todas", unique(dados_pegadas$cidade)), width = "200px"),
                          selectInput("ano_pegada", "Ano do Projeto:", choices = c("Todos", unique(dados_pegadas$ano_projeto)), width = "200px"),
                          selectInput("ciclo_pegada", "Ciclo:", choices = c("Todos", unique(dados_pegadas$ciclo)), width = "200px")
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Visão Geral", 
                                     wellPanel(
                                       withSpinner(plotOutput("Grafico_barras_pegada", width = "70%")),
                                       downloadButton("downloadVisaoGeral_pegada", "Baixar Visão Geral")
                                     ),
                                     wellPanel(
                                       withSpinner(plotOutput("grafico_setores"))
                                     )
                            ),
                            tabPanel("Análise das Pontuações",
                                     wellPanel(
                                       plotOutput("graficoPontuacao", width = "90%"),
                                       downloadButton("downloadPontuacao", "Baixar Pontuações")
                                     )
                            ),
                            tabPanel("Pegada Por Sector",
                                     wellPanel(
                                       plotOutput("graficoBaseline", width = "100%"),
                                       downloadButton("downloadBaseline", "Baixar Baseline")
                                     ),
                                     wellPanel(
                                       plotOutput("graficoEndline", width = "100%"),
                                       downloadButton("downloadEndline", "Baixar Endline")
                                     )
                            )
                          ),
                          width = 10.5
                        )
                      )
             )
  )
)


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
        nomes <- unique(dados_ficticios$Nome[dados_ficticios$Cidade == input$cidade & dados_ficticios$Ano_Projeto == as.character(input$ano)])
      } else {
        nomes <- unique(dados_ficticios$Nome[dados_ficticios$Cidade == input$cidade])
      }
    } else {
      if (input$ano != "Todos") {
        nomes <- unique(dados_ficticios$Nome[dados_ficticios$Ano_Projeto == as.character(input$ano)])
      } else {
        nomes <- unique(dados_ficticios$Nome)
      }
    }
    
    updateSelectInput(session, "nome", choices = c("Todos", nomes), selected = nomes[1])
  })
  
 
  
  # Subconjunto dos dados com base nos filtros selecionados
  dados_filtrados <- reactive({
    df <- dados_ficticios
    if (input$cidade != "Todas") {
      df <- df[df$Cidade == input$cidade, ]
    }
    if (input$ano != "Todos") {
      df <- df[df$Ano_Projeto == as.character(input$ano), ]  # Converte para caractere
    }
    df
  })
  
  # Calcular número total de inscritas
  total_inscritas <- reactive({
    df <- dados_ficticios
    if (input$cidade != "Todas") {
      df <- df[df$Cidade == input$cidade, ]
    }
    if (input$ano != "Todos") {
      df <- df[df$Ano_Projeto == as.character(input$ano), ]
    }
    n_distinct(df$Nome)
  })
  
  # Gerar a legenda dinâmica
  legenda_dinamica <- reactive({
    cidade_selecionada <- if (input$cidade == "Todas") "todas as cidades" else paste("na cidade", input$cidade)
    ano_selecionado <- if (input$ano == "Todos") "todos os anos/ciclos" else paste("o ano", input$ano)
    paste("Esta visualização mostra o total de empreendedoras inscritas distribuidas em sectores,", cidade_selecionada, "e durante", ano_selecionado, "no projeto PAM_VERDE. Total:", total_inscritas())
  })
  
  # Output do número total de inscritas
  output$total_inscritas <- renderUI({
    h4(paste("Total Inscritas:", total_inscritas()))
  })
  
  # Output da legenda dinâmica
  output$legenda_dinamica <- renderUI({
    h4(legenda_dinamica())
  })
  
  
  # Função para download dos dados das inscritas em formato Excel
  output$downloadInscritas <- downloadHandler(
    filename = function() {
      paste("dados_inscritas_", Sys.Date(), ".xlsx", sep = "") # Nome do arquivo com a data atual
    },
    content = function(file) {
      # Escreve os dados das inscritas em um arquivo Excel
      write.xlsx(dados_filtrados(), file, rowNames = FALSE)
    }
  )
  
  # Botão de download
  downloadButton("downloadInscritas", "Baixar Dados de Inscritas")
  

 
  
#############################Distribuicão SETOR########################  
  
  
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
                position = position_stack(vjust = 0.5), size = 6) +
      labs(x = "Setor de Negócio", y = "Número de Inscritas", title = "Número de Inscritas por Setor de Negócio") +
      theme(panel.grid = element_blank(),
            axis.text.x = element_blank()) +  # Retirar rótulos dos setores abaixo das barras
      scale_y_continuous(labels = NULL) +  # Retirar valores do eixo vertical
      theme(legend.position = "right", legend.box = "vertical", legend.text = element_text(size = 11))  # Aumentar o tamanho das letras da legenda
  })
  
  
  ########################LUCRO MES########################################################
  
  output$grafico_lucro_mes <- renderPlot({
    dados_filtrados() %>%
      group_by(Periodo_Mes, Ano_Projeto) %>%
      summarise(Media_Lucro = mean(Lucro)) %>%
      mutate(Periodo_Mes = factor(Periodo_Mes, levels = c("Primeiro Mês", "Segundo Mês", "Terceiro Mês", "Quarto Mês", "Quinto Mês"))) %>%
      ggplot(aes(x = Periodo_Mes, y = Media_Lucro, color = Ano_Projeto, group = Ano_Projeto)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      geom_text(aes(label = round(Media_Lucro, 2)), vjust = -0.5, size = 4, color = "black") +  # Adiciona os valores dos pontos
      labs(x = "Mês", y = "Média de Lucro", title = "Média de Lucro por Mês VS Ciclo") +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 12),
            legend.position = "bottom", # Coloca a legenda na parte inferior
            legend.text = element_text(size = 12))  # Aumenta o tamanho da legenda
  })
  
  # Função para download dos dados de lucro em formato Excel
  output$downloadLucro <- downloadHandler(
    filename = function() {
      paste("dados_lucro_", Sys.Date(), ".xlsx", sep = "") # Nome do arquivo com a data atual
    },
    content = function(file) {
      # Escreve os dados de lucro em um arquivo Excel
      write.xlsx(dados_filtrados(), file, row.names = FALSE)
    }
  )
  
  downloadButton("downloadLucro", "Baixar Dados de Lucro")
  
  
  ########################LUCRO SECTORES######################### 
  output$grafico_barras <- renderPlot({ 
    # Filtro dos dados
    dados_filtrados <- dados_ficticios
    
    if (input$cidade != "Todas") {
      dados_filtrados <- dados_filtrados[dados_filtrados$Cidade == input$cidade, ]
    }
    
    # Filtro por ano do projeto
    if (input$ano != "Todos") {
      dados_filtrados <- dados_filtrados[dados_filtrados$Ano_Projeto == input$ano, ]
    }
    
    # Calcular a média por setor de negócio e ano do projeto
    media_por_setor_ano <- dados_filtrados %>%
      group_by(Sector_Negocio, Ano_Projeto) %>%
      summarise(Media_lucro = mean(Lucro, na.rm = TRUE)) %>%
      arrange(desc(Media_lucro))  # Ordenar os setores pela média de lucro em ordem decrescente
    
    # Criar o gráfico de barras empilhadas com cores diferentes para cada ano
    cores <- brewer.pal(n = length(unique(media_por_setor_ano$Ano_Projeto)), name = "Set3")  # Paleta de cores
    
    ggplot(media_por_setor_ano, aes(x = Media_lucro, y = reorder(Sector_Negocio, Media_lucro), fill = as.factor(Ano_Projeto))) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = round(Media_lucro, 2)), hjust = 1.5, size = 3.5, color = "black") +  # Aumenta o tamanho da letra para 6
      labs(x = "Média de Lucro", y = "Setor de Negócio", title = "Comparação da Média de Lucro por Setor de Negócio e Ano do Projeto") +
      theme_minimal() + 
      scale_fill_manual(values = ifelse(media_por_setor_ano$Media_lucro < 0, "red", cores), guide = guide_legend(title = "Ano do Projeto")) +
      theme(legend.position = "bottom", text = element_text(size = 12)) +  # Aumenta o tamanho da letra para 12
      geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Adiciona a barra vermelha para valores abaixo de zero
      theme(axis.text.y = element_text(size = 12))  # Aumenta o tamanho das letras do eixo y  
        })
  
  
  #############################GRAFICO DE METRICAS#########################################
 #################################SEMANAL#############################

  output$grafico_semanal <- renderPlot({
    dados_filtrados <- reactive({
      df <- Financeiro_Report
      if (input$cidade != "Todas") {
        df <- df[df$`Cidade de implementação` == input$cidade, ]
      }
      if (input$ano != "Todos") {
        df <- df[df$`Ano Ciclo` == as.character(input$ano), ]
      }
      if (input$nome != "" && input$nome != "Todos") {
        df <- df[df$`Nome empreendedoras` == input$nome, ]
      }
      if (input$Periodo != "Todos") {
        df <- df[df$Periodo == input$Periodo, ]
      }
      df$Semanas <- factor(df$Semanas, levels = c("Primeira Semana", "Segunda Semana", "Terceira Semana", "Quarta Semana", "Quinta Semana"))
      
      # Remodelar os dados
      df <- tidyr::gather(df, key = "Metrica", value = "Valor", Lucro_Semanal, `Custo Operacional`, Custo_Produto, Rendimento)
      
      df
    })
    
    ggplot(dados_filtrados(), aes(x = Semanas, y = Valor, fill = Metrica)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = Valor), position = position_dodge(width = 0.9), vjust = -0.5) + # Adicionando os valores
      labs(x = "Semanas", y = "Total", title = "Métricas Financeiras Semanais por Participante") +
      scale_fill_manual(name = "Métricas",
                        values = c("Lucro_Semanal" = "purple", 
                                   "Custo Operacional" = "blue", 
                                   "Custo_Produto" = "green", 
                                   "Rendimento" = "orange")) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      theme(axis.text.x = element_text(size = 12), 
            legend.text = element_text(size = 12)) 
  })
  
  



###################MENSAL############################# 
  
  output$grafico <- renderPlot({
    dados_filtrados <- reactive({
      df <- dados_ficticios
      if (input$cidade != "Todas") {
        df <- df[df$Cidade == input$cidade, ]
      }
      if (input$ano != "Todos") {
        df <- df[df$Ano_Projeto == as.character(input$ano), ]
      }
      if (input$nome != "" && input$nome != "Todos") {
        df <- df[df$Nome == input$nome, ]
      }
      df$Periodo_Mes <- factor(df$Periodo_Mes, levels = c("Primeiro Mês",
                                                          "Segundo Mês",
                                                          "Terceiro Mês",
                                                          "Quarto Mês",
                                                          "Quinto Mês"))
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
      geom_text(aes(y = Lucro, label = Lucro), vjust = -1.5, color = "#aa00ff", size = 5) +
      geom_text(aes(y = Custo_Operacional, label = Custo_Operacional), vjust = -1.5, color = "blue", size = 5) +
      geom_text(aes(y = Custo_Produto, label = Custo_Produto), vjust = 1.5, color = "#e30075", size = 5) +
      geom_text(aes(y = Rendimento, label = Rendimento), vjust = 1.5, color = "#480048", size = 5) +
      labs(x = "Mês", y = "Total", title = "Métricas Financeiras por Mês") + 
      scale_color_manual(name = "Variável",
                         values = c("Lucro" = "#aa00ff", "Custo Operacional" = "blue", "Custo Produto" = "#e30075", "Rendimento" = "#480048"),
                         labels = c("Custo Operacional", "Custo Produto", "Lucro", "Rendimento")) +
      theme_stata() +
      theme(legend.text = element_text(size = 13),
            axis.text.x = element_text(size = 13)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red")
  })
  
            ############PAGINA PEGADA ################
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
  
  # Definir a função para download do arquivo Excel
  output$downloadResumo <- downloadHandler(
    filename = function() {
      "Resumo_Por_Setor.xlsx"
    },
    content = function(file) {
      dados_resumo <- dados_pegadas %>%
        group_by(`SECTOR DA EMPREENDEDORA`) %>%
        summarise(Numero_de_Participantes = n_distinct(Nome))  # Conta o número total de participantes em cada setor
      
      # Escrever os dados no arquivo Excel
      write.xlsx(dados_resumo, file)
    }
  )
  
  
  ################Numero de Inscritas Por Sector#############################
  
 # Atualizar opções do filtro de ano
  observe({
    if (input$cidade_pegada != "Todas") {
      anos <- unique(dados_pegadas$ano_projeto[dados_pegadas$cidade == input$cidade_pegada])
    } else {
      anos <- unique(dados_pegadas$ano_projeto)
    }
    updateSelectInput(session, "ano_pegada", choices = c("Todos", sort(anos)))
  })
  
  # Atualizar opções do filtro de ciclo
  observe({
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
  
  # Função para gerar o gráfico
  output$grafico_setores <- renderPlot({
    # Subconjunto dos dados com base nos filtros selecionados para a Pegada de Carbono
    df <- dados_pegadas
    if (input$cidade_pegada != "Todas") {
      df <- df[df$cidade == input$cidade_pegada, ]
    }
    if (input$ano_pegada != "Todos") {
      df <- df[df$ano_projeto == as.numeric(input$ano_pegada), ]
    }
    if (input$ciclo_pegada != "Todos") {
      df <- df[df$ciclo == input$ciclo_pegada, ]
    }
    
    # Calcular valores e percentagens
    dados_resumo <- df %>%
      distinct(`Nome empreendedoras`, .keep_all = TRUE) %>%
      group_by(`SECTOR DA EMPREENDEDORA`) %>%
      summarise(n = n()) %>%
      mutate(Percentagem = n / sum(n) * 100)
    
    ggplot(dados_resumo, aes(x = reorder(`SECTOR DA EMPREENDEDORA`, -n), y = n, fill = `SECTOR DA EMPREENDEDORA`)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(n, " (", round(Percentagem, 1), "%)")), vjust = 1.5, size = 5) +
      labs(x = NULL, y = "Número de Inscritas", title = "Número de Inscritas por Setor da Empreendedora") +
      theme(panel.grid = element_blank(),
            axis.text.x = element_blank(),  # Remover rótulos do eixo x
            legend.position = "right", legend.box = "vertical", legend.text = element_text(size = 12))  # Aumentar o tamanho das letras da legenda
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
