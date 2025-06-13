library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readxl)
library(DT)
library(rmarkdown)

addResourcePath("www", ".")

# Função para criar valueBox
value_box <- function(title, value, icon_name) {
  valueBox(value = format(value, big.mark = ".", decimal.mark = ","), 
           subtitle = title, 
           icon = icon(icon_name), 
           color = "teal")
}

# Leitura da base de dados
dados <- read_excel("BD GERAL DE 01 DE MAI A 03 DE JUN DE 2025..xlsx")
names(dados)[names(dados) == "DATA DO FATO"] <- "DATA_DO_FATO"
dados$DATA_DO_FATO <- as.Date(dados$DATA_DO_FATO)

# Colunas
colunas_armas_industriais <- c(
  "QUANTIDADE DE REVOLVERES", "QUANTIDADE DE FUZIS", "QUANTIDADE DE ESPINGARDAS",
  "QUANTIDADE DE PISTOLAS", "QUANTIDADE DE CARABINA", "QUANTIDADE DE METRALHADORA"
)

colunas_veiculos_abordados <- c(
  "QUANTIDADE DE ABORDAGENS A TRANSEUNTES", "QUANTIDADE DE CICLISTAS ABORDADOS",
  "QUANTIDADE DE MOTOCICLISTAS ABORDADOS", "QUANTIDADE DE MOTORISTAS (AUTOMÓVEIS) ABORDADOS",
  "QUANTIDADE DE MOTORISTAS (VANS) ABORDADOS", "QUANTIDADE DE MOTORISTAS (CAMINHÕES) ABORDADOS",
  "QUANTIDADE DE ÔNIBUS AVERIGUADOS", "QUANTIDADE DE EMBARCAÇÕES AVERIGUADAS",
  "QUANTIDADE DE CASAS DE SHOW, BARES, RESTAURANTES, ACADEMIAS OU SIMILARES AVERIGUADOS (AMBIENTES SUJEITOS A AGLOMERAÇÃO)"
)

colunas_entorpecentes <- c(
  "QUANTIDADE DE MACONHA (em gramas)", "QUANTIDADE DE COCAÍNA (em gramas)",
  "QUANTIDADE DE CRACK (em gramas)", "QUANTIDADE DE OUTRAS DROGAS (em gramas)",
  "QUANTIDADE DE DROGAS SINTÉTICAS (em gramas)", "QUANTIDADE DE ECSTASY (em gramas)",
  "QUANTIDADE DE HAXIXE (em gramas)", "QUANTIDADE DE HEROÍNA (em gramas)"
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "DGO - PRODUTIVIDADE", titleWidth = 300,
                  tags$li(div(img(src = 'brasão_dourado_01.png', height = "35px"),
                              style = "padding-top:10px; padding-right:300px;"), class = "dropdown")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Histórico do DGO", tabName = "historico", icon = icon("book")),
      menuItem("GLOSSÁRIO", tabName = "glossario", icon = icon("book-open")),
      menuItem("MICRODADOS", tabName = "microdados", icon = icon("database")),
      menuItem("REAL TIME", tabName = "realtime", icon = icon("clock")),
      menuItem("ARMAS APREENDIDAS", icon = icon("shield"),
               menuSubItem("Gráfico", tabName = "grafico"),
               menuSubItem("Tabela", tabName = "tabela")),
      menuItem("RANKING", tabName = "ranking", icon = icon("trophy"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(".main-header .logo, .main-header .navbar { background-color: #004d61 !important; }"))),
    tabItems(
      tabItem("historico", box("HISTÓRICO DO DGO", width = 12, status = "primary", solidHeader = TRUE,
                               tags$iframe(src = "www/historico_dgo.html", width = "100%", height = "600px", frameborder = 0))),
      tabItem("glossario", h2("Glossário - Em construção")),
      tabItem("microdados", h2("Microdados - Em construção")),
      tabItem("ranking", h2("Ranking - Em construção")),
      
      # REAL TIME
      tabItem("realtime",
              fluidRow(
                column(6, dateRangeInput("filtro_data", "Data do Fato:",
                                         start = min(dados$DATA_DO_FATO, na.rm = TRUE),
                                         end = max(dados$DATA_DO_FATO, na.rm = TRUE),
                                         format = "dd/mm/yyyy", language = "pt")),
                column(6, selectInput("filtro_coint", "Selecione o COINT:",
                                      choices = c("Todos", unique(na.omit(dados$`INFORME O COINT`)))))
              ),
              fluidRow(
                downloadButton("exportar_pdf", "Exportar Indicadores (PDF)")
              ),
              fluidRow(
                valueBoxOutput("pessoas_abordadas"), valueBoxOutput("veiculos_abordados"), valueBoxOutput("entorpecentes")
              ),
              fluidRow(
                valueBoxOutput("adultos_presos"), valueBoxOutput("menores_apreendidos"), valueBoxOutput("foragidos_recapturados")
              ),
              fluidRow(
                valueBoxOutput("celulares_apreendidos"), valueBoxOutput("embarcacoes_abordadas"), valueBoxOutput("estabelecimentos_fiscalizados")
              ),
              fluidRow(
                valueBoxOutput("violencia_domestica"), valueBoxOutput("veiculos_apreendidos"), valueBoxOutput("presos_trafico")
              )
      ),
      
      # Armas
      tabItem("grafico", 
              dateRangeInput("data_input", "Intervalo de DATA DO FATO:",
                             start = min(dados$DATA_DO_FATO), end = max(dados$DATA_DO_FATO)),
              plotOutput("plot_coint")
      ),
      tabItem("tabela",
              dateRangeInput("data_input2", "Intervalo de DATA DO FATO:",
                             start = min(dados$DATA_DO_FATO), end = max(dados$DATA_DO_FATO)),
              DTOutput("tabela_dados"),
              downloadButton("download_tabela", "Baixar Tabela")
      )
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  dados_filtrados_rt <- reactive({
    df <- dados %>%
      filter(DATA_DO_FATO >= input$filtro_data[1], DATA_DO_FATO <= input$filtro_data[2])
    if (input$filtro_coint != "Todos") {
      df <- df %>% filter(`INFORME O COINT` == input$filtro_coint)
    }
    df %>%
      mutate(across(everything(), ~ suppressWarnings(as.numeric(.x)))) %>%
      mutate(
        VEICULOS_ABORDADOS = rowSums(across(all_of(colunas_veiculos_abordados)), na.rm = TRUE),
        ENTORPECENTES = rowSums(across(all_of(colunas_entorpecentes)), na.rm = TRUE)
      )
  })
  
  output$pessoas_abordadas <- renderValueBox({
    total <- sum(dados_filtrados_rt()[["QUANTIDADE DE ABORDAGENS A TRANSEUNTES"]], na.rm = TRUE)
    value_box("PESSOAS ABORDADAS", total, "users")
  })
  output$veiculos_abordados <- renderValueBox({
    total <- sum(dados_filtrados_rt()$VEICULOS_ABORDADOS, na.rm = TRUE)
    value_box("VEÍCULOS ABORDADOS", total, "car")
  })
  output$entorpecentes <- renderValueBox({
    total <- sum(dados_filtrados_rt()$ENTORPECENTES, na.rm = TRUE)
    value_box("ENTORPECENTES (g)", total, "cannabis")
  })
  output$adultos_presos <- renderValueBox({
    total <- sum(dados_filtrados_rt()[["QUANTIDADE DE ADULTOS PRESOS EM FLAGRANTE"]], na.rm = TRUE)
    value_box("ADULTOS PRESOS", total, "user-shield")
  })
  output$menores_apreendidos <- renderValueBox({
    total <- sum(dados_filtrados_rt()[["QUANTIDADE DE MENORES APREENDIDOS"]], na.rm = TRUE)
    value_box("MENORES APREENDIDOS", total, "child")
  })
  output$foragidos_recapturados <- renderValueBox({
    total <- sum(dados_filtrados_rt()[["QUANTIDADE DE FORAGIDOS RECAPTURADOS"]], na.rm = TRUE)
    value_box("FORAGIDOS RECAPTURADOS", total, "user-lock")
  })
  output$celulares_apreendidos <- renderValueBox({
    total <- sum(dados_filtrados_rt()[["QUANTIDADE CELULARES RECUPERADOS?"]], na.rm = TRUE)
    value_box("CELULARES APREENDIDOS", total, "mobile-alt")
  })
  output$embarcacoes_abordadas <- renderValueBox({
    total <- sum(dados_filtrados_rt()[["QUANTIDADE DE EMBARCAÇÕES AVERIGUADAS"]], na.rm = TRUE)
    value_box("EMBARCAÇÕES ABORDADAS", total, "ship")
  })
  output$estabelecimentos_fiscalizados <- renderValueBox({
    total <- sum(dados_filtrados_rt()[["QUANTIDADE DE CASAS DE SHOW, BARES, RESTAURANTES, ACADEMIAS OU SIMILARES AVERIGUADOS (AMBIENTES SUJEITOS A AGLOMERAÇÃO)"]], na.rm = TRUE)
    value_box("ESTABELECIMENTOS FISC.", total, "store")
  })
  output$violencia_domestica <- renderValueBox({
    total <- sum(dados_filtrados_rt()[["QUANTIDADE DE ATENDIMENTOS DE VIOLÊNCIA DOMÉSTICA (MARIA DA PENHA)"]], na.rm = TRUE)
    value_box("ATEND. VIOL. DOMEST.", total, "venus")
  })
  output$veiculos_apreendidos <- renderValueBox({
    total <- sum(dados_filtrados_rt()[["QUANTIDADE DE VEÍCULOS APREENDIDOS?"]], na.rm = TRUE)
    value_box("VEÍCULOS APREENDIDOS", total, "car-crash")
  })
  output$presos_trafico <- renderValueBox({
    total <- sum(dados_filtrados_rt()[["QUANTOS DOS ADULTOS PRESOS EM FLAGRANTE FORAM POR TRÁFICO DE DROGAS ?"]], na.rm = TRUE)
    value_box("PRESOS/TRÁFICO", total, "balance-scale")
  })
  
  # Exportar PDF
  output$exportar_pdf <- downloadHandler(
    filename = function() {
      paste0("indicadores_real_time_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      rmarkdown::render(
        input = "www/relatorio_real_time.Rmd",
        output_file = file,
        params = list(
          data = dados_filtrados_rt(),
          periodo = paste(format(input$filtro_data[1], "%d/%m/%Y"), "a", format(input$filtro_data[2], "%d/%m/%Y")),
          coint = input$filtro_coint
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  # Plot e Tabela
  dados_armas <- reactive({
    dados %>% filter(DATA_DO_FATO >= input$data_input[1], DATA_DO_FATO <= input$data_input[2]) %>%
      mutate(across(all_of(c(colunas_armas_industriais, "QUANTIDADE DE ARMAS DE FOGO DE ORIGEM CASEIRA",
                             "QUANTIDADE DE ARMA BRANCA", "QUANTIDADE DE SIMULACRO")), as.numeric)) %>%
      mutate(ARMAS_INDUSTRIAIS = rowSums(across(all_of(colunas_armas_industriais)), na.rm = TRUE))
  })
  
  output$plot_coint <- renderPlot({
    df <- dados_armas() %>%
      group_by(`INFORME O COINT`) %>%
      summarise(Quantidade = sum(ARMAS_INDUSTRIAIS, na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(`INFORME O COINT`))
    ggplot(df, aes(x = reorder(`INFORME O COINT`, Quantidade), y = Quantidade)) +
      geom_col(fill = "steelblue") +
      geom_text(aes(label = Quantidade), hjust = -0.1, size = 4) +
      coord_flip() + labs(x = "COINT", y = "Quantidade", title = "Comparativo de Armas Apreendidas por COINT") +
      theme_minimal()
  })
  
  dados_armas_tab <- reactive({
    dados %>% filter(DATA_DO_FATO >= input$data_input2[1], DATA_DO_FATO <= input$data_input2[2]) %>%
      mutate(across(all_of(c(colunas_armas_industriais, "QUANTIDADE DE ARMAS DE FOGO DE ORIGEM CASEIRA",
                             "QUANTIDADE DE ARMA BRANCA", "QUANTIDADE DE SIMULACRO")), as.numeric)) %>%
      mutate(ARMAS_INDUSTRIAIS = rowSums(across(all_of(colunas_armas_industriais)), na.rm = TRUE))
  })
  
  output$tabela_dados <- renderDT({
    df <- dados_armas_tab() %>%
      group_by(`INFORME O COINT`) %>%
      summarise(`ARMAS INDUSTRIAIS` = sum(ARMAS_INDUSTRIAIS, na.rm = TRUE),
                `ARMAS CASEIRAS` = sum(`QUANTIDADE DE ARMAS DE FOGO DE ORIGEM CASEIRA`, na.rm = TRUE),
                `ARMAS BRANCAS` = sum(`QUANTIDADE DE ARMA BRANCA`, na.rm = TRUE),
                `SIMULACROS` = sum(`QUANTIDADE DE SIMULACRO`, na.rm = TRUE), .groups = "drop")
    datatable(df)
  })
  
  output$download_tabela <- downloadHandler(
    filename = function() {
      paste0("tabela_armas_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- dados_armas_tab() %>%
        group_by(`INFORME O COINT`) %>%
        summarise(`ARMAS INDUSTRIAIS` = sum(ARMAS_INDUSTRIAIS, na.rm = TRUE),
                  `ARMAS CASEIRAS` = sum(`QUANTIDADE DE ARMAS DE FOGO DE ORIGEM CASEIRA`, na.rm = TRUE),
                  `ARMAS BRANCAS` = sum(`QUANTIDADE DE ARMA BRANCA`, na.rm = TRUE),
                  `SIMULACROS` = sum(`QUANTIDADE DE SIMULACRO`, na.rm = TRUE), .groups = "drop")
      write.csv(df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
