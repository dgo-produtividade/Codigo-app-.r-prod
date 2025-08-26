# Carregamento de pacotes
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readxl)
library(DT)
library(rmarkdown)
library(flexdashboard)
library(DiagrammeR)
library(visNetwork)
library(webshot2)
library("htmlwidgets")
library('rsconnect')



addResourcePath("www", ".")

# Função para criar valueBox
# Função para criar valueBox
value_box <- function(title, value, icon_name, color = "teal") {
  shinydashboard::valueBox(
    value = format(value, big.mark = ".", decimal.mark = ","),
    subtitle = title,
    icon = icon(icon_name),
    color = color
  )
}


# Leitura da base de dados
dados <- read_excel("BD_GERAL_2025.xlsx")
names(dados)[names(dados) == "DATA DO FATO"] <- "DATA_DO_FATO"
dados$DATA_DO_FATO <- as.Date(dados$DATA_DO_FATO)

# Detecta automaticamente colunas com nome similar a "QUAL O BPM"
colunas_opm <- grep("^QUAL O BPM.*\\?", names(dados), value = TRUE)

# Cria coluna unificada OPM
dados$OPM <- apply(dados[, colunas_opm], 1, function(x) {
  x_valid <- na.omit(trimws(as.character(x)))
  if (length(x_valid) > 0) x_valid[1] else NA
})

# Colunas de somatórios
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

glossario <- data.frame(
  Termo = c("OPM", "COINT", "Entorpecentes", "Apreensão", "Flagrante"),
  Definicao = c(
    "Organização Policial Militar responsável pela área.",
    "Comando de Operações Integradas da Polícia.",
    "Substâncias proibidas pela lei, como drogas ilícitas.",
    "Ato de apreender armas, veículos ou drogas.",
    "Situação em que o indivíduo é preso no momento do crime."
  ),
  stringsAsFactors = FALSE
)


# Interface
ui <- dashboardPage(
  dashboardHeader(
    title = "DGO - PRODUTIVIDADE",
    titleWidth = 300,
    tags$li(
      div(
        img(src = 'brasão_dourado_01.png', height = "35px"),
        style = "padding-top:10px; padding-right:300px;"
      ), class = "dropdown"
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Histórico do DGO", tabName = "historico", icon = icon("book")),
      menuItem("ORGANOGRAMA", tabName = "organograma", icon = icon("sitemap")),
      menuItem("GLOSSÁRIO", tabName = "glossario", icon = icon("book-open")),
      menuItem("MICRODADOS", tabName = "microdados", icon = icon("database")),
      menuItem("REAL TIME", tabName = "realtime", icon = icon("clock")),
      menuItem("APREENSÕES", icon = icon("shield"),
               menuSubItem("RANKING", tabName = "grafico"),
               menuSubItem("ARMAS POR TIPO", tabName = "tabela")
      ),
      tags$li(
        div(
          img(src = 'pmpa_01.png', height = "100px"),
          style = "height: 250px; display: flex; justify-content: center; align-items: center; padding-top: 20px;"
        ),
        class = "dropdown"
      ),
      tags$li(
        div(
          img(src = 'dgo_01.png', height = "110px"),
          style = "height: 200px; display: flex; justify-content: center; align-items: center; padding-top: 20px;"
        ),
        class = "dropdown"
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML(".main-header .logo, .main-header .navbar { background-color: #004d61 !important; }"))
    ),
    tabItems(
      tabItem(
        tabName = "historico",
        fluidRow(
          column(
            width = 3,
            tags$img(src = "getulio (1).png", width = "100%", style = "border-radius: 10px; margin-bottom: 5px;"),
            tags$p(
              "GETÚLIO CÂNDIDO ROCHA JÚNIOR – CEL QOPM RG 26316",
              style = "text-align: center; font-weight: bold; font-size: 12px; margin-bottom: 0; word-wrap: break-word; white-space: normal;"
            ),
            tags$p(
              "Chefe do Departamento Geral de Operações",
              style = "text-align: center; font-size: 14px; margin-top: 0; word-wrap: break-word; white-space: normal;"
            )
          ),
          column(
            width = 9,
            box(
              "HISTÓRICO DO DGO", width = 12, status = "primary", solidHeader = TRUE,
              tags$iframe(src = "www/historico_dgo_sn.html", width = "100%", height = "600px", frameborder = 0)
            )
          )
        )
      ),
      tabItem(
        tabName = "glossario",
        box(
          title = "Glossário de Termos", width = 12, status = "primary", solidHeader = TRUE,
          DT::dataTableOutput("tabela_glossario")
        )
      ),
      tabItem(tabName = "microdados", h2("Microdados - Em construção")),
      tabItem(
        tabName = "organograma",
        box(
          title = "Organograma de Funções DGO",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          visNetworkOutput("grafico_organograma", height = "calc(100vh - 180px)"),
          br(),
          downloadButton("exportar_pdf_organograma", "Exportar Organograma PDF")
        )
      )
      ,
      
      
      # Aba RANKING (tabName = "grafico")
      tabItem(
        tabName = "grafico",
        
        fluidRow(
          column(6,
                 dateRangeInput(
                   "data_input", "Selecione o intervalo de DATA DO FATO:",
                   start = min(dados$DATA_DO_FATO, na.rm = TRUE),
                   end = max(dados$DATA_DO_FATO, na.rm = TRUE),
                   format = "dd/mm/yyyy", language = "pt"
                 ),
                 h4(textOutput("intervalo_data_ranking"))
          ),
          column(6,
                 selectInput(
                   "filtro_indicador", "Indicador:",
                   choices = c(
                     "Pessoas Abordadas", "Veículos Abordados", "Entorpecentes (g)",
                     "Adultos Presos", "Menores Apreendidos", "Foragidos Recapturados",
                     "Celulares Apreendidos", "Embarcações Abordadas", "Estabelecimentos Fiscalizados",
                     "Atend. Violência Doméstica", "Veículos Apreendidos", "Presos por Tráfico",
                     "Armas Apreendidas"
                   )
                 )
          )
        ),
        
        fluidRow(
          column(6, style = "padding-right: 0px;",
                 box(
                   title = "Indicador por COINT", width = 12, status = "primary", solidHeader = TRUE,
                   plotOutput("plot_coint", height = "500px")
                 )
          ),
          column(6, style = "padding-left: 0px;",
                 box(
                   title = "Indicador por OPM", width = 12, status = "primary", solidHeader = TRUE,
                   div(
                     style = "overflow-y: auto; height: 500px;",
                     plotOutput("plot_opm", height = "1000px")
                   )
                 )
          )
        )
        
      ),
      
      
      tabItem(
        tabName = "tabela",  # Aba ARMAS POR TIPO
        fluidRow(
          column(4,
                 dateRangeInput(
                   "data_input2", "Intervalo de DATA DO FATO:",
                   start = min(dados$DATA_DO_FATO, na.rm = TRUE),
                   end = max(dados$DATA_DO_FATO, na.rm = TRUE),
                   format = "dd/mm/yyyy", language = "pt"
                 )
          )
        ),
        fluidRow(
          column(12,
                 h4(textOutput("intervalo_data_tabela"))
          )
        ),
        fluidRow(
          column(6,
                 box(
                   title = "Armas Apreendidas por COINT", width = 12, status = "primary", solidHeader = TRUE,
                   DTOutput("tabela_dados"),
                   downloadButton("download_tabela", "Baixar por COINT")
                 )
          ),
          column(6,
                 box(
                   title = "Armas Apreendidas por OPM", width = 12, status = "primary", solidHeader = TRUE,
                   DTOutput("tabela_opm"),
                   downloadButton("download_opm", "Baixar por OPM")
                 )
          )
        )
      ),
      
      # Aba REAL TIME (tabName = "realtime")
      
      #caixas
      
      tabItem(
        tabName = "realtime",
        fluidRow(
          column(4,
                 dateRangeInput("filtro_data", "Data do Fato:",
                                start = min(dados$DATA_DO_FATO, na.rm = TRUE),
                                end = max(dados$DATA_DO_FATO, na.rm = TRUE),
                                format = "dd/mm/yyyy", language = "pt")
          ),
          column(4,
                 selectInput("filtro_coint", "Selecione o COINT:",
                             choices = c("Todos", sort(unique(na.omit(dados$`INFORME O COINT`)))))
          ),
          column(4,
                 selectInput("filtro_opm", "Qual a sua OPM:",
                             choices = c("Todas", sort(unique(na.omit(dados$OPM)))))
          )
        ),
        fluidRow(
          valueBoxOutput("pessoas_abordadas"),
          valueBoxOutput("veiculos_abordados"),
          valueBoxOutput("entorpecentes")
        ),
        fluidRow(
          valueBoxOutput("mandados_de_prisão"),
          valueBoxOutput("menores_apreendidos"),
          valueBoxOutput("foragidos_recapturados")
        ),
        fluidRow(
          valueBoxOutput("celulares_apreendidos"),
          valueBoxOutput("embarcacoes_abordadas"),
          valueBoxOutput("estabelecimentos_fiscalizados")
        ),
        fluidRow(
          valueBoxOutput("violencia_domestica"),
          valueBoxOutput("veiculos_apreendidos"),
          valueBoxOutput("presos_trafico")
        ),
        
        #velocimetros
        
        fluidRow(
          column(3,
                 tags$div(
                   style = "position: relative; margin-bottom: -20px;",
                   gaugeOutput("velocimetro_armas", height = "300px"),
                   HTML("<div style='
             position: absolute;
             top: 110px;
             left: 50%;
             transform: translateX(-50%);
             text-align: center;'>
             <span style='font-size: 20px; font-weight: bold; font-style: italic;'>ARMAS APREENDIDAS</span><br>
             <span style='font-size: 12px; color: navy;'>Industrial, caseira e simulacro</span>
           </div>")
                 )
          ),
          column(3,
                 tags$div(
                   style = "position: relative; margin-bottom: -20px;",
                   gaugeOutput("velocimetro_entorpecentes", height = "300px"),
                   HTML("<div style='
             position: absolute;
             top: 110px;
             left: 50%;
             transform: translateX(-50%);
             text-align: center;'>
             <span style='font-size: 20px; font-weight: bold; font-style: italic;'>ENTORPECENTES</span><br>
             <span style='font-size: 12px; color: navy;'>Maconha, cocaína, crack,sintéticas, ecstasy, 
             haxixe, heroína, outras.</span>
           </div>")
                 )
          ),
          column(3,
                 tags$div(
                   style = "position: relative; margin-bottom: -20px;",
                   gaugeOutput("velocimetro_adultos", height = "300px"),
                   HTML("<div style='
             position: absolute;
             top: 110px;
             left: 50%;
             transform: translateX(-50%);
             text-align: center;'>
             <span style='font-size: 20px; font-weight: bold; font-style: italic;'>ADULTOS PRESOS</span><br>
             <span style='font-size: 12px; color: navy;'>Prisões em flagrante</span>
           </div>")
                 )
          ),
          column(3,
                 tags$div(
                   style = "position: relative; margin-bottom: -20px;",
                   gaugeOutput("velocimetro_valor", height = "300px"),
                   HTML("<div style='
             position: absolute;
             top: 110px;
             left: 50%;
             transform: translateX(-50%);
             text-align: center;'>
             <span style='font-size: 20px; font-weight: bold; font-style: italic;'>VALOR DEVOLVIDO</span><br>
             <span style='font-size: 12px; color: navy;'>Carros e motos apreendidos/recuperados</span>
           </div>")
                 )
          )
        )
        ,
        
        
        
        fluidRow(
          column(12,
                 h4(textOutput("intervalo_data_real_time"))  # texto do intervalo aqui
          )
        ),
        fluidRow(
          column(12,
                 downloadButton("exportar_pdf", "Exportar Indicadores (PDF)")  # botão movido para cá
          )
        )
      )
      
    )
  )
  
  
  
)


# Server
server <- function(input, output, session) {
  
  dados_filtrados_rt <- reactive({
    df <- dados %>%
      filter(DATA_DO_FATO >= input$filtro_data[1],
             DATA_DO_FATO <= input$filtro_data[2])
    
    if (input$filtro_coint != "Todos") {
      df <- df %>% filter(`INFORME O COINT` == input$filtro_coint)
    }
    if (input$filtro_opm != "Todas") {
      df <- df %>% filter(OPM == input$filtro_opm)
    }
    
    df %>%
      mutate(across(everything(), ~ suppressWarnings(as.numeric(.x)))) %>%
      mutate(
        VEICULOS_ABORDADOS = rowSums(across(all_of(colunas_veiculos_abordados)), na.rm = TRUE),
        ENTORPECENTES = rowSums(across(all_of(colunas_entorpecentes)), na.rm = TRUE)
      )
  })
  output$intervalo_data_tabela <- renderText({
    paste0("Intervalo selecionado: ", 
           format(input$data_input2[1], "%d/%m/%Y"), " a ", 
           format(input$data_input2[2], "%d/%m/%Y"))
  })
  output$intervalo_data_ranking <- renderText({
    paste0("Intervalo selecionado: ", 
           format(input$data_input[1], "%d/%m/%Y"), " a ", 
           format(input$data_input[2], "%d/%m/%Y"))
  })
  
  output$intervalo_data_real_time <- renderText({
    paste0("Intervalo selecionado: ", 
           format(input$filtro_data[1], "%d/%m/%Y"), " a ", 
           format(input$filtro_data[2], "%d/%m/%Y"))
  })
  
  output$tabela_glossario <- DT::renderDataTable({
    DT::datatable(
      glossario,
      options = list(pageLength = 10, searching = TRUE, lengthChange = FALSE),
      rownames = FALSE,
      colnames = c("Termo", "Definição")
    )
  })
  
  
  # valueBoxes
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
  output$mandados_de_prisão <- renderValueBox({
    total <- sum(dados_filtrados_rt()[["QUANTIDADE DE MANDADOS DE PRISÃO?"]], na.rm = TRUE)
    value_box("MANDADOS DE PRISÃO", total, "user-shield")
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
    total <- sum(dados_filtrados_rt()[["QUANTIDADE DE ATENDIMENTOS DE VIOLÊNCIA DOMÉSTICA  (MARIA DA PENHA)"]], na.rm = TRUE)
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
  output$velocimetro_armas <- renderGauge({
    colunas <- c(
      "QUANTIDADE DE REVOLVERES", "QUANTIDADE DE FUZIS", "QUANTIDADE DE ESPINGARDAS",
      "QUANTIDADE DE PISTOLAS", "QUANTIDADE DE CARABINA", "QUANTIDADE DE METRALHADORA",
      "QUANTIDADE DE ARMAS DE FOGO DE ORIGEM CASEIRA","QUANTIDADE DE SIMULACRO"
    )
    
    total_armas <- dados_filtrados_rt() %>%
      mutate(TOTAL_ARMAS = rowSums(across(all_of(colunas)), na.rm = TRUE)) %>%
      summarise(total = sum(TOTAL_ARMAS, na.rm = TRUE)) %>%
      pull(total)
    
    gauge(
      value = total_armas,
      min = 0,
      max = max(5000, total_armas * 1.1),
      symbol = "",
      label = paste(format(round(total_armas, 0), big.mark = ".", decimal.mark = ","), "armas"),
      gaugeSectors(success = c(0, 1500), warning = c(1500, 3000), danger = c(3000, 5000))
    )
  })
  
  
  output$velocimetro_entorpecentes <- renderGauge({
    colunas_drogas <- c(
      "QUANTIDADE DE MACONHA (em gramas)", "QUANTIDADE DE COCAÍNA (em gramas)",
      "QUANTIDADE DE CRACK (em gramas)", "QUANTIDADE DE OUTRAS DROGAS (em gramas)",
      "QUANTIDADE DE DROGAS SINTÉTICAS (em gramas)", "QUANTIDADE DE ECSTASY (em gramas)",
      "QUANTIDADE DE HAXIXE (em gramas)", "QUANTIDADE DE HEROÍNA (em gramas)"
    )
    
    total_drogas <- dados_filtrados_rt() %>%
      mutate(TOTAL_ENTORPECENTES = rowSums(across(all_of(colunas_drogas)), na.rm = TRUE)) %>%
      summarise(total = sum(TOTAL_ENTORPECENTES, na.rm = TRUE)) %>%
      pull(total)
    
    gauge(
      value = total_drogas,
      min = 0,
      max = 10000000,
      symbol = "",
      label = paste(format(round(total_drogas, 0), big.mark = ".", decimal.mark = ","), "gramas"),
      gaugeSectors(success = c(0, 3000000), warning = c(3000000, 7000000), danger = c(7000000, 10000000))
    )
  })
  
  
  output$velocimetro_adultos <- renderGauge({
    total_adultos <- dados_filtrados_rt() %>%
      summarise(total = sum(`QUANTIDADE DE ADULTOS PRESOS EM FLAGRANTE`, na.rm = TRUE)) %>%
      pull(total)
    
    gauge(
      value = total_adultos,
      min = 0,
      max = max(15000, total_adultos * 1.1),
      symbol = "",
      label = paste(format(round(total_adultos, 0), big.mark = ".", decimal.mark = ","), "pessoas"),
      gaugeSectors(success = c(0, 5000), warning = c(5000, 10000), danger = c(10000, 15000))
    )
  })
  
  
  output$velocimetro_valor <- renderGauge({
    total_valor <- dados_filtrados_rt() %>%
      mutate(VALOR_TOTAL = rowSums(across(c("VALOR MOTO", "VALOR CARRO")), na.rm = TRUE)) %>%
      summarise(total = sum(VALOR_TOTAL, na.rm = TRUE)) %>%
      pull(total)
    
    gauge(
      value = total_valor,
      min = 0,
      max = max(1000000, total_valor * 1.2),
      symbol = "",
      label = paste("R$", format(round(total_valor, 0), big.mark = ".", decimal.mark = ",")),
      gaugeSectors(success = c(0, 300000), warning = c(300000, 700000), danger = c(700000, 1000000))
    )
  })
  
  output$grafico_organograma <- renderVisNetwork({
    nodes <- data.frame(
      id = 1:27,
      label = c(
        "DGO\nCEL GETÚLIO",                      # 1
        "1️⃣ Assistente\nTCEL SULLIVAN",         # 2
        "2️⃣ Seção Preventivo\nMAJ FÉLIX",       # 3
        "3️⃣ Seção Repressivo\nMAJ CHARLLENY",   # 4
        
        "CAP MOURA\nPlanejamento",               # 5
        "CB SIQUEIRA", "SD GONÇALVES", "SD MARIA", "SD B. FARIAS",  # 6–9
        
        "CAP ELIAQUIM\nAvaliação Prev.",         # 10
        "3º SGT ROGÉRIO", "CB RAMOM", "CB GISELY", "SD DENISON",    # 11–14
        
        "CAP CLEBERSOM\nAvaliação Repr.",        # 15
        "TEN KATIA",                              # 16
        
        "CAP XXXXX\nPlanejamento",               # 17
        "2º SGT DARLENE", "3º SGT CEZAR", "3º SGT JACIANE", "3º SGT HUGO",  # 18–21
        "CB PAULA", "CB LAISSE", "CB EVELLY",     # 22–24
        "SD WENDEL", "SD CAIO NEVES", "SD HACKENHAAR"  # 25–27
      ),
      group = c(
        "comando", "comando",
        "preventivo", "repressivo",
        
        rep("oficiais_prev", 1), rep("praças_prev", 4),
        rep("oficiais_prev", 1), rep("praças_prev", 4),
        
        rep("oficiais_repr", 2),
        rep("oficiais_repr", 1),
        rep("praças_repr", 10)
      ),
      level = c(
        0, 1,        # GETÚLIO, SULLIVAN
        2, 2,        # FÉLIX, CHARLLENY
        3, 5, 5, 5, 5,  # MOURA, subordinados em nível 5
        3, 5, 5, 5, 5,  # ELIAQUIM, subordinados em nível 5
        3, 3,           # CLEBERSON e KATIA
        3,              # CAP XXXXX
        rep(6, 10)      # subordinados do repressivo → nível 6 (mais à direita)
      ),
      font.size = 18,
      shadow = TRUE
    )
    
    edges <- data.frame(
      from = c(
        1,
        2, 2,
        3, 3,
        5, 5, 5, 5,
        10, 10, 10, 10,
        4, 4,
        4,
        rep(17, 10)
      ),
      to = c(
        2,
        3, 4,
        5, 10,
        6, 7, 8, 9,
        11, 12, 13, 14,
        15, 16,
        17,
        18:27
      )
    )
    
    visNetwork(nodes, edges, width = "100%", height = "700px") %>%
      visNodes(shape = "box", borderWidth = 1) %>%
      visEdges(arrows = "to", smooth = list(enabled = TRUE, type = "dynamic")) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE) %>%
      visHierarchicalLayout(
        direction = "LR",
        sortMethod = "directed",
        levelSeparation = 300,
        nodeSpacing = 200,
        treeSpacing = 300
      ) %>%
      visGroups(groupname = "comando", color = list(background = "#FFCDD2", border = "#C62828")) %>%
      visGroups(groupname = "preventivo", color = list(background = "#E3F2FD", border = "#1565C0")) %>%
      visGroups(groupname = "repressivo", color = list(background = "#FFF3E0", border = "#EF6C00")) %>%
      visGroups(groupname = "oficiais_prev", color = list(background = "#E8F5E9", border = "#43A047")) %>%
      visGroups(groupname = "praças_prev", color = list(background = "#F1F8E9", border = "#33691E")) %>%
      visGroups(groupname = "oficiais_repr", color = list(background = "#F3E5F5", border = "#8E24AA")) %>%
      visGroups(groupname = "praças_repr", color = list(background = "#EDE7F6", border = "#6A1B9A"))
  })
  
  
  
  output$exportar_pdf_organograma <- downloadHandler(
    filename = function() {
      paste0("organograma_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      library(htmlwidgets)
      library(webshot2)
      
      nodes <- data.frame(
        id = 1:27,
        label = c(
          "DGO\nCEL GETÚLIO",                      # 1
          "1️⃣ Assistente\nTCEL SULLIVAN",         # 2
          "2️⃣ Seção Preventivo\nMAJ FÉLIX",       # 3
          "3️⃣ Seção Repressivo\nMAJ CHARLLENY",   # 4
          
          "CAP MOURA\nPlanejamento",               # 5
          "CB SIQUEIRA", "SD GONÇALVES", "SD MARIA", "SD B. FARIAS",  # 6–9
          
          "CAP ELIAQUIM\nAvaliação Prev.",         # 10
          "3º SGT ROGÉRIO", "CB RAMOM", "CB GISELY", "SD DENISON",    # 11–14
          
          "CAP CLEBERSOM\nAvaliação Repr.",        # 15
          "TEN KATIA",                              # 16
          
          "CAP XXXXX\nPlanejamento",               # 17
          "2º SGT DARLENE", "3º SGT CEZAR", "3º SGT JACIANE", "3º SGT HUGO",  # 18–21
          "CB PAULA", "CB LAISSE", "CB EVELLY",     # 22–24
          "SD WENDEL", "SD CAIO NEVES", "SD HACKENHAAR"  # 25–27
        ),
        group = c(
          "comando", "comando",
          "preventivo", "repressivo",
          rep("oficiais_prev", 1), rep("praças_prev", 4),
          rep("oficiais_prev", 1), rep("praças_prev", 4),
          rep("oficiais_repr", 2),
          rep("oficiais_repr", 1),
          rep("praças_repr", 10)
        ),
        level = c(
          0, 1,
          2, 2,
          3, 5, 5, 5, 5,
          3, 5, 5, 5, 5,
          3, 3,
          3,
          rep(6, 10)
        ),
        font.size = 18,
        shadow = TRUE
      )
      
      edges <- data.frame(
        from = c(
          1,
          2, 2,
          3, 3,
          5, 5, 5, 5,
          10, 10, 10, 10,
          4, 4,
          4,
          rep(17, 10)
        ),
        to = c(
          2,
          3, 4,
          5, 10,
          6, 7, 8, 9,
          11, 12, 13, 14,
          15, 16,
          17,
          18:27
        )
      )
      
      graph <- visNetwork(nodes, edges, width = "100%", height = "700px") %>%
        visNodes(shape = "box", borderWidth = 1) %>%
        visEdges(arrows = "to", smooth = list(enabled = TRUE, type = "dynamic")) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE) %>%
        visHierarchicalLayout(
          direction = "LR",
          sortMethod = "directed",
          levelSeparation = 300,
          nodeSpacing = 200,
          treeSpacing = 300
        ) %>%
        visGroups(groupname = "comando", color = list(background = "#FFCDD2", border = "#C62828")) %>%
        visGroups(groupname = "preventivo", color = list(background = "#E3F2FD", border = "#1565C0")) %>%
        visGroups(groupname = "repressivo", color = list(background = "#FFF3E0", border = "#EF6C00")) %>%
        visGroups(groupname = "oficiais_prev", color = list(background = "#E8F5E9", border = "#43A047")) %>%
        visGroups(groupname = "praças_prev", color = list(background = "#F1F8E9", border = "#33691E")) %>%
        visGroups(groupname = "oficiais_repr", color = list(background = "#F3E5F5", border = "#8E24AA")) %>%
        visGroups(groupname = "praças_repr", color = list(background = "#EDE7F6", border = "#6A1B9A"))
      
      temp_html <- tempfile(fileext = ".html")
      saveWidget(graph, temp_html, selfcontained = TRUE)
      
      webshot2::webshot(temp_html, file = file, vwidth = 1400, vheight = 800)
    }
  )
  
  
  
  
  
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
          coint = input$filtro_coint,
          opm = input$filtro_opm
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  #Filtro "Indicador" funcional na aba 'RANKING'
  
  output$plot_coint <- renderPlot({
    dados_filtrados <- dados %>%
      filter(DATA_DO_FATO >= input$data_input[1],
             DATA_DO_FATO <= input$data_input[2]) %>%
      mutate(
        VEICULOS_ABORDADOS = rowSums(across(all_of(colunas_veiculos_abordados)), na.rm = TRUE),
        ENTORPECENTES = rowSums(across(all_of(colunas_entorpecentes)), na.rm = TRUE),
        ARMAS_APREENDIDAS = rowSums(across(all_of(colunas_armas_industriais)), na.rm = TRUE)
      )
    
    
    indicador_coluna <- switch(input$filtro_indicador,
                               "Pessoas Abordadas" = "QUANTIDADE DE ABORDAGENS A TRANSEUNTES",
                               "Veículos Abordados" = "VEICULOS_ABORDADOS",
                               "Entorpecentes (g)" = "ENTORPECENTES",
                               "Adultos Presos" = "QUANTIDADE DE ADULTOS PRESOS EM FLAGRANTE",
                               "Menores Apreendidos" = "QUANTIDADE DE MENORES APREENDIDOS",
                               "Foragidos Recapturados" = "QUANTIDADE DE FORAGIDOS RECAPTURADOS",
                               "Celulares Apreendidos" = "QUANTIDADE CELULARES RECUPERADOS?",
                               "Embarcações Abordadas" = "QUANTIDADE DE EMBARCAÇÕES AVERIGUADAS",
                               "Estabelecimentos Fiscalizados" = "QUANTIDADE DE CASAS DE SHOW, BARES, RESTAURANTES, ACADEMIAS OU SIMILARES AVERIGUADOS (AMBIENTES SUJEITOS A AGLOMERAÇÃO)",
                               "Atend. Violência Doméstica" = "QUANTIDADE DE ATENDIMENTOS DE VIOLÊNCIA DOMÉSTICA  (MARIA DA PENHA)",
                               "Veículos Apreendidos" = "QUANTIDADE DE VEÍCULOS APREENDIDOS?",
                               "Presos por Tráfico" = "QUANTOS DOS ADULTOS PRESOS EM FLAGRANTE FORAM POR TRÁFICO DE DROGAS ?",
                               "Armas Apreendidas" = "ARMAS_APREENDIDAS"
                               
    )
    
    dados_plot <- dados_filtrados %>%
      group_by(`INFORME O COINT`) %>%
      summarise(TOTAL = sum(.data[[indicador_coluna]], na.rm = TRUE)) %>%
      arrange(desc(TOTAL))
    
    ggplot(dados_plot, aes(x = reorder(`INFORME O COINT`, TOTAL), y = TOTAL)) +
      geom_col(fill = "#46BFB1") +
      geom_text(aes(label = format(TOTAL, big.mark = ".", decimal.mark = ",")),
                hjust = -0.1, size = 5) +
      coord_flip() +
      labs(title = paste("Ranking:", input$filtro_indicador),
           x = "COINT", y = "Total") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold")) +
      expand_limits(y = max(dados_plot$TOTAL) * 1.1)
    
  })
  
  output$plot_opm <- renderPlot({
    dados_filtrados <- dados %>%
      filter(DATA_DO_FATO >= input$data_input[1],
             DATA_DO_FATO <= input$data_input[2]) %>%
      mutate(
        VEICULOS_ABORDADOS = rowSums(across(all_of(colunas_veiculos_abordados)), na.rm = TRUE),
        ENTORPECENTES = rowSums(across(all_of(colunas_entorpecentes)), na.rm = TRUE),
        ARMAS_APREENDIDAS = rowSums(across(all_of(colunas_armas_industriais)), na.rm = TRUE)
      )
    
    indicador_coluna <- switch(input$filtro_indicador,
                               "Pessoas Abordadas" = "QUANTIDADE DE ABORDAGENS A TRANSEUNTES",
                               "Veículos Abordados" = "VEICULOS_ABORDADOS",
                               "Entorpecentes (g)" = "ENTORPECENTES",
                               "Adultos Presos" = "QUANTIDADE DE ADULTOS PRESOS EM FLAGRANTE",
                               "Menores Apreendidos" = "QUANTIDADE DE MENORES APREENDIDOS",
                               "Foragidos Recapturados" = "QUANTIDADE DE FORAGIDOS RECAPTURADOS",
                               "Celulares Apreendidos" = "QUANTIDADE CELULARES RECUPERADOS?",
                               "Embarcações Abordadas" = "QUANTIDADE DE EMBARCAÇÕES AVERIGUADAS",
                               "Estabelecimentos Fiscalizados" = "QUANTIDADE DE CASAS DE SHOW, BARES, RESTAURANTES, ACADEMIAS OU SIMILARES AVERIGUADOS (AMBIENTES SUJEITOS A AGLOMERAÇÃO)",
                               "Atend. Violência Doméstica" = "QUANTIDADE DE ATENDIMENTOS DE VIOLÊNCIA DOMÉSTICA  (MARIA DA PENHA)",
                               "Veículos Apreendidos" = "QUANTIDADE DE VEÍCULOS APREENDIDOS?",
                               "Presos por Tráfico" = "QUANTOS DOS ADULTOS PRESOS EM FLAGRANTE FORAM POR TRÁFICO DE DROGAS ?",
                               "Armas Apreendidas" = "ARMAS_APREENDIDAS"
    )
    
    dados_plot <- dados_filtrados %>%
      filter(!is.na(OPM)) %>%
      group_by(OPM) %>%
      summarise(TOTAL = sum(.data[[indicador_coluna]], na.rm = TRUE)) %>%
      arrange(desc(TOTAL))
    
    ggplot(dados_plot, aes(x = reorder(OPM, TOTAL), y = TOTAL)) +
      geom_col(fill = "#46BFB1") +
      geom_text(aes(label = format(TOTAL, big.mark = ".", decimal.mark = ",")),
                hjust = -0.1, size = 4.5) +
      coord_flip() +
      labs(title = paste("Ranking OPM:", input$filtro_indicador),
           x = "OPM", y = "Total") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold")) +
      expand_limits(y = max(dados_plot$TOTAL) * 1.1)
  })
  
  
  
  # Tabela de Armas na aba “ARMAS POR TIPO”
  
  # Dados filtrados por data para essa aba
  dados_filtrados2 <- reactive({
    df <- dados %>%
      filter(DATA_DO_FATO >= input$data_input2[1],
             DATA_DO_FATO <= input$data_input2[2])
    
    validate(
      need(nrow(df) > 0, "Nenhum dado disponível para o intervalo selecionado.")
    )
    
    df %>%
      mutate(
        ARMAS_INDUSTRIAIS = rowSums(across(all_of(colunas_armas_industriais)), na.rm = TRUE)
      )
  })
  
  
  # Renderização da Tabela
  output$tabela_dados <- renderDT({
    df <- dados_filtrados2() %>%
      filter(!is.na(`INFORME O COINT`)) %>%
      group_by(`INFORME O COINT`) %>%
      summarise(
        `ARMAS INDUSTRIAIS` = sum(`QUANTIDADE DE REVOLVERES`, na.rm = TRUE) +
          sum(`QUANTIDADE DE FUZIS`, na.rm = TRUE) +
          sum(`QUANTIDADE DE ESPINGARDAS`, na.rm = TRUE) +
          sum(`QUANTIDADE DE PISTOLAS`, na.rm = TRUE) +
          sum(`QUANTIDADE DE CARABINA`, na.rm = TRUE) +
          sum(`QUANTIDADE DE METRALHADORA`, na.rm = TRUE),
        `ARMAS CASEIRAS` = sum(`QUANTIDADE DE ARMAS DE FOGO DE ORIGEM CASEIRA`, na.rm = TRUE),
        `ARMAS BRANCAS` = sum(`QUANTIDADE DE ARMA BRANCA`, na.rm = TRUE),
        `SIMULACROS` = sum(`QUANTIDADE DE SIMULACRO`, na.rm = TRUE),
        `MUNIÇÕES` = sum(`QUANTIDADE DE MUNIÇÕES (CASEIRA)`, na.rm = TRUE) +
          sum(`QUANTIDADE DE MUNIÇÕES`, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(`ARMAS INDUSTRIAIS`))
    
    datatable(df,
              options = list(
                pageLength = 10,
                dom = 'tip',
                scrollY = "400px",
                scrollCollapse = TRUE
              ),
              rownames = FALSE)
  })
  
  
  # Tabela adicional por OPM
  output$tabela_opm <- renderDT({
    df <- dados_filtrados2() %>%
      filter(!is.na(OPM)) %>%
      group_by(OPM) %>%
      summarise(
        `ARMAS INDUSTRIAIS` = sum(`QUANTIDADE DE REVOLVERES`, na.rm = TRUE) +
          sum(`QUANTIDADE DE FUZIS`, na.rm = TRUE) +
          sum(`QUANTIDADE DE ESPINGARDAS`, na.rm = TRUE) +
          sum(`QUANTIDADE DE PISTOLAS`, na.rm = TRUE) +
          sum(`QUANTIDADE DE CARABINA`, na.rm = TRUE) +
          sum(`QUANTIDADE DE METRALHADORA`, na.rm = TRUE),
        `ARMAS CASEIRAS` = sum(`QUANTIDADE DE ARMAS DE FOGO DE ORIGEM CASEIRA`, na.rm = TRUE),
        `ARMAS BRANCAS` = sum(`QUANTIDADE DE ARMA BRANCA`, na.rm = TRUE),
        `SIMULACROS` = sum(`QUANTIDADE DE SIMULACRO`, na.rm = TRUE),
        `MUNIÇÕES` = sum(`QUANTIDADE DE MUNIÇÕES (CASEIRA)`, na.rm = TRUE) +
          sum(`QUANTIDADE DE MUNIÇÕES`, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(`ARMAS INDUSTRIAIS`))
    
    datatable(df,
              options = list(
                pageLength = 10,
                dom = 'tip',
                scrollY = "400px",
                scrollCollapse = TRUE
              ),
              rownames = FALSE)
  })
  
  
  # Botão de download
  output$download_tabela <- downloadHandler(
    filename = function() {
      paste0("armas_por_tipo_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- dados_filtrados2() %>%
        filter(!is.na(`INFORME O COINT`)) %>%
        group_by(`INFORME O COINT`) %>%
        summarise(
          `ARMAS INDUSTRIAIS` = sum(`QUANTIDADE DE REVOLVERES`, na.rm = TRUE) +
            sum(`QUANTIDADE DE FUZIS`, na.rm = TRUE) +
            sum(`QUANTIDADE DE ESPINGARDAS`, na.rm = TRUE) +
            sum(`QUANTIDADE DE PISTOLAS`, na.rm = TRUE) +
            sum(`QUANTIDADE DE CARABINA`, na.rm = TRUE) +
            sum(`QUANTIDADE DE METRALHADORA`, na.rm = TRUE),
          `ARMAS CASEIRAS` = sum(`QUANTIDADE DE ARMAS DE FOGO DE ORIGEM CASEIRA`, na.rm = TRUE),
          `ARMAS BRANCAS` = sum(`QUANTIDADE DE ARMA BRANCA`, na.rm = TRUE),
          `SIMULACROS` = sum(`QUANTIDADE DE SIMULACRO`, na.rm = TRUE),
          `MUNIÇÕES` = sum(`QUANTIDADE DE MUNIÇÕES (CASEIRA)`, na.rm = TRUE) +
            sum(`QUANTIDADE DE MUNIÇÕES`, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(`ARMAS INDUSTRIAIS`))
      
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  output$download_opm <- downloadHandler(
    filename = function() {
      paste0("armas_por_opm_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- dados_filtrados2() %>%
        filter(!is.na(OPM)) %>%
        group_by(OPM) %>%
        summarise(
          `ARMAS INDUSTRIAIS` = sum(`QUANTIDADE DE REVOLVERES`, na.rm = TRUE) +
            sum(`QUANTIDADE DE FUZIS`, na.rm = TRUE) +
            sum(`QUANTIDADE DE ESPINGARDAS`, na.rm = TRUE) +
            sum(`QUANTIDADE DE PISTOLAS`, na.rm = TRUE) +
            sum(`QUANTIDADE DE CARABINA`, na.rm = TRUE) +
            sum(`QUANTIDADE DE METRALHADORA`, na.rm = TRUE),
          `ARMAS CASEIRAS` = sum(`QUANTIDADE DE ARMAS DE FOGO DE ORIGEM CASEIRA`, na.rm = TRUE),
          `ARMAS BRANCAS` = sum(`QUANTIDADE DE ARMA BRANCA`, na.rm = TRUE),
          `SIMULACROS` = sum(`QUANTIDADE DE SIMULACRO`, na.rm = TRUE),
          `MUNIÇÕES` = sum(`QUANTIDADE DE MUNIÇÕES (CASEIRA)`, na.rm = TRUE) +
            sum(`QUANTIDADE DE MUNIÇÕES`, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(`ARMAS INDUSTRIAIS`))
      
      write.csv(df, file, row.names = FALSE)
    }
  )
  
}


# Executa o App
shinyApp(ui = ui, server = server)


