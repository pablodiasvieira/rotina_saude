dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Mapas", tabName = "mapas", icon = icon("dashboard"))
    )
  ),
  
  dashboardBody(
    shinyDashboardThemes(
      theme = "grey_light"
    ),

    
    tabItems(
      # First tab content
      tabItem(tabName = "mapas",
                  fluidRow(
                column(width = 3,
                       #selectInput("tam_let_map", "Letra do Mapa"),
                       fluidRow(
                         column(width=12,
                                box(
                                  selectInput("aglomeracao", "Aglomerações Geográficas", 
                                              #choices=c("Gerências Regionais de Saúde","Macrorregiões de Saúde","Regiões de Saúde"),
                                              choices = c("GRS","MACROREG","REGIAO_SAUDE") ,
                                              selected = "REGIAO_SAUDE"), width = "100%"))
                       ),
                       fluidRow(
                         column(width=6,
                           box(
                             checkboxInput("nome_mun", label= "Mostrar Municipio", value = T),
                             conditionalPanel(condition = "input.nome_mun == true ",
                                              numericInput("tam_let_mun", "Tamanho do Município", 
                                                           min = 0, max = 5, value = 3, step = 0.5),
                                              colourInput("cor_mun", "Cor Municipio", "black", palette = "limited")
                                              ), width = "100%")),
                         column(width=6,
                           box(
                             checkboxInput("nome_est", label= "Mostrar Unidade", value = T),
                             conditionalPanel(condition = "input.nome_est == true ",
                                              selectInput("tipo_est", "Tipo", 
                                                          choices = c("Ambulatório", "Centros", "Fundação", "Hospital","Laboratório","Todos"),
                                                          selected ="Hospital"),
                                              numericInput("tam_let_est", "Tamanho da Unidade", 
                                                           min = 0, max = 5, value = 3, step = 0.5),
                                              colourInput("cor_est", "Cor Unidade", "#DE8C08")
                                              ), width = "100%"))
                       ),
                       fluidRow(
                         column(width=6,
                                box(
                                checkboxInput("rodovias", label= "Mostrar BR-364", value = F),
                                conditionalPanel(condition = "input.rodovias == true ",
                                  colourInput("cor_rod", "Cor", "yellow")
                                  ), width = "100%")
                                ),
                         column(width=6,
                                box(
                                checkboxInput("pontos", label= "Mostrar Pontos", value = F),
                                conditionalPanel(condition = "input.pontos == true ",
                                                 colourInput("cor_pon", "Cor", "red"),
                                                 numericInput("tam_pon", "Tamanho", min = 0, max = 5, value = 1, step = 0.5)
                                ), width = "100%")
                         )
                         )
                       ),
                column(width = 9,
                       withSpinner(plotOutput("mapa", height = "500px"), type=8, color="#0dc5c1"),
                       downloadButton('baixarmapapng', 
                                      'Baixar (.png)'))

              )
      )
      )
    )
)
