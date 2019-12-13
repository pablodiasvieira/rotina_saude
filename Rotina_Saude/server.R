function(input, output, session) {
   
  ##BASE DE DADOS
  # Macrorregioes
  base_dados <- reactiveValues()
  
  observe({
    #Dados gerais
    base_dados$base <- readxl::read_excel("data/POPULACAO_RO_2018.xlsx", sheet = 1)
    base_dados$estab <- readxl::read_excel("data/POPULACAO_RO_2018.xlsx", sheet = 2)
    base_dados$malha_est_pav <- read_sf("data/malhaviaria_ro_comp_rodo_est_pav/malhaviaria_ro_comp_rodo_est_pav.shp")
    base_dados$malha_est_npav <- read_sf("data/malhaviaria_ro_comp_rodo_est_npav/malhaviaria_ro_comp_rodo_est_npav.shp")
    base_dados$malha_fed_pav <- read_sf("data/malhaviaria_ro_comp_rodo_fed_pav/malhaviaria_ro_comp_rodo_fed_pav.shp")
    base_dados$malha_fed_npav <- read_sf("data/malhaviaria_ro_comp_rodo_fed_npav/malhaviaria_ro_comp_rodo_fed_npav.shp")
    
    #dados dos municipios de rondonia
    base_dados$dt <- get_brmap("City", geo.filter = list(State = 11))
    
  })
  
  mapinha <- reactive({
    aglomera <- input$aglomeracao#switch(rlang::syms(input$aglomeracao),
                          #"Gerências Regionais de Saúde" = "GRS",
                          #"Macrorregiões de Saúde" = "MACROREG",
                          #"Regiões de Saúde" = "REGIAO_SAUDE")
    
    graf_br <- get_brmap("State") %>% ggplot() +
      geom_sf()+
      theme_map()+
      theme(panel.grid = element_blank(),
            panel.grid.major = element_line(colour = 'transparent'),
            plot.background = element_rect(fill = 'transparent'))+
      geom_rect(xmin = -67.14, xmax = -59.242, ymin = -7.68, ymax = -14.011, 
                fill = NA, colour = "black", size = 1) 
    
    #Mapa Geral
    graf_ro <- base_dados$dt %>% left_join(base_dados$base[,c("CODIGO","MUNICIPIO", "POPULAÇAO","REGIAO_SAUDE","MACROREG","GRS")], c("City" = "CODIGO")) %>% 
      ggplot() +
      geom_sf(aes_string(fill = aglomera), color = "gray40")+
      theme_map()+
      theme(panel.grid.major = element_line(colour = 'transparent'),
            panel.grid = element_blank(),
            legend.background = element_rect(fill = 'transparent'),
            legend.title = element_text(face = "bold"))+
      scale_fill_manual(values=c("#C2DF21", "#98D9F0", "#00A8E5","#24B04F", "#FDF001", "#FBAEC6", "#C6C1E9"))+
      labs(subtitle=paste0(ifelse(input$aglomeracao=="GRS","Gerências Regionais de Saúde ",
                                  ifelse(input$aglomeracao=="MACROREG","Macrorregiões de Saúde ",
                                         "Regiões de Saúde ")),ifelse(input$nome_est,"vs Unidades de Saúde Estaduais","")), 
           title="ESTADO DE RONDÔNIA - RO, BRASIL - BR (State of Rondonia, Brazil)", 
           caption = "Elaborado por: Páblo Dias Vieira - ASTEC/SESAU/RO\nFeito em R")
    if(input$nome_mun){
      graf_ro <-  graf_ro+
        geom_text_repel(data=base_dados$base,
                      mapping = aes(x=Longitude, y=Latitude, label = MUNICIPIO),
                      color = input$cor_mun,
                      fontface = 'italic',
                      size = input$tam_let_mun,
                      point.padding = unit(0.5, "lines"),
                      segment.color = "grey30")
      }
    
    if(input$rodovias){
      graf_ro <- graf_ro +
        geom_sf(data = dplyr::filter(base_dados$malha_fed_pav, nome_rodo == "BR-364"),  
                color=ifelse(input$rodovias, input$cor_rod, "yellow"), size = 1, alpha=0.1)
    }
    if(input$pontos){
      graf_ro <- graf_ro +
        geom_point(data=base_dados$base,
                   mapping = aes(x=Longitude, y=Latitude), color = input$cor_pon, size = input$tam_pon)
    }
    
    
    #Mapa de ro com o brasil  
    graf_cons <- graf_ro + annotation_custom(
      grob = ggplotGrob(graf_br),
      xmin = -67.14 + 6,
      xmax = -59.242,
      ymin = -7.68,
      ymax = -14.011+3
    )
  
    ifelse(input$tipo_est == "Todos", 
           tipo_est <- base_dados$estab, 
           tipo_est <- filter(base_dados$estab, Tipo == input$tipo_est))
    
    #Grafico com estabelecimentos
    if(input$nome_est){
    graf_cons +
      geom_label_repel(data=tipo_est,
                       aes(x=Latitude, y=Longitude, label = Estabelecimento),
                       color = input$cor_est,
                       fontface = 'bold',
                       segment.colour = "#027D19",
                       segment.color = "#027D19",
                       box.padding = unit(0.35, "lines"),
                       point.padding = unit(0.5, "lines"),
                       size = input$tam_let_est) +
      geom_image(data=tipo_est, aes(x=Latitude, y=Longitude, image=image), size=.02, colour="red")
    }else{
      graf_cons
      }
  })
  
  output$mapa <- renderPlot({
   mapinha()
  })
  
  #baixar mapa png
  output$baixarmapapng <- downloadHandler(
    filename = function() {
      paste('Mapa-RO-', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      ggsave(file, mapinha(), width = 16, height = 10.4)
    }

  )
  
}
