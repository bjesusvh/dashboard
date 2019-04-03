##-------------------------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------------------------##
##       DASHBOARD OF IFNBs WORKING WITH FIRA
##       THIS IS A PERSONAL PROYECT DEVELOPED BY: DR. BARTOLO VILLAR
##       SMIF
##       Version 1.0.
##       April-2019
##-------------------------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------------------------##
library(shiny)
library(plotly)
library(plyr)
library(tidyverse)
library(readr)
library(scales)
library(reshape2)
library(cluster)
library(factoextra)
library(magrittr)
library(mice)
theme_set(theme_bw()) #bw

## Directorio de trabajo automatico.
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)

load("data_all.RData")
source("multiplot_ranking_others.R")

##-------------------------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------------------------##
##       P R E A M B L E 
##-------------------------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------------------------##
## Clave de los activos (incluyendo 819-ACCEDE )
IF_ACTIVOS  <- read.csv("IF_ACTIVOS.csv", header = TRUE)$CLAVE_A
dat <- dat[dat$CLAVE  %in% IF_ACTIVOS ,]

## Clave de los IFNB que entran en el ranking
IF_RANKING  <- read.csv("IF_RANKING.csv", header = TRUE)$CLAVE_A

dat_ranking <- dat[dat$CLAVE %in% IF_RANKING,]
dat_ranking <- dat_ranking %>% filter( DATE ==  max(dat_ranking$DATE))

## Variables que entran en el ranking.
dat_ranking <- dat_ranking %>% select("NOMBRE", `TOTAL ACTIVO`, `CARTERA DE CREDITO`,
                                      `TOTAL PASIVO`,
                                      "ICV", "ICVN", "ICAP", "ROE", "ROA","LIQUIDEZ_ACTIVOS", 
                                      "APALANC", "EFI_OPER")

colnames(dat_ranking) <- c("NOMBRE","TOTAL_ACTIVO", "CARTERA_CREDITO", "TOTAL_PASIVO",
                           "ICV", "ICVN", "ICAP", "ROE", "ROA", "LIQUIDEZ_ACTIVOS",
                           "APALANC", "EFI_OPER")


##-------------------------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------------------------##
##       BEGIN AUXILIAR FUNCTION TO DO THE CLUSTER ANALYSIS
##-------------------------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------------------------##
# datos_pca <- dat %>% select( "DATE", "CLAVE", `TOTAL ACTIVO`, `CARTERA DE CREDITO`, `CARTERA DE CREDITO VIGENTE`,
#                              `CARTERA DE CREDITO VENCIDA`, `(-) ESTIMACIONES PREVENTIVAS PARA RIESGOS CREDITICIOS`,
#                              `TOTAL PASIVO`, `TOTAL CAPITAL CONTABLE`, `INGRESOS POR INTERESES`,
#                              `GASTOS POR INTERESES`, `MARGEN FINANCIERO`, `ESTIMACION PREVENTIVA PARA RIESGOS CREDITICIOS`, `MARGEN FINANCIERO AJUSTADO POR RIESGOS CREDITICIOS`,
#                              `GASTOS DE ADMINISTRACION`, `RESULTADO DE LA OPERACION`, `RESULTADO_NETO_EDORES`,
#                              "ICV", "ICVN", "EFI_OPER", "LIQUIDEZ_ACTIVOS", "APALANC",
#                              "TASA_A_IMP", "TASA_P_IMP", "TASA_DE_EQUILIBRIO", "CARTERA_DE_EQUILIBRIO", "ROE")
# 
# datos_last_year = datos_pca %>% filter(DATE == "2017-12-01") %>% select(-DATE) %>% column_to_rownames("CLAVE") %>% scale()
# datos_current_month = datos_pca %>% filter(DATE == max(datos_pca$DATE)) %>% select(-DATE) %>% column_to_rownames("CLAVE") %>% scale()
# 
# 
# km_last_year <- kmeans(na.omit(datos_last_year), 4, nstart = 25)
# km_current_m <- kmeans(na.omit(datos_current_month), 4, nstart = 25)
# 
# pp1 = fviz_cluster(km_last_year,
#                    data = datos_last_year,
#                    ellipse.type = "convex", pointsize = 2.5,
#                    palette = "jco", main = "Datos a diciembre de 2017",
#                    labelsize = 9, 
#                    repel = TRUE)
# 
# pp2 = fviz_cluster(km_current_m,
#                    data = datos_current_month,
#                    ellipse.type = "convex", pointsize = 2.5,
#                    palette = "jco", main = "Datos al mes actual",
#                    labelsize = 9, 
#                    repel = TRUE)

##-------------------------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------------------------##
##       END AUXILIAR FUNCTION TO DO THE CLUSTER ANALYSIS
##-------------------------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------------------------##


##-------------------------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------------------------##
##       B E G I N   "U I"  A P P L I C A T I O N
##-------------------------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------------------------##
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Datos longitudinales IFNBs"),
  
  # PANEL PRINCIPAL
  sidebarLayout(
    sidebarPanel(
      # AQUI SELECCIONO QUE DE QUE INTERMEDIARIO QUIERO DESPLEGAR INFORMACION
      selectInput(inputId = "selected_IF",
                  label = "Seleccione un IFNB",
                  choices = c(sort(unique(dat$NOMBRE))),
                  selected = "807 CRECE")
    ),
    ### SALIDAS
    mainPanel(
      tabsetPanel(type = "tabs",
                  ## Tab 1
                  ### AQUI VOY A MOSTRAR UN GRAFICO CUALQUIERA DE UN INDICADOR
                  tabPanel(title = "Graficos",
                           br(),
                           plotOutput(outputId = "CCT"),
                           br(),
                           plotOutput(outputId = "RES_NETO"),
                           br(),
                           plotOutput(outputId = "ICV"),
                           br(),
                           plotOutput(outputId = "ROE"),
                           br(),
                           plotOutput(outputId = "ROA"),
                           br(),
                           plotOutput(outputId = "ICAP"),
                           br(),
                           plotOutput(outputId = "APALANC"),
                           br(),
                           plotOutput(outputId = "SUFICIENCIA_EST_PREV"),
                           br(),
                           plotOutput(outputId = "COB_RESERVAS"),
                           br(),
                           plotOutput(outputId = "EFIC_OPER"),
                           br(),
                           plotOutput(outputId = "TASAS"),
                           br(),
                           plotOutput(outputId = "CARTERA_DE_EQUILIBRIO")
                  ),
                  # Tab 2
                  # Probando
                   tabPanel(title = "Graficos interactivos",
                            br(),
                            plotlyOutput(outputId = "CCT_2",  height = "500px"),
                            br(),
                            plotlyOutput(outputId = "RES_NETO_2",  height = "500px"),
                            br(),
                            plotlyOutput(outputId = "ICV_2",  height = "500px"),
                            br(),
                            plotlyOutput(outputId = "ROE_2",  height = "500px"),
                            br(),
                            plotlyOutput(outputId = "ROA_2",  height = "500px"),
                            br(),
                            plotlyOutput(outputId = "ICAP_2",  height = "500px"),
                            br(),
                            plotlyOutput(outputId = "APALANC_2",  height = "500px"),
                            br(),
                            plotlyOutput(outputId = "SUFICIENCIA_EST_PREV_2",  height = "500px"),
                            br(),
                            plotlyOutput(outputId = "COB_RESERVAS_2",  height = "500px"),
                            br(),
                            plotlyOutput(outputId = "EFIC_OPER_2",  height = "500px"),
                            br(),
                            plotlyOutput(outputId = "TASAS_2",  height = "500px"),
                            br(),
                            plotlyOutput(outputId = "CARTERA_DE_EQUILIBRIO_2",  height = "500px")
                  ),
                  
                  ## Tab 3         
                  ### AQUI MUESTRO LA TABLA CON LAS CUENTAS DE BG, EDORES E ICAP
                  tabPanel(title = "Resumen Estados Financieros",
                           br(),
                           h4("En esta seccion usted puede descargar los datos y/o visualizarlos"),
                           h5("Cantidades monetarias en millones de pesos"),
                           br(),
                           downloadButton(outputId = "download_data", label = "Descargar datos"),
                           br(),
                           DT::dataTableOutput(outputId = "tabla_if"),
                           br(),
                           h4("Abraviaturas:"),
                           h5("BG: Balance General"),
                           h5("ER: Estado de Resultados"),
                           h5("RF: Razones Financieras")
                  ),
                  
                  ## Tab 4         
                  ### AQUI MUESTRO LA TABLA DEL ANALISIS DUPONT
                  tabPanel(title = "Analisis DuPont",
                           br(),
                           h4("En esta seccion usted puede descargar los datos y/o visualizarlos"),
                           h5("Cantidades monetarias en millones de pesos"),
                           br(),
                           downloadButton(outputId = "download_data_dupont", label = "Descargar datos"),
                           br(),
                           DT::dataTableOutput(outputId = "tabla_if_dupont")
                  ),
                  
                  
                  ## Tab 5
                  ### RANKING
                  tabPanel(title = "Ranking",
                           br(),
                           h4("Ranking de principales cuentas y razones financieras"),
                           br(),
                           plotOutput(outputId = "RANKING"),
                           br(),
                           h5("ND: NO DISPONIBLE"),
                           br(),
                           br(),
                           downloadButton(outputId = "download_data_ranking", label = "Descargar datos"),
                           br(),
                           DT::dataTableOutput(outputId = "tablita_if_ranking"),
                           br()
                  ),
                  ## Tab 6
                  ### AQUI VOY A MOSTRAR LA MISCELANEA DE GRAFICOS
                  tabPanel(title = "Otros graficos",
                           br(),
                           plotOutput(outputId = "misc_plots")#,
                           #br(),
                           #br(),
                           #plotOutput(outputId = "cluster_plot")
                  ),
                  
                  ## Tab 7         
                  ### Creditos perrones
                  tabPanel(title = "Creditos",
                           h3("Realizado por: "),
                           h4("Bartolo de Jesus Villar Hernandez, Dr."),
                           h5("Especialista - SMIF, dic-2018"),
                           h5("Cualquier duda, comentario o sugerencia hacerla llegar a: bjvillar@fira.gob.mx"))
      ) # End tabsetPanel
      
    ) # End mainPanel
  )  # End sidebarLayout
) # End fluidPage
##-------------------------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------------------------##
##       E N D  "U I"  A P P L I C A T I O N
##-------------------------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------------------------##



##-------------------------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------------------------##
##       B E G I N       S E R V E R
##-------------------------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------------------------##
server <- function(input, output) {
  ## Creamos un dataframe general de los datos del IF seleccionado.
  datos_if <- reactive({
    req(input$selected_IF)   # garantizamos que este seleccionado un IF
    dat %>% filter(NOMBRE == input$selected_IF) #%>% select(input$selected_vars)
  })
  
  ##-------------------------------------------------------------------------------------------------##
  ##-------------------------------------------------------------------------------------------------##
  ## C U E N T A S       R E S U M E N.
  ##-------------------------------------------------------------------------------------------------##
  ## Creamos un dataframe de las cuentas resumen.
  datos_if_res <- reactive({
    req(input$selected_IF)   # garantizamos que este seleccionado un IF
    # datos_ifv <- datos_if()[, cuentas_id]
    datos_ifv <- datos_if() %>% select(`TOTAL ACTIVO`, `CARTERA DE CREDITO`, `CARTERA DE CREDITO VIGENTE`,
                                       `CARTERA DE CREDITO VENCIDA`, `(-) ESTIMACIONES PREVENTIVAS PARA RIESGOS CREDITICIOS`,
                                       `TOTAL PASIVO`, `TOTAL CAPITAL CONTABLE`, `INGRESOS POR INTERESES`,
                                       `GASTOS POR INTERESES`, `MARGEN FINANCIERO`, `ESTIMACION PREVENTIVA PARA RIESGOS CREDITICIOS`, `MARGEN FINANCIERO AJUSTADO POR RIESGOS CREDITICIOS`,
                                       `GASTOS DE ADMINISTRACION`, `RESULTADO DE LA OPERACION`, `RESULTADO_NETO_EDORES`,
                                       "ICAP", "ICV", "ICVN", "EFI_OPER", "LIQUIDEZ_ACTIVOS", "APALANC",
                                       "TASA_A_IMP", "TASA_P_IMP", "TASA_DE_EQUILIBRIO", "CARTERA_DE_EQUILIBRIO", "ROE", "DATE")
    
    ## Transformamos a millones de pesos
    id_millones <- c(1:15, 25)
    datos_ifv[,id_millones] <- lapply(
      datos_ifv[,id_millones],function(x){as.numeric(x/1e6)
      })
    
    ## Redondeamos a dos cifras decimales
    datos_ifv[,1:(ncol(datos_ifv)-1)] <- lapply(
      datos_ifv[,1:(ncol(datos_ifv)-1)],function(x){as.numeric(round(x, 2))
      })
    
    ## Nos quedamos con anio-mes (AAAA-MM)
    datos_ifv$DATE <- substr(datos_ifv$DATE, 1, 7)
    
    
    ### Filtramos los meses a cierre del anio (diciembre)
    last_month <- datos_ifv$DATE[length(datos_ifv$DATE)]
    id_date_selected <- c("2012-12", "2013-12","2014-12", "2015-12", "2016-12", "2017-12","2018-12" ,last_month)
    datos_ifv_resumen <- datos_ifv %>% filter(DATE %in% id_date_selected)
    
    PP <- melt(datos_ifv_resumen, variable.name = "CUENTA", value.name = "VALOR")
    QQ <- dcast(PP, CUENTA~DATE)
    
    UNIDAD <- c(rep("mpd", 15), rep("%", 5), "--", rep("%", 3), "mdp", "%")
    FUENTE <- c(rep("BG", 7), rep("ER", 8), rep("RF", 9), "--", "RF")
    
    QQ <- QQ %>% add_column(UNIDAD, 
                            .after = "CUENTA")
    QQ <- QQ %>% add_column(FUENTE, 
                            .after = "UNIDAD")
    
   # QQ$CUENTA[which(QQ$CUENTA == `ESTIMACION PREVENTIVA PARA RIESGOS CREDITICIOS`)] <- "CONSTITUCION DE RESERVAS PREVENTIVAS"
  })
  
  ## Aqui mostramos la Tabla del resumen de estados financieros y razones financieras
  output$tabla_if <- DT::renderDataTable({
    req(input$selected_IF)
    DT::datatable(data = datos_if_res(),
                  options = list(paging = FALSE), #pageLenght= 25
                  rownames = FALSE
    )
  })
  
  ### Aqui descargamos la Tabla de resumen de cuentas y razones financiera
  # Download file
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(input$selected_IF, "_resumen.csv")
    },
    content = function(file){
      write_csv(datos_if_res(), file) 
      
    }
  )
  ##-------------------------------------------------------------------------------------------------##
  ##-------------------------------------------------------------------------------------------------##
  ## E N D      C U E N T A S      R E S U M E N.
  ##-------------------------------------------------------------------------------------------------##
  
  ##-------------------------------------------------------------------------------------------------##
  ##-------------------------------------------------------------------------------------------------##
  ## ANALISIS DUPONT.
  ##-------------------------------------------------------------------------------------------------##
  datos_dupont <- reactive({
    req(input$selected_IF)   # garantizamos que este seleccionado un IF
    # datos_ifv <- datos_if()[, cuentas_id]
    
    datos_ifv <- datos_if() %>% select( "CCT", "TASA_A_IMP", `INGRESOS POR INTERESES`,
                                        `PRESTAMOS BANCARIOS Y DE OTROS ORGANISMOS`,
                                        "TASA_P_IMP", `GASTOS POR INTERESES`,
                                        `MARGEN FINANCIERO`, "ICV", `ESTIMACION PREVENTIVA PARA RIESGOS CREDITICIOS`,
                                        `MARGEN FINANCIERO AJUSTADO POR RIESGOS CREDITICIOS`,
                                        `TOTAL INGRESOS (EGRESOS) DE LA OPERACION`,
                                        `GASTOS DE ADMINISTRACION`,
                                        `RESULTADO DE LA OPERACION`,
                                        `OTROS PRODUCTOS`, `RESULTADO_NETO_EDORES`,
                                        `TOTAL CAPITAL CONTABLE`, "ROE", "TASA_DE_EQUILIBRIO", "CARTERA_DE_EQUILIBRIO", "DATE")   
    
    ## Nos quedamos con anio-mes (AAAA-MM)
    datos_ifv$DATE <- substr(datos_ifv$DATE, 1, 7)
    
    ### Filtramos los meses a cierre del anio (diciembre)
    last_month <- datos_ifv$DATE[length(datos_ifv$DATE)]
    id_date_selected <- c("2012-12", "2013-12","2014-12", "2015-12", "2016-12", "2017-12", "2018-12", last_month)
    datos_ifv_resumen <- datos_ifv %>% filter(DATE %in% id_date_selected)
    
    
    ## Transformamos a millones de pesos
    id_millones <- c(1, 3, 4, 6, 7, 9:16, 19)
    datos_ifv_resumen[,id_millones] <- lapply(
      datos_ifv_resumen[,id_millones],function(x){as.numeric(x/1e6)
      })
    
    ## Redondeamos a dos cifras decimales
    datos_ifv_resumen[,1:(ncol(datos_ifv_resumen)-1)] <- lapply(
      datos_ifv_resumen[,1:(ncol(datos_ifv_resumen)-1)],function(x){as.numeric(round(x, 2))
      })
    
    
    PP <- melt(datos_ifv_resumen, variable.name = "CUENTA", value.name = "VALOR")
    QQ <- dcast(PP, CUENTA~DATE)
    
    UNIDAD <- c("mdp", 	"%", "mpd",	"mdp", "%",	"mdp",	"mdp",	"%",	"mdp",	"mdp",	"mdp",	"mdp",	
                "mdp",	"mdp",	"mdp",	"mdp",	"%",	"%",	"mdp")
    
    QQ <- QQ %>% add_column(UNIDAD, 
                            .after = "CUENTA")
    
  })
  
  
  ## Aqui mostramos la Tabla del Analisis Dupont
  output$tabla_if_dupont <- DT::renderDataTable({
    req(input$selected_IF)
    DT::datatable(data = datos_dupont(),
                  options = list(paging = FALSE), #pageLenght= 25
                  rownames = FALSE
    )
  })
  
  ### Aqui descargamos la Tabla del analisis dupont
  # Download file
  output$download_data_dupont <- downloadHandler(
    filename = function() {
      paste0(input$selected_IF, "_dupont.csv")
    },
    content = function(file){
      write_csv(datos_dupont(), file) 
      
    }
  )
  
  ##-------------------------------------------------------------------------------------------------##
  ##-------------------------------------------------------------------------------------------------##
  ## END DUPONT.
  ##-------------------------------------------------------------------------------------------------##
  
  ##### AQUI REALIZAMOS EN GRAFICO DE INDICADORES Y CANTIDADES DE INTERES
  ## CARTERA DE CREDITO TOTAL
  output$CCT <- renderPlot({
    req(input$selected_IF)
    ggplot(data = datos_if(), aes(x = DATE)) +
      geom_area(aes(y = CCT/1e6), fill = "#2c7fb8", alpha = 0.6) +
      geom_area(aes(y = CARTERA_FIRA/1e6), fill = "blue", alpha = 0.4)+
      geom_line(aes(y = CCT/1e6), col = "#2c7fb8", size = 1.0)+
      geom_line(aes(y = CARTERA_FIRA/1e6), col = "blue", size = 1.0)+
      theme(axis.text.x = element_text(angle = 90, vjust=0.5), panel.grid.minor = element_blank()) +
      scale_x_date(labels = date_format("%m/%Y"), breaks = "3 month") +
      labs(title = "CARTERA DE CREDITO TOTAL y CARTERA CON RECURSOS FIRA" ,
           subtitle = "Cifras en Millones de Pesos",
           caption = "Fuente: SIIOF / SMIF",
           y = "Millones de pesos",
           x = "" ) + theme(aspect.ratio = 0.5) 
  })
  
  ## Resultado neto
  output$RES_NETO <- renderPlot({
    req(input$selected_IF)
    ggplot(data = datos_if(), aes(x = DATE)) +
      geom_line(aes(y  = RESULTADO_NETO_BG/1e6), colour = "darkgray", size=1.0) +
      geom_point(aes(y = RESULTADO_NETO_BG/1e6, colour  = cut(RESULTADO_NETO_BG/1e6, c(-Inf, 0, Inf))), size = 2) + 
      scale_color_manual(name = "Resultado neto",
                         values = c("(-Inf,0]" = "red",
                                    "(0, Inf]" = "blue")) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5), panel.grid.minor = element_blank()) +
      scale_x_date(labels = date_format("%m/%Y"), breaks = "3 month") +
      theme(legend.position = 'none') + 
      labs(title = "RESULTADO NETO" ,
           subtitle = "Cifras en Millones de Pesos",
           caption = "Fuente: SIIOF / SMIF",
           y = "Millones de Pesos",
           x = "") + theme(aspect.ratio = 0.5) 
  })
  
  ## Resultado neto
  # output$RES_NETO <- renderPlot({
  #   req(input$selected_IF)
  #   ggplot(data=datos_if(), aes(x=DATE, y=RESULTADO_NETO_BG/1e6, fill=INDICADOR_RES_NETO)) +
  #     geom_bar(stat = "identity", position = "identity", alpha = 0.5) +
  #     theme(axis.text.x = element_text(angle = 90, vjust=0.5), panel.grid.minor = element_blank()) +
  #     scale_x_date(labels = date_format("%m/%Y"), breaks = "3 month") +
  #     scale_fill_manual(values = c("#e34a33", "#3182bd"), guide=FALSE) + 
  #     labs(title = "RESULTADO NETO (BG)",
  #          subtitle = "Cifras en Millones de Pesos",
  #          caption = "Fuente: SIIOF / SMIF",
  #          y="Millones de pesos",
  #          x="")
  # })
  
  ## ICV
  output$ICV <- renderPlot({
    req(input$selected_IF)
    ggplot(data = datos_if(), aes(x = DATE)) +
      geom_line(aes(y=ICV), col = "#2b8cbe", size = 1.0) +
      # geom_hline(yintercept = 6.5, col="green", size = 1, linetype = 2) + 
      geom_hline(yintercept = 10, col="red", size = 1, linetype = 2) + 
      geom_line(aes(y = ICVN), col = "#a6bddb", size = 1.0) + 
      # geom_hline(yintercept = 3.0, col="green", size = 1, linetype = 2) + 
      geom_hline(yintercept = 3.5, col="red", size = 1, linetype = 2) + 
      theme(axis.text.x = element_text(angle = 90, vjust=0.5), panel.grid.minor = element_blank()) +
      scale_x_date(labels = date_format("%m/%Y"), breaks = "3 month") +
      # scale_color_manual(name="", 
      #                    values = c("ICV"="#2b8cbe", "ICVN"="#a6bddb")) +  # line color
      labs(title = "ICV e ICVN" ,
           subtitle = "Cifras en Porcentaje",
           caption = "Fuente: SIIOF / SMIF",
           y = "Porcentaje",
           x = "") + theme(aspect.ratio = 0.5) 
  })
  

  ## ROE
  output$ROE <- renderPlot({
    req(input$selected_IF) 
    ggplot(data=datos_if(), aes(x=DATE)) + 
      geom_line(aes(y=ROE), col = "#636363", size=1.0, linetype = 1) +
      theme(axis.text.x = element_text(angle = 90, vjust=0.5), panel.grid.minor = element_blank()) +
      scale_x_date(labels = date_format("%m/%Y"), breaks = "3 month") +
      labs(title = "ROE" ,
           subtitle = "Cifras en Porcentaje",
           caption = "Fuente: SIIOF / SMIF",
           y="Porcentaje",
           x="") + theme(aspect.ratio = 0.5) 
  })
  
  ## ROA
  output$ROA <- renderPlot({
    req(input$selected_IF)
    ggplot(data=datos_if(), aes(x=DATE)) +
      geom_line(aes(y=ROA), col = "#d95f0e", size=1.0) +
      theme(axis.text.x = element_text(angle = 90, vjust=0.5), panel.grid.minor = element_blank()) +
      scale_x_date(labels = date_format("%m/%Y"), breaks = "3 month") +
      labs(title = "ROA" ,
           subtitle = "Cifras en Porcentaje",
           caption = "Fuente: SIIOF / SMIF",
           y="Porcentaje",
           x="") + theme(aspect.ratio = 0.5) 
  })
  
  
  ## ICAP
  output$ICAP <- renderPlot({
    req(input$selected_IF) 
    ggplot(data=datos_if(), aes(x=DATE)) + 
      geom_line(aes(y=ICAP), col = "#2c7fb8", size=1.0) +
      geom_hline(yintercept = 10, col="green", size = 1, linetype = 2) + 
      geom_hline(yintercept = 8, col="red", size = 1, linetype = 2) + 
      theme(axis.text.x = element_text(angle = 90, vjust=0.5), panel.grid.minor = element_blank()) +
      scale_x_date(labels = date_format("%m/%Y"), breaks = "3 month") +
      labs(title = "ICAP",
           subtitle = "Cifras en Porcentaje",
           caption = "Fuente: SIIOF / SMIF",
           y="Porcentaje",
           x="") + theme(aspect.ratio = 0.5) 
  })
  
  ## SUFICIENCIA_EST_PREV = RESERVAS_CONSTITUIDAS/RESERVAS_PREVENTIVAS_CALC*100, 
  output$SUFICIENCIA_EST_PREV <- renderPlot({
    req(input$selected_IF) 
    ggplot(data=datos_if(), aes(x=DATE)) + 
      geom_line(aes(y=SUFICIENCIA_EST_PREV), col = "#c51b8a", size=1.0) +
      geom_hline(yintercept = 100, col="green", size = 1, linetype = 2) + 
      theme(axis.text.x = element_text(angle = 90, vjust=0.5), panel.grid.minor = element_blank()) +
      scale_x_date(labels = date_format("%m/%Y"), breaks = "3 month") +
      labs(title = "SUFICIENCIA DE ESTIMACIONES PREVENTIVAS",
           subtitle = "Cifras en Porcentaje",
           caption = "Fuente: SIIOF / SMIF",
           y="Porcentaje",
           x="") + theme(aspect.ratio = 0.5) 
  })
  
  ## COBERTURA DE RESERVAS = RESERVAS_CONSTITUIDAS/CARTERA DE CREDITO VENCIDA*100,
  output$COB_RESERVAS <- renderPlot({
    req(input$selected_IF) 
    ggplot(data=datos_if(), aes(x=DATE)) + 
      geom_line(aes(y=COBERTURA_RESERVAS), col = "#31a354", size=1.0) +
      theme(axis.text.x = element_text(angle = 90, vjust=0.5), panel.grid.minor = element_blank()) +
      scale_x_date(labels = date_format("%m/%Y"), breaks = "3 month") +
      labs(title = "COBERTURA DE RESERVAS" ,
           subtitle = "Cifras en Porcentaje",
           caption = "Fuente: SIIOF / SMIF",
           y="Porcentaje",
           x="") + theme(aspect.ratio = 0.5) 
  })
  
  # APALANC = (CAPTACION TRADICIONAL (PARA UNIONES DE CREDITO, SOFIPOS Y COOPERATIVAS) + PRESTAMOS BANCARIOS Y DE OTROS ORGANISMOS)/TOTAL CAPITAL CONTABLE
  output$APALANC <- renderPlot({
    req(input$selected_IF) 
    ggplot(data=datos_if(), aes(x=DATE)) + 
      geom_line(aes(y=APALANC), col = "#67a9cf", size=1.0) +
      theme(axis.text.x = element_text(angle = 90, vjust=0.5), panel.grid.minor = element_blank()) +
      scale_x_date(labels = date_format("%m/%Y"), breaks = "3 month") +
      labs(title = "APALANCAMIENTO",
           subtitle = "Cifras en Porcentaje",
           caption = "Fuente: SIIOF / SMIF",
           y="Porcentaje",
           x="") + theme(aspect.ratio = 0.5) 
  })
  
  ## EFICIENCIA OPERATIVA
  output$EFIC_OPER <- renderPlot({
    req(input$selected_IF) 
    ggplot(data=datos_if(), aes(x=DATE)) + 
      geom_line(aes(y=EFI_OPER), col = "#de2d26", size=1.0) +
      theme(axis.text.x = element_text(angle = 90, vjust=0.5), panel.grid.minor = element_blank()) +
      scale_x_date(labels = date_format("%m/%Y"), breaks = "3 month") +
      labs(title = "EFICIENCIA OPERATIVA" ,
           subtitle = "Cifras en Porcentaje",
           caption = "Fuente: SIIOF / SMIF",
           y="Porcentaje",
           x="") + theme(aspect.ratio = 0.5) 
  })
  
  ### TASAS Y SUS DIFERENCIAS
  output$TASAS <- renderPlot({
    req(input$selected_IF)
    ggplot(data=datos_if(), aes(x=DATE)) +
      geom_line(aes(y=TASA_A_IMP), col = "blue", size=1.0) + 
      geom_line(aes(y=TASA_P_IMP), col = "red", size=1.0) + 
      theme(axis.text.x = element_text(angle = 90, vjust=0.5), panel.grid.minor = element_blank()) +
      scale_x_date(labels = date_format("%m/%Y"), breaks = "3 month") +
      # scale_color_manual(name="", 
      #                    values = c("ICV"="#feb24c",
      #                               "ICVN"="red")) +
      labs(title = "TASA ACTIVA IMPLICITA y TASA PASIVA IMPLICITA" ,
           subtitle = "Cifras en Porcentaje",
           caption = "Fuente: SIIOF / SMIF",
           y="Porcentaje",
           x="") + theme(aspect.ratio = 0.5) 
  })
  
  # CARTERA TOTAL vs CARTERA DE EQUILIBRIO
  # CARTERA_DE_EQUILIBRIO = (TASA_DE_EQUILIBRIO*CCT)/TASA_A_IMP
  output$CARTERA_DE_EQUILIBRIO <- renderPlot({
    req(input$selected_IF)
    ggplot(data=datos_if(), aes(x=DATE)) +
      geom_line(aes(y=CARTERA_DE_EQUILIBRIO/1e6, col = "CARTERA_DE_EQUILIBRIO"), size=1.0) + 
      geom_line(aes(y=CCT/1e6, col = "CCT"), size=1.0) + 
      theme(axis.text.x = element_text(angle = 90, vjust=0.5), panel.grid.minor = element_blank()) +
      scale_x_date(labels = date_format("%m/%Y"), breaks = "3 month") +
      scale_color_manual(name = "", 
                         values = c("CARTERA_DE_EQUILIBRIO"="#91cf60", "CCT"="#fc8d59"),
                         labels = c("CARTERA_EQUILIBRIO", "CCT")) +
      theme(legend.position="top") +
      labs(title = "CARTERA TOTAL vs CARTERA DE EQUILIBRIO" ,
           subtitle = "Cifras en Millones de Pesos",
           caption = "Fuente: SIIOF / SMIF",
           y="Millones de pesos",
           x="") + theme(aspect.ratio = 0.5) 
  })
  
  
  
  # ##############################################
  ## GRAFICOS INTERACTIVOS
  ## CARTERA DE CREDITO TOTAL
  output$CCT_2 <- renderPlotly({
    req(input$selected_IF)
    ggplot(data=datos_if(), aes(x=DATE)) +
      geom_area(aes(y=CCT/1e6), fill = "#2c7fb8", alpha = 0.6) +
      geom_area(aes(y=CARTERA_FIRA/1e6), fill = "blue", alpha = 0.4)+
      geom_line(aes(y=CCT/1e6), col = "#2c7fb8", size=1.0)+
      geom_line(aes(y=CARTERA_FIRA/1e6), col = "blue", size=1.0)+
      theme(axis.text.x = element_text(angle = 90, vjust=0.5), panel.grid.minor = element_blank()) +
      scale_x_date(labels = date_format("%m/%Y"), breaks = "3 month") +
      labs(title = "CARTERA DE CREDITO TOTAL y CARTERA CON RECURSOS FIRA" ,
           subtitle = "Cifras en Millones de Pesos",
           caption = "Fuente: SIIOF / SMIF",
           y="Millones de pesos",
           x="") + theme(aspect.ratio = 0.5) 
  })
  
  ## Resultado neto
  output$RES_NETO_2 <- renderPlotly({
    req(input$selected_IF)
    ggplot(data=datos_if(), aes(x=DATE)) +
      geom_line(aes(y=RESULTADO_NETO_BG/1e6), col = "darkgray", size=1.0) +
      geom_point(aes(y=RESULTADO_NETO_BG/1e6, colour = cut(RESULTADO_NETO_BG/1e6, c(-Inf, 0, Inf))), size=2) + 
      scale_color_manual(name = "Resultado neto",
                         values = c("(-Inf,0]" = "red",
                                    "(0, Inf]" = "blue")) +
      theme(axis.text.x = element_text(angle = 90, vjust=0.5), panel.grid.minor = element_blank()) +
      scale_x_date(labels = date_format("%m/%Y"), breaks = "3 month") +
      theme(legend.position='none') + 
      labs(title = "RESULTADO NETO" ,
           subtitle = "Cifras en Millones de Pesos",
           caption = "Fuente: SIIOF / SMIF",
           y="Millones de Pesos",
           x="") + theme(aspect.ratio = 0.5) 
  })
  
  ## ICV
  output$ICV_2 <- renderPlotly({
    req(input$selected_IF)
    ggplot(data=datos_if(), aes(x=DATE)) +
      geom_line(aes(y=ICV), col = "#2b8cbe", size=1.0) +
      # geom_hline(yintercept = 6.5, col="green", size = 1, linetype = 2) + 
      geom_hline(yintercept = 10, col="red", size = 1, linetype = 2) + 
      geom_line(aes(y=ICVN), col = "#a6bddb", size=1.0) + 
      # geom_hline(yintercept = 3.0, col="green", size = 1, linetype = 2) + 
      geom_hline(yintercept = 3.5, col="red", size = 1, linetype = 2) + 
      theme(axis.text.x = element_text(angle = 90, vjust=0.5), panel.grid.minor = element_blank()) +
      scale_x_date(labels = date_format("%m/%Y"), breaks = "3 month") +
      # scale_color_manual(name="", 
      #                    values = c("ICV"="#2b8cbe", "ICVN"="#a6bddb")) +  # line color
      labs(title = "ICV e ICVN" ,
           subtitle = "Cifras en Porcentaje",
           caption = "Fuente: SIIOF / SMIF",
           y="Porcentaje",
           x="") + theme(aspect.ratio = 0.5) 
  })
  
  
  ## ROE
  output$ROE_2 <- renderPlotly({
    req(input$selected_IF) 
    ggplot(data=datos_if(), aes(x=DATE)) + 
      geom_line(aes(y=ROE), col = "#636363", size=1.0, linetype = 1) +
      theme(axis.text.x = element_text(angle = 90, vjust=0.5), panel.grid.minor = element_blank()) +
      scale_x_date(labels = date_format("%m/%Y"), breaks = "3 month") +
      labs(title = "ROE" ,
           subtitle = "Cifras en Porcentaje",
           caption = "Fuente: SIIOF / SMIF",
           y="Porcentaje",
           x="") + theme(aspect.ratio = 0.5) 
  })
  
  ## ROA
  output$ROA_2 <- renderPlotly({
    req(input$selected_IF)
    ggplot(data=datos_if(), aes(x=DATE)) +
      geom_line(aes(y=ROA), col = "#d95f0e", size=1.0) +
      theme(axis.text.x = element_text(angle = 90, vjust=0.5), panel.grid.minor = element_blank()) +
      scale_x_date(labels = date_format("%m/%Y"), breaks = "3 month") +
      labs(title = "ROA" ,
           subtitle = "Cifras en Porcentaje",
           caption = "Fuente: SIIOF / SMIF",
           y="Porcentaje",
           x="") + theme(aspect.ratio = 0.5) 
  })
  
  
  ## ICAP
  output$ICAP_2 <- renderPlotly({
    req(input$selected_IF) 
    ggplot(data=datos_if(), aes(x=DATE)) + 
      geom_line(aes(y=ICAP), col = "#2c7fb8", size=1.0) +
      geom_hline(yintercept = 10, col="green", size = 1, linetype = 2) + 
      geom_hline(yintercept = 8, col="red", size = 1, linetype = 2) + 
      theme(axis.text.x = element_text(angle = 90, vjust=0.5), panel.grid.minor = element_blank()) +
      scale_x_date(labels = date_format("%m/%Y"), breaks = "3 month") +
      labs(title = "ICAP",
           subtitle = "Cifras en Porcentaje",
           caption = "Fuente: SIIOF / SMIF",
           y="Porcentaje",
           x="") + theme(aspect.ratio = 0.5) 
  })
  
  ## SUFICIENCIA_EST_PREV = RESERVAS_CONSTITUIDAS/RESERVAS_PREVENTIVAS_CALC*100, 
  output$SUFICIENCIA_EST_PREV_2 <- renderPlotly({
    req(input$selected_IF) 
    ggplot(data=datos_if(), aes(x=DATE)) + 
      geom_line(aes(y=SUFICIENCIA_EST_PREV), col = "#c51b8a", size=1.0) +
      geom_hline(yintercept = 100, col="green", size = 1, linetype = 2) + 
      theme(axis.text.x = element_text(angle = 90, vjust=0.5), panel.grid.minor = element_blank()) +
      scale_x_date(labels = date_format("%m/%Y"), breaks = "3 month") +
      labs(title = "SUFICIENCIA DE ESTIMACIONES PREVENTIVAS",
           subtitle = "Cifras en Porcentaje",
           caption = "Fuente: SIIOF / SMIF",
           y="Porcentaje",
           x="") + theme(aspect.ratio = 0.5) 
  })
  
  ## COBERTURA DE RESERVAS = RESERVAS_CONSTITUIDAS/CARTERA DE CREDITO VENCIDA*100,
  output$COB_RESERVAS_2 <- renderPlotly({
    req(input$selected_IF) 
    ggplot(data=datos_if(), aes(x=DATE)) + 
      geom_line(aes(y=COBERTURA_RESERVAS), col = "#31a354", size=1.0) +
      theme(axis.text.x = element_text(angle = 90, vjust=0.5), panel.grid.minor = element_blank()) +
      scale_x_date(labels = date_format("%m/%Y"), breaks = "3 month") +
      labs(title = "COBERTURA DE RESERVAS" ,
           subtitle = "Cifras en Porcentaje",
           caption = "Fuente: SIIOF / SMIF",
           y="Porcentaje",
           x="") + theme(aspect.ratio = 0.5) 
  })
  
  # APALANC = (CAPTACION TRADICIONAL (PARA UNIONES DE CREDITO, SOFIPOS Y COOPERATIVAS) + PRESTAMOS BANCARIOS Y DE OTROS ORGANISMOS)/TOTAL CAPITAL CONTABLE
  output$APALANC_2 <- renderPlotly({
    req(input$selected_IF) 
    ggplot(data=datos_if(), aes(x=DATE)) + 
      geom_line(aes(y=APALANC), col = "#67a9cf", size=1.0) +
      theme(axis.text.x = element_text(angle = 90, vjust=0.5), panel.grid.minor = element_blank()) +
      scale_x_date(labels = date_format("%m/%Y"), breaks = "3 month") +
      labs(title = "APALANCAMIENTO",
           subtitle = "Cifras en Porcentaje",
           caption = "Fuente: SIIOF / SMIF",
           y="Porcentaje",
           x="") + theme(aspect.ratio = 0.5) 
  })
  
  ## EFICIENCIA OPERATIVA
  output$EFIC_OPER_2 <- renderPlotly({
    req(input$selected_IF) 
    ggplot(data=datos_if(), aes(x=DATE)) + 
      geom_line(aes(y=EFI_OPER), col = "#de2d26", size=1.0) +
      theme(axis.text.x = element_text(angle = 90, vjust=0.5), panel.grid.minor = element_blank()) +
      scale_x_date(labels = date_format("%m/%Y"), breaks = "3 month") +
      labs(title = "EFICIENCIA OPERATIVA" ,
           subtitle = "Cifras en Porcentaje",
           caption = "Fuente: SIIOF / SMIF",
           y="Porcentaje",
           x="") + theme(aspect.ratio = 0.5) 
  })
  
  ### TASAS Y SUS DIFERENCIAS
  output$TASAS_2 <- renderPlotly({
    req(input$selected_IF)
    ggplot(data=datos_if(), aes(x=DATE)) +
      geom_line(aes(y=TASA_A_IMP), col = "blue", size=1.0) + 
      geom_line(aes(y=TASA_P_IMP), col = "red", size=1.0) + 
      theme(axis.text.x = element_text(angle = 90, vjust=0.5), panel.grid.minor = element_blank()) +
      scale_x_date(labels = date_format("%m/%Y"), breaks = "3 month") +
      # scale_color_manual(name="", 
      #                    values = c("ICV"="#feb24c",
      #                               "ICVN"="red")) +
      labs(title = "TASA ACTIVA IMPLICITA y TASA PASIVA IMPLICITA" ,
           subtitle = "Cifras en Porcentaje",
           caption = "Fuente: SIIOF / SMIF",
           y="Porcentaje",
           x="") + theme(aspect.ratio = 0.5) 
  })
  
  # CARTERA TOTAL vs CARTERA DE EQUILIBRIO
  # CARTERA_DE_EQUILIBRIO = (TASA_DE_EQUILIBRIO*CCT)/TASA_A_IMP
  output$CARTERA_DE_EQUILIBRIO_2 <- renderPlotly({
    req(input$selected_IF)
    ggplot(data=datos_if(), aes(x=DATE)) +
      geom_line(aes(y=CARTERA_DE_EQUILIBRIO/1e6), col = "#91cf60", size=1.0) + 
      geom_line(aes(y=CCT/1e6), col = "#fc8d59", size=1.0) + 
      theme(axis.text.x = element_text(angle = 90, vjust=0.5), panel.grid.minor = element_blank()) +
      scale_x_date(labels = date_format("%m/%Y"), breaks = "3 month") +
      labs(title = "CARTERA TOTAL vs CARTERA DE EQUILIBRIO" ,
           subtitle = "Cifras en Millones de Pesos",
           caption = "Fuente: SIIOF / SMIF",
           y="Millones de pesos",
           x="") + theme(aspect.ratio = 0.5) 
  })
  
  
  ##############################################
  
  ##-------------------------------------------------------------------------------------------------##
  ##-------------------------------------------------------------------------------------------------##
  ##       R A N K I N G
  ##-------------------------------------------------------------------------------------------------##
  ##-------------------------------------------------------------------------------------------------##
  
  tablita_if <- reactive({
    req(input$selected_IF)
    tablita <- ldply(2:ncol(dat_ranking), calcular_ranking, dd=dat_ranking, IF=input$selected_IF)
    colnames(tablita) <- c("Variable", "Posicion", "Posicion_rel", "Etiqueta")
    tablita$IF <- input$selected_IF
    return(tablita)
  })
  
  ## Aqui mostramos la Tabla del ranking
  output$tablita_if_ranking <- DT::renderDataTable({
    req(input$selected_IF)
    DT::datatable(data = tablita_if(),
                  options = list(paging = FALSE), #pageLenght= 25
                  rownames = FALSE
    )
  })
  
  output$RANKING <- renderPlot({
    req(input$selected_IF)
    ggplot(data = tablita_if(), aes(x = fct_reorder(factor(Variable), Posicion_rel, .desc = TRUE), y = Posicion_rel)) +
      geom_bar(stat = "identity", position = "identity", alpha = 0.8,
               color = "#3182bd", fill = "#3182bd") + coord_flip() +
      geom_hline(yintercept = 50, col="red", size = 1, linetype = 2) + 
      theme(axis.title.x = element_blank(),
            axis.text.x  = element_blank(),
            axis.ticks.x = element_blank()) + 
      ylim(0, 103) +
      geom_text(aes(label = Etiqueta, y= Posicion_rel + 5), size = 4.5, col="black") +
      labs(title = "RANKING",
           subtitle = paste0("Para cada variable, 1 = el mejor", " ; " , paste0(length(IF_RANKING) , " = el peor." )),
           caption = "Fuente: SIIOF / SMIF",
           y="",
           x="") + theme(aspect.ratio = 0.7) 
    
    ## Ranking as a spider plot
    # output$RANKING <- renderPlot({
    #   req(input$selected_IF)
    #   ggplot(data = tablita_if(), aes(x = fct_reorder(factor(Variable), Posicion_rel, .desc = TRUE), y = Posicion_rel)) +
    #     geom_bar(stat = "identity", position = "identity", alpha = 0.8,
    #              color = "#3182bd", fill = "#3182bd") + coord_flip() +
    #     geom_hline(yintercept = 50, col="red", size = 1, linetype = 2) + 
    #     theme(axis.title.x = element_blank(),
    #           axis.text.x  = element_blank(),
    #           axis.ticks.x = element_blank()) + 
    #     ylim(0, 103) +
    #     geom_text(aes(label = Etiqueta, y= Posicion_rel + 3), size = 4.5, col="black") +
    #     labs(title = "RANKING",
    #          subtitle = paste0("Para cada variable, 1 = el mejor", " ; " , paste0(length(IF_RANKING) , " = el peor." )),
    #          caption = "Fuente: SIIOF / SMIF",
    #          y="",
    #          x="")
    
    
  })
  ### Aqui descargamos la Tabla del ranking
  # Download file
  output$download_data_ranking <- downloadHandler(
    filename = function() {
      paste0(input$selected_IF, "_ranking.csv")
    },
    content = function(file){
      write_csv(tablita_if()[,1:3], file) 
      
    }
  )
  ##-------------------------------------------------------------------------------------------------##
  ##-------------------------------------------------------------------------------------------------##
  ##       E N D     R A N K I N G
  ##-------------------------------------------------------------------------------------------------##
  ##-------------------------------------------------------------------------------------------------##
  
  ##-------------------------------------------------------------------------------------------------##
  ##-------------------------------------------------------------------------------------------------##
  ## M I S C E L A N E A    D E    G R A F I C O S
  ##-------------------------------------------------------------------------------------------------##
  ## Creamos un dataframe de las cuentas de la miscelanea de resumen
  datos_if_misc <- reactive({
    req(input$selected_IF)   # garantizamos que este seleccionado un IF

    datos_ifv_misc <- datos_if() %>% select( "DATE", "NOMBRE", `TOTAL ACTIVO`, `CARTERA DE CREDITO`, `CARTERA DE CREDITO VIGENTE`,
                                             `CARTERA DE CREDITO VENCIDA`, `(-) ESTIMACIONES PREVENTIVAS PARA RIESGOS CREDITICIOS`,
                                             `TOTAL PASIVO`, `TOTAL CAPITAL CONTABLE`, `INGRESOS POR INTERESES`,
                                             `GASTOS POR INTERESES`, `MARGEN FINANCIERO`, `ESTIMACION PREVENTIVA PARA RIESGOS CREDITICIOS`, `MARGEN FINANCIERO AJUSTADO POR RIESGOS CREDITICIOS`,
                                             `GASTOS DE ADMINISTRACION`, `RESULTADO DE LA OPERACION`, `RESULTADO_NETO_EDORES`, "ICAP",
                                             "ICV", "ICVN", "EFI_OPER", "LIQUIDEZ_ACTIVOS", "APALANC",
                                             "TASA_A_IMP", "TASA_P_IMP", "TASA_DE_EQUILIBRIO", "CARTERA_DE_EQUILIBRIO", "ROE", "ROA")
  })
  
  ### Dataframe to plot some miscelanea plots
  current_values_misc <- reactive({
    req(input$selected_IF)   # garantizamos que este seleccionado un IF
    current_date = max(datos_if_misc()$DATE)
    current_values = datos_if_misc() %>% filter(DATE == current_date)
  })
  
  ## Graficos densidades 
  output$misc_plots <- renderPlot({
    req(input$selected_IF)
    pp1 = ggplot(datos_if_misc(), aes(x=ICAP))+
      # geom_density(color="darkblue", fill="lightblue", alpha = 0.75) +
      geom_histogram(aes(y=..density.., alpha = 0.5, colour = "#999999", fill = "#999999")) +
      theme(legend.position="none") +
      geom_vline(xintercept = current_values_misc()$ICAP, col="black", size = 1, linetype = 2)

    
    pp2 = ggplot(datos_if_misc(), aes(x=ICV))+
      # geom_density(color="darkblue", fill="lightblue", alpha = 0.75) +
      geom_histogram(aes(y=..density.., alpha = 0.5, colour = "#999999", fill = "#999999")) +
      theme(legend.position="none") +
      geom_vline(xintercept = current_values_misc()$ICV, col="black", size = 1, linetype = 2)
    
    pp3 = ggplot(datos_if_misc(), aes(x = ICVN ))+
      # geom_density(color="darkblue", fill="lightblue", alpha = 0.75) +
      geom_histogram(aes(y=..density.., alpha = 0.5, colour = "#999999", fill = "#999999")) +
      theme(legend.position="none") +
      geom_vline(xintercept = current_values_misc()$ICVN, col="black", size = 1, linetype = 2)
    
    pp4 = ggplot(datos_if_misc(), aes(x = ROE ))+
      # geom_density(color="darkblue", fill="lightblue", alpha = 0.75) +
      geom_histogram(aes(y=..density.., alpha = 0.5, colour = "#999999", fill = "#999999")) +
      theme(legend.position="none") +
      geom_vline(xintercept = current_values_misc()$ROE, col="black", size = 1, linetype = 2)
    
    pp5 = ggplot(datos_if_misc(), aes(x = ROA ))+
      # geom_density(color="darkblue", fill="lightblue", alpha = 0.75) +
      geom_histogram(aes(y=..density.., alpha = 0.5, colour = "#999999", fill = "#999999")) +
      theme(legend.position="none") +
      geom_vline(xintercept = current_values_misc()$ROA, col="black", size = 1, linetype = 2)
    
    pp6 = ggplot(datos_if_misc(), aes(x = EFI_OPER ))+
      # geom_density(color="darkblue", fill="lightblue", alpha = 0.75) +
      geom_histogram(aes(y=..density.., alpha = 0.5, colour = "#999999", fill = "#999999")) +
      theme(legend.position="none") +
      geom_vline(xintercept = current_values_misc()$EFI_OPER, col="black", size = 1, linetype = 2)
    
    pp7 = ggplot(datos_if_misc(), aes(x = LIQUIDEZ_ACTIVOS ))+
      # geom_density(color="darkblue", fill="lightblue", alpha = 0.75) +
      geom_histogram(aes(y=..density.., alpha = 0.5, colour = "#999999", fill = "#999999")) +
      theme(legend.position="none") +
      geom_vline(xintercept = current_values_misc()$LIQUIDEZ_ACTIVOS, col="black", size = 1, linetype = 2)
    
    pp8 = ggplot(datos_if_misc(), aes(x = APALANC ))+
      # geom_density(color="darkblue", fill="lightblue", alpha = 0.75) +
      geom_histogram(aes(y=..density.., alpha = 0.5, colour = "#999999", fill = "#999999")) +
      theme(legend.position="none") +
      geom_vline(xintercept = current_values_misc()$APALANC, col="black", size = 1, linetype = 2)
    
    pp9 = ggplot(datos_if_misc(), aes(x = CARTERA_DE_EQUILIBRIO ))+
      # geom_density(color="darkblue", fill="lightblue", alpha = 0.75) +
      geom_histogram(aes(y=..density.., alpha = 0.5, colour = "#999999", fill = "#999999")) +
      theme(legend.position="none") +
      geom_vline(xintercept = current_values_misc()$CARTERA_DE_EQUILIBRIO, col="black", size = 1, linetype = 2)
    
    multiplot(pp1, pp2, pp3, pp4, pp5, pp6, pp7, pp8, pp9, cols=3)
    
  })
  
  ## Graficos de clusters 
  output$cluster_plot <- renderPlot({
    multiplot(pp1, pp2, cols=2)
    })

  
  ##-------------------------------------------------------------------------------------------------##
  ##-------------------------------------------------------------------------------------------------##
  ## E N D    M I S C E L A N E A    D E    G R A F I C O S
  ##-------------------------------------------------------------------------------------------------##
  
  
}

##-------------------------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------------------------##
##       E N D      S E R V E R
##-------------------------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------------------------##


# Run the application 
shinyApp(ui = ui, server = server)

