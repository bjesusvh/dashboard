##-------------------------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------------------------##
## AYUDA RANKING
##-------------------------------------------------------------------------------------------------##
average_vars_ranking = colMeans(dat_ranking[,-1], na.rm = TRUE)/1e6
valores_vars_ranking_ifs = dat_ranking_prima[5,-1]/1e6

View(medias_by_ifs)




##-------------------------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------------------------##
## END AYUDA RANKING
##-------------------------------------------------------------------------------------------------##





datos_pca <- dat %>% select( "DATE", "NOMBRE", `TOTAL ACTIVO`, `CARTERA DE CREDITO`, `CARTERA DE CREDITO VIGENTE`,
                                   `CARTERA DE CREDITO VENCIDA`, `(-) ESTIMACIONES PREVENTIVAS PARA RIESGOS CREDITICIOS`,
                                   `TOTAL PASIVO`, `TOTAL CAPITAL CONTABLE`, `INGRESOS POR INTERESES`,
                                   `GASTOS POR INTERESES`, `MARGEN FINANCIERO`, `ESTIMACION PREVENTIVA PARA RIESGOS CREDITICIOS`, `MARGEN FINANCIERO AJUSTADO POR RIESGOS CREDITICIOS`,
                                   `GASTOS DE ADMINISTRACION`, `RESULTADO DE LA OPERACION`, `RESULTADO_NETO_EDORES`,
                                   "ICV", "ICVN", "EFI_OPER", "LIQUIDEZ_ACTIVOS", "APALANC",
                                   "TASA_A_IMP", "TASA_P_IMP", "TASA_DE_EQUILIBRIO", "CARTERA_DE_EQUILIBRIO", "ROE")



datos_last_year = datos_pca %>% filter(DATE == "2017-12-01") %>% select(-DATE) %>% column_to_rownames("NOMBRE") %>% scale()
datos_current_month = datos_pca %>% filter(DATE == max(datos_pca$DATE)) %>% select(-DATE) %>% column_to_rownames("NOMBRE") %>% scale()


km_last_year <- kmeans(datos_last_year, 4, nstart = 25)
km_current_m <- kmeans(datos_current_month, 4, nstart = 25)

pp1 = fviz_cluster(km_last_year,
                   data = datos_last_year,
                   ellipse.type = "convex",
                   palette = "jco",
                   ggtheme = theme_bw(),
                   labelsize = 7, 
                   repel = TRUE)

pp2 = fviz_cluster(km_current_m,
                   data = datos_current_month,
                   ellipse.type = "convex",
                   palette = "jco",
                   ggtheme = theme_bw(),
                   labelsize = 7, 
                   repel = TRUE)

multiplot(pp1, pp2)




names_data_pca = c("TOTAL_ACTIVO",	"CC",	"CCVIG",	"CCVEN",	"EST_PREV_RCRED",	"TOTAL_PASIVO",
                   "TOTAL_CAPITAL_CONTABLE",	"INGRESOS_POR_INTERESES",	"GASTOS_POR_INTERESES",
                   "MARGEN_FINANCIERO",	"EST_PREV_RCRED2",	"MAR_FIN_AJUS_RCRED",	"GASTOS_DE_ADMON",
                   "RES_OPER",	"RESULTADO_NETO",	"ICV",	"ICVN",	"EFI_OPER",	"LIQ_ACTIVOS",
                   "APALANC",	"TASA_A_IMP",	"TASA_P_IMP",	"TASA_DE_EQUILIBRIO",	"CARTERA_DE_EQUILIBRIO",	"ROE")

# colnames(last_year) = names_data_pca
# colnames(current_mes) = names_data_pca

# 
# imputed_last_year = last_year %>% mice(m=5, maxit = 50, method = 'pmm', seed = 500) %>% scale()
# summary(imputed_Data)

library(ggfortify) # for autplot support for the clustering
library(cluster) # for pam
library(ggrepel) # for geom_text_repel
library(kernlab) # for kpca function

cl <- pam(last_year, 4)

autoplot(cl, frame = TRUE, frame.type = 'norm', loadings.label = TRUE,
         loadings.colour = "black", loadings.label.colour = "black")

autoplot(cl, frame = TRUE, frame.type = 'norm', label.repel = TRUE,
         label = TRUE, label.colour = "black", shape = FALSE)


# use the default of sigma = 0.1 because it works well
pca.k <- kpca(as.matrix(last_year), kpar = list(sigma = 0.1))
plot(pca.k@eig) # eigenvalues
pc <- pca.k@pcv


kpca.df <- data.frame("Label" = row.names(last_year), "Grupo"=factor(cl$clustering), 
                      "PC1" = pc[,1], "PC2" = pc[,2])
ggplot(kpca.df, aes(label = Label, x = PC1, y = PC2, color = Grupo)) + 
  geom_text_repel()


ggplot(last_year, aes(label = Label, x = PC1, y = PC2, color = Grupo)) + 
  geom_text_repel()



res.km1 <- eclust(last_year, "kmeans", 3, nstart = 25)
res.km2 <- eclust(current_mes, "kmeans", 3, nstart = 25)

multiplot(res.km1, res.km2)

##-------------------------------------------------------------------------------------------------##
##-------------------------------------------------------------------------------------------------##
## ANALISIS POR TIPO IF - DESCRIPTIVE STATISTICS
##-------------------------------------------------------------------------------------------------##


df_if = dat %>% filter(CLAVE == "948") %>% select("DATE", "ICAP", "ICV", "ICVN", "ROE", "ROA", "EFI_OPER", "LIQUIDEZ_ACTIVOS", "APALANC",
                   "TASA_A_IMP", "TASA_P_IMP", "TASA_DE_EQUILIBRIO", "CARTERA_DE_EQUILIBRIO") %>% as.data.frame()

current_date = max(df_if$DATE)
current_values = df_if %>% filter(DATE == current_date)

pp1 = ggplot(df_if, aes(x=ICAP))+
  geom_density(color="darkblue", fill="lightblue", alpha = 0.75) +
  geom_vline(xintercept = current_values$ICAP, col="black", size = 1, linetype = 2)


pp2 = ggplot(df_if, aes(x=ICV))+
  geom_density(color="darkblue", fill="lightblue", alpha = 0.75) +
  geom_vline(xintercept = current_values$ICV, col="black", size = 1, linetype = 2)

pp3 = ggplot(df_if, aes(x = ICVN ))+
  geom_density(color="darkblue", fill="lightblue", alpha = 0.75) +
  geom_vline(xintercept = current_values$ICVN, col="black", size = 1, linetype = 2)

pp4 = ggplot(df_if, aes(x = ROE ))+
  geom_density(color="darkblue", fill="lightblue", alpha = 0.75) +
  geom_vline(xintercept = current_values$ROE, col="black", size = 1, linetype = 2)

pp5 = ggplot(df_if, aes(x = ROA ))+
  geom_density(color="darkblue", fill="lightblue", alpha = 0.75) +
  geom_vline(xintercept = current_values$ROA, col="black", size = 1, linetype = 2)


pp6 = ggplot(df_if, aes(x = EFI_OPER ))+
  geom_density(color="darkblue", fill="lightblue", alpha = 0.75) +
  geom_vline(xintercept = current_values$EFI_OPER, col="black", size = 1, linetype = 2)


pp7 = ggplot(df_if, aes(x = LIQUIDEZ_ACTIVOS ))+
  geom_density(color="darkblue", fill="lightblue", alpha = 0.75) +
  geom_vline(xintercept = current_values$LIQUIDEZ_ACTIVOS, col="black", size = 1, linetype = 2)

pp8 = ggplot(df_if, aes(x = APALANC ))+
  geom_density(color="darkblue", fill="lightblue", alpha = 0.75) +
  geom_vline(xintercept = current_values$APALANC, col="black", size = 1, linetype = 2)

pp9 = ggplot(df_if, aes(x = EFI_OPER ))+
  geom_density(color="darkblue", fill="lightblue", alpha = 0.75) +
  geom_vline(xintercept = current_values$EFI_OPER, col="black", size = 1, linetype = 2)

multiplot(pp1, pp2, pp3, pp4, pp5, pp6, pp7, pp8, pp9, cols=3)



# df_stat_long = melt(df_statistics, id.vars = c("CLAVE", "INDICADORA"),  na.rm = TRUE)
# 
# dt_s = df_stat_long %>% filter(variable == "EFI_OPER")

# Density plots with semi-transparent fill
ggplot(df_stat_long, aes(x=value, fill=INDICADORA)) + geom_density(alpha=.3) +
  facet_grid(facets = variable~., scales = "free") 





df_statistics_melted = melt(df_statistics)

ggplot(df_statistics_melted, aes(y=value)) + geom_boxplot(outlier.colour="red")+
  facet_grid(facets = variable~TIPO_IF, scales = "free") 


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
  id_date_selected <- c("2012-12", "2013-12","2014-12", "2015-12", "2016-12", "2017-12", last_month)
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