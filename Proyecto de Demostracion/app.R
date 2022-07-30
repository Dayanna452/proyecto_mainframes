
library(shiny)
library(dplyr)
library(sqldf)
library(RMySQL)
library(plotly)
library(class)
library(ISLR)
library(tidyr)
library(ggplot2)
library(tsfknn)
library(ggfortify)
library(e1071)
library(psych)
library(cluster)
source("www/presentacion.R")
source("www/recoleccion.R")
source("www/transformacion.R")
source("www/modelo.R")
source("www/visualizacion.R")
source("www/interpretacion.R")


ui <- fluidPage(
  ui <- navbarPage(title = "Proyecto Ejemplo de Ciencia de Datos",
                   tabPanel("Presentacion",presentacion),
                   tabPanel( "Recoleccion",recoleccion),
                   tabPanel("Transformacion",transformacion),
                   tabPanel("Visualizacion",modelo),
                   tabPanel("Modelo",visualizacion),
                   tabPanel("Interpretacion", interpretacion)
  )
  
)


server <- function(input, output) {
  
  ########################## RECOLECCION DE DATOS
  output$contents <- renderTable({
    
    req(input$file1)
    
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header)
        
        selector<-input$nullData
        selector2<-input$duplicateData
        if(selector==FALSE&selector2==FALSE){
          return (df)
        }
        if(selector==TRUE){
           return(df %>%
              drop_na())
         
        }
        if(selector2==TRUE){
          q1<-sqldf('SELECT DISTINCT * FROM df;', drv="SQLite")
          return(q1)
        }
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
  })
  
  ######################### TRANSFORMACION
  
  file_data <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, header = TRUE,
             sep = ",")
  })

  output$resSQLDF<-renderDataTable({
    dt<-req(file_data())
    selector<-input$transSQLDF
    if(is.null(selector)){
      return(NULL)
    }
    if(selector=="1"){
      query1<-sqldf('Select NRO_PROY,BENEF_PRE, CANT_AULA from dt where CANT_AULA=20;',drv="SQLite")
      return(query1)
    }
    if(selector=="2"){
      query2<-sqldf('SELECT NRO_PROY,LIDER_PROY,DEPARTAMENTO,CANT_AULA from dt where DEPARTAMENTO="LIMA" ORDER BY CANT_AULA DESC;', drv="SQLite")
      return(query2)
    }
    if(selector=='3'){
      query3<-sqldf('select LIDER_PROY, NOMB_IE, DET_COD_MOD, count(DET_COD_MOD), DEPARTAMENTO, UGEL from dt where NOMB_IE <> "NA" and DET_COD_MOD <> "NA"  group by DET_COD_MOD having count(DET_COD_MOD) > 10 order by UGEL desc;', drv="SQLite")
      return(query3)
    }
    if(selector=='4'){
      query4<-sqldf('select LIDER_PROY, DET_COD_MOD, count(DET_COD_MOD), DEPARTAMENTO from dt where DET_COD_MOD <> "NA"  group by UBIGEO having DEPARTAMENTO="LIMA";', drv="SQLite")
      return(query4)
    }
    if(selector=='5'){
      query5<-sqldf('select NOMB_IE, PROVINCIA, DISTRITO, CAT_IE, BENEF_PRE from dt where NOMB_IE <> "NA" AND DET_COD_MOD <> "NA" group by CAT_IE having BENEF_PRE > 1000  order by BENEF_PRE desc;', drv="SQLite")
      return(query5)
    }
    if(selector=='6'){
      query6<-sqldf('select NOMB_IE, PROVINCIA, DISTRITO, CANT_AULA from dt where NOMB_IE <> "NA" AND DET_COD_MOD <> "NA" group by NOMB_IE having CANT_AULA >15  order by CANT_AULA desc;', drv="SQLite")
      return(query6)
    }
    if(selector=='7'){
      query7<-sqldf('select nomb_ie, cui, benef_pre from dt where region <> "LIMA" and NOMB_IE <> "NA"  group by ubigeo having BENEF_PRE > 500 order by cui desc;', drv = "SQLite")
      return(query7)
    }
    if(selector=="8"){
      query8<-sqldf('select nomb_ie, det_cod_mod, region, distrito from dt where distrito = "SAN JUAN DE LURIGANCHO" AND DET_COD_MOD <> "NA" group by DET_COD_MOD order by REGION;', drv = "SQLite")
      return(query8)
    }
    if(selector=="9"){
      query9<-sqldf('select departamento, benef_pre, cat_ie, count(cat_ie) from dt where BENEF_PRE > 900 group by ubigeo order by benef_pre desc;',drv = "SQLite")
      return(query9)
    }
    if(selector=="10"){
      query10<-sqldf('select lider_proy, nomb_ie, region, departamento, provincia, distrito, cui, cat_ie, sfl from dt where cant_aula > 20 group by provincia having sfl="PROPIEDAD" order by cat_ie desc;',drv = "SQLite")
      return(query10)
    }
  })
  
  output$resDPLYR<-renderDataTable({
    dt<-req(file_data())
    selector<-input$transDPLYR
    if(is.null(selector)){
      return(NULL)
    }
    if(selector=="1"){
      return (dt%>%filter(BENEF_PRE>1000&DEPARTAMENTO=='LA LIBERTAD'&CANT_AULA>20))
    }
    if(selector=="2"){
      return (dt%>%filter(grepl('ATE',DISTRITO))%>%arrange(desc(COS_INV_PRE)))
    }
    if(selector=="3"){
      return(dt%>%distinct() %>%
               select(NRO_PROY, LIDER_PROY, FECHA_VIAB, FECHA_CORTE, DEPARTAMENTO, BENEF_PRE)%>%filter(grepl('CARLOS SOTO',LIDER_PROY))%>%arrange(desc(FECHA_VIAB)))
    }
    if(selector=="4"){
      return(dt%>%filter(grepl('20190806',FECHA_VIAB))%>%select(LIDER_PROY,contains('f'))%>%slice(1))
    }
    if(selector=="5"){
      return(dt%>%select(LIDER_PROY,FECHA_VIAB,COS_INV_PRE,UGEL,BENEF_PRE,CANT_AULA)%>%group_by(LIDER_PROY)%>%slice_sample(n = 5)%>%filter(UGEL=='UGEL 04'&BENEF_PRE>1400))
    }
    if(selector=="6"){
      return(dt%>%select(NOMB_IE, REGION, DEPARTAMENTO, PROVINCIA, SFL)%>%filter(SFL=='AFECTADO EN USO'&DEPARTAMENTO=='LIMA'))
    }
    if(selector=="7"){
      return(dt%>%select(LIDER_PROY, NOMB_IE, CAT_IE, DET_COD_MOD, CANT_AULA)%>%filter(grepl('COSTA',CAT_IE)&grepl('Secundaria',DET_COD_MOD))%>%arrange(desc(CANT_AULA)))
    }
    if(selector=="8"){
      return(dt%>%select(LIDER_PROY, COD_LOC, NOMB_IE, DEPARTAMENTO, CANT_AULA, BENEF_PRE)%>%filter(CANT_AULA > 20)%>%group_by(COD_LOC)%>%arrange(desc(BENEF_PRE)))
    }
    if(selector=="9"){
      return(dt%>%select(NRO_PROY, LIDER_PROY, COORD_CART_PROY, DET_COD_MOD, NOMB_IE, SFL)%>%filter(grepl('PROPIEDAD',SFL)&DET_COD_MOD!='Primaria'&NOMB_IE!='I.E. JORGE BASADRE GROHMANN')%>%group_by(NRO_PROY))
    }
    if(selector=="10"){
      return(dt%>%select(LIDER_PROY,CAT_IE,COS_INV_PRE,UGEL,BENEF_PRE,CANT_AULA)%>%filter(between(CANT_AULA, 20, 50))%>%group_by(LIDER_PROY)%>%arrange(desc(CANT_AULA)))
    }
  })
  
  
  ################## MODELO
  
  
  output$resGGPLOT<-renderPlot({
    dt<-req(file_data())
    selector<-input$modelGGPLOT
    if(is.null(selector)){
      return(NULL)
    }
    if(selector=="1"){
      gr1<-ggplot(dt,aes(x=CANT_AULA))+geom_bar(width=0.5, colour="blue", fill="red")
      return(gr1)
    }
    if(selector=="2"){
      gr2<-ggplot(dt, aes(UGEL, fill=CAT_IE)) + geom_bar(position="dodge")
      return(gr2)
    }
    if(selector=="3"){
      gr3<-ggplot(dt, aes(CANT_AULA, BENEF_PRE)) + geom_line()
      return(gr3)
    }
    if(selector=="4"){
      gr4<-ggplot(dt, aes(BENEF_PRE)) + geom_histogram(fill="red", colour="black")
      return(gr4)
    }
    if(selector=="5"){
      sdt<-subset(dt,CAT_IE=='SIERRA')
      gr5<-ggplot(sdt, aes(x=BENEF_PRE, y=COS_INV_PRE, group=1)) + geom_line() +geom_point()
      return(gr5)
    }
    if(selector=="6"){
      gr6 <- ggplot(dt, aes(COS_INV_PRE, BENEF_PRE)) + geom_point() + stat_smooth()
      return(gr6)
    }
    if(selector=="7"){
      gr7<- ggplot(dt[dt$CAT_IE!="COSTA", ], (aes(CAT_IE, fill=LIDER_PROY))) + 
        geom_bar(aes(CAT_IE, (..count..)/sum(..count..)), position="dodge") + 
        scale_y_continuous(name = 'CANTIDAD POR REGION') + scale_fill_grey() + theme_bw()
      return(gr7)
    }
    if(selector=="8"){
      gr8<-ggplot(dt, aes(x=CAT_IE, y=BENEF_PRE)) + geom_bar(stat="identity")
      grid<-gr8 + facet_grid(.~ DET_COD_MOD=='Primaria')
      return(grid)
    }
    if(selector=="9"){
      gr9<-ggplot(dt, aes(COS_INV_PRE, CANT_AULA)) +
        geom_point() +
        stat_smooth(n = 5) +
        facet_wrap(~ CAT_IE)
      
        return(gr9)
    }
    if(selector=="10"){
      gr10<-ggplot(dt, aes(CAT_IE, CANT_AULA)) +
        geom_boxplot(notch = TRUE) + 
        stat_summary(fun.y=mean, geom="point", shape=18,
                     size=3, color="red") #
        return(gr10)
    }
  })
  
  output$resPLOTLY<-renderPlotly({
    dt<-req(file_data())
    selector<-input$modelPLOTLY
    if(is.null(selector)){
      return(NULL)
    }
    if(selector=="1"){
      fig1 <- plot_ly(data = dt, x = ~CANT_AULA, y = ~COS_INV_PRE)
      return(fig1)
    }
    if(selector=="2"){
      fig <- plot_ly(data = dt, x = ~BENEF_PRE, y = ~CANT_AULA, type = 'scatter',
                     mode = 'markers', symbol = ~CAT_IE, symbols = c('circle','x','o'),
                     color = I('black'), marker = list(size = 10))
      
      return(fig)
    }
    if(selector=="3"){
      dset<-select(dt,CANT_AULA,LIDER_PROY)%>%filter(CANT_AULA>35)
      
      fig <- plot_ly(dset, labels = ~LIDER_PROY, values = ~CANT_AULA, type = 'pie')
      fig <- fig %>% layout(title = 'Lideres de proyecto de acuerdo a la cantidad de alumnos que palntea por aula',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      return(fig)
      
    }
    if(selector=="4"){
      fig <- plot_ly(data = dt,
        type='histogram',
        x=~PIM_2022,
        bingroup=1)
      
      fig <- fig %>% add_trace(data = dt,
        type='histogram',
        x=~COS_INV_PRE,
        bingroup=1)
      
      fig <- fig %>% layout(
        barmode="overlay",
        bargap=0.1)
      
      return(fig)
    }
    if(selector=="5"){
      d <- dt[sample(nrow(dt), 100), ]
      
      fig <- plot_ly(
        d, x = ~CANT_AULA, y = ~PIM_2022,
        color = ~CANT_AULA, size = ~CANT_AULA
      )
      
      return(fig)
    }
    if(selector=="6"){
      fig2<-plot_ly(data=dt, x = ~CANT_AULA, y = ~BENEF_PRE)
      ff<-subplot(
        fig2 %>% add_markers(alpha = 0.2),
        fig2 %>% add_histogram2d()
      )
      return(ff)
    }
    if(selector=="7"){
      fig3<-plot_ly(dt, y = ~CANT_AULA, color = ~UGEL, type = 'box')
      return(fig3)
    }
    if(selector=="8"){
      fig4<-plot_ly(x = ~dt$DEPARTAMENTO, y = ~dt$COS_INV_PRE) %>% 
        add_lines() %>%
        rangeslider()
      return(fig4)
    }
    if(selector=="9"){
  
      
      data <-select(dt,CANT_AULA, COS_INV_PRE, BENEF_PRE)
      
      fig <- plot_ly(data, x = ~dt$PIM_2022)
      fig <- fig %>% add_trace(y = ~CANT_AULA, name = 'trace 0',mode = 'lines')
      fig <- fig %>% add_trace(y = ~COS_INV_PRE, name = 'trace 1', mode = 'lines+markers')
      fig <- fig %>% add_trace(y = ~BENEF_PRE, name = 'trace 2', mode = 'markers')
      
      return(fig)
    }
   
    if(selector=="10"){
      g <- list(
        scope = 'south america',
        showland = TRUE,
        landcolor = toRGB("gray95")
      )
      
      geo<-plot_geo(dt, lat = ~LATITUD, lon = ~LONGITUD) %>%
        add_markers(text = ~paste(DEPARTAMENTO,
                                  PROVINCIA,
                                  DISTRITO,
                                  paste("CANTIDAD INVERTIDA:", COS_INV_PRE),
                                  sep = "<br />"),
                    color = ~COS_INV_PRE,
                    symbol = I("square"),
                    size = I(8),
                    hoverinfo = "text") %>%
        colorbar(title = "CANTIDAD INVERTIDA PROYECTO BICENTENARIO MAYO 2022") %>%
        layout(title = 'PERU \n(POR DEPARTAMENTO)',
               geo = g)
      
      return(geo)
      
    }
  })
  
  
  ###################### ALGORITMOS
  
  output$resAlg<-renderPlot({
    dt<-req(file_data())
    dtn<-na.omit(dt)
    set<-select(dtn,NRO_PROY,BENEF_PRE,CANT_AULA)
    frset<-data.frame(set)
    mxtset<-as.matrix(frset)
    
    selector<-input$visAlg
    if(is.null(selector)){
      return(NULL)
    }
    if(selector=="1"){
      
      entrenar<-mxtset[1:70,]
      testear<-mxtset[71:140,]
      
      class_error = function(actual, predicted) {
        mean(actual != predicted)
      }
      
      k_to_try = 1:70
      err_k = rep(x = 0, times = length(k_to_try))
      
      for (i in seq_along(k_to_try)) {
        predict = knn(train = scale(entrenar[,-3]), 
                   test  = scale(testear[,-3]), 
                   cl    = entrenar[,3], 
                   k     = k_to_try[i], prob = TRUE)
        err_k[i] = class_error(testear[,3], predict)
      }
      
      plot(err_k, type = "b", col = "dodgerblue", cex = 1, pch = 20, 
           xlab = "k, number of neighbors", ylab = "classification error",
           main = "(Test) Error Rate vs Neighbors")
      
      abline(h = min(err_k), col = "darkorange", lty = 3)
      abline(h = mean(testear[,3] == "Yes"), col = "grey", lty = 2)
    }
    
  })
  
  output$resAlg2<-renderPlotly({
    dt<-req(file_data())
    selector<-input$visAlg2
    if(is.null(selector)){
      return(NULL)
    }
    
    if(selector=="1"){
      set.seed(1)
      ds<-select(dt,BENEF_PRE,CANT_AULA)
      p <- autoplot(pam(ds[-5], input$tree), frame = TRUE, frame.type = 'norm')
      alg1<-ggplotly(p)
      return(alg1)
    }
  })
  output$resAlg3<-renderPlot({
    dt<-req(file_data())
    selector<-input$visAlg3
    if(is.null(selector)){
      return(NULL)
    }
    if(selector=="1"){
      line <- lm(BENEF_PRE ~ CANT_AULA, data = dt )
      plot(dt$CANT_AULA, dt$BENEF_PRE)
      abline(line, col = "red")
    }
  })
  
}




shinyApp(ui = ui, server = server)



