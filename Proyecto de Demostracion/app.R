
library(shiny)
library(dplyr)
library(sqldf)
library(RMySQL)
library(plotly)
library(ggplot2)
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
                   tabPanel("Modelo",modelo),
                   tabPanel("Visualizacion",visualizacion),
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
       
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    selector<-input$cleaner
    if(is.null(selector)){
      return(df)
    }
    if(selector=="1"){
      delete.na <- function(df, n=0) {
        df[rowSums(is.na(df)) <= n,]
      }
      return (delete.na(df))
    }
    if(selector=="2"){
      return(datos[!duplicated(datos), ])
    }
    
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
      query3<-sqldf('select LIDER_PROY, NOMB_IE, DET_COD_MOD, count(DET_COD_MOD), DEPARTAMENTO, UGEL from dt where NOMB_IE <> "FALSO" AND DET_COD_MOD <> "FALSO"  group by DET_COD_MOD having count(DET_COD_MOD) > 10 order by UGEL desc;', drv="SQLite")
      return(query3)
    }
    if(selector=='4'){
      query4<-sqldf('select LIDER_PROY, DET_COD_MOD, count(DET_COD_MOD), DEPARTAMENTO from dt where DET_COD_MOD <> "FALSO"  group by UBIGEO having DEPARTAMENTO="LIMA";', drv="SQLite")
      return(query4)
    }
    if(selector=='5'){
      query5<-sqldf('select NOMB_IE, PROVINCIA, DISTRITO, CAT_IE, BENEF_PRE from dt where NOMB_IE <> "FALSO" AND DET_COD_MOD <> "FALSO" group by CAT_IE having BENEF_PRE > 1000  order by BENEF_PRE desc;', drv="SQLite")
      return(query5)
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
      gr3<-ggplot(dt, aes(BENEF_PRE)) + geom_histogram(fill="red", colour="black")
      return(gr3)
    }
    if(selector=="4"){
      gr4<-ggplot(dt, aes(x=CAT_IE, y=BENEF_PRE)) + geom_bar(stat="identity")
      grid<-gr4 + facet_grid(.~ DET_COD_MOD=='Primaria')
      return(grid)
    }
    if(selector=="5"){
      sdt<-subset(dt,CAT_IE=='SIERRA')
      gr5<-ggplot(sdt, aes(x=BENEF_PRE, y=COS_INV_PRE, group=1)) + geom_line() +geom_point()
      return(gr5)
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
      fig2<-plot_ly(data=dt, x = ~CANT_AULA, y = ~BENEF_PRE)
      ff<-subplot(
        fig2 %>% add_markers(alpha = 0.2),
        fig2 %>% add_histogram2d()
      )
        return(ff)
    }
    if(selector=="3"){
      fig3<-plot_ly(dt, y = ~CANT_AULA, color = ~UGEL, type = 'box')
      return(fig3)
    }
    if(selector=="4"){
      fig4<-plot_ly(x = ~dt$DEPARTAMENTO, y = ~dt$COS_INV_PRE) %>% 
        add_lines() %>%
        rangeslider()
        return(fig4)
    }
    if(selector=="5"){
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
    
    ###################### ALGORITMOS

    # output$resAlg<-renderPlotly({
    #   selector<-input$visAlg
    #   if(is.null(selector)){
    #     return(NULL)
    #   }
    #   if(selector=="1"){
    #     set.seed(1)
    #     ds<-select(dt,BENEF_PRE,CANT_AULA)
    #     p <- autoplot(pam(ds[-5], 3), frame = TRUE, frame.type = 'norm')
    #     alg1<-ggplotly(p)
    #     return(alg1)
    #   }
    #   if(selector=="2"){
    #   }
    # })
  })
  
}



shinyApp(ui = ui, server = server)



