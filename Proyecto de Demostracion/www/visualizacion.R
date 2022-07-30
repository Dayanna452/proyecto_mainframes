visualizacion<-{
  navlistPanel(
    tabPanel("MODELOS: KNN",h4("KNN"),selectInput("visAlg","Graficos",choices = c("KNN"='1')),  
             sliderInput("knn","Seleccione K:", min = 2, max = 5, value = 3),
             plotOutput("resAlg")
    ),
    tabPanel("MODELOS: K-MEANS",h4("K-MEANS"),selectInput("visAlg2","Graficos",choices = c("K-MEANS"='1')),  
             sliderInput("tree","Clusters:", min = 1, max = 5, value = 3),
             plotlyOutput("resAlg2")
    ),
    tabPanel("MODELOS: Regresión Lineal",h4("Regresión Lineal"),selectInput("visAlg3","Graficos",choices = c("REGRESION LINEAL"='1')),  
             plotOutput("resAlg3")
    ),
  )


}