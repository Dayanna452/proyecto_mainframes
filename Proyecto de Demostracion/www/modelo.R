modelo<-{navlistPanel(
  tabPanel("Graficos con GGPLOT", 
           h4("GGPLOT"),selectInput("modelGGPLOT","Graficos 1",choices = c("Consulta 1"='1',"Consulta 2"='2',"Consulta 3"='3', "Consulta 4"='4', "Consulta 5"='5', "Consulta 6"='6', "Cantidad por región según líder de proyecto excluyendo la costa"='7', "Consulta 8"='8', "Consulta 9"='9', "Consulta 10"='10')),
           plotOutput("resGGPLOT")
           
           
  ),
  tabPanel("Graficos con PLOTLY", 
           h4("PLOTLY"),selectInput("modelPLOTLY","Graficos 2",choices = c("Consulta 1"='1',"Consulta 2"='2',"Consulta 3"='3', "Consulta 4"='4', "Consulta 5"='5', "Consulta 6"='6', "Consulta 7"='7', "Consulta 8"='8', "Consulta 9"='9', "Costo de inversión por cada departamento del país en el proyecto"='10')),
           plotlyOutput("resPLOTLY")
           
  )
)}