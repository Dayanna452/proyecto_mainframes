modelo<-{navlistPanel(
  tabPanel("Graficos con GGPLOT", 
           h4("GGPLOT"),selectInput("modelGGPLOT","Graficos 1",choices = c("Consulta 1"='1',"Consulta 2"='2',"Consulta 3"='3', "Consulta 4"='4', "Consulta 5"='5')),
           plotOutput("resGGPLOT")
           
           
  ),
  tabPanel("Graficos con PLOTLY", 
           h4("PLOTLY"),selectInput("modelPLOTLY","Graficos 2",choices = c("Consulta 1"='1',"Consulta 2"='2',"Consulta 3"='3', "Consulta 4"='4', "Consulta 5"='5')),
           plotlyOutput("resPLOTLY")
           
  )
)}