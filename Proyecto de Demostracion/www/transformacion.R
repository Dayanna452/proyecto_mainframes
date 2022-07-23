transformacion<-{
  navlistPanel(
    tabPanel("Consultas en sqldf", 
             h4("SQLDF"),selectInput("transSQLDF","Consultas exploratorias 1",choices = c("Consulta 1"='1',"Consulta 2"='2',"Consulta 3"='3', "Consulta 4"='4', "Consulta 5"='5')),
             dataTableOutput("resSQLDF")
             
             
    ),
    tabPanel("Consultas en dplyr", 
             h4("DPLYR"),selectInput("transDPLYR","Consultas exploratorias 2",choices = c("Consulta 1"='1',"Consulta 2"='2',"Consulta 3"='3', "Consulta 4"='4', "Consulta 5"='5')),
             dataTableOutput("resDPLYR")
             
    )
  )
}