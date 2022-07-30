transformacion<-{
  navlistPanel(
    tabPanel("Consultas en sqldf", 
             h4("SQLDF"),selectInput("transSQLDF","Consultas exploratorias 1",choices = c("Datos por una cantidad por alumnos de 20"='1',"Datos del departamento LIMA organizados descendentemente por la cantidad por aula"='2',"Datos agrupados por detalle segun codigo modular en  una cuenta mayor a 10 ordenados descendentemente por la UGEL"='3', 
                                                                                          "Agrupamiento de datos por el ubigeo y departamento de Lima"='4', "Consulta 5"='5', "Consulta 6"='6', "Consulta 7"='7', "Consulta 8"='8', "Consulta 9"='9',"Consulta 10"='10')),
             dataTableOutput("resSQLDF")
             
             
    ),
    tabPanel("Consultas en dplyr", 
             h4("DPLYR"),selectInput("transDPLYR","Consultas exploratorias 2",choices = c("Consulta 1"='1',"Consulta 2"='2',"Consulta 3"='3', "Fecha de inicio 20190806, el primer líder de proyectos que contengan f "='4', "Consulta 5"='5', "Consulta 6"='6', "Consulta 7"='7', "Consulta 8"='8', "Consulta 9"='9',"Consulta 10"='10')),
             dataTableOutput("resDPLYR")
             
    )
  )
}