#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("ggplot2")

library(shiny)
library(shinydashboard)
library(ggplot2)


#--------------------------------------Lectura del archivo----------------------#

setwd("C:/Users/rocha/OneDrive/Documentos/Proyecto_BioinfoAplicada/Data sets/Data sets/Tuberose flower")

SetDeDatos<-read.csv("thesis_data.csv")
#SetDeDatos

#--------------------------------------Funciones--------------------------------#

# Funcion que busca dentro del dataset dos columnas dadas
find_data <- function(dataset, param1, param2) {
  
  # Encuentra los indices de las columnas que hagan match
  matching_cols <- which(colnames(dataset) == param1 | colnames(dataset) == param2)
  
  # Retorna una matriz con los datos de las dos columnas
  return(dataset[, matching_cols])
}


# Funcion que genera ScatterPlots. Parametros data = SetdeDatos 
GeneraScatter <- function(data, x_col, y_col) {
  
  # Convierte los datos a dta frame
  df <- data.frame(data)
  
  # Genera el grafico tomando como parametro las columnas de 
  p <- ggplot(df, aes(x = df[, x_col], y = df[, y_col])) +
    geom_point() +
    xlab(colnames(df)[x_col]) +
    ylab(colnames(df)[y_col])

  # retorna el plot
  return(p)
}


#------------------------------Interfaz-------------------------#

# Define la interfaz grafica
ui <- dashboardPage(
  
  # header
  dashboardHeader(title = "Agave amica"),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Información General", tabName = "Información General", icon = icon("leaf")),
      menuItem("Tab 2", tabName = "tab2", icon = icon("chart-bar")),
      menuItem("Tab 3", tabName = "tab3", icon = icon("chart-pie"))
    )
  ),
  
  # Body
  dashboardBody(
    
    # First tab
    tabItems(
      tabItem(
        tabName = "tab1",
        h1("This is the first tab"),
        p("Here is some content for the first tab.")
      ),

      # Second tab
      tabItem(
        tabName = "tab2",
        h1("This is the second tab"),
        p("Here is some content for the second tab."),
        
        ##Desplegamos el plot en la segunda tab
        plotOutput("myPlot") 
      ),

      # Third tab
      tabItem(
        tabName = "tab3",
        h1("This is the third tab"),
        p("Here is some content for the third tab.")
      )
    )
  )
)

# Define the server
server <- function(input, output) {


  
  Seleccion <- find_data(SetDeDatos, "Treatments", "Sprouting")
  #MyPlot <- GeneraScatter(Seleccion, 1, 2)
  #print(MyPlot)
  
  #Render the plot in the second tab
  output$myPlot <- renderPlot(GeneraScatter(Seleccion, 1, 2))
}

# Run the shiny app
shinyApp(ui, server)


