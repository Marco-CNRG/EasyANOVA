# ---- Instalar automáticamente paquetes faltantes ----
paquetes <- c("shiny", "tidyverse", "agricolae", "tibble")

paquetes_faltantes <- setdiff(paquetes, rownames(installed.packages()))

if (length(paquetes_faltantes) > 0) {
  install.packages(paquetes_faltantes, dependencies = TRUE)
}

invisible(lapply(paquetes, require, character.only = TRUE))
 ###########################################################################

library(shiny)
library(tidyverse)
library(agricolae)
library(tibble)

ui <- fluidPage(
  titlePanel("EasyANOVA"),
  
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        h4("Carga del Archivo"),
        fileInput("archivo", "Carga el archivo TXT, CSV o TSV",
                  accept = c(".csv", ".tsv", ".txt", "text/csv", "text/tab-separated-values", "text/plain"))
      ),
      wellPanel(
        h4("Gráficos: Selección de Variables"),
        uiOutput("var_select_ui")
      ),
      wellPanel(
        h4("Selecciona las variables para ANOVA"),
        uiOutput("var_select_ui_anova")
      )
    ),
    
    mainPanel(
      wellPanel(h4("Contenido del Archivo"), tableOutput("contenido")),
      wellPanel(h4("Gráficas"), plotOutput("grafico")),
      wellPanel(h4("Estadísticas básicas"), tableOutput("basicas")),
      wellPanel(h4("Resultado ANOVA"), verbatimTextOutput("anova")),
      wellPanel(h4("Comparación múltiple de medias por Tukey"), tableOutput("tukey")),
      wellPanel(h4("Gráfica de Tukey"), plotOutput("plot_tukey")),
      wellPanel(h4("Comparación múltiple de medias por Duncan"), tableOutput("duncan")),
      wellPanel(h4("Gráfica de Duncan"), plotOutput("plot_duncan"))
    )
  )
)

server <- function(input, output) {
  
  data <- reactive({
    req(input$archivo)
    path <- input$archivo$datapath
    
    lines <- readLines(path, n = 5, warn = FALSE)
    count_delim <- function(delim) {
      mean(sapply(lines, function(line) length(strsplit(line, delim)[[1]])))
    }
    
    counts <- c(
      comma = count_delim(","),
      tab = count_delim("\t"),
      semicolon = count_delim(";")
    )
    
    best_delim <- names(which.max(counts))
    
    df <- switch(best_delim,
                 comma = read_csv(path, show_col_types = FALSE),
                 tab = read_tsv(path, show_col_types = FALSE),
                 semicolon = read_delim(path, delim = ";", show_col_types = FALSE)
    )
    
    return(df)
  })
  
  output$contenido <- renderTable({
    head(data())
  })
  
  output$var_select_ui <- renderUI({
    req(data())
    vars <- names(data())
    tagList(
      selectInput("eje_X", "Variable en eje X:", choices = vars),
      selectInput("eje_Y", "Variable en eje Y:", choices = vars),
      selectInput("color", "Color (relleno):", choices = vars),
      selectInput("tipo", "Tipo de gráfico:", choices = list("Barras" = "barras", "Boxplot" = "boxplot", "Jitter" = "jitter"))
    )
  })
  
  output$var_select_ui_anova <- renderUI({
    req(data())
    vars <- names(data())
    tagList(
      selectInput("factor", "Variable independiente:", choices = vars),
      selectInput("dependiente", "Variable dependiente:", choices = vars)
    )
  })
  
  # ---- GRÁFICO PRINCIPAL (eje X numérico -> factor) ----
  output$grafico <- renderPlot({
    req(data(), input$eje_X, input$eje_Y, input$color, input$tipo)
    df <- data()
    
    # Verificar que Y sea numérica
    if (!is.numeric(df[[input$eje_Y]])) return()
    
    # Si X es numérica, convertirla a factor para el gráfico
    if (is.numeric(df[[input$eje_X]])) {
      df[[input$eje_X]] <- as.factor(df[[input$eje_X]])
    }
    
    if (input$tipo == "barras") {
      ggplot(df, aes_string(x = input$eje_X, y = input$eje_Y, fill = input$color)) +
        geom_bar(stat = "identity", position = "dodge")
    } else if (input$tipo == "boxplot") {
      ggplot(df, aes_string(x = input$eje_X, y = input$eje_Y, fill = input$color)) +
        geom_boxplot()
    } else {
      ggplot(df, aes_string(x = input$eje_X, y = input$eje_Y, color = input$color)) +
        geom_jitter()
    }
  })
  
  output$basicas <- renderTable({
    req(data(), input$factor, input$dependiente)
    df <- data()
    
    if (!is.numeric(df[[input$dependiente]])) {
      return(tibble(Advertencia = "La variable seleccionada no es numérica. No se pueden calcular estadísticas."))
    }
    
    mode_func <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    
    df %>%
      group_by(!!sym(input$factor)) %>%
      summarise(
        Media = mean(!!sym(input$dependiente), na.rm = TRUE),
        Mediana = median(!!sym(input$dependiente), na.rm = TRUE),
        Moda = mode_func(!!sym(input$dependiente)),
        DE = sd(!!sym(input$dependiente), na.rm = TRUE),
        Varianza = var(!!sym(input$dependiente), na.rm = TRUE),
        Q1 = quantile(!!sym(input$dependiente), 0.25, na.rm = TRUE),
        Q2 = quantile(!!sym(input$dependiente), 0.5, na.rm = TRUE),
        Q3 = quantile(!!sym(input$dependiente), 0.75, na.rm = TRUE)
      )
  })
  
  fit_model <- reactive({
    req(data(), input$factor, input$dependiente)
    df <- data()
    if (!is.numeric(df[[input$dependiente]])) return(NULL)
    df[[input$factor]] <- as.factor(df[[input$factor]])
    aov(as.formula(paste(input$dependiente, "~", input$factor)), data = df)
  })
  
  tukey_results <- reactive({
    model <- fit_model()
    if (is.null(model)) return(NULL)
    HSD.test(model, input$factor, group = TRUE)
  })
  
  duncan_results <- reactive({
    model <- fit_model()
    if (is.null(model)) return(NULL)
    duncan.test(model, input$factor, group = TRUE)
  })
  
  output$anova <- renderPrint({
    model <- fit_model()
    if (is.null(model)) {
      cat("La variable seleccionada no es numérica. No se puede realizar ANOVA.")
    } else {
      summary(model)
    }
  })
  
  output$tukey <- renderTable({
    tukey <- tukey_results()
    if (is.null(tukey)) return(tibble(Advertencia = "No se puede calcular Tukey: variable dependiente no numérica."))
    cbind(Grupo = rownames(tukey$groups), tukey$groups)
  })
  
  output$duncan <- renderTable({
    duncan <- duncan_results()
    if (is.null(duncan)) return(tibble(Advertencia = "No se puede calcular Duncan: variable dependiente no numérica."))
    cbind(Grupo = rownames(duncan$groups), duncan$groups)
  })
  
  output$plot_tukey <- renderPlot({
    df <- data()
    tukey <- tukey_results()
    if (is.null(tukey)) return()
    
    groups <- as.data.frame(tukey$groups) %>%
      rownames_to_column(var = "grupo") %>%
      mutate(grupo = as.character(grupo))
    
    medias <- df %>%
      group_by(!!sym(input$factor)) %>%
      summarise(
        Avg = mean(!!sym(input$dependiente), na.rm = TRUE),
        se = sd(!!sym(input$dependiente), na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      ) %>%
      mutate(grupo = as.character(!!sym(input$factor)))
    
    df_plot <- left_join(medias, groups, by = "grupo")
    
    ggplot(df_plot, aes(x = grupo, y = Avg)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      geom_errorbar(aes(ymin = Avg - se, ymax = Avg + se), width = 0.2) +
      geom_text(aes(label = groups, y = Avg + se), vjust = -0.5, size = 5) +
      theme_minimal()
  })
  
  output$plot_duncan <- renderPlot({
    df <- data()
    duncan <- duncan_results()
    if (is.null(duncan)) return()
    
    groups <- as.data.frame(duncan$groups) %>%
      rownames_to_column(var = "grupo") %>%
      mutate(grupo = as.character(grupo))
    
    medias <- df %>%
      group_by(!!sym(input$factor)) %>%
      summarise(
        Avg = mean(!!sym(input$dependiente), na.rm = TRUE),
        se = sd(!!sym(input$dependiente), na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      ) %>%
      mutate(grupo = as.character(!!sym(input$factor)))
    
    df_plot <- left_join(medias, groups, by = "grupo")
    
    ggplot(df_plot, aes(x = grupo, y = Avg)) +
      geom_bar(stat = "identity", fill = "salmon") +
      geom_errorbar(aes(ymin = Avg - se, ymax = Avg + se), width = 0.2) +
      geom_text(aes(label = groups, y = Avg + se), vjust = -0.5, size = 5) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
