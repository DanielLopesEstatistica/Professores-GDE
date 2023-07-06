library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(readxl)

dados = read_excel("DadosCompletos.xlsx")
dados$Ranking = 1:nrow(dados)
dados = dados[,c(6,1:5)]

plotHist = function(dados, instituto = NULL, professor = NULL){
  if(!is.null(instituto)){
    dados = dados %>% filter(Instituto == instituto)
  }
  dados$cor = 1
  if(!is.null(professor)){
    numero = match(professor, dados$Nome)
    numero = numero[1] 
    nota = dados[numero,4] |> as.double()
    score = dados[numero,6] |> as.double()
    dados$cor = ifelse(dados$`Score (1-5)` > score, 0, 1) |> as.factor()
    
    # Adiciona o texto com informações do professor
    quartil = as.integer(ecdf(dados$`Score (1-5)`)(score) * 4)
    texto = paste("Professor:", professor,
                  "\nMédia no GDE:", nota,
                  "\nScore (1-5):", score,
                  "\nPosição:", numero, "/", nrow(dados),
                  "\nPorcentagem:", round((1-numero/nrow(dados)) * 100,2), "%")
    plotfinal = ggplot(dados)+
      geom_histogram(aes(x = `Score (1-5)`, fill = cor))+
      geom_vline(xintercept = score)+
      theme_classic()+
      labs(y = "Contagem")+
      theme(legend.position = "none")+
      geom_text(aes(x = 2.5, y = 500, label = texto), color = "black", size = 7)
  }
  else{
    nota = NULL
    plotfinal = ggplot(dados)+
      geom_histogram(aes(x = `Score (1-5)`, fill = cor))+
      theme_classic()+
      labs(y = "Contagem")+
      theme(legend.position = "none")
  }
  
  return(plotfinal)
}

tab_explicacao <- column(12,
                         h1("Sobre"),
                         h3("O presente Dashboard foi feito a partir de dados recolhidos do GDE (https://grade.daconline.unicamp.br/) para os votos de 1 a 5 sobre determinado professor por alunos que cursaram uma matéria com o mesmo. As informações utilizadas foram a média e a quantidade de votos que um professor recebeu ao longo dos anos em diversas matérias."),
                         h3("Como o número de votos em cada professor varia muito, uma estatística adicional, chamada de 'Score', foi introduzida para ordenar as avaliações. O score utiliza do princípio da média Bayesiana das notas dos professores, utilizando o número de votos como parâmetro relevante para a nota final. O score pode ser calculado como:"),
                         h3("Score[i] = W[i]*A[i] + (1 - W[i]) * S"),
                         h3("W[i] = M[i]/(M[i] + Mavg)"),
                         h3("Onde W[i] é o peso da i-ésima observação, A é sua média aritmética encontrada no GDE, S é a média aritmética de todas as notas, M[i] é o número de votos e Mavg é a média do número de votos de todo o sistema."),
                         h3("Os dados não englobam todos os professores da Unicamp pois o GDE não possui notas para todos eles. Além disso, alguns dos professores no banco de dados já se aposentaram.")
)

ui <- dashboardPage(
  dashboardHeader(title = "Ranking"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Pesquisar Scores", tabName = "plot"),
      menuItem("Tabela de Dados", tabName = "tabela"),
      menuItem("Sobre", tabName = "Explicação")
    )
  ),
  dashboardBody(
    tags$style(".scrollable-content { overflow-x: auto; }"),
    div(class = "scrollable-content",
      tabItems(
        tabItem(
          tabName = "plot",
          fluidRow(
            column(
              width = 6,
              radioButtons(
                inputId = "select_type",
                label = "Comparar",
                choices = c("Professores", "Institutos"),
                selected = "Professores"
              )
            ),
            column(
              width = 6,
              uiOutput(outputId = "select_input")
            )
          ),
          fluidRow(
            column(
              width = 12,
              plotOutput(outputId = "histogram1", width = "800px")
            )
          )
        ),
        tabItem(
          tabName = "tabela",
          fluidRow(
            column(
              width = 12,
              dataTableOutput(outputId = "table1")
            )
          )
        ),
        tabItem(
          tabName = "Explicação",
          fluidPage(
            fluidRow(
              tab_explicacao
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$select_input <- renderUI({
    if (input$select_type == "Professores") {
      selectizeInput(
        inputId = "professor",
        label = "Professor",
        choices = c("", unique(dados$Nome) |> sort()),
        multiple = FALSE
      )
    } else if (input$select_type == "Institutos") {
      selectizeInput(
        inputId = "instituto",
        label = "Instituto",
        choices = c("", unique(dados$Instituto)[-3] |> sort()),
        multiple = FALSE
      )
    }
  })
  
  output$histogram1 <- renderPlot({
    if (is.null(input$instituto)) {
      param2 <- ""
    } else {
      param2 <- input$instituto
    }
    if (is.null(input$professor)) {
      param1 <- ""
    } else {
      param1 <- input$professor
    }
    
    if(input$select_type == "Professores"){
      param2 = ""
    }
    else{
      param1 = ""
    }
    
    if (param1 == "" && param2 == "") {
      plotHist(dados)
    } else {
      if (param1 != "") {
        plotHist(dados, professor = param1)
      } else if (param2 != "") {
        plotHist(dados, instituto = param2)
      }
    }
  })
  
  output$table1 <- renderDataTable({
    dados
  })
  
  observeEvent(input$select_type, {
    updateSelectizeInput(session, "professor", selected = "")
    updateSelectizeInput(session, "instituto", selected = "")
  })
  
  observeEvent(input$professor, {
    updateSelectizeInput(session, "instituto", selected = "")
  })
  
  observeEvent(input$instituto, {
    updateSelectizeInput(session, "professor", selected = "")
  })
}

shinyApp(ui, server)