# install.packages(c("shiny","googlesheets4","dplyr","stringr","tidytext","wordcloud2"))

library(shiny)
library(googlesheets4)
library(dplyr)
library(stringr)
library(tidytext)
library(wordcloud2)

url_sheet <- "https://docs.google.com/spreadsheets/d/1je8tYjM17oZR76BJb6VLpdzBL86FBuvn7Tuvgv6VxNM/edit#gid=274474723"
aba <- "soluco"

ui <- fluidPage(
  tags$style("body { font-family: Arial; }"),
  h2("Nuvem de palavras — soluço"),
  p("Atualiza automaticamente a cada 5 segundos."),
  wordcloud2Output("wc", height = "650px")
)

server <- function(input, output, session) {
  
  # autenticar 1x por sessão (vai abrir navegador na primeira execução)
  observeEvent(TRUE, { gs4_auth(); }, once = TRUE)
  
  auto <- reactivePoll(
    intervalMillis = 5000, session = session,
    checkFunc = function() Sys.time(),
    valueFunc = function() {
      dados <- read_sheet(url_sheet, sheet = aba)
      
      col_resposta <- names(dados)[ncol(dados)]
      
      respostas <- dados |>
        select(resposta = all_of(col_resposta)) |>
        filter(!is.na(resposta), str_trim(resposta) != "")
      
      respostas |>
        mutate(resposta = str_to_lower(resposta),
               resposta = str_replace_all(resposta, "[^[:alpha:]À-ÿ ]", " "),
               resposta = str_squish(resposta)) |>
        unnest_tokens(word, resposta) |>
        filter(nchar(word) >= 2) |>
        count(word, sort = TRUE)
    }
  )
  
  output$wc <- renderWordcloud2({
    wordcloud2(auto(), size = 0.9)
  })
}

shinyApp(ui, server)
