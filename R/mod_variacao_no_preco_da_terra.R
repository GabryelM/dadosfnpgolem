#' variacao_no_preco_da_terra UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinyWidgets

mod_variacao_no_preco_da_terra_ui <- function(id){
  ns <- NS(id)
  
  slider_periodo <- sliderTextInput(
    ns("slider_periodo"),
    label = "Período:",
    grid = FALSE,
    force_edges = TRUE,
    selected = c("12/2018", "12/2019" ),
    choices = datas_como_string
  )
  
  botao_download_usinas <- downloadButton(
    ns("download_usinas"),
    label = "Baixar usinas"
  )
  
  tabela_crescimento <- gt_output(ns("tabela_crescimento"))
  
  tabPanel(
    title = "Variação no preço da terra",
     sidebarLayout(
      sidebarPanel(
        width = 2,
        slider_periodo,
        botao_download_usinas
      ),
      mainPanel(
        tabela_crescimento
      )
    )
  )
  
}
    
#' variacao_no_preco_da_terra Server Functions
#'
#' @noRd 
mod_variacao_no_preco_da_terra_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    crescimentos <- reactive({
      selecionado = input$slider_periodo
      datas_filtro <- str_glue("01/{selecionado}") %>% 
        dmy() 
      precos_inicio <- precos_usina %>% 
        filter(data == datas_filtro[1] ) %>% 
        select(usina, preco_inicio = preco, data ) 
      precos_fim <- precos_usina %>% 
        filter(data == datas_filtro[2] ) %>% 
        select(usina, preco_fim = preco, data ) 
      precos_inicio %>% 
        full_join(precos_fim, by = c("usina")) %>% 
        mutate(
          variacao_preco = preco_fim/preco_inicio-1
        ) %>% 
        ungroup()
    })
    
    output$tabela_crescimento <- render_gt({
      crescimentos() %>% 
        select(
          usina,
          preco_inicio,
          preco_fim,
          variacao_preco
        ) %>% 
        arrange(usina) %>% 
        gt() %>% 
        fmt_percent(
          columns = vars(variacao_preco),
          decimals = 2
        ) %>% 
        tab_header(
          title = "Variação dos preços da terra (R$/ha)",
          subtitle = str_glue("Período: {input$slider_periodo[1]} a {input$slider_periodo[2]}")
        ) %>% 
        fmt_currency(
          columns = vars(preco_inicio, preco_fim),
          currency = "BRL",
          sep_mark = ".",
          dec_mark = ","
        ) %>% 
        cols_label(
          variacao_preco = html("% Preço"),
          preco_inicio = html(input$slider_periodo[1]),
          preco_fim = html(input$slider_periodo[2])
        )
    })
    
    output$download_usinas <- downloadHandler(
      filename = "variacaoprecos.xlsx",
      content = function(file) {
        write_xlsx(crescimentos(), file)
      }
    )
    
  })
}
    
## To be copied in the UI
# mod_variacao_no_preco_da_terra_ui("variacao_no_preco_da_terra_ui_1")
    
## To be copied in the server
# mod_variacao_no_preco_da_terra_server("variacao_no_preco_da_terra_ui_1")
