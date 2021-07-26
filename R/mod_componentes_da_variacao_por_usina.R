#' componentes_da_variacao_por_usina UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_componentes_da_variacao_por_usina_ui <- function(id){
  ns <- NS(id)

  slider_periodo_componentes <- sliderTextInput(
    ns("slider_periodo_componentes"), 
    label = "Período:", 
    grid = FALSE, 
    force_edges = TRUE,
    selected = c("12/2018", "12/2019" ),
    choices = datas_como_string
  )
    
  select_usina_componentes <- selectInput(
    ns("select_usina_componentes"),
    label = "Usina:",
    choices = preco_pedacos_usina$usina %>% unique() %>% sort(),
    multiple = FALSE
  )
    
  botao_download_componentes <- downloadButton(
    ns("download_componentes"),
    label = "Baixar componentes")
 
  tabPanel(
    title = "Componentes da variação por usina",
    sidebarLayout(
      sidebarPanel(
        width = 2,
        select_usina_componentes,
        slider_periodo_componentes,
        botao_download_componentes
      ),
      mainPanel(
        gt_output(ns("tabela_componentes"))
      )
    )
  )

}
    
#' componentes_da_variacao_por_usina Server Functions
#'
#' @noRd 
mod_componentes_da_variacao_por_usina_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    componentes <- reactive({
      print("fará componentes")
      selecionado = input$slider_periodo_componentes
      datas_filtro <- str_glue("01/{selecionado}") %>%
        dmy()
      componentes_precos_inicio <- preco_pedacos_usina %>%
        filter(data == datas_filtro[1] ) %>%
        filter(usina == input$select_usina_componentes ) %>%
        select(usina, preco_inicio = r_ha, data,
               percent_uso_e_cobertura,
               grupo_atividade,
               capacidade,
               detalhamento,
               uf,
               municipio,
               percentual_municipio_geral,
               relatorio_inicio = num_relatorio
        )
      print(componentes_precos_inicio)
      componentes_precos_fim <- preco_pedacos_usina %>%
        filter(data == datas_filtro[2] ) %>%
        filter(usina == input$select_usina_componentes ) %>%
        select(usina, preco_fim = r_ha, data,
               grupo_atividade,
               capacidade,
               detalhamento,
               percent_uso_e_cobertura,
               uf,
               municipio,
               percentual_municipio_geral,
               relatorio_fim = num_relatorio
        )
      print(componentes_precos_fim)
      saida <- componentes_precos_inicio %>%
        full_join(componentes_precos_fim,
                  by =
                    c(
                      "percent_uso_e_cobertura",
                      "grupo_atividade",
                      "capacidade",
                      "detalhamento",
                      "uf",
                      "municipio",
                      "percentual_municipio_geral"
                    )
        ) %>%
        mutate(
          variacao_preco = preco_fim/preco_inicio-1
        ) %>%
        ungroup() %>%
        select(
          grupo_atividade,
          capacidade,
          detalhamento,
          percent_uso_e_cobertura,
          uf,
          municipio,
          percentual_municipio_geral,
          preco_inicio,
          preco_fim,
          variacao_preco,
          relatorio_inicio,
          relatorio_fim
        )
      print(saida)
      print("fez componentes")
      saida
    })
    
    output$tabela_componentes <- render_gt({
      
      
      componentes() %>% 
        ungroup() %>% 
        mutate(
          grupo = str_glue("{grupo_atividade}, {capacidade}, {detalhamento}, {percent(percent_uso_e_cobertura,.01)}"),
          impacto = variacao_preco * percentual_municipio_geral 
        ) %>% 
        select(
          -c(
            grupo_atividade,
            capacidade,
            detalhamento,
            percent_uso_e_cobertura
          )
        ) %>%
        group_by(grupo) %>% 
        mutate(
          linha = row_number()
        ) %>% 
        gt(rowname_col = "linha") %>% 
        fmt_percent(
          columns = vars(variacao_preco, percentual_municipio_geral, impacto),
          decimals = 2,
          dec_mark = ","
        ) %>% 
        tab_header(
          title = "Componentes da variação dos preços da terra (R$/ha)",
          subtitle = str_glue("Período: {input$slider_periodo_componentes[1]} a {input$slider_periodo_componentes[2]}")
        ) %>% 
        fmt_currency(
          columns = vars(preco_inicio, preco_fim),
          currency = "BRL",
          sep_mark = ".",
          dec_mark = ","
        ) %>% 
        cols_label(
          variacao_preco = html("% Preço"),
          impacto = html("% no Preço Total"),
          preco_inicio = html(input$slider_periodo[1]),
          preco_fim = html(input$slider_periodo[2]),
          uf = html("UF"),
          municipio = html("Município"),
          percentual_municipio_geral = html("% uso"),
          relatorio_inicio = html("Nº FNP início"),
          relatorio_fim = html(html("Nº FNP fim"))
          
        ) %>% 
        tab_options(
          container.width = pct(80),
          table.width = pct(98) 
        ) %>% 
        tab_style(
          style = cell_borders(
            sides = c("left", "right"),
            color = "#dddddd",
            weight = px(2)
          ),
          locations = cells_body()
        ) %>% 
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_row_groups()
        )
      
      
      
      
    })
    
    output$download_componentes <- downloadHandler(
      filename = "componentesvariacaoprecos.xlsx",
      content = function(file) {
        write_xlsx(componentes(), file)
      }
    )
    
  })
}
    
## To be copied in the UI
# mod_componentes_da_variacao_por_usina_ui("componentes_da_variacao_por_usina_ui_1")
    
## To be copied in the server
# mod_componentes_da_variacao_por_usina_server("componentes_da_variacao_por_usina_ui_1")
