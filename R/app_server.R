#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import lubridate
#' @import stringr
#' @import scales
#' @import janitor
#' @import writexl
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  mod_variacao_no_preco_da_terra_server("variacao_no_preco_da_terra_ui_1")
  mod_componentes_da_variacao_por_usina_server("componentes_da_variacao_por_usina_ui_1")
  
}
