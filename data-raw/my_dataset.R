## code to prepare `my_dataset` dataset goes here

library(tidyverse)
library(feather)

dados_fnp <- feather::read_feather("dados/dados_fnp.feather")
usethis::use_data(dados_fnp, overwrite = TRUE)

municipios_usinas <- feather::read_feather("dados/municipios_usinas.feather")
usethis::use_data(municipios_usinas, overwrite = TRUE)

terrenos_usinas <- feather::read_feather("dados/terrenos_usinas.feather")
usethis::use_data(terrenos_usinas, overwrite = TRUE)


datas <- dados_fnp %>% 
  select(data) %>% 
  distinct()
usethis::use_data(datas, overwrite = TRUE)


preco_pedacos_usina <- terrenos_usinas %>%
  mutate(indice = row_number()) %>% 
  left_join(
    municipios_usinas,
    by = c("usina" = "usina", "grupo_atividade" = "grupo_atividade_se_especifico")
  ) %>% 
  separate(
    municipio_fnp,
    into = c("uf", "municipio"),
    sep = "-"
  ) %>% 
  filter(  is.na(detalhamento_se_especifico) | (detalhamento == detalhamento_se_especifico  & capacidade == capacidade_se_especifica) ) %>% 
  group_by(indice) %>% 
  mutate(percentual_municipio = 1/n()) %>% 
  ungroup() %>% 
  mutate(
    percentual_municipio_geral = percent_uso_e_cobertura * percentual_municipio,
  ) %>% 
  group_by(
    usina
  ) %>% 
  mutate(
    total_antes_precos = sum(percentual_municipio_geral)
  ) %>% 
  ungroup() %>% 
  crossing(
    datas
  ) %>% 
  left_join(
    dados_fnp,
    by = c(
      "uf" = "uf",
      "municipio" = "regiao_municipio",
      "grupo_atividade" = "grupo_atividade",
      "capacidade" = "capacidade_produtiva",
      "detalhamento" = "detalhamento",
      "data" = "data"
    )
  ) %>% 
  group_by(
    usina,
    data
  ) %>% 
  mutate(
    total = sum(percentual_municipio_geral)
  ) 
usethis::use_data(preco_pedacos_usina, overwrite = TRUE)


precos_usina <- preco_pedacos_usina %>% 
  group_by(
    usina,
    data
  ) %>% 
  summarise(
    preco = weighted.mean( r_ha, percentual_municipio_geral, na.rm = FALSE),
    fracao = sum(percentual_municipio_geral)
  )
usethis::use_data(precos_usina, overwrite = TRUE)


datas_como_string <- datas %>% 
  arrange(data) %>% 
  pull(data) %>% 
  format("%m/%Y")
usethis::use_data(datas_como_string, overwrite = TRUE)