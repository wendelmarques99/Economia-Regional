library(sidrar)
library(magrittr)


# PRIMEIRA QUESTAO --------------------------------------------------------


# Porcentagem dos estados -------------------------------------------------
porcentagem_dos_estados <- httr::GET("https://apisidra.ibge.gov.br/values/t/5434/g/103/v/4090/p/last%201/c693/33355") %>% 
  httr::content(as = "text")  %>% 
  jsonlite::fromJSON() %>% 
  dplyr::filter(
    NN == "Unidade da Federação"
  ) %>% 
  dplyr::mutate(
    V = V %>% as.numeric()
  ) %>% 
  dplyr::mutate(
    Total = sum(V)
  ) %>% 
  dplyr::mutate(
    Porcentagem = V/Total
  ) %>% 
  dplyr::arrange(
    dplyr::desc(Porcentagem)
  ) 

cat(porcentagens)
estados <- porcentagem_dos_estados$D1N[1:10]
porcentagens <- porcentagem_dos_estados$Porcentagem[1:10] *100


# 3 QUESTAO ---------------------------------------------------------------


PIB_PERCAPITA <- httr::GET("https://apisidra.ibge.gov.br/values/t/1194/n1/all/n3/all/v/93,2363/p/2010/d/v2363%200") %>% 
  httr::content(as = "text")  %>% 
  jsonlite::fromJSON() %>%
  dplyr::slice(-1) %>% 
  dplyr::mutate(
    V = as.numeric(V)
  ) 


pib_percapita_brasil_75 <- PIB_PERCAPITA %>% 
  dplyr::filter(D1N == "Brasil") %>%
  dplyr::filter(
    MN == "Reais"
  ) %>% 
  dplyr::pull(V) * 0.75


pop_brasil <- PIB_PERCAPITA %>% 
  dplyr::filter(D1N == "Brasil") %>%
  dplyr::filter(
    MN == "Pessoas"
  ) %>% 
  dplyr::pull(V)

estados_menor_que_75_pibpercapita_br <- PIB_PERCAPITA %>% 
  dplyr::filter(D1N != "Brasil") %>%
  dplyr::filter(
    MN == "Reais",
    V < pib_percapita_brasil_75
  ) %>% 
  dplyr::pull( D1N )

quantidades_estados <- length(estados_menor_que_75_pibpercapita_br)


pop_estados_menor_75_pibpercapita <- PIB_PERCAPITA %>% 
  dplyr::filter(D1N %in% estados_menor_que_75_pibpercapita_br) %>%
  dplyr::filter(
    MN == "Pessoas"
  ) %>% 
  dplyr::pull(V) %>% 
  sum()
## PORCENTAGEM DOS ESTADOS EM RELACAO AO BR
pop_estados_menor_75_pibpercapita/pop_brasil *100


tabelas <- list.files()[2:6]

ler_arquivos <- function(x){
  readxl::read_excel(x) %>% 
    dplyr::select(-c(1, 2)) %>% 
    tidyr::pivot_longer(cols = -1) %>% 
    janitor::clean_names() %>% 
    dplyr::rename(
      "Ano" = "name",
      "PIB PERCAPITA" = "value"
    )

}

# 4 questao
purrr::map_dfr(ler_arquivos, .x = tabelas) %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(
    ano = paste0(ano, "-01-01") %>% as.Date()
  ) %>% 
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = ano, y = pib_percapita, color = regiao), size = 2) +
  ggplot2::labs(title = "PIB PER CAPITA")


# 5 questao ---------------------------------------------------------------

httr::GET("https://apisidra.ibge.gov.br/values/t/7435/n3/all/v/10681/p/2020/d/v10681%203") %>% 
  httr::content(as = "text")  %>% 
  jsonlite::fromJSON()  %>% 
  dplyr::slice(-1) %>% 
  dplyr::mutate(
    V = as.numeric(V)
  ) 


httr::GET("https://apisidra.ibge.gov.br/values/t/6894/n2/all/v/all/p/last%201/c58/95253/d/v10004%201") %>% 
  httr::content(as = "text") %>% 
  jsonlite::fromJSON() %>% 
  dplyr::slice(-1) %>% 
  dplyr::mutate(
    V = as.numeric(V)
  ) %>% 
  dplyr::mutate(
    total = sum(V),
    porcentagem = 
  ) 
