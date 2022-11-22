library(magrittr)

dados <- haven::read_dta("C:\\Users\\werico\\Downloads\\peer_effect_melhores amigos.dta")

# tirando idades menor ou igual a zero
dados <- dados %>% 
  dplyr::filter(
    Widade > 0, 
    W2idade > 0)

modelo <- lm(data = dados, N1_matematica ~ Wbullied + Wmasculino + Widade + Westuda + 
                                            Wseguranca + Wcor + Wdiscip +  
                                            W2bullied + W2masculino + W2idade + W2estuda +
                                            W2seguranca + W2cor + W2discip)
# sumario
summary(modelo)

plot(modelo)

abline(modelo)

# Perguntas a serem feitas

# Usar apenas variaveis dos amigos está correto?
# Usar no modelo apenas variaveis estatisticamente significantes?
# Pedir um dicionario dos dados 
# desconsiderar os amigos com idade 0