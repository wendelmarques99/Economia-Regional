library(haven)
library(magrittr)

dados_de_salario_recife <- haven::read_dta("hedonico_RecifeCNAE.dta")

# Tirando salarios igual 0 ------------------------------------------------
dados_de_salario_recife <- dados_de_salario_recife %>% 
  dplyr::filter(Nominal_Wage > 0, Working_hours > 0) 

# Log do salario nominal --------------------------------------------------
dados_de_salario_recife$Nominal_Wage <- log(dados_de_salario_recife$Nominal_Wage)

# Montando o modelo para caracteristicas apenas de trabalho (sem a --------
modelo_sem_amenidade <- lm(Nominal_Wage ~ Working_hours + Job_tenure + No_Working_Days, data = dados_de_salario_recife)

summary(modelo_sem_amenidade)

# Montando modelo com amenidades

# Modelo com o setor Culture, Arts and recreation 
dados_do_modelo_c_amenidade <- fastDummies::dummy_cols(dados_de_salario_recife, "cnae") %>% 
  na.omit()

modelo_com_dummy <- dados_do_modelo_c_amenidade %>% 
  dplyr::select(-c("id", "cnae", "CEP_to_CBD", "CEP_Estab_to_CBD", "CEP_to_CEP_Estab"))

modelo_com_amenidades <- lm(Nominal_Wage ~ .-Nominal_Wage, data = modelo_com_dummy)

summary(modelo_com_amenidades)

