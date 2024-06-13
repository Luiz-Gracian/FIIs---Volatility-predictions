library('pacman')

pacman::p_load(tidyverse,rugarch,quantmod,fGarch,rmgarch,fPortfolio,quadprog,MarkowitzR,PortfolioAnalytics,timetk,plotly)


###### https://www.codingfinance.com/post/2018-05-31-portfolio-opt-in-r/


##### Carregar dados

inicio =  as.Date('2010-01-01')

fonte = 'yahoo' 

fiis = c("GTWR11.SA","KNCR11.SA","TGAR11.SA")

n_port = length(fiis)

getSymbols(fiis, src=fonte, from=inicio)

# Valor ajustado para calcular retornos.

valor_ajustado = merge(Ad(GTWR11.SA), Ad(KNCR11.SA), Ad(TGAR11.SA))['2010/2023-12']

## Banco teste para validar modelo

valor_ajustado_teste = merge(Ad(GTWR11.SA), Ad(KNCR11.SA), Ad(TGAR11.SA))['2024/2024-03']

# Retornos logarítmicos

log_returns <- na.omit(diff(log(valor_ajustado))) #Ps: omitindo os na, ficamos com dados a partir de 2019

head(log_returns)




############################ CHECAR FIORUCCI (?)


cov_matrix <- matrix(c(1.750771e-04, 1.493605e-05, 2.727978e-05,
                       1.493605e-05, 6.182415e-05, 2.415248e-05,
                       2.727978e-05, 2.415248e-05, 2.409094e-04), nrow = 3, byrow = TRUE)


mean_ret = colMeans(log_returns)



print(mean_ret *252) # Anual


# Estamo pegando o retorno médio desde 2019, isto é, desde o ínicio dos dados e multiplicando por 252
# que seria a quantidade de dias do ano, porém claramente é errôneo essa perspectiva, apenas teste.
#


# pesos

wts <- runif(n_port)
wts <- wts/sum(wts)

# retornos

port_returns <- sum(wts * mean_ret)*252 #### MUDAR TRIMESTRAL

# risco 

port_risk <- sqrt(t(wts) %*% (cov_matrix %*% wts))

# Sharpe Ratio

sharpe_ratio <- port_returns/port_risk ### Tendo Rf (taxa livro de risco) = 0%

print(wts)

print(port_returns)

print(port_risk)

num_port <- 5000

# ARMAZEM

all_wts <- matrix(nrow = num_port,
                  ncol = n_port)




port_returns <- vector('numeric', length = num_port)

port_risk <- vector('numeric', length = num_port)

sharpe_ratio <- vector('numeric', length = num_port) 




for (i in seq_along(port_returns)) {
  
  wts <- runif(n_port)
  wts <- wts/sum(wts)
  

  all_wts[i,] <- wts #armazenar pesos em matriz
  

  
  port_ret <- sum(wts * mean_ret)
  
  port_ret <- port_ret*252
  

  
  port_returns[i] <- port_ret
  
  
  # Armazenar riscos
  
  port_sd <- sqrt(t(wts) %*% (cov_matrix  %*% wts)) #operação matricial.
  
  port_risk[i] <- port_sd
  
  # Armazernar sharpe ratio (0% taxa de risco)
  
  sr <- port_ret/port_sd
  
  sharpe_ratio[i] <- sr
  
}


# Mudar colnames
portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)

all_wts <- tk_tbl(all_wts)

colnames(all_wts) <- fiis

# Combinação - plot

portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))

min_var <- portfolio_values[which.min(portfolio_values$Risk),]

max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]




p <- min_var %>%
  gather(GTWR11.SA:TGAR11.SA, key = FII,
         value = Weights) %>%
  mutate(FII = as.factor(FII)) %>%
  ggplot(aes(x = fct_reorder(FII,Weights), y = Weights, fill = FII)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'FIIs', y = 'Pesos', title = "Portfólio com menor risco") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)



p <- max_sr %>%
  gather(GTWR11.SA:TGAR11.SA, key = FII,
         value = Weights) %>%
  mutate(FII = as.factor(FII)) %>%
  ggplot(aes(x = fct_reorder(FII,Weights), y = Weights, fill = FII)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'FIIs', y = 'Pesos', title = "Portifólio com melhor SR") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)



p <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous() +
  labs(x = 'Risco',
       y = 'Retornos anuais',
       title = "Fronteira Eficiente") +
  geom_point(aes(x = Risk,
                 y = Return), data = min_var, color = 'red') +
  geom_point(aes(x = Risk,
                 y = Return), data = max_sr, color = 'red')


ggplotly(p)

print(min_var)

print(max_sr)



#########################################

##### TESTE - Retorno Trimestral ! (retorno trimestral constante, averiguar manipulação --- complex)

expected_returns <- merge(Ad(GTWR11.SA), Ad(KNCR11.SA), Ad(TGAR11.SA))['2023-10/2023-12'] 

expected_returns = na.omit(diff(log(expected_returns)))

#retorno trimestral

mean_ret = sapply(expected_returns,mean)


#Ver o retorno trimestral

print(sapply(expected_returns,mean)*length(expected_returns$GTWR11.SA.Adjusted)) #59

#mean(expected_returns$GTWR11.SA.Adjusted)



all_wts <- matrix(nrow = num_port,
                  ncol = n_port)




port_returns <- vector('numeric', length = num_port)

port_risk <- vector('numeric', length = num_port)

sharpe_ratio <- vector('numeric', length = num_port) 




for (i in seq_along(port_returns)) {
  
  wts <- runif(n_port)
  wts <- wts/sum(wts)
  
  
  all_wts[i,] <- wts #armazenar pesos em matriz
  
  
  
  port_ret <- sum(wts * mean_ret)
  
  port_ret <- port_ret*59
  
  
  
  port_returns[i] <- port_ret
  
  
  # Armazenar riscos
  
  port_sd <- sqrt(t(wts) %*% (cov_matrix  %*% wts)) #operação matricial.
  
  port_risk[i] <- port_sd
  
  # Armazernar sharpe ratio (0% taxa de risco)
  
  sr <- port_ret/port_sd
  
  sharpe_ratio[i] <- sr
  
}


# Mudar colnames
portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)

all_wts <- tk_tbl(all_wts)

colnames(all_wts) <- fiis

# Combinação - plot

portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))

min_var <- portfolio_values[which.min(portfolio_values$Risk),]

max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]

print(min_var)


p <- min_var %>%
  gather(GTWR11.SA:TGAR11.SA, key = FII,
         value = Weights) %>%
  mutate(FII = as.factor(FII)) %>%
  ggplot(aes(x = fct_reorder(FII,Weights), y = Weights, fill = FII)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'FIIs', y = 'Pesos', title = "Portfólio com menor risco") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)



p <- max_sr %>%
  gather(GTWR11.SA:TGAR11.SA, key = FII,
         value = Weights) %>%
  mutate(FII = as.factor(FII)) %>%
  ggplot(aes(x = fct_reorder(FII,Weights), y = Weights, fill = FII)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'FIIs', y = 'Pesos', title = "Portifólio com melhor SR") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)



p <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous() +
  labs(x = 'Risco',
       y = 'Retornos anuais',
       title = "Fronteira Eficiente") +
  geom_point(aes(x = Risk,
                 y = Return), data = min_var, color = 'red') +
  geom_point(aes(x = Risk,
                 y = Return), data = max_sr, color = 'red')


ggplotly(p)
