
library('pacman')

pacman::p_load(tidyverse,rugarch,quantmod,fGarch,rmgarch,fPortfolio,quadprog,MarkowitzR,PortfolioAnalytics)


##### Save data_interação

dt_retorno_selic = rep(0,12)

dt_retorno_10 = rep(0,12)



##### Retorno selic de 2023


data_selic_23 = data.frame("Juros" = c(1.12,0.92,1.17,0.92,1.12,1.07,1.07,1.14,0.97,1,0.92,0.89))

selic_23 = c(t(data_selic_23))

selic_23_10 = selic_23*1.1

selic_23[1]
selic_23_10[1]

##### Carregar dados

inicio =  as.Date('2010-01-01')

fonte = 'yahoo' 

fiis = c("GTWR11.SA","KNCR11.SA","TGAR11.SA","BCFF11.SA","BRCR11.SA","HGBS11.SA","HGLG11.SA")

getSymbols(fiis, src=fonte, from=inicio)



#Lista para loop

list_mes_modelo = c('2010/2022-12','2010/2023-01','2010/2023-02','2010/2023-03','2010/2023-04','2010/2023-05','2010/2023-06',
                    '2010/2023-07','2010/2023-08','2010/2023-09','2010/2023-10','2010/2023-11')

list_mes_previ = c('2023-01/2023-01','2023-02/2023-02','2023-03/2023-03','2023-04/2023-04','2023-05/2023-05','2023-06/2023-06',
                   '2023-07/2023-07','2023-08/2023-08','2023-09/2023-09','2023-10/2023-10','2023-11/2023-11','2023-12/2023-12')

list_mes_retorno = c("2022-01/2022-12", "2022-02/2023-01", "2022-03/2023-02", "2022-04/2023-03", "2022-05/2023-04", "2022-06/2023-05",
                     "2022-07/2023-06", "2022-08/2023-07", "2022-09/2023-08", "2022-10/2023-09", "2022-11/2023-10", "2022-12/2023-11")


data = merge(Ad(GTWR11.SA), Ad(KNCR11.SA), Ad(TGAR11.SA),Ad(BCFF11.SA),Ad(BRCR11.SA),Ad(HGBS11.SA),Ad(HGLG11.SA))
data_returns <- na.omit(diff(log(data)))

##### ITERAÇÃO

for(i in 1:12){
  
  # Valor ajustado para calcular retornos.
  
  valor_ajustado = merge(Ad(GTWR11.SA), Ad(KNCR11.SA), Ad(TGAR11.SA),Ad(BCFF11.SA),Ad(BRCR11.SA),Ad(HGBS11.SA),Ad(HGLG11.SA))[list_mes_modelo[i]]
  
  # Retornos logarítmicos
  
  log_returns <- na.omit(diff(log(valor_ajustado))) #Ps: omitindo os na, ficamos com dados a partir de 2019
  
  #head(log_returns)
  
  
  # Modelo GARCH bivariado (versão simplificada) ->
  
  modelo_padrao <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                              distribution.model = "norm") #usei ordem 1,1 pelo instinto
  
  #fit <- ugarchfit(modelo_padrao, data = log_returns)
  
  uspec = multispec(replicate(7, modelo_padrao))
  
  spec1 = rmgarch::dccspec(uspec = uspec, dccOrder = c(1, 1), distribution = 'mvnorm')
  
  spec1a = rmgarch::dccspec(uspec = uspec, dccOrder = c(1, 1), model='aDCC', distribution = 'mvnorm')
  
  #cl = makePSOCKcluster(4)
  
  multf = multifit(uspec, log_returns)#, cluster = cl
  
  fit1 = rmgarch::dccfit(spec1, data = log_returns, fit.control = list(eval.se = TRUE), fit = multf) #, cluster = cl
  
  fit_adcc = rmgarch::dccfit(spec1a, data = log_returns, fit.control = list(eval.se = TRUE), fit = multf)#, cluster = cl
  
  #print(fit1) 
  
  
  #plot(fit1)
  
  
  #print(fit_adcc)
  
  #stopCluster(cl)
  
  
  dias_uteis_mes = nrow(data[list_mes_previ[i]])
  
  
  previsao = rmgarch::dccforecast(fit1, n.ahead = dias_uteis_mes) #
  
  matrix_cov = rcov(previsao)[[1]]
  
  matrix_cov = matrix_cov[,,]
  
  # Calculando a média das covariâncias.
  
  matrix_cov = apply(matrix_cov, MARGIN = 1:2, FUN = mean) #Aplica a média para possuir a média das covariâncias e multiplica a diagonal pelo n, possuinto a soma das variâncias.
  
  
  diag(matrix_cov) = diag(matrix_cov) * dias_uteis_mes ##DIAS =  IGUAL A SOMA
  
  #matrix_cov
  
  
  
  assets = c("GTWR11", "KNCR11", "TGAR11","BCFF11","BRCR11","HGBS11","HGLG11")
  
  colnames(matrix_cov) = assets
  
  rownames(matrix_cov) = assets
  
  # Retornos esperados (6 meses)
  
  
  
  expected_returns = merge(Ad(GTWR11.SA), Ad(KNCR11.SA), Ad(TGAR11.SA),Ad(BCFF11.SA),Ad(BRCR11.SA),Ad(HGBS11.SA),Ad(HGLG11.SA))[list_mes_retorno[i]]
  
  
  expected_returns = na.omit(diff(log(expected_returns)))
  
  
  expected_returns = sapply(expected_returns,sum) #retorno semestral
  
  expected_returns = expected_returns*(100/12)
  
  if(max(expected_returns)<1.35){ #correção, calculo dos retornos deve ser feito por análise técnica
    

    
    retorno_port = selic_23[i]
    
    retorno_port_10 = selic_23[i]
    
  }
  
  else{
    
    # Encontrando os pesos otimizados (https://stackoverflow.com/questions/24090037/r-portfolio-optimization-solve-qp-constraints-are-inconsistent)
    
    
    
    opt_portfolio_selic = solve.QP(Dmat = as.matrix(matrix_cov),#Menor variância para retorno FIXO.
                                   dvec = expected_returns,
                                   Amat = cbind(matrix(c(1,1,1,1,1,1,1), ncol=1), expected_returns,diag(7)),
                                   bvec = c(1,selic_23[i] ,rep(0, 7)), #data_selic_23
                                   meq = 2)$solution
    
    
    
    #opt_portfolio_selic
    
    opt_portfolio_selic_10 = solve.QP(Dmat = as.matrix(matrix_cov),#Menor variância para retorno FIXO.
                                      dvec = expected_returns,
                                      Amat = cbind(matrix(c(1,1,1,1,1,1,1), ncol=1), expected_returns,diag(7)),
                                      bvec = c(1,selic_23_10[i] ,rep(0, 7)), #data_selic_23 + 10%
                                      meq = 2)$solution
    
    
    retorno_real = data_returns[list_mes_previ[i]]
    
    retorno_real = sapply(retorno_real,sum)*100
    
    
    
    
    #opt_portfolio_selic
    
    retorno_port = opt_portfolio_selic*retorno_real
    
    retorno_port_10 = opt_portfolio_selic_10*retorno_real
    
    
  }
  

  
  
  #opt_portfolio_selic_10
  
  
  

  
  
  
  dt_retorno_selic[i] = sum(retorno_port)
  
  dt_retorno_10[i] = sum(retorno_port_10)
  
  
  
}





dt = data.frame("Selic" = c(1.12,0.92,1.17,0.92,1.12,1.07,1.07,1.14,0.97,1,0.92,0.89),
                "Portfólio-Selic" = dt_retorno_selic,
                "Portfólio-Selic-10%" = dt_retorno_10)





dates <- seq(as.Date("2023-01-01"), by = "month", length.out = nrow(dt))


