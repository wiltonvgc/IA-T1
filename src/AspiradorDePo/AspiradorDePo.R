source("Estado.R")

## Classe e métodos para o problema do Aspirador de Po => 2 x 2
AspiradorDePo <- function(desc = NULL, pai = NULL){
  
  e <- environment()
  
  assign("desc", desc, envir = e)
  assign("pai", pai, envir = e)
  assign("g", 0, envir = e)
  assign("h", Inf, envir = e)
  assign("f", Inf, envir = e)
  
  class(e) <- c("AspiradorDePo", "Estado")
  
  return(e)
}

## Sobrecarregando o operador "==" para comparação entre estados
Ops.AspiradorDePo = function(obj1,obj2){
  if(.Generic == "=="){
    return(all(obj1$desc[1:4] == obj2$desc[1:4]))
  }
}

## Sobrecarga da função genérica "print" do R
print.AspiradorDePo <- function(obj) {
  cat("(M1,M2,M3,M4,X,Y): (", obj$desc, ")\n")
  cat("G(n): ", obj$g, "\n")
  cat("H(n): ", obj$h, "\n")
  cat("F(n): ", obj$f, "\n")
}

## Sobrecarga da função genérica "heuristica", definida por Estado.R
heuristica.AspiradorDePo <- function(atual){
  
  if(is.null(atual$desc))
    return(Inf)
  ## h(obj) = M1 + M2 + M3 + M4
  return(sum(atual$desc[1:4]))
}


#Metodo para gerar filhos de um dado estado corrente
geraFilhos.AspiradorDePo <- function(obj) {
  
  #Um estado e representado : (M1,M2,M3,M4,X,Y) => Mi = 1, posicao suja, Mi=0, limpa
  #                                           => X,Y , posicao atual do aspirador
  
  filhosDesc <- list() #lista de filhos inclusive incompativeis
  filhos <- list() # lista de filhos sem incompativeis
  
  desc <- obj$desc #desc recebe estado a ter filhos gerados
  
  #operadores
  
  #Caso aspirador em (1,1)
  if(desc[5]==1 && desc[6]==1){
      operadores <- list(c(-1,0,0,0,0,0), c(0,0,0,0,-1,0), c(0,0,0,0,1,0), c(0,0,0,0,0,-1), c(0,0,0,0,0,1))
      #custo aplicacao dos operadores
      custo <- c(2,1,1,3,3)
      
  #Caso operador em (1,2)
  }else if(desc[5]==1 && desc[6]==2){
    operadores <- list(c(0,-1,0,0,0,0), c(0,0,0,0,-1,0), c(0,0,0,0,1,0), c(0,0,0,0,0,-1), c(0,0,0,0,0,1))
    #custo aplicacao dos operadores
    custo <- c(2,1,1,3,3)
    
    }#Caso operado em (2,1)
  else if(desc[5]==2 && desc[6]==1){
    operadores <- list(c(0,0,-1,0,0,0), c(0,0,0,0,-1,0), c(0,0,0,0,1,0), c(0,0,0,0,0,-1), c(0,0,0,0,0,1))
    #custo aplicacao dos operadores
    custo <- c(2,1,1,3,3)
    
   }#Caso operador em (2,2)
  else if(desc[5]==2 && desc[6]==2){
    operadores <- list(c(0,0,0,-1,0,0), c(0,0,0,0,-1,0), c(0,0,0,0,1,0), c(0,0,0,0,0,-1), c(0,0,0,0,0,1))
    #custo aplicacao dos operadores
    custo <- c(2,1,1,3,3)
    
  }
  
  #geracao de filhos inclusive com incompativeis
  filhosDesc <- lapply(operadores, function(op) desc+op)
  
  #remocao de estados incompativeis => quando alguma posicao de celula for <0 OU quando posicao X,Y for fora dos limites 2 X 2
  incompativeis <- sapply(1:length(filhosDesc),
                          function(i){
                            fDesc = filhosDesc[[i]] #pega estado
                            
                            retorno <- 0
                            
                            #verifica se posicao celula negativa
                            for(k in 1:4){
                              if(fDesc[k]<0){
                                retorno <- i
                              }
                            }
                            
                            #verifica limites de posicao em grid 2X2
                            if(fDesc[5]<1 || fDesc[5]>2 || fDesc[6]<1 || fDesc[6]>2){
                              retorno <- i
                            }
                            
                            return(retorno)
                            
                            
                          }) #fim sapply
  
    #Manter no vetor incompativeis apenas indices dos incompativeis => != 0
    incompativeis <- incompativeis[incompativeis!=0]
    
    #Colocar no vetor FilhosDesc apenas filhos compativeis
    filhosDesc <- filhosDesc[-incompativeis]
    custo <- custo[-incompativeis]
    
    #Geracao de objetos AspiradorDePo para filhos
    i <- 1 #contador
    for(filhoDesc in filhosDesc){
      filho <- AspiradorDePo(desc=filhoDesc,pai=obj)
      filho$h <- heuristica(filho)
      filho$g <- obj$g + custo[i] #pega custo gasto na geracao deste estado filho
      i <- i + 1
      filhos <- c(filhos, list(filho))
    }
    
  
    return(filhos)
  
}

