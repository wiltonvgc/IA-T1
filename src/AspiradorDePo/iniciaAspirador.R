debugSource("AspiradorDePo.R")
debugSource("buscaDesinformada.R")
debugSource("buscaInformada.R")

inicial <- AspiradorDePo(desc = c(1,1,1,1,2,1)) #dois quadrados sujos com inicio em (1,1)

objetivo <- AspiradorDePo() #criacao de estado objetivo

objetivo$desc <- c(0,0,0,0,1,1) #possilidade com objetivo em posicao (1,1). Nao faz direnca posicao!



cat("====\tBusca em Largura\t====\n")
print(unlist(buscaEmLargura(inicial, objetivo)))

cat("====\tBusca em Profundidade\t=====\n")
print(buscaEmProfundidade(inicial, objetivo))

cat("====\tBusca de Custo Uniforme\t=====\n")
print(buscaCustoUniforme(inicial, objetivo))

cat("====\tBusca Best-First (Gulosa)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "Gulosa"))

cat("====\tBusca Best-First (A*)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "AEstrela"))


