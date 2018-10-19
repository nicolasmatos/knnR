knn <- function(treinamento, k, e) {
  distancias<-c()
  for (i in 1: (nrow(treinamento))) {
    distancias[i] = sqrt((treinamento[i,1] - e[1])^2 + (treinamento[i,2] - e[2])^2 + (treinamento[i,3] - e[3])^2 + (treinamento[i,4] - e[4])^2)
  }
  
  menoresCoordenadas<-list()
  menoresDistancias<-c()
  c1 = 0
  c2 = 0
  c3 = 0
  for (i in 1: k) {
    menor = min(distancias)
    indiceMenorDistancia = match(menor,distancias)
    
    menoresDistancias[i] = menor
    menoresCoordenadas[[i]] = treinamento[indiceMenorDistancia,]
    
    distancias[indiceMenorDistancia] = 1000000
    
    if(treinamento[indiceMenorDistancia,5] == 1) {
      c1 = c1 + 1
    }
    else if(treinamento[indiceMenorDistancia,5] == 2) {
      c2 = c2 + 1
    }
    else {
      c3 = c3 + 1
    }
  }
  
  classes<-c(c1,c2,c3)
  
  #Retornando uma lista com as probabilidades
  ret = list()
  ret$menoresCoordenadas = menoresCoordenadas
  ret$menoresDistancias = menoresDistancias
  ret$e = e
  ret$classe = match(max(classes),classes)
  ret$acertou = match(max(classes),classes) == e[5]
  
  return (ret)
}

processaKnn <- function(classe1, classe2, classe3, k) {
  txAcertos<-c()
  txAcertosUm<-c()
  txAcertosDois<-c()
  txAcertosTres<-c()
  
  #Laço para rodar 30vezes a base de teste
  for (j in 1:30) {
    #Embaralhando a base original
    classe1 <- classe1[sample(1:nrow(classe1)), ]
    classe2 <- classe2[sample(1:nrow(classe2)), ]
    classe3 <- classe3[sample(1:nrow(classe3)), ]

    #Quebrando o dataset em treino e teste (80/20) OBS: Buscando as linhas de forma aleotória
    library(dplyr)
    
    dataTreinoClasse1<-sample_frac(classe1, 0.80)
    dataTesteClasse1<-setdiff(classe1, dataTreinoClasse1)
    
    dataTreinoClasse2<-sample_frac(classe2, 0.80)
    dataTesteClasse2<-setdiff(classe2, dataTreinoClasse2)
    
    dataTreinoClasse3<-sample_frac(classe3, 0.80)
    dataTesteClasse3<-setdiff(classe3, dataTreinoClasse3)
    
    dataTreino <- rbind(dataTreinoClasse1, dataTreinoClasse2, dataTreinoClasse3)
    dataTeste <- rbind(dataTesteClasse1, dataTesteClasse2, dataTesteClasse3)
    
    #return(dataTeste)
    
    #Variáveis para controlar os acertos do algoritmo
    qntUm = 0
    qntDois = 0
    qntTres = 0
    qntAcertosUm = 0
    qntAcertosDois = 0
    qntAcertosTres = 0
    
    #Laço para percorrer todas as linha do dataframe de teste
    for (i in 1: (nrow(dataTeste))) {
      #Recebe a linha atual do conjunto de teste
      linha = dataTeste[i,]

      #Chamando a função que classifica a linha atual
      resultKnn = knn(dataTreino, k, linha)
      
      #return(resultKnn)
      
      #Verificando quantas chamadas teve de cada classe
      if (resultKnn$e[5] == 1) {
        qntUm = qntUm + 1
      }
      else if (resultKnn$e[5] == 2) {
        qntDois = qntDois + 1
      }
      else {
        qntTres = qntTres + 1
      }
      
      #Verificando se o algoritmo acertou na classificação
      if (resultKnn$acertou) {
        if(resultKnn$classe == 1) {
          qntAcertosUm = qntAcertosUm + 1
        }
        else if(resultKnn$classe == 2) {
          qntAcertosDois = qntAcertosDois + 1
        }
        else {
          qntAcertosTres = qntAcertosTres + 1
        }
      }
    }
    
    #Calculando a taxa de acertos (Numero de acertos total / Quantidade de elementos testados)
    txAcerto = (qntAcertosUm + qntAcertosDois + qntAcertosTres) / (qntUm + qntDois + qntTres)
    
    #Calculando a taxa de acertos para classe um (Numero de acertos um / Quantidade de elementos testados para um)
    txAcertoUm = qntAcertosUm / qntUm
    
    #Calculando a taxa de acertos para classe dois (Numero de acertos dois / Quantidade de elementos testados para dois)
    txAcertoDois = qntAcertosDois / qntDois
    
    #Calculando a taxa de acertos para classe tres (Numero de acertos tres / Quantidade de elementos testados para tres)
    txAcertoTres = qntAcertosTres / qntTres
    
    #r = list()
    #r$txAcerto = txAcerto
    #r$txAcertoUm = txAcertoUm
    #r$txAcertoDois = txAcertoDois
    #r$txAcertoTres = txAcertoTres
    #r$qntUm = qntUm
    #r$qntDois = qntDois
    #r$qntTres = qntTres
    #r$qntAcertosUm = qntAcertosUm
    #r$qntAcertosDois = qntAcertosDois
    #r$qntAcertosTres = qntAcertosTres
    
    #return(r)
    
    txAcertos[j]<-txAcerto
    txAcertosUm[j]<-txAcertoUm
    txAcertosDois[j]<-txAcertoDois
    txAcertosTres[j]<-txAcertoTres
  }
  
  resultado = list()
  
  resultado$txAcertosMin = min(txAcertos)  
  resultado$txAcertosMax = max(txAcertos)
  resultado$txAcertosMed = median(txAcertos)
  resultado$txAcertosUmMed = median(txAcertosUm)
  resultado$txAcertosDoisMed = median(txAcertosDois)
  resultado$txAcertostresMed = median(txAcertosTres)
  
  return (resultado)
}

processaKnn2 <- function(classe1, classe2, classe3, k) {
  txMedAcertos<-c()
  testes<-c()
  
  #Laço para alterar as proporções de 1 em 1 de 20 até 80
  for (k in 1:61) {
    txAcertos<-c()
    
    #Laço para rodar 30vezes a base de teste
    for (j in 1:30) {
      #Embaralhando a base original
      classe1 <- classe1[sample(1:nrow(classe1)), ]
      classe2 <- classe2[sample(1:nrow(classe2)), ]
      classe3 <- classe3[sample(1:nrow(classe3)), ]
      
      #Quebrando o dataset em treino e teste (80/20) OBS: Buscando as linhas de forma aleotória
      library(dplyr)
      dataTreinoClasse1<-sample_frac(classe1, (0.19 + (k/100)))
      dataTesteClasse1<-setdiff(classe1, dataTreinoClasse1)
      
      dataTreinoClasse2<-sample_frac(classe2, (0.19 + (k/100)))
      dataTesteClasse2<-setdiff(classe2, dataTreinoClasse2)
      
      dataTreinoClasse3<-sample_frac(classe3, (0.19 + (k/100)))
      dataTesteClasse3<-setdiff(classe3, dataTreinoClasse3)
      
      dataTreino <- rbind(dataTreinoClasse1, dataTreinoClasse2, dataTreinoClasse3)
      dataTeste <- rbind(dataTesteClasse1, dataTesteClasse2, dataTesteClasse3)
      
      #Variável para controlar os acertos do algoritmo
      qntAcertosTotal = 0
      
      #Laço para percorrer todas as linha do dataframe de teste
      for (i in 1: (nrow(dataTeste))) {
        #Recebe a linha atual do conjunto de teste
        linha = dataTeste[i,]
        
        #Chamando a função que classifica a linha atual
        resultKnn = knn(dataTreino, k, linha)
        
        #Verificando se o algoritmo acertou na classificação
        if (resultKnn$acertou) {
          qntAcertosTotal = qntAcertosTotal + 1
        }
      }
      
      #Calculando a taxa de acertos (Numero de acertos total / Quantidade de elementos testados)
      txAcerto = qntAcertosTotal / nrow(dataTeste)
      
      txAcertos[j]<-txAcerto
    }
    
    txMedAcertos[k]<-median(txAcertos)
    testes[k]<-(0.19 + (k/100))
  }
  
  resultado = list()
  
  resultado$txMedAcertos = txMedAcertos 
  resultado$testes = testes
  
  return (plot(resultado$txMedAcertos~resultado$testes))
}