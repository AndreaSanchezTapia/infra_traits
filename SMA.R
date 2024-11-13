setwd('C:/Users/Nathalie/Desktop/DOUTORADO/TESE/Capítulo 2/RStudio')
dados<-read.table("completa.csv", header = TRUE, dec=",", sep=";")
head(dado)

available.packages()["smatr", ]
install.packages("C:/Users/Nathalie/Desktop/DOUTORADO/TESE/Capítulo 2/RStudio/smatr", repos = NULL, type = "source")

install.packages("smatr")
install.packages(c("ggplot2", "dplyr"))
library(smatr)
library(ggplot2)
library(dplyr)

setwd(choose.dir("C:/Users/Nathalie/Desktop/DOUTORADO/TESE/Capítulo 2/RStudio")) #diretório para salvar os resultados

dados<-read.csv(choose.files("completa.csv"),sep=";",dec=",") %>% #não esqueça de usar "." para decimal
  arrange(Age,Species) %>% 
  mutate(.,
         Age=as.factor(Age),#transforma em fator
         Species=as.factor(Species))#transforma em fator

vari<-names(dados[,3:9])#nome das variáveis
combi<-combn(names(dados[,3:9]),2) # combinação de todas as variáveis

formulasEle<-apply(combi,2,function(x){#transforma em formulas
  paste0(x[1],"~",x[2],"+Age") %>% 
    as.formula()
})
formulasSlo<-apply(combi,2,function(x){#transforma em formulas
  paste0(x[1],"~",x[2],"*Age") %>% 
    as.formula()
})

resultadosSlo<-lapply(formulasSlo, function(formula){#lista dos ajustes dos modelos de slope e elevation
  slop<-sma(formula,log="xy",data=dados,multcomp=TRUE,multcompmethod="adjusted")
})

resultadosEle<-lapply(formulasEle, function(formula){#lista dos ajustes dos modelos de slope e elevation
  slop<-sma(formula,log="xy",data=dados,multcomp=TRUE,multcompmethod="adjusted")
})


names(resultadosSlo)<-formulasSlo # atribui as formulas como nome dos itens da lista de resultados
names(resultadosEle)<-formulasEle # atribui as formulas como nome dos itens da lista de resultados

resultados<-list(slope=resultadosSlo,elevation=resultadosEle)

#avaliação das premissas
for(x in 1:ncol(combi)){
  png(paste0(combi[1,x],combi[2,x],"_res.png"))
  plot(resultados[[x]][[1]],which="res");abline(h=0)
  dev.off()
  png(paste0(combi[1,x],combi[2,x],"_qq.png"))
  plot(resultados[[x]][[1]],which="qq")
  dev.off()
}

#gráfico das relações
for(x in 3:9){
  for(y in 3:9){
    if(x!=y){
      p<-ggplot(dados,aes(y=dados[,y],x=dados[,x],colour=Age))+
        geom_point()+
        theme_bw()+
        stat_smooth(method=lm)+
        xlab(names(dados)[x])+ ylab(names(dados)[y])
      print(p)
      ggsave(paste0(names(dados)[x],"_",names(dados)[y],".png"))
    }
  }
}

sma(formula = formula, data = dados, log = "xy", multcomp = TRUE, 
    multcompmethod = "adjusted")
