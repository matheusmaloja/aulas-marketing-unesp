library(lattice)

#Puxando Dados 
seg.df <- read.csv("trabalho.csv", sep=",", header = TRUE)
seg.df$age <- round(seg.df$age, 0)
View(seg.df)

### Encontrando Descritivos por Grupo em R

#### Usando a Função `by()`para descobrir a média da renda por segmento

by(seg.df$income, seg.df$Segment, mean)
by(seg.df$income, list(seg.df$Segment, seg.df$subscribe), mean)

#### Usando a Função `aggregate()` para o mesmo exercício feito anteriormente
aggregate(seg.df$income, list(seg.df$Segment), mean)

#Variáveis respostas a esquerda e explicativas a direita
#y ~ x  # Fórmula Simples

#Agregar renda por segmento
#aggregate(formula, data, FUN)
aggregate(income ~ Segment, data = seg.df, mean)

#Descritivo para grupo de duas variáveis
aggregate(income ~ Segment + ownHome, data = seg.df, mean)

#Estrutura permite quantas quiser
aggregate(income ~ Segment + ownHome + subscribe, data = seg.df, mean)

#Atribuir resultado a dataframe
agg.data <- aggregate(income ~ Segment + ownHome, data = seg.df, mean)

#Obter frequencias de diferentes Segmentos e Casa Própria
with(seg.df, table(Segment, ownHome))

#Obter frequencias de crianças por segmentos
with(seg.df, table(kids, Segment))

#Somatório de filhos por segmentos
xtabs(kids ~ Segment, data = seg.df)
#ou
aggregate(kids ~ Segment, data = seg.df, sum)

# Frequências de cada segmento
table(seg.df$Segment)

#Proporção subscribe por segmento. Proporção vem por default
seg.df$subscribe <- as.factor(seg.df$subscribe)
histogram(~subscribe | Segment, data=seg.df)

#se quisermos contar os subscribes 
histogram(~subscribe | Segment, data=seg.df, type="count", layout=c(4,1), col=c("burlywood", "darkolivegreen"))

#Ver dentro de cada segmento, discriminado por casa própria ou não
histogram(~subscribe | Segment + ownHome, data=seg.df)

#Por fim, poderíamos plotar apenas as proporções de "sim" em vez de barras de "sim" e "não".
prop.table(table(seg.df$subscribe, seg.df$Segment), margin=2)
barchart(prop.table(table(seg.df$subscribe, seg.df$Segment), margin=2)[2,],
         xlab="Proporção de Assinantes por Segmento", col="darkolivegreen")

#O resultado comunica fortemente que o segmento "Suburb mix" tem uma taxa de #assinatura aparentemente baixa

#Visualização por Grupos - dados continuos

#Mas e quanto aos dados contínuos? Como plotar a renda por segmento em nossos dados? 
seg.mean <- aggregate(income ~ Segment, data=seg.df, mean)
library(lattice)
barchart(income ~ Segment, data=seg.mean, col="red")

#Dividindo ainda mais os dados de posse por casa
seg.income.agg <- aggregate(income ~ Segment + ownHome, data=seg.df, mean)
barchart(income ~ Segment, data=seg.income.agg,
         groups=ownHome, auto.key=TRUE,
         par.settings = simpleTheme(col=terrain.colors(3)))

#Usando o `boxplot()` para plotar um box-and-whiskers plot por fator para comparar valores de dados contínuos, como a renda para diferentes grupos.
boxplot(income ~ Segment, data=seg.df, yaxt="n", ylab="Renda ($k)")
ax.seq <- seq(from=0, to=120000, by=20000)
axis(side=2, at=ax.seq, labels=paste(ax.seq/1000, "k", sep=""), las=1)

#Para plotar um gráfico mais bonito
bwplot(Segment ~ income, data=seg.df, horizontal=TRUE, xlab="Renda")

#detalhar a posse de casa como uma variável de condicionamento usando `| ownHome` na fórmula
bwplot(Segment ~ income | ownHome, data=seg.df, horizontal=TRUE, xlab="Renda")

#Neste gráfico, descobrimos—entre outras coisas—que, em nossos dados simulados, o #segmento "Travelers" tem uma distribuição muito mais ampla de renda entre aqueles que #possuem suas casas do que entre aqueles que não possuem.
#Dados para comparação de Grupos
#Verifica um resumo dos dados
summary(seg.df)

#Teste de Chi-Quadrado

#Colocar na apresentação

#Tamanho da amostra por segmento
chisq.test(table(seg.df$Segment))

#O valor de p é 0.0006, o que indica que os tamanhos 
#dos segmentos são significativamente diferentes.

#Independência entre fatores

#Para verificar se o status de assinatura é independente da posse de casa, construímos uma tabela cruzada e aplicamos o teste qui-quadrado:

table(seg.df$subscribe, seg.df$ownHome)
chisq.test(table(seg.df$subscribe, seg.df$ownHome))

#O valor de p é 0.919, indicando que não há evidências suficientes para sugerir uma relação entre status de assinatura e posse de casa.

#TESTE T: Média entre grupos

#Um teste t compara a média de uma amostra com a média de outra amostra (ou com um #valor específico, como 0). O ponto importante é que ele compara a média de exatamente #dois conjuntos de dados. Por exemplo, nos dados segmentados, poderíamos querer saber #se a renda domiciliar é diferente entre aqueles que possuem uma casa e aqueles que não #possuem.

#Verificando distribuição

hist(seg.df$income)  
with(seg.df, hist(income[ownHome == "ownYes"])) 
with(seg.df, hist(income[ownHome == "ownNo"]))

#Teste T de renda por status de casa

t.test(income ~ ownHome, data=seg.df)  

#Há várias informações importantes na saída do `t.test()`. 
#Primeiro, vemos que a estatística #t é -3.2, com um p-valor de 0.0012. 
#Isso significa que a hipótese nula de nenhuma 
#diferença na renda por posse de casa é rejeitada. 
#Os dados sugerem que pessoas que #possuem suas casas têm uma renda mais alta.

#Mesma diferença mas dentro do grupo de viajantes
t.test(income ~ ownHome, data=subset(seg.df, Segment == "Travelers"))  

#O intervalo de confiança de -8508 a 11107 inclui 0, e, 
#portanto, concluímos—como evidenciado pelo p-valor de 0.79—que não há uma 
#diferença significativa na renda média 
#entre os "Travelers" em nossos dados que possuem casas e os que não possuem.

#Localizando onde está a diferença de salario: ANOVA 

#Uma análise de variância (ANOVA) compara as médias de múltiplos grupos. #Tecnicamente, isso é feito comparando o grau em que os grupos diferem, medido pela #variância em suas médias (entre os grupos), em relação à variância das observações em #torno de cada média (dentro de cada grupo). 

#Renda por Status de Casa
seg.aov.own <- aov(income ~ ownHome , data=seg.df)
anova(seg.aov.own)

#Renda por Segmento
seg.aov.seg <- aov(income ~ Segment , data=seg.df)
anova(seg.aov.seg)

#E para ambos?
seg.aov.segownhome <- anova(aov(income ~ Segment + ownHome , data=seg.df))
seg.aov.segownhome

#Rodar uma anova com todos as variáveis

seg.aov.step <- step(aov(income ~ ., data=seg.df))

