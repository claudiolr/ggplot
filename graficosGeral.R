##########################################################################
## Gráficos
##########################################################################

##########################################################################
## Base Graphics
##########################################################################

# Base de dados de IDH
dd <- read.table("D:/CienciaDados/Cursos/R/DataWrangling/BasesDados/TXT/IDH_BR.txt",
                 header = TRUE, sep = "\t", skip = 12, dec=",")

# Criando regiões
dd$REGIAO <- ifelse(dd$UF %in% c("AM","RR","AP","PA","TO","RO","AC"), "1.Norte",
                    ifelse(dd$UF %in% c("MA","PI","CE","RN","PE","PB","SE","AL","BA"), "2.Nordeste",
                           ifelse(dd$UF %in% c("MT","MS","GO"), "3.Centro-Oeste",
                                  ifelse(dd$UF %in% c("SP","RJ","ES","MG"), "4.Sudeste", "5.Sul"))))
dd$REGIAO <- factor(dd$REGIAO, ordered = TRUE)
prop.table(table(dd$REGIAO))

# Criando escala
## Escala de IDH referência
## 0,800 a 1,000 Muito Alto
## 0,700 a 0,799 Alto
## 0,600 a 0,699 Médio
## 0,500 a 0,599 Baixo
## 0,000 a 0,499 Muito Baixo

dd$ESCALA <- ifelse(dd$IDHM_2010 <= 0.499, "1.Muito Baixo",
                    ifelse(dd$IDHM_2010 <= 0.599, "2.Baixo",
                           ifelse(dd$IDHM_2010 <= 0.699, "3.Médio",
                                  ifelse(dd$IDHM_2010 <= 0.799, "4.Alto","5.Muito Alto"))))

dd$ESCALA <- factor(dd$ESCALA, ordered = TRUE)                 
prop.table(table(dd$ESCALA))

##########################################################################
# Exemplos: Função par()
##########################################################################

pa <- par()         # Salva parâmtros gráficos no objeto pa
names(pa)[1:30]

### Redefine a área gráfica para dois gráficos com magens alteradas
par(mfrow = c(1, 3), mar = c(5,5,1,1), oma = c(0,0,2,0))
# mfrow divide a área gráfica em x linhas e y colunas
# argumento 'mar' = 'margens', no caso abaixo, esquerda, acima, direita
# argumento 'oma' = valores das margens externas, na ordem: baixo, esquerda, cima, direita


plot(dd$IDHM_2010, dd$IDHM_RENDA);
plot(x = runif(50), y = runif(50))
# Redefine a área gráfica para dois gráficos com magens internas
# e externas alteradas e com disposição do texto dos eixos 
# x e y alterados

par(mfrow = c(1, 2), mar = c(5,5,1,1), oma = c(1,1,0,0), las = 1)
# argumento 'las' = posição dos valores dos eixos (0 a 3)

plot(dd$IDHM_2010, dd$IDHM_RENDA);
plot(x = runif(50), y = runif(50))
par(pa)  # Redefine a janela gráfica

##########################################################################
# Exemplos: layout()
##########################################################################

# Matriz de tamanhos
A <- matrix(c(1,1,2,3), ncol = 2, byrow = TRUE)
layout(A)
par(mar = c(4,4,1,1), oma = c(1,1,1,1))

plot(dd$IDHM_2010, dd$IDHM_RENDA);
plot(x = runif(50), y = runif(50))
plot(x = runif(50), y = runif(50))

pa <- par()         # Salva parâmtros gráficos no objeto pa
# Matriz de tamanhos

A <- matrix(c(1,2,1,3), ncol = 2, byrow = TRUE)

layout(A, widths = c(1,1), heights = c(1,1))
par(mar = c(4,4,1,1), oma = c(1,1,1,1))
plot(dd$IDHM_2010, dd$IDHM_RENDA);
plot(x = runif(50), y = runif(50))
plot(x = runif(50), y = runif(50))

##########################################################################
# Exemplos: A função plot()
##########################################################################

# Gráfico padrão
par(mfrow = c(1,2), las = 1) # Dois gráficos na tela
plot(dd$IDHM_2010, dd$IDHM_RENDA)
plot(dd$REGIAO)

# Nomeando títulos e eixos
par(mfrow = c(1,1), las = 1)
plot(dd$IDHM_2010, dd$IDHM_RENDA,
     main = "Gráfico IDH",
     xlab = "IDH 2010",
     ylab = "IDH Renda")

# Incluindo cores e tipo de traço
plot(dd$IDHM_2010,
     main = "Gráfico IDH",
     ylab = "IDH 2010",
     xlab = "Frequência",
     col="2", type="b")

par(mfrow = c(2,2), mar = c(4,4,1,1)) # Quatro gráficos na tela

plot(IDHM_2010 ~ IDHM_RENDA, data = dd, main = "Barras", #Uso formula
     xlab = "Renda(x)", ylab="IDH(y)",
     col="blue", type="h", lwd = 1)

plot(IDHM_2010 ~ IDHM_RENDA, data = dd, main = "Linhas", #Uso formula
     xlab = "Renda(x)", ylab="IDH(y)",
     col="darkgreen", type="l", lwd = 1)

plot(IDHM_2010 ~ IDHM_RENDA, data = dd, main = "Pontos", #Uso formula
     xlab = "Renda(x)", ylab="IDH(y)",
     col="grey90", type="p", lwd = 3)

plot(IDHM_2010 ~ IDHM_RENDA, data = dd, main = "Ponto e linha", #Uso formula
     xlab = "Renda(x)", ylab="IDH(y)",
     col="pink", type="b", lwd = 4)

# Exemplos: Alterando valores e direção dos eixos
par(mfrow = c(2,2), mar = c(4,4,1,1)) # Quatro gráficos na tela

plot(IDHM_2010 ~ IDHM_RENDA, data = dd,
     main = "Gráfico IDH", #Uso formula
     xlab = "IDH Renda(x)", ylab="IDH geral(y)",
     col="darkgreen",
     xlim = c(0.4, 1),
     ylim = c(0.4, 1),
     pch  = 1, lwd = 1, las = 1) # pch: tipo de pontos (ver ajuda de point(graphics))

plot(IDHM_2010 ~ IDHM_RENDA, data = dd,
     main = "Gráfico IDH", #Uso formula
     xlab = "IDH Renda(x)", ylab="IDH geral(y)",
     col="red",
     xlim=c(0.4, 1),
     ylim=c(0.4, 1),
     pch = 2, lwd = 6, las = 3)


plot(IDHM_2010 ~ IDHM_RENDA, data = dd,
     main = "Gráfico IDH", #Uso formula
     xlab = "IDH Renda(x)", ylab="IDH geral(y)",
     col="blue",
     xlim=c(0.4,1),
     ylim=c(0.4, 1),
     pch = 3, lwd = 6, las = 2)

# Eixos customizados
par(mfrow = c(1,1))

plot(IDHM_2010 ~ IDHM_RENDA, data = dd,
     main = "Gráfico IDH", #Uso formula
     xlab = "IDH Renda(x)", ylab="IDH geral(y)",
     col="9", axes = FALSE,
     pch = 3, lwd = 0.1, las = 2)

seq_eixos <- round(seq(0, 1, length.out = 10), 1)

axis(side = 1, seq_eixos, lwd = 1, col = "black", cex.axis = 1)
axis(side = 2, seq_eixos, lwd = 1, col = "black", las = 2, cex.axis = 1)

##########################################################################
## Adicionando elementos no gráfico
##########################################################################

# Exemplos: Legendas
par(mfrow = c(1,1), mar = c(5,5,1,1)) #
plot(x = dd$IDHM_RENDA,
     type = "o",
     main = "Gráfico IDH", 
     xlab = "Índice",
     ylab="",
     col="darkgreen",
     lwd = 1,
     pch = 5,
     las = 1)

lines(x = dd$IDHM_2010,
      type = "p",
      main = "Gráfico IDH",
      xlab = "Índice",
      ylab="",
      col="red",
      lwd = 3,
      pch = 3,
      las = 1)

legend("bottom",
       legend = c("Renda","Geral"),
       col = c("darkgreen","red"),
       cex = 1,
       bty = "n", # remove borda da legenda
       pch = c(5, 6),
       lwd = c(2, 2))

# Exemplo com mtcars: Pontos, linhas e textos
par(mfrow = c(1,1), mar = c(4,4,2,2)) # Um
plot(cars, main = "",
     xlab = "Velocidade(x)",
     ylab = "Distribuição(y)",
     xlim = c(0, 28),
     ylim = c(0, 110),
     col  ="blue",
     pch  = 19,
     lwd  = 5,
     las  = 1)

legend("bottomright",
       legend = "velocidade x distância",
       col="blue",
       pch=21,
       lwd=1,
       bty = "n" # remove a borda da legenda
)

arrows(x0 = 10, y0 = 85, x1 = 22, y1=91, lwd = 2, pch = 2, code = 2, col = 3)
points(x  = 24, y  = 92, col = "red", pch = 5, lwd = 15)
text(x=10, y=80, labels="Batmovel")

abline(v = median(cars$speed), col = "black", lwd = 3, lty = 3)
abline(h = median(cars$dist),  col = "black", lwd = 3, lty = 3)


# Desenhe um quadrado
plot(0:11, 0:11, pch = ".")
arrows(x0 =  1, y0 =  1, x1 =  1, y1 = 10, lwd = 2, pch = 2, code = 0, col = 2)
arrows(x0 =  1, y0 =  1, x1 = 10, y1 =  1, lwd = 2, pch = 2, code = 0, col = 3)
arrows(x0 = 10, y0 =  1, x1 = 10, y1 = 10, lwd = 2, pch = 2, code = 0, col = 4)
arrows(x0 =  1, y0 = 10, x1 = 10, y1 = 10, lwd = 2, pch = 2, code = 0, col = 5)

abline(h = 9)
abline(h = 2)
abline(v = 9)
abline(v = 2)

##########################################################################
## Gráficos Estatísticos
##########################################################################

# Preparando uma palette de cores legais com RColorBrewer

# RColorBrewer::display.brewer.all()
# brewer.pal.info
require("RColorBrewer")

RColorBrewer::display.brewer.all()

palette1 <- brewer.pal(5,"Greens")
palette2 <- brewer.pal(5,"Blues")
palette3 <- brewer.pal(9, "BuPu")[2:6]
palette4 <- brewer.pal(11, "PRGn")

# Cores com base nos dados
dd$minha_cor_1 <-ifelse(dd$ESCALA == "1.Muito Baixo", palette1[1],
                        ifelse(dd$ESCALA == "2.Baixo", palette1[2],
                               ifelse(dd$ESCALA == "3.Médio", palette1[3],
                                      ifelse(dd$ESCALA == "4.Alto", palette1[4], palette1[5]))))

dd$minha_cor_2 <-ifelse(dd$ESCALA == "1.Muito Baixo", palette2[1],
                        ifelse(dd$ESCALA == "2.Baixo", palette2[2],
                               ifelse(dd$ESCALA == "3.Médio", palette2[3],
                                      ifelse(dd$ESCALA == "4.Alto", palette2[4], palette2[5]))))

dd$minha_cor_3 <-ifelse(dd$ESCALA == "1.Muito Baixo", palette3[1],
                        ifelse(dd$ESCALA == "2.Baixo", palette3[2],
                               ifelse(dd$ESCALA == "3.Médio", palette3[3],
                                      ifelse(dd$ESCALA == "4.Alto", palette3[4], palette3[5]))))


##########################################################################
## Gráficos Estatísticos: Na prática
##########################################################################

# Divisão da Janela Gráfica
A <- matrix(c(1,1,2,3), ncol = 2, byrow = TRUE)
layout(A)
par(mar = c(4,4,1,1))

# Scatterplot
plot(dd$IDHM_2010, dd$IDHM_LONGEVIDADE,
     col = dd$ESCALA,
     pch = 16)
legend("bottomright",
       legend = levels(dd$ESCALA),
       col = 1:5,
       pch = 16,
       bty = "n")

plot(dd$IDHM_2010, dd$IDHM_LONGEVIDADE,
     col = dd$minha_cor_1, 
     pch = 16, lwd = 15)
legend("bottomright", 
       legend = levels(dd$ESCALA), 
       col = palette1,
       pch = 16,
       bty = "n")

plot(dd$IDHM_2010, dd$IDHM_LONGEVIDADE,
     col = dd$minha_cor_3,
     pch = 16, lwd = 15)
legend("bottomright",
       legend = levels(dd$ESCALA), 
       col = palette3, 
       pch = 16,
       bty = "n")

# Histograma
#brewer.pal.info
hist(dd$IDHM_2010, breaks = 20, freq = FALSE,  col = brewer.pal(8,"BrBG"))
hist(dd$IDHM_2010, breaks = 20, freq = TRUE,  col = brewer.pal(8,"RdYlBu"))
hist(dd$IDHM_2010, breaks = 20, freq = TRUE,  col = brewer.pal(8,"Greens"))
# freq: define se a escala será mostrada em densidade ou em valores


# Densidade
plot(density(dd$IDHM_2010), type = "h", col = 3)
plot(density(dd$IDHM_2010), type = "h", col = palette2[2])
plot(density(dd$IDHM_2010), type = "h", col = palette4[2])

# Barras
tab <- table(dd$ESCALA)
par(mfrow = c(2, 2), mar = c(5,5,1,1), oma = c(1,1,1,1))
barplot(tab, col = palette1, space = 0.05)
barplot(tab, col = palette2, space = 0.05, horiz = TRUE)
barplot(tab, col = palette3, space = 0.10, horiz = TRUE, las = 1)
barplot(tab, col = palette4, space = 0.10, horiz = TRUE, las = 1, border = NA)

# Box-Plot
mat <- matrix(c(1,1,3,2,2,3), ncol = 3, byrow = TRUE)
layout(mat)
par(mar = c(4,4,1,1), oma = c(1,1,1,1))
boxplot(IDHM_2010 ~ REGIAO, data = dd,
        col = palette1, las = 1,
        cex.lab = 2, cex.axis = 2, cex.main = 2)

boxplot(dd[,4:7], col = palette2, las = 1, cex.axis = 2)
boxplot(dd$IDHM_LONGEVIDADE, col = palette1[4], las = 1,
        notch = TRUE, cex.axis = 2)

# Setores
tab1 <- prop.table(table(dd$REGIAO))
tab2 <- prop.table(table(dd$ESCALA))

par(mfrow = c(2,2), mar = c(4,4,1,1), oma = c(1,1,1,1))
pct1 <- paste(names(tab1), " (", round(100*tab1, 2),"%)", sep="")

pie(prop.table(tab1), col = 1:5)
pie(tab1, labels = pct1, col = palette1)

pct2 <- paste(names(tab2), " (", round(100*tab2, 2),"%)", sep="")
pie(prop.table(tab2), col = palette2)
pie(tab2, labels = pct2, col = palette3)

# Função com curve
fx <- function(x)  
     curve(fx, from = -8, to = 13,
           col = palette1[4], lwd = 2,
           main = expression(f(x) == 12 + 4*x - 13*x^2 - 10*x^3 + x^4 - 56),
           xlab = "x[-8, 13]", ylab = "",
           axes = FALSE
     )

x <- round(seq(-8, 13, length.out = 10), 1)
y <- round(seq(min(fx(x)), max(fx(x)), length.out = 10),  1)

axis(1, x)
axis(2, y, las = 1)

# Mosaico

tab3 <- table(dd[,c("REGIAO","ESCALA")])

par(mfrow = c(1,1))
mosaicplot(tab3, color = brewer.pal(9, "YlGnBu"),
           dir = "h", las = 1)

mosaicplot(~REGIAO + ESCALA, data = dd, 
           color = brewer.pal(11, "BrBG"), 
           dir = "h", las = 1,
           cex.axis = 1.2)

##########################################################################
## lattice Graphics
##########################################################################
require(grid)
require(gridExtra)
require(lattice)
require(latticeExtra)

# Scatteplot
g1 <- xyplot(IDHM_2010 ~ IDHM_EDUCACAO,
             data = dd,
             main = "IDH BR 2010",
             xlab = "IHD educação",
             ylab="IDH geral",
             col = 3,
             lwd = 3
)

g2 <- xyplot(IDHM_2010 ~ IDHM_EDUCACAO, data = dd, main = "IDH BR 2010",
             xlab = "IHD educação", ylab="IDH geral", col = 4, lwd = 3)

grid.arrange(g1, g2, nrow = 2)


g1 <- xyplot(IDHM_2010 ~ IDHM_EDUCACAO + IDHM_RENDA,
             data = dd,
             main = "IDH BR 2010",
             xlab = "", ylab="IDH geral",
             col = 2:3, lwd = 2:3, pch = 21:22,
             
             key = list(text   = list(lab = c("IDH edu","IDH renda")),
                        points = list(pch = 21:22, col = 2:3),
                        space  = "bottom", cex=1.0, just = 0.95, lwd = 2:3)
             #lines = list(lty = 2:3, lwd = 4:5, col = 2:3))
); g1

g1 <- xyplot(IDHM_2010 ~ IDHM_EDUCACAO | REGIAO, data = dd,
             main = "IDH BR 2010",
             xlab = "",
             ylab="IDH geral",
             layout = c(5, 1),
             col = palette1[4],
             lwd = 3,
             pch = 21,
             key = list(text = list(lab = c("IDH edu")),
                        points = list(pch = 21, col = 3),
                        space = "bottom",
                        cex=1.0,
                        just = 0.95,
                        lwd = 3)
); g1

# Histograma
histogram(~ IDHM_EDUCACAO | REGIAO,
          data = dd, 
          type = "percent", #percent, count
          col = palette4,
          breaks = 5,
          layout = c(5,1))

# Densidade
densityplot( ~ IDHM_EDUCACAO | ESCALA,
             data = dd, kernel = "gaussian",
             type = "percent", #percent, count
             col = palette1[5],
             layout = c(5,1),
             lwd = 3
)

densityplot( ~ IDHM_EDUCACAO | ESCALA + REGIAO,
             data = dd, kernel = "gaussian",
             type = "percent", #percent, count
             col = palette1[5],
             #layout = c(5,1),
             lwd = 3
)

# Box-Plot
bwplot( IDHM_EDUCACAO ~ REGIAO,
        data = dd, aspect = 1,
        col = palette3, horizontal = FALSE,
        lwd = 3)

bwplot( REGIAO ~IDHM_EDUCACAO,
        data = dd, aspect = 1,
        fill = palette2,
        col = "black",
        horizontal = TRUE,
        lwd = 3)

bwplot( IDHM_EDUCACAO ~ REGIAO | ESCALA,
        data = dd, aspect = 1,
        col = palette3,
        layout = c(3,2),
        lwd = 3)

# Barras
barchart(IDHM_EDUCACAO ~ ESCALA,
         data = dd,
         #         groups = REGIAO,
         col = palette4,
         stack = FALSE
)

barchart(IDHM_EDUCACAO ~ REGIAO,
         data = dd,
         #groups = REGIAO,
         col = palette4,
         fill = palette3,
         stack = TRUE,
         auto.key = list(space = "bottom"),
         scales = list(x = list(rot = 45)))


##########################################################################
## ggplot Graphics
##########################################################################
require(ggplot2)
require(gridExtra)
require("RColorBrewer")

palette1 <- brewer.pal(9,"Greens")
palette2 <- brewer.pal(9,"Blues")

# Lendo dados
dd <- read.table("D:/CienciaDados/Cursos/R/DataWrangling/BasesDados/TXT/IDH_BR.txt",
                 header = TRUE, sep = "\t", skip = 12, dec=",")

# Criando regiões
dd$REGIAO <- ifelse(dd$UF %in% c("AM","RR","AP","PA","TO","RO","AC"), "1.Norte",
                    ifelse(dd$UF %in% c("MA","PI","CE","RN","PE","PB","SE","AL","BA"), "2.Nordeste",
                           ifelse(dd$UF %in% c("MT","MS","GO"), "3.Centro-Oeste",
                                  ifelse(dd$UF %in% c("SP","RJ","ES","MG"), "4.Sudeste", "5.Sul"))))
dd$REGIAO <- factor(dd$REGIAO, ordered = TRUE)

# Criando escala
dd$ESCALA <- ifelse(dd$IDHM_2010 <= 0.499, "1.Muito Baixo",
                    ifelse(dd$IDHM_2010 <= 0.599, "2.Baixo",
                           ifelse(dd$IDHM_2010 <= 0.699, "3.Médio",
                                  ifelse(dd$IDHM_2010 <= 0.799, "4.Alto","5.Muito Alto"))))

dd$ESCALA <- factor(dd$ESCALA, ordered = TRUE)                 


# Mapeamentos estéticos
# Exemplo com nossos dados de IDH. Criando um scatterplot
ggplot(data = dd) +
     geom_point(mapping = aes(x = IDHM_2010, y = IDHM_RENDA, color = REGIAO, size = IDHM_EDUCACAO)) +
     labs(title = "Gráfico de pontos entre IDH total vs de renda por região",
          x = "IDH renda", y = "IDH Brasil") + 
     scale_colour_brewer(palette = "Greens")

# Facets simples

ggplot(data = dd) +
     geom_point(mapping = aes(x = IDHM_2010, y = IDHM_RENDA)) +
     ggtitle("Gráfico de pontos entre IDH total vs de renda por região") +
     xlab("IDH renda") + ylab("IDH Brasil") +
     facet_wrap( ~ REGIAO, nrow = 1, ncol = 5)

# Facets composto
da <- dd[order(dd$REGIAO, dd$UF), ] # Ordenando dados
ggplot(data = da) +
     geom_point(mapping = aes(x = IDHM_2010, y = IDHM_RENDA)) +
     ggtitle("Gráfico de pontos entre IDH total vs de renda por região") +
     xlab("IDH renda") + ylab("IDH Brasil") +
     facet_wrap(REGIAO ~ UF) # formula com direita e esquerda

# Geometrias (geoms)
g1 <- ggplot(data = da) +
     geom_point(mapping = aes(x = IDHM_2010, y = IDHM_RENDA)) +
     ggtitle("Gráfico de pontos") + xlab("IDH renda") + ylab("IDH Brasil")

g2 <-  ggplot(data = da) +
     geom_smooth(mapping = aes(x = IDHM_2010, y = IDHM_RENDA)) +
     ggtitle("Gráfico de linha suavizada") + xlab("IDH renda") + ylab("IDH Brasil")

gridExtra::grid.arrange(g1, g2, nrow = 1)


g1 <-  ggplot(data = da) +
     geom_smooth(mapping = aes(x = IDHM_2010, y = IDHM_RENDA, linetype = REGIAO)) +
     ggtitle("Gráfico de linha suavizada (tipo de linha)") + xlab("IDH renda") + ylab("IDH Brasil")

g2 <-  ggplot(data = da) +
     geom_smooth(mapping = aes(x = IDHM_2010, y = IDHM_RENDA, group = REGIAO)) +
     ggtitle("Gráfico de linha suavizada (grupo)") + xlab("IDH renda") + ylab("IDH Brasil")

g3 <-  ggplot(data = da) +
     geom_smooth(mapping = aes(x = IDHM_2010, y = IDHM_RENDA, color = REGIAO)) +
     ggtitle("Gráfico de linha suavizada (cor)") + xlab("IDH renda") + ylab("IDH Brasil")

gridExtra::grid.arrange(g1, g2, g3, nrow = 1)

# Passando mappings para a função ggplot, melhora o uso dos geoms.
g1 <- ggplot(data = da, mapping = aes(x = IDHM_2010, y = IDHM_RENDA)) +
     geom_smooth() +
     geom_point(mapping = aes(color = REGIAO)) +
     ggtitle("Gráfico de linha suavizada (multiplos geoms)") + xlab("IDH renda") + ylab("IDH Brasil")
g1

# Transformações estatísticas (Gráfico de Barras)
g1 <- ggplot(data = da) + geom_bar(mapping = aes(x = REGIAO)) +
     ggtitle("Gráfico de barras (Frequências)")

g2 <- ggplot(data = da) + geom_bar(mapping = aes(x = REGIAO, y = ..prop.., group = 1)) +
     ggtitle("Gráfico de barras (Proporção)")

gridExtra::grid.arrange(g1, g2, nrow = 1)

# Transformações estatísticas (Gráfico de Barras, cores e prenchimento)
g1 <- ggplot(data = da, mapping = aes(x = REGIAO, color = REGIAO)) + 
     geom_bar(fill = NA, position = "identity") + ggtitle("Gráfico de barras (fill)") +   scale_colour_brewer()

g2 <- ggplot(data = da, mapping = aes(x = REGIAO, fill = REGIAO)) + 
     geom_bar(alpha = 0.3, position = "identity") + ggtitle("Gráfico de barras (transparência)")

g3 <- ggplot(data = da) + geom_bar(mapping = aes(x = REGIAO, fill = UF), position = "dodge") +
     ggtitle("Gráfico de barras (fill)")

gridExtra::grid.arrange(g1, g2, g3, nrow = 1)

# Coordenadas
br <- map_data("world2", "brazil") # Dados mapa do Brasil

g1 <- ggplot(data = da, mapping = aes(x = REGIAO, fill = REGIAO)) +
     ggtitle("Gráfico de barras (horizontal)") +
     geom_bar() + coord_flip()

g2 <- ggplot(data = da, mapping = aes(x = REGIAO, fill = REGIAO)) +
     ggtitle("Gráfico de barras (polar)") +
     geom_bar() + coord_polar()

g3 <- ggplot(br) + ggtitle("Mapa do Brasil") +
     geom_polygon(mapping = aes(long, lat, group = group, fill = group)) +
     coord_quickmap() + scale_fill_gradient(low = "blue", high = "green")

gridExtra::grid.arrange(g1, g2, g3, nrow = 2)


##########################################################################
## Gráficos por tipo e quantidade de variáveis
##########################################################################

# Exemplos: variável quantitativa
g <- ggplot(data = da, mapping = aes(x = IDHM_2010))
g1 <- g + geom_histogram(bins = 10, col = "white")
g2 <- g + geom_density(alpha = 0.6, color = "black", fill = "pink")
g3 <- g + geom_freqpoly(bins = 10)
g4 <- g + geom_dotplot(col = "red")

gridExtra::grid.arrange(g1, g2, g3, g4, nrow = 2)


# Exemplos: variável quantitativa por grupos
f <- ggplot(data = da, mapping = aes(x = IDHM_2010, fill = REGIAO))
g <- ggplot(data = da, mapping = aes(x = REGIAO, y = IDHM_EDUCACAO, fill = REGIAO))
g1 <- f + geom_density(alpha = 0.4)
g2 <- g + geom_boxplot()
g3 <- g + geom_boxplot() + coord_flip()
g4 <- g + geom_violin()

gridExtra::grid.arrange(g1, g4, g2, g3, nrow = 2)


# Exemplos: variável categórica
g <- ggplot(data = da, mapping = aes(x = REGIAO, fill = REGIAO))
g1 <- g + geom_bar() + scale_fill_brewer(palette = "Blues")
g2 <- g + geom_bar() + coord_flip() + scale_fill_brewer(palette = "Blues")
g3 <- g + geom_bar(position = "fill", width = 2) + coord_flip()+ scale_fill_brewer(palette = "Greens")
g4 <- g + geom_bar(position = "fill", width = 2) + coord_polar()+ scale_fill_brewer(palette = "Greens")

gridExtra::grid.arrange(g1, g2, g3, g4, nrow = 2)


##########################################################################
## ggplot Graphics: Extras ggally
##########################################################################

# Multiplos gráficos com ggally
require(GGally, quietly = TRUE)
da <- dd[, c(4:5, 7:8)]

g1 <- ggpairs(da, mapping = aes(color = REGIAO)) +
     ggtitle("Correlograma")
g1

# Correlogramas
ggcorr(mtcars, nbreaks = 10)

ggcorr(dd[,4:7],
       name = expression(rho),
       geom = "circle",
       max_size = 10,
       min_size = 2,
       #size = 3,
       hjust = 0.75,
       nbreaks = 10,
       angle = -45,
       palette = "RdYlGn" # paleta do RColorBrewer
)

scatmat(dd[,4:7])

##########################################################################
## ggplot Graphics: Extras Temas ggplot2 + ggthemes
##########################################################################
require(ggthemes, quietly = TRUE)

g1 <- ggplot(data = dd,
             mapping = aes(x = REGIAO, y = IDHM_EDUCACAO, fill = REGIAO))

g2 <- g1 + geom_boxplot(show.legend = FALSE) +
     labs(x = "", y = "", title = "")+
     scale_fill_brewer(palette = "RdYlGn")

g3 <- g1 + geom_col(show.legend = FALSE) +
     labs(x = "", y = "", title = "")+
     scale_fill_brewer(palette = "Greens")

g2 + theme_base()
g2 + theme_economist()
g2 + theme_excel()

f1 <- g2 + theme_hc(base_size = 15)
f2 <- g2 + theme_tufte(base_size = 15)
f3 <- g3 + theme_stata(base_family = "Verdana")
f4 <- g3 + theme_fivethirtyeight()
gridExtra::grid.arrange(f1, f2, f3, f4, nrow = 2)

##########################################################################
## ggplot Graphics: Extras Interatividade do _ggplot_ com _plotly_
##########################################################################

require(plotly, quietly = TRUE)
require(ggplot2)

g1 <- ggplot(data = da, 
             mapping = aes(x = REGIAO, y = IDHM_EDUCACAO, fill = REGIAO))

g2 <- g1 + geom_boxplot(show.legend = FALSE, size = 0.5) +
     labs(x = "", y = "", title = "") +
     scale_fill_brewer(palette = "RdYlGn")
g2
ggplotly(g2)

# Omite a leganda
g2 <- hide_legend(ggplotly(g2))

g3 <- ggplot(data = da) + 
     geom_histogram(aes(x = IDHM_EDUCACAO, fill = REGIAO), show.legend = FALSE) +
     labs(x = "", y = "", title = "") + 
     scale_fill_brewer(palette = "RdYlGn")

ggplotly(g3)

g3 <- hide_legend(ggplotly(g3))

# Organiza mais de uma figura por visualização.
subplot(g2, g3)


##########################################################################
## Fim do script
##########################################################################
