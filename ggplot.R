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
