# Geração de Amostras

n_amostras <- 1000  
tamanho_amostra <- 15  
cv <- 0.1  


set.seed(124)  
media <- runif(n_amostras,min = 100, max = 200)
desvio_padrao <- cv * media

mean(media)
medias_amostrais <- numeric(n_amostras)
desvios_amostrais <- numeric(n_amostras)
ics <- matrix(nrow = n_amostras, ncol = 2)


for (i in 1:n_amostras) {
  amostra <- rnorm(tamanho_amostra, mean = media[i], sd = desvio_padrao[i])
  medias_amostrais[i] <- mean(amostra)
  desvios_amostrais[i] <- sd(amostra)
  ics[i, ] <- t.test(amostra)$conf.int
}

# Análise Descritiva das Médias Amostrais

cat("Estatísticas das médias amostrais:\n")
cat("Média:", mean(medias_amostrais), "\n")
cat("Mediana:", median(medias_amostrais), "\n")
cat("Desvio Padrão:", sd(medias_amostrais), "\n")

hist(medias_amostrais, breaks = "FD", main = "Histograma das Médias Amostrais")


# Análise dos Intervalos de Confiança (IC95%)

ic_contem_media <- 0  
ic_aleatorios <- sample(1:n_amostras, 100)  

for (i in ic_aleatorios) {
  if (ics[i, 1] <= media[i] && ics[i, 2] >= media[i]) {
    ic_contem_media <- ic_contem_media + 1
  }
}


# Gráfico dos ICs aleatórios
plot(1, type = "n", xlim = c(0, n_amostras), ylim = range(ics), xlab = "Amostra", ylab = "IC95%")
for (i in ic_aleatorios) {
  lines(c(i, i), ics[i, ], col = "blue")
}
points(ic_aleatorios, media[ic_aleatorios], pch = 19, col = "red")





