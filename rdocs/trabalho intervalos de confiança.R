caminho_lucas <- "resultados"


# Definindo o tamanho da amostra e o número de amostras
tamanho_amostra <- 15
numero_amostras <- 1000

# Gerando a média e o desvio padrão
media <- runif(1, min = 100, max = 200)
media <- as.integer(media)
cv <- 0.1  # 10% de coeficiente de variação
desvio_padrao <- media * cv

# Inicializando uma matriz para armazenar as amostras
amostras <- matrix(nrow = numero_amostras, ncol = tamanho_amostra)

# Gerando as amostras
for (i in 1:numero_amostras) {
  amostras[i,] <- rnorm(tamanho_amostra, mean = media, sd = desvio_padrao)
}

# Função para calcular o intervalo de confiança de 95%
calculate_ci <- function(x) {
  n <- length(x)
  media <- mean(x)
  desvio_padrao <- sd(x)
  erro_padrao <- desvio_padrao / sqrt(n)
  margem_erro <- qt(0.975, df = n - 1) * erro_padrao
  limite_inferior <- media - margem_erro
  limite_superior <- media + margem_erro
  return(c(media, desvio_padrao, limite_inferior, limite_superior))
}

# Calculando média, desvio padrão e intervalo de confiança para cada amostra
resultados <- apply(amostras, 1, calculate_ci)

# Convertendo a matriz de resultados em um data frame
resultados_df <- data.frame(t(resultados))

# Nomeando as colunas
colnames(resultados_df) <- c("Media_Amostral", "Desvio_Padrao_Amostral", "Limite_Inferior_CI", "Limite_Superior_CI")

# Carregando a biblioteca para plotar gráficos
library(ggplot2)

# Medidas de posição
medidas_posicao <- summary(resultados_df$Media_Amostral)

# Criando um histograma
histograma <- ggplot(resultados_df, aes(x = Media_Amostral)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black", alpha = 0.7) +
  labs(x = "Média Amostral",
       y = "Frequência") +
  theme_minimal()
ggsave(filename = file.path(caminho_lucas, "histograma.pdf"), width = 158, height = 93, units = "mm")


histograma
# Exibindo medidas de posição e histograma
print("Medidas de Posição:")
print(medidas_posicao)

print("Histograma:")
print(histograma)

# Contando quantos intervalos de confiança contêm a média populacional
contagem_contem_media <- sum(resultados_df$Limite_Inferior_CI <= media & resultados_df$Limite_Superior_CI >= media)

print(paste("Número de intervalos de confiança que contêm a média populacional:", contagem_contem_media))

# Selecionando aleatoriamente 100 intervalos de confiança
amostra_aleatoria <- resultados_df[sample(nrow(resultados_df), 100), ]

# Criando um gráfico de barras para os intervalos de confiança
grafico_intervalos_confianca <- ggplot(amostra_aleatoria, aes(x = 1:nrow(amostra_aleatoria))) +
  geom_errorbar(aes(ymin = Limite_Inferior_CI, ymax = Limite_Superior_CI), width = 0.2, color = "blue") +
  geom_point(aes(y = Media_Amostral), color = "red", size = 2) +
  labs(title = "Intervalos de Confiança (100 amostras aleatórias)",
       x = "Amostras",
       y = "Média Amostral") +
  theme_minimal()
ggsave(filename = file.path(caminho_lucas, "intervalo_Confiança(100).pdf"), width = 158, height = 93, units = "mm")



# Exibindo o gráfico
print(grafico_intervalos_confianca)

# Criando um gráfico de barras para os intervalos de confiança
grafico_intervalos_confianca2 <- ggplot(amostra_aleatoria, aes(x = 1:nrow(amostra_aleatoria))) +
  geom_errorbar(aes(ymin = Limite_Inferior_CI, ymax = Limite_Superior_CI), width = 0.2, color = "blue") +
  geom_point(aes(y = Media_Amostral), color = "red", size = 2) +
  geom_hline(yintercept = media, linetype = "dashed", color = "green", size = 1) +  # Adicionando linha para a média populacional
  labs(title = "Intervalos de Confiança (100 amostras aleatórias)",
       x = "Amostras",
       y = "Média Amostral") +
  theme_minimal()
ggsave(filename = file.path(caminho_lucas, "Intervalo_Confianca_mediaV.pdf"), width = 158, height = 93, units = "mm")

# Exibindo o gráfico
print(grafico_intervalos_confianca2)

