
## ------------------------------------------------------------------------
library(MASS)
library(xtable)
library(bestglm)
library(ggplot2)


## ------------------------------------------------------------------------
N <- 300
p <- 30 # supondo 31 com o intercepto
pairwiseCor <- .85 # correla��o dois a dois

sdX <- matrix(pairwiseCor, p, p)
diag(sdX) <- 1

(mu <- 1:p) # vetor de m�dias qualquer

sdErro <- sqrt(6.25)

BsNaoZeros <- 10 # N�mero de Betas diferentes de zero  (menor ou igual a p)


## ------------------------------------------------------------------------
X <- mvrnorm(n = N, mu = mu, Sigma = sdX)

# correla��es
table(round(cor(X), 2))

X <- cbind(1, X)
dim(X)



## ------------------------------------------------------------------------
epsilon <- rnorm(N, 0, sd = sdErro)


## ------------------------------------------------------------------------
# Betas
B <- c(rnorm(BsNaoZeros, 0, 4), rep(0, p - BsNaoZeros))[sample(p)]
B <- c(0, B) # intercepto igual a zero


## ----results='asis'------------------------------------------------------
params_verdadeiros <- data.frame(Param = (sprintf("$\\beta_{%s}$",0:p)), Valor = round(B, 2))
params_verdadeiros <- params_verdadeiros[ params_verdadeiros$Valor != 0, ]

print(
  xtable(params_verdadeiros), 
  type="html",
  include.rownames=FALSE
)


## ------------------------------------------------------------------------
Y <- X%*%B + epsilon


## ------------------------------------------------------------------------
geraY <- function(N = 300,
                  p = 30,
                  mu = 1:p,
                  pairwiseCor = .85,
                  sdX = 1,
                  sdErro = 2.5,
                  BsNaoZeros = p%/%3,
                  sdB = 4,
                  intercepto = 0) {
  Sigma <- sdX * matrix(pairwiseCor, p, p) + diag(p) * (1 - pairwiseCor)
  X <- mvrnorm(n = N, mu = mu, Sigma = Sigma)
  epsilon <- rnorm(N, 0, sd = sdErro)
  B <- c(rnorm(BsNaoZeros, 0, sdB), rep(0, p - BsNaoZeros))[sample(p)]
  B <- c(intercepto, B)
  Y <- cbind(1, X)%*%B + epsilon
  return(invisible(list(X = X, 
                        epsilon = epsilon, 
                        B = B, 
                        Y = Y)))
}


## ------------------------------------------------------------------------
set.seed(1)


## ------------------------------------------------------------------------
n_simulacoes <- 50
simulacoes <- lapply(1:n_simulacoes, function(x) geraY())


## ------------------------------------------------------------------------
# Fun��o que retorna um data.frame com dimens�o (p+1)x(p), com a coluna 'k' contendo os coeficientes ajustados do melhor modelo de tamanho 'k'. As linhas s�o definidas pelos par�metros. Os melhores modelos s�o selecionados pelo m�todo 'method'. O default � "exhaustive" (best subset).
beta_dos_melhores_de_tam_k <- function(simulacao, method = "exhaustive") {
  # parametros
  p <- ncol(simulacao$X)
  Xy <- with(simulacao, data.frame(X = X, y = Y))
  selecionados <- regsubsets(y ~ ., data = Xy, nvmax=(p+1), method = method)
  selecionados_bool <- summary(selecionados)
  # faz ajuste para cada um dos melhores modelos de tamanho k e guarda o vetor de coeficientes
  vetores_de_Bchapeu <- data.frame(sapply(1:p, function(id) {
    Bchapeu <- rep(0, p+1)
    Bchapeu[selecionados_bool$which[id, ]] <- coef(selecionados, id)
    return(Bchapeu)
  }))
  return(invisible(vetores_de_Bchapeu))
}

# Fun��o que calcula o desvio do Bchapeu em rela��o ao verdadeiro valor B. 
desvio_do_param_verdadeiro <- function(Bchapeu, B) {
  sqrt(mean((Bchapeu - B)^2))
}

# Fun��o que devolve o desvio Bchapeu de B para cada tamanho k de uma simula��o.
desvios_de_B <- function(simulacao, B, method) {
  desvios <- sapply(beta_dos_melhores_de_tam_k(simulacao, method), desvio_do_param_verdadeiro, B = B)
  return(desvios)
}


## ------------------------------------------------------------------------
metodos <- c("exhaustive", "forward", "backward")

desvio_medio <- lapply(metodos, function(metodo) {
  cat(metodo)
  tempo_inicial <- Sys.time()
  desvio <- sapply(simulacoes, desvios_de_B, B = B, method = metodo)
  desvio_medio <- rowMeans(desvio)
  p <- length(desvio_medio)
  desvio_medio <- data.frame(desvio_medio = desvio_medio/max(desvio_medio),
                              tamanho = 1:p,
                              method = metodo)
  duracao <- Sys.time() - tempo_inicial
  cat(paste0(" OK (",round(as.numeric(duracao),0)," ",attr(duracao,"units"),")\n"))
  return(desvio_medio)
})

desvio_medio <- do.call(rbind, desvio_medio)


## ----desvioMedioComparacao-----------------------------------------------
ggplot(desvio_medio, aes(x = tamanho, y = desvio_medio, colour = method)) +
  geom_point() +
  geom_line() +
  labs(x = "Tamanho", 
       y = expression(paste(plain(E),"||",hat(beta),"(k) - ",beta,"||")^2),
       colour = "M�todo") +
  theme_bw()


