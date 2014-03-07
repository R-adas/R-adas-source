# Encontra Rmds que ainda não viraram posts e faz os mds deles.

# Carrega função que parseia e transforma Rmd em md.
source("newpostKnit.r")

# Lista de todos os Rmds

Rmds <- list.files("./Rmd")
mds <- list.files("./md")

# Rmds <- "simulacaoComparacaoSelecaoDeModelo.Rmd"
# mds <- ""

# Lista dos nomes dos Rmds e dos mds sem a extensão .Rmd para compará-los e filtrar os Rmds que deverão ser parseados.
RmdRaiz  <- basename(tools::file_path_sans_ext(Rmds))
mdRaiz  <- basename(tools::file_path_sans_ext(mds))

# Filtro
RmdRaiz <- RmdRaiz[!RmdRaiz%in%mdRaiz]

sapply(RmdRaiz, function(raiz) {
  fIn <- paste0("./Rmd/", raiz, ".Rmd")
  fOut <- paste0("./md/", raiz, ".md")
  markdEngine <- "pandoc"
  siteGen <- "nanoc"
  newpostKnit(fIn, fOut, markdEngine, siteGen)
  return(invisible("OK"))
})


writeUtf8 <- function(arq, extensao) {
    arqOrig <- paste0(arq, extensao)
    arqTmp <- paste0(arq,"Tmp",extensao)
    file.copy(arqOrig, arqTmp)
    writeBin(iconv(readChar(arqTmp, 10^8), from = "latin1", to = "UTF8"), arqOrig)
    file.remove(arqTmp)
  }

# converte tudo para utf-8
sapply(paste0(getwd(), "/md/", RmdRaiz), writeUtf8, ".md")
