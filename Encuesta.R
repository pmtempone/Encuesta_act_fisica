library(FactoMineR)
library(questionr)
library(amap) #crear la matriz de burt
library(profileR)
library(foreign)
library(psych)
library(knitr)
library(xtable)
suppressPackageStartupMessages(library(dendextend))
suppressPackageStartupMessages(library(dendextendRcpp))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggfortify))
library(corrplot)
#library(Stuff)
suppressPackageStartupMessages(library(dplyr))
library(broom)
library(EnQuireR)



encuesta <- `ENCUESTA...ACTIVIDAD.FISICA.(respuestas)...Respuestas.de.formulario.1`
remove(`ENCUESTA...ACTIVIDAD.FISICA.(respuestas)...Respuestas.de.formulario.1`)

colnames(encuesta) <- c("fecha","edad","frecuencia","motivo","efectivo_ejercicio","factor_econ","monto")

encuesta$motivo[encuesta$motivo=='Razones econÃ³micas'] <- "Razones economicas"
encuesta$motivo[encuesta$motivo=='Razones fÃsicas'] <- "Razones Fisicas"


encuesta$frecuencia[encuesta$frecuencia=='4 o mÃ¡s'] <- "4 o mas"

encuesta$monto[encuesta$monto=='MÃ¡s de $ 800'] <- "Mas de $ 800"

encuesta$edad <- factor(encuesta$edad)
encuesta$frecuencia <- factor(encuesta$frecuencia)
encuesta$motivo <- factor(encuesta$motivo)
encuesta$efectivo_ejercicio <- factor(encuesta$efectivo_ejercicio)
encuesta$factor_econ <- factor(encuesta$factor_econ)
encuesta$monto <- factor(encuesta$monto)
otro <- as.data.frame(which(summary(encuesta$motivo)<3))

encuesta$motivo <- as.character(encuesta$motivo)
encuesta$motivo[encuesta$motivo %in% rownames(otro)] <- "Otro"
encuesta$motivo <- factor(encuesta$motivo)

encuesta <- encuesta[encuesta$edad!='1-17',]


rownames(otro)

summary(encuesta)
encuesta$fecha <- NULL

encuesta_matrix <- burt(encuesta)

res.ca.rows = CA(encuesta_matrix, invisible="col")
res.ca.col = CA(encuesta_matrix, invisible="row") 

factor_ana <- MCA(encuesta)
factor_ana$eig

dimdesc(factor_ana)

summary(encuesta)

plotellipses(factor_ana)

#clustering


d2 = dist(encuesta_matrix,method = "euclidean")
encuesta.clust.sin = as.dendrogram(hclust(d2, method = "single")) %>% set("branches_lwd", 2)
encuesta.clust.com = as.dendrogram(hclust(d2, method = "complete")) %>% set("branches_lwd", 2)
encuesta.clust.avg = as.dendrogram(hclust(d2, method = "average")) %>% set("branches_lwd", 2)
encuesta.clust.ward = as.dendrogram(hclust(d2, method = "ward.D")) %>% set("branches_lwd", 2)
encuesta.dend = dendlist("Cercano" = encuesta.clust.sin, "Lejano" = encuesta.clust.com, "Promedio" = encuesta.clust.avg,"Ward"=encuesta.clust.ward)

corrplot(cor.dendlist(encuesta.dend), "pie", "lower")

plot(encuesta.clust.sin %>% set("branches_k_color", k=3) %>% set("branches_lwd", 2), main = "Cercano")

plot(encuesta$monto,encuesta$edad)
plot(encuesta$monto,encuesta$edad,col=encuesta$edad)


#analisis encuesta

ENdensity(factor_ana)
p_inertia(encuesta)

clustergente <- ENMCA(encuesta)
res.catdes <- catdes(encuesta, num.var=6)
res.catdes


res.enmca  <- ENMCA(encuesta,report=TRUE)
res.enmca
