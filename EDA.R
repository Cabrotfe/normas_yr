
pacman::p_load(tidyverse, haven, cNORM,gamlss,gamlss.dist,labelled)


# Apertura datos ----------------------------------------------------------

yr = read_sav("YR_datos_normas.sav")
yr = yr[1:275,]

yr$Gender = unlabelled(yr$Gender)
yr$SES = unlabelled(yr$SES)
yr$Grade = unlabelled(yr$Grade)





# gráficos edad y pruebas:

yr %>% glimpse()

yr %>% dplyr::select(Age, Arrows_Total_Score, Binding_Total_Score, Flies_Total_Score,
              Triads_Total_Score, CatDog_Modif) %>% 
  gather(key=prueba,value=puntaje, -Age) %>% 
  ggplot(aes(x=Age,y=puntaje)) + stat_smooth(span = 3, se=F) + geom_point(alpha=.5,size=1) + theme_bw() +
  facet_wrap(~prueba, scale = "free")




yr %>% ggplot(aes(x=Age)) + geom_histogram(color="black", fill="lightblue", binwidth = 1) +
  theme_bw() + scale_x_continuous(breaks = seq(6,13,1))



# cNORM  ------------------------------------------------------------------


# Teoría: cuando generamos normas estamos poniendo en relación una variable independiente
# observada (ej. edad) otra independiente no observada (percentil) y otra dependiente observada
# (puntaje bruto). En este modelo, podemos esperar un puntaje en una prueba dependiendo de la
# posición que la persona ocupe en la población (percentil) junto con su variable de agrupación
# edad

#



# Pasos:

# 1.- Determinar la forma de calcular normas al interior de una variable continua (ej. construir grupos de edad)
# 2.- Determinar los percentiles (emíricamente) y calcular su puntaje en una normal (Z, T, CI etc)
# 3.- 



# Vamos a utilizar cursos para las normas:
## Elaboración de grupos:



yr = yr %>% mutate(GradeF = factor(Grade,levels = c("Kinder","1st Grade",
                                         "2nd Grade","3rd Grade",
                                         "4th Grade", "5th Grade",
                                         "6th Grade")))
yr$GradeG = as.numeric(yr$GradeF)


## Elaboración de normas tradicionales:

norm_arrows <- rankByGroup(yr,
                        group = "GradeG",
                        raw = "Arrows_Total_Score",
                        scale = c(0,1),
                        method = 4)




# Ver si los puntajes escala son en base a la inversa de la normal o son puntajes Z:

norm_arrows = norm_arrows %>% mutate(gz_score = (raw-m)/sd)



norm_arrows %>% ggplot(aes(x=gz_score, y = normValue)) + geom_point() + theme_bw() +
  facet_wrap(~group, scales = "free")


powers_arrows = computePowers(norm_arrows)


bestModel(powers_arrows)

resultados = cnorm(raw = yr$Arrows_Total_Score,group = yr$GradeG)

resultados
