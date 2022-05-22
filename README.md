Construcción de normas YR
================

# Teoría de la construcción de normas:

## El problema de la construcción de normas:

La construcción de normas se basan en una muestra que pretende ser
representativa, no solo de una población, sino que también de múltiples
grupos, principalmente de edad o de grado. En la práctica esta tarea es
muy difícil de realizar, ya que contar con muestras para cada grupo de
referencia requiere tamaños muestrales grandes y representa un desafío
logístico importante. El cálculo de normas basadas en distintos
subgrupos está expuesto a resultados poco plausibles, como por ejemplo
que en un subgrupo frente a un mismo puntaje bruto estén asociados
percentiles muy distintos, debido a características de la muestra. Se
supondría en una prueba cognitiva, por ejemplo, que los estudiantes de
10 años tenderían a tener puntajes percentiles más bajos frente al mismo
puntaje bruto de una norma generada por un grupo de estudiantes de 8
años. Esto sin embargo, por características de la muestra podría no
ocurrir.

## Soluciones a este problema:

Es posible suponer que existe una habilidad no observada que explica,
junto con la variable de agrupación (ej. edad), los puntajes brutos. Lo
que se hace es modelar la relación entre puntajes brutos (variable
dependiente) a partir de la variable de agrupación (VI1) y los puntajes
normativos (VI2). La estructura de la relación teórica supuesta permite
utilizar toda la información disponible en los datos. La forma de
hacerlo es a través de la construcción de un plano, tal como ocurre con
la regresión múltiple de 2 variables.

## Supuestos del modelo:

1.- Existe un rasgo latente distribuido normalmente para cada nivel de
la variable explicativa (ej. Edad).

2.- Este rasgo latente explica los puntajes brutos observados. Es
posible experar un cierto puntaje bruto para un cierto nivel del rasgo
latente.

3.- Los puntajes normalizados de la distribución empírica de puntajes
brutos refleja a la variable latente.

4.- Existe una relación monotonamente creciente entre el rasgo latente y
el puntaje bruto observado.

5.- Existe continuidad entre el cambio en la variable explicativa y la
variable observada, la cual no necesariamente es monotonamente creciente
(ej. con la edad hay un crecimiento pero después hay un decrecimiento en
los puntajes de pruebas cognitivas).

## Descripción matemática:

Lo que se modela es el valor esperado bruto, a partir de un puntaje
norma (norm score) y una variable explicativa observada (ej. edad,
curso). Para ello, se utiliza como referencia la norma empírica
(puntajes normalizados) y la variable explicativa. Para ello, se ajustan
modelos de regresión usando polinomios de diverso exponente, buscando
como resultado la mejor aproximación a los datos.

# Construcción de normas Yellow Red:

``` r
yr = read_sav("YR_datos_normas.sav")
yr = yr[1:275,]
yr$Gender = unlabelled(yr$Gender)
yr$SES = unlabelled(yr$SES)
yr$Grade = unlabelled(yr$Grade)
```

Construir normas para cada edad es ineficiente, considerando que existe
una relación matemática evidente entre la edad y los puntajes brutos en
las distintas subpruebas:

``` r
yr %>% dplyr::select(Age, Arrows_Total_Score, Binding_Total_Score, Flies_Total_Score,
              Triads_Total_Score, CatDog_Modif) %>% 
  gather(key=prueba,value=puntaje, -Age) %>% 
  ggplot(aes(x=Age,y=puntaje)) + stat_smooth(span = 3, se=F) + geom_point(alpha=.5,size=1) + theme_bw() +
  facet_wrap(~prueba, scale = "free")
```

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning: Removed 2 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 2 rows containing missing values (geom_point).

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Adicionalmente, no existe un tamaño muestral suficiente para hacer
normas por edad:

``` r
yr %>% ggplot(aes(x=Age)) + geom_histogram(color="black", fill="lightblue", binwidth = 1) +
  theme_bw() + scale_x_continuous(breaks = seq(6,13,1))
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### Elaboración del modelo:

Se tomará el curso como variable de agrupación:

``` r
yr = yr %>% mutate(GradeF = factor(Grade,levels = c("Kinder","1st Grade",
                                         "2nd Grade","3rd Grade",
                                         "4th Grade", "5th Grade",
                                         "6th Grade")))
yr$GradeG = as.numeric(yr$GradeF)
```

Modelo: puntaje bruto como función del percentil y el grupo de edad.
Aquello permite la construcción de un plano con el que podemos luego
generar las normas.

``` r
resultados = cnorm(raw = yr$Arrows_Total_Score,group = yr$GradeG)
```

    ## Powers of location: k = 5
    ## Powers of age:      t = 3
    ## Multiple R2 between raw score and explanatory variable: R2 = 0.5476
    ## 
    ## Final solution: 5 terms
    ## R-Square Adj. = 0.971593
    ## Final regression model: raw ~ L1 + L2 + A1 + L1A2 + L2A3
    ## Regression function: raw ~ -41.95358018 + (1.483774707*L1) + (-0.008623622026*L2) + (7.295796909*A1) + (-0.01763273653*L1A2) + (1.574670815e-05*L2A3)
    ## Raw Score RMSE = 1.18644

    ## 
    ## Use 'printSubset(model)' to get detailed information on the different solutions, 'plotPercentiles(model) to display percentile plot, plotSubset(model)' to inspect model fit.

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Lo que está diciendo es que existe una función que permite estimar con
una alta presición el puntaje bruto de una persona si es que conocemos
su localización (en percentil) y su grupo de pertenencia.

Podemos utilizar los términos de la función para estimar los puntajes
observados a partir de los percentiles y el grupo de pertenencia, y el
error promedio será de 1.18644 puntos.

``` r
L = 99
A = 2

-41.95358018 + 1.483774707*L + -0.008623622026*L^2 + 
  7.295796909*A + -0.01763273653*L*A^2 + 0.00001574*L^2*A^3
```

    ## [1] 29.26317

.
