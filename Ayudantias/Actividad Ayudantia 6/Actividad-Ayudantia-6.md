Actividad Ayudantia 6: Clusters Jerárquicos
================

# Importar Librerias

``` r
library(tidyverse)
```

    ## Warning in as.POSIXlt.POSIXct(Sys.time()): unable to identify current timezone 'H':
    ## please set environment variable 'TZ'

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.3     v purrr   0.3.4
    ## v tibble  3.1.0     v dplyr   1.0.5
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(cluster)
library(factoextra)
```

    ## Warning: package 'factoextra' was built under R version 4.0.5

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
library(janitor)
```

    ## Warning: package 'janitor' was built under R version 4.0.5

    ## 
    ## Attaching package: 'janitor'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library("ggdendro")
```

    ## Warning: package 'ggdendro' was built under R version 4.0.5

# Cargar Datos:

``` r
setwd("C:/Users/Felipe/Documents/GitHub/Entregas_mineria_de_datos/Ayudantias/Actividad Ayudantia 6")

library(readr)
data <- read_csv("C:/Users/Felipe/Documents/GitHub/Entregas_mineria_de_datos/Ayudantias/Actividad Ayudantia 6/Spotify_Songs.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   .default = col_double(),
    ##   track_id = col_character(),
    ##   track_name = col_character(),
    ##   track_artist = col_character(),
    ##   track_popularity = col_character(),
    ##   track_album_id = col_character(),
    ##   track_album_name = col_character(),
    ##   track_album_release_date = col_character(),
    ##   playlist_name = col_character(),
    ##   playlist_id = col_character(),
    ##   playlist_genre = col_character(),
    ##   playlist_subgenre = col_character()
    ## )
    ## i Use `spec()` for the full column specifications.

    ## Warning: 752 parsing failures.
    ## row              col           expected actual                                                                                                            file
    ##  89 track_name       delimiter or quote      P 'C:/Users/Felipe/Documents/GitHub/Entregas_mineria_de_datos/Ayudantias/Actividad Ayudantia 6/Spotify_Songs.csv'
    ##  89 track_name       delimiter or quote      ) 'C:/Users/Felipe/Documents/GitHub/Entregas_mineria_de_datos/Ayudantias/Actividad Ayudantia 6/Spotify_Songs.csv'
    ##  89 track_album_name delimiter or quote      P 'C:/Users/Felipe/Documents/GitHub/Entregas_mineria_de_datos/Ayudantias/Actividad Ayudantia 6/Spotify_Songs.csv'
    ##  89 track_album_name delimiter or quote      ) 'C:/Users/Felipe/Documents/GitHub/Entregas_mineria_de_datos/Ayudantias/Actividad Ayudantia 6/Spotify_Songs.csv'
    ## 164 track_name       delimiter or quote      T 'C:/Users/Felipe/Documents/GitHub/Entregas_mineria_de_datos/Ayudantias/Actividad Ayudantia 6/Spotify_Songs.csv'
    ## ... ................ .................. ...... ...............................................................................................................
    ## See problems(...) for more details.

``` r
summary(data)
```

    ##    track_id          track_name        track_artist       track_popularity  
    ##  Length:32833       Length:32833       Length:32833       Length:32833      
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##  track_album_id     track_album_name   track_album_release_date
    ##  Length:32833       Length:32833       Length:32833            
    ##  Class :character   Class :character   Class :character        
    ##  Mode  :character   Mode  :character   Mode  :character        
    ##                                                                
    ##                                                                
    ##                                                                
    ##                                                                
    ##  playlist_name      playlist_id        playlist_genre     playlist_subgenre 
    ##  Length:32833       Length:32833       Length:32833       Length:32833      
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##   danceability         energy             key             loudness      
    ##  Min.   : 0.0000   Min.   :-13.320   Min.   :-19.846   Min.   :-46.448  
    ##  1st Qu.: 0.5630   1st Qu.:  0.580   1st Qu.:  2.000   1st Qu.: -8.155  
    ##  Median : 0.6720   Median :  0.721   Median :  6.000   Median : -6.146  
    ##  Mean   : 0.6607   Mean   :  0.706   Mean   :  5.321   Mean   : -6.680  
    ##  3rd Qu.: 0.7610   3rd Qu.:  0.841   3rd Qu.:  9.000   3rd Qu.: -4.623  
    ##  Max.   :11.0000   Max.   : 11.000   Max.   : 11.000   Max.   :  1.275  
    ##                                                                         
    ##       mode         speechiness      acousticness    instrumentalness   
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000000  
    ##  1st Qu.:0.0000   1st Qu.:0.0410   1st Qu.:0.0148   1st Qu.:0.0000000  
    ##  Median :1.0000   Median :0.0625   Median :0.0796   Median :0.0000175  
    ##  Mean   :0.5634   Mean   :0.1072   Mean   :0.1749   Mean   :0.0859785  
    ##  3rd Qu.:1.0000   3rd Qu.:0.1320   3rd Qu.:0.2540   3rd Qu.:0.0053500  
    ##  Max.   :1.0000   Max.   :0.9180   Max.   :0.9940   Max.   :0.9940000  
    ##                                                                        
    ##     liveness           valence              tempo         duration_ms    
    ##  Min.   :  0.0000   Min.   :     0.00   Min.   :     0   Min.   :  4000  
    ##  1st Qu.:  0.0929   1st Qu.:     0.33   1st Qu.:   100   1st Qu.:187745  
    ##  Median :  0.1280   Median :     0.51   Median :   122   Median :216000  
    ##  Mean   :  0.3237   Mean   :   260.18   Mean   :  1025   Mean   :225765  
    ##  3rd Qu.:  0.2510   3rd Qu.:     0.70   3rd Qu.:   134   3rd Qu.:253573  
    ##  Max.   :178.0760   Max.   :242649.00   Max.   :384733   Max.   :517810  
    ##                                         NA's   :41       NA's   :164

``` r
head(data)
```

    ## # A tibble: 6 x 23
    ##   track_id    track_name          track_artist  track_popularity track_album_id 
    ##   <chr>       <chr>               <chr>         <chr>            <chr>          
    ## 1 6f807x0ima~ I Don't Care (with~ Ed Sheeran    66               2oCs0DGTsRO98G~
    ## 2 0r7CVbZTWZ~ Memories - Dillon ~ Maroon 5      67               63rPSO264uRjW1~
    ## 3 1z1Hg7Vb0A~ All the Time - Don~ Zara Larsson  70               1HoSmj2eLcsrR0~
    ## 4 75FpbthrwQ~ Call You Mine - Ke~ The Chainsmo~ 60               1nqYsOef1yKKuG~
    ## 5 1e8PAfcKUY~ Someone You Loved ~ Lewis Capaldi 69               7m7vv9wlQ4i0LF~
    ## 6 7fvUMiyapM~ Beautiful People (~ Ed Sheeran    67               2yiy9cd2QktrNv~
    ## # ... with 18 more variables: track_album_name <chr>,
    ## #   track_album_release_date <chr>, playlist_name <chr>, playlist_id <chr>,
    ## #   playlist_genre <chr>, playlist_subgenre <chr>, danceability <dbl>,
    ## #   energy <dbl>, key <dbl>, loudness <dbl>, mode <dbl>, speechiness <dbl>,
    ## #   acousticness <dbl>, instrumentalness <dbl>, liveness <dbl>, valence <dbl>,
    ## #   tempo <dbl>, duration_ms <dbl>

# Pre Procesamiento de los Datos

## Limpieza Datos:

-   Verificar la existencia de valores NA o faltantes

``` r
data[data == ""] <- NA

data %>%  summarise_all(funs(sum(is.na(.))))
```

    ## Warning: `funs()` was deprecated in dplyr 0.8.0.
    ## Please use a list of either functions or lambdas: 
    ## 
    ##   # Simple named list: 
    ##   list(mean = mean, median = median)
    ## 
    ##   # Auto named with `tibble::lst()`: 
    ##   tibble::lst(mean, median)
    ## 
    ##   # Using lambdas
    ##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))

    ## # A tibble: 1 x 23
    ##   track_id track_name track_artist track_popularity track_album_id
    ##      <int>      <int>        <int>            <int>          <int>
    ## 1        0          5            5                0              0
    ## # ... with 18 more variables: track_album_name <int>,
    ## #   track_album_release_date <int>, playlist_name <int>, playlist_id <int>,
    ## #   playlist_genre <int>, playlist_subgenre <int>, danceability <int>,
    ## #   energy <int>, key <int>, loudness <int>, mode <int>, speechiness <int>,
    ## #   acousticness <int>, instrumentalness <int>, liveness <int>, valence <int>,
    ## #   tempo <int>, duration_ms <int>

``` r
data_pre <- data %>%  filter(!(is.na(track_name)|is.na(track_artist)|is.na(track_album_name)|is.na(duration_ms)))

data_pre %>% summarise_all(funs(sum(is.na(.))))
```

    ## # A tibble: 1 x 23
    ##   track_id track_name track_artist track_popularity track_album_id
    ##      <int>      <int>        <int>            <int>          <int>
    ## 1        0          0            0                0              0
    ## # ... with 18 more variables: track_album_name <int>,
    ## #   track_album_release_date <int>, playlist_name <int>, playlist_id <int>,
    ## #   playlist_genre <int>, playlist_subgenre <int>, danceability <int>,
    ## #   energy <int>, key <int>, loudness <int>, mode <int>, speechiness <int>,
    ## #   acousticness <int>, instrumentalness <int>, liveness <int>, valence <int>,
    ## #   tempo <int>, duration_ms <int>

-   Segundo filtrar y remover datos duplicados

``` r
data_pre <- data_pre[!duplicated(data_pre$track_id),]
```

-   Tercero verificar la existencia de errores en los datos de las
    observaciones

``` r
data_pre$track_popularity <- as.numeric(as.character(data_pre$track_popularity))

data_pre <- data_pre %>%  filter(!(is.na(track_popularity)))

data_pre <- data_pre[!grepl("<U",data_pre$track_name),]
data_pre <- data_pre[!grepl("<U",data_pre$track_artist),]

data_pre %>% count(duplicated(data_pre$track_name))
```

    ## # A tibble: 2 x 2
    ##   `duplicated(data_pre$track_name)`     n
    ##   <lgl>                             <int>
    ## 1 FALSE                             23101
    ## 2 TRUE                               4846

``` r
data_pre %>% distinct(track_name, .keep_all = TRUE, )
```

    ## # A tibble: 23,101 x 23
    ##    track_id    track_name          track_artist track_popularity track_album_id 
    ##    <chr>       <chr>               <chr>                   <dbl> <chr>          
    ##  1 6f807x0ima~ "I Don't Care (wit~ Ed Sheeran                 66 2oCs0DGTsRO98G~
    ##  2 0r7CVbZTWZ~ "Memories - Dillon~ Maroon 5                   67 63rPSO264uRjW1~
    ##  3 1z1Hg7Vb0A~ "All the Time - Do~ Zara Larsson               70 1HoSmj2eLcsrR0~
    ##  4 75FpbthrwQ~ "Call You Mine - K~ The Chainsm~               60 1nqYsOef1yKKuG~
    ##  5 1e8PAfcKUY~ "Someone You Loved~ Lewis Capal~               69 7m7vv9wlQ4i0LF~
    ##  6 7fvUMiyapM~ "Beautiful People ~ Ed Sheeran                 67 2yiy9cd2QktrNv~
    ##  7 2OAylPUDDf~ "Never Really Over~ Katy Perry                 62 7INHYSeusaFlyr~
    ##  8 6b1RNvAcJj~ "Post Malone (feat~ Sam Feldt                  69 6703SRPsLkS4bP~
    ##  9 7bF6tCO3gF~ "Tough Love - Ti\x~ Avicii                     68 7CvAfGvq4RlIwE~
    ## 10 1IXGILkPm0~ "If I Can't Have Y~ Shawn Mendes               67 4QxzbfSsVryEQw~
    ## # ... with 23,091 more rows, and 18 more variables: track_album_name <chr>,
    ## #   track_album_release_date <chr>, playlist_name <chr>, playlist_id <chr>,
    ## #   playlist_genre <chr>, playlist_subgenre <chr>, danceability <dbl>,
    ## #   energy <dbl>, key <dbl>, loudness <dbl>, mode <dbl>, speechiness <dbl>,
    ## #   acousticness <dbl>, instrumentalness <dbl>, liveness <dbl>, valence <dbl>,
    ## #   tempo <dbl>, duration_ms <dbl>

``` r
data_pre$duplicate <- duplicated(data_pre[,c("track_name", "track_artist")])

data_dupli <- data_pre %>%  filter(data_pre$duplicate == TRUE) %>% arrange("track_name", "track_popularity", desc(track_popularity))

data_dupli <- data_dupli %>%  distinct(track_name, track_artist, .keep_all = TRUE)

data_pre <- data_pre[!(data_pre$duplicate == TRUE),]

data_pre <- rbind(data_pre, data_dupli)

data_pre$duplicate <- NULL
```

## Revisar Estructura Datos

``` r
data_pre$track_id <- as.character(data_pre$track_id)
data_pre$track_name <- as.character(data_pre$track_name)
data_pre$track_artist <- as.character(data_pre$track_artist)
data_pre$track_album_id <- as.character(data_pre$track_album_id)
data_pre$track_album_name <-  as.character(data_pre$track_album_name)
data_pre$playlist_name <- as.character(data_pre$playlist_name)
data_pre$playlist_id <- as.character(data_pre$playlist_id)
data_pre$playlist_genre <- as.character(data_pre$playlist_genre)
data_pre$playlist_subgenre <- as.character(data_pre$playlist_subgenre)

data_pre$danceability <- as.double(as.character(data_pre$danceability))
data_pre$energy <- as.double(as.character(data_pre$energy))
data_pre$key <- as.double(as.character(data_pre$key))
data_pre$loudness <- as.double(as.character(data_pre$loudness))
data_pre$mode <- as.double(as.character(data_pre$mode))
data_pre$speechiness <- as.double(as.character(data_pre$speechiness)) 
data_pre$acousticness <- as.double(as.character(data_pre$acousticness))
data_pre$instrumentalness <- as.double(as.character(data_pre$instrumentalness))
data_pre$liveness <- as.double(as.character(data_pre$liveness))
data_pre$valence <- as.double(as.character(data_pre$valence))
data_pre$tempo <- as.double(as.character(data_pre$tempo))
data_pre$duration_ms <- as.double(as.character(data_pre$duration_ms))

data_char <- c("track_id", "track_name", "track_artist", "track_album_id", "track_album_name", "playlist_name", "playlist_id", "playlist_genre", "playlist_subgenre")

data_dou <- c("track_popularity","danceability", "energy", "key", "loudness", "mode", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo", "duration_ms")

data_pre <- data_pre %>% filter(!(is.na(key)|is.na(danceability)))
```

## Separar Datos

``` r
datanum <- data_pre %>% select(data_dou)
```

    ## Note: Using an external vector in selections is ambiguous.
    ## i Use `all_of(data_dou)` instead of `data_dou` to silence this message.
    ## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    ## This message is displayed once per session.

``` r
datachar <- data_pre %>% select(data_char)
```

    ## Note: Using an external vector in selections is ambiguous.
    ## i Use `all_of(data_char)` instead of `data_char` to silence this message.
    ## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    ## This message is displayed once per session.

## Escalar Datos

``` r
data_sca <- sapply(datanum, scale)
```

# Procesamiento de los Datos

## Clustering Jerarquico

En esta parte destaco que utilicé 10.000 datos de la data total en “data
escalada” dado que es una muy grande cantidad de datos y algunas
funciones no se realizan por falta de espacio

``` r
data_sca <- data_sca[1:10000,]
```

-   Matriz de Distancias

``` r
d = dist(data_sca, method = "euclidean")

hist(d, main = "Histograma Distancia Euclideana")
```

![](Actividad-Ayudantia-6_files/figure-gfm/matriz%20distancia-1.png)<!-- -->

## Tipo 1: Clustering Aglomerativo

Utilizando la funcion de R base hclust, aplicamos hierarchical
clustering, a partir de la matriz de distancias d, y utilizamos el
criterio complete linkage

-   Complete Model

``` r
set.seed(369)

model_complete <- hclust(d, method = "complete")

summary(model_complete)
```

    ##             Length Class  Mode     
    ## merge       19998  -none- numeric  
    ## height       9999  -none- numeric  
    ## order       10000  -none- numeric  
    ## labels          0  -none- NULL     
    ## method          1  -none- character
    ## call            3  -none- call     
    ## dist.method     1  -none- character

-   Ward Model

``` r
set.seed(369)

model_ward <- hclust(d, method = "ward.D")

summary(model_ward)
```

    ##             Length Class  Mode     
    ## merge       19998  -none- numeric  
    ## height       9999  -none- numeric  
    ## order       10000  -none- numeric  
    ## labels          0  -none- NULL     
    ## method          1  -none- character
    ## call            3  -none- call     
    ## dist.method     1  -none- character

-   Comparacion de los coeficientes de aglomeracion para cada metodo

``` r
models <- c("complete", "ward")
names(models) <- c("complete", "ward")

agcoef <- function(x) {
  agnes(data_sca, method = x)$ac
}
```

Generamos un dendrograma para visualizar la jerarquia. La libreria
‘ggdendro’ permite hacer estos diagramas en una sintaxis equivalente a
ggplot.

``` r
ggdendrogram(model_complete, rotate = TRUE, theme_dendro = TRUE) 
```

![](Actividad-Ayudantia-6_files/figure-gfm/grafico%20dendrograma-1.png)<!-- -->

## Corte del arbol

Aqui tambien tuve que hacer un pequeño ajuste por truncar la data en una
etapa anterior.

Ojo que yo quiero 2 Clusters, por eso ocupé el k=2, de ocupar h=2 me da
una cantidad de clasters innecesarios.

``` r
groups <- cutree(model_complete, k = 2)
table(groups)
```

    ## groups
    ##    1    2 
    ## 9308  692

``` r
data_pre=data_pre[1:10000,]
datanum=datanum[1:10000,]

data_pre$clust <- as.factor(groups)
datanum$clust <- as.factor(groups)

fviz_cluster(list(data = data_sca, cluster = groups))
```

![](Actividad-Ayudantia-6_files/figure-gfm/corte%20arbol-1.png)<!-- -->

## Caracteristicas de los clusters encontrados

``` r
datanum$clust <- as.numeric(as.character(datanum$clust))

infoclusters <- aggregate(datanum, by=list(cluster=datanum$clust), mean)

infoclusters$clust <- NULL

infoclusters <- infoclusters %>% mutate(duration_min = infoclusters$duration_ms/60000)

infoclusters$duration_ms <- NULL

infoclusters
```

    ##   cluster track_popularity danceability    energy      key   loudness      mode
    ## 1       1         43.93500    0.6796015 0.6955121 5.322841  -6.490523 0.5634938
    ## 2       2         43.09104    0.5887392 0.3706310 5.813584 -11.662470 0.5303468
    ##   speechiness acousticness instrumentalness  liveness   valence   tempo
    ## 1  0.13774847    0.1578463       0.04676877 0.1880332 0.5111209 121.156
    ## 2  0.08670679    0.5693269       0.49442981 0.1401328 0.3527299 118.349
    ##   duration_min
    ## 1     3.640673
    ## 2     3.033023

## Filtro por clusters con mas datos

``` r
data_c1 <- data_pre %>% filter(data_pre$clust == 1)

data_c2 <- data_pre %>% filter(data_pre$clust == 2)
```

## Desde aqui en adelante solo se trabaja con c2 dado que necesito pocos datos para ocupar el cluster divisivo, consume muchos recursos de hardware ese.

``` r
data_c2$clust <- NULL

datanumc2 <- data_c2 %>% select(data_dou) %>%  scale() %>%  as_tibble()
```

Ahora a C2 le aplicaremos un clustering divisivo

## Tipo 2: Clustering Divisivo

``` r
modelo_div <- diana(datanumc2)

modelo_div$dc
```

    ## [1] 0.8134729

``` r
pltree(modelo_div, cex = 0.8, hang = -1.5, main = "Dendrogram of diana")
```

![](Actividad-Ayudantia-6_files/figure-gfm/clustering%20divisivo-1.png)<!-- -->

## Cantidad Clusters seleccionados es 2 dentro del cluster ya seleccionado por el metodo anterior

``` r
groupsc2 <- cutree(modelo_div, k = 2)

table(groupsc2)
```

    ## groupsc2
    ##   1   2 
    ## 309 383

``` r
data_c2$clust <- as.factor(groupsc2)

fviz_cluster(list(data = datanumc2, cluster = groupsc2))
```

![](Actividad-Ayudantia-6_files/figure-gfm/division%20arbol-1.png)<!-- -->

``` r
datanumc2$clust <- as.factor(groupsc2)
```

## Caracteristicas Clusters encontrados

``` r
datanumc2$clust <- as.numeric(as.character(datanumc2$clust))

infoclustersc2 <- aggregate(datanumc2, by=list(cluster=datanumc2$clust), mean)

infoclustersc2$clust <- NULL

infoclustersc2 <- infoclustersc2 %>% mutate(duration_min = infoclustersc2$duration_ms/60000)

infoclustersc2$duration_ms <- NULL

infoclustersc2
```

    ##   cluster track_popularity danceability     energy         key   loudness
    ## 1       1      -0.11494329   -0.5344447  0.5089431  0.07176158  0.5073345
    ## 2       2       0.09273493    0.4311838 -0.4106094 -0.05789642 -0.4093117
    ##          mode speechiness acousticness instrumentalness    liveness    valence
    ## 1  0.04615424  -0.3472960   -0.2374231       -0.8885104  0.03136064 -0.4281653
    ## 2 -0.03723671   0.2801945    0.1915502        0.7168400 -0.02530141  0.3454388
    ##        tempo  duration_min
    ## 1  0.4273586  1.299479e-05
    ## 2 -0.3447880 -1.048405e-05

Y asi es como utilicé un modelo para generar 2 grandes clusters y luego
a uno de esos clusters lo volví a dividir en otros 2 clusters pero
utilizando otro modelo.
