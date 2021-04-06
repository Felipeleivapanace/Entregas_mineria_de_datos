Entrega Tarea 1 Minería de Datos
================

En primer lugar, el proyecto consiste en determinar cual es la mejor
combinacion de ingredientes para generar el mejor pan. El procedimiento
pretende identificar cuales son los ingredientes mas repetidos en los
restorantes que tienen las mejores calificaciones. Luego, se pretende
utilizar un algoritmo de fuerza bruta para determionar cuales deben ser
los mas relevantes dentro de los ya desyacados ingredientes con las
mejores calificaciones.

## Incorporación de Base de datos

En primer lugar, se importa la base de datos asignada.

``` r
library(readr)
sanguchez <- read_delim("sanguchez.csv",  ";", escape_double = FALSE, trim_ws = TRUE)
```

## Reducción de dimensionalidad y Selección de Variables

El primer paso es eliminar las columnas que se utilizara, tales como el
comentario, la dirección y su URL. El nombre del local se mantiene
porque sirve como identificador en cada fila. El precio se mantiene por
ser la variable que permite filtrar datos en el siguente paso.

``` r
sanguchez$url = NULL
sanguchez$Direccion = NULL
sanguchez$texto = NULL
```

## Tratamieento y Filtro de datos

En esta etapa se pretende filtrar los valores que no son utiles tales
como los valores vacíos. Esto se realizará con la función “is.na” por lo
que se debe utilizar una variable numerica, razón por la cual se
utilizará la variable “notas”. La primera instreucción identificar los
valores en donde se encuentras datos vacios en notas y la segunda
instrucción los elimina mientras guarda los datos “limpios” en la
variable “sanguchez\_clean”.

Se identifico la persistente ausencia del valor NA en la fila 310
mediante visual analisis pero no se consideró relevante su eliminación.

``` r
notas_a_eliminar = which(is.na(sanguchez$nota)) 
sanguchez_clean = sanguchez[-c(notas_a_eliminar),] 
```

A continuación se procede a selecionar los datos relevantes, los cuales
son los ingredientes presentes en los restorantes que obtuvieron nota
superior a 5 por lo que se procede a incoprorar este filtro y guardar el
resultado en la variable “sanguchez\_usefull”.

``` r
library(quanteda)
sanguchez_usefull = sanguchez_clean[sanguchez_clean$nota >= 5,]
```

Con los datos filtrados, se pretende tratar la forma en la que los
ingredientes estan expuestos. La primera linea de código a continuación
transforma las mayusculas en minusculas para mantener a todos los
ingredientes en la misma forma mientras que la segunda linea de código
elimina los tildes (los cuales no son leidos correctamente por R Studio)
y modifica los caracteres en tipo Ascii para que sea visualmente mas
agradable trabaajr con ellos.

``` r
cant_ingr=char_tolower(sanguchez_usefull$Ingredientes) 
cant_ingr=iconv(cant_ingr, to="ASCII//TRANSLIT")   
```

## Limmpiar caracteres

Luego del filtrado, algunos datos presentaron problemas de syntaxis
especificos mientras que otros reiteraban el mismo error dentro de su
etiqueta por lo que se procede a limpiarlos. En primer lugar se
reemplazan las palabras “y”,“en”,“con”,“sobre” y “una” dado que se
encuentran dentro de varios ingredientes ya identificados. El ciclo for
se utiza debido a que al correr solo una vez la linea del código, no se
eliminan la totalidad de las palabras pero al correr la linea de codigo
las suficientes veces si se logra la correcta limpieza.

Luego se eliminan las palabras irrelevantes para los ingredientes.

``` r
library(stringr)

for (i in 1:56){
  cant_ingr= str_replace(cant_ingr," y "," ,")
  cant_ingr= str_replace(cant_ingr," en "," ,")
  cant_ingr= str_replace(cant_ingr," con "," ,")
  cant_ingr=str_replace(cant_ingr,"sobre",",")
  cant_ingr=str_replace(cant_ingr,"una",",")
}

for(i in 1:56){
  cant_ingr=str_replace(cant_ingr,"marraqueta","")
  cant_ingr=str_replace(cant_ingr,"1/2","")
  cant_ingr=str_replace(cant_ingr,"prieta","")
  cant_ingr=str_replace(cant_ingr,"50%","")
  cant_ingr=str_replace(cant_ingr,"un","")
  cant_ingr=str_replace(cant_ingr,"braseado","")
  cant_ingr=str_replace(cant_ingr,"desmenuzado","")
  cant_ingr=str_replace(cant_ingr,"montado","")
  cant_ingr=str_replace(cant_ingr,"exquisito","")
  cant_ingr=str_replace(cant_ingr,"especial","")
  cant_ingr=str_replace(cant_ingr,"aceit,s","")
  cant_ingr=str_replace(cant_ingr,"verdes","")
  cant_ingr=str_replace(cant_ingr,"base de","")
  cant_ingr=str_replace(cant_ingr,"toque","")
  cant_ingr=str_replace(cant_ingr,"hecha","")
  cant_ingr=str_replace(cant_ingr,"ahi","")
  cant_ingr=str_replace(cant_ingr,"mismo","")
  cant_ingr=str_replace(cant_ingr,"doble","")
  cant_ingr=str_replace(cant_ingr,"aderezo","")
  cant_ingr=str_replace(cant_ingr,"servido","")
}
```

## Generación de Matriz de ingredientes

Se utilizará la función “dfm”, la cual construye un documento de
atributos en formato de matriz. Para eso, identifica todos los espacios
vacios y comas y lo interpreta como un nuevo ingrediente.

Debido a que algunos ingredientes como “pure de papas” es un ingrediente
pero la función lo identificaría como tres diferentes, con la primera
linea de codigo uno los strings que se encuentren entre dos comas (es
decir, que sean solo un ingrediente pero con separación de espacios) y
reemplazo este espacio por un guion, con la finalidad de que solo sea un
string y la función lo identifique como solo un elemento.

Luego, se genera la matriz de ingredientes, en la cual cada columna
corresponde a un ingrediente y cada fila a un sanfwich de un restorante,
por lo cual la intersección entre filas y columnas indica con un valor 1
si aparece en la receta del restorante o con un 0 si no fue utilizada en
ese sandwich. Se eliminan tres columnas de esta matriz dado que la
función identifica como ingredientes el string “.”,“–” y "\_".

``` r
cant_ingr=cant_ingr= chartr(" ","_",cant_ingr)

ingredientes_finales=dfm(cant_ingr, remove=c(stopwords("es"),",",")","("))
ingredientes_finales=ingredientes_finales[,-8]        
ingredientes_finales=ingredientes_finales[,-9]        
ingredientes_finales=ingredientes_finales[,-139]   
```

## Identificación de ingrdientes relevantes

A partir de la matriz generada, se procede a crear la variable
“cantidad\_total”,la cual indica la suma de cada columna para de esta
manera identificar por cuantas recetas cada ingrediente fue utilizado.
Luego se procede a ordenar esa variable de mayor valor (es decir, los
ingredientes relevantes o parcitivos primero) y a menor valor.

Varios ingredientes presentes en la matriz solo destacan una vez en las
recetas identificadas. Estos ingredientes no se asumen relevantes para
obtener una buena nota sino mas bien un sello de cada restorante, por lo
cual no se eutilizarán.

En la variable “cantidad\_final” quedan los ingredientes pertenecientes
a las recetas con nota 5 y que tienen una activa participación en
diferentes recetas. Para garantizar un buen sandwich deben ser usados
ingredeientes pertenecientes a esta lista, siendo esta una respuesta
parcial al desafío dado que una receta podría ser incluir los elementos
con mayor participación en las recetas.

``` r
cantidad_total= colSums(ingredientes_finales)                   
cantidad_total=cantidad_total[order(cantidad_total, decreasing = TRUE)]

cantidad_Final=cantidad_total[unname(cantidad_total) > 1]      
cantidad_Final
```

    ##                   _tomate                    _palta                  _lechuga 
    ##                        11                         6                         6 
    ##           _cebolla_morada               _pepinillos                 _mayonesa 
    ##                         4                         4                         4 
    ##                   _tocino            _queso_cheddar                  _tomate_ 
    ##                         4                         4                         3 
    ##             miel_de_maple                   _rucula      cebolla_caramelizada 
    ##                         3                         3                         3 
    ##               pan_brioche          _queso_americano         _tocino_glaseado_ 
    ##                         2                         2                         2 
    ##                  _rucula_           _tomates_asados               sopaipillas 
    ##                         2                         2                         2 
    ##                   mechada           mayonesa_casera          _mayonesa_casera 
    ##                         2                         2                         2 
    ##          _cebolla_morada_              _huevo_frito             carne_mechada 
    ##                         2                         2                         2 
    ##                _aji_verde                 _porotos_     _cebolla_caramelizada 
    ##                         2                         2                         2 
    ##           _queso_de_cabra          _salsa_de_tomate                  _tocino_ 
    ##                         2                         2                         2 
    ##                    _queso               queso_crema         _queso_mozzarella 
    ##                         2                         2                         2 
    ## _aros_de_cebolla_apanados          _tocino_crocante           _mayonesa_spicy 
    ##                         2                         2                         2 
    ##                   lechuga                     palta             _huevo_frito_ 
    ##                         2                         2                         2 
    ##        _aji_cherry_pepper     _mermelada_de_cebolla 
    ##                         2                         2

## Utilización de algoritmo Brutal Force

Dado que se cuenta con una lista de por lo menos 50 ingredientes con los
cuales se puede crear un sandwich con calificación maxima, se procede a
utilizar el algoritmo Brutal Force para estudiar las cominaciones que
estos ingredientes pueden lograr y obtener la mejor. A forma de
repsuesta se buscaran 5 ingredientes pero este valor puede ser
modificado en la estructura del codigo.

En primer lugar se procede a crear una matriz para ingredarla al
algoritmo. La matriz “ingredientes\_finales2” corresponde a un
data.frame que surge del Form Class DFM “ingredientes\_finales”, el cual
contiene la lista total de los ingredientes utilizados en todas las
recetas con notas 5. Se debe eliminar la primera columna dado que al
utilizar la funcion “convert” se agrega una columna de ID y ademas, se
debe eliminar a todos los ingredientes que solo participen en una
receta, dejando asi los ingredientes que tengan reiteración en por lo
menos 2 recetas.

De esta manera, la matriz “Filtro\_de\_ingredientes” contiene la
cantidad final de ingredientes identificados en la etapa anterior pero
esta vez dentro de un data.drame y no dentro de un strings de
caracteres.

``` r
ingredientes_finales2=convert(ingredientes_finales, to="data.frame") 
ingredientes_finales2=ingredientes_finales2[,-1]                      
Filtro_de_ingredientes=ingredientes_finales2[colSums(ingredientes_finales2)>1]   
```

Luego, se utiliza el algoritmo de fuerza bruta sobre esta matriz, en
donde nuevamente se destaca que se buscan los 5 ingredientes mas
relevantes dentro de este data.frame pero de ser necesaria la busqueda
de otra cantidad de ingredientes se debe cambiar el valor dentro de la
linea de codigo.

``` r
library(stuart) 

#resultadoFB <- bruteforce(Filtro_de_ingredientes,list(ra = names(Filtro_de_ingredientes)),5, cores = NULL)  #El 5 son los ingredientes que quiero
#summary(resultadoFB)
```

De esta manera, el resultado del algoritmo al buscar 5 ingredientes es
“pan\_brioche”, “sopaipillas”, “mechada”, “mayonesa\_casera” y
“carne\_mechada”. Siendo estos ingredientes perteneicentes a recetas con
nota 5 y con alta participación en estas, se garantiza una buena
calificación con la receta, logrando el objetivo del desafio.
