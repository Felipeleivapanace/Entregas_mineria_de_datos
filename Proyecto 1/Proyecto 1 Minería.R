

library(readr)
sanguchez <- read_delim("sanguchez.csv",  ";", escape_double = FALSE, trim_ws = TRUE)

sanguchez$url = NULL
sanguchez$Direccion = NULL
sanguchez$texto = NULL

notas_a_eliminar = which(is.na(sanguchez$nota)) 
sanguchez_clean = sanguchez[-c(notas_a_eliminar),] 

library(quanteda)
sanguchez_usefull = sanguchez_clean[sanguchez_clean$nota >= 5,]

cant_ingr=char_tolower(sanguchez_usefull$Ingredientes) 
cant_ingr=iconv(cant_ingr, to="ASCII//TRANSLIT")      
cant_ingr

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
cant_ingr

cant_ingr=cant_ingr= chartr(" ","_",cant_ingr)
cant_ingr
ingredientes_finales=dfm(cant_ingr, remove=c(stopwords("es"),",",")","("))
ingredientes_finales=ingredientes_finales[,-8]        #Elimina la columna que se genera del caracter punto
ingredientes_finales=ingredientes_finales[,-9]        #Elimina la columna que se genera del caracter guin despues de eliminar la del punto
ingredientes_finales=ingredientes_finales[,-139]      #Luego de eliminar las 2 anteriores, en el 139 queda esta columna de dos guines bajo. 

head(ingredientes_finales[,135:145])

cantidad_total= colSums(ingredientes_finales)                   
cantidad_total=cantidad_total[order(cantidad_total, decreasing = TRUE)]
cantidad_Final=cantidad_total[unname(cantidad_total) > 1]       
cantidad_Final

#Utilización del algoritmo Brutal Force

ingredientes_finales2=convert(ingredientes_finales, to="data.frame") #Esta matriz crea 266 variables, tengo que sacar el ID
ingredientes_finales2=ingredientes_finales2[,-1]                      #Saco la columna de ID

Filtro_de_ingredientes=ingredientes_finales2[colSums(ingredientes_finales2)>1]    #Saco todas las columnas en la que los ingredientes sumen solo 1 (se ocuparon solo una vez, no relevantes)

#Checkeo de etapa, los nombres de las dos cosas de abajo deben ser los mismos
names(Filtro_de_ingredientes)
cantidad_Final
##Fin checkeo

library(stuart) 
resultadoFB <- bruteforce(Filtro_de_ingredientes,list(ra = names(Filtro_de_ingredientes)),5, cores = NULL)  #El 5 son los ingredientes que quiero
summary(resultadoFB)
