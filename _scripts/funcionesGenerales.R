# Archivo contiene funciones generales para análisis o graficar datos
# Cada función se describe en detalle al definirla

library(lattice)
library(gdata, verbose=FALSE, quietly=TRUE)


# Función para graficar datos en un histograma trellis
# El modelo debe estar definido dentro de with, para que tenga referencia al
# data.frame, p.e. with(datos, casos~week|year) grafica el conteo de casos,
# por semana y año. # Los títulos de ejes se dan en c('tituloY', 'tituloX')
#
epiTimeHist <- function(modelo,
                        titulosEjes=c('Casos','Año y semana epidemiológica'))
{
	histogram	(	modelo,
				type='count',
				ylab=as.expression(bquote(bold(titulosEjes[1]))),
				xlab=as.expression(bquote(bold(titulosEjes[2]))),
				col='gray', strip=strip.custom(bg='white'),
				scales=list(
						x=list(	alternating=1,
							at=seq(1,52,4)
					)
				)
			)
}



# Lista las variables de .datos y su tipo 
tipos <- function(.datos)
{
	require(gdata)

	.nombres <- names(.datos)
	.largo <- length(.nombres)

	.listadoTipos <-  matrix(
                  rep(NA, times=.largo), ncol=1, dimnames=list(.nombres,'Tipo')
                    )

	for(.i in 1:.largo)
	{
		.clases <- is.what(.datos[1,.i])
		.clase <- .clases[
      match(
        c('is.factor', 'is.integer', 'is.double',
          'is.character', 'is.logical'), .clases
        )[!is.na(match(c('is.factor', 'is.integer', 'is.double',
                         'is.character', 'is.logical'), .clases))]
    ]
		.listadoTipos[.i,] <- paste(.clase, collapse=' ')
	}
	.listadoTipos
}




# Muestra el 'table' para cada variable que incluye
# '.contieneEnNombre' en el nombre.
# Considerar que es útil para factores, con pocos niveles.
#
tablaVariables <- function(.contieneEnNombre = 'viralP', .datos)
{
	.nombres <- names(.datos)
	.columnas <- grep(.contieneEnNombre,.nombres, value=TRUE)

	.resumen <- table(as.vector(as.matrix(.datos[.columnas])), useNA='always')
	.valores <- names(.resumen)
	.nValores <- length(.valores)
	.valores <- c(.valores[-.nValores],'NA')

	.tabla <- matrix(rep(NA,times=.nValores), ncol=.nValores,
                   dimnames=list(c(),.valores))
	for(.variable in .columnas)
	{
		.nuevaLinea <- matrix(rep(NA,times=.nValores), ncol=.nValores,
                          dimnames=list(.variable,c()))
		.tmp <- table(.datos[.variable], useNA='always')
		.orden <- match(names(.tmp),.valores)
		.nuevaLinea[.orden[!is.na(.orden)]] <- as.integer(
                                              .tmp[names(.tmp)[!is.na(.orden)]])
		if (sum(is.na(names(.tmp)))>0) .nuevaLinea[.nValores] <- as.integer(
                                                            .tmp[length(.tmp)])
		.tabla <- rbind(.tabla, .nuevaLinea)
	}
	(.tabla <- .tabla[-1,])
}



# Muestra la población para el municipio (código), el grupo de edad y año.
# Los datos son obtenidos de las proyecciones de población del INE (2002),
# para los años 2006-2010. La población de 2011 se calculó a partir de la tasa
# de crecimiento anual de cada municipio.
# Los datos se encuentran en la variable proyPopulation, del workspace
# proyeccionesINEPoblación2006-2011.RData, que debe haber sido cargado.
# Hay datos disponibles para los años 2006-2011, y los grupos de edad
# 0-4, 5-19, 10-14, ... , 60-64, 65+
poblacion <- function(.municipio, .ageGroup='all', .year, .type='sum')
{
  #.municipio <- as.factor(.municipio)
  #.ageGroup <- as.factor(.ageGroup)
  #.year <- as.character(.year)
  
#   .levelsMunicipio <- as.character(c(601, 602, 603, 604, 605, 
#                                      606, 610, 612, 613, 614))
#   .levelsAgeGroup <- as.character(
#                         c('0-4', '10-14', '15-19', '20-24', 
#                           '25-29', '30-34', '35-39', '40-44', 
#                           '45-49', '5-9', '50-54', '55-59', 
#                           '60-64', '65+'))
#   .levelsYear <- as.character(2006:2011)
#   
#   # Validar datos según la información disponible en proyPopulation 
#   .municipiosInvalidos <- as.character(.municipio)!= .levelsMunicipio
#   .gruposInvalidos <- as.character(.ageGroup) != .levelsAgeGroup
#   .yearInvalidos <- as.character(.year) != .levelsYear
#   
#   if(sum(.municipiosInvalidos, .gruposInvalidos, .yearInvalidos, na.rm=TRUE)>0){
#     .municipio <- .municipio[-.municipiosInvalidos]
#     .ageGroup <- .ageGroup[-.gruposInvalidos]
#     .year <- .year[-.yearInvalidos]
#     warning(
#       'Revisar las categorías utilizadas en \n',
#       quote(.municipio), ' ', quote(.ageGroup), ' ', quote(.year), '...'
#     )
#   }
#   
#   levels(.municipio) <- .levelsMunicipio
#   levels(.ageGroup) <- .levelsAgeGroup
#   levels(.year) <- .levelsYear
#   
  .numMunicipios <- length(.municipio)
  .numGrupos <- length(.ageGroup)
  
  # Ajusta el vector .municipios para que cada municipio
  # tenga una entrada por cada grupo de edad,
  # y el vector .ageGroup para que cada grupo de edad
  # tenga una entrada por cada municipio
  if(.numGrupos>1) .municipio <- rep(.municipio, each=.numGrupos)
  if(.numMunicipios>1) .ageGroup <- rep(.ageGroup, times=.numMunicipios)
  
  if(length(.ageGroup)==1){
  	if(.ageGroup!='all'){
  		.cumpleCriterios <- proyPopulation$IDMunicipio==.municipio &
    	                  	proyPopulation$AgeGroup==.ageGroup
  	}else{.cumpleCriterios <- proyPopulation$IDMunicipio==.municipio}
  } else {
  	.cumpleCriterios <- proyPopulation$IDMunicipio==.municipio &
    	                  proyPopulation$AgeGroup==.ageGroup
  }
    
  switch(.type,
  	sum = 
  	 	{	.pop<-sum(proyPopulation[.cumpleCriterios, paste('pa', .year, sep='')])
  	 		names(.pop)<-'Suma'
  	 	},
  	list =
  		{	.pop<-proyPopulation[.cumpleCriterios, paste('pa', .year, sep='')]
  			names(.pop)<-paste(.year, .municipio, .ageGroup, sep=':')
  		}
  )
  
  .pop
}