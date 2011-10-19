#*******************************************************************************
#* Script del análisis del sistema de vigilancia ViCo:                         *
#*                                                                             *
#* Análisis de la Enfermedad tipo influenza en Santa Rosa, Guatemala,          *
#* sistema de Vigilancia Integrada Comunitaria (ViCo), 2009 - 2011             *
#*                                                                             *
#* Como producto de Oscar de León para el nivel intermedio del FETP            *
#* Autor: Oscar F. de León                                                     *
#* Fecha: Junio-Julio 2011                                                     *
#*******************************************************************************
#* Si se cuenta con el archivo 25072011VicoBasicaResp.Rdata o se tiene acceso  *
#* a la tabla ViCo.Clinicos.Basica_Respira_R en la base de datos ViCo del      *
#* servidor FSX-GT3 del CES-UVG, se puede copiar y pegar este script en R para *
#* replicar el análisis realizado. En el análisis original se incluye datos    *
#* actualizados hasta el 2011-07-25.                                           *
#*******************************************************************************


# Carga paquetes o código utilizados en el script.
library(lattice)  # 'lattice', para gráficas trellis
library(tools)  # Para verificar MD5 sums de los archivos adjuntos
library(ggplot2, quietly = TRUE) # Para gráficas ggplot
library(car) # Para utilizar la función recode (recodificar una variable)
source('_scripts//funcionesGenerales.R') # Carga funciones adicionales


# Para propósitos de réplica del análisis, a continuación se muestra 
# la versión de R y los paquetes usados. Además, se muestra el md5 sum
# para cada archivo externo usado (para verificar que son los mismos).
# Verificar con los datos en el anexo  'Salida de R'

# Muestra la versión de R usada
R.version

# Muestra la versión de los paquetes cargados
cargados <- grep('package',search(), value=TRUE)
cargados <- substr(cargados, 9, nchar(cargados))
vPaquetes <- data.frame(packageDescription(cargados[1])[c('Package', 'Version')])
for(.p in cargados[-1]) {
  a <- .p
  vPaquetes <- rbind(
    vPaquetes,
    data.frame(packageDescription(.p)[c('Package', 'Version')]))
}
vPaquetes
rm(vPaquetes, cargados, .p)

# Revisa si los archivos de datos y scrips son los mismos
# que los usados para el análisis de datos original.
# Los archivos listados en 'archivos' deben estar
# en el mismo directorio que este script.
# Verificar MD5 sums con los datos en el anexo  'Salida de R'
archivos <- c(
  '20110725VicoBasicaResp.Rdata',   # Archivo con los datos de ViCo
  'proyeccionesINEPoblación2006-2011.RData',  # Archivo con datos de población
  '20110725 análisis.R',            # Archivo con este script
  '_scripts//funcionesGenerales.R'  # Archivo con funciones adicionales
)
data.frame(md5sum(archivos))
rm(archivos) # Borrar el objeto que ya no se va a usar


# Carga el workspace con los datos de la tabla VicoBasicaRespira,
# con datos hasta el 20110725 (en el dataframe resp).
# El archivo debe estar en la misma carpeta que este script
load('20110725VicoBasicaResp.Rdata')
dim(resp)


# Carga el workspace con datos de proyecciones de población por grupo de edad
# del INE, 2006-2010, en base al censo 2002. Datos para 2011
# calculados a partir de la tasa de crecimiento de cada municipio
load('proyeccionesINEPoblación2006-2011.RData')
dim(proyPopulation)


# Limpieza inicial de datos
# Reemplaza los datos vacíos con NA
resp <- replace(resp, resp=='', NA)
# SASubjectID no es un factor, es un character
# Es el identificador del paciente inscrito
resp$SASubjectID <- as.character(resp$SASubjectID)


columnasEleg <- grep('eleg',names(resp), value=TRUE)
largoresp <- nrow(resp)
# Función para saber si un registro es elegibleViCo
resp[,columnasEleg] <- replace(resp[,columnasEleg],is.na(resp[,columnasEleg]),0)
elegvico <- function(x) any(resp[x,columnasEleg]==1)
# Elegibles para algún programa de ViCo
resp$elegibleViCo <- tapply(1:largoresp,1:largoresp,elegvico)
rm(columnasEleg, largoresp, elegvico)


# Organización de datos
# Columnas de interés para mi análisis.
# Se muestran en una columna, y a la derecha una descripción
# breve de su contenido, en forma de comentario.
# (-ETI) en los comentarios indica que es una variable que excluye
# el registro como caso de ETI
# Por el límite de caracteres para un statement,
# la definición de 'columnasInteres' se divide en tres statements.
# Statement 1 para la definición de columnasInteres
columnasInteres <- c(

  # Datos generales de admisión
  'elegibleRespira',    # Elegible para el estudio de respiratorias,
            # en centro y puesto significa ETI
  'epiWeekAdmision',    # Semana en que fue reclutado
  'epiYearAdmision',    # Año en que fue reclutado

  # Lugar donde fue admitido
  'SubjectSiteID',      # ID del sitio en que fue reclutado
  'SiteType',        # Tipo de sitio. C=Centro de salud, P=Puesto de salud
  'SiteDepartamento',    # Departamento en que fue reclutado
  'NombreDepartamento',    # nombre del departamento
  'NombreMunicipio',    # nombre del municipio
  'departamento',      # departamento de origen (dirección)
  'municipio',      # municipio de origen (dirección)
  'comunidad',      #
  'catchment',      # ¿Si es del área de captación?

  # Datos demográficos
  'sexo',        # 1 es hombre, 2 es mujer
  'edadAnios',      # Años cumplidos
  'edadMeses',      # Si años es 0, meses cumplidos
  'edadDias',        # Si meses es 0, dias cumplidos
  'fechaDeNacimiento',    # Fecha de nacimiento, para verificar edad
  'pacienteGrupoEtnico'    # Grupo étnico
)

# Statement 2 para la definición de columnasInteres
columnasInteres <- c(
  columnasInteres,
  # Datos de salud
  'muerteViCo',      #
  'temperaturaPrimeras24Horas',  # Presentación de fiebre
  'sintomasRespiraFiebre',  # Presentación de fiebre
  'sintomasRespiraHipotermia',  # Presentación de hipotermia
  'sintomasFiebre',      # Presentación de fiebre
  'sintomasFiebreDias',    # Presentación de fiebre
  'fiebreOHistoriaFiebre'  # Historial de presentación de fiebre
)

# Statement 3 para la definición de columnasInteres
columnasInteres <- c(
  columnasInteres,
  # Datos de laboratorio
  'viralPCR_Hizo',      # Se realizó pruebas de PCR para virus
  'viralPCR_RSV',      # PCR para Respiratory syncytial virus
  'viralPCR_hMPV',      # PCR para Human meta-pneumo virus
  'viralPCR_hPIV1',      # PCR para Human parainfluenza virus (1)
  'viralPCR_hPIV2',      # PCR para Human parainfluenza virus (2)
  'viralPCR_hPIV3',      # PCR para Human parainfluenza virus (3)
  'viralPCR_AD',      # PCR para Adenovirus
  'viralPCR_FluA',      # PCR para Influenza virus A
  'viralPCR_FluAH1',    # PCR para Influenza virus A, H1
  'viralPCR_FluAH3',    # PCR para Influenza virus A, H3
  'viralPCR_FluAH5a',    # PCR para Influenza virus A, H5a
  'viralPCR_FluAH5b',    # PCR para Influenza virus A, H5b
  'viralPCR_FluASwA',    # PCR para Influenza virus A, A porcino
  'viralPCR_FluASwH1',    # PCR para Influenza virus A, H1 porcino
  'viralPCR_FluB',      # PCR para Influenza virus B
  'viralPCR_RNP',      # PCR para ribonucleoproteína de Influenza
  'bacterialPCR_Hizo',    # Se realizó pruebas de PCR para bacterias
  'bacterialPCR_CP',    # 
  'bacterialPCR_LP',    # 
  'bacterialPCR_LL',    # 
  'bacterialPCR_MP',    # 
  'bacterialPCR_Lsp',    # 
  'bacterialPCR_SP',    # 
  'bacterialBinax_Hizo',    # 
  'bacterialBinax_Lsp',    # 
  'bacterialBinax_Sp'    # 
)

CdNuevaStaRosa <- 614
# Criterios a cumplir para las filas
# Se muestran en una columna, y a la derecha una descripción
# breve de su significado, en forma de comentario.
criteriosFilas <- 
  !is.na(resp$SASubjectID)   &   # Fue enrolado en el estudio
  resp$SiteType!='H '    &  # Fue tamizado en un centro o puesto de salud
  resp$SiteDepartamento=='SR'  &  # Fue tamizado en Santa Rosa
  resp$catchment == 1 &  # Vive en el área de captación
  resp$municipio == CdNuevaStaRosa & # Vive en Nueva Santa Rosa
  resp$epiYearAdmision >= 2008 & # A partir de 2008
  is.na(resp$indicacionRespira_otra)  &  # No presenta otra indicacionRespira
  is.na(resp$indicacionFebril_otra)  &  # No presenta otra indicacionFebril
  is.na(resp$fiebreOtraRazon_esp)  # No hay otra razón para la fiebre

# Crea un data.frame con el subset de los incluidos al estudio (enrolados)
datos <- resp[criteriosFilas, columnasInteres]
dim(datos)  # datos contiene la información de los enrolados en Santa Rosa
rm(criteriosFilas, columnasInteres) # Elimina objetos que ya no se usarán


# Recodifica por grupo etario, en grupos de 5 años
datos$grupoEtario <- as.factor(recode(datos$edadAnios,
      " 0:4='00-04';
        5:9='05-09';
        10:14='10-14';
        15:19='15-19';
        20:24='20-24';
        25:29='25-29';
        30:34='30-34';
        35:39='35-39';
        40:44='40-44';
        45:49='45-49';
        50:54='50-54';
        55:59='55-59';
        60:64='60-64';
        65:100='65+'
      "))


# Crea carpetas para escribir los resultados
carpetas <- c('_resultTablas', '_resultGráficas')
shell(paste('md',carpetas,collapse=' ')) # Usa el comando makedir


# Información Tabla1
# 'Pirámide' de captación de ViCo
criterioTamizadoSR <-  resp$SiteDepartamento=='SR' &
                      resp$epiYearAdmision >= 2008
CS <- resp$SiteType=='CS'
PS <- resp$SiteType=='PS'

nrow(resp)  # Tamizados ViCo
piramideViCo <- rbind(
  data.frame(
    Tamizados = sum(criterioTamizadoSR & CS, na.rm=TRUE),
    Elegibles = sum(criterioTamizadoSR & CS & resp$elegibleViCo, na.rm=TRUE),
    Enrolados = nrow(datos[datos$SiteType=='CS',]),
    ETI = sum(datos$elegibleRespira==1 & datos$SiteType=='CS', na.rm=TRUE)
  ),
  data.frame(
    Tamizados = sum(criterioTamizadoSR & PS, na.rm=TRUE),
    Elegibles = sum(criterioTamizadoSR & PS & resp$elegibleViCo, na.rm=TRUE),
    Enrolados = nrow(datos[datos$SiteType=='PS',]),
    ETI = sum(datos$elegibleRespira==1 & datos$SiteType=='PS', na.rm=TRUE)
  )
)
piramideViCo <- rbind(piramideViCo, colSums(piramideViCo))
rownames(piramideViCo) <- c('Centro de salud', 'Puestos de salud', 'Total')
piramideViCo
# Escribe el resultado como csv
write.csv(piramideViCo, '_resultTablas//piramideViCo.csv')
# Limpia
rm(criterioTamizadoSR, CS, PS)


# Tabla de enrolados y ETI por grupo de edad
EEporGrupoEdad <- cbind(
  'Enrolados' = t(table(datos$elegibleRespira!=0,datos$grupoEtario))[,1],
  '(%)' =  paste(
              '(',
              round(t(table(datos$elegibleRespira!=0,datos$grupoEtario))[,1]*
                    100/piramideViCo[3,3],2),
              ')', sep=''
            ),
  'ETI' = t(table(datos$elegibleRespira==1,datos$grupoEtario))[,2],
  '(%)' =  paste(
              '(',
              round(t(table(datos$elegibleRespira==1,datos$grupoEtario))[,2]*
                    100/piramideViCo[3,4],2),
              ')', sep=''
            )
)
EEporGrupoEdad <- rbind(
  EEporGrupoEdad,
  c(  sum(as.integer(EEporGrupoEdad[,1])),
       paste(
         '(',
       sum(as.double(substr(EEporGrupoEdad[,2],2,nchar(EEporGrupoEdad[,2])-1))),
         ')', sep=''
       ),
       sum(as.integer(EEporGrupoEdad[,3])),
       paste(
         '(',
      sum(as.double(substr(EEporGrupoEdad[,4],2,nchar(EEporGrupoEdad[,4])-1))),
         ')', sep=''
       )
  )
)
EEporGrupoEdad
write.csv(EEporGrupoEdad, '_resultTablas//EEporGrupoEdad.csv', quote=c(1))


# Tabla de enrolados y ETI por grupo sexo
EEporSexo <- cbind(
  'Enrolados' = t(table(datos$elegibleRespira!=0,datos$sexo))[,1],
  '(%)' =  paste(
              '(',
              round(t(table(datos$elegibleRespira!=0,datos$sexo))[,1]*
                    100/piramideViCo[3,3],2),
              ')', sep=''
            ),
  'ETI' = t(table(datos$elegibleRespira==1,datos$sexo))[,2],
  '(%)' =  paste(
              '(',
              round(t(table(datos$elegibleRespira==1,datos$sexo))[,2]*
                    100/piramideViCo[3,4],2),
              ')', sep=''
            )
)
EEporSexo <- rbind(
  EEporSexo,
  c(  sum(as.integer(EEporSexo[,1])),
       paste(
         '(',
         sum(as.double(substr(EEporSexo[,2],2,nchar(EEporSexo[,2])-1))),
         ')', sep=''
       ),
       sum(as.integer(EEporSexo[,3])),
       paste(
         '(',
        sum(as.double(substr(EEporSexo[,4],2,nchar(EEporSexo[,4])-1))),
         ')', sep=''
       )
  )
)
rownames(EEporSexo) <- c('Hombres', 'Mujeres', 'Total')
EEporSexo
write.csv(EEporSexo, '_resultTablas//EEporSexo.csv')


# Limpieza de datos
# Revisar fechas de admisión
table(datos$epiWeekAdmision, useNA='always')
table(datos$epiYearAdmision, useNA='always')
# No faltan datos de tiempo epi de admisión. Todo bien.

   
# Limpieza de datos
# Por el contenido de las columnas, R define el tipo de las variables
# en resp de forma diferente a la esperada, p.e.
# si está codificada en números, le asigna tipo integer,
# cuando en realidad es un factor.
# Sigue una matriz que especifica el tipo deseado para cada columna de datos
# tiposDeseados[,1] contiene el nombre de cada columna en datos
# tiposDeseados[,2] contiene el tipo que se desea
tiposDeseados <- 
matrix(ncol=2, byrow=TRUE,  
  data= c(
    'elegibleRespira'  , 'factor',
    'epiWeekAdmision'  , 'factor',
    'epiYearAdmision'  , 'factor',
    'SubjectSiteID'  , 'factor',
    'SiteType'  , 'factor',
    'SiteDepartamento'  , 'factor',
    'NombreDepartamento'  , 'factor',
    'NombreMunicipio'  , 'factor',
    'departamento'  , 'factor',
    'municipio'  , 'factor',
    'comunidad'  , 'factor',
    'catchment'  , 'factor',
    'sexo'  , 'factor',
    'edadAnios'  , 'integer',
    'edadMeses'  , 'integer',
    'edadDias'  , 'integer',
    'fechaDeNacimiento'  , 'factor',
    'pacienteGrupoEtnico'  , 'factor',
    'muerteViCo'  , 'factor',
    'temperaturaPrimeras24Horas'  , 'double',
    'sintomasRespiraFiebre'  , 'factor',
    'sintomasRespiraHipotermia'  , 'factor',
    'sintomasFiebre'  , 'factor',
    'sintomasFiebreDias'  , 'integer',
    'fiebreOHistoriaFiebre'  , 'factor',
    'viralPCR_Hizo'  , 'factor',
    'viralPCR_RSV'  , 'factor',
    'viralPCR_hMPV'  , 'factor',
    'viralPCR_hPIV1'  , 'factor',
    'viralPCR_hPIV2'  , 'factor',
    'viralPCR_hPIV3'  , 'factor',
    'viralPCR_AD'  , 'factor',
    'viralPCR_FluA'  , 'factor',
    'viralPCR_FluAH1'  , 'factor',
    'viralPCR_FluAH3'  , 'factor',
    'viralPCR_FluAH5a'  , 'factor',
    'viralPCR_FluAH5b'  , 'factor',
    'viralPCR_FluASwA'  , 'factor',
    'viralPCR_FluASwH1'  , 'factor',
    'viralPCR_FluB'  , 'factor',
    'viralPCR_RNP'  , 'factor',
    'bacterialPCR_Hizo'  , 'factor',
    'bacterialPCR_CP'  , 'factor',
    'bacterialPCR_LP'  , 'factor',
    'bacterialPCR_LL'  , 'factor',
    'bacterialPCR_MP'  , 'factor',
    'bacterialPCR_Lsp'  , 'factor',
    'bacterialPCR_SP'  , 'factor',
    'bacterialBinax_Hizo'  , 'factor',
    'bacterialBinax_Lsp'  , 'factor',
    'bacterialBinax_Sp'  , 'factor'
    )
)

# Cambia el tipo de las columnas en datos al tipo deseado
for(.var in 1:length(tiposDeseados[,1])){
  datos[tiposDeseados[.var,1]] <- 
    eval(
      call(paste('as.',tiposDeseados[.var,2], sep=''), 
      eval(datos[,tiposDeseados[.var,1]]))
    )
}
rm(tiposDeseados) # Elimina el objeto que ya no se usará


# Tablas para conteos variables de diagnóstico de laboratorio
# para saber las causas de ETI en los pacientes enrolados
# Tabla para diagnóstico de virus
(labVirus <- tablaVariables('viralP',datos))
write.csv(labVirus, '_resultTablas//labVirus.csv')
    
# Tabla para diagnóstico de bacterias
(labBacter <- tablaVariables('bacte',datos))
write.csv(labBacter, '_resultTablas//labBacter.csv')


datos$labPatogeno <- 'No_identificado'
#  Adenovirus
criteriolab <- datos$viralPCR_AD==1
datos$labPatogeno[criteriolab] <- 'Adenovirus'
#  RSV
criteriolab <- datos$viralPCR_RSV==1
datos$labPatogeno[criteriolab] <- 'RSV'
#  hMPV
criteriolab <-datos$viralPCR_hMPV==1
datos$labPatogeno[criteriolab] <- 'hMPV'
#  hPIV
criteriolab <- with(datos, viralPCR_hPIV1==1 |viralPCR_hPIV2==1 |
viralPCR_hPIV3==1
)
datos$labPatogeno[criteriolab] <- 'hPIV'
#  InfluenzaA
criteriolab <- with(datos, viralPCR_FluA==1 | viralPCR_FluAH1==1 |
  viralPCR_FluAH3==1 | viralPCR_FluAH5a==1 | viralPCR_FluAH5b==1
)
datos$labPatogeno[criteriolab] <- 'InfluenzaA'
#  InfluenzaAH1N1
criteriolab <- datos$viralPCR_FluASwA==1 & datos$viralPCR_FluASwH1==1
datos$labPatogeno[criteriolab] <- 'InfluenzaAH1N1'
#  InfluenzaB
criteriolab <- datos$viralPCR_FluB==1
datos$labPatogeno[criteriolab] <- 'InfluenzaB'
# Bacteria
criteriolab <- with(datos, bacterialPCR_CP==1 | bacterialPCR_LP==1 |
  bacterialPCR_LL==1 | bacterialPCR_MP==1 | bacterialPCR_Lsp==1 |
  bacterialPCR_SP==1
)
datos$labPatogeno[criteriolab] <- 'Bacteria'
# Crea la tabla
tablaLabPAtogeno <- data.frame(
  table(datos$labPatogeno[datos$elegibleRespira==1]))
names(tablaLabPAtogeno)<-c('Patógeno','Casos')
tablaLabPAtogeno
write.csv(tablaLabPAtogeno, '_resultTablas//tablaLabPAtogeno.csv')

# Patógeno por grupo de edad
labPatogenoEtario <- table(datos$labPatogeno[datos$elegibleRespira==1],
      datos$grupoEtario[datos$elegibleRespira==1])


# Gráficas de incidencia de patógeno por grupo de edad
labPatogenoEtarioIncidencia <-ceiling(labPatogenoEtario/
with(proyPopulation, proyPopulation[IDMunicipio==614,'pa2011']) *
100000)
write.csv(labPatogenoEtarioIncidencia,
          '_resultTablas//labPatogenoEtarioIncidencia.csv')

.d<-getwd()
setwd('_resultGráficas')
shell('md patogenoEtario')

# Calcula el límite vertical para las gráficas de incidencia por patógeno,
# para que todas tengan el mismo y sea comparable
maxI<-max(labPatogenoEtarioIncidencia[-7,]) # Máxima incidencia
ordenmaxI <- maxI / as.double(paste('0.', maxI, sep='')) / 10
maxI <- ceiling(maxI/ordenmaxI)*ordenmaxI
patogenos <- rownames(labPatogenoEtarioIncidencia)
for(.pat in 1:length(patogenos)){
  png(paste('patogenoEtario//', patogenos[.pat], '.png', sep=''),
      width=800, height=300)
  barplot(labPatogenoEtarioIncidencia[.pat,],
          xlab=as.expression(bquote(bold('Grupo etario'))),
          ylab=as.expression(bquote(bold('Incidencia (x100 000)'))),
          ylim=c(0,6000)
          )
  dev.off()
}


# Calcula la incidencia semanal de Influenza A H1N1 durante 2009

# Casos por semana y grupo etario. 79 casos en 20 semanas de 2009, 23 semanas de inicio a fin
influenzaAH1N12009Etario <- 
  with(datos[datos$labPatogeno=='InfluenzaAH1N1' &
             datos$epiYearAdmision==2009,],
       table(epiWeekAdmision, grupoEtario)
  )

# Incidencia por 100 000
incidenciaH1N12009Etario <-
	influenzaAH1N12009Etario/
	sum(proyPopulation[proyPopulation$IDMunicipio==614,'pa2009'])*100000

semanasBroteAH1N12009 <- 1+ max(which(rowSums(incidenciaH1N12009Etario)!=0)) - 
                         min(which(rowSums(incidenciaH1N12009Etario)!=0))
(semanasBroteConCasos <- sum(rowSums(incidenciaH1N12009Etario)!=0))


png('incidenciaH1N1_2009.png', width=2000, height=300)
	par(mfrow=c(1,5), cex.lab=1.5, cex.axis=1.5)
	layout(matrix(c(0,1,1,1,2,2), nrow=1, byrow=TRUE))
	
	barplot(rowSums(incidenciaH1N12009Etario),
	        xlab=as.expression(bquote(bold('Semana epidemiológica'))),
	        ylab=as.expression(bquote(bold('Incidencia (x100 000)'))),
	        ylim=c(0,70)
	)
	barplot(colSums(incidenciaH1N12009Etario)/semanasBroteAH1N12009,
	        xlab=as.expression(bquote(bold('Grupo etario'))),
	        ylab=as.expression(bquote(bold(
	        	'Incidencia durante el brote (x100 000)'))),
	        ylim=c(0,7)
	)
dev.off()

# 18 casos en 2010
casosAH1N12010 <- with(datos[datos$labPatogeno=='InfluenzaAH1N1' &
             datos$epiYearAdmision==2010,],
       sum(table(epiWeekAdmision))
  )
casosAH1N12010


setwd(.d)
rm(.d, .pat)

# Agrega columna de totales
labPatogenoEtario <- t(cbind(labPatogenoEtario,
      Total = rowSums(labPatogenoEtario),
      '(%)' = round( rowSums(labPatogenoEtario)/
                    sum(rowSums(labPatogenoEtario))*100, 2))
)
# Escribe la tabla con el total de labPatógeno por grupo de edad
write.csv(labPatogenoEtario, '_resultTablas//labPatogenoEtario.csv')


# Otras gráficas
  

# Define el período 2008-2010 para datos históricos del sistema
# Los datos actuales son los de 2011
periodo = datos$epiYearAdmision!='2011'


# Modelo para epiTimeHist()
modeloHist <- with( datos[datos$elegibleRespira==1,],
      epiWeekAdmision ~ epiWeekAdmision | epiYearAdmision #+ SiteType
)
# El histograma
curvaEpidemica  <-   
  histogram  (  modeloHist,
                type='count',
                ylab=as.expression(bquote(bold('Casos de ETI'))),
                xlab=as.expression(bquote(bold('Año y semana epidemiológica'))),
                col='gray', strip=strip.custom(bg='white'),
                scales=list(
                    x=list(  alternating=1,
                      at=seq(1,52,4)
                  )
                )
              )
png('_resultGráficas//CurvaEpidémicaETI_2008-2011.png',
    width = 1200, height = 300)
curvaEpidemica
dev.off()

# Casos por semana y año
casosConteo <- with(  datos[datos$elegibleRespira==1,],
                table(epiWeekAdmision, epiYearAdmision)
              )
#Incidencia por semana y año
casos<-casosConteo/
          rep(  colSums(poblacion(CdNuevaStaRosa,'all',2008:2011, 'list')),
                each=52)*100000


# Barplot de incidencia por semana epidemiológica, 2008-2011
png('_resultGráficas//barrasIncidencias.png',
    width=2400, height=600)
barplot(  as.vector(casos),
           ylim=c(0,100),
           ann=FALSE, xaxt="n", yaxt="n",        # No prepara ejes
           #ylab=quote(bold('Tasa de incidencia semanal (x100 000)')),  # Eje y
          xlab=quote(bold('Año y semana epidemiológica')),  # Título de eje x
           cex.lab = 2,  # factor para agrandar los títulos de eje
          mgp=c(3,1,0)  # Deja espacio abajo
         )
# Agrega eje y
axis(side=2, pos=0, at=seq(from=0, to=100, by=20))
# Dibuja lineas para agrupar por año, abajo de ejex
par(xpd=TRUE)
lines(c(0.5,62),c(-2,-2))
lines(c(0.5,62)+62.5,c(-2,-2))
lines(c(0.5,62)+125,c(-2,-2))
lines(c(1,62)+187,c(-2,-2))
# Escribe las etiquetas de los años
text(c(31,94,155,219),c(-5,-5,-5,-5),2008:2011, cex=2)
text(-7, 50,quote(bold('Tasa de incidencia semanal (x100 000)')), cex=2, srt=90)
par(xpd=FALSE)
dev.off()


# Calcula datos para corredor endémico (Q1, Q2, Q3)
# Este corredor es con tasas
# No hay datos para 2011 a partir de la semana 30
casos[30:52,'2011']<-NA # Quita los datos de 2011 que no están incluídos

rowSummary<-function(x) summary(casos[x,1:3])[c(2,3,5)]
datosCorredor <- tapply(X=1:52, INDEX=1:52, FUN=rowSummary)
datosCorredor <- as.data.frame(do.call("rbind",datosCorredor))
datosCorredor <- cbind(datosCorredor, max(datosCorredor)+3)
listaCorredor <- as.vector(t(as.matrix(datosCorredor)))
Corredor <- data.frame(  Semana  =  rep(1:52, each=4),
                            Zona  =  rep(c(  '4 Éxito','3 Seguridad',
                                          '2 Alerta', '1 Epidémico'), times=52),
                              Tasa  =  listaCorredor
                          )
# Prepara la gráfica del corredor endémico
colores <- c('dodgerblue3','chartreuse3','goldenrod1','firebrick')
grafCorredor <- ggplot(  Corredor,
                        aes(x=Semana, y=Tasa, group=Zona, fill=colores)) + 
                          geom_area(position="identity") + 
                          scale_fill_identity(
                            name='Zona',
                            breaks=colores,
                            labels=c(  '4 Éxito','3 Seguridad',
                                      '2 Alerta', '1 Epidémico')
                          )
V2011 <- as.vector(t(cbind(casos[,'2011'], NA, NA, NA)))
df2011 <- data.frame(
  Semanas2011 = rep(1:52, each=4),
  Valores2011 = V2011
)
color2011 <- c('black',colores)    
grafCorredor <-    grafCorredor + geom_line(  aes(  x=df2011$Semanas2011,
                                     y=df2011$Valores2011),
                              size=2, position="identity") +
                                scale_fill_identity(
                            name='Clave',
                            breaks=color2011,
                            labels=c('2011', c(  '4 Éxito','3 Seguridad',
                                                '2 Alerta', '1 Epidémico'))
                          )
# Dibuja el corredor endémico, a un archivo
png('_resultGráficas//corredorEndémicoTasas.png', width = 1200, height = 600)
grafCorredor
dev.off()

# Fin del análisis
save.image('_workspaceResultado.RData')
data.frame(md5sum('_workspaceResultado.RData'))
# Fin del script