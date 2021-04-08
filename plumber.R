# plumber.R
library(memoise)
library(EpiEstim)

cache_timeout <- 1800 #half an hour

##AUXILIARY FUNCTIONS

#Get complete data_frame from GUIAD
get_data_guiad <- memoise(function(location="https://raw.githubusercontent.com/GUIAD-COVID/datos-y-visualizaciones-GUIAD/master/datos/estadisticasUY.csv") {
    download.file(location, "estadisticasUY.csv")
    data <- read.csv(file = 'estadisticasUY.csv',na="N/A")
    data[,"fecha"] <- as.Date(data[,"fecha"],format="%d/%m/%Y")
    data <- data[order(data[,"fecha"]),]
    return(data)
}, cache = cachem::cache_mem(max_age = cache_timeout))

guiad <- get_data_guiad()

process_data_guiad <- function(datos, W=7, W2=3) {

        fecha <- datos[,"fecha"]
        incidencia <- datos[,"cantCasosNuevosAjustado"]
        incidencia_ma <- stats::filter(as.numeric(incidencia), rep(1,W)/W, sides=1)
        activos <- datos[,"cantPersonasConInfeccionEnCurso"]
        ratio_test <- as.numeric(datos[,"cantCasosNuevosAjustado"])/as.numeric(datos[,"cantTest"])*100
        ratio_test_suavizado_num <- stats::filter(as.numeric(datos[,"cantCasosNuevosAjustado"]), rep(1,W2), sides=1)
        ratio_test_suavizado_den <- stats::filter(as.numeric(datos[,"cantTest"]), rep(1,W2), sides=1)
        ratio_test_suavizado <- ratio_test_suavizado_num/ratio_test_suavizado_den*100
        return(data.frame(Tiempo = fecha, Incidencia = incidencia, MediaMovil = incidencia_ma, Activos = activos, RatioTest = ratio_test, RatioTestSuavizado = ratio_test_suavizado))

 }

estimate_R_country <- function(datos,window=7,mean_covid_si=3.95,sd_covid_si=4.75,delta_si=30) {

         discrete_si_distr <- discr_si(seq(0, delta_si), mean_covid_si, sd_covid_si)
         datos[is.na(datos[,"Incidencia"]),"Incidencia"] <- 0

         #vectores auxiliares para cambiar la ventana
         tt<-nrow(datos)
         t_start <- seq(2, tt-window+1)
         t_end <- t_start + window-1

         res <- estimate_R(incid = pmax(datos$Incidencia,0),
                         method = "non_parametric_si",
                         config = make_config(list(si_distr = discrete_si_distr,t_start=t_start,t_end=t_end)))

         shortened_times <- tail(datos$Tiempo,-window)
         datos_R_country <- data.frame(Tiempo=shortened_times,R=res$R[,c("Median(R)")],Rl=res$R[,"Quantile.0.025(R)"],Ru=res$R[,"Quantile.0.975(R)"])
         return(datos_R_country)
 }

update_csv <- memoise(function() {
        write.csv(estimate_R_country(process_data_guiad(get_data_guiad())),"estimacionR.csv", row.names = FALSE)
}, cache = cachem::cache_mem(max_age = cache_timeout))

guiad <- get_data_guiad()
update_csv()

#* @get /uruguay
function(req, res){

        attachmentString = paste0("attachment; filename=", "estimacionR.csv")
        res$setHeader("Content-Disposition", attachmentString)

        include_file("estimacionR.csv", res)

}
