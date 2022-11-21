# ---- FUNKCJE POMOCNICZE ----
cefficient_of_variation = function(df){
  w <- c()
  for (i in 1:ncol(df)){
    w[i] <- round((sd(df[,i])/mean(df[,i])),2)}

  w <- data.frame(w, row.names = colnames(df))

  return(w)
}

wss <- function(df, k) {
  stats::kmeans(df, k, nstart = 10 )$tot.withinss
}

colculate_wss = function(df){

  k.values <- 1:15
  wss_values <- c()

  for (i in k.values){
    wss_values[i] <- wss(df, i)
  }

  return(wss_values)
}

standardize = function(df){return(scale(df))}

optimasize_k = function(df){

  wss_values <- colculate_wss(df)

  i = 2

  while (((wss_values[i-1] - wss_values[i])/wss_values[i-1]) > 0.25){
    i = i + 1
  }

  return(i - 1)
}


# --- CLUSTER ANALSIS -----

#' Create clusterAnalysis
#'
#' @param df input data
#' @return clusterAnalysis
#' @export
clusterAnalysis<-function(df){
  df_scale = standardize(df)
  k <- optimasize_k(df_scale)
  km <- stats::kmeans(df, k, nstart = 10)

  a <- km$cluster
  df['grup'] <- a

  this<-list(df=df, df_scale=df_scale, k=k, km=km)
  class(this)<-append(class(this), 'clusterAnalysis')
  return(this)
}

#' Plot clusterAnalysis
#'
#' @param x clusterAnalysis
#' @export
plot.clusterAnalysis<-function(x){
  corrgram::corrgram(cor(x$df),   lower.panel = corrgram::panel.shade, upper.panel = corrgram::panel.cor, main = "Macierz nasilenia korelacji")

  wss_values <- colculate_wss(x$df_scale)
  k.values <- 1:15
  plot(k.values, wss_values,
       type="b", pch = 19, frame = FALSE,
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")

  factoextra::fviz_cluster(x$km, data=x$df)
}

#' Print clusterAnalysis
#'
#' @param x clusterAnalysis
#' @export
print.clusterAnalysis = function(x){
  cat("Ten zbiór danych najlepiej podzielić na", x$k, "klastrów \n \n")
  cat("Centroidy klastrów: \n \n")
  print(x$km$centers)

  cat("\n \n Elementy w grupach \n \n")
  print(dplyr::arrange(x$df['grup'], grup))
}

#' Print summary of clusterAnalysis
#'
#' @param x clusterAnalysis
#' @export
summary.clusterAnalysis = function(x){
  cat("Statystyki opisowe zbioru danych \n")
  print(summary(x$df))

  cat("\n\nKorelacje między zmiennymi \n")
  print(cor(x$df))

  cat("\n\nWspółczynniki zmienności \n")
  print(cefficient_of_variation(x$df))

  print(x)
  plot(x)
}
