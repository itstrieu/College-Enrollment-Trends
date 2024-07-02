loadPackages <- function(packages) {
  if (!requireNamespace("pacman", quietly = TRUE)) {
    install.packages("pacman")
  }
  
  library(pacman)
  
  p_load(char = packages)
}


removeNA <- function(dfList, threshold = 80) {
    na_percentages <- colMeans(is.na(df)) * 100
    columns_to_remove <- names(na_percentages[na_percentages > threshold])
    df_clean <- df[, !names(df) %in% columns_to_remove]
    cleaned_df_list[[i]] <- df_clean
  return(cleaned_df_list)
}

# Function to calculate near-zero variance metrics and remove 
removeNZV <- function(df) {
  zeroVarDf <- nearZeroVar(df, saveMetrics = TRUE)
  zeroVarCols <- rownames(zeroVarDf)[zeroVarDf$nzv == TRUE]
  dfFiltered <- df[, !colnames(df) %in% zeroVarCols]
  return(dfFiltered)
}