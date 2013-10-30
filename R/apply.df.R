apply.df <- function(X, FUN, NAME, which = NULL) {
  FUN <- match.fun(FUN)
  X.names <- colnames(X)
  
  df <- data.frame()
  for (i in seq_len(nrow(X))) {
    # Get the arguments to pass to the function
    values <- X[i, , drop = FALSE]
    # Include only those function arguments that are needed by the function
    args <- as.list(formals(FUN))
    for (col in X.names) {
      if (col %in% names(args)) {
        args[[match(col, names(args))]] <- values[,col]
      }
    }
    if (! missing(which) && 'which' %in% names(args)) args$which <- which
    # Call the function
    result <- do.call(FUN, args)
    #print(str(result))
    # Convert the function results into a data.frame
    args <- list(name.FUN = names(result),
                 params.FUN = attr(result, 'function.arguments'),
                 FUN = result,
                 stringsAsFactors = FALSE)
    if (! missing(NAME)) {
      names(args)[1:3] <- c(paste0('name.', NAME),
                            paste0('params.', NAME),
                            NAME)
    }
    df.new <- do.call(data.frame, args)
    #print(str(df.new))
    # 
    df.new <- merge(values, df.new,
                    by = NULL, all = TRUE)
    if (nrow(df) == 0) {
      df <- df.new
    } else {
      df <- rbind(df, df.new)
    }
  }
  return(df)
}