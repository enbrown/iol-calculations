make.df <- function(argument.list = list(L = 23.45,
                                         K = 45,
                                         A = c(118.4))) {
  df <- data.frame()
  for (i in names(argument.list)) {
    name.column <- paste0('name.', i)
    value.column <- i
    names <- names(argument.list[[i]])
    values <- argument.list[[i]]
    if (is.null(names)) names <- paste0(i, '=', values)
    args <- list(name = names,
                 value = values,
                 stringsAsFactors = FALSE)
    names(args)[1:2] <- c(name.column, value.column)
    df.new <- do.call(data.frame, args)
    if (nrow(df) == 0) {
      df <- df.new
    } else {
      df <- merge(df, df.new,
                  by = NULL, all = TRUE)
    }
  }
  return(df)
}