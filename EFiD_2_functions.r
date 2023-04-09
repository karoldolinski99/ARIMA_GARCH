
function_rate_of_return <- function(data_frame)
{
  # data_frame:
  # 1st column - Date
  # 2nd column - Closing
  
  result <- as.data.frame(matrix(NA, nrow = nrow(data_frame)-1, ncol = 2))
  result[, 1] <- as.Date(data_frame[2:nrow(data_frame), 1])
         
  for(i in 2:nrow(data_frame))
  {
    result[i-1, 2] <- log(data_frame[i,2] / data_frame[i-1,2])
  }
  colnames(result) <- c('Date', 'Rate_of_Return')
  return(result)
}