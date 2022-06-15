bar_data <- function(data0=taipeiall,locationname="台北" ) {
  data0[2:4,] ->data0
  colnames(data0)[1] <- "year"
  data0$year |> stringr::str_extract("[0-9]{4}") |> as.numeric() -> data0$year
  
  data0 |> tidyr::pivot_longer(
    cols = -1,
    names_to = "spots_name",
    values_to = "number"
  )->data0
  
  data0$number |> as.numeric() ->data0$ number
  data0$location <- locationname
  
  data0|>
    arrange(desc(year*100000000000+number))|>
    mutate(
      order=rep(seq_len(length(year)/3),3)
    )->data0
  return(data0)
}