get_summaries <- function(data) {
  data <- data %>% dplyr::group_by(Team) %>% 
    dplyr::mutate(NumericValue = sapply(Fee, convert_value)) %>% 
    dplyr::summarise(Total_Spent=sum(NumericValue,na.rm = TRUE),
                     Total_Spent_Convert=format_value(Total_Spent))
  return(data)
}



# Function to make child tables
make_player_table <- function(data) {
  reactable(
    data %>% select(Player, Fee, Position),
    bordered = TRUE, highlight = TRUE, compact = TRUE,pagination = F,resizable = T,striped = T,
    columns = list(
      Player = colDef(minWidth = 140,align = "center"),
      Fee = colDef(align = "center"))
    # rowStyle = JS(
    #   paste0(
    #     "function(rowInfo) {
    #   if (!rowInfo) return {};
    #   var colors = ", jsonlite::toJSON(team_colors), ";
    #   return { backgroundColor: colors[rowInfo.index] };
    # }"
    #   )
    # )
  )
}


merge_tables<-function(inc_data,out_data,teams,pl_inc_data){
  combined<-merge(inc_data,out_data,by="Team")
  combined$Team <- factor(combined$Team, levels = teams)
  combined <- combined[order(combined$Team), ]
  team_logos=unique(pl_incomings$team_logo)
  combined$team_logo<-team_logos
  return(combined)
}
