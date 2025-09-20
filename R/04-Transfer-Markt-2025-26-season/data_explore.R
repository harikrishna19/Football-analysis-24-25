



# Function to convert values to Numeric -----------------------------------

convert_value <- function(x) {
  x <- toupper(x)
  if (grepl("M$", x)) {
    as.numeric(readr::parse_number(x)) * 1e6
  } else if (grepl("K$", x)) {
    as.numeric(readr::parse_number(x)) * 1e3
  } else if (suppressWarnings(!is.na(as.numeric(x)))) {
    as.numeric(x)
  } else {
    NA_real_
  }
}

# Format result with M/K suffix
format_value <- function(val) {
  if (val >= 1e6) {
    paste0(round(val / 1e6, 1), "M")
  } else if (val >= 1e3) {
    paste0(round(val / 1e3, 1), "K")
  } else {
    as.character(val)
  }
}


get_summaries <- function(data) {
  data <- data %>% dplyr::group_by(Team) %>% 
    dplyr::mutate(NumericValue = sapply(Fee, convert_value)) %>% 
    dplyr::summarise(Total_Spent=sum(NumericValue,na.rm = TRUE),
                     Total_Spent_Convert=format_value(Total_Spent))
  return(data)
}
Sp<-get_summaries(pl_incomings)
Re<-get_summaries(pl_outgoings)
combined<-merge(Sp,Re,by="Team")
combined$Team <- factor(combined$Team, levels = teams)
combined <- combined[order(combined$Team), ]
team_logos=unique(pl_incomings$team_logo)
combined$team_logo<-team_logos

colnames(pl_incomings)[3]<-"Player"
colnames(pl_outgoings)[3]<-"Player"

pl_incomings$Direction="Incoming"
# Function to make child tables
make_player_table <- function(data) {
  reactable(
    data %>% select(Player, Fee, Position, Age),
    bordered = TRUE, highlight = TRUE, compact = TRUE,pagination = F,
    columns = list(
      Player = colDef(minWidth = 140,align = "center"),
      Fee = colDef(align = "center"))
  )
}

datasets <- list(
  Incoming = pl_incomings,
  Outgoing = pl_outgoings
)

# Build main table
reactable(
  theme = reactableTheme(
    headerStyle = list(
      "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
      "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
      borderColor = "#555"
    )
  ),
  rowStyle = JS(
    paste0(
      "function(rowInfo) {
        if (!rowInfo) return {};
        var colors = ", jsonlite::toJSON(team_colors), ";
        return { backgroundColor: colors[rowInfo.index] };
      }"
    )
  ),
  pagination = F,
  combined %>% select(team_logo,Team,Total_Spent_Convert.x,Total_Spent_Convert.y,-Total_Spent.x,-Total_Spent.y),
  details = function(index) {
    team <- combined$Team[index]
    subtables <- lapply(names(datasets), function(dir) {
      sub <- datasets[[dir]] %>% filter(Team == team)
      if (nrow(sub) > 0) {
        htmltools::div(
          htmltools::h4(paste(dir)),
          make_player_table(sub)
        )
      }
    })
    htmltools::div(subtables)
  },
  bordered = TRUE,searchable = T,
  striped = TRUE,fullWidth = T,
  compact = T,
  highlight = TRUE,
  columns = list(
    team_logo = colDef(
      name = "",
      width = 30,
      # render team logos from their image address
      style = background_img(position = "center")
    ),
    Team = colDef(minWidth = 80,align = "center",vAlign = "center"),
    Total_Spent_Convert.x = colDef(name = "Total Money Spent (£M)",align="center"),
    Total_Spent_Convert.y = colDef(name = "Total Money From Transfers (£M)",align = "center")
  )
)




