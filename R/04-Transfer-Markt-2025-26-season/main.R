


# Sourcing functions  -----------------------------------------------------

source("utils.R")
source("func_data_clean.R")
source("func_subtables.R")


# Read Transfer Markt Data from the data folder ------------------------------------------------

pl_incomings<-read.csv("data/pl_incomings.csv",check.names = T)
pl_outgoings<-read.csv("data/pl_outgoings.csv",check.names = T)



# Clean the Data sets using functions -------------------------------------

datasets <- list(pl_incomings, pl_outgoings)

datasets <- lapply(datasets, function(df) {
  if(any(colnames(df) %in% c("Left.1","In"))){
    keep_order=c("team_logo","Team","In","Age","Position","Pos","Fee","Market.value","Left.1")
    drop=c("Left","Nat.")
    player_name_col = "In"
  }
  else{
    keep_order=c("team_logo","Team","Out","Age","Position","Pos","Fee","Market.value","Joined.1")
    drop=c("Joined","Nat.")
    player_name_col = "Out"
  }
  data_clean<-clean_dataset(
    df,
    keep_order = keep_order,
    drop       = drop,
    player_name_col = player_name_col
  )
  list(
    data_clean   = data_clean,
    summary   = get_summaries(data_clean)
  )
})




# Outer table logic -------------------------------------------------------

outer_table<-merge_tables(inc_data = datasets[[1]]$summary,
                          out_data=datasets[[2]]$summary,teams = teams,pl_inc_data = pl_incomings
                            )
outer_table$Expenditure<-NA
inner_table<-list(Incoming=datasets[[1]]$data_clean,
                  Outgoing=datasets[[2]]$data_clean)

# Build main table reactable ----------------------------------------------


# Build main table
htmltools::browsable(
  htmltools::div(
    style = "display:grid;align-content:center;justify-content:center;width: 70%; margin: 0 auto; text-align: center;",
    div(
      style = "display: flex; align-items: center; gap: 10px;",
      
      # Premier League logo
      tags$img(
        src = "https://upload.wikimedia.org/wikipedia/en/f/f2/Premier_League_Logo.svg",
        height = "60px"
      ),
      
      # Title + Subtitle
      div(
        tags$h1("2025-26 Premier League Transfers"),
        tags$h4(
          style = "color: black; font-weight: normal;",
          "All the incoming and outgoings for each club in the Premier League for 2025/26 season"
        )
      )
    ),
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
      outer_table %>% select(team_logo,Team,Total_Spent_Convert.x,Total_Spent_Convert.y,Expenditure,-Total_Spent.x,-Total_Spent.y),
      details = function(index) {
        team <- outer_table$Team[index]
        subtables <- lapply(names(inner_table), function(dir) {
          sub <- inner_table[[dir]] %>% filter(Team == team)
          if (nrow(sub) > 0) {
            htmltools::div(
              htmltools::h4(paste(dir)),
              make_player_table(sub)
            )
          }
        })
        htmltools::div(subtables)
      },
      bordered = TRUE,searchable = T,    width = 900,
      height = 1100,
      striped = TRUE,
      wrap = FALSE,
      showSortIcon = FALSE,
      compact = T,outlined = T,
      highlight = TRUE,
      columns = list(
        team_logo = colDef(name="",
                           cell = embed_img(width=30,height=40)
        ),
        Team = colDef(minWidth = 80,align = "center",vAlign = "center"),
        Total_Spent_Convert.x = colDef(name = "Total Money Spent (£M)",align="center"),
        Total_Spent_Convert.y = colDef(name = "Total Money From Transfers (£M)",align = "center"),
        Expenditure=colDef(
                           cell = function(value, index) {
                           paste0(parse_number(outer_table$Total_Spent_Convert.x[index]) - 
                                    parse_number(outer_table$Total_Spent_Convert.y[index]),"M")
                           })
      )
    ),
    tags$p(style = "margin-top: 10px; color: #666;", "Data source:Transfer Markt Data,Table Design:By Hari Krishna")
    
  ))




























