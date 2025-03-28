

library(patchwork)

gr<-calculate_goal_ranges(data=liv_match_summaries,team_name = "Liverpool",team_colour = "red")
gs<-calculate_goals_assists(data = liv_match_summaries,team_name="Liverpool")
table_df

build_pitch


(build_pitch+gr/gs) / wrap_elements(full = table_grob)
