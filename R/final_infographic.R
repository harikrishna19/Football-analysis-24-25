

library(patchwork)

gr<-calculate_goal_ranges(data=liv_match_summaries,team_name = "Liverpool",team_colour = "red")
gs<-calculate_goals_assists(data = liv_match_summaries,team_name="Liverpool")
table_df

build_pitch+gr+gs+test_plot+wrap_elements(full = table_df)


(table_df+gr/gs) / wrap_elements(full = build_pitch)+test_plot
build_pitch+gr+gs+final_plot
