from understatapi import UnderstatClient
import pandas as pd
import pdb
# Initialize Understat
understat = UnderstatClient()


from understatapi import UnderstatClient
import pandas as pd

understat = UnderstatClient()

def get_pl_table(season,teams):
    league = understat.league(league="EPL").get_team_data(season=season)
    
    
    df = pd.DataFrame(league).T  # transpose
    # Example: df['history'] contains list of dicts
    df_exploded = df.explode('history')

    # Convert dict → columns
    history_df = pd.json_normalize(df_exploded['history'])
    
    final_df = pd.concat([df_exploded.drop(columns=['history']).reset_index(drop=True),
                      history_df.reset_index(drop=True)],
                     axis=1)
    

    final_df["season"] = f"{season}/{str(season+1)[-2:]}"
    final_df=final_df[final_df["title"].isin(teams)]
    
    return final_df

  
  
def get_selected_seasons():
    seasons = [2022, 2023, 2024, 2025]
    teams=["Arsenal","Manchester City"]
    
    all_data = []
    
    for season in seasons:
        df = get_pl_table(season,teams)
        all_data.append(df)
    
    final_df = pd.concat(all_data, ignore_index=True)
    
    return final_df
  
df = get_selected_seasons()
print(df)
