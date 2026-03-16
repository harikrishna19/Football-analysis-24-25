from understatapi import UnderstatClient
import pandas as pd
from matplotlib import pyplot as plt
import dtale
# Initialize Understat
understat = UnderstatClient()

# Seasons to pull
seasons = [str(year) for year in range(2016, 2026)]

# Store data
all_players = []

for season in seasons:
    
    print(f"Fetching season {season}...")
    
    league_player_data = understat.league(league="EPL").get_player_data(season=season)
    
    df = pd.DataFrame(league_player_data)
    df['yr']=int(season)
    
    df["season"] = season
    
    all_players.append(df)

# Combine all seasons
player_data = pd.concat(all_players, ignore_index=True)


# Convert numeric columns (Understat returns strings)
numeric_cols = ["goals","assists","xG","xA","shots","key_passes","time"]

for col in numeric_cols:
    if col in player_data.columns:
        player_data[col] = pd.to_numeric(player_data[col], errors="coerce")


# Function to filter any team
def get_team_players(team_name, min_goals=0):
    
    team_df = player_data[player_data["team_title"] == team_name]
    
    team_df = team_df[team_df["goals"] > min_goals]
    
    return team_df


# Example: Chelsea players
chelsea_players = get_team_players("Chelsea", min_goals=1)


seasons = range(2016, 2026)   # 2016 to 2025
all_data = []


for season in seasons:
    data = understat.team(team="Chelsea").get_match_data(season=str(season))
    df = pd.DataFrame(data)
    df["season"] = season   # add season column
    all_data.append(df)

team_match_data = pd.concat(all_data, ignore_index=True)

team_match_data.head()


chelsea_players.to_csv("data/chelsea_data.csv")
team_match_data.to_csv("data/team_data.csv")




# Sample plot
top_scorers = (
    chelsea_players.groupby("player_name")["goals"]
    .sum()
    .sort_values(ascending=False)
    .head(10)
)

top_scorers.plot(kind="barh")

plt.title("Chelsea Top Premier League Scorers (2016-2025)")
plt.xlabel("Goals")
plt.ylabel("Player")
plt.gca().invert_yaxis()

plt.show()


