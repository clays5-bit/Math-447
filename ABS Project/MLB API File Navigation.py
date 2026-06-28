from pybaseball import statcast
from datetime import date, timedelta, datetime
from zoneinfo import ZoneInfo
import pandas as pd
import statsapi as stp
import json

#mlb_2026_start_date = "2026-03-25"

#start_date = "2026-06-26"
#end_date = start_date

start_date = str(date.today()) 
end_date = start_date

schedule = stp.schedule(start_date=start_date, end_date=end_date)
#schedule = stp.schedule(start_date=mlb_2026_start_date, end_date=end_date)
#schedule_df = pd.DataFrame(schedule)

player_query_str = 'Jacob Misiorowski'
player_data = stp.lookup_player(player_query_str)
player_team = player_data[0].get('currentTeam', {}).get('id', {})

for game in schedule:
    away_id = game.get('away_id', {})
    home_id = game.get('home_id', {})
    if away_id == player_team or home_id == player_team:
        away_probable_pitcher = game.get('away_probable_pitcher', {})
        home_probable_pitcher = game.get('home_probable_pitcher', {})
        
        if away_probable_pitcher == player_query_str or home_probable_pitcher == player_query_str:
            away_team = game.get('away_name', {})
            home_team = game.get('home_name', {})
            game_datetime = game.get('game_datetime', {})

            #from gemini
            utc_dt = datetime.strptime(game_datetime, "%Y-%m-%dT%H:%M:%SZ").replace(tzinfo=ZoneInfo("UTC"))
            local_dt = utc_dt.astimezone()
            formatted_time = local_dt.strftime("%I:%M %p %Z")

            print(f'{player_query_str} is playing today!')
            print(f'{away_team} vs. {home_team}')
            print(f'{formatted_time}')
            break


#schedule_ids = schedule_df['game_id'].to_list()
#print(schedule_ids)
#n=0
#m=0
#rows = []
#data = stp.get('game', {'gamePk': schedule_ids[5]})

#print(data.keys())
#print(data.get('gameData', {}).get('teams',{}).get('away',{}).get('name',{}))
#print(data.get('gameData', {}).get('teams',{}).get('home',{}).get('name',{}))

#allPlays = data["liveData"]["plays"]["allPlays"]
#print(type(allPlays))
#print(allPlays.keys())

#data = stp.get('game', {'gamePk': schedule_ids[5]})
#play = data["liveData"]["plays"]["allPlays"][2]  # first at bat
#for event in play["playEvents"]:
#    if event.get("isPitch"):
#        print(event.get("count"))
#        print(event["details"].get("description"))
#        print("---")