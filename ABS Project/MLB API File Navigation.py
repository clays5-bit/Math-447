from pybaseball import statcast
from datetime import date, timedelta
import pandas as pd
import numpy as np
import statsapi as stp
import json

mlb_2026_start_date = "2026-03-25"

start_date = "2026-04-18" #str(date.today()-timedelta(days=2)) 
end_date = "2026-04-18" #str(date.today()-timedelta(days=1))

schedule = stp.schedule(start_date=start_date, end_date=end_date)
#schedule = stp.schedule(start_date=mlb_2026_start_date, end_date=end_date)
schedule_df = pd.DataFrame(schedule)

schedule_ids = schedule_df['game_id'].to_list()
#print(schedule_ids)
n=0
m=0
rows = []
data = stp.get('game', {'gamePk': schedule_ids[10]})

#print(data.keys())
#print(data.get('gameData', {}).get('teams',{}).get('away',{}).get('name',{}))
#print(data.get('gameData', {}).get('teams',{}).get('home',{}).get('name',{}))

allPlays = data["liveData"]["plays"]["allPlays"]
print(data["liveData"]['plays'].keys())
#print(allPlays.get('decisions', {}))



for play in allPlays:
    print(play.get('reviewDetails', {}))
    for event in play["playEvents"]:
        if(event.get('isSubstitution', {})):
            n += 1
            print(n)
            print(event.keys())
            print('Substitution')
            print(event.get('details', {}))
        elif(event.get('isPitch', {})):
            n += 1
            print(n)
            print('Pitch')
            print(event.get('details', {}))