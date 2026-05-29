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
rows = []
data = stp.get('game', {'gamePk': schedule_ids[10]})

#print(data.keys())
#print(data.get('gameData', {}).get('teams',{}).get('away',{}).get('name',{}))
#print(data.get('gameData', {}).get('teams',{}).get('home',{}).get('name',{}))

allPlays = data["liveData"]["plays"]["allPlays"]




for play in allPlays:
    print(play.get('reviewDetails'),{})
    for event in play["playEvents"]:
        if(event.get('details', {}).get('hasReview')): 
            n += 1
            print(n)
            print(event.get('isPitch', {}))
            #print(event.get('player',{}))
            print(event.get('count', {}))
            print(event.get('details',{}))
            #print(event.get('reviewDetails', {}).keys())
            #print(event.get('reviewDetails', {}).get('challengeTeamId'))
            #print(event.get('reviewDetails', {}).get('reviewType'))
            #print(event.get('details', {}).get('hasReview'))
            #print(event.get('reviewDetails', {}).get('isOverturned'))
            #print(event.get('reviewDetails', {}).get('player', {}).get('id'))