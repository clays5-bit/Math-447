from pybaseball import statcast
from datetime import date, timedelta
import pandas as pd
import numpy as np
import statsapi as stp
import json

mlb_2026_start_date = "2026-03-25"

start_date = str(date.today()-timedelta(days=2)) 
end_date = str(date.today()-timedelta(days=1))

schedule = stp.schedule(start_date=start_date, end_date=end_date)
#schedule = stp.schedule(start_date=mlb_2026_start_date, end_date=end_date)
schedule_df = pd.DataFrame(schedule)

schedule_ids = schedule_df['game_id'].to_list()

n=0
rows = []
data = stp.get('game', {'gamePk': schedule_ids[0]})
allPlays = data["liveData"]["plays"]["allPlays"]

for play in allPlays:
    for event in play["playEvents"]:
        if event.get('isPitch') and event.get('details', {}).get('hasReview'):
            n += 1
            print(n)
            print(event.get('reviewDetails', {}).keys())
            print(event.get('details', {}).get('hasReview'))
            print(event.get('reviewDetails', {}).get('isOverturned'))
            print(event.get('reviewDetails', {}).get('player'))
            print(event.get('reviewDetails', {}).get('reviewType'))