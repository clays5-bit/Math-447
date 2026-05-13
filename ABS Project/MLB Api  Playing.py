from datetime import date, timedelta
import pandas as pd
import statsapi as stp
import json
import requests
import numpy as np

file_name = "game_data_1.csv"
mlb_2026_start_date =  "2026-03-25"

start_date = str(date.today()-timedelta(days=2)) 
end_date = str(date.today()-timedelta(days=1))

schedule = stp.schedule(start_date=start_date, end_date=end_date)
schedule_df = pd.DataFrame(schedule)
schedule_ids = schedule_df['game_id'].to_list()

game_data = stp.get('game', {'gamePk': schedule_ids[0]})
all_plays = game_data['liveData']['plays']['allPlays']

for play in all_plays:
    for event in play['playEvents']:
        if 'pitchData' in event and not 'hitData' in event:
            print(event['details'].keys())
            #print(event['count'].keys())
            #print(event['pitchData'].keys())
