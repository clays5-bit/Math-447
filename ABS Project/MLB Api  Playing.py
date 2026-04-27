from pybaseball import statcast
from datetime import date, timedelta
import pandas as pd
import numpy as np
import statsapi as stp

mlb_2026_start_date =  "2026-03-25"

start_date = str(date.today()-timedelta(days=2)) 
end_date = str(date.today()-timedelta(days=1))

schedule = stp.schedule(start_date=mlb_2026_start_date, end_date=end_date)
schedule_df = pd.DataFrame(schedule)

schedule_ids = schedule_df['game_id'].to_list()

game_data = stp.get('game', {'gamePk': schedule_ids[0]})