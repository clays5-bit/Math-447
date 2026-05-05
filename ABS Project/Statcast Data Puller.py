#Baseball Savant Data PUller

from pybaseball import statcast
from datetime import date, timedelta
import pandas as pd
import Baseball_Funcs as bf
import statsapi as stp

data_path = "C:/Users/Sean's Desktop/Documents/GitHub/Math-447/pitch_data_current.csv"
mlb_2026_start_date =  "2026-03-25"

start_date = str(date.today()-timedelta(days=2)) 
end_date = str(date.today()-timedelta(days=1))

bf.pull_full_pitch(data_path, mlb_2026_start_date, end_date)
#bf.pull_yesterday_pitch(data_path, start_date, end_date)