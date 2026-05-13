#Baseball Savant Data PUller

from pybaseball import statcast
from datetime import date, timedelta
import pandas as pd
import Baseball_Funcs as bf
import statsapi as stp

data_path1 = "C:/Users/Sean's Desktop/Documents/GitHub/pitch_data_current.csv"
data_path2 = "C:/Users/Sean's Desktop/Documents/GitHub/pitch_data_test.csv"
mlb_2026_start_date =  "2026-03-25"

start_date = str(date.today()-timedelta(days=2)) 
end_date = str(date.today()-timedelta(days=1))

bf.update_pitch_file_mlb_api(data_path1)

