#Baseball Savant Data PUller

from pybaseball import statcast
from datetime import date, timedelta
import pandas as pd
import Baseball_Funcs as bf
import statsapi as stp

#Desktop File Paths
data_path_desk1 = "C:/Users/Sean's Desktop/Documents/GitHub/pitch_data_current.csv"
data_path_desk2 = "C:/Users/Sean's Desktop/Documents/GitHub/pitch_data_test.csv"

#Laptop File Paths
data_path_lap1 = "C:/Users/clays/Documents/GitHub/pitch_data_current.csv"
data_path_lap2 = "C:/Users/clays/Documents/GitHub/pitch_data_test.csv"

mlb_2026_start_date =  "2026-03-25"

start_date = str(date.today()-timedelta(days=2))
end_date = str(date.today()-timedelta(days=1))

bf.pull_pitch_mlb_api(data_path_desk1, mlb_2026_start_date, end_date)
#bf.update_pitch_file_mlb_api(data_path_lap2)