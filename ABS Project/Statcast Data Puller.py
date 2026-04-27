#Baseball Savant Data PUller

from pybaseball import statcast
from datetime import date, timedelta
import pandas as pd

data_path = 'C:/Users/clays/Documents/GitHub/Math-447/ABS Project/pitch_data_current.csv'

start_date = '2026-03-25' 
end_date = str(date.today()-timedelta(days=1))

pitch_data_26 = statcast(start_dt = start_date, end_dt = end_date)
pitch_data_26.to_csv(data_path, index=False)