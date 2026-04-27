#Function Set For Pulling Data

from pybaseball import statcast
from datetime import date, timedelta
import pandas as pd
import numpy as np
import statsapi as stp

def pull_full_pitch(data_path, mlb_season_start, last_day):
    current_data = statcast(start_dt = mlb_season_start, end_dt = last_day)

    current_data['challenge'] =  current_data['des'].str.contains(r'(?=.*challenge)(?=.*pitch result)', case=False)
    current_data['overturned'] = current_data['des'].str.contains(r'(?=.*overturned)(?=.*pitch result)', case=False)

    current_data['run_1b'] = np.where(current_data['on_1b'].notna(), True, False)
    current_data['run_2b'] = np.where(current_data['on_2b'].notna(), True, False)
    current_data['run_3b'] = np.where(current_data['on_3b'].notna(), True, False)

    current_data = current_data.drop_duplicates()
    current_data.to_csv(data_path, index=False)

def pull_yesterday_pitch(data_path, from_day, last_day):
    current_data = pd.read_csv(data_path)
    previous_day_data = statcast(start_dt = from_day, end_dt = last_day)

    previous_day_data['challenge'] =  previous_day_data['des'].str.contains(r'(?=.*challenge)(?=.*pitch result)', case=False)
    previous_day_data['overturned'] = previous_day_data['des'].str.contains(r'(?=.*overturned)(?=.*pitch result)', case=False)

    previous_day_data['run_1b'] = np.where(current_data['on_1b'].notna(), True, False)
    previous_day_data['run_2b'] = np.where(current_data['on_2b'].notna(), True, False)
    previous_day_data['run_3b'] = np.where(current_data['on_3b'].notna(), True, False)

    current_data = pd.concat([previous_day_data, current_data], axis = 0)
    current_data = current_data.drop_duplicates()
    current_data.to_csv(data_path, index=False)