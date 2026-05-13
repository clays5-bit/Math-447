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

n=0
rows = []
for game in schedule_ids:
    game_data = stp.get('game', {'gamePk': game})
    allPlays = game_data["liveData"]["plays"]["allPlays"]
    for play in allPlays:
        for event in play["playEvents"]:
            if event.get("isPitch"):
                print(n)
                n += 1
                row = {
                    #in the details section
                    "description": event["details"].get('description'),
                    "code": event["details"].get('code'),
                    "isStrike": event["details"].get('isStrike'),
                    "isBall": event["details"].get('isBall'),
                    "pitchTypeCode": event["details"]['type'].get('code'),
                    "pitchTypeDescription": event["details"]['type'].get('description'),
                    "hasReview": event["details"].get('hasReview'),

                    #in the pitchData section
                    "startSpeed": event["pitchData"].get('startSpeed'),
                    "endSpeed": event["pitchData"].get('endSpeed'),
                    "strikeZoneTop": event["pitchData"].get('strikeZoneTop'),
                    "strikeZoneBottom": event["pitchData"].get('strikeZoneBottom'),
                    "strikeZoneWidth": event["pitchData"].get('strikeZoneWidth'),
                    "pX": event["pitchData"]['coordinates'].get('pX'),
                    "pZ": event["pitchData"]['coordinates'].get('pZ'),
                    "breakAngle": event["pitchData"]['breaks'].get('breakAngle'),
                    "breakLength": event["pitchData"]['breaks'].get('breakLength'),
                    "breakY": event["pitchData"]['breaks'].get('breakY'),
                    "breakVertical": event["pitchData"]['breaks'].get('breakVertical'),
                    "breakVerticalInduced": event["pitchData"]['breaks'].get('breakVerticalInduced'),
                    "breakHorizontal": event["pitchData"]['breaks'].get('breakHorizontal'),
                    "spinRate": event["pitchData"]['breaks'].get('spinRate'),
                    "spinDirection": event["pitchData"]['breaks'].get('spinDirection'),
                    'zone': event["pitchData"].get('zone'),
                    'plateTime': event["pitchData"].get('plateTime'),
                    'plateTime': event["pitchData"].get('extension'),

                    #Count section
                    "balls": event["count"].get('balls'),
                    "strikes": event["count"].get('strikes'),
                    "outs": event["count"].get('outs')
                }
                rows.append(row)

ultimateBaseballData = pd.DataFrame(rows)
print(ultimateBaseballData)