#Function Set For Pulling Data

from pybaseball import statcast
import statsapi as stp

from datetime import date, timedelta
import pandas as pd
import numpy as np


def pull_pitch_statcast(data_path, start_day, end_day):
    current_data = statcast(start_dt = start_day, end_dt = end_day)

    current_data['challenge'] = current_data['des'].str.contains(r'(?=.*challenge)(?=.*pitch result)', case=False)
    current_data['overturned'] = current_data['des'].str.contains(r'(?=.*overturned)(?=.*pitch result)', case=False)

    current_data['run_1b'] = np.where(current_data['on_1b'].notna(), True, False)
    current_data['run_2b'] = np.where(current_data['on_2b'].notna(), True, False)
    current_data['run_3b'] = np.where(current_data['on_3b'].notna(), True, False)

    current_data = current_data.drop_duplicates()
    current_data.to_csv(data_path, index=False)

def add_pitch_statcast(data_path, start_day, end_day):
    current_data = pd.read_csv(data_path)
    previous_day_data = statcast(start_dt = start_day, end_dt = end_day)

    previous_day_data['challenge'] =  previous_day_data['des'].str.contains(r'(?=.*challenge)(?=.*pitch result)', case=False)
    previous_day_data['overturned'] = previous_day_data['des'].str.contains(r'(?=.*overturned)(?=.*pitch result)', case=False)

    previous_day_data['run_1b'] = np.where(current_data['on_1b'].notna(), True, False)
    previous_day_data['run_2b'] = np.where(current_data['on_2b'].notna(), True, False)
    previous_day_data['run_3b'] = np.where(current_data['on_3b'].notna(), True, False)

    current_data = pd.concat([previous_day_data, current_data], axis = 0)
    current_data = current_data.drop_duplicates()
    current_data.to_csv(data_path, index=False)

def pull_pitch_mlb_api(data_path, start_day, end_day):
    schedule = stp.schedule(start_date=start_day, end_date=end_day)
    schedule_df = pd.DataFrame(schedule)
    schedule_ids = schedule_df['game_id'].to_list()
    n=0
    rows = []
    for game in schedule_ids:
        data = stp.get('game', {'gamePk': game})
        allPlays = data["liveData"]["plays"]["allPlays"]
        awayTeamRuns = 0
        homeTeamRuns = 0

        runOn1id = None
        runOn1Name = None

        runOn2id = None
        runOn2Name = None

        runOn3id = None
        runOn3Name = None

        runOn1 = False
        runOn2 = False
        runOn3 = False

        offDate = data.get('gameData', {}).get('datetime', {}).get('officialDate')

        for officialNo in data.get('liveData').get('boxscore').get('officials'):
            if officialNo.get("officialType") == "Home Plate":
                home_umpire = officialNo.get('official', {}).get('fullName')
    
        batting_team1 = data.get('gameData').get('teams').get('away').get('name')
        batting_team2 = batting_team1

        for play in allPlays:
            if batting_team2 is not batting_team1:
                runOn1id = None
                runOn1Name = None

                runOn2id = None
                runOn2Name = None

                runOn3id = None
                runOn3Name = None

                runOn1 = False
                runOn2 = False
                runOn3 = False

            if play.get('about').get('isTopInning'):
                batting_team1 = data.get('gameData').get('teams').get('away').get('name')
                pitching_team1 = data.get('gameData').get('teams').get('home').get('name')
            else:
                batting_team1 = data.get('gameData').get('teams').get('home').get('name')
                pitching_team1 = data.get('gameData').get('teams').get('away').get('name')
        
            inning = play.get('about', {}).get('inning')
            topInning = play.get('about', {}).get('isTopInning')

            balls = 0
            strikes = 0

            for event in play["playEvents"]:
                if event.get("isPitch"): #and event.get('details', {}).get('hasReview')
                    print(n)
                    n += 1
                    row = {
                        "date": offDate,
                        "batting_team": batting_team1,
                        "batter_id": play.get('matchup', {}).get('batter', {}).get('id'),
                        "batter_name": play.get('matchup', {}).get('batter', {}).get('fullName'),

                        "pitching_team": pitching_team1,
                        "pitcher_id": play.get('matchup', {}).get('pitcher', {}).get('id'),
                        "pitcher_name": play.get('matchup', {}).get('pitcher', {}).get('fullName'),

                        #Modify to get prior count
                        #Count section
                        "balls": balls,
                        "strikes": strikes,
                        "outs": event["count"].get('outs'),
                    
                        #Modify to get prior score
                        #Score information
                        "awayTeamRuns": awayTeamRuns,
                        "homeTeamRuns": homeTeamRuns,
                        "score_diff": awayTeamRuns-homeTeamRuns,

                        "inning": inning,
                        "topInning": topInning,

                        "umpire": home_umpire,

                        #Modify to get prior base info
                        #Runners On Base Information
                        "runOn1id": runOn1id,
                        "runOn1Name": runOn1Name,

                        "runOn2id": runOn2id,
                        "runOn2Name": runOn2Name,

                        "runOn3id": runOn3id,
                        "runOn3Name": runOn3Name,

                        "runOn1": runOn1,
                        "runOn2": runOn2,
                        "runOn3": runOn3,

                        #in the details section
                        "description": event["details"].get('description'),
                        "code": event["details"].get('code'),
                        "isStrike": event["details"].get('isStrike'),
                        "isBall": event["details"].get('isBall'),
                        "pitchTypeCode": event["details"].get('type', {}).get('code'),
                        "pitchTypeDescription": event["details"].get('type', {}).get('description'),
                        "hasReview": event["details"].get('hasReview'),
                        "isOverturned": event.get('reviewDetails', {}).get('isOverturned'),

                        #in the pitchData section
                        "startSpeed": event["pitchData"].get('startSpeed'),
                        "endSpeed": event["pitchData"].get('endSpeed'),

                        "strikeZoneTop": event["pitchData"].get('strikeZoneTop'),
                        "strikeZoneBottom": event["pitchData"].get('strikeZoneBottom'),
                        "strikeZoneWidth": event["pitchData"].get('strikeZoneWidth'),

                        "pX": event["pitchData"].get('coordinates', {}).get('pX'),
                        "pZ": event["pitchData"].get('coordinates', {}).get('pZ'),

                        "breakAngle": event["pitchData"].get('breaks', {}).get('breakAngle'),
                        "breakLength": event["pitchData"].get('breaks', {}).get('breakLength'),
                        "breakY": event["pitchData"].get('breaks', {}).get('breakY'),
                        "breakVertical": event["pitchData"].get('breaks', {}).get('breakVertical'),
                        "breakVerticalInduced": event["pitchData"].get('breaks', {}).get('breakVerticalInduced'),
                        "breakHorizontal": event["pitchData"].get('breaks', {}).get('breakHorizontal'),

                        "spinRate": event["pitchData"].get('breaks', {}).get('spinRate'),
                        "spinDirection": event["pitchData"].get('breaks', {}).get('spinDirection'),

                        'zone': event["pitchData"].get('zone'),
                        'plateTime': event["pitchData"].get('plateTime'),
                        'plateTime': event["pitchData"].get('extension')
                    }
                    rows.append(row)
                    balls = event["count"].get('balls')
                    strikes = event["count"].get('strikes')
                    balls = balls
                    strikes = strikes

                    awayTeamRuns = play.get('result', {}).get('awayScore')
                    homeTeamRuns = play.get('result', {}).get('homeScore')
    
            #Stores prior runners on base
            runOn1id = play.get('matchup', {}).get('postOnFirst', {}).get('id')
            runOn1Name = play.get('matchup', {}).get('postOnFirst', {}).get('fullName')

            runOn2id = play.get('matchup', {}).get('postOnSecond', {}).get('id')
            runOn2Name = play.get('matchup', {}).get('postOnSecond', {}).get('fullName')

            runOn3id = play.get('matchup', {}).get('postOnThird', {}).get('id')
            runOn3Name = play.get('matchup', {}).get('postOnThird', {}).get('fullName')

            runOn1 = play.get('matchup', {}).get('postOnFirst', {}).get('id') is not None
            runOn2 = play.get('matchup', {}).get('postOnSecond', {}).get('id') is not None
            runOn3 = play.get('matchup', {}).get('postOnThird', {}).get('id') is not None
            batting_team2 = batting_team1

    ultimateBaseballData = pd.DataFrame(rows)
    ultimateBaseballData.to_csv(data_path)
    return ultimateBaseballData
    
def update_pitch_file_mlb_api(data_path):
    end_day = str(date.today()-timedelta(days=1))
    
    old_data = pd.read_csv(data_path)

    last_day = date.strptime(old_data['date'].iloc[-1], '%m/%d/%Y')
    start_day = str(last_day+timedelta(days=1))

    new_data = pull_pitch_mlb_api(data_path, start_day, end_day)
    updated_data = pd.concat([old_data, new_data])

    updated_data.to_csv(data_path)
    return updated_data