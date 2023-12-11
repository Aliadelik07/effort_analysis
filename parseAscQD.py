#!/usr/bin/env python
# -*- coding: utf-8 -*-
#-------------------------------------------------------------------------------
# Functions for parsing Eyelink ascii file and extracting gaze events synchronized with scanner onset (ONSETS_MS and OFFSET_MS columns)
# Created By  : F. Geringswald
# Created Date: 2022-05-23
# ------------------------------------------------------------------------------
# TODOs : check for lost messages at beginning, assign gaze events to trials and extract summary stats per trial / phase, file and folder structure, clean up file names for use with args, check BIDS specifications, add logger, etc etc

# import modules
import pandas as pd
import numpy as np
from pathlib import Path
from sklearn.linear_model import LinearRegression
# import matplotlib.pyplot as plt
# plt.ion()

############
# messages #
############

def parse_messages(eye_dat, tsv):
    MSG = [line.split() for line in eye_dat if not line[0].isdigit() and 'MSG' in line]
    # select messages between begin and end
    marker_begin = tsv.iloc[0]['CONDITIONS']
    marker_end = tsv.iloc[-1]['CONDITIONS']
    #idx = [i for i, j in enumerate(MSG) if marker_begin in j or marker_end in j] # does not work for localizer
    idx = [i for i, j in enumerate(MSG) if marker_begin in j and len(j) == 3 or marker_end in j and len(j) == 3]
    # event messages
    headers = ['label', 'timestamp', 'CONDITIONS']
    MSG_events = pd.DataFrame(MSG[idx[0]:idx[-1]+1], columns = headers)
    #dtypes = ['str', 'int', 'str']
    MSG_events['timestamp'] = MSG_events['timestamp'].apply(pd.to_numeric, errors='coerce')
    keep = ['CONDITIONS', 'timestamp']
    MSG_events = MSG_events[keep]
    # check if all events parsed from ET correspond with tsv events
    if not MSG_events['CONDITIONS'].equals(tsv['CONDITIONS']):
        raise ValueError('Could not parse event markers from ascii file')
    
    return MSG, MSG_events

##########
# blinks #
##########

def parse_blinks(eye_dat):
    BLI = [line.split() for line in eye_dat if not line[0].isdigit() and 'EBLINK' in line]
    # label eye  start stop duration
    headers = ['label', 'eye', 'start', 'stop', 'duration']
    #dtypes = ['str', 'str', 'int', 'int', 'int']
    BLI = pd.DataFrame(BLI, columns = headers)
    # keep
    keep = ['start', 'stop', 'duration']
    BLI = BLI[keep].apply(pd.to_numeric, errors='coerce')
    
    return BLI

############
# saccades #
############

def parse_saccades(eye_dat):
    SAC = [line.split() for line in eye_dat if not line[0].isdigit() and 'ESACC' in line]
    # label eye  start stop duration x_start y_start x_end y_end amplitude peak_velocity  
    headers = ['label', 'eye', 'start', 'stop', 'duration', 'x_start', 'y_start', 'x_end', 'y_end', 'amplitude', 'peak_velocity']
    #dtypes = ['str', 'str', 'int', 'int', 'int', 'float', 'float', 'float', 'float', 'float', 'int']
    SAC = pd.DataFrame(SAC, columns = headers)
    # keep
    keep = ['start', 'stop', 'duration', 'x_start', 'y_start', 'x_end', 'y_end', 'amplitude', 'peak_velocity']
    SAC = SAC[keep].apply(pd.to_numeric, errors='coerce')
    # calculate velocity angle_deg direction
    # velocity = amplitude / (duration / 1000)
    SAC['velocity'] = SAC['amplitude'] / (SAC['duration']/1000)
    # angle: deltaY = P2_y - P1_y; deltaX = P2_x - P1_x;
    # angleInDegrees = atan2(deltaY, deltaX) * 180 / PI
    SAC['angle_deg'] = np.arctan2(SAC['y_end'] - SAC['y_start'], SAC['x_end'] - SAC['x_start']) * 180/np.pi
    # direction
    # http://gamedev.stackexchange.com/questions/49290/whats-the-best-way-of-transforming-a-2d-vector-into-the-closest-8-way-compass-d
    directions = np.array(['right', 'up', 'left', 'down'])#['R', 'U', 'L', 'D'])
    SAC['direction'] = 'nan'
    SAC.loc[np.isnan(SAC['angle_deg']) == False, 'direction'] = directions[np.mod(np.round(4 * SAC['angle_deg'][np.isnan(SAC['angle_deg']) == False] / 360 + 4), 4).astype(int)] # direction according to eyelink manual
    
    # label pseudo-saccades that occurred before or after blink
    
    return SAC

#############
# fixations #
#############

def parse_fixations(eye_dat):
    FIX = [line.split() for line in eye_dat if not line[0].isdigit() and 'EFIX' in line]
    # label eye start stop duration x_mean y_mean pupil
    headers = ['label', 'eye', 'start', 'stop', 'duration', 'x_mean', 'y_mean', 'pupil']
    #dtypes = ['str', 'str', 'int', 'int', 'int', 'float', 'float', 'int']
    FIX = pd.DataFrame(FIX, columns = headers)
    # keep
    keep = ['start', 'stop', 'duration', 'x_mean', 'y_mean', 'pupil']
    FIX = FIX[keep].apply(pd.to_numeric, errors='coerce')
    
    return FIX

############
# raw data #
############

def parse_data(eye_dat):
    DATA = [line.split() for line in eye_dat if line[0].isdigit()]
    # timestamp x y pupil misc
    headers = ['timestamp', 'x', 'y', 'pupil', 'misc']
    #dtypes = ['int', 'float', 'float', 'float',  'str']
    DATA = pd.DataFrame(DATA, columns = headers)
    # keep
    keep = ['timestamp', 'x', 'y', 'pupil']
    DATA = DATA[keep].apply(pd.to_numeric, errors='coerce')
    
    return DATA


############################################################
# index 'saccades' that enclose blinks (not real saccades) #
############################################################

def check_saccades(SAC, BLI):
    
    # reshape
    start_blink = np.tile(BLI['start'], (SAC.shape[0], 1))
    start_sac = np.tile(SAC['start'], (BLI.shape[0], 1)).transpose()
    end_blink = np.tile(BLI['stop'], (SAC.shape[0], 1))
    end_sac = np.tile(SAC['stop'], (BLI.shape[0], 1)).transpose()
    
    # index of 'saccades' that enclose blinks
    idx = np.nonzero((start_sac < start_blink) & (end_sac > end_blink))[0]
    
    blink_sac = np.zeros(SAC.shape[0], dtype = int)
    blink_sac[idx] = 1
    
    return blink_sac


#########################################################
# code valid gaze events according to common parameters #
#########################################################

def filter_gaze(FIX, SAC, BLI):
    
    # fixations: duration >= 100 ms
    FIX['valid_event'] = 1
    FIX.loc[FIX['duration'] < 100, 'valid_event'] = 0
    
    # saccades: duration >= 10 ms & amplitude > 0 deg VA & amplitude < 100 deg VA
    SAC['valid_event'] = 1
    SAC.loc[(SAC['duration'] < 10) | (SAC['amplitude'] <= 0) | (SAC['amplitude'] > 100), 'valid_event'] = 0
    
    # blinks: duration >= 100 ms & duration <= 500 ms
    BLI['valid_event'] = 1
    BLI.loc[(BLI['duration'] < 100) | (BLI['duration'] > 500), 'valid_event'] = 0
    
    return FIX, SAC, BLI


####################################################################
# recode eyedate time stamps to be in sync with scanner timestamps #
####################################################################

# linear model to predict eye log time stamps from tsv timestamps
def makeTimestampMapper(x, y):
    
    mapper = LinearRegression()
    mapper.fit(x, y)
    
    return mapper


# apply mapping
def mapTimestamps(timeStamps, mapper):
    
    predictions = mapper.predict(timeStamps)
    predictions = np.array(np.round(predictions).flatten(), dtype=int)
    
    return predictions





# run
if __name__ == "__main__":
    
    # data paths
    bids_dir = Path('/home/geringswald/projects/johannes/PREDYS/fmri_pilot/PREDYS')
    
    sub = 'sub-pilote'
    task = 'StatisticalLearning'
    run = 'run-01'
    seq = 'seq1'
    timestamp = '22_05_16_12_20'
    
    #task = 'ArchiLocalizer'
    #timestamp = '22_05_16_12_40'
    
    # ascii file in eyetrack folder
    #asc_file = bids_dir.joinpath('{sub}/{ses}/eyetrack/{sub}_task-{task}_{run}_{timestamp}_eyetrack.asc'.format(sub=sub, ses=ses, task=task, run=run, timestamp=timestamp))
    if task == 'ArchiLocalizer':
        asc_file = bids_dir.joinpath('sourcedata/{sub}/{sub}_task-{task}_{timestamp}.asc'.format(sub=sub, task=task, seq=seq, run=run, timestamp=timestamp))
    else:
        asc_file = bids_dir.joinpath('sourcedata/{sub}/{sub}_task-{task}-{seq}_{run}_{timestamp}.asc'.format(sub=sub, task=task, seq=seq, run=run, timestamp=timestamp))
    # input tsv to load and modify in func folder (we don't have the tsv files, use txt instead)
    tsv_file = asc_file.with_suffix('.txt')
    # output tsv to create in eyetrack folder under 'derivatives'
    output_eyetrack = bids_dir.joinpath('derivatives/eyetrack/{sub}'.format(sub=sub))
    output_eyetrack.mkdir(exist_ok=True, parents=True)
    output_binks = output_eyetrack.joinpath(asc_file.name.replace('{}.asc'.format(timestamp), 'blinks.tsv'))
    output_fixations = output_eyetrack.joinpath(asc_file.name.replace('{}.asc'.format(timestamp), 'fixations.tsv'))
    output_saccades = output_eyetrack.joinpath(asc_file.name.replace('{}.asc'.format(timestamp), 'saccades.tsv'))
    output_events = output_eyetrack.joinpath(asc_file.name.replace('{}.asc'.format(timestamp), 'events.tsv'))
    
    # load asc file
    with open(asc_file, 'r') as f:
        eye_dat = f.readlines()
    
    # load tsv
    if task == 'StatisticalLearning':
        tsv = pd.read_csv(tsv_file, header = 0, sep='\t')
    else:
        tsv = pd.read_csv(tsv_file, header = 0, sep='\t', encoding = 'iso-8859-15')
    
    # convert 'CONDITIONS' to string because of localizer coding
    tsv['CONDITIONS'] = tsv['CONDITIONS'].astype(str)
    
    # parse ascii
    MSG, MSG_events = parse_messages(eye_dat, tsv)
    BLI = parse_blinks(eye_dat)
    SAC = parse_saccades(eye_dat)
    FIX = parse_fixations(eye_dat)
    #DATA = parse_data(eye_dat)
    
    # exclude invalid 'saccades' that enclose blinks (not real saccades)
    blink_sac = check_saccades(SAC, BLI)
    SAC = SAC.iloc[blink_sac==0]
    
    # map timestamps
    mapper = makeTimestampMapper(np.array(MSG_events['timestamp'], dtype=float).reshape(-1, 1),  np.array(tsv['ONSETS_MS'], dtype=float).reshape(-1, 1))
    
    MSG_events['ONSETS_MS'] = mapTimestamps(np.array(MSG_events['timestamp'], dtype=float).reshape(-1, 1), mapper)
    BLI['ONSETS_MS'] = mapTimestamps(np.array(BLI['start'], dtype=float).reshape(-1, 1), mapper)
    BLI['OFFSETS_MS'] = mapTimestamps(np.array(BLI['stop'], dtype=float).reshape(-1, 1), mapper)
    SAC['ONSETS_MS'] = mapTimestamps(np.array(SAC['start'], dtype=float).reshape(-1, 1), mapper)
    SAC['OFFSETS_MS'] = mapTimestamps(np.array(SAC['stop'], dtype=float).reshape(-1, 1), mapper)
    FIX['ONSETS_MS'] = mapTimestamps(np.array(FIX['start'], dtype=float).reshape(-1, 1), mapper)
    FIX['OFFSETS_MS'] = mapTimestamps(np.array(FIX['stop'], dtype=float).reshape(-1, 1), mapper)
    #DATA['ONSETS_MS'] = mapTimestamps(np.array(DATA['timestamp'], dtype=float).reshape(-1, 1), mapper)
    
    # code valid gaz events according to common parameters
    FIX, SAC, BLI = filter_gaze(FIX, SAC, BLI)
    
    
    event_filter = MSG_events['CONDITIONS'].str.split('_',expand=True)
    idx_start = np.where(np.arange(event_filter.shape[0]) % 25 == 0)[0][0:-1] + 1
    
    onsets = MSG_events.loc[idx_start,:]
    onsets['trial'] = np.arange(onsets.shape[0]) + 1
    
    stim_order = []
    for a,b in zip(idx_start, idx_start+23):
        #print(tsv['STIM_BITMAP'].loc[a:b])
        stim_order.append('-'.join([item for item in tsv['STIM_BITMAP'].loc[a:b]]))
    
    onsets['stim_order'] = stim_order
    
    offsets = MSG_events.loc[idx_start+24,:]
    
    FIX['condition'] = ''
    FIX['stim_order'] = ''
    FIX['trial'] = 0
    
    SAC['condition'] = ''
    SAC['stim_order'] = ''
    SAC['trial'] = 0
    
    counter = 0
    for index, row in onsets.iterrows():
        #print(row)
        FIX.loc[(FIX['start'] > row['timestamp']) & (FIX['start'] < offsets.iloc[counter]['timestamp']), 'condition'] = row.loc['CONDITIONS']
        FIX.loc[(FIX['start'] > row['timestamp']) & (FIX['start'] < offsets.iloc[counter]['timestamp']), 'stim_order'] = row.loc['stim_order']
        FIX.loc[(FIX['start'] > row['timestamp']) & (FIX['start'] < offsets.iloc[counter]['timestamp']),'trial'] = row.loc['trial']
        
        SAC.loc[(SAC['start'] > row['timestamp']) & (SAC['start'] < offsets.iloc[counter]['timestamp']), 'condition'] = row.loc['CONDITIONS']
        SAC.loc[(SAC['start'] > row['timestamp']) & (SAC['start'] < offsets.iloc[counter]['timestamp']), 'stim_order'] = row.loc['stim_order']
        SAC.loc[(SAC['start'] > row['timestamp']) & (SAC['start'] < offsets.iloc[counter]['timestamp']),'trial'] = row.loc['trial']
        
        counter = counter + 1
    
    
    
    
    
    
    # write output
    BLI.to_csv(output_binks, sep='\t', float_format='%.4f', index=False)
    SAC.to_csv(output_saccades, sep='\t', float_format='%.4f', index=False)
    FIX.to_csv(output_fixations, sep='\t', float_format='%.4f', index=False)
    MSG_events.to_csv(output_events, sep='\t', float_format='%.4f', index=False)
