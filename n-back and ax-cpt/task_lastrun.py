#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
This experiment was created using PsychoPy3 Experiment Builder (v2022.2.5),
    on octobre 06, 2023, at 17:55
If you publish work using this script the most relevant publication is:

    Peirce J, Gray JR, Simpson S, MacAskill M, Höchenberger R, Sogo H, Kastman E, Lindeløv JK. (2019) 
        PsychoPy2: Experiments in behavior made easy Behav Res 51: 195. 
        https://doi.org/10.3758/s13428-018-01193-y

"""

# --- Import packages ---
from psychopy import locale_setup
from psychopy import prefs
from psychopy import sound, gui, visual, core, data, event, logging, clock, colors, layout, iohub, hardware
from psychopy.constants import (NOT_STARTED, STARTED, PLAYING, PAUSED,
                                STOPPED, FINISHED, PRESSED, RELEASED, FOREVER)

import numpy as np  # whole numpy lib is available, prepend 'np.'
from numpy import (sin, cos, tan, log, log10, pi, average,
                   sqrt, std, deg2rad, rad2deg, linspace, asarray)
from numpy.random import random, randint, normal, shuffle, choice as randchoice
import os  # handy system and path functions
import sys  # to get file system encoding

import psychopy.iohub as io
from psychopy.hardware import keyboard



# Ensure that relative paths start from the same directory as this script
_thisDir = os.path.dirname(os.path.abspath(__file__))
os.chdir(_thisDir)
# Store info about the experiment session
psychopyVersion = '2022.2.5'
expName = 'ADOEFFORT'  # from the Builder filename that created this script
expInfo = {
    'participant': 'Sub',
    'record': ["0","1"],
    'version': ["train","test"],
    'task': ["n-back","ax-cpt"],
}
# --- Show participant info dialog --
dlg = gui.DlgFromDict(dictionary=expInfo, sortKeys=False, title=expName)
if dlg.OK == False:
    core.quit()  # user pressed cancel
expInfo['date'] = data.getDateStr()  # add a simple timestamp
expInfo['expName'] = expName
expInfo['psychopyVersion'] = psychopyVersion

# Data file name stem = absolute path + name; later add .psyexp, .csv, .log, etc
filename = _thisDir + os.sep + u'data/%s_%s_%s' % (expInfo['participant'], expName, expInfo['date'])

# An ExperimentHandler isn't essential but helps with data saving
thisExp = data.ExperimentHandler(name=expName, version='',
    extraInfo=expInfo, runtimeInfo=None,
    originPath='C:\\Users\\effort cognitif\\n-back and ax-cpt\\task_lastrun.py',
    savePickle=True, saveWideText=True,
    dataFileName=filename)
# save a log file for detail verbose info
logFile = logging.LogFile(filename+'.log', level=logging.EXP)
logging.console.setLevel(logging.WARNING)  # this outputs to the screen, not a file
frameTolerance = 0.001  # how close to onset before 'same' frame

# Start Code - component code to be run after the window creation

# --- Setup the Window ---
win = visual.Window(
    size=[1920, 1080], fullscr=True, screen=1, 
    winType='pyglet', allowStencil=False,
    monitor='Monitor', color='0.0039, 0.0039, 0.0039', colorSpace='rgb',
    blendMode='avg', useFBO=True, 
    units='height')
win.mouseVisible = False
# store frame rate of monitor if we can measure it
expInfo['frameRate'] = win.getActualFrameRate()
if expInfo['frameRate'] != None:
    frameDur = 1.0 / round(expInfo['frameRate'])
else:
    frameDur = 1.0 / 60.0  # could not measure, so guess
# --- Setup input devices ---
ioConfig = {}

# Setup eyetracking
ioConfig['eyetracker.hw.sr_research.eyelink.EyeTracker'] = {
    'name': 'tracker',
    'model_name': 'EYELINK 1000 DESKTOP',
    'simulation_mode': False,
    'network_settings': '100.1.1.1',
    'default_native_data_file_name': 'EXPFILE',
    'runtime_settings': {
        'sampling_rate': 500.0,
        'track_eyes': 'LEFT_EYE',
        'sample_filtering': {
            'sample_filtering': 'FILTER_LEVEL_2',
            'elLiveFiltering': 'FILTER_LEVEL_OFF',
        },
        'vog_settings': {
            'pupil_measure_types': 'PUPIL_DIAMETER',
            'tracking_mode': 'PUPIL_CR_TRACKING',
            'pupil_center_algorithm': 'ELLIPSE_FIT',
        }
    }
}

# Setup iohub keyboard
ioConfig['Keyboard'] = dict(use_keymap='psychopy')

ioSession = '1'
if 'session' in expInfo:
    ioSession = str(expInfo['session'])
ioServer = io.launchHubServer(window=win, experiment_code='ADOEFFORT', session_code=ioSession, datastore_name=filename, **ioConfig)
eyetracker = ioServer.getDevice('tracker')

# create a default keyboard (e.g. to check for escape)
defaultKeyboard = keyboard.Keyboard(backend='iohub')

# --- Initialize components for Routine "FX" ---
the_corss = visual.TextStim(win=win, name='the_corss',
    text='+',
    font='Arial',
    pos=(0, 0), height=0.05, wrapWidth=None, ori=0.0, 
    color='black', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=0.0);

# --- Initialize components for Routine "block_info" ---
text_info = visual.TextStim(win=win, name='text_info',
    text='',
    font='Arial',
    pos=(0, 0), height=0.03, wrapWidth=None, ori=0.0, 
    color='black', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=0.0);
# Run 'Begin Experiment' code from code_init
same = 0
trial = 0
score = 0
n_miss = 0
n_error = 0
n_pass = 0
n_hit = 0
fd = 0
block = 0
n_block = 0
fd_text = ""
calibing = 1

stimuli_mouse = event.Mouse()
stimuli_mouse.setVisible(visible=0)

record = int(expInfo['record'])
if record == 1:
    ioConfig['eyetracker.hw.sr_research.eyelink.EyeTracker']['default_native_data_file_name'] = expInfo['participant']
    from psychopy import parallel
    parallel.setPortAddress(0x0000BFE8)
    parallel.setData(0)
    eyetracker.setConnectionState(True)
    eyetracker.setRecordingState(True)
    def send_trigger(trigger):
        global parallel
        global eyetracker
        eyetracker.sendMessage(str(trigger))
        parallel.setData(trigger)
        core.wait(0.002)
        parallel.setData(0)


if expInfo['task'] == "n-back":
    def check(x,y):
        global same
        global fd
        global score
        global n_hit
        global n_error
        global n_miss
        global n_pass
        global trial
        if x == y:
            if same == 1:
                score += 1
                n_hit += 1
                fd = 13
            else:
                n_miss += 1
                fd = 11
        else:
            if same==1:
                score -= 1
                n_error += 1
                fd=12
            else:
                n_pass += 1
                fd = 9



    if expInfo['version'] == "train":
        #train letters
        Letters = [['n', 'n', 'c', 'n', 'm', 'm', 'u', 'u', 'c', 'c', 'n', 'n', 'u', 'u', 'n', 'm', 'm', 's', 'c', 'n', 'n', 'c', 'n', 'n', 's', 'm', 'm', 's', 'n', 'c', 'c', 's', 'm', 'c', 'm', 'm', 's', 'm', 'c', 'u', 'u', 'c', 'c', 'n', 'c', 'c', 'n', 'u', 'u', 'c', 'c', 'n', 'n', 'u', 'u', 'c', 'c', 'u', 'c', 'm'] ,['m', 'n', 'u', 'c', 's', 'n', 'm', 'n', 'm', 'n', 'c', 's', 'c', 's', 'c', 'n', 'c', 'n', 'm', 's', 'n', 'c', 'm', 'u', 'n', 'c', 'm', 'u', 'm', 'n', 'c', 'n', 'u', 's', 'u', 'm', 'u', 'c', 'c', 'm', 'n', 'm', 'u', 'm', 'm', 'u', 'n', 'c', 'm', 's', 'm', 'u', 'm', 'u', 'n', 'm', 'c', 'm', 'c', 'm'] ,['u', 'm', 'n', 'm', 'm', 'n', 'u', 'n', 'u', 'u', 'm', 'n', 'm', 'n', 'c', 'n', 'c', 'u', 'c', 'n', 'u', 'm', 'u', 'n', 'u', 'm', 'n', 'u', 'm', 'n', 'c', 'm', 'n', 'c', 's', 'c', 'm', 'u', 'n', 'c', 'n', 'm', 'n', 'c', 'n', 'c', 'u', 'c', 'u', 's', 'u', 'm', 'u', 'm', 'c', 'c', 'n', 'm', 'm', 'n'] ,['n', 'm', 'n', 'u', 'm', 'n', 'n', 'c', 'm', 'n', 's', 'n', 'u', 'n', 'c', 'u', 's', 'u', 'n', 'n', 'u', 'n', 'c', 'n', 'm', 'm', 'u', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'n', 'm', 'n', 'c', 'm', 'n', 'c', 'm', 'n', 'c', 'm', 'n', 'c', 'm', 'u', 'm', 'u', 'c', 'n', 'u', 'n', 's', 'n', 'c', 'm', 'n'] ,['u', 'c', 'm', 'n', 's', 'n', 'c', 'u', 'n', 'm', 'u', 'm', 'n', 'u', 'c', 'u', 'u', 'm', 'm', 'n', 'n', 'm', 'n', 'c', 'u', 'm', 'c', 'u', 'c', 'u', 's', 'u', 'm', 'm', 'u', 'n', 'c', 'u', 'm', 'u', 'n', 'm', 'u', 'm', 'c', 'u', 'm', 'c', 'u', 'n', 'c', 'm', 'u', 'c', 'm', 'n', 'n', 'n', 'c', 'n'] ,['n', 'm', 'c', 'u', 'm', 'c', 'u', 's', 'u', 'c', 'n', 'u', 'm', 'u', 'n', 'm', 'u', 'c', 'm', 'u', 'c', 'n', 's', 'n', 'c', 'n', 'c', 'n', 'm', 'c', 'n', 'm', 'n', 'c', 'c', 'n', 'm', 'c', 'n', 'm', 'c', 'm', 'n', 'c', 'u', 'u', 'm', 's', 'm', 'n', 'm', 's', 'm', 'm', 'n', 's', 'n', 'm', 'u', 'n'] ,['u', 'n', 'm', 'u', 'c', 'm', 'n', 'm', 'u', 'm', 'n', 'u', 'u', 'n', 'c', 'u', 'n', 'n', 'u', 'm', 'n', 's', 'n', 's', 'n', 'm', 'c', 'c', 'u', 'n', 'c', 'n', 'u', 'c', 'n', 'c', 'm', 'n', 'c', 'u', 'n', 's', 'n', 'n', 'c', 'u', 'm', 'c', 'u', 's', 'u', 'm', 'u', 'm', 'n', 'u', 'm', 'u', 'u', 'c'] ,['m', 'n', 'c', 'm', 'n', 'u', 'n', 'n', 'm', 'u', 'c', 'm', 'c', 'n', 'm', 'u', 'c', 'c', 'm', 'n', 'u', 'm', 'n', 'c', 'u', 'n', 'm', 'u', 'c', 'm', 'u', 'n', 'n', 'u', 'm', 'c', 'm', 'u', 'n', 'm', 'c', 'n', 'm', 'n', 'u', 'm', 'n', 'm', 'u', 'm', 'm', 'c', 'u', 'n', 'u', 'm', 'n', 'u', 'u', 'u']]
        m_correct = [4,4,12,4,4,12,12]
        n_back = [1,2,2,3,3,3,3,3]
    else:
        #test letters
        Letters = [['n', 'n', 'c', 'u', 'm', 'm', 'c', 'c', 'n', 'n', 's', 'u', 'c', 'c', 's', 'n', 'n', 's', 'm', 'c', 'c', 'n', 'n', 'u', 'u', 'n', 'c', 'u', 'c', 'n', 'n', 's', 'n', 'n', 's', 'c', 'c', 'n', 'n', 'u', 'n', 'n', 'c', 'n', 'n', 'u', 'u', 'n', 'u', 'u', 'n', 'n', 's', 'm', 'u', 'u', 'm', 'm', 'u', 'n'] ,['c', 'n', 'u', 'n', 'c', 'n', 'm', 'n', 'u', 'c', 'm', 'm', 'n', 'c', 'n', 's', 'n', 'u', 'n', 'm', 'c', 'u', 'm', 'u', 's', 'u', 's', 'u', 'n', 'm', 'u', 'm', 'n', 'm', 'c', 'c', 'u', 'c', 'n', 'c', 'n', 'c', 'm', 'n', 'u', 'm', 'n', 'c', 'n', 'c', 'm', 'c', 's', 'c', 'c', 'n', 'u', 'm', 'm', 'u'] ,['c', 'n', 's', 'c', 'n', 'm', 'c', 'n', 'c', 'c', 'n', 'c', 'u', 'n', 'm', 'u', 'm', 'c', 'u', 'n', 'm', 's', 'm', 'n', 'u', 'n', 's', 'n', 'm', 'c', 'n', 'u', 'm', 'n', 'u', 'u', 'n', 'u', 'm', 'n', 'c', 'm', 'c', 'n', 'u', 'n', 'm', 'u', 'u', 'c', 'u', 'c', 'n', 'c', 'n', 's', 'n', 'n', 'n', 'u'] ,['m', 'c', 'u', 'c', 'u', 's', 'u', 'n', 'c', 'n', 'm', 'n', 'c', 'c', 'u', 'c', 'n', 'c', 'u', 'n', 'n', 'm', 'c', 's', 'c', 'u', 'c', 'u', 'n', 'm', 'n', 'c', 's', 'c', 'n', 'm', 'n', 'm', 'u', 'n', 'u', 'c', 's', 'c', 'n', 'u', 'c', 'm', 'n', 'c', 'm', 'c', 'u', 'c', 'u', 'c', 'm', 'n', 'n', 'm'] ,['u', 'u', 'c', 'c', 'm', 'm', 'c', 'c', 'u', 'u', 's', 'm', 'm', 'u', 'c', 'm', 'm', 'c', 'n', 'n', 'm', 'm', 'c', 'u', 'n', 'c', 'u', 'c', 'c', 'u', 'n', 'n', 'm', 'm', 'c', 'n', 'n', 'c', 'c', 's', 'c', 'u', 'm', 'm', 'c', 'u', 'u', 's', 'c', 'c', 'u', 'u', 'n', 'c', 'u', 'u', 'c', 'c', 'u', 'c'] ,['n', 'u', 'u', 'c', 's', 'n', 'c', 'm', 'u', 'c', 'u', 'u', 'm', 'u', 'u', 'm', 'u', 'n', 'c', 'u', 'm', 'n', 's', 'n', 'u', 'n', 'm', 'u', 'n', 'm', 'u', 'm', 'u', 'c', 's', 'c', 'm', 'c', 'u', 'c', 'm', 'u', 'u', 'm', 'c', 'n', 'u', 'n', 'u', 'm', 'c', 'n', 'm', 'c', 'u', 'm', 'c', 'u', 'c', 'c'] ,['s', 'm', 'n', 'u', 's', 'u', 'n', 's', 'n', 'm', 'u', 'm', 'n', 'u', 'm', 'c', 'm', 'u', 'm', 'u', 'm', 'u', 'c', 'u', 'c', 'n', 'm', 'n', 'm', 'n', 'u', 'n', 's', 'n', 'm', 'm', 'u', 'c', 'u', 'n', 'm', 'u', 'n', 'm', 'u', 'm', 'n', 'n', 'c', 'u', 'm', 'c', 'm', 'n', 'c', 'n', 'u', 'm', 'c', 'm'] ,['m', 'n', 'm', 'n', 'c', 'n', 'u', 'n', 'c', 'm', 'n', 'm', 'n', 'u', 'u', 'n', 'u', 'm', 'c', 'n', 'u', 'c', 'm', 'n', 'c', 'm', 'n', 'm', 'm', 'n', 's', 'n', 'm', 'm', 'n', 'u', 'c', 'm', 'n', 'c', 'n', 'u', 'n', 'n', 'u', 'n', 'm', 'u', 'c', 'm', 'n', 'c', 'm', 'n', 'u', 'm', 'c', 'c', 'c', 'n'] ,['c', 'c', 's', 'm', 'c', 'u', 'n', 'n', 'c', 'c', 'm', 'n', 'n', 's', 'c', 'c', 's', 'n', 'n', 'u', 'm', 'm', 'n', 'n', 'u', 'u', 's', 'n', 'c', 'c', 'm', 'm', 'u', 'c', 'c', 'n', 'n', 'c', 'c', 'u', 'c', 'c', 'm', 'm', 'c', 'c', 'u', 'u', 'm', 'c', 'c', 's', 'm', 'u', 'u', 's', 'c', 'm', 'c', 'u'] ,['u', 's', 'u', 'n', 'm', 'u', 'n', 'c', 'u', 'c', 'n', 'u', 'u', 'c', 'n', 'n', 'u', 'n', 'c', 'u', 'n', 'u', 'm', 'u', 'u', 'm', 'c', 'm', 'u', 'c', 'u', 'n', 'm', 's', 'm', 'c', 'm', 'c', 'n', 'c', 'm', 'c', 'c', 'n', 'm', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'm', 'n', 'm', 'n', 'm', 'u', 'n'] ,['s', 'c', 'u', 'n', 's', 'm', 'c', 's', 'n', 'u', 's', 'u', 'm', 'm', 'c', 'm', 'n', 'c', 'n', 'c', 'c', 'n', 'c', 'n', 'm', 'u', 'c', 'n', 'u', 'm', 'n', 'c', 'n', 'c', 's', 'c', 's', 'c', 'm', 'u', 'c', 'u', 'c', 'm', 'c', 'n', 'm', 'n', 'u', 's', 'u', 'n', 'u', 'u', 'm', 's', 'm', 'u', 'm', 'u'] ,['m', 'm', 'c', 'n', 'c', 'c', 'n', 'n', 'u', 'u', 'n', 'u', 'u', 's', 'm', 'm', 's', 'n', 'n', 'm', 'n', 'u', 'n', 'n', 'c', 'u', 'u', 'n', 'u', 'u', 'c', 'c', 'm', 'm', 'n', 'n', 'm', 'c', 'c', 'u', 'n', 'n', 'c', 'c', 'u', 'n', 'n', 'u', 'c', 'n', 'n', 's', 'c', 'm', 'u', 'u', 'm', 'm', 'u', 'm'] ,['n', 'm', 'c', 'n', 'm', 's', 'n', 'u', 'c', 's', 'c', 'u', 'n', 'u', 'm', 'c', 'u', 'm', 'c', 'n', 'u', 'n', 's', 'n', 'u', 'n', 'm', 'u', 'm', 'n', 'u', 'c', 'u', 'c', 'c', 'u', 'n', 'm', 'c', 'm', 'n', 'c', 'n', 'c', 'm', 'n', 'c', 'c', 'n', 'm', 'c', 'n', 'u', 'c', 'n', 'u', 'n', 'n', 'm', 'c'] ,['c', 'c', 'm', 'u', 'n', 'u', 'n', 'c', 'm', 'u', 'c', 'm', 'n', 'c', 'm', 'u', 'm', 'c', 'n', 'm', 'c', 'n', 'm', 's', 'm', 'u', 'c', 'm', 'c', 's', 'c', 'c', 'u', 'n', 'n', 'u', 'n', 'u', 'c', 'n', 'm', 'c', 'm', 'u', 'c', 'n', 'u', 'm', 'n', 'm', 'c', 'n', 'u', 'u', 'c', 'u', 'n', 'u', 'u', 'u'] ,['m', 'u', 'n', 'u', 'n', 's', 'n', 'c', 'u', 'u', 'c', 'n', 'm', 'n', 'u', 'm', 'c', 'u', 'n', 'u', 'n', 'u', 'c', 'u', 's', 'u', 'm', 'n', 'n', 'm', 'n', 'm', 'u', 'c', 's', 'c', 'm', 'm', 'u', 's', 'u', 'u', 'n', 'u', 's', 'u', 'c', 'm', 'c', 'm', 'u', 'm', 'u', 'n', 'u', 'm', 'n', 'u', 'c', 's'] ,['n', 'n', 'c', 'n', 'n', 'u', 'u', 'n', 'n', 's', 'u', 'u', 'c', 'm', 'n', 'm', 'm', 'c', 'c', 'm', 'm', 's', 'u', 'u', 'c', 'c', 'm', 'n', 'c', 'm', 'u', 'n', 'n', 'm', 'm', 'c', 'c', 'u', 'u', 's', 'm', 'c', 'n', 'c', 'c', 's', 'n', 'n', 'c', 'c', 's', 'n', 'n', 'c', 'c', 'm', 'm', 'c', 'u', 'c'] ,['u', 's', 'm', 'u', 'c', 's', 'n', 'c', 'n', 'm', 's', 'm', 'u', 'n', 'm', 'c', 'u', 'n', 'm', 'u', 'm', 'c', 'u', 'm', 'c', 'u', 'u', 'c', 'u', 'c', 'm', 'u', 's', 'u', 'c', 'u', 'c', 'c', 'n', 'c', 's', 'c', 'u', 'n', 'c', 'u', 'n', 'm', 'n', 'm', 'n', 's', 'n', 'n', 'm', 'n', 'c', 'u', 'n', 'c'] ,['n', 'u', 's', 'u', 'm', 'u', 'n', 'c', 'u', 'c', 'n', 'u', 'c', 'u', 'n', 'm', 'c', 'n', 'u', 'n', 'u', 'n', 'u', 'm', 'c', 'm', 'n', 'u', 'm', 'c', 'c', 'n', 'm', 'c', 'm', 'c', 'c', 'u', 'c', 'u', 'u', 'n', 'm', 'c', 'm', 'c', 'm', 'm', 'n', 'u', 'n', 'u', 'm', 'u', 'm', 'c', 'u', 'n', 'm', 'm'] ,['m', 'u', 'n', 'm', 'c', 'n', 'm', 'n', 'c', 'm', 'c', 'm', 'c', 'n', 'm', 'u', 'n', 'm', 'n', 'm', 'u', 'c', 'u', 'n', 'u', 'c', 'u', 'n', 'c', 'm', 'n', 'c', 'n', 's', 'n', 's', 'n', 'c', 'm', 'n', 'c', 'm', 'u', 'c', 'm', 'n', 'u', 'm', 'n', 'm', 'm', 'c', 'm', 'c', 'm', 's', 'm', 'n', 'u', 'm'] ,['c', 'n', 'u', 'm', 'c', 'u', 's', 'n', 'm', 'n', 'u', 'c', 'm', 'c', 'u', 'm', 'u', 'n', 'n', 'm', 'n', 'm', 'u', 'm', 'n', 's', 'n', 'm', 'c', 'm', 's', 'm', 'n', 'n', 'm', 'c', 'n', 'c', 'n', 'c', 'u', 'c', 'm', 'c', 'n', 'm', 'c', 'n', 'u', 'n', 'm', 'n', 'm', 'n', 'u', 's', 'u', 'm', 'u', 'n'] ,['n', 'c', 'c', 'm', 'm', 'u', 'u', 'c', 'c', 'u', 'u', 'm', 'm', 'n', 'n', 'm', 'm', 's', 'n', 'u', 'c', 'c', 'n', 'c', 'c', 'm', 'm', 'n', 'n', 'u', 'c', 'c', 's', 'n', 'm', 'm', 'u', 'm', 'u', 'u', 'c', 'm', 'c', 'c', 'm', 'm', 'n', 'n', 'm', 'c', 'm', 'm', 'u', 'm', 'u', 'm', 'm', 'u', 'm', 's'] ,['c', 'u', 'm', 'c', 'u', 'm', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'm', 'c', 'n', 'm', 'u', 'n', 'm', 'c', 'u', 'u', 'c', 'u', 'u', 'm', 'u', 'c', 'm', 'u', 'n', 'u', 'n', 'c', 'n', 'u', 'c', 'm', 'm', 'n', 'm', 'u', 'm', 'c', 's', 'c', 'c', 'm', 'n', 'u', 'm', 'n', 'm', 'c', 'n', 'm', 'u', 'c', 'c']]
        m_correct = [4,4,4,8,8,8,12,12,12,4,8,4,4,12,12,12,8,12,12,8,8,8]
        n_back = [1,2,3,2,1,3,2,3,1,2,2,1,3,3,2,1,3,2,3,2,1,3]
    

elif expInfo['task'] == "ax-cpt":
    def check(x,y):
        global same
        global fd
        global score
        global n_hit
        global n_error
        global n_miss
        global n_pass
        if x == "c" and y == "n":
            if same == 1:
                score += 1
                n_hit += 1
                fd = 13
            else:
                n_miss += 1
                fd = 11
        else:
            if same==1:
                score -= 1
                n_error += 1
                fd= 12
            else:
                n_pass += 1
                fd = 9

    if expInfo['version'] == "train":
        #train letters
        Letters = [['n', 'c', 'n', 'c', 'm', 'u', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'm', 'u', 'm', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'm', 'c', 'm', 'c', 'm', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'c'], ['m', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'm', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'u', 'm', 'c', 'n', 'c', 'n', 'c', 'm', 'u'], ['n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'u', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'u', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'u', 'n', 'u', 'n', 'c', 'n', 'c', 'n', 'u', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c'], ['n', 'c', 'n', 'c', 'n', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'u', 'n', 'c', 'n', 'c', 'n', 'u', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'u', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'u', 'm', 'u', 'n', 'c'], ['n', 'c', 'm', 'u', 'm', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'u', 'n', 'c', 'm', 'u', 'n', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'm', 'c', 'm', 'c', 'n', 'c', 'n', 'c'], ['n', 'c', 'n', 'c', 'm', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'm', 'c', 'm', 'c', 'm', 'c', 'm', 'u', 'n', 'c', 'm', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u'], ['m', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'm', 'c', 'n', 'c', 'm', 'u', 'm', 'c', 'm', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c'], ['n', 'c', 'n', 'c', 'm', 'u', 'n', 'u', 'n', 'c', 'n', 'u', 'n', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'u', 'm', 'u', 'n', 'c', 'n', 'u', 'n', 'c']]
        m_correct = [4,4,12,4,4,12,12]
        n_back = [1,2,2,3,3,3,3,3]
    else:
        #test letters
        Letters = [['m', 'u', 'n', 'c', 'm', 'c', 'm', 'u', 'm', 'c', 'n', 'c', 'm', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'm', 'u', 'n', 'c'], ['m', 'c', 'n', 'c', 'n', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'm', 'u', 'n', 'c', 'm', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'm', 'u', 'n', 'c', 'n', 'u', 'n', 'c', 'n', 'c', 'n', 'c'], ['m', 'u', 'n', 'u', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'u', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'u', 'n', 'c', 'n', 'u', 'n', 'c', 'n', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c'], ['n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'u', 'm', 'u', 'n', 'c', 'n', 'c', 'm', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'm', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'm', 'u', 'm', 'c', 'n', 'c'], ['m', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'm', 'c', 'n', 'c', 'm', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'c', 'm', 'u', 'm', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'm', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c'], ['m', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'u', 'n', 'c', 'n', 'u', 'n', 'c', 'n', 'u', 'n', 'c', 'm', 'u', 'n', 'u'], ['n', 'c', 'm', 'u', 'n', 'c', 'm', 'c', 'n', 'c', 'n', 'c', 'm', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'u', 'n', 'c', 'm', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'u'], ['n', 'c', 'n', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'u', 'm', 'u', 'n', 'c', 'n', 'u', 'n', 'c', 'm', 'u', 'm', 'u'], ['n', 'c', 'n', 'c', 'n', 'c', 'm', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'm', 'u', 'm', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'm', 'u', 'n', 'c', 'm', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'c', 'n', 'c', 'n', 'c'], ['n', 'c', 'n', 'c', 'm', 'u', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'u', 'm', 'u', 'm', 'u', 'm', 'c', 'm', 'c', 'm', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c'], ['m', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'u', 'n', 'u', 'n', 'c', 'm', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'm', 'u', 'm', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c'], ['n', 'c', 'm', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'm', 'u', 'm', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'm', 'c', 'm', 'u', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'c', 'n', 'c'], ['n', 'c', 'n', 'c', 'n', 'c', 'n', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'u', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'u', 'n', 'u', 'm', 'u', 'n', 'c', 'n', 'u', 'n', 'c', 'n', 'c', 'm', 'u'], ['n', 'c', 'm', 'u', 'n', 'u', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'u', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'u', 'n', 'c', 'n', 'c'], ['n', 'u', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'm', 'u', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'm', 'c', 'n', 'c', 'm', 'c', 'm', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'u', 'm', 'u', 'n', 'c', 'n', 'c'], ['n', 'c', 'm', 'u', 'm', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'c', 'n', 'c', 'm', 'c', 'm', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'm', 'c', 'n', 'c', 'n', 'c', 'n', 'c'], ['m', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'u', 'n', 'c', 'n', 'u', 'm', 'u', 'n', 'c', 'm', 'u', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'u', 'n', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'u', 'n', 'c'], ['n', 'c', 'n', 'u', 'n', 'c', 'm', 'c', 'n', 'u', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'm', 'c', 'm', 'u', 'n', 'c', 'm', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c'], ['n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'u', 'n', 'c', 'm', 'u', 'n', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'u', 'n', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'u', 'n', 'c', 'n', 'c'], ['n', 'c', 'n', 'c', 'n', 'c', 'n', 'u', 'n', 'c', 'm', 'c', 'm', 'c', 'n', 'c', 'm', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c'], ['n', 'c', 'n', 'c', 'm', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'm', 'u', 'm', 'u', 'm', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'm', 'c', 'm', 'c', 'n', 'c'], ['m', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'u', 'n', 'u', 'n', 'u', 'n', 'c', 'n', 'u', 'n', 'c', 'n', 'c', 'n', 'u', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'n', 'c', 'm', 'u', 'm', 'u', 'n', 'c', 'm', 'u', 'n', 'c']]
        m_correct = [4,4,4,8,8,8,12,12,12,4,8,4,4,12,12,12,8,12,12,8,8,8]
        n_back = [1,2,3,2,1,3,2,3,1,2,2,1,3,3,2,1,3,2,3,2,1,3]

#just to orgnise data: nback is weight here.
text_next = visual.TextStim(win=win, name='text_next',
    text='',
    font='Open Sans',
    pos=(0, -0.3), height=0.03, wrapWidth=None, ori=0.0, 
    color='black', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-2.0);

# --- Initialize components for Routine "FX" ---
the_corss = visual.TextStim(win=win, name='the_corss',
    text='+',
    font='Arial',
    pos=(0, 0), height=0.05, wrapWidth=None, ori=0.0, 
    color='black', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=0.0);

# --- Initialize components for Routine "trial" ---
letters_display = visual.TextStim(win=win, name='letters_display',
    text='',
    font='Arial',
    pos=(0, 0), height=0.2, wrapWidth=None, ori=0.0, 
    color='black', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=0.0);
gap = visual.TextStim(win=win, name='gap',
    text=' ',
    font='Open Sans',
    pos=(0, 0), height=0.05, wrapWidth=None, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-1.0);
score_text = visual.TextStim(win=win, name='score_text',
    text='',
    font='Open Sans',
    pos=(0.25, 0.25), height=0.03, wrapWidth=None, ori=0.0, 
    color='black', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-2.0);
instruction_text = visual.TextStim(win=win, name='instruction_text',
    text='',
    font='Open Sans',
    pos=(-0.25, 0.25), height=0.03, wrapWidth=None, ori=0.0, 
    color='black', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-4.0);
polygon = visual.Rect(
    win=win, name='polygon',
    width=(0.06, 0.06)[0], height=(0.06, 0.06)[1],
    ori=0.0, pos=(-0.87, 0), anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='black', fillColor='black',
    opacity=None, depth=-5.0, interpolate=True)

# --- Initialize components for Routine "FXfd" ---
fd_corss = visual.TextStim(win=win, name='fd_corss',
    text='+',
    font='Arial',
    pos=(0, 0), height=0.05, wrapWidth=None, ori=0.0, 
    color='black', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=0.0);
# Run 'Begin Experiment' code from code_FXFD
if expInfo['task'] == "n-back":
    eg = {1: "A,A",2:"A,B,A",3:"A,B,C,A"}
else:
    eg = {1: "n,c",2:"n,c",3:"n,c"}

# --- Initialize components for Routine "fd" ---
feedback_text = visual.TextStim(win=win, name='feedback_text',
    text='',
    font='Arial',
    pos=(0, 0), height=0.03, wrapWidth=None, ori=0.0, 
    color='black', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=0.0);

# --- Initialize components for Routine "FX" ---
the_corss = visual.TextStim(win=win, name='the_corss',
    text='+',
    font='Arial',
    pos=(0, 0), height=0.05, wrapWidth=None, ori=0.0, 
    color='black', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=0.0);

# --- Initialize components for Routine "report" ---
How_much_effort = visual.TextStim(win=win, name='How_much_effort',
    text="Mon niveau d'effort cognitif se situe:",
    font='Open Sans',
    pos=(0, 0.4), height=0.03, wrapWidth=None, ori=0.0, 
    color='black', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=0.0);
slider_effort = visual.Slider(win=win, name='slider_effort',
    startValue=None, size=(1.0, 0.1), pos=(0, 0.3), units=None,
    labels=['min', 'max'], ticks=(0,100), granularity=0.0,
    style='slider', styleTweaks=(), opacity=0.0,
    labelColor='black', markerColor=[0.3255, 0.3255, 0.3255], lineColor='black', colorSpace='rgb',
    font='Open Sans', labelHeight=0.03,
    flip=False, ori=0.0, depth=-1, readOnly=False)
How_much_arous = visual.TextStim(win=win, name='How_much_arous',
    text='A quel point vous sentez-vous actif/passif en ce moment?',
    font='Open Sans',
    pos=(0, 0.2), height=0.03, wrapWidth=None, ori=0.0, 
    color='black', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-2.0);
slider_arous = visual.Slider(win=win, name='slider_arous',
    startValue=None, size=(1.0, 0.1), pos=(0, 0.1), units=None,
    labels=['passif', 'actif'], ticks=(0,100), granularity=0.0,
    style='slider', styleTweaks=(), opacity=0.0,
    labelColor='black', markerColor=[0.3255, 0.3255, 0.3255], lineColor='black', colorSpace='rgb',
    font='Open Sans', labelHeight=0.03,
    flip=False, ori=0.0, depth=-3, readOnly=False)
How_much_valence = visual.TextStim(win=win, name='How_much_valence',
    text='A quel point vous sentez-vous positif/négatif en ce moment ?',
    font='Open Sans',
    pos=(0, 0), height=0.03, wrapWidth=None, ori=0.0, 
    color='black', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-4.0);
slider_valenc = visual.Slider(win=win, name='slider_valenc',
    startValue=None, size=(1.0, 0.1), pos=(0, -0.1), units=None,
    labels=['négatif', 'positif'], ticks=(0,100), granularity=0.0,
    style='slider', styleTweaks=(), opacity=0.0,
    labelColor='black', markerColor=[0.3255, 0.3255, 0.3255], lineColor='black', colorSpace='rgb',
    font='Open Sans', labelHeight=0.03,
    flip=False, ori=0.0, depth=-5, readOnly=False)
text_report = visual.TextStim(win=win, name='text_report',
    text='',
    font='Open Sans',
    pos=(0, -0.45), height=0.03, wrapWidth=None, ori=0.0, 
    color='black', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-7.0);
How_much_time = visual.TextStim(win=win, name='How_much_time',
    text='Comment avez-vous ressenti le temps qui passe?',
    font='Open Sans',
    pos=(0, -0.2), height=0.03, wrapWidth=None, ori=0.0, 
    color='black', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-8.0);
slider_time = visual.Slider(win=win, name='slider_time',
    startValue=None, size=(1.0, 0.1), pos=(0, -0.3), units=None,
    labels=['0', '4 min'], ticks=(0,100), granularity=0.0,
    style='slider', styleTweaks=(), opacity=0.0,
    labelColor='black', markerColor=[0.3255, 0.3255, 0.3255], lineColor='black', colorSpace='rgb',
    font='Open Sans', labelHeight=0.03,
    flip=False, ori=0.0, depth=-9, readOnly=False)

# --- Initialize components for Routine "rest" ---
rest_cross = visual.TextStim(win=win, name='rest_cross',
    text='+',
    font='Open Sans',
    pos=(0, 0), height=0.05, wrapWidth=None, ori=0.0, 
    color='black', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=0.0);
rest_click = visual.TextStim(win=win, name='rest_click',
    text='Cliquez avec le bouton droit de la souris pour continuer',
    font='Arial',
    pos=(0, 0), height=0.03, wrapWidth=None, ori=0.0, 
    color='black', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-1.0);

# --- Initialize components for Routine "calib" ---

# --- Initialize components for Routine "end" ---
end_text = visual.TextStim(win=win, name='end_text',
    text='Fini!',
    font='Arial',
    pos=(0, 0), height=0.1, wrapWidth=None, ori=0, 
    color='black', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);

# Create some handy timers
globalClock = core.Clock()  # to track the time since experiment started
routineTimer = core.Clock()  # to track time remaining of each (possibly non-slip) routine 
# define target for calibration
calibrationTarget = visual.TargetStim(win, 
    name='calibrationTarget',
    radius=0.01, fillColor='black', borderColor='black', lineWidth=2.0,
    innerRadius=0.0035, innerFillColor='green', innerBorderColor='black', innerLineWidth=2.0,
    colorSpace='rgb', units=None
)
# define parameters for calibration
calibration = hardware.eyetracker.EyetrackerCalibration(win, 
    eyetracker, calibrationTarget,
    units=None, colorSpace='rgb',
    progressMode='space key', targetDur=1.0, expandScale=1.5,
    targetLayout='THREE_POINTS', randomisePos=False, textColor='black',
    movementAnimation=True, targetDelay=1.0
)
# run calibration
calibration.run()
# clear any keypresses from during calibration so they don't interfere with the experiment
defaultKeyboard.clearEvents()
# the Routine "calibration" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
task_blocks = data.TrialHandler(nReps=9999.0, method='random', 
    extraInfo=expInfo, originPath=-1,
    trialList=[None],
    seed=None, name='task_blocks')
thisExp.addLoop(task_blocks)  # add the loop to the experiment
thisTask_block = task_blocks.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisTask_block.rgb)
if thisTask_block != None:
    for paramName in thisTask_block:
        exec('{} = thisTask_block[paramName]'.format(paramName))

for thisTask_block in task_blocks:
    currentLoop = task_blocks
    # abbreviate parameter names if possible (e.g. rgb = thisTask_block.rgb)
    if thisTask_block != None:
        for paramName in thisTask_block:
            exec('{} = thisTask_block[paramName]'.format(paramName))
    
    # --- Prepare to start Routine "FX" ---
    continueRoutine = True
    routineForceEnded = False
    # update component parameters for each repeat
    # Run 'Begin Routine' code from code_FX
    stimuli_mouse.setVisible(visible=0)
    if record == 1:
        win.callOnFlip(send_trigger,3)
    # keep track of which components have finished
    FXComponents = [the_corss]
    for thisComponent in FXComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "FX" ---
    while continueRoutine and routineTimer.getTime() < 0.5:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *the_corss* updates
        if the_corss.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            the_corss.frameNStart = frameN  # exact frame index
            the_corss.tStart = t  # local t and not account for scr refresh
            the_corss.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(the_corss, 'tStartRefresh')  # time at next scr refresh
            the_corss.setAutoDraw(True)
        if the_corss.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > the_corss.tStartRefresh + 0.5-frameTolerance:
                # keep track of stop time/frame for later
                the_corss.tStop = t  # not accounting for scr refresh
                the_corss.frameNStop = frameN  # exact frame index
                the_corss.setAutoDraw(False)
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in FXComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "FX" ---
    for thisComponent in FXComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # Run 'End Routine' code from code_FX
    win.callOnFlip(send_trigger,200+ n_block)
    
    # using non-slip timing so subtract the expected duration of this Routine (unless ended on request)
    if routineForceEnded:
        routineTimer.reset()
    else:
        routineTimer.addTime(-0.500000)
    
    # --- Prepare to start Routine "block_info" ---
    continueRoutine = True
    routineForceEnded = False
    # update component parameters for each repeat
    text_info.setText("Bloc "+str(block+1)+" sur "+str(len(n_back))+". \n\nL’instruction est "+str(eg[n_back[block]])+".\n\n Vous devez collecter au moin de points "+str(m_correct[block]))
    # Run 'Begin Routine' code from code_init
    stimuli_mouse.setVisible(visible=0)
    
    if record == 1:
        win.callOnFlip(send_trigger,1)
    text_next.setText('Cliquez avec le bouton droit de la souris pour continuer')
    # keep track of which components have finished
    block_infoComponents = [text_info, text_next]
    for thisComponent in block_infoComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "block_info" ---
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *text_info* updates
        if text_info.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
            # keep track of start time/frame for later
            text_info.frameNStart = frameN  # exact frame index
            text_info.tStart = t  # local t and not account for scr refresh
            text_info.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_info, 'tStartRefresh')  # time at next scr refresh
            text_info.setAutoDraw(True)
        # Run 'Each Frame' code from code_init
        if frameN > 5:
            if stimuli_mouse.getPressed()[2]==1:
                stimuli_mouse.clickReset()
                continueRoutine=False
        
        # *text_next* updates
        if text_next.status == NOT_STARTED and tThisFlip >= 1-frameTolerance:
            # keep track of start time/frame for later
            text_next.frameNStart = frameN  # exact frame index
            text_next.tStart = t  # local t and not account for scr refresh
            text_next.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_next, 'tStartRefresh')  # time at next scr refresh
            text_next.setAutoDraw(True)
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in block_infoComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "block_info" ---
    for thisComponent in block_infoComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # the Routine "block_info" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # --- Prepare to start Routine "FX" ---
    continueRoutine = True
    routineForceEnded = False
    # update component parameters for each repeat
    # Run 'Begin Routine' code from code_FX
    stimuli_mouse.setVisible(visible=0)
    if record == 1:
        win.callOnFlip(send_trigger,3)
    # keep track of which components have finished
    FXComponents = [the_corss]
    for thisComponent in FXComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "FX" ---
    while continueRoutine and routineTimer.getTime() < 0.5:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *the_corss* updates
        if the_corss.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            the_corss.frameNStart = frameN  # exact frame index
            the_corss.tStart = t  # local t and not account for scr refresh
            the_corss.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(the_corss, 'tStartRefresh')  # time at next scr refresh
            the_corss.setAutoDraw(True)
        if the_corss.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > the_corss.tStartRefresh + 0.5-frameTolerance:
                # keep track of stop time/frame for later
                the_corss.tStop = t  # not accounting for scr refresh
                the_corss.frameNStop = frameN  # exact frame index
                the_corss.setAutoDraw(False)
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in FXComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "FX" ---
    for thisComponent in FXComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # Run 'End Routine' code from code_FX
    win.callOnFlip(send_trigger,200+ n_block)
    
    # using non-slip timing so subtract the expected duration of this Routine (unless ended on request)
    if routineForceEnded:
        routineTimer.reset()
    else:
        routineTimer.addTime(-0.500000)
    
    # set up handler to look after randomisation of conditions etc
    trials = data.TrialHandler(nReps=60.0, method='sequential', 
        extraInfo=expInfo, originPath=-1,
        trialList=[None],
        seed=None, name='trials')
    thisExp.addLoop(trials)  # add the loop to the experiment
    thisTrial = trials.trialList[0]  # so we can initialise stimuli with some values
    # abbreviate parameter names if possible (e.g. rgb = thisTrial.rgb)
    if thisTrial != None:
        for paramName in thisTrial:
            exec('{} = thisTrial[paramName]'.format(paramName))
    
    for thisTrial in trials:
        currentLoop = trials
        # abbreviate parameter names if possible (e.g. rgb = thisTrial.rgb)
        if thisTrial != None:
            for paramName in thisTrial:
                exec('{} = thisTrial[paramName]'.format(paramName))
        
        # --- Prepare to start Routine "trial" ---
        continueRoutine = True
        routineForceEnded = False
        # update component parameters for each repeat
        letters_display.setText(Letters[block][trial])
        score_text.setText("# " +str(m_correct[block]))
        # Run 'Begin Routine' code from code_main
        stimuli_mouse.setVisible(visible=0)
        if record == 1:
            win.callOnFlip(send_trigger,n_back[block]*10)
        
        timer = core.Clock()
        Rtrial = 1.5
        instruction_text.setText(" " + str(eg[n_back[block]]))
        # keep track of which components have finished
        trialComponents = [letters_display, gap, score_text, instruction_text, polygon]
        for thisComponent in trialComponents:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "trial" ---
        while continueRoutine and routineTimer.getTime() < 2.0:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *letters_display* updates
            if letters_display.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                letters_display.frameNStart = frameN  # exact frame index
                letters_display.tStart = t  # local t and not account for scr refresh
                letters_display.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(letters_display, 'tStartRefresh')  # time at next scr refresh
                letters_display.setAutoDraw(True)
            if letters_display.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > letters_display.tStartRefresh + 1-frameTolerance:
                    # keep track of stop time/frame for later
                    letters_display.tStop = t  # not accounting for scr refresh
                    letters_display.frameNStop = frameN  # exact frame index
                    letters_display.setAutoDraw(False)
            
            # *gap* updates
            if gap.status == NOT_STARTED and tThisFlip >= 1.0-frameTolerance:
                # keep track of start time/frame for later
                gap.frameNStart = frameN  # exact frame index
                gap.tStart = t  # local t and not account for scr refresh
                gap.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(gap, 'tStartRefresh')  # time at next scr refresh
                gap.setAutoDraw(True)
            if gap.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > gap.tStartRefresh + 1-frameTolerance:
                    # keep track of stop time/frame for later
                    gap.tStop = t  # not accounting for scr refresh
                    gap.frameNStop = frameN  # exact frame index
                    gap.setAutoDraw(False)
            
            # *score_text* updates
            if score_text.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                score_text.frameNStart = frameN  # exact frame index
                score_text.tStart = t  # local t and not account for scr refresh
                score_text.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(score_text, 'tStartRefresh')  # time at next scr refresh
                score_text.setAutoDraw(True)
            if score_text.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > score_text.tStartRefresh + 2-frameTolerance:
                    # keep track of stop time/frame for later
                    score_text.tStop = t  # not accounting for scr refresh
                    score_text.frameNStop = frameN  # exact frame index
                    score_text.setAutoDraw(False)
            # Run 'Each Frame' code from code_main
            if frameN == 5:
                if record == 1:
                    win.callOnFlip(send_trigger,100+m_correct[block])
            
            if stimuli_mouse.getPressed()[0]==1:
                if record == 1:
                    win.callOnFlip(send_trigger,8)
                    send_trigger(8)
            
                Rtrial = timer.getTime()-0.5
                stimuli_mouse.clickReset()
                same = 1
            
            
            # *instruction_text* updates
            if instruction_text.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                instruction_text.frameNStart = frameN  # exact frame index
                instruction_text.tStart = t  # local t and not account for scr refresh
                instruction_text.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(instruction_text, 'tStartRefresh')  # time at next scr refresh
                instruction_text.setAutoDraw(True)
            if instruction_text.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > instruction_text.tStartRefresh + 2-frameTolerance:
                    # keep track of stop time/frame for later
                    instruction_text.tStop = t  # not accounting for scr refresh
                    instruction_text.frameNStop = frameN  # exact frame index
                    instruction_text.setAutoDraw(False)
            
            # *polygon* updates
            if polygon.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                polygon.frameNStart = frameN  # exact frame index
                polygon.tStart = t  # local t and not account for scr refresh
                polygon.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(polygon, 'tStartRefresh')  # time at next scr refresh
                polygon.setAutoDraw(True)
            if polygon.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > polygon.tStartRefresh + 1.0-frameTolerance:
                    # keep track of stop time/frame for later
                    polygon.tStop = t  # not accounting for scr refresh
                    polygon.frameNStop = frameN  # exact frame index
                    polygon.setAutoDraw(False)
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in trialComponents:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "trial" ---
        for thisComponent in trialComponents:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # Run 'End Routine' code from code_main
        if expInfo['task'] == "n-back":
            check(Letters[block][trial],Letters[block][trial-n_back[block]])
        else:
            check(Letters[block][trial],Letters[block][trial-1])
        
        
        if record == 1:
            win.callOnFlip(send_trigger,fd)
        
        
        if score  == -1:
            score  = 0
            
        thisExp.addData("score",score)
        thisExp.addData("feedback",fd)
        thisExp.addData("responseTime",Rtrial)
        thisExp.addData("letter", Letters[block][trial])
        thisExp.addData("trial",trial)
        
        
        trial += 1
        same = 0
        stimuli_mouse.clickReset()
        core.wait(0.002)
        # using non-slip timing so subtract the expected duration of this Routine (unless ended on request)
        if routineForceEnded:
            routineTimer.reset()
        else:
            routineTimer.addTime(-2.000000)
        thisExp.nextEntry()
        
    # completed 60.0 repeats of 'trials'
    
    # get names of stimulus parameters
    if trials.trialList in ([], [None], None):
        params = []
    else:
        params = trials.trialList[0].keys()
    # save data for this loop
    trials.saveAsExcel(filename + '.xlsx', sheetName='trials',
        stimOut=params,
        dataOut=['n','all_mean','all_std', 'all_raw'])
    trials.saveAsText(filename + 'trials.csv', delim=',',
        stimOut=params,
        dataOut=['n','all_mean','all_std', 'all_raw'])
    
    # --- Prepare to start Routine "FXfd" ---
    continueRoutine = True
    routineForceEnded = False
    # update component parameters for each repeat
    # Run 'Begin Routine' code from code_FXFD
    thisExp.addData("n_back",n_back[block])
    thisExp.addData("m_correct",m_correct[block])
    thisExp.addData("DifficultyLevel",3*(n_back[block]-1)+m_correct[block])
    thisExp.addData("false_alarm",n_error)
    thisExp.addData("hit",n_hit)
    thisExp.addData("correct_rejection",n_pass)
    thisExp.addData("miss",n_miss)
    thisExp.addData("score",score)
    thisExp.addData("n_block",n_block)
    n_block += 1
    
    if score >= m_correct[block]:
        block +=1
    
    fd_score = score
    score = 0
    trial = 0
    n_miss = 0
    n_error = 0
    n_pass = 0
    n_hit = 0
    
    
    if record == 1:
        win.callOnFlip(send_trigger,3)
    # keep track of which components have finished
    FXfdComponents = [fd_corss]
    for thisComponent in FXfdComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "FXfd" ---
    while continueRoutine and routineTimer.getTime() < 0.5:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *fd_corss* updates
        if fd_corss.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            fd_corss.frameNStart = frameN  # exact frame index
            fd_corss.tStart = t  # local t and not account for scr refresh
            fd_corss.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(fd_corss, 'tStartRefresh')  # time at next scr refresh
            fd_corss.setAutoDraw(True)
        if fd_corss.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > fd_corss.tStartRefresh + 0.5-frameTolerance:
                # keep track of stop time/frame for later
                fd_corss.tStop = t  # not accounting for scr refresh
                fd_corss.frameNStop = frameN  # exact frame index
                fd_corss.setAutoDraw(False)
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in FXfdComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "FXfd" ---
    for thisComponent in FXfdComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # using non-slip timing so subtract the expected duration of this Routine (unless ended on request)
    if routineForceEnded:
        routineTimer.reset()
    else:
        routineTimer.addTime(-0.500000)
    
    # --- Prepare to start Routine "fd" ---
    continueRoutine = True
    routineForceEnded = False
    # update component parameters for each repeat
    feedback_text.setText("Vous gangez "+ str(fd_score))
    # Run 'Begin Routine' code from code
    if record == 1:
        win.callOnFlip(send_trigger,14)
    # keep track of which components have finished
    fdComponents = [feedback_text]
    for thisComponent in fdComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "fd" ---
    while continueRoutine and routineTimer.getTime() < 3.0:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *feedback_text* updates
        if feedback_text.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            feedback_text.frameNStart = frameN  # exact frame index
            feedback_text.tStart = t  # local t and not account for scr refresh
            feedback_text.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(feedback_text, 'tStartRefresh')  # time at next scr refresh
            feedback_text.setAutoDraw(True)
        if feedback_text.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > feedback_text.tStartRefresh + 3-frameTolerance:
                # keep track of stop time/frame for later
                feedback_text.tStop = t  # not accounting for scr refresh
                feedback_text.frameNStop = frameN  # exact frame index
                feedback_text.setAutoDraw(False)
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in fdComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "fd" ---
    for thisComponent in fdComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # using non-slip timing so subtract the expected duration of this Routine (unless ended on request)
    if routineForceEnded:
        routineTimer.reset()
    else:
        routineTimer.addTime(-3.000000)
    
    # --- Prepare to start Routine "FX" ---
    continueRoutine = True
    routineForceEnded = False
    # update component parameters for each repeat
    # Run 'Begin Routine' code from code_FX
    stimuli_mouse.setVisible(visible=0)
    if record == 1:
        win.callOnFlip(send_trigger,3)
    # keep track of which components have finished
    FXComponents = [the_corss]
    for thisComponent in FXComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "FX" ---
    while continueRoutine and routineTimer.getTime() < 0.5:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *the_corss* updates
        if the_corss.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            the_corss.frameNStart = frameN  # exact frame index
            the_corss.tStart = t  # local t and not account for scr refresh
            the_corss.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(the_corss, 'tStartRefresh')  # time at next scr refresh
            the_corss.setAutoDraw(True)
        if the_corss.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > the_corss.tStartRefresh + 0.5-frameTolerance:
                # keep track of stop time/frame for later
                the_corss.tStop = t  # not accounting for scr refresh
                the_corss.frameNStop = frameN  # exact frame index
                the_corss.setAutoDraw(False)
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in FXComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "FX" ---
    for thisComponent in FXComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # Run 'End Routine' code from code_FX
    win.callOnFlip(send_trigger,200+ n_block)
    
    # using non-slip timing so subtract the expected duration of this Routine (unless ended on request)
    if routineForceEnded:
        routineTimer.reset()
    else:
        routineTimer.addTime(-0.500000)
    
    # --- Prepare to start Routine "report" ---
    continueRoutine = True
    routineForceEnded = False
    # update component parameters for each repeat
    slider_effort.reset()
    slider_arous.reset()
    slider_valenc.reset()
    # Run 'Begin Routine' code from code_report
    stimuli_mouse.setVisible(visible=1)
    
    if record == 1:
        win.callOnFlip(send_trigger,5)
    text_report.setText('Cliquez avec le bouton droit de la souris pour continuer')
    slider_time.reset()
    # keep track of which components have finished
    reportComponents = [How_much_effort, slider_effort, How_much_arous, slider_arous, How_much_valence, slider_valenc, text_report, How_much_time, slider_time]
    for thisComponent in reportComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "report" ---
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *How_much_effort* updates
        if How_much_effort.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
            # keep track of start time/frame for later
            How_much_effort.frameNStart = frameN  # exact frame index
            How_much_effort.tStart = t  # local t and not account for scr refresh
            How_much_effort.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(How_much_effort, 'tStartRefresh')  # time at next scr refresh
            How_much_effort.setAutoDraw(True)
        
        # *slider_effort* updates
        if slider_effort.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
            # keep track of start time/frame for later
            slider_effort.frameNStart = frameN  # exact frame index
            slider_effort.tStart = t  # local t and not account for scr refresh
            slider_effort.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(slider_effort, 'tStartRefresh')  # time at next scr refresh
            slider_effort.setAutoDraw(True)
        
        # *How_much_arous* updates
        if How_much_arous.status == NOT_STARTED and slider_effort.rating:
            # keep track of start time/frame for later
            How_much_arous.frameNStart = frameN  # exact frame index
            How_much_arous.tStart = t  # local t and not account for scr refresh
            How_much_arous.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(How_much_arous, 'tStartRefresh')  # time at next scr refresh
            How_much_arous.setAutoDraw(True)
        
        # *slider_arous* updates
        if slider_arous.status == NOT_STARTED and slider_effort.rating:
            # keep track of start time/frame for later
            slider_arous.frameNStart = frameN  # exact frame index
            slider_arous.tStart = t  # local t and not account for scr refresh
            slider_arous.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(slider_arous, 'tStartRefresh')  # time at next scr refresh
            slider_arous.setAutoDraw(True)
        
        # *How_much_valence* updates
        if How_much_valence.status == NOT_STARTED and slider_arous.rating:
            # keep track of start time/frame for later
            How_much_valence.frameNStart = frameN  # exact frame index
            How_much_valence.tStart = t  # local t and not account for scr refresh
            How_much_valence.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(How_much_valence, 'tStartRefresh')  # time at next scr refresh
            How_much_valence.setAutoDraw(True)
        
        # *slider_valenc* updates
        if slider_valenc.status == NOT_STARTED and slider_arous.rating:
            # keep track of start time/frame for later
            slider_valenc.frameNStart = frameN  # exact frame index
            slider_valenc.tStart = t  # local t and not account for scr refresh
            slider_valenc.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(slider_valenc, 'tStartRefresh')  # time at next scr refresh
            slider_valenc.setAutoDraw(True)
        # Run 'Each Frame' code from code_report
        if stimuli_mouse.getPressed()[0]==1:
            if record == 1:
                win.callOnFlip(send_trigger,8)
        
        if slider_time.rating:
            if stimuli_mouse.getPressed()[2]==1:
                stimuli_mouse.clickReset()
                continueRoutine=False
        
        # *text_report* updates
        if text_report.status == NOT_STARTED and slider_time.rating:
            # keep track of start time/frame for later
            text_report.frameNStart = frameN  # exact frame index
            text_report.tStart = t  # local t and not account for scr refresh
            text_report.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_report, 'tStartRefresh')  # time at next scr refresh
            text_report.setAutoDraw(True)
        
        # *How_much_time* updates
        if How_much_time.status == NOT_STARTED and slider_valenc.rating:
            # keep track of start time/frame for later
            How_much_time.frameNStart = frameN  # exact frame index
            How_much_time.tStart = t  # local t and not account for scr refresh
            How_much_time.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(How_much_time, 'tStartRefresh')  # time at next scr refresh
            How_much_time.setAutoDraw(True)
        
        # *slider_time* updates
        if slider_time.status == NOT_STARTED and slider_valenc.rating:
            # keep track of start time/frame for later
            slider_time.frameNStart = frameN  # exact frame index
            slider_time.tStart = t  # local t and not account for scr refresh
            slider_time.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(slider_time, 'tStartRefresh')  # time at next scr refresh
            slider_time.setAutoDraw(True)
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in reportComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "report" ---
    for thisComponent in reportComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    task_blocks.addData('slider_effort.response', slider_effort.getRating())
    task_blocks.addData('slider_effort.rt', slider_effort.getRT())
    task_blocks.addData('slider_arous.response', slider_arous.getRating())
    task_blocks.addData('slider_arous.rt', slider_arous.getRT())
    task_blocks.addData('slider_valenc.response', slider_valenc.getRating())
    task_blocks.addData('slider_valenc.rt', slider_valenc.getRT())
    # Run 'End Routine' code from code_report
    stimuli_mouse.setVisible(visible=0)
    
    task_blocks.addData('slider_time.response', slider_time.getRating())
    task_blocks.addData('slider_time.rt', slider_time.getRT())
    # the Routine "report" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # --- Prepare to start Routine "rest" ---
    continueRoutine = True
    routineForceEnded = False
    # update component parameters for each repeat
    # Run 'Begin Routine' code from code_rest
    if record == 1:
        win.callOnFlip(send_trigger,2)
    # keep track of which components have finished
    restComponents = [rest_cross, rest_click]
    for thisComponent in restComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "rest" ---
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *rest_cross* updates
        if rest_cross.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            rest_cross.frameNStart = frameN  # exact frame index
            rest_cross.tStart = t  # local t and not account for scr refresh
            rest_cross.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(rest_cross, 'tStartRefresh')  # time at next scr refresh
            rest_cross.setAutoDraw(True)
        if rest_cross.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > rest_cross.tStartRefresh + 10-frameTolerance:
                # keep track of stop time/frame for later
                rest_cross.tStop = t  # not accounting for scr refresh
                rest_cross.frameNStop = frameN  # exact frame index
                rest_cross.setAutoDraw(False)
        
        # *rest_click* updates
        if rest_click.status == NOT_STARTED and tThisFlip >= 10-frameTolerance:
            # keep track of start time/frame for later
            rest_click.frameNStart = frameN  # exact frame index
            rest_click.tStart = t  # local t and not account for scr refresh
            rest_click.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(rest_click, 'tStartRefresh')  # time at next scr refresh
            rest_click.setAutoDraw(True)
        # Run 'Each Frame' code from code_rest
        if rest_cross.status == FINISHED:
            if stimuli_mouse.getPressed()[2]==1:
                stimuli_mouse.clickReset()
                continueRoutine=False
        
        if defaultKeyboard.getKeys(keyList=["escape"]):
            if record == 1:
                eyetracker.setRecordingState(False)
                eyetracker.setConnectionState(False)
            core.quit()
            
        if defaultKeyboard.getKeys(keyList=["n"]):
            block +=1
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in restComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "rest" ---
    for thisComponent in restComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # Run 'End Routine' code from code_rest
    
    if block == len(n_back):
        task_blocks.finished = True
    # the Routine "rest" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # --- Prepare to start Routine "calib" ---
    continueRoutine = True
    routineForceEnded = False
    # update component parameters for each repeat
    # keep track of which components have finished
    calibComponents = []
    for thisComponent in calibComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "calib" ---
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        # Run 'Each Frame' code from code_calib
        if record == 1:
            win.callOnFlip(send_trigger,50)
        calibration.run()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in calibComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "calib" ---
    for thisComponent in calibComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # the Routine "calib" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 9999.0 repeats of 'task_blocks'

# get names of stimulus parameters
if task_blocks.trialList in ([], [None], None):
    params = []
else:
    params = task_blocks.trialList[0].keys()
# save data for this loop
task_blocks.saveAsExcel(filename + '.xlsx', sheetName='task_blocks',
    stimOut=params,
    dataOut=['n','all_mean','all_std', 'all_raw'])
task_blocks.saveAsText(filename + 'task_blocks.csv', delim=',',
    stimOut=params,
    dataOut=['n','all_mean','all_std', 'all_raw'])

# --- Prepare to start Routine "end" ---
continueRoutine = True
routineForceEnded = False
# update component parameters for each repeat
# Run 'Begin Routine' code from end_code
if record == 1:
    eyetracker.setRecordingState(False)
    eyetracker.setConnectionState(False)
# keep track of which components have finished
endComponents = [end_text]
for thisComponent in endComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
frameN = -1

# --- Run Routine "end" ---
while continueRoutine and routineTimer.getTime() < 1.0:
    # get current time
    t = routineTimer.getTime()
    tThisFlip = win.getFutureFlipTime(clock=routineTimer)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *end_text* updates
    if end_text.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        end_text.frameNStart = frameN  # exact frame index
        end_text.tStart = t  # local t and not account for scr refresh
        end_text.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(end_text, 'tStartRefresh')  # time at next scr refresh
        end_text.setAutoDraw(True)
    if end_text.status == STARTED:
        # is it time to stop? (based on global clock, using actual start)
        if tThisFlipGlobal > end_text.tStartRefresh + 1.0-frameTolerance:
            # keep track of stop time/frame for later
            end_text.tStop = t  # not accounting for scr refresh
            end_text.frameNStop = frameN  # exact frame index
            end_text.setAutoDraw(False)
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        routineForceEnded = True
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in endComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# --- Ending Routine "end" ---
for thisComponent in endComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# using non-slip timing so subtract the expected duration of this Routine (unless ended on request)
if routineForceEnded:
    routineTimer.reset()
else:
    routineTimer.addTime(-1.000000)

# --- End experiment ---
# Flip one final time so any remaining win.callOnFlip() 
# and win.timeOnFlip() tasks get executed before quitting
win.flip()

# these shouldn't be strictly necessary (should auto-save)
thisExp.saveAsWideText(filename+'.csv', delim='comma')
thisExp.saveAsPickle(filename)
logging.flush()
# make sure everything is closed down
if eyetracker:
    eyetracker.setConnectionState(False)
thisExp.abort()  # or data files will save again on exit
win.close()
core.quit()
