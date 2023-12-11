"""
Presenting visual stimuli with ultra-high temporal resolution
using Python 2.7.

A full description of the presentation method can be found in
the paper: 

Poth, Foerster, Behler, Schwanecke, Schneider, & Botsch (2017). 
Ultra-high temporal resolution of visual presentation using 
gaming monitors and G-Sync. Behavior Research Methods.

This code uses the Psychopy2 extension 
(Pierce, 2007, J Neuro Meth, 2009, Front Neuroinform) for
Python 2.7.

Christian H. Poth, 2017, c.poth@uni-bielefeld.de

"""

# Import python modules from Psychopy2.
# -----------------------------------------------------------------
from psychopy import core, visual, event , monitors, data
from psychopy import parallel
parallel.setPortAddress(0x0000BFE8)
# for file save
import os 

# import for parallel port
from ctypes import windll
import time

# Stimulus production.
# -----------------------------------------------------------------
#stimColor = 'DarkGreen' # Stimulus color, here white.
#stimColor = 'FireBrick' # Stimulus color, here white.
stimColor = 1 # Stimulus color, here white.
baselineColor = -1 # Pre- and post-stimulus color, here black.

# Dimensions of the (full-screen) stimulus.
xRes = 2560 # x-resolution. 
#xRes = 1920 # x-resolution. 
yRes = 1440 # y-resolution.

# conditions
#conditions = {.007:1 , .0075:2 , .008:4 , .0085:8 , .009:16 , .023:32 , .050:64 }
#conditions = {.007:1 , .008:2 , .010:4 , .012:8 , .015:16 , .020:32 , .050:64 }
#conditions = {.009:1 , .010:4 ,.050:64 }
conditions = {1:1 , 2:2 , 3:4 , 7:64 , 36: 128}
#conditions = {100:1 }


# Open a window in Psychopy2.
win = visual.Window(size = [xRes, yRes], screen = 0,
                    fullscr = True, 
                    monitor = 'acer predator', # Psychopy's calibration.
                    color = baselineColor)

win.mouseVisible = False # Hide the mouse cursor.
mon = monitors.Monitor('acer predator')
print(mon.getSizePix())

# Create the pre-stimulus.
preStim = visual.Rect(win = win,
                   units = 'pix',
                   width = xRes,
                   height = yRes,
                   fillColor = baselineColor,  
                   lineColor = None)
preStim.draw() # Draw once already, because the first draw may be slower.
                    
# Create the stimulus.
stim = visual.Rect(win = win,
                   units = 'pix',
                   width = xRes,
                   height = yRes,
                   fillColor = stimColor, 
                   lineColor = None)
stim.draw()
stim.setAutoLog(True)

# Create the post-stimulus.
postStim = visual.Rect(win = win,
                   units = 'pix',
                   width = xRes,
                   height = yRes,
                   fillColor = baselineColor, 
                   lineColor = None)
postStim.draw()

win.clearBuffer() # Clear the back buffer of previously drawn stimuli.

# Waiting function (waits for the duration that is input in s).
# -----------------------------------------------------------------
def myWait(duration):
    clock = core.Clock()
    while True:
        if (clock.getTime() > duration):
            break
    return

pp1 = 0x2fb8
pp2 = 0xffb8
pp = pp1


# def trigger function
def send_trigger(code):
    parallel.setData(code)
    myWait(0.002)
    parallel.setData(0)

# initializes parallel port 1
#windll.inpout32.Out32(pp, 255)
#myWait(1)
parallel.setData(0)

n = 20  # number of trials
isi = 2 # time between stim

# create stim List
stimList = []
for duration,trg in conditions.items():
    stimList.append({'wanted_duration' : duration, 'trigger': trg})

# to save theoretical durations
trials = data.TrialHandler(stimList, n,
                           extraInfo={'participant': '', 'session':'001'})

# Stimulus presentation.
# -----------------------------------------------------------------
#for duration,trg in conditions.items():
for thisTrial in trials:

    # print stimulus duration.
    durationFrame = thisTrial['wanted_duration']
    trg = thisTrial['trigger']
    print('tested duration = %f frame(s)' %(durationFrame))
    
#    for i in range(n): # n trials.
    #    event.waitKeys() # Wait for key-press.
    myWait(isi)

#    # Pre-stimulus adaptation of monitor refresh rate.
#    for j in range(int(round(1 / duration))): # For about 1 s,...
#        clock_preStim = core.Clock()
#        preStim.draw()   # ...draw the pre-stimulus (into the back buffer),...
#        win.flip(False)  # ...display it by performing a buffer swap,...
#        myWait(duration) # ...and wait for the specified duration.
#        t = clock_preStim.getTime()
#        #print('preStim:' + str(t))
            
    # Stimulus.
    frameN = 0
    stim.draw()         # Draw the stimulus,...
    win.flip()
    send_trigger(trg)
    clock_stim = core.Clock()
    frameN += 1
    while frameN < durationFrame:
        stim.draw()         # Draw the stimulus,...
        win.flip()          # ...display it (now clearing the back buffer afterwards),...
        frameN += 1
    send_trigger(254)

    # Post-stimulus, which terminates the presentation of the stimulus.
    postStim.draw()
    win.flip(False)
    t = clock_stim.getTime()
    send_trigger(255)
    print('stim duration : ' + str(t))
    trials.data.add('measured_duration', t)  # add the data to our set


trials.saveAsWideText(fileName=os.path.join(os.curdir,'Data','testData_frames'))


# Shut down.
# -----------------------------------------------------------------
win.close()
core.quit()
