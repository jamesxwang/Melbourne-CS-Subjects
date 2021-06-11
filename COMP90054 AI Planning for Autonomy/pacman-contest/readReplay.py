import os
import pickle

def generateLayoutFile(replayDir, replayLayoutsDir):
    files= os.listdir(replayDir)

    for file in files:
        try:
            if not os.path.isdir(file):
                with open(replayDir+'/'+file, 'rb') as replayFile:
                    info = pickle.load(replayFile)
                    layout = info['layout']
                    actions = info['actions']
                    with open(replayLayoutsDir+'/'+file+'.capture.lay', 'w+') as outputFile:
                        outputFile.write(str(layout))
            rerunReplayGame(file, actions)
        except:
            os.remove(replayLayoutsDir+'/'+file+'.capture.lay')
            os.remove(replayLayoutsDir+'/'+file+'.capture.lay')

def rerunReplayGame(replayFileName, actions):
    layoutPath = '.' + replayLayoutsDir + '/' + replayFileName + '.capture.lay'
    with open('./replayAgentActions','wb') as f:
        f.write(pickle.dumps(actions))

    os.system('python capture.py -l {} -z 0.6 -r replayAgent  -b replayAgent'.format(layoutPath))

"""
$ python readReplay.py
"""
if __name__ == '__main__':
    replayDir = './replays'
    replayLayoutsDir = './replayLayouts'
    generateLayoutFile(replayDir, replayLayoutsDir)
