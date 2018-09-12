
# coding: utf-8

# In[109]:


from __future__ import division
import editdistance


# In[110]:


f = open ("dict.txt","r")
my_dict = f.readlines()
f.close


# In[111]:


f2 = open("wiki_correct.txt","r")
correctList = []
for words in f2:
    correct = words.strip()
    correctList.append(correct)
f2.close()


# In[112]:


f1 = open("E:\KT\project\wiki_misspell.txt","r")
totalCorrectNum = 0
totalPrecision = 0
bestsList = []
listForTheWord = []
misspellList = []
index = 0
dis = []
for i in range(1,7):
    dis.append(0)
for line in f1:
    misspell = line.strip()
    misspellList.append(misspell)
    bestv = 10000000
    bests = ""
    print "=============================================","\r\n"
    for entry in my_dict:
        thisv = editdistance.eval(misspell,entry.strip())
        if (thisv < bestv):
            bests = entry.strip()
            bestv = thisv
            listForTheWord = []
            listForTheWord.append(bests)
        elif (thisv == bestv):
            bests = entry.strip()
            bestv = thisv
            ## the number '3' below means threshold
            if (len(listForTheWord)<3):
                listForTheWord.append(bests)
    for i in range(1,6):
        if i == bestv:
            dis[i] += 1
    prediction = []
    prediction.append(listForTheWord)
    totalPrecision += len(listForTheWord)
    
    if str(correctList[index]) in str(prediction):
        totalCorrectNum += 1
    
    
    print "Prediction: ",prediction,"\r\n"
    print "Misspell: ",misspell ,"\r\n"
    print "Correct: ",correctList[index] ,"\r\n"
    print "The precision is: ",("%.2f" % (100* totalCorrectNum / totalPrecision)), "%" ,"\r\n"
    print "The recall is: ",("%.2f" % (100* totalCorrectNum / (len(misspellList)))), "%" ,"\r\n"
    for i in range(1,6):
        print "- Distance is ",i,": ", dis[i]
    index += 1
f1.close()

