
# coding: utf-8

# In[10]:


from __future__ import division
import ngram


# In[11]:


f = open ("dict.txt","r")
my_dict = f.readlines()
f.close


# In[12]:


f2 = open("wiki_correct.txt","r")
correctList = []
for words in f2:
    correct = words.strip()
    correctList.append(correct)
f2.close()


# In[13]:


f1 = open("wiki_misspell.txt","r")
totalCorrectNum = 0
totalPrecision = 0
bestsList = []
listForTheWord = []
misspellList = []
index = 0

for line in f1:
    misspell = line.strip()
    misspellList.append(misspell)
    bestv = 0
    bests = ""
    print "=============================================","\r\n"
    for entry in my_dict:
        ## the number '2' below means N
        thisv = ngram.NGram.compare(misspell,entry.strip(),N=2)
        if (thisv > bestv):
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
    prediction = []
    prediction.append(listForTheWord)
    totalPrecision += len(listForTheWord)
    
    if str(correctList[index]) in str(prediction):
        totalCorrectNum += 1

    print "Prediction: ",prediction,"\r\n"
    print "Misspell: ",misspell ,"\r\n"
    print "Highest similarity: ",("%.2f" % (100* bestv)), "%" ,"\r\n"
    print "Correct: ",correctList[index] ,"\r\n"
    print "The precision is: ",("%.2f" % (100* totalCorrectNum / totalPrecision)), "%" ,"\r\n"
    print "The recall is: ",("%.2f" % (100* totalCorrectNum / (len(misspellList)))), "%" ,"\r\n"
    index += 1

f1.close()

