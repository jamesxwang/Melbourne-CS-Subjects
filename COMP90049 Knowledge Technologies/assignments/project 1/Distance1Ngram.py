
# coding: utf-8

# In[49]:


from __future__ import division
import editdistance
import ngram


# In[50]:


f = open ("E:\KT\project\dict.txt","r")
my_dict = f.readlines()
f.close


# In[51]:


f2 = open("E:\KT\project\wiki_correct.txt","r")
correctList = []
for words in f2:
    correct = words.strip()
    correctList.append(correct)
f2.close()


# In[52]:


f1 = open("E:\KT\project\wiki_misspell.txt","r")
bestsList = []
listForTheWord = []
misspellList = []
dis_1_list = []
dis_1_corr = []
index = 0
for i in range(1,7):
    dis.append(0)
for line in f1:
    misspell = line.strip()
    misspellList.append(misspell)
    bestv = 10000000
    bests = ""
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
    if bestv == 1:
        dis_1_list.append(misspell)
        dis_1_corr.append(correctList[index])
    index += 1
print "dis=1 misspell: ",dis_1_list
print "dis=1 correct : ",dis_1_corr
f1.close()


# In[ ]:



NtotalCorrectNum = 0
NtotalPrecision = 0
NbestsList = []
NlistForTheWord = []
Nindex = 0

for line in dis_1_list:
    Nbestv = 0
    Nbests = ""
    print "=============================================","\r\n"
    for entry in my_dict:
        ## the number '2' below means N
        Nthisv = ngram.NGram.compare(line,entry.strip(),N=2)
        if (Nthisv > Nbestv):
            Nbests = entry.strip()
            Nbestv = Nthisv
            NlistForTheWord = []
            NlistForTheWord.append(Nbests)
        elif (Nthisv == Nbestv):
            Nbests = entry.strip()
            Nbestv = Nthisv
            ## the number '3' below means threshold
            if (len(NlistForTheWord)<3):
                NlistForTheWord.append(Nbests)
    Nprediction = []
    Nprediction.append(NlistForTheWord)
    NtotalPrecision += len(NlistForTheWord)
    
    if str(dis_1_corr[Nindex]) in str(Nprediction):
        NtotalCorrectNum += 1

    print NtotalCorrectNum
    print len(dis_1_list)
    print "Prediction: ",Nprediction,"\r\n"
    print "Misspell: ",line ,"\r\n"
    print "Highest similarity: ",("%.2f" % (100* Nbestv)), "%" ,"\r\n"
    print "Correct: ",dis_1_corr[Nindex] ,"\r\n"
    Nindex += 1
print "The precision is: ",("%.2f" % (100* NtotalCorrectNum / NtotalPrecision)), "%" ,"\r\n"
print "The recall is: ",("%.2f" % (100* NtotalCorrectNum / (len(dis_1_list)))), "%" ,"\r\n"

