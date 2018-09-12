A simple spelling correction system base on edit distance.  
This is project 1 of [Knowledge Technologies (COMP90049) Semester 2, 2018](https://handbook.unimelb.edu.au/2018/subjects/comp90049), University of Melbourne.

### Environment  

Python Version: 2.7.10  
External Packages:
+ [python-editdistance](https://pypi.org/project/editdistance/). `pip install editdistance`
+ [python-ngram](https://pypi.org/project/ngram/). `pip install ngram`  

### How to use?  
Levenshtein Distance: `python GlobalEditDistance`  
N-Gram: `python NGram`   
N-Gram for LD = 1: `python Distance1Ngram`

### Dataset  
- dict.txt: This is a list of approximately 370K English entries, which should
    comprise the dictionary for your approximate string search method(s). This
    dictionary is a slightly-altered version of the data from:
    [https://github.com/dwyl/english-words](https://github.com/dwyl/english-words)
    The format of this file is one entry per line, in alphabetical order.
    You may use a different dictionary if you wish; if so, you should state
    the data source and justification in your report.

- wiki_misspell.txt: This is a list of 4453 tokens that have been identified 
as common errors made by Wikipedia editors. It has been scraped from the
following page:
[https://en.wikipedia.org/wiki/Wikipedia](https://en.wikipedia.org/wiki/Wikipedia):Lists_of_common_misspellings
The format of this file is one misspelling per line, in alphabetical
order.

- wiki_correct.txt: This is a list of the truly intended spellings of the 
corresponding misspelled tokens from wiki_misspell.txt - again, one item
per line.