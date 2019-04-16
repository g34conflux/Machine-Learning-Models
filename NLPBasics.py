# -*- coding: utf-8 -*-
"""
Created on Sat Feb  2 12:32:32 2019

@author: aadit
"""
from nltk.tokenize import sent_tokenize, word_tokenize

#corpora and lexicon
#corpora is the paragraph, body of text ex: medical journals, presidential speeches, any text in english language
#Lexicon is the dictionary, investor speak and regular english speak bull: market, bull: scary animal
#

#######################################################################################

                  ################Tokenizing
                  
#######################################################################################
#example_sentence = "Hello Mr. Smith, How are you doing today? Hope all is well with you"
#
#words = word_tokenize(example_sentence)
#
#sent = sent_tokenize(example_sentence)

                  
                  
#######################################################################################

                  ################StopWords
                  
####################################################################################### 

from nltk.corpus import stopwords  
#stopwords - literally come up and give up like sacarstic words, fluffy words like from, of                
#stop_words = (stopwords.words("english"))
#
#print(type(stop_words))
#print(stop_words)
#print(words)
#print(sent)
#
#final_words = []
#for w in words:
#    if w not in stop_words:
#        final_words.append(w)
#print(final_words)
#
#final_words_1 = [w for w in words if w not in stop_words]
#print(final_words_1)

#######################################################################################

                  ################Stemming
                  
#######################################################################################
from nltk.stem import PorterStemmer, LancasterStemmer # for stemming or finding the base word
ps = PorterStemmer()
ls = LancasterStemmer()

example_words = ["Pythonic","Pythonmaniac","Pythoned","Pythoner","Pythonly"]#Lancaster performed better
#example_2 = ["Done","Doing","Don't"]
example_2 = ["civilization", "civility", "civilian"]#base word is civil and Lancaster performs better


#for w in example_words:
#    print(w)
#    print("PorterStemmer : {0} ".format(ps.stem(w)))
#    print("Lancaster : {0}".format(ls.stem(w)))
#for s in example_2:
#    print(s)
#    print("PorterStemmer : {0}".format(ps.stem(s)))
#    print("Lancaster : {0}".format(ls.stem(s)))

#example_sentence = "try trying tried"
#
#for w in word_tokenize(example_sentence):
#    print(w)
#    print("PorterStemmer : {0}".format(ps.stem(w)))
#    print("Lancaster : {0}".format(ls.stem(w)))
#    

#######################################################################################

                  ################POS - Parts of Speech
                  
#######################################################################################
#######################################################################################

                  ################Chunking - making meaning out of the sentence, noun phrases
                  #Named Entity , noun, there can be many nouns in a sentence
                  
#######################################################################################
                  
                  

import nltk                  
from nltk.corpus import state_union # state of union addresses by presidents for the last 70 years
from nltk.tokenize import PunktSentenceTokenizer # unsupervised machine learning tokenizer, its pre trained , you could train it                 
import re
train_text = state_union.raw("2005-GWBush.txt")
sample_text = state_union.raw("2006-GWBush.txt")
custom_sent_tokenizer = PunktSentenceTokenizer(train_text)
tokenized = custom_sent_tokenizer.tokenize(sample_text)
#print(train_text)
summary_sentence = "The boy from Brooklyn quickly kicked his bouncy ball past a defender from Boston, but in his haste he stumbled on the player from Detroit"
tokenized = PunktSentenceTokenizer().tokenize(summary_sentence)
regExp = r'Brooklyn.defender'
match_obj = re.search(regExp,summary_sentence)
if match_obj:
  print(match_obj.group(0))
else:
  print("Match not found")
def process_content():
    try:
        for i in tokenized:
            words = nltk.word_tokenize(i)
            tagged = nltk.pos_tag(words)#pos - parts of speech
            chunkGram = r"""Chunk: {<RB.>*<VB.>*<NN>?} """
            ChunkParser = nltk.RegexpParser(chunkGram)
            chunked = ChunkParser.parse(tagged)
            print(chunked)
            chunked.draw()
#            named_entity = nltk.ne_chunk(tagged, binary=True)
#            print(named_entity)
#            named_entity.draw()
#            help(nltk.ne_chunk)
    except Exception as e:
        print(str(e))
   
#process_content()
        


#######################################################################################

                  ################lemmatizer
                  
#######################################################################################        
from nltk.stem import WordNetLemmatizer

lemmatizer = WordNetLemmatizer()

print(lemmatizer.lemmatize("feet"))#foot
print(lemmatizer.lemmatize("geese"))#goose
print(lemmatizer.lemmatize("better", pos="a"))  #defautl pos is noun
print(lemmatizer.lemmatize("cacti"))#cactus
print(lemmatizer.lemmatize("vertices"))#vertec,
print(lemmatizer.lemmatize("corpora")) #corpus

#######################################################################################

                  ################Corpus raw data
                  
#######################################################################################
from nltk.corpus import gutenberg
from nltk.tokenize import sent_tokenize
sample = gutenberg.raw("bible-kjv.txt")

#sent_tok = sent_tokenize(sample)
#print(sent_tok[5:10])

#######################################################################################

                  ################WordNet - Synonyms, Antonyms, word similarity
                  
#######################################################################################

from nltk.corpus import wordnet
#
#syns = wordnet.synsets("guru")
#print(syns[0])
#print(syns)
##gives the synonym
#print(syns[0].lemmas()[0].name())#program
#print(syns[0].definition())
#
#
#for word in syns:
#    for l in word.lemmas():
#        print("{0} : {1}".format(l.name(),word.definition()))
#        print(word.examples())
        
#syn_set = wordnet.synsets("match")
#synonyms = []
#antonym = []
#for word in syn_set:
#    for l in word.lemmas():
#        synonyms.append(l.name())
#        if l.antonyms():
#            antonym.append(l.antonyms()[0].name())
#            print(l.antonyms()[0].name())
#print(synonyms)
#print(antonym)
#
#w1 = wordnet.synset("ship.n.01")
#w2 = wordnet.synset("boat.n.01")
#
#print(w1.wup_similarity(w2))#.9 similar



#######################################################################################

                  ################Text Classifivation - opinion mining
                  
#######################################################################################
                  
#spam or no spam categories
#sentimental anlysis - positive or negative
                  
#import random
#from nltk.corpus import movie_reviews
#
#documents = [(list(movie_reviews.words(fileid)), category)
#            for category in movie_reviews.categories()
#            for fileid in movie_reviews.fileids(category)]
#        
#        
##print(documents[0:2])
#random.shuffle(documents)
#all_words = []
#for w in movie_reviews.words():
#    all_words.append(w.lower())
#
#
#all_words = nltk.FreqDist(all_words)
##print(all_words.most_common(15))
##print(all_words["brilliant"])
#
#word_features = list(all_words.keys())[:3000]
#
#def find_features(documents):
#    words = set(documents)
#    features = {}
#    for w in word_features:
#        features[w] = (w in words)
#    return features
##print(find_features(movie_reviews.words("neg/cv000_29416.txt")))
#
#featuresets = [(find_features(rev),category) for (rev, category) in documents]
#print(featuresets[0])
#for k, v in (featuresets[0][0].items()):
#    
#    if v == True:
#        print(k)
#        print(v)
#
 ###############################################################################
                        #Bigrams, trigrams and Ngrams

################################################################################
from nltk import bigrams, trigrams, ngrams            
string = "the best and the most beautiful things in the world cannot be seen or even touched, they must be felt with the heart"
quotes_tokens = nltk.word_tokenize(string)

#quotes_bigrams = list(bigrams(quotes_tokens))
#print(quotes_bigrams)
#
#
#quotes_trigrams = list(trigrams(quotes_tokens))
#print(quotes_trigrams)

#
#quotes_Ngrams = list(ngrams(quotes_tokens,4))
#print(quotes_Ngrams)

new = "The big cat ate the little tiny mouse guy who was after the fresh cheese"
tokens = word_tokenize(new)
pos_tags = nltk.pos_tag(tokens)
print(pos_tags)


chunk_tok = nltk.ne_chunk(pos_tags)
print(chunk_tok)

gram = r"Chunk : {<DT>?<JJ>*<NN>*}"#? 0 or 1, * 0 or more

parser = nltk.RegexpParser(gram)
chunk_tokens = parser.parse(pos_tags)
print(chunk_tokens)




#####################################################################################

#############################Processing

#####################################################################################

import pandas as pd
import numpy as np
import os
from sklearn.feature_extraction.text import CountVectorizer
from nltk.corpus import movie_reviews

#print(os.listdir(nltk.data.find("corpora")))


print(movie_reviews.categories())
pos_rev = (movie_reviews.fileids("pos"))
neg_rev = (movie_reviews.fileids("neg"))
rev = movie_reviews.words("pos/cv000_29590.txt")

rev_string = []
for review in pos_rev:
    rev_one_string = " ".join(movie_reviews.words(review))
    rev_one_string = rev_one_string.replace(' ,', ',')
    rev_one_string = rev_one_string.replace(' .', '.')
    rev_one_string = rev_one_string.replace(" \'", "'")
    rev_one_string = rev_one_string.replace("\'", "'")
    rev_string.append(rev_one_string)##
    
for review in neg_rev:
    rev_one_string = " ".join(movie_reviews.words(review))
    rev_one_string = rev_one_string.replace(' ,', ',')
    rev_one_string = rev_one_string.replace(' .', '.')
    rev_one_string = rev_one_string.replace(" \'", "'")
    rev_one_string = rev_one_string.replace("\'", "'")
    rev_string.append(rev_one_string)##


print(len(rev_string))


neg_targets = np.zeros((1000,),dtype=np.int)
pos_targets = np.ones((1000,),dtype=np.int)

target_list = []

for neg in neg_targets:
    target_list.append(neg)
    
for pos in pos_targets:
    target_list.append(pos)
    
print(target_list)


