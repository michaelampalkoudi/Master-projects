# -*- coding: utf-8 -*-
"""
Created on Mon Dec 14 11:52:06 2020

@author: mixmp
"""


import nltk

import re
from nltk import word_tokenize, sent_tokenize
from nltk.corpus import stopwords
from nltk.stem import LancasterStemmer, WordNetLemmatizer

import numpy as np
from nltk.corpus import gutenberg
from collections import Counter
from nltk import ngrams

#############################erwthma1#############################################
print(gutenberg.fileids())
books_10 = [nltk.corpus.gutenberg.words("austen-emma.txt"), nltk.corpus.gutenberg.words("milton-paradise.txt"),
          nltk.corpus.gutenberg.words("chesterton-ball.txt") , nltk.corpus.gutenberg.words("melville-moby_dick.txt") ,
          nltk.corpus.gutenberg.words("edgeworth-parents.txt") , nltk.corpus.gutenberg.words("bryant-stories.txt") ,
          nltk.corpus.gutenberg.words("whitman-leaves.txt"),nltk.corpus.gutenberg.words("chesterton-brown.txt"),
          nltk.corpus.gutenberg.words("bible-kjv.txt"),nltk.corpus.gutenberg.words("shakespeare-hamlet.txt") ]

############For preprocessing##############################3
def to_lowercase(words):
    new_words = []
    for word in words:
        new_word = word.lower()
        new_words.append(new_word)
    return new_words

def remove_digits(words):
    new_words = []
    for word in words:
        new_word = re.sub("\d+", "", word)
        if new_word != '':
            new_words.append(new_word)
    return new_words

def remove_punctuation(words):
    new_words = []
    for word in words:
        new_word = re.sub(r'[^\w\s]', '', word)
        if new_word != '':
            new_words.append(new_word)
    return new_words


def preproccesing(words):
    words = to_lowercase(words)
    words= remove_digits(words)
    words = remove_punctuation(words)
    return words

#def stem_words(words):
    #stemmer = LancasterStemmer()
    #stems = []
    #for word in words:
        #stem = stemmer.stem(word)
        #stems.append(stem)
    #return stems

#def lemmatize_words(words):
    #Lemmatize verbs in list of tokenized words"""
    #lemmatizer = WordNetLemmatizer()
    #lemmas = []
    #for word in words:
        #lemma = lemmatizer.lemmatize(word, pos='v')
        #lemmas.append(lemma)
    #return lemmas

#def stem_and_lemmatize(words):
    #stems = stem_words(words)
    #lemmas = lemmatize_words(words)
    #return stems + lemmas


lists_of_words_of_books = [ [] for _ in range(10) ]#an empty list of 10 empty lists

###applying preprocessing to each list of 10 books words
for i in range(10):
    lists_of_words_of_books[i]=preproccesing(books_10[i])
    #lists_of_words_of_books[i]=stem_and_lemmatize(lists_of_words_of_books[i])



def unique(list1): 
  
    # intilize a null list 
    unique_list = [] 
      
    # traverse for all elements 
    for x in list1: 
        # check if exists in unique_list or not 
        if x not in unique_list: 
            unique_list.append(x)
    return unique_list

#words_of_all_books is the union of the list of words of 10 books
words_of_all_books= lists_of_words_of_books[0]+ lists_of_words_of_books[1]+lists_of_words_of_books[2]+lists_of_words_of_books[3]+ lists_of_words_of_books[4]+lists_of_words_of_books[5]+lists_of_words_of_books[6]+ lists_of_words_of_books[7]+lists_of_words_of_books[8]+lists_of_words_of_books[9]
#dictionary of words of 10 books
unique_words_of_all_books=unique(words_of_all_books)
print("The dictionary of 10 words of the 10 books is:")
unique_words_of_all_books=unique(words_of_all_books)

###############################question2
books_10_raw= [nltk.corpus.gutenberg.raw("austen-emma.txt"), nltk.corpus.gutenberg.raw("milton-paradise.txt"),
          nltk.corpus.gutenberg.raw("chesterton-ball.txt") , nltk.corpus.gutenberg.raw("melville-moby_dick.txt") ,
          nltk.corpus.gutenberg.raw("edgeworth-parents.txt") , nltk.corpus.gutenberg.raw("bryant-stories.txt") ,
          nltk.corpus.gutenberg.raw("whitman-leaves.txt"),nltk.corpus.gutenberg.raw("chesterton-brown.txt"),
          nltk.corpus.gutenberg.raw("bible-kjv.txt"),nltk.corpus.gutenberg.raw("shakespeare-hamlet.txt") ]



for i in range(10):
   books_10_raw[i] = re.sub(r"(''|``)", '',books_10_raw[i] ) # replace double quotes
   books_10_raw[i]  = re.sub(r"(\!\s*)+", '!',books_10_raw[i] ) # replace multiple !
   books_10_raw[i]  = re.sub(r"(\?\s*)+", '?', books_10_raw[i] ) # replace multiple ?
   books_10_raw[i] = re.sub(r'\s+', ' ', books_10_raw[i] ) # replace multiple whitespace by single
   #books_10_raw[i] = re.sub(r'[^\w\s]', '', books_10_raw[i] ) #remove punctuation

########combibing the senteences of 10 books
sentences0 = nltk.tokenize.sent_tokenize(books_10_raw[0])
sentences0=[x.replace('\n',' ') for x in sentences0]


sentences1 = nltk.tokenize.sent_tokenize(books_10_raw[1])
sentences1=[x.replace('\n',' ') for x in sentences1]

sentences2 = nltk.tokenize.sent_tokenize(books_10_raw[2])
sentences2=[x.replace('\n',' ') for x in sentences2]

sentences3 = nltk.tokenize.sent_tokenize(books_10_raw[3])
sentences3=[x.replace('\n',' ') for x in sentences3]


sentences4 = nltk.tokenize.sent_tokenize(books_10_raw[4])
sentences4=[x.replace('\n',' ') for x in sentences4]

sentences5 = nltk.tokenize.sent_tokenize(books_10_raw[5])
sentences5=[x.replace('\n',' ') for x in sentences5]

sentences6 = nltk.tokenize.sent_tokenize(books_10_raw[6])
sentences6=[x.replace('\n',' ') for x in sentences6]

sentences7 = nltk.tokenize.sent_tokenize(books_10_raw[7])
sentences7=[x.replace('\n',' ') for x in sentences7]


sentences8 = nltk.tokenize.sent_tokenize(books_10_raw[8])
sentences8=[x.replace('\n',' ') for x in sentences8]

sentences9 = nltk.tokenize.sent_tokenize(books_10_raw[9])
sentences9=[x.replace('\n',' ') for x in sentences9]

all_sentences=sentences0+sentences1+sentences2+sentences3+sentences4+sentences5+sentences6+sentences7+sentences8+sentences9
# split the sentences into tokens
# each sentence becomes a list of tokens
sentences_to_tokens = []
for sentence in all_sentences:
 words = nltk.tokenize.word_tokenize(sentence)
 sentences_to_tokens.append(words)


def unigram_model(sents):
  unigram_freq = Counter()
  unigram_probs= Counter()
  for sent in sents:
   for word in sent: 
     unigram_freq[word] +=1
  all_word_freq = float(sum(unigram_freq.values()))
  for sent in sents:
   for word in sent:
     unigram_probs[word] = unigram_freq[word] / all_word_freq
  return unigram_freq,all_word_freq,unigram_probs

uni_freq=unigram_model(sentences_to_tokens)
#uni_freq[0] is a dictionary whose keys are  the words of the 10 books and the values are their frequence in the 
#10 books
print(uni_freq[0])
#uni_freq[2] is a dictionary whose keys are the words of the 10 books and values are their probabilities
#of their occurance at the 10 books
print(uni_freq[2])

#bigram dictionary with probabilities
def model_for_bigrams(sents):
	bigram_dict={}
	for sent in sents:
		for word1,word2 in ngrams(sent,2, pad_left=True,pad_right=True,left_pad_symbol='<s>', right_pad_symbol='</s>'):
			if word1 not in bigram_dict:
				bigram_dict[word1]={}
			if word2 not in bigram_dict[word1]:
				bigram_dict[word1][word2]=0
			bigram_dict[word1][word2]+=1

	for word1 in bigram_dict:	
		total_freq=float(sum(bigram_dict[word1].values()))
		for word2 in bigram_dict[word1]:
			bigram_dict[word1][word2]/=total_freq
	return bigram_dict

dictionary_bigram_probs=model_for_bigrams(sentences_to_tokens)
print("The dictionary for bigrams and their probabilities is:","\n")
dictionary_bigram_probs

#trigram dictionary with probabilities
def model_for_trigrams(sents):
	trigram_dict={}
	for sent in sents:
		for word1,word2,word3 in ngrams(sent,3, pad_left=True,pad_right=True,left_pad_symbol='<s>', right_pad_symbol='</s>'):
			if (word1,word2) not in trigram_dict:
				trigram_dict[(word1,word2)]={}
			if word3 not in trigram_dict[(word1,word2)]:
				trigram_dict[(word1,word2)][word3]=0
			trigram_dict[(word1,word2)][word3]+=1

	for (word1,word2) in trigram_dict:
		total_freq=float(sum(trigram_dict[(word1,word2)].values()))
		for word3 in trigram_dict[(word1,word2)]:
			trigram_dict[(word1,word2)][word3]/=total_freq
	return trigram_dict

dictionary_trigram_probs= model_for_trigrams(sentences_to_tokens)
print("The dictionary for trigrams and their probabilities is:","\n")
dictionary_trigram_probs



#################3 question/Generating sentences
def bigram_sentence_generator(bigram_prob):
    present_word = "<s>"
    create_sentence = [present_word]
    while present_word != "</s>":
        former_word = present_word
        former_word_probs = bigram_prob[former_word]
        #random selecting of the next word
        present_word = np.random.choice(list(former_word_probs.keys()), p=list(former_word_probs.values()))
        create_sentence.append(present_word)
    create_sentence= " ".join(create_sentence[1:-1])
    return create_sentence

bigram_sentence_generator(dictionary_bigram_probs)


def generate_sentence_tri(trigram_probs):
    present_word1 = "<s>"
    present_word2 = "<s>"
    create_sentence = [present_word1,present_word2]
    while create_sentence[-1] != "</s>":
        # get counts for previous word
        former_word1 = present_word1
        former_word2 = present_word2
        former_word_probs = trigram_probs[former_word1,former_word2]
        present_word1=former_word2
        present_word2 = np.random.choice(list(former_word_probs.keys()), p=list(former_word_probs.values()))
        create_sentence.append(present_word2)
        
    create_sentence = " ".join(create_sentence[2:-1])
    return create_sentence

generate_sentence_tri(dictionary_trigram_probs)

#creating 10 sentences from bigram probabilities
bigram_sentence_generator(dictionary_bigram_probs)
sentences_for_bigrams=[]
for i in range(10):
   sentences_for_bigrams.append(bigram_sentence_generator(dictionary_bigram_probs))
   print(bigram_sentence_generator(dictionary_bigram_probs))
   print('\n') 
   

#creating 10 sentences from trigram probabilities
sentences_for_trigrams=[]
for i in range(10):
   sentences_for_trigrams.append(generate_sentence_tri(dictionary_trigram_probs))
   print(generate_sentence_tri(dictionary_trigram_probs))
   print('\n') 
   

