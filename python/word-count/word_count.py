from collections import Counter
import re


def count_words(sentence):
    return Counter(re.findall(r"[a-z0-9]+(?:'[a-z]{1,2})?", sentence.lower()))
