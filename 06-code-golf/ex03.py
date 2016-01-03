import re, sys
from collections import Counter as C

s = open("../stop_words.txt").read().split(",")
w = [re.findall("\w{2,}", open(sys.argv[1]).read().lower())
f = C(x for x in w if x not in s)
for x, c in f.most_common(25):
    print x, "-", c
