Capstone - Sentence Completion
========================================================
author: Willy Heng
date: 14 Dec 2018
autosize: true

How to use the app
========================================================

1. Enter the text input
2. Wait up to 3 secs for the prediction to be calculated
3. The graph output shows the total correlation scores of each of the possible predicted words

Description of algorithm [1/2]
========================================================

### Pre-calculation

1. Randomly sample 100,000 entries from Blogs, News and Twitter feeds each
2. Calculate 2 to 4-grams using the entries
3. Calculate the pairwise correlation between individual words by observing their co-occurrence across entries

Description of algorithm [2/2]
========================================================

### In app calculation

4. When user keys in input, the last 3 words are used to search in the precalculated ngrams, what are the possible predicted words
5. The words from the entire input is used to calculate the total pairwise correlation score of each predicted word
6. The word with the highest correlation is then selected as the prediction


Thank you
========================================================

Thank you for using my program and grading my assignment!
