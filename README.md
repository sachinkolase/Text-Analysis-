# Text-Analysis
Peeforming text analysis of independance speech given by PM Mr.Modi in 2018 and 2017 by using tf-idf method and by simple word count method
To perform this anlysis we have multiple packages like tm(), quanteda(),tidytext() in R.But I have used quanteda package for following reasons.
a.quanteda’s and tidytext’s execution times are very similar as both packages rely on the stringi package for tokenization.
  The operation takes much longer in tm than in the other two packages.
b.quanteda is much faster than tm in both feature selection and document-feature matrix construction.

