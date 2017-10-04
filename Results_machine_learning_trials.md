# Results

List of attempts and results 

### 1st Attempt

* Binary labels only [`positive`, `negative`]
* Sample size `n=907` 
* Support features kept as `factors`
* Support features: `count(Vneg)`, `count(neg)`, `count(pos)`, `count(Vpos)`

| 1 | Actual negative | Actual positive | 
| ---- | ------------ | --------------- | 
| predicted negative | 713 | 154 | 
| predicted positive | 0 | 40 | 

```
# 95 percent confidence interval: 0.8041619 0.8540908
# Final probability of success: 0.8302095 

Improvement: NA
```

### 2nd Attempt

* Binary labels only [`positive`, `negative`]
* Sample size `n=907` 
* Support features converted to `numeric`
* Support features: `count(Vneg)`, `count(neg)`, `count(pos)`, `count(Vpos)`

| 2 | Actual negative | Actual positive | 
| ---- | ------------ | --------------- | 
| predicted negative | 711 | 161 | 
| predicted positive | 2 | 33 | 

```
# 95 percent confidence interval:  0.7937209 0.8447525
# Final probability of success: 0.8202867

Improvement: -0.010138
```

### 3rd Attempt

* Binary labels only [`positive`, `negative`]
* Sample size `n=907` 
* Support features converted to `numeric`
* Support features: `count(Vneg)`, `count(neg)`, `count(pos)`, `count(Vpos)`, `hour_num`, `dayofweek_num`

| 3 | Actual negative | Actual positive | 
| ---- | ------------ | --------------- | 
| predicted negative | 711 | 159 | 
| predicted positive | 4 | 35 | 

```
# 95 percent confidence interval:  0.7941674 0.8450989
# Final probability of success: 0.8206821 

Improvement: +0.0003954
```

### 4th Attempt

* Trinary labels [`positive`, `negative`, `neutral`]
* Sample size `n=1205` 
* Support features converted to `numeric`
* Support features: `count(Vneg)`, `count(neg)`, `count(pos)`, `count(Vpos)`, `hour_num`, `dayofweek_num`, `num_words`

| 4 | Actual negative | Actual neutral | Actual positive | 
| ---- | ------------ | -------------- | --------------- |
| predicted negative | 177 | 11 | 8 
| predicted neutral | 537 | 282 | 154
| predicted positive | 1 | 3 | 32

```
# 95 percent confidence interval:  0.3787464 0.4349884
# Final probability of success 0.406639

Improvement: NA (first trinary attempt)
```

### 5th Attempt

* Trinary labels [`positive`, `negative`, `neutral`]
* Sample size `n=1205` 
* Support features converted to `numeric`
* Support features: `count(Vneg)`, `count(neg)`, `count(pos)`, `count(Vpos)`, `num_words`, `net_score`

| 5 | Actual negative | Actual neutral | Actual positive | 
| ---- | ------------ | -------------- | --------------- |
| predicted negative | 180 | 7 | 6 
| predicted neutral | 525 | 284 | 132
| predicted positive | 10 | 5 | 56

```
# 95 percent confidence interval: 0.4033516 0.4600539
# Final probability of success: 0.4315353

Improvement: +0.0248963
```

### 6th(A) Attempt - Uber Dataset

* Trinary labels [`positive`, `negative`, `neutral`]
* Sample size `n=1468`, *additional `neutral` samples tagged* 
* Support features converted to `numeric`
* Support features: `count(Vneg)`, `count(neg)`, `count(pos)`, `count(Vpos)`, `num_words`, `net_score`

| 6 | Actual negative | Actual neutral | Actual positive | Total |
| ---- | ------------ | -------------- | --------------- | ----- |
| predicted negative | 174 | 34 | 16 | 224 (15%)
| predicted neutral | 219 | 807 | 165 | 1191 (81%)
| predicted positive | 13 | 12 | 79 | 104 (7%)
| Total | 406 (27%) | 853 (56%) | 260 (17%)

```
# 95 percent confidence interval: 0.6740394 0.7208483
# Final probability of success: 0.6978275 

Improvement: +26.6%
```

### 6th(B) Attempt - Grabtaxi Dataset

* Trinary labels [`positive`, `negative`, `neutral`]
* Sample size `n=1519`, *additional `neutral` samples tagged* 
* Support features converted to `numeric`
* Support features: `count(Vneg)`, `count(neg)`, `count(pos)`, `count(Vpos)`, `num_words`, `net_score`

| 6 | Actual negative | Actual neutral | Actual positive | Total | 
| ---- | ------------ | -------------- | --------------- | ----- |
| predicted negative | 18 | 44 | 175 | 237 (16%)
| predicted neutral | 113 | 740 | 179 | 1032 (70%)
| predicted positive | 137 | 33 | 29 | 199 (14%)
| Total | 268 (20%) | 817 (55%) | 383 (25%)

```
# 95 percent confidence interval: 0.6928107 0.7395618
# Final probability of success: 0.7166213 

Improvement: +28.5%
```

### 7th(A) Attempt - Uber Dataset

* Trinary labels [`positive`, `negative`, `neutral`]
* Sample size `n=1391`,*removed non-timeperiod data*
* Support features converted to `numeric`; sentiments should be as `factor`
* Support features: `count(Vneg)`, `count(neg)`, `count(pos)`, `count(Vpos)`, `num_words`, 
 `mean sentiment score` (AFINN), `SentimentR` 

| 7 | Actual negative | Actual neutral | Actual positive | Total |
| ---- | ------------ | -------------- | --------------- | ----- |
| predicted negative | 131 | 29 | 11 | 171 (15%)
| predicted neutral | 238 | 749 | 140 | 1127 (81%)
| predicted positive | 7 | 16 | 79 | 102 (7%)
| Total | 376 (27%) | 794 (57%) | 221 (16%)

```
# 95 percent confidence interval: 0.6577817 0.7073668
# Final probability of success: 0.6829619 
```
### 7th(B) Attempt - Grabtaxi Dataset

* Trinary labels [`positive`, `negative`, `neutral`]
* Sample size `n=1237`, *removed non-timeperiod data*
* Support features converted to `numeric`; sentiments should be as `factor`
* Support features:  `count(Vneg)`, `count(neg)`, `count(pos)`, `count(Vpos)`, `num_words`, 
`mean sentiment score` (AFINN), `SentimentR` 

| 7 | Actual negative | Actual neutral | Actual positive | Total | 
| ---- | ------------ | -------------- | --------------- | ----- |
| predicted negative | 33 |  6 |  13 | 52 (4%)
| predicted neutral | 162 | 698 | 222 | 1082 (87%)
| predicted positive | 12 | 12 | 79 | 103 (8%)
| Total | 207 (17%) | 716 (58%) | 334 (27%)

```
# 95 percent confidence interval: 0.6275680 0.6813139
# Final probability of success: 0.65481
```

> Note to self: Appears that counts of Vneg, Vpos scores are pulling machine down. Will remove.

### 8th(A) Attempt - Uber Dataset

* Trinary labels [`positive`, `negative`, `neutral`]
* Sample size `n=1391`,*removed non-timeperiod data*
* Support features converted to `numeric`; sentiments should be as `factor`
* Support features: `num_words`, `mean sentiment score` (AFINN), `SentimentR` 

| 8 | Actual negative | Actual neutral | Actual positive |
| ---- | ------------ | -------------- | --------------- |
| predicted negative | 57 | 17 | 8 |
| predicted neutral | 126 | 659 | 137 | 
| predicted positive | 24 | 40 | 169 |

```
# 95 percent confidence interval: 0.6894006 0.7404522
# Final probability of success: 0.7154406 
```

### 8th(B) Attempt - Grabtaxi Dataset

* Trinary labels [`positive`, `negative`, `neutral`]
* Sample size `n=1237`, *removed non-timeperiod data*
* Support features converted to `numeric`; sentiments should be as `factor`
* Support features: `num_words`, `mean sentiment score` (AFINN), `SentimentR` 

| 8 | Actual negative | Actual neutral | Actual positive | 
| ---- | ------------ | -------------- | --------------- | 
| predicted negative | 192 |  53 |  14 | 
| predicted neutral | 168 | 666 | 101 |
| predicted positive | 16 | 75 | 106 |

```
# 95 percent confidence interval: 0.6680392 0.7171959
# Final probability of success: 0.6930266 
```

# Conclusion

* `Naive Bayes` skewed towards neutral labels rather than pos or neg, makes ideal choice (cautious)
* Keep support features to `factors` where possible; (eg. for standard features like `days of week` with only 7 options)
* For other support features, leave as numeric
* `MAX_ENTROPY` model shows most promise, but can't seem to make it work with trinary labels. 

Baseline: [This detailed comparison](https://github.com/trinker/sentimentr#comparing-sentimentr-syuzhet-meanr-and-stanford) 
between package accuracies on the 3 most basic industry datasets: IMDB movie reviews, Amazon.com, Yelp.com. 
Package accuracy ranges from `55%` to `80%`, but hover around `60-75%`. Having trinary results that fall within 
this range would be satisfactory. 

Overall mostly satisfied with model 8's accuracy considering that trinary labels have 33% chance of guessing correct; 
model prediction doubles that performance and allows for scale over big datasets.
