------------------------------------------------------------------------

output: md\_document
--------------------

    library(arules)

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'arules'

    ## The following objects are masked from 'package:base':
    ## 
    ##     abbreviate, write

    library(pander)

**Processing Data**

Initialize the dataframe to be processed by the apriori algorithm

    itemlists <- data.frame(0,0)
    names(itemlists) <- c("num","item")

Read each line and tag transaction numbers to each item

    #Keep track of the number of line(transaction)

    trans = 0
    for(line in readLines("groceries.txt")){
        trans = trans + 1
        basket <- unlist(strsplit(line,","))
        for(i in 1:length(basket)){
            if((trans == 1) & (i == 1)){
                itemlists[1,] <- c(trans, basket[i])
            }
            else{itemlists <- rbind(itemlists, c(trans, basket[i]))}
        }
    }

Turn number of transaction into a factor

    itemlists$num <- factor(itemlists$num)
    pander(head(itemlists))

<table style="width:38%;">
<colgroup>
<col width="8%" />
<col width="29%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">num</th>
<th align="center">item</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">1</td>
<td align="center">citrus fruit</td>
</tr>
<tr class="even">
<td align="center">1</td>
<td align="center">semi-finished bread</td>
</tr>
<tr class="odd">
<td align="center">1</td>
<td align="center">margarine</td>
</tr>
<tr class="even">
<td align="center">1</td>
<td align="center">ready soups</td>
</tr>
<tr class="odd">
<td align="center">2</td>
<td align="center">tropical fruit</td>
</tr>
<tr class="even">
<td align="center">2</td>
<td align="center">yogurt</td>
</tr>
</tbody>
</table>

The table above shows the head of transaction dataframe before splitting
it by transactions.

apriori algorithm expects a list of baskets in a special format In this
case, one "transaction" of items per user First split data into a list
of items for each transaction

    transactions <- split(x=itemlists$item, f=itemlists$num)
    length(transactions)

    ## [1] 9835

There are 9835 transactions.

    ## Remove duplicates 
    transactions <- lapply(transactions, unique)

    ## Cast this variable as a special arules "transactions" class.
    transactions <- as(transactions, "transactions")

**Applying Apriori Algorithm**

Now run the 'apriori' algorithm

    basketrules <- apriori(transactions, parameter=list(support=.01, confidence=.5, maxlen=10))

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##         0.5    0.1    1 none FALSE            TRUE       5    0.01      1
    ##  maxlen target   ext
    ##      10  rules FALSE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 98 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[169 item(s), 9835 transaction(s)] done [0.00s].
    ## sorting and recoding items ... [88 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3 4 done [0.00s].
    ## writing ... [15 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

    inspect(head(sort(basketrules, by="lift"),10))

    ##      lhs                     rhs                   support confidence     lift
    ## [1]  {citrus fruit,                                                           
    ##       root vegetables}    => {other vegetables} 0.01037112  0.5862069 3.029608
    ## [2]  {root vegetables,                                                        
    ##       tropical fruit}     => {other vegetables} 0.01230300  0.5845411 3.020999
    ## [3]  {rolls/buns,                                                             
    ##       root vegetables}    => {other vegetables} 0.01220132  0.5020921 2.594890
    ## [4]  {root vegetables,                                                        
    ##       yogurt}             => {other vegetables} 0.01291307  0.5000000 2.584078
    ## [5]  {curd,                                                                   
    ##       yogurt}             => {whole milk}       0.01006609  0.5823529 2.279125
    ## [6]  {butter,                                                                 
    ##       other vegetables}   => {whole milk}       0.01148958  0.5736041 2.244885
    ## [7]  {root vegetables,                                                        
    ##       tropical fruit}     => {whole milk}       0.01199797  0.5700483 2.230969
    ## [8]  {root vegetables,                                                        
    ##       yogurt}             => {whole milk}       0.01453991  0.5629921 2.203354
    ## [9]  {domestic eggs,                                                          
    ##       other vegetables}   => {whole milk}       0.01230300  0.5525114 2.162336
    ## [10] {whipped/sour cream,                                                     
    ##       yogurt}             => {whole milk}       0.01087951  0.5245098 2.052747

**Choice of confidence and lift** We chose confidence = 0.5 because we
want to make sure that if iem on rhs appears, item on lhs will also
appear. However, this only accounts for how popular the items on rhs
are, but not those on the lhs. If rhs items appear regularly in general,
there is a greater chance that items on the rhs will contain items on
the lhs. To account for this bias, we select our final itemlists based
on lift since lift measures how likely item on lhs is purchased when
item rhs is purchased. Therefore, we sort the items by lift and rank the
top 10 rules, which is the result generated by the algorithm.

Our final result also makes sense. With this insight, we can give this
to information to store managers to help plan inventory for these
perisable items.
