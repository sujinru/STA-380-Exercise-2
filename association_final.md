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

    basketrules <- apriori(transactions, parameter=list(support=.001, confidence=.5, maxlen=10))

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##         0.5    0.1    1 none FALSE            TRUE       5   0.001      1
    ##  maxlen target   ext
    ##      10  rules FALSE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 9 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[169 item(s), 9835 transaction(s)] done [0.00s].
    ## sorting and recoding items ... [157 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3 4 5 6 done [0.01s].
    ## writing ... [5668 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

    inspect(head(sort(basketrules, by="lift"),10))

    ##      lhs                        rhs                  support confidence     lift
    ## [1]  {Instant food products,                                                    
    ##       soda}                  => {hamburger meat} 0.001220132  0.6315789 18.99565
    ## [2]  {popcorn,                                                                  
    ##       soda}                  => {salty snack}    0.001220132  0.6315789 16.69779
    ## [3]  {baking powder,                                                            
    ##       flour}                 => {sugar}          0.001016777  0.5555556 16.40807
    ## [4]  {ham,                                                                      
    ##       processed cheese}      => {white bread}    0.001931876  0.6333333 15.04549
    ## [5]  {Instant food products,                                                    
    ##       whole milk}            => {hamburger meat} 0.001525165  0.5000000 15.03823
    ## [6]  {curd,                                                                     
    ##       other vegetables,                                                         
    ##       whipped/sour cream,                                                       
    ##       yogurt}                => {cream cheese }  0.001016777  0.5882353 14.83409
    ## [7]  {domestic eggs,                                                            
    ##       processed cheese}      => {white bread}    0.001118454  0.5238095 12.44364
    ## [8]  {other vegetables,                                                         
    ##       tropical fruit,                                                           
    ##       white bread,                                                              
    ##       yogurt}                => {butter}         0.001016777  0.6666667 12.03058
    ## [9]  {hamburger meat,                                                           
    ##       whipped/sour cream,                                                       
    ##       yogurt}                => {butter}         0.001016777  0.6250000 11.27867
    ## [10] {domestic eggs,                                                            
    ##       other vegetables,                                                         
    ##       tropical fruit,                                                           
    ##       whole milk,                                                               
    ##       yogurt}                => {butter}         0.001016777  0.6250000 11.27867

**Choice of parameters**

We chose support= 0.001 because higher levels of support gave too few
rules for us to inspect. We chose confidence = 0.5 because we want to
make sure that if iem on rhs appears, item on lhs will also appear.
However, this only accounts for how popular the items on rhs are, but
not those on the lhs. If rhs items appear regularly in general, there is
a greater chance that items on the rhs will contain items on the lhs. To
account for this bias, we select our final itemlists based on lift since
lift measures how likely item on lhs is purchased when item rhs is
purchased. Therefore, we sort the items by lift and rank the top 10
rules, which is the result generated by the algorithm.

**Recommendation**

Our final result also makes sense. With this insight, we can give this
to information to store managers to help plan inventory for these
perishable items. Also, this information can be used for product
placement strategy. For example, store manager can put ham, cheese, and
white bread near each other to stimulate more sales
