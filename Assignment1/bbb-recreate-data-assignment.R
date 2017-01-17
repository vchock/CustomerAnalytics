## Recreate the BBB data (individual assignment)

## The goal is to recreate the bbb.rda data exactly from its components
## Investigate each of the files loaded below and determine what data
## transformations are needed and how the data should be combined into a
## data.frame you should name 'bbb_rec'
## The results should be completely reproducible (i.e., generate the same
## results on another computer) and, of course, you cannot 'copy' any
## information from bbb into bbb_rec
## The final coding step will be to check that your code produces a data.frame
## identical to data.frame in the bbb.rda file using the all.equal command
## When you are done save your code and commit and push the results to gitlab
## using the Git tab in Rstudio. Of course you can commit and push code as
## often as you like before the due date

## make all required libraries available
library(radiant)
library(dplyr)
library(reshape2)
## finding the path to the class dropbox folder
fp <- file.path(find_dropbox(),"MGTA455-2017")

## loading the original data so we can compare
load(file.path(fp, "data/bbb.rda"))

## view the data description of the original data so you can determine what
## needs to be created (result will pop-up in the Viewer tab in Rstudio)
describe(bbb)

## loading the component datasets
load(file.path(fp, "data/bbb_demographics.rda"))
load(file.path(fp, "data/bbb_purchase.rda"))
load(file.path(fp, "data/bbb_nonbook.rda"))
load(file.path(fp, "data/bbb_buyer.rda"))

## add the zip3 variable

## use the following reference date (i.e., "today" for the analysis)
start_date <- as_ymd("2010-3-8")

## call this function to calculate the difference in months between "today" and
## the first (last) date on which a customer purchased a product
diff_months <- function(date1, date2) {
  y <- year(date1) - year(date2)
  m <- month(date1) - month(date2)
  y * 12 + m
}

## generate the required dplyr code below ...
View(head(bbb))
View(head(bbb_buyer))
View(head(bbb_demographics))
View(head(bbb_nonbook))
View(head(bbb_purchase))

# adding demographics and zip3
bbb_rec <- bbb_demographics
bbb_rec$zip3 <- substr(bbb_rec$zip,0,3)

# adding first and last columns
#arranging by date and account number in ascending order
bbb_purchase1 <- bbb_purchase[order(as.Date(bbb_purchase$date, format="%Y/%d/%m")),]
bbb_purchase1 <- bbb_purchase1[with(bbb_purchase1, order(acctnum)), ]
# ensuring that the final file also is ordered by account number in ascending order
bbb_rec <- bbb_rec[with(bbb_rec, order(acctnum)), ]
# taking the entire row for the first occurrence of the account number
t.first <- bbb_purchase1[match(unique(bbb_purchase1$acctnum), bbb_purchase1$acctnum),]
# calculating month difference with the first dates
first <- diff_months(start_date,t.first$date)
bbb_rec$first <- first
View(head(bbb_rec))

# arranging date column alone in the descending order and following the same process
bbb_purchase1 <- bbb_purchase[order(as.Date(bbb_purchase$date, format="%Y/%d/%m"), decreasing = TRUE),]
bbb_purchase1 <- bbb_purchase1[with(bbb_purchase1, order(acctnum)), ]
bbb_rec <- bbb_rec[with(bbb_rec, order(acctnum)), ]
t.first <- bbb_purchase1[match(unique(bbb_purchase1$acctnum), bbb_purchase1$acctnum),]
last <- diff_months(start_date,t.first$date)
bbb_rec$last <- last
View(head(bbb_rec))

# adding price for books
book <- aggregate(bbb_purchase$price, by=list(bbb_purchase$acctnum), FUN=sum)[2]
bbb_rec$book <- as.integer(unlist(book))
View(head(bbb_rec))

#adding nonbook
bbb_rec <- merge(x = bbb_rec, y = bbb_nonbook, by = "acctnum", all.x = TRUE)

# adding total

bbb_rec$total <- bbb_rec$book + bbb_rec$nonbook

# adding purch
purch <- aggregate(bbb_purchase$acctnum, by=list(bbb_purchase$acctnum), FUN=length)[2]
bbb_rec$purch <- as.integer(unlist(purch))
View(head(bbb_rec))

#adding child to geog columns
#grouping based on purchase category
grouping <- group_by(bbb_purchase,acctnum,purchase)
purchaseGroup <- summarize(grouping,count=n()) 
#reshaping the dataframe to convert categorical column to its separate columns
recastData <- dcast(purchaseGroup, acctnum  ~ purchase, value = "count",fill=FALSE)

#reordering the columns
recastData <- recastData[c("acctnum","child","youth","cook","do_it","reference","art","geog")]

# left join with bbb_rec dataset
bbb_rec <- merge(x = bbb_rec, y = recastData, by = "acctnum", all.x = TRUE)
View(head(bbb_rec))

# adding buyer and training
bbb_rec <- merge(x = bbb_rec, y = bbb_buyer, by = "acctnum", all.x = TRUE)
View(head(bbb_rec))

## double checking of all columns are of the same type
all.equal(bbb_rec, bbb, check.attributes = FALSE)
