factAncillaryBilled <- read.csv("factAncillaryBilled.csv")

#most common items
ItemSkey_frequency <- factAncillaryBilled %>%
  group_by(ItemProductCategory) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

library(arules)
ancillaryTransactions <- read.transactions("factAncillaryBilled.csv", 
                                           format = "single",
                                           sep = ",",
                                           cols = c("ClaimSKey", "PKSourceItem"),
                                           header = TRUE)

summary(ancillaryTransactions)

#set support and confidence limits
rules <- apriori(ancillaryTransactions, parameter = list(supp=0.001, conf=0.5, 
                                                         target = "rules", maxlen = 2))

summary(rules)

basket_rules <- sort(rules, by = 'confidence', decreasing = TRUE)
inspect(basket_rules)

rules <- rules[!is.redundant(rules)]

out <- cbind(labels = labels(rules), quality(rules))
