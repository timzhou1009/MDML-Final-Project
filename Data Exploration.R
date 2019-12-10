
length(unique(final$business_id))



range(final$date)


final <- final %>% mutate(review_count.x = review_count.business)  %>% 
                            select(-review_count.x)