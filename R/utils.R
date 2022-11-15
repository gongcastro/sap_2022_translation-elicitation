# custom ggplot theme
theme_custom <- function(){
    theme_minimal() +
        theme(
            panel.grid = element_line(colour = "grey", linetype = "dotted"),
            axis.line = element_line(colour = "black"),
            text = element_text(size = 12, colour = "black"),
            axis.text = element_text(colour = "black")
        )
}

logit_to_prob <- function(x, variable) ifelse(grepl("intercept", tolower(variable)), plogis(x), x/4)
rescale_values <- function(x, vct) (x*sd(vct))+mean(vct)
scale_values <- function(x, vct) (x-mean(vct))/sd(vct)
# proportion adjusted from boundary values (Gelman, Hill & Vehtari, 2020)
prop_adj <- function(x, n){
    e <- (x+2)/(n+4)
    return(e)
}

# adjusted standard error of proportion (Gelman, Hill & Vehtari, 2020)
prop_adj_se <- function(x, n) {
    e <- (x+2)/(n+4)
    se <- sqrt(e*(1-e)/(n+4))
    return(se)
}

# adjusted standard error of proportion (Gelman, Hill & Vehtari, 2020)
prop_adj_ci <- function(x, n, .width = 0.95) {
    e <- (x+2)/(n+4)
    se <- sqrt(e*(1-e)/(n+4))
    ci <-  e + qnorm(c((1-.width)/2, (1-(1-.width)/2)))*se
    ci[1] <- ifelse(ci[1]<0, 0, ci[1]) # truncate at 0
    ci[2] <- ifelse(ci[2]>1, 1, ci[2]) # truncate at 1
    return(ci)
}


#### Word-level marginal effects

# ```{r}
#| label: fig-coefs-word-joint
#| fig-height: 13
#| fig-width: 5
#| warnings: false
#| messages: false
#| eval: false
# empirical_means <- list(
#     "Study 1" = results$responses, 
#     "Study 2" = results$questionnaire_responses
# ) %>% 
#     bind_rows(.id = "study") %>% 
#     mutate(group = factor(group, levels = c("cat-ENG", "spa-ENG", "cat-SPA"), ordered = TRUE)) %>% 
#     group_by(study, group) %>% 
#     summarise(successes = sum(correct, na.rm = TRUE), n = n(), .groups = "drop") %>% 
#     mutate(
#         prop = prop_adj(successes, n),
#         prop_se = prop_adj_se(successes, n)
#     )
# 
# empirical_accuracy <- list(
#     "Study 1" = results$responses,
#     "Study 2" = results$questionnaire_responses
# ) %>% 
#     bind_rows(.id = "study") %>% 
#     mutate(
#         translation = paste0(translation, " (", percent(lv, accuracy = 1), ")"),
#         group = factor(group, levels = c("cat-ENG", "spa-ENG", "cat-SPA"), ordered = TRUE) 
#     ) %>% 
#     group_by(study, group, translation, lv) %>% 
#     summarise(prop = mean(correct), .groups = "drop")
# 
# nd_re_words <- expand.grid(
#     translation_id = unique(results$fit_5$data$translation_id),
#     nd_std = 0,
#     freq_zipf_2_std = 0,
#     lv_std = 0,
#     group = NA
# )
# 
# questionnaire_nd_re_words <- expand.grid(
#     translation_id = unique(results$questionnaire_fit_5$data$translation_id),
#     nd_std = 0,
#     freq_zipf_2_std = 0,
#     lv_std = 0,
#     group = NA
# )
# 
# post_re <- add_epred_draws(nd_re_words, results$fit_5, ndraws = 50, re_formula = ~ (1 | translation_id))
# questionnaire_post_re <- add_epred_draws(questionnaire_nd_re_words, results$questionnaire_fit_5, ndraws = 50, re_formula = ~ (1 | translation_id))
# 
# post_re_join <- list(
#     "Study 1" = post_re, 
#     "Study 2" = questionnaire_post_re
# ) %>% 
#     bind_rows(.id = "study") %>% 
#     mutate(group = factor(group, levels = c("cat-ENG", "spa-ENG", "cat-SPA"), ordered = TRUE)) %>% 
#     group_by(study, translation_id, nd_std, freq_zipf_2_std, lv_std, group) %>% 
#     summarise(
#         .mean = mean_qi(.epred)[,1], 
#         .lower = mean_qi(.epred)[,2],
#         .upper = mean_qi(.epred)[,3],
#         .groups = "drop"
#     ) %>% 
#     ungroup() %>% 
#     select(-c(group)) %>% 
#     left_join(select(results$stimuli, group, translation, translation_id, lv)) %>% 
#     mutate(
#         translation = paste0(translation, " (", percent(lv, accuracy = 1), ")"),
#         translation_ord = reorder_within(translation, lv, group),
#         group = factor(group, levels = c("cat-ENG", "spa-ENG", "cat-SPA"), ordered = TRUE)
#     ) %>% 
#     left_join(select(empirical_accuracy, study, group, translation, prop)) 
# 
# post_re_join %>% 
#     ggplot(aes(x = .mean, y = translation_ord, colour = study, fill = study)) +
#     facet_grid(study~group) + 
#     geom_errorbar(aes(xmin = .lower, xmax = .upper),
#                   size = 0.5, width = 0, alpha = 0.25,
#                   position = position_dodge(width = 0.5)) + 
#     geom_point(aes(x = prop), size = 2, shape = 18, alpha = 0.25,
#                position = position_dodge(width = 0.5), colour = "grey") +
#     geom_smooth(
#         aes(group = study), orientation = "y", alpha = 0.25, size = 1,
#         method = "glm", method.args = list(family = "binomial")) +
#     labs(
#         y = "Percentage of correct responses",
#         x = "Item (ordered by Levenshtein distance)",
#         colour = "Study",
#         fill = "Study"
#     ) +
#     scale_x_continuous(limits = c(0, 1), labels = percent) +
#     scale_y_reordered() +
#     theme(
#         legend.position = "top",
#         legend.key = element_rect(fill = NA, colour = NA),
#         axis.text.x = element_blank(),
#         axis.ticks = element_blank(),
#         axis.title = element_text(size = 10),
#         panel.grid.major.x = element_blank(),
#         plot.background = element_rect(fill = "white", colour = NA),
#         panel.grid.major.y = element_line(size = 0.15, linetype = "dotted", colour = "grey")
#     ) +
#     coord_flip()
# ```
# 
# ### Confidence ratings
# 
# ```{r}
# #| label: fig-confidence-knowledge
# #| warning: false
# #| fig-height: 6
# 
# # plot confidence ratings by knowledge
# results$questionnaire_responses %>% 
#     count(knowledge, confidence, group) %>% 
#     replace_na(list(n = 0)) %>% 
#     ggplot(aes(confidence, n, fill = knowledge, colour = knowledge, label = format(n, big.mark = ","))) +
#     facet_wrap(~group) +
#     geom_col(position = position_dodge(width = 1), show.legend = FALSE) +
#     geom_text(aes(y = n+70), position = position_dodge(width = 1), size = 3, show.legend = FALSE, hjust = 0.5) +
#     scale_y_continuous(labels = ~format(., big.mark = ",")) +
#     theme(
#         axis.title.x = element_blank()
#     ) +
#     
#     results$questionnaire_responses %>%  
#     # left_join(select(questionnaire_data_clean, participant_id, word_1, knowledge, confidence)) %>% 
#     count(knowledge, confidence, group) %>% 
#     
#     ggplot(aes(confidence, n, fill = knowledge, colour = knowledge, label = n)) +
#     facet_wrap(~group) +
#     geom_col(position = position_fill()) +
#     scale_y_continuous(labels = percent) +
#     
#     plot_layout(nrow = 2) &
#     labs(x = "Confidence rating", y = "Responses", colour = "Knows the word", fill = "Knows the word") &
#     scale_fill_manual(values = c("#1A85FF", "#ff2976")) &
#     scale_colour_manual(values = c("#1A85FF", "#ff2976")) &
#     scale_x_continuous(breaks = 0:7) &
#     theme_custom() &
#     theme(
#         legend.position = "top",
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor = element_blank()
#     )
# ```