library(baseballr)
library(tidyverse)
library(jsonlite)

runsPerInning <- fromJSON("runs_per_inning.json") %>% as.data.frame

calculate_expected_runs <- function(num_outs, first, second, third) {
    exp_runs <- runsPerInning %>%
        mutate (
            freq = count / outs_category_total
        ) %>%
        filter(
            outs == ifelse(is.na(num_outs), 0, num_outs),
            first_base == first,
            second_base == second,
            third_base == third
        )
    print(glue::glue("Outs: {num_outs} / Runners: {first} | {second} | {third}"))
    ev <- ifelse(nrow(exp_runs) == 0, 0, weighted.mean(exp_runs$runs, exp_runs$freq))
    return(ev)
}

vec_exp_runs <- Vectorize(calculate_expected_runs)

plays <- baseballr::get_ncaa_baseball_pbp("http://stats.ncaa.org/contests/2016478/box_score")

positions <- c(
    "1b","2b","3b","c","lf","cf","rf","dh","dp"
)
position_change_match = paste(positions,collapse="|")

summary <- plays %>%
    filter(
        grepl("R:|H:|LOB:", description, ignore.case = FALSE) == FALSE,
        grepl("pinch hit",description, ignore.case = TRUE) == FALSE
           ) %>%
    group_by(inning, inning_top_bot) %>%
    mutate(
        substitution = str_extract(description, glue::glue("to ({position_change_match})(\\.| for)")),
        single = grepl(" singled", description, ignore.case = TRUE),
           walk = grepl(" walked ", description, ignore.case = TRUE),
           balk = grepl(" balk", description, ignore.case = TRUE),
           steal_attempt = grepl(" steal|stole", description, ignore.case = TRUE),
            caught_stealing = grepl("caught stealing", description, ignore.case = TRUE),
            picked_off = grepl("picked off", description, ignore.case = TRUE),
            successful_steal = (steal_attempt & !caught_stealing & !picked_off),
           double = grepl(" doubled ", description, ignore.case = TRUE),
           triple = grepl(" tripled ", description, ignore.case = TRUE),
           is_out = grepl(" out | out.|popped up", description, ignore.case = TRUE),
           outs = cumsum(is_out),
            advanced_first = grepl("advanced to first", description, ignore.case = TRUE),
            advanced_second = grepl("advanced to second", description, ignore.case = TRUE),
            advanced_third = grepl("advanced to third", description, ignore.case = TRUE),
        substitution = case_when(
            is_out ~ FALSE,
            TRUE ~ !is.na(substitution)
        ),
        pitches = str_extract(description, glue::glue("\\(\\d-\\d\\s?(\\w+)*\\)")),# get pitch sequence
        pitch_sequence = str_extract(pitches, "[A-Z]+"),
        num_pitches = ifelse(is.na(pitch_sequence), 0, nchar(pitch_sequence))
           ) %>%
    mutate(lag_is_out = lag(is_out),
           lag_outs = lag(outs),
           first_base = walk | single | balk | advanced_first,
           lag_first_base = lag(first_base),
           second_base = double | advanced_second | (single & lag_first_base),
           lag_second_base = lag(second_base),
           third_base = triple | advanced_third | (double & lag_first_base) | (single & lag_second_base),
           exp_runs_before = vec_exp_runs(lag_outs, first_base, second_base, third_base),
           exp_runs_after = case_when(
               row_number() == n() ~ 0,
               TRUE ~ lead(exp_runs_before)
           ),
           exp_runs_added = exp_runs_after - exp_runs_before
           ) %>%
    ungroup() %>%
    filter(substitution == FALSE) %>%
    select(-pitches)

## Runs left on base
final_exp_runs_lob <- summary %>%
    filter(substitution == FALSE) %>%
    group_by(inning, inning_top_bot) %>%
    filter(row_number() == n()) %>%
    ungroup()

aggregate(final_exp_runs_lob$exp_runs_before, by=list(batting=final_exp_runs_lob$batting), FUN=sum)

## pitches per AB
aggregate(summary$num_pitches, by=list(batting=summary$batting), FUN=mean)

## Expected Runs Added totals
aggregate(summary$exp_runs_added, by=list(batting=summary$batting), FUN=sum)



