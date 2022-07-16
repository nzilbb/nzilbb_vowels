# Want to create some sample datasets for use in examples.
library(tidyverse)

# First: raw ONZE means and random intercepts for the same speakers.

# I've loaded the anon_ONZE_mean sample to the environment from my vowel space
# post, at https://joshua.wilsonblack.nz/post/visualising-vowel-space-gamms/

# Collect speaker names to get the corresponding intercepts. We have taken
# a random sample of 100 speakers.
onze_speakers <- anon_ONZE_mean_sample %>%
  pull(Speaker) %>%
  unique()

length(onze_speakers)
# [1] 100

# We rename the dataset to onze_means
onze_means <- anon_ONZE_mean_sample

# Set up the data as part of the package.
use_data(onze_means)

# Now we take the corresponding intercepts.
onze_intercepts <- read_csv('C://Users/jbl91/qb_covar/Data/gam_intercepts_tmp_new.csv')

# filter for speakers.
onze_intercepts <- onze_intercepts %>%
  filter(
    Speaker %in% onze_speakers
  )

# This one will be set up so that it is already wide.
use_data(onze_intercepts)

# Second: filtered QB data for sample speakers and imputed intervals for the same.
qb_speakers <- Quakebox_filtered %>%
  pull(Speaker) %>%
  unique()

# We've got 281 speakers we can use here. Want to have a subset of speakers for
# each age category.

Quakebox_filtered %>%
  group_by(participant_age_category) %>%
  summarise(Speakers = n_distinct(Speaker))

# Lets take 11 from each age category

qb_vowels <- Quakebox_filtered %>%
  group_by(participant_age_category) %>%
  filter(!participant_age_category %in% c('85+', 'na')) %>%
  nest() %>%
  mutate(
    speakers = map(data, ~ .x %>% pull(Speaker) %>% unique()),
    sample_speakers = map(speakers, ~ sample(.x, size = 11)),
    sample_data = map2(data, sample_speakers, ~ .x %>% filter(Speaker %in% .y))
  )

sample_speakers <- qb_vowels %>%
  select(
    participant_age_category, sample_speakers
  )

qb_vowels <- qb_vowels %>%
  select(
    participant_age_category, sample_data
  ) %>%
  unnest(sample_data)

qb_vowels %>%
  group_by(participant_gender) %>%
  summarise(
    speakers = n_distinct(Speaker)
  )

qb_vowels %>%
  group_by(participant_nz_ethnic) %>%
  summarise(
    speakers = n_distinct(Speaker)
  )

# About what you'd expect in terms of distribution.

# which variables should we take?

qb_vowels <- qb_vowels %>%
  select(
    Speaker,
    participant_age_category,
    participant_gender,
    participant_nz_ethnic,
    Target.stress,
    Target.celex.frequency,
    Target.segments.start,
    vowel_duration,
    Vowel,
    F1_50,
    F2_50,
    utterance.articulation.rate,
    following_segment_category,
    intensity_max
  ) %>%
  rename(
    stress = Target.stress,
    word_freq = Target.celex.frequency,
    time = Target.segments.start,
    articulation_rate = utterance.articulation.rate,
    amplitude = intensity_max
  )

use_data(qb_vowels, overwrite = TRUE)

# Now take the corresponding imputed intervals for the same dataset.

all_sample_speakers <- unlist(sample_speakers$sample_speakers)

qb_intervals <- qb_imputed %>%
  filter(
    Speaker %in% all_sample_speakers
  ) %>%
  select(
    -n_int, -n_vowels
  ) %>%
  rename(
    formant_value = mean_int,
    articulation_rate = art_int,
    amplitude = amp_int
  )

use_data(qb_intervals)
