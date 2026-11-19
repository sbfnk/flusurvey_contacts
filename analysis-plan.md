# Analysis plan: social contacts in Flusurvey, 2011/12 and 2012/13

Draft for comment. Changes to this plan are agreed before the analysis changes.

## Research question

How much of the variation in daily social contacts lies between people and how much within the same person from week to week, and how do a person's contacts change when they have respiratory symptoms, and do people with more contacts fall ill more often?

## Background

Contact surveys usually ask each participant about a single day, so they cannot separate differences between people from day-to-day variation in the same person. Flusurvey, the UK's online participatory surveillance cohort, asked participants in the 2011/12 and 2012/13 seasons to report their contacts on the previous day alongside their weekly symptom report. The result is 12,215 contact reports from 2,845 participants (provisional until the pipeline fixes in `sbfnk/flusurvey` are merged), many with repeated reports and with symptom status recorded on the same day. These data have not been described or published. A recent method partitions the variance in contact rates into within-person and between-person parts using repeated measurements from France and Hong Kong ([contact-survey-sample-size](https://github.com/cirm-epidemic-surveillance-systems/contact-survey-sample-size); paper in preparation), and the same method applied here gives a third population and a comparison between conversational and physical contacts.

## Methods

Data: Flusurvey background, symptom and contact surveys for 2011/12 and 2012/13. Contact reports give the number of conversational and physical contacts on the previous day by setting (home, work, other) and age band (0-4, 5-18, 19-44, 45-64, 65+). The raw data cannot be shared, so the repository holds code only and the paper reports every cleaning step with counts.

Outcomes, in the order used throughout the paper:

1. Description of the dataset: participants, reports per participant, contacts by setting and age band, and comparison of participants with the UK population.
2. Variance partitioning: a Poisson mixed model with random effects for age, gender, occupation, participant and observation, fitted with `lme4` using the code in the [contact-survey-sample-size](https://github.com/cirm-epidemic-surveillance-systems/contact-survey-sample-size) repository, partitioned into within-person variance, between-person variance explained by each covariate, and unexplained between-person variance. Fitted to conversational and physical contacts, with France and Hong Kong for comparison.
3. Contacts when ill: within-person comparison of contacts on days with and without respiratory symptoms, using illness status rebuilt from the same-day symptom report and symptom-based ARI and ILI definitions.
4. Contacts and the risk of illness: a joint model of contact reports and symptom reports in which each participant's usual contact rate, estimated from their symptom-free days, enters their rate of new ARI and ILI episodes, adjusted for age, season, calendar time and children in the household. The model is written in R if `brms` or Stan can fit it and in Julia otherwise.

Sensitivity analyses: weekday of the contact day added to the model (most reports are filed on the same weekday), first two reports per participant only, proxy reports excluded, and the April 2013 reports included or excluded.

## Results

Planned figures and tables:

- Table 1: participants and reports by season, age group, gender, occupation and household size, against UK population figures.
- Figure 1: reports over time by season, and the number of reports per participant.
- Figure 2: distribution of daily contacts by type and setting, and mean contacts by participant age and contact age band.
- Figure 3: variance partition for Flusurvey conversational and physical contacts next to France and Hong Kong.
- Figure 4: contacts by illness status, overall and by setting, from the within-person model.
- Figure 5: rate of new ARI and ILI episodes by usual contact rate, from the joint model.
- Table 2: joint model estimates of the rate ratio of new ARI and ILI episodes per doubling of usual contacts, by contact type and setting.
- Supplementary table: number of reports removed at each cleaning step.
