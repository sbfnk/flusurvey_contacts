# Notebook

Dated entries, newest first, one per update to a line of inquiry (see `AGENTS.md`). Each entry is tagged with its line of inquiry, for example `[paper]`, and links the issues and pull requests it covers. An entry is never edited once merged; if later work changes what it says, a new entry records that and names the entry it supersedes.

Each entry has three parts: the intent, in a person's own words, tidied for clean language (what they set out to do and why); what was decided or changed, including anything dropped; and what is still uncertain. The last two are brief and only present where there is something to say. What was done belongs in the pull requests the entry links to. The pull request that does the work adds its entry.

## Entries

### 2026-11-18 `[paper]` Scope and plan of the paper

Pull request: [#1](https://github.com/sbfnk/flusurvey_contacts/pull/1).

**Intent.** The Flusurvey contact data from 2011/12 and 2012/13 have never been published. Adding them to the contact-survey-sample-size paper would mean describing an unpublished dataset inside someone else's paper, so the better plan is a separate paper that describes the dataset, applies their variance partitioning as one of its analyses with a reference to their work, and answers the other questions the data can address. The raw data cannot be published, so they have to stay out of the repository.

**Decided or changed.**

- The paper has four outcomes, in this order: description of the dataset, variance partitioning, contacts when ill, and contacts and the risk of illness. `analysis-plan.md` sets them out.
- The variance partitioning uses the code from [contact-survey-sample-size](https://github.com/cirm-epidemic-surveillance-systems/contact-survey-sample-size) as it stands.
- The analysis is in R. The joint model of contacts and illness is written in Julia only if R cannot fit it.
- Predictors of contacts are reported as the shares of variance explained by age, gender and occupation.
- The pipeline that builds the contact dataset is reviewed and fixed in `sbfnk/flusurvey`, one pull request per problem, before any result is final.
