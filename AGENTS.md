# Working with agents on this repository

Conventions for anyone, human or agent, opening issues or pull requests here. They exist so that a reviewer can tell where a decision is a person's and where it is a model's, and where contributions are invited, without reading every session that produced the work.

## Lines of inquiry

A line of inquiry (a question worth tracking on its own, such as a methods decision or a recurring source of bugs) gets a parent issue. Issues that belong to it are opened as sub-issues of that parent, not as standalone items. GitHub only allows issues as sub-issues, so a pull request joins a line of inquiry by closing one of its sub-issues. This keeps a flat issue list from hiding which items are routine steps and which are the question that matters.

## The `input wanted` label

The label is optional, and most useful where outsiders look for places to contribute. Where every contributor's input is wanted by default it says little. To ask a particular person, @-mention them, assign the item or request their review. Each of these notifies them, which a label does not.

Where it is used, apply `input wanted` to an issue or pull request where outside contributions or opinions are wanted, and remove it once that is no longer the case. Its absence is not a judgement on an item, only silence on whether comment is wanted.

## The notebook

`NOTEBOOK.md` holds dated entries, newest first, each tagged with its line of inquiry (for example `[paper]`) and linked to the issues and pull requests it covers. Each entry has three parts:

1. The intent, in a person's own words, tidied for clean language: what they set out to do and why.
2. What was decided or changed, including anything dropped, briefly and only where there is something to say.
3. What is still uncertain, likewise.

What was done belongs in the pull requests, which the entry links to.

The pull request that does the work adds its entry, and the entry is reviewed with the change. An LLM may draft it, and a person reviews and edits the text. Entries are never edited once merged. If later work changes what an entry says, a new entry records that and names the entry it supersedes.

The notebook is not a replacement for issues and pull requests, which keep the full detail; it is where someone catching up starts.

## Tracking intentions and decisions

While working with a person, an agent keeps track of the intentions and decisions that person expresses, including later revisions. When the work goes public, the agent collates them into a brief, readable statement of where the person ended up, leaving out the back-and-forth, and proposes it as the issue rationale and the notebook entry. The person reviews and edits both.

## Intent lines

Every issue drafted by an LLM opens with a line or two of intent in a person's own words: why this is worth opening, stated by the person who wants it done, not generated. The rest of the issue can be drafted by the LLM. The intent line may be tidied for clean language, but stays the person's own reasoning, not a rewrite of it, and leaves out any mention of other people.

Every pull request that does not close an issue opens the same way: a line or two from a person on why the change is wanted. A pull request that closes an issue already has its intent in that issue and does not need to repeat it.

Example:

> **Intent** (in the author's own words): the counts in the dataset description are stale since the cleaning steps changed.
>
> *The rest of this issue was drafted by an LLM.*

An automated check (`.github/workflows/pr-conventions.yml`) fails a pull request whose description has neither an intent line nor a reference to an issue it closes. It checks for the presence of either, not for whether an intent line reads as genuinely a person's own words; that judgement is left to review.

## Attribution

Text an LLM drafted, in an issue, pull request, or comment, is either posted from the `sbfnk-bot` account or says plainly that an LLM drafted it. Nobody should have to guess, from the text alone, whether a person or a model wrote it.

A pull request names a person's decision only where that person overturned the agent: went against what the agent proposed or did. Everywhere else, a decision is described without naming who made it.

## Evidence in pull requests

A pull request shows evidence that the change does what it says: the command that was run and its output, or the table, figure or counts the change produced. If the results should stay the same, the evidence is the check that they do, such as a rebuilt dataset matching the checksum of the previous one. The raw Flusurvey data cannot be shared, so evidence shows aggregate counts only: no rows of participant data, no participant identifiers, and no table with cells small enough to identify a participant.

## Repeated corrections

When a reviewer makes the same correction twice, the correction goes into this file. Agents follow it from then on.

## Open problem

These conventions do not address unequal access to AI tools across contributors. Someone without access to an agent can still open issues and pull requests in the usual way; they are simply not expected to add an LLM-drafted notebook entry or sub-issue.
