---
title: "hutch: local code reviews in emacs for the mildly disenfranchised"
date: 2026-10-05
---

I haven't had a real job in four years. I closed down a [startup](https://tramline.app) I'd been building, just last month. During all those years, I spent most of my time at the back end of the frontier of AI agents. But I have finally caught up, now that coding agents have really picked up over the last [500 days](https://en.wikipedia.org/wiki/List_of_large_language_models#2025). They are genuinely more productive than, previously, [instructed](https://www.youtube.com/watch?v=U_cSLPv34xk).

Even though I still prefer the pedagogical aspect of AI over the task-completing automaton, the latter is where most of my work happens. Since we've collectively realized that simply shooting code out the door isn't necessarily wise, we now have background agents reviewing code too. The typical review agent party-line is: agents jump in, before your colleagues do, bury twenty pull requests in logorrhea before you have had a chance to wake up and look at your phone. This works, sometimes, for some people. But if you're like me, you still have humans reviewing code before it ships to users, and it's better to respect those people and their time. This is the case, regardless of where you sit on the balance of game-changer to curmudgeon.

All that is to say, no matter which direction agents take to get better with time, I hope we still _care_ about things. Not in the way of formalizing care, with high-fidelity agent instructions and prompts or some superior upholding of taste sort of thing, but something as simple as announcing: _hey I'm still here, and I understand all this_.

So as a long-time emacs user, I present yet another attempt at wedging LLMs, agents and coding harnesses, now inside your text buffers (!) with [Hutch](https://github.com/adjaecent/magit-hutch). It's a small, Magit-induced code-review interface that fits a standard Magit commit-push workflow _locally_ and hopefully helps reclaim some load created upstream.

## quick tour

Open up Magit, hit the dispatcher binding (usually `d`) and you'll see a `Hutch code review` action put up next to the DWIM binding. Hutch operates on three different scopes: staged changes, un-pushed changes, and changes between current branch and working branch. By default, it's staged changes only, since that's most useful.

![Staged changes for Hutch](/blog/images/hutch-staged-changes.png)

Once a review starts, you'll see a nice little progress bar in a new `*magit-hutch: code review*` buffer until the findings[^hat-tip-gptel] are complete. This is a read-only buffer, but you can still perform the [pre-bound actions](https://github.com/adjaecent/magit-hutch#usage).

![Hutch reviewing changes](/blog/images/hutch-reviewing.png)

Each finding has a type (suggestion, comment or LGTM), a file name, relevant line numbers, a title and a description. Suggestions additionally have a patch diff. Hutch piggybacks on [Magit](https://magit.vc) and [Transient](https://github.com/magit/transient) to render these menus so it behaves much like its own interface; keyboard-driven sub-menus, diff coloring, and highlighting. Suggestions are special since they can be applied. To mark a suggestion for application, you queue it with `m`.

![Queued fix in Hutch](/blog/images/hutch-queued-fix.png)

Then bulk-apply all queued suggestions with `A`. The application is scope-aware, so if you queue a finding for staged changes, it will apply the fix directly to the staged files.

![Applied fix in Hutch](/blog/images/hutch-applied-fix.png)

That's it! Getting started should hopefully be pretty simple and intuitive for existing emacs users. There are a handful of other interesting things going on behind the scenes, some of which I'll cover in the next few sections.

## patches over comments

A big UX handicap of showing review comments and suggested patches in-buffer is that there is no existing connective tissue of a commenting system. With GitHub, though, the review UI collapses outdated comments on new commits and most review bots sit over the [suggestion](https://docs.github.com/en/pull-requests/how-tos/review-pull-requests/incorporating-feedback-in-your-pull-request#applying-suggested-changes) mechanic if they have changes to suggest.

Hutch is made with a bias towards patches, rather than just prosaic comments. According to the [Aider leaderboard](https://aider.chat/docs/leaderboards) (and through some of my own experiments), the `SEARCH/REPLACE` diffs are a lot more obedient across different models than just asking the model to author correct patches with precise line numbers.

For an [Aider-style diff](https://aider.chat/docs/more/edit-formats.html), you have to ensure there's enough surrounding context for the `SEARCH` to be unique, and ideally also preserve indentation. In Hutch's case, the tool's function schema naturally decomposes the _file, search, and replace_ fields:

```diff
src/utils.clj
<<<<<<< SEARCH
(defn add [a b]
  (+ a b))
=======
(defn add [a b c]
  (+ a b c))
>>>>>>> REPLACE
```

I've noticed that a lot of older (or cheaper) models tend to recall the `SEARCH` block from memory when asked for diffs, instead of copying it verbatim from `read_file`, `read_diff` or `surrounding_context` calls. This invariably botches them entirely. So we get them verified before submission. If `SEARCH` is missing or matches more than once, the finding is downgraded to a plain comment. On a unique hit, Hutch locally creates a unified diff:

![SEARCH/REPLACE blocks are verified against the file, then emitted as a unified diff](/blog/images/hutch-patch-loop.svg "mono")

Once a series of udiffs and comments are rendered, they can be marked and bulk applied. Hutch applies them per-file, lowest hunk first (bottom-up) so line positions are minimally disturbed. Each finding runs its own `git apply` and a bad application marks itself `invalid` so the rest can continue to land.

All this patching and commenting infrastructure pulls its weight, since with only a couple of keystrokes, you hopefully get less reading and parsing work and more actionable triaging. None of this guarantees patches-always of course, and it shouldn't.

With more powerful models, a simpler diffing method might generally work pretty well. But for a tool that's built to work across different and cheaper models, it's essential to be maximally supportive. In general, I feel like a key point of much of the agentic infrastructure we build is to have knobs for optimizing token:cost ratios. This could often mean thorny workarounds for good-enough models.

## barely enough tooling

Hutch has a fairly minimal toolset for pulling context:

1. `read_diff`
2. `read_file`
3. `search_codebase`
4. `surrounding_context`

Out of these, `surrounding_context` is the more interesting one. It wraps over [Tree-sitter](https://batsov.com/articles/2026/02/27/building-emacs-major-modes-with-treesitter-lessons-learned/#why-tree-sitter) and uses grammars that are installed. It works by letting the model widen out to the enclosing definition of a relevant line and further out, as needed. In my tests, the overall read token consumption compared to simply blasting `read_file` was anecdotally lower with comparable levels of review quality[^anecdotal-claim-about-tree-sitter].

All the findings from the model are submitted to the agent at once. On the write side of things, `verify_block` locally verifies diffs, and along with other comments and LGTM notices, submits them through a `submit_review` tool call. `submit_review` itself runs through some post-processing work, like gating hallucinations about files and line numbers, trimming the length of descriptions and downgrading patches to comments if they don't apply cleanly.

Once the submission lands, the output from all this work is persisted durably under `refs/hutch/id` and can be separately committed as a means of sharing (with `magit-post-commit-hook`) or for repainting later. If you squint hard enough, it might appear like a change identifier for a stacked-diff [review tool](https://blog.tangled.org/stacking), but its purpose is to keep reviews in the git tree, rather than identify changesets for human reviews. We don't really care about multi-party human reviews, it's all local.

## evaluating

The one unfortunate part about benchmarking Hutch is how ungainly it is to pull comparison-ready output from text buffers. I initially ran the evals by invoking multiple headless emacsen and tee-ing the agent output before it was rendered, but eventually settled on emitting [Perfetto](https://perfetto.dev) traces and using them as the underlying medium for evals.

![Perfetto trace for a Hutch eval](/blog/images/hutch-perfetto.png)

I haven't seen agents traced through Perfetto elsewhere. This is likely for good reason. They aren't meant for this kind of thing really. They don't have a first-class notion of what a "prompt" or a "tool call" is. It's designed for kernels and browsers and not an abstract system with tons of prose. 

But for a single-player, emacs-local agent, it sort of works. You can answer all kinds of structural questions like _why did this review take 40 rounds?_, _what tools were run in parallel?_, or _how much wall time was spent in reading diffs?_, and so on. But more importantly, it's free and infra-free. If you set `hutch-trace-dir`, it will emit Perfetto traces and you can just load them up on [ui.perfetto.dev](https://ui.perfetto.dev). Easy.

Here's an example to fetch tool calls and their total times. This is the entire pipeline. No dashboards or SDKs required:

```plsql
SELECT
  name                       AS tool,
  COUNT(*)                   AS calls,
  ROUND(SUM(dur) / 1e6, 1)   AS total_ms
FROM slice
WHERE category = 'tool'
GROUP BY name
ORDER BY total_ms DESC;

-- tool                 calls  total_ms
--------------------------------------
-- search_codebase      18     4210.3
-- read_diff            7      2103.1
-- surrounding_context  12     880.5
-- read_file            3      412.7
-- submit_review        1      42.9
```

With this set up, we take a mix of strategies from [Martian’s code review](https://github.com/withmartian/code-review-benchmark) benchmark and the [CR-Bench preprint](https://arxiv.org/html/2603.11078v1) and compute Precision, Recall, and Fβ scores. The evals are described in more detail[^explain-eval-nuances] in the [eval/README.org](https://github.com/adjaecent/magit-hutch/blob/main/eval/README.org) section. But broadly, we run the bench against 40 PRs, 132 goldens, and use GPT 5.2 as a classifying judge. The eval pipeline goes off and runs queries directly on the traces. Looking at the numbers, I believe we land somewhere around the #16 mark on Martian’s Offline Benchmark [leaderboard](https://codereview.withmartian.com/?mode=offline), which is pretty competitive for a no-memory, single-shot agent.

Outside of classified scoring, there are a few interesting things about the agent itself:

![Stacked bar chart of distinct goldens hit per model, split into unique, shared with one other model, and shared with both. opus-4.8: 15 unique, 17 shared with one, 18 shared with both (50 total). glm-5.2: 13, 19, 18 (50). gpt-5.5: 6, 16, 18 (40). The union across all three is 78.](/blog/images/hutch-complementarity.svg)

Different models tend to catch different bugs. Out of 132 goldens, each model hits 40-50 goldens, with an overlap of 18 hits across all three models. Which means hypothetically, if all three ran combined, it would catch ~55% more bugs than one model alone.

Pretty lousy agreement across the models on what a bug is, I'd say.

![Three bar charts of agent rounds per PR across 40 PRs, sorted ascending. opus-4.8: median 8 rounds, max 26. glm-5.2: median 11, max 39. gpt-5.5: median 35, max 81, with several PRs near the 80-round limit.](/blog/images/hutch-rounds-by-model.svg)

GPT 5.5 tends to hit my default round limit (80) a lot more than the other models for roughly the same hit rate. Opus 4.8 takes 3x fewer turns to complete.

![Three bar charts of output tokens per useful finding, one bar per PR, sorted ascending. opus-4.8: 31 PRs, median about 2,600, max about 13,000. glm-5.2: 34 PRs, median about 3,600, max about 15,000. gpt-5.5: 34 PRs, median about 3,000, max about 27,500.](/blog/images/hutch-tokens-by-model.svg)

On token efficiency, Opus is much cheaper on output tokens used per good finding by a respectable margin, but burns 3x more context on inputs, possibly due to the growing context Hutch resends each round.

## dead on arrival

This is probably all too late, as I've been told. No one really writes or reviews code, uses editors or version control by hand anymore. I made this for myself and for workflows that I still practice. I don't want to purport any arguments about whether one should or shouldn't use LLMs with emacs. The tool has more to do with unlocking a certain kind of workflow than the overreach of agents in niche locations.

If this continues to be useful, I'd like to add a conversational mode for every finding (like CodeRabbit) and perhaps maintain a context tree learnt from and committable to the codebase to improve review quality and speed.

[^hat-tip-gptel]: In the example, I use GLM-5.2 as the underlying model, but this is configurable to whatever backend the excellent [gptel](https://github.com/karthink/gptel) project supports.

[^anecdotal-claim-about-tree-sitter]:  The characterization tests and evals are covered under the [evaluating](#evaluating) section, but I haven't yet gotten a chance to verify this claim empirically.

[^explain-eval-nuances]: There are some biases and nuances to consider before treating the hard metrics as truly objective. But I've elided them from the post since they are described in more detail in the [README](https://github.com/adjaecent/magit-hutch/blob/main/eval/README.org).
