---
name: ashfaq-farooqui-technical-style-profile
description: Extracted writing-style profile from Ashfaq Farooqui's 18 academic papers (2016-2025) plus a full deep-read of the 2021 PhD thesis kappa, used as reference material for the write-technical-like-me skill.
metadata:
  type: reference
---

# Style profile: Ashfaq Farooqui, academic/technical writing

Built from `pdftotext -layout` extraction of 18 papers/reports (2016 ETFA paper through 2025 ICCMA paper, plus the 2021 PhD thesis), reading abstracts, introductions, related-work sections, method descriptions, and conclusions across all of them. This is reference material — see `SKILL.md` for the directive version used at write time.

## 1. Abstract architecture

Consistent five-beat template, almost always in this order:

1. **Domain/problem statement** — one broad sentence establishing why the topic matters.
2. **Gap or challenge** — what's missing or hard about existing approaches.
3. **Contribution statement**, usually opening with "This paper presents/proposes/introduces..." or "We...".
4. **Method in one sentence** — what was actually done (not just claimed).
5. **Result/demonstration** — closes on "we demonstrate", "results show", "feasibility is shown", never a hedge.

Verbatim examples:
- "This paper presents an event-based data pipeline architecture, that can be applied to legacy systems as well as new state-of-the-art systems, to collect data from the factory floor." (01, IJPR 2020)
- "Manual construction of formal models is time-consuming, error-prone, and intractable for large systems." (08, Electronics 2022 — note the tricolon of adjectives, a recurring device)
- "This paper is a preliminary study of the feasibility of automatically obtaining formal models from virtual simulations." (13, CASE 2018)
- "A tool, MIDES, for automatic learning of models and supervisors for discrete event systems is presented." (MIDES_CASE2021 — one of the few abstracts opening with a passive construction, tool name given immediately)
- "We advocate for a more cohesive approach by identifying five areas of mutual support between formal methods and fault injection." (16, arXiv 2023)

Abstracts almost never end with a hedge or a "future work" gesture — that's reserved for the conclusion. The abstract's job is to state what was done and that it worked.

## 2. Introduction opening moves

Nearly every introduction opens with a **broad, citation-light domain claim** — establishing stakes before narrowing. The move is: broad domain sentence → complexity/growth claim → the specific gap.

- "Manufacturing companies are welcoming the ongoing digital revolution and looking towards incorporating Industry 4.0 technologies." (01)
- "Complexity of automotive manufacturing industry is constantly increasing to keep up with advancement in technology, market trends, legislative requirements, and most of all high quality products." (07, 2016 — note near-identical phrasing reused in Ashfaq____CIRP2 two years later: "The complexity of the automotive manufacturing constantly increases to keep up with advancements in technology, market trends, legislative requirements...")
- "In recent years, the global automotive industry has made significant progress towards the development of autonomous vehicles." (08, 2022; reused near-verbatim in Yuvaraj_MODELS2020)
- "Safety- and security-critical systems continue to be integrated into our daily lives." (16, 2023; near-identical opener in CarFASE 2023: "Safety- and security-critical automotive systems continue to be integrated into our daily lives")
- "As connected and automated vehicles (CAVs) become increasingly complex, ensuring their safe and reliable operation requires systematic evaluation of both nominal and edge-case scenarios." (18, 2025)

Notable habit: **self-reuse of opening formulas across papers** — a "Complexity is increasing... this leads to X" template recurs across a decade with light rewording. This is a genuine fingerprint, not a flaw to fix.

## 3. Contribution phrasing

Mixed but leans prose over lists in older/journal papers, explicit bullets in later/short-format papers:
- 2016 ETFA paper: dedicated "A. Contribution" subsection, written as prose paragraph, no bullets.
- CIRP2 (2018): "1.1. Contribution" subsection, prose, referencing a prior paper's findings first.
- WayWiseR (2025, IEEE conf format): bulleted list — "Assessment of an open UAS design for maritime use. / Expert survey on UAS usefulness. / Analysis of legal compliance and SWOT analysis." — short noun-phrase bullets, no verbs, no numbering, introduced by "we present the following contributions:"
- Longer/theoretical papers (LearningSupervisors, MIDES) fold the contribution into the last paragraph of the intro as a "This paper presents/introduces the tool X, that..." sentence rather than a labeled section at all.

Rule of thumb: **short conference/workshop papers get a labeled contribution list (terse noun phrases); journal articles and the thesis fold contributions into intro prose.**

## 4. Sentence-level habits

- **Sentence openers**: "This paper presents/proposes/introduces/describes/addresses/details/bridges" is the dominant contribution-sentence opener (5+ instances of "This paper presents" alone). "We" openers are common mid-paragraph for method/action sentences ("We use", "We then", "We further", "We demonstrate", "We present") — first-person plural is the default voice, not passive.
- Passive constructions appear but are usually reserved for describing artifacts already built ("The approach is realized in the tool MIDES...", "A tool, MIDES,... is presented.") rather than for hedging the contribution itself.
- **Dashes**: correction after direct verification — raw em-dash (—) count across the corpus is only ~52, and nearly all of those are bibliography artifacts (repeated-author marks "——,", IEEE "Abstract—" template boilerplate, page-range rendering), NOT stylistic prose. The real, verified fingerprint is the **spaced en-dash (–)** used as an appositive definitional insertion — 617 raw en-dash hits, and a genuine subset are prose asides, not just page ranges: "Fault tolerant systems – systems that can handle an error without affecting the service delivered – have been studied..."; "Sequence Planner – a tool for modeling and analyzing production systems – and is currently..."; "the modeling formalisms – Operations and Automata – are defined"; "for the three terms – fault, error, and failure –". Use **en-dash (–), not em-dash (—)**, for this pattern — always to define a term or acronym just introduced, never for dramatic pause or as a comma substitute. Most en-dash hits are still bibliography page-ranges (e.g. "87–106"), so don't overuse this in prose — it should read as occasional, not constant.
- **Semicolons** are common (~550 instances) for joining two independent but tightly related clauses, especially in lists of near-synonymous problems: "models are typically created manually and hence are prone to errors. Secondly, once a model is created, tested, and put into use on the factory floor, there is an added effort required to maintain and update it."
- **Tricolon lists** recur constantly, especially of adjectives describing a problem: "time-consuming, error-prone, and intractable"; "expensive, prone to human errors, and even intractable"; "flexibility, efficiency, and development time." This is one of the strongest fingerprints — problems are almost always characterized in exactly three adjectives/clauses.
- **Hedging is light and specific, not generic throat-clearing.** Counts across corpus: "note that" (25, used for genuine asides, not filler), "seems to" (4), "we believe" (3, always attached to a substantive claim, never a filler opener — e.g. "development is a key reason we believe this to be the right time to start investigating..." (16, arXiv); "We believe the most crucial reason..." (thesis)), "to the best of our knowledge"/"to the best of the authors' knowledge" (2-3, used as a specific gap-claim, e.g. "To the best of the authors' knowledge, active automata learning has not been used previously to learn formal models from MATLAB." (08)), "could potentially" (5), "it is likely" (0), "leverage" (~4 in the thesis kappa, technical-verb sense only — see §9 correction). When hedging, preferred forms are "may" + verb ("may have catastrophic consequences") and "is expected to" — not "we believe" or "arguably." Limitations in conclusions are stated as direct fact, not softened: "the validation of the learnt model was admittedly rather superficial, visual inspection and comparison..." (Yuvaraj/08) — "admittedly" substitutes for hedging here, a genuine personal tic worth reusing.
- **Connective words** (precise grep counts across corpus): "However" 230, "Hence" 130, "Thus" 110, "Furthermore" 87, "Additionally" (also common) — these four are the workhorse connectives and dominate over "Therefore" (52, present but secondary) and "Moreover" (17, a distant, deliberate minor variant — never filler). Never "So," as a sentence opener.
- **"To this end"** is a signature transition (23 occurrences) bridging a stated problem to the paper's response to it — appears in nearly every paper. This is one of the most distinctive personal fingerprints found.
- **One confirmed outlier**: "delve" appears exactly twice in the entire corpus, both confined to a single paper — 16_formal-fault-injection-arxiv.txt ("This paper delves into the integration's potential..."; "it becomes imperative to delve into the realm of..."). This is the one paper in the corpus with a detectable generic-AI-writing tell; treat it as a lapse, not a personal habit, and do not reproduce it.

## 5. Voice and stance toward prior work

Related-work treatment is **neutral-to-generous summary followed by a specific, narrow limitation**, never a dismissive or sweeping critique. Pattern: "[Author] et al. propose/present X [cite]." then one sentence identifying precisely what's missing (scale, availability, a specific assumption). E.g., "AVFI proposes a tool for end-to-end resilience assessment... The paper presents a high-level overview... However, detailed experimental results and discussions on the validity of the experiments are missing to the best of our knowledge." Similarly: "While both these tools are demonstrated to be quite advanced, they are not publicly available." Prior work is credited for what it does before its gap is named — never "X fails to..." as an opening move. Other examples of the summarize-then-pivot move: "Such methods depend on considerable manual (and skilled) work to understand the semantics of the MATLAB commands..." (08) → "Though such research indicates the use of active automata learning for real-life systems, challenges exist to broaden its impact for practical use [29,48,49]." (08).

**Citation placement**: the overwhelming default is bracket-at-end-of-clause, citation as trailing tag, not grammatical subject — "Grammatical inference [9] is associated with various fields...", "The L* algorithm [5] is a prominent algorithm...". Named-author-first ("Jeong et al. (2022) presented...") is used specifically when characterizing one paper's specific contribution in a related-work paragraph. **One confirmed outlier**: 15_uas-maritime-firefighting-att2024 uses full APA author-year style throughout ("Jeong et al. (2022) presented an Unmanned Aerial Vehicle (UAV)... [4]"), almost certainly venue-mandated rather than a personal shift — don't generalize this to other venues.

**Gap-transition template** (the "pivot from related work into your own contribution" sentence): summarize breadth of prior work → note a specific limiting phrase → state what "the presented work" does differently. This template recurs near-verbatim across multiple papers, i.e. it is genuinely self-reused, not just a coincidence: "In contrast, by actively interacting with the actual MATLAB code the work in this article learns a formal model, which allows us to use general purpose formal methods tools to asses properties of the code." (08) / "In contrast, the work presented in this paper removes such dependencies and learns the formal model by actively interacting with the MATLAB code." (Yuvaraj) / "Active automata learning mitigates these restrictions and learns models of black-box systems through interaction." (reused near-verbatim in 13, 08, and Yuvaraj).

**Definitions**: numbered "Definition N (Name):" followed by a tuple, then a bulleted plain-English gloss of each symbol, e.g. `Definition 1 (DFA): A DFA is defined as a 5-tuple ⟨Q, Σ, δ, qi, Qm⟩, where:` followed by "Q is the finite set of states; Σ is the alphabet...". In the **papers**, no Theorem/Lemma/Proof environments were found — formalism stays at the Definition level with informal prose justification. **Correction from the thesis deep-read (see §9)**: the PhD thesis kappa Chapter 6 *does* use Theorem/Lemma/Property/Proof environments (Theorems 2-3, Lemmas 1-4, Properties 1-4, explicit "Proof." blocks). Treat the environments as available in any document — including papers — whenever the content calls for a proved claim; their absence from the corpus papers reflects those papers' content, not a stylistic avoidance.

**Figures/tables**: terse declarative "Figure N verb-s [object]" opening the sentence, active voice, verb is show/present/depict — "Figure 4 shows the model after traversing a fixed number of steps." (13); "Table 2 presents our targets." (19). "As shown in Fig. X" as an opener is essentially absent from the corpus.

**Semicolons in prose** (confirmed, not just in bibliographies or tuple-glosses): used to fuse two tightly related independent clauses — "there exists a string t such that u = st; t is then a suffix of u." / "...to the SUL in this article was used; the only thing that was specifically implemented..." (08).

## 5b. Conclusion structure (confirmed pattern)

Consistent 3-beat pattern: (1) restate what was done/presented — "In conclusion, ...", "This paper presented an approach to...", "This paper presents..."; (2) a limitation or scope caveat, often via "However," folded into the same paragraph rather than a separate "Limitations" heading; (3) a short, terse future-work close, sometimes bulleted. Examples:
- "In conclusion, the main motivation for the work presented in this paper comes from the need to have..." (01)
- "This paper presented an approach to learn a modular discrete-event model of a system using a simulation of it." (06)
- "In this paper, we have described a new, as far as we know, application area of active automata learning..." (Yuvaraj)
- "In summary, this paper introduces the concept of formal fault injection, an approach that synergizes formal methods and fault injection techniques..." (16)

Future work is terse and declarative, sometimes bulleted: "Further research on the procedure to define the PSH is needed." (06); a bulleted list in 07 ("Based on future trends within the industry, this paper presented a need for further study on: • An additional assurance phase after resynchronization. • Use of logged data...").

## 6. Vocabulary fingerprints

Favored/recurring (technical and connective): to this end, hence, thus, however, furthermore, additionally, crucial (32x), robust (17x), seamless(ly) (12x), leverage (used, but sparingly — 8x), state-of-the-art, feasibility, dependable/dependability, discrete-event, virtual commissioning, industrial adoption, active learning, formal methods, correct-by-construction/correct-by-design, plant model, supervisor synthesis, scalability, state-space explosion, "well-known", "to name a few" (used after short example lists).

Rare or absent: "delve" (2 occurrences total, both later/non-technical-adjacent workshop papers — not a core habit despite being an AI-writing red flag word, avoid amplifying it), "arguably" (0), "it is likely that" (0), "in conclusion, this paper has demonstrated..." (never — conclusions restate findings plainly, not this reflexive summary phrase), "landscape"/"realm" (1-2 each, avoid), "underscore"/"pivotal" (rare, avoid), "showcase" (1, avoid — "demonstrate" is the standing verb of choice).

## 6b. Spelling and small-scale conventions (grep-verified)

- **American spelling is the default and dominates**: -ize forms exclusively (78 hits for synthesize/formalize/generalize/realize/utilize vs 0 for -ise), "behavior" 275 vs "behaviour" 52, "modeling" 54 vs "modelling" 14. The thesis (17) is pure American (behavior=72, behaviour=0). The "behaviour" hits cluster in a few earlier/co-authored papers (01, 06, 12, 13, Yuvaraj) — treat American as the current default.
- **"learnt"** (British past participle) is a genuine quirk that persists in the automata-learning papers (42 hits, concentrated in 13, Yuvaraj, MIDES — "the learnt model"), though "learned" is more common overall (124). Don't force either; "learnt model" is authentic in learning-paper contexts.
- **"in order to"** is genuinely used (41 hits) — do not "optimize" it away to bare "to" everywhere; it appears naturally.
- **Acronyms** are introduced with the standard "Full Name (ACRO)" pattern on first use ("Supervisory Control Theory (SCT)", "Plant Structure Hypothesis (PSH)", "The system under learning (SUL)"), then used bare.
- **Titles**: exploratory papers favor "Towards ..." titles ("Towards Automatic Learning...", "Towards data-driven approaches..."); tool papers lead with the tool name ("MIDES: ...", "CarFASE: A Carla-based Tool for...", "WayWiseR: ...").

## 7. Formatting and technical conventions

- Figures/tables introduced with direct, unadorned reference sentences: "Fig. 1 shows...", "Figure 1 shows an overview of...", never "As can be seen in Figure 1..." padding.
- Citations are placed at the end of the clause they support, bracketed, often stacked when summarizing a body of work: "[6], [7]", "[9]–[15]". Named-author citations ("Jeong et al. (2022) presented...") appear specifically when characterizing one paper's specific contribution in related work; bracket-only citations are used for background/survey claims.
- Definitions are numbered and labeled explicitly in formal/theoretical papers: "Definition 1 (Operation):", "Definition 2 (Controllability):" — always followed by an unpacking of each component of a tuple, one clause per element.
- Section transitions/outlines are extremely explicit and formulaic: nearly every intro ends with an "A. Outline" or unlabeled outline paragraph mapping each remaining section: "Section II provides...Section III...Finally, Section V...". This is present in almost all conference papers (absent mainly from the very short workshop/vision papers).
- Notation: formal papers consistently use ⟨tuple⟩ notation with roman/calligraphic symbols (Q, Σ, δ, q0, Qm for automata) and define the alphabet/language machinery (Σ*, prefix-closure, etc.) before using it.

## 8. Evolution 2016 → 2025

- **2016 (ETFA) and 2018 (CASE/INCOM)** papers are the most hedged and most exploratory in framing — "This paper is a preliminary study...", frequent "towards" in titles ("Towards Automatic Learning...", "Towards data-driven approaches..."), explicit "Outline" subsections, heavier use of manual-process description.
- **2020-2022 (IFAC, Electronics, thesis)** period is the most technically dense — long related-work paragraphs, formal definitions, tightly reused sentence templates across sibling papers (the MIDES/CASE and Electronics/MODELS papers share near-identical paragraphs, since Farooqui was reusing and adapting prior intros for closely related follow-up work).
- **2023-2025 (CarFASE, arXiv fault injection, WayWiseR, UAS firefighting)** period shows a shift: shorter, more applied papers (tool/system papers and vision/position papers), contributions given as terse bullet lists rather than folded into prose, related work sections that are more narrowly scoped ("For this paper, we will confine our attention to papers that present tools that..."), and the domain shifted from manufacturing/DES theory to autonomous-driving/UAS safety and fault injection.
- The **thesis (2021)** is the most formal and measured in tone — it is the "cleanest" instance of the neutral-narration voice, with a very plain, declarative abstract structure and is a reasonable default template for formal, longer-form writing (thesis chapters, journal articles). For short conference/workshop papers, the 2023-2025 papers (CarFASE, WayWiseR, arXiv fault injection) are the better template — terser sentences, bulleted contributions, narrower related-work scoping.

**Current default**: for journal/thesis-length academic prose, follow the thesis/Electronics-2022 template (dense, prose contributions, full definitions). For conference/workshop papers, follow the 2023-2025 template (terser, bulleted contributions, narrower scoped related work).

## 9. Thesis kappa deep-read (2021 PhD thesis, "On Supervisor Synthesis via Active Automata Learning" — full pass added 2026-08-14)

The earlier corpus pass sampled the thesis; this section is from a dedicated read of the complete kappa (Part I, ~74 pages: Introduction, Preliminaries, Supervisory Control, Active Automata Learning, Learning Supervisors, Correctness of Modular Learning, Summary of Papers, Concluding Remarks) plus abstract and acknowledgments. Connective counts for the kappa alone: However 29, Hence 31, Thus 24, Furthermore 20, Additionally 11, Therefore 4, Moreover 2, "To this end" 5 — note Hence actually edges out However here; Therefore/Moreover stay marginal.

### Structures unique to the thesis register
- **Chapter bridges**: every chapter opens with an unnumbered recap-and-handoff paragraph naming prior chapters: "Chapter 3 introduced supervisor synthesis... Automata learning, introduced in Chapter 4, provides an approach to... This chapter discusses an approach to integrate the two fields of study. First, some related work is presented."
- **Thesis abstract** is 4 paragraphs, not the papers' 5-beat single paragraph: societal motivation → "This thesis introduces *supervisor learning*, an approach to..." → mechanism ("The idea here is to learn a supervisor for the *system under learning* (SUL) by active interaction and experimentation. To this end, we present two algorithms...") → tool + case studies + "These case studies show the feasibility and applicability of the proposed approach, in addition to helping identify future directions for research." Ends with a Keywords line. Italics used to flag each newly coined term on first appearance.
- **Research questions**: RQ1/RQ2/RQ3 labels, italic question, rationale paragraph under each. The concluding chapter re-states each RQ verbatim and answers it in a labeled block.
- **Contribution bullets are full sentences** cross-referencing papers ("The SupL∗ (see Paper A) algorithm is presented that is an extension of the well-known L∗ algorithm."), unlike the noun-phrase bullets of short papers.
- **Summary-of-papers chapter**: citation block, one neutral summary paragraph, then "My contributions:" in first-person singular — the only place "I" appears in the body: "I am the main responsible for developing, implementing, and proving the algorithm. ... I am the primary author of the paper." ("main responsible" is his authentic phrasing — don't correct it to "mainly responsible".)
- **Research Methods section** narrated plainly in "we": "Before jumping into the implementations, a first step is to engage with the literature..." — hypothesis → proof-of-concept → refine loop, described without methodology jargon.
- **Future Work** opens on an aphorism with a semicolon: "Models are not perfect; they represent only a part of reality." Then one paragraph per thread ("Further research into relaxing these requirements may broaden...", "it would be interesting to study...", "a pain point in this method is the creation of the PSH"). Closing humility line before it: "The overall work in this thesis is but one step towards..."
- **Formal machinery**: Ch. 6 uses Theorem/Lemma/Property/Proof environments with short prose proofs; every formal statement gets a plain-English restatement right after; proof staging is narrated ("Now we are ready for the proof of Theorem 3."). Numbered Example environments (Example 1: the Machine Buffer Machine; coffee-machine model in the intro) interleave with definitions.
- **Pedagogical devices**: "Consider a coffee machine." / "Consider the example..."; "Recall that..." (4x); rhetorical questions in running prose: "The question arises, how does the teacher answer these queries?" / "given a hypothesis, does this hypothesis describe the behavior of the SUL?"; "interested readers are directed to [20]"; an anthropomorphic analogy motivating the approach ("Humans interact with and learn unknown systems by repeatedly forming conjectures... and falsifying these conjectures until they cannot be falsified. Doing the same algorithmically has been studied under...").

### New verified sentence-level quirks (kappa)
- **"Thus/Thereby + gerund"** consequence clause is pervasive: "Thus, ensuring that the controlled system behaves...", "thereby speeding up the learning process", "thereby mitigating the state-space explosion problem". Occasionally stands alone as a (loose) sentence: "Thus, making the task of modeling error-prone and time-consuming."
- **"Else" as sentence opener**: "Else it is not." / "Else, the model checker provides a counterexample..." / "Else, a counterexample needs to be provided..." (3x) — terse, programmer-flavored verdict sentences.
- Idioms: "in the best of scenarios / in the best of cases" (2x), "the other way around" (2x, where others would write "vice versa"), "Doing so is possible using...", "we turn our attention to", "It suffices to here say that..." (inverted word order, authentic), "In brief,", "Here, the problem is two-fold.", "goes a long way in popularizing", "gain a footing in the industry", "has gained a lot of traction", "a.k.a" (3x, no trailing period).
- Evaluative stance words: "Unfortunately," and "This is quite unfortunate, since..."; task-difficulty adjectives "tedious", "arduous", "tricky", "not an easy task", "no trivial task", "a pain point"; efficient exploration is "smart": "explores, in a smart way, only a subset of the state-space", "smartly explore the state-space" (6 smart-hits in the kappa).
- Hedges: "can indeed help" / "would indeed benefit" (indeed as a mild booster, 2x); "could potentially"; "The results are promising as it was possible to..."; "preliminary results show...".
- **Corrections to earlier sections**: (a) "leverage" is NOT zero-frequency — the kappa has ~4 uses, all the narrow technical verb sense: "leverages upon the known foundations", "it is leveraged in Paper A", "can be leveraged to find counterexamples", "leveraging domain-specific knowledge". Keep it rare (≤1/document) but don't treat a single use as an AI tell. (b) "It is worth noting that..." appears exactly once in the whole thesis ("It is worth noting that the supervisor can only restrict the behavior by disabling certain events.") — vanishingly rare, keep the ban in generated text. (c) One true em-dash appears in prose (Ch. 7 "...uncontrollable events—the learning results...") against dozens of spaced en-dash appositives ("the plant – that needs to be controlled – does not meet...", "a supervisor (denoted by S) – a device to restrict the behavior of the plant") — the spaced en-dash rule stands.
- His prose tolerates small grammatical looseness ("This thesis we will use deterministic finite-state automata...", "In Example 1 discusses the behavior...", "the application os supervisor learning" [typo]) — never manufacture errors, but don't over-polish drafts into sterile perfection either; slightly loose participial constructions are part of the texture.

### Acknowledgments (informal register — feeds write-like-me)
Warm, direct second-person address ("You have been an impeccable example of what a supervisor must be"), free exclamation marks ("Thank you!", "we should have them more often!"), parenthetical self-deprecation ("my (often) incoherent ramblings"), colloquial idioms ("Special shout out to Sarmad", "kept my sanity in check", "I can't put my finger on any specific events"), fragment openers ("Feels like it was a couple of months ago that I first stepped into the EDIT building."), extended journey/odyssey metaphor ("My odyssey as a doctoral student draws to a close"), intensifier "way too" ("I have spent way too little time with you; I promise to make up for it!" — semicolon straight into an exclamation), and an unspaced en-dash in the dedication register ("To my wonderful wife Tania–the love of my life").

## 10. Known gaps in this profile

- **Results/case-study/evaluation sections** were not analyzed as a dedicated pass (the analysis covered abstracts, intros, related work, methods, conclusions, and corpus-wide vocabulary). The figure/table-reference conventions in §7 partially cover them. If drafting a results section, re-read one or two real examples first (08_electronics2022 and MIDES_CASE2021 have substantial evaluation sections) rather than extrapolating.
- Word-frequency counts came from simple grep over `pdftotext` output and include some cross-file noise (bibliographies, running headers); treat counts as directional, not exact.
- The corpus includes co-authored papers where other authors may have drafted some sections (e.g. TASE_2021_Hagebring is first-authored by someone else); patterns confirmed across many papers are safe, single-paper patterns less so.
