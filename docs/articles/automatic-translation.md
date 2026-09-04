# Automatic Model Translation

*Last updated: 2026-09-04*

## Motivation

**Imagine how convenient it would be for any model described in the
literature to be translated and simulated in real-time with a click of a
button!**

With the recent advancements on generative AI, it is now possible (and
*feasible*) to upload a file to a large language model (LLM), extract
its relevant contents, and convert it into model code in \<1 minute.

This feature in MVP is potentially a huge time-saver, especially for
more complicated models such as complex PopPK/PD, PBPK, or QSP models.

## Disclaimer

By using this functionality, the user should be aware that data will be
sent externally, and may be used for training purposes (depending on the
provider). Please check with your organization’s policies regarding data
privacy and data protection.

## Pre-requisites

To use this feature, you **must have access to API keys** for calling
LLM service providers. Please contact your system administrator if you
have questions about API usage and costs, which is outside the scope of
this post.

If you don’t, luckily there is a **free option** available via Gemini,
which you can do after signing up to [Google AI
Studio](https://aistudio.google.com/) and clicking the “Create API Key”
button on the lower left (info current as of 2026-02). Please be aware
that you may be rate limited with free options.

Alternatively, [OpenRouter](https://openrouter.ai/models) also has a
free tier for API keys, although in general the free models do not
perform as well as the frontier models, and in addition you would have
to specify your exact model of choice to be more optimal (more on that
later), so we recommend spending a few dollars to have a vastly superior
user-experience and reserve the free option for testing purposes.

## Setup

### Required Packages

Install the `ellmer` and the `pdftools` R packages if you don’t
currently have it (`install.packages(c("ellmer", "pdftools")`). If you
are not sure, you can check by
`print(requireNamespace(c("ellmer", "pdftools")))`.

### API Keys

As of the writing of this post (2026-Feb), most of the major providers
are supported in MVP, and any other providers who share the same API
protocol as OpenAI should also work (by selecting the
“OpenAI-Compatible” option). Please see below for a brief instruction of
configuring API keys after package installation:

1.  Open up the `.Renviron` file (if you have the `usethis` package, run
    [`usethis::edit_r_environ()`](https://usethis.r-lib.org/reference/edit.html)on
    the console)  
2.  Create a new entry for each provider that you would like to use, as
    follows:  

- Claude: `ANTHROPIC_API_KEY="your-key-here"`  
- Gemini: `GEMINI_API_KEY="your-key-here"`  
- OpenAI: `OPENAI_API_KEY="your-key-here"`  
- OpenRouter: `OPENROUTER_API_KEY="your-key-here"`
- OpenAI-Compatible: `OPENAI_COMPATIBLE_API_KEY="your-key-here"`
- DeepSeek: `DEEPSEEK_API_KEY="your-key-here"`  
- Azure OpenAI: `AZURE_OPENAI_API_KEY="your-key-here"`,
  `AZURE_TENANT_ID`, `AZURE_CLIENT_ID`, `AZURE_CLIENT_SECRET`,
  `AZURE_OPENAI_ENDPOINT` [refer to ellmer
  documentation](https://ellmer.tidyverse.org/reference/chat_azure_openai.html)  
- AWS Bedrock: [refer to ellmer
  documentation](https://ellmer.tidyverse.org/reference/chat_aws_bedrock.html)  

3.  Save and close the `.Renviron` file, and restart your R session (you
    have to do this once for each new key).

### Configuring Default Options

When you launch MVP
([`run_mvp()`](https://stevechoy.github.io/MVPapp/reference/run_mvp.md)),
you may specify defaults associated with your provider of choice using
the LLM-specific arguments:

- `llm_choices`: By default, `Claude`, `OpenAI`, `Gemini`, `OpenRouter`,
  `OpenAI-Compatible`, `Azure OpenAI`, `AWS Bedrock` are included. The
  first name is used as default.
- `reuse_context`: keep the same conversation during retries to improve
  subsequent results, defaults to `FALSE` (see explanation below)  
- `model_*`: Default model names to use for your preferred provider,
  e.g. `model_openai` for ChatGPT, or `model_anthropic` for Claude, etc.
  Please check each provider for the latest list of accepted model
  names.  
- `api_chat`: URL path if you are using an OpenAI-Compatible provider,
  which feeds into the `base_url` argument during
  [`ellmer::chat_openai_compatible()`](https://ellmer.tidyverse.org/reference/chat_openai_compatible.html)
  call  
- `api_upload`: URL path if you are using a Dify-style provider for the
  upload location of files  
- `temperature`: Temperature setting, ranging from 0 (more
  deterministic) to 1 (more creativity), defaults to `0`. (Note: From
  mid-2026, most “thinking” models have stopped supporting the use of
  temperature. A warning pop-up box will notify users whether
  temperature is used during translation.)
- `llm_seed`: Seed number if supported by the LLM (however it still does
  not guarantee reproducibility), defaults to `42`
- `model_lang`: Default output to `mrgsolve`, `nonmem`, or `rxode2` code
  in the response (see below).
- `prompts_path`: Path of the [prompts
  file](https://github.com/stevechoy/MVPapp/blob/master/inst/shiny/prompts.R),
  by default it is in the installed package directory of `MVPapp` (see
  below).

For example, if you want to use Moonshot AI’s Kimi K2.5 model as default
(i.e. a OpenAI-Compatible provider), one would launch MVP as follows
(after configuring the corresponding `OPENAI_COMPATIBLE_API_KEY` field
in .Renviron):

`run_mvp(llm_choices = "OpenAI-Compatible", model_openai_compatible = "kimi-k2.5", api_chat = "https://api.moonshot.ai/v1/chat/completions")`

In a second example, if you already have Claude API access, and just
want to change the default model, you could try something like:

`run_mvp(llm_choices = "Claude", model_anthropic = "claude-haiku-4-5-20251001")`

Note that all of the arguments only set up the default option for
convenience - all of these options can be changed dynamically within
MVP.

In addition, as the advancement of models happens frequently (on a time
scale of months), it is recommended that the user pays particular
attention and **specify a preferred model name** as appropriate, as the
default options may become outdated and deprecated.

## Usage

From the Model Selection drop-down list, choose the option called
“Upload File (AI Translation)”. Next, all you need to do is to upload a
file (or multiple files, we currently support PDF or text files
`.txt/.mod/.ctl`), click the “Send” button, and then wait for the
results to be generated, which takes about a minute.

When the results become available, the code editor will be updated and
then you can proceed as normal. If the default settings are not to your
preference, you can click on the “Gear” button to further customize
settings.

⚠️ Multiple files are supported, as long as they have the same extension
(e.g. all PDF files). For example, if the model descriptions are in the
main body of the article, while the model equations and/or parameters
are provided in the supplementary files, simply upload them all at the
same time. Note: the same extension rule does not apply when using
deterministic translation from NONMEM files (see below).

💡 For better results, try **limiting the file contents** to just the
relevant portions. If it is a PDF file, you may use
[pdfcombiner](https://github.com/stevechoy/pdfcombiner) to help trim
some pages off.

### Locally Parsing Files

The user has the option to bypass uploading of files, and instead parse
the text locally and include that as part of the prompt to be sent to
the LLM (currently supports PDF and text files). The advantage is that
this is usually quicker, and with a reduced token usage (i.e. cheaper).
The downside is that if the file contains lots of images such as model
schematics, the contents will not be extracted and thus the response may
become less accurate, compared to uploading the file. In addition, not
all providers allow separate uploading of files, so locally parsing
files is the workaround solution.

💡 Whether it makes sense to parse locally is **highly dependent** on
the file itself, e.g. in MVP, NONMEM control streams or CPP files
(`.mod/.ctl/.cpp`) are considered equivalent to text files (it is
converted internally to `.txt`). Therefore, it will be automatically
parsed locally since any actual upload accepts only PDF files to ensure
maximum compatibility for most providers. If there are context window
limits which prevents lots of text to fit within the prompt, the user
should then save the file(s) as a PDF so it can be properly uploaded.

### Automatic Retries

MVP has built-in automatic retries (up to 3 times, default: 2) if the
compilation fails. As mrgsolve syntax can be quite strict, the initial
response may fail to compile on simple syntax errors which is often an
easy fix.

How this works behind the scenes is that an internal compilation check
is performed when the initial response is received. If unsuccessful, the
error message from the compilation will be attached together with the
original model code and sent to the provider, asking it to correct the
mistakes. By default, this initiates a new conversation (i.e. no memory)
so the LLM is not aware of the original conversation with the file
contents.

💡 For better results, the user can set argument `reuse_context = TRUE`
(or check the “Memory” option in Settings) to keep the same conversation
in order to provide better context to the LLM to improve the response,
although this would incur additional token usage.

### Deterministic Translation of NONMEM files

MVP (\>= v0.4.1) supports deterministic translation (i.e. not using
LLMs) of NONMEM files, courtesy of the
[nonmem2mrgsolve](https://github.com/Andy00000000000/nonmem2mrgsolve)
and [nonmem2rx](https://github.com/nlmixr2/nonmem2rx) R packages.

When either packages are installed, new checkbox options would be
available during customization (default not used). When checked,
deterministic translation will be triggered when:

- nonmem2mrgsolve - If the user uploads **EXACTLY** 2 files: the NONMEM
  (`.ctl/.mod`) file **AND** the corresponding `.ext` file.  
- nonmem2rx - If the user uploads **AT LEAST** the NONMEM
  (`.ctl/.mod/.lst/.xml`) file, and optionally **ANY** number of
  supporting files (`.phi/.ext/.cov/.xml/.grd`).

💡 Automatic retries can still happen when using nonmem2mrgsolve, as
internal compilation checks can still be performed. However, no
self-validation will be performed when using nonmem2rx.

### Cost

Typical usage assuming a 10-page PDF costs anywhere between 20k-70k
tokens (including 2 retries), which translates to about 10-20 cents for
a frontier model such as Claude Sonnet. If you are price conscious, you
could consider cheaper models and/or providers. However please be aware
that performance may be inferior for non-frontier models. In general,
**you get what you pay for**.

After each LLM response has been received, a notification will be
displayed on the bottom right hand corner, which includes token usage
for the user’s information.

### Notes

- The `prompts_path` argument when launching
  [`run_mvp()`](https://stevechoy.github.io/MVPapp/reference/run_mvp.md)
  allows the user to provide their own prompts. By default, the included
  `prompts.R` file is used (located in the `/shiny` subfolder of
  `MVPapp`, whereby the path can be located by
  `find.package("MVPapp")`). The prompts are fine-tuned based on
  trial-and-error, and may change in newer versions to improve first-try
  success rates based on user feedback. It is recommended that users
  make a copy of the file before making adjustments.  
- A seed of `42` and a temperature setting of `0` is used by default.
  Please note that the use of a seed or setting a temperature to 0 may
  not necessarily guarantee reproducibility (depending on the provider).
  It is also important to note that from mid-2026, most “thinking”
  models have stopped supporting the use of temperature. A warning
  pop-up box will notify users whether temperature is used during
  translation.
- While the feature is best supported by translating and compiling into
  mrgsolve, in principle it can be quite easily adapted to any other
  language. The `model_lang` argument controls the output language,
  which is set to `mrgsolve` by default. For testing purposes, `nonmem`
  and `rxode2` is also available (MVP app does not support executing
  NONMEM or RxODE2, so retries will be automatically set to 0).  
- If the launch argument `show_debugging_msg = TRUE`, the responses will
  be saved as the R objects `llm_result` (for initial translation) and
  `llm_refine` (for retries) in the global environment for review.

## Performance Evaluation

To assess the translation accuracy of AI using MVP, a curated list of 20
published models from the literature representing a range of model
complexity was used as the reference. Translation performance was
evaluated according to the following hierarchical criterion of
(generally) increasing difficulty:

- **Level 1:** Successful compilation  
- **Level 2:** Successful translation of fixed effects
- **Level 3:** Successful translation of random effects (including
  residuals)
- **Level 4:** Successful translation of model relationships (i.e. ODEs,
  covariate relationships)

The test was conducted using `claude-sonnet-5`, with a temperature of
`1` (i.e. adaptive thinking), with up to 2 retries per journal article.
If supplementary materials contains relevant information such as
parameter tables or model code, those would also be included as part of
the input files. Standardized prompts as provided in the
[prompts.R](https://github.com/stevechoy/MVPapp/blob/master/inst/shiny/prompts.R)
file is used.

The spreadsheet containing results per journal article, and
miscellaneous notes is available
[here](https://github.com/stevechoy/MVPapp/blob/master/llm-eval/LLM_Results.xlsx),
and the translated code are available
[here](https://github.com/stevechoy/MVPapp/blob/master/llm-eval/List_Of_Papers_Results_Only)
(after being re-formatted with mrgsolve’s `mwrite_cpp` function).

In summary, successful compilation and correct translation of fixed
effects occurred 85-90% of the time, while random effects were
successful around half the time. Most failures on translating random
effects were related to the LLMs failing to distinguish between
variance, standard deviations, or %CV to be used as the correct value.
Model relationships generally can be accurately translated (~80%),
provided that the original text contains key information such as
equations and ODEs. Overall, success rates tend to be inversely
proportional to how complex the model structure is (e.g. QSP models
tended to struggle more).

Based on this assessment, **translating from the literature is generally
reliable** (at least for the structural model), assuming a parameter
table is present and model relationships are well explained in the text.
When there happen to be errors, they are usually quite obvious and easy
to correct from the user’s perspective. Given that this is as worst as
it’s going to get (as of Sept 2026), future LLMs are expected to score
even better than the current state-of-the-art models, which becomes
quickly outdated every few months.

This work is presented in ACoP 2026, with the poster available here
(link TBD).

## Summary

The goal of this feature is to further enable internal team sharing and
discussion of models from the literature, with a side benefit of
translating existing NONMEM models so users don’t have to start from
scratch as they prepare for simulations.

Please note that successful compilation does not guarantee code
correctness, therefore it is on the user to ensure that any generated
model code is accurate.
