# draft_overview.R
#
# Rewrites two sections of docs/soil_progress_report.qmd:
#   1. "## Lastest description"  — LLM-assisted summary via ellmer
#   2. '## Required elements ("Waiting for")' — deterministic from open PRs
#
# Usage (locally):  Rscript .github/scripts/draft_overview.R
# Usage (CI):       called from weekly_soil_report.yml
#
# Requires: gh, dplyr, purrr, lubridate, glue, ellmer
# Needs:    GITHUB_TOKEN, GOOGLE_API_KEY env vars

library(gh)
library(dplyr)
library(purrr)
library(lubridate)
library(glue)
library(ellmer)

owner <- "ImperialCollegeLondon"
repo <- "ve_data_science"
user <- "hrlai"
qmd <- "docs/soil_progress_report.qmd"

# ── Shared helper ─────────────────────────────────────────────────────────────

splice_section <- function(lines, heading, new_content) {
  # Replaces content between `heading` and the next `## ` heading (exclusive)
  start <- which(lines == heading)
  if (length(start) != 1) {
    stop("Could not find heading: ", heading)
  }
  ends <- which(grepl("^## ", lines))
  end <- min(ends[ends > start]) - 1L
  c(
    lines[seq_len(start - 1)],
    heading,
    "",
    new_content,
    "",
    lines[seq(end + 1, length(lines))]
  )
}

# ── Read the current .qmd ────────────────────────────────────────────────────
lines <- readLines(qmd)

# ══════════════════════════════════════════════════════════════════════════════
# Section B — LLM-assisted: rewrite "## Lastest description"
# ══════════════════════════════════════════════════════════════════════════════

since_7d <- format(Sys.time() - days(7), "%Y-%m-%dT%H:%M:%SZ")

# Fetch recently merged PRs (last 7 days, by hrlai)
raw_merged_prs <- tryCatch(
  gh(
    "/repos/{owner}/{repo}/pulls",
    owner = owner,
    repo = repo,
    state = "closed",
    sort = "updated",
    direction = "desc",
    .limit = 20
  ),
  error = function(e) list()
)

merged_prs <- tibble(
  number = map_int(raw_merged_prs, "number"),
  title = map_chr(raw_merged_prs, "title"),
  html_url = map_chr(raw_merged_prs, "html_url"),
  author = map_chr(raw_merged_prs, c("user", "login")),
  merged_at = map_chr(raw_merged_prs, function(p) {
    p$merged_at %||% NA_character_
  })
) |>
  filter(
    author == user,
    !is.na(merged_at),
    ymd_hms(merged_at) >= ymd_hms(since_7d)
  )

# Fetch issues closed in the last 7 days (by hrlai)
raw_closed_issues <- tryCatch(
  gh(
    "/repos/{owner}/{repo}/issues",
    owner = owner,
    repo = repo,
    state = "closed",
    since = since_7d,
    .limit = 20
  ),
  error = function(e) list()
)

closed_issues <- tibble(
  number = map_int(raw_closed_issues, "number"),
  title = map_chr(raw_closed_issues, "title"),
  html_url = map_chr(raw_closed_issues, "html_url"),
  author = map_chr(raw_closed_issues, c("user", "login")),
  is_pr = map_lgl(raw_closed_issues, function(i) !is.null(i$pull_request))
) |>
  filter(author == user, !is_pr)

# Fetch issues opened in the last 7 days (by hrlai)
raw_new_issues <- tryCatch(
  gh(
    "/repos/{owner}/{repo}/issues",
    owner = owner,
    repo = repo,
    state = "open",
    since = since_7d,
    .limit = 20
  ),
  error = function(e) list()
)

new_issues <- tibble(
  number = map_int(raw_new_issues, "number"),
  title = map_chr(raw_new_issues, "title"),
  html_url = map_chr(raw_new_issues, "html_url"),
  author = map_chr(raw_new_issues, c("user", "login")),
  created_at = map_chr(raw_new_issues, "created_at"),
  is_pr = map_lgl(raw_new_issues, function(i) !is.null(i$pull_request))
) |>
  filter(
    author == user,
    !is_pr,
    ymd_hms(created_at) >= ymd_hms(since_7d)
  )

# Build context summary for the LLM
fmt_items <- function(
  df,
  number_col = "number",
  title_col = "title",
  url_col = "html_url"
) {
  if (nrow(df) == 0) {
    return("(none)")
  }
  map_chr(seq_len(nrow(df)), function(i) {
    glue(
      "  - #{df[[number_col]][i]}: {df[[title_col]][i]} ({df[[url_col]][i]})"
    )
  }) |>
    paste(collapse = "\n")
}

context_block <- glue(
  "Merged PRs (last 7 days):\n{fmt_items(merged_prs)}\n\n",
  "Closed issues (last 7 days):\n{fmt_items(closed_issues)}\n\n",
  "Newly opened issues (last 7 days):\n{fmt_items(new_issues)}"
)

prompt <- glue(
  "
  You are helping an ecologist draft the opening paragraph of a weekly progress report.

  Write a single paragraph of 3 to 5 sentences in the first person. Keep the tone terse, professional, and factual. Do not use self-congratulation, filler, bullet points, headings, or markdown.

  Base the paragraph only on the source material provided below. Summarize both recent achievements and near-term planned focus. Organize the content implicitly by grouping related work where appropriate across these categories: input data, model parameterisation, calibration, validation, and general tools. Do this naturally in prose rather than by naming the categories mechanically if that would make the paragraph awkward.

  When an issue, pull request, or issue comment is relevant, reference the actual number exactly as given, for example #123. If the output format supports links, render each issue or PR number as a hyperlink to its URL and use the issue or PR title as hover text; if the output format does not support that, keep the plain reference number and do not invent formatting. If a recent issue references older issues or PRs that materially matter for context, mention those older references briefly and only if they help explain the current work.

  Treat issue comments as evidence of recent discussion, clarification, or coordination. Do not present commenting activity by itself as a completed outcome unless the surrounding source material supports that interpretation.

  If the source material shows that a task involved learning a new method, tool, or workflow, mention that briefly, especially when it appears relevant for shared team learning. Do not infer progress, intent, or significance beyond what the source material supports.

  Prioritize specificity, compression, and accurate grouping over completeness. If the source material is thin or uneven, write a restrained paragraph that reflects that rather than overfilling gaps.
  \n\n
  ",
  "{context_block}"
)

description_content <- tryCatch(
  {
    chat <- ellmer::chat_google_gemini()
    response <- chat$chat(prompt)
    trimws(response)
  },
  error = function(e) {
    message("LLM call failed: ", conditionMessage(e))
    "*[Auto-draft failed — please update manually.]*"
  }
)

lines <- splice_section(lines, "## Lastest description", description_content)

# ══════════════════════════════════════════════════════════════════════════════
# Section A — Deterministic: rewrite '## Required elements ("Waiting for")'
# ══════════════════════════════════════════════════════════════════════════════

raw_prs <- gh(
  "/repos/{owner}/{repo}/pulls",
  owner = owner,
  repo = repo,
  state = "open",
  .limit = Inf
)

prs <-
  tibble(
    number = map_int(raw_prs, "number"),
    title = map_chr(raw_prs, "title"),
    html_url = map_chr(raw_prs, "html_url"),
    author = map_chr(raw_prs, c("user", "login")),
    created_at = map_chr(raw_prs, "created_at") |> ymd_hms(),
    reviewers = map(raw_prs, \(p) map_chr(p$requested_reviewers, "login"))
  ) |>
  filter(
    author == user,
    map_lgl(reviewers, \(r) length(r) > 0)
  ) |>
  mutate(
    age_days = as.numeric(difftime(Sys.time(), created_at, units = "days")),
    urgent = age_days > 7,
    reviewer_list = map_chr(reviewers, paste, collapse = ", ")
  ) |>
  arrange(desc(urgent), desc(age_days))

build_bullets <- function(df, label) {
  if (nrow(df) == 0) {
    return(character(0))
  }
  items <- map_chr(seq_len(nrow(df)), \(i) {
    r <- df[i, ]
    glue(
      "  - {r$reviewer_list} to review PR [#{r$number}]({r$html_url}) — *{r$title}*"
    )
  })
  c(label, items)
}

waiting_for_content <- c(
  build_bullets(filter(prs, urgent), "- Urgent"),
  build_bullets(filter(prs, !urgent), "- Not urgent")
)

lines <- splice_section(
  lines,
  '## Required elements ("Waiting for")',
  waiting_for_content
)

# ── Write back ────────────────────────────────────────────────────────────────
writeLines(lines, qmd)
message(
  "Updated '",
  qmd,
  "' — Lastest description and Waiting for sections refreshed."
)
