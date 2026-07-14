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
# Needs:    GITHUB_TOKEN, ANTHROPIC_API_KEY env vars

library(gh)
library(dplyr)
library(purrr)
library(lubridate)
library(glue)
library(ellmer)

owner <- "ImperialCollegeLondon"
repo  <- "ve_data_science"
user  <- "hrlai"
qmd   <- "docs/soil_progress_report.qmd"

# ── Shared helper ─────────────────────────────────────────────────────────────

splice_section <- function(lines, heading, new_content) {
  # Replaces content between `heading` and the next `## ` heading (exclusive)
  start <- which(lines == heading)
  if (length(start) != 1) stop("Could not find heading: ", heading)
  ends <- which(grepl("^## ", lines))
  end <- min(ends[ends > start]) - 1L
  c(lines[seq_len(start - 1)], heading, "", new_content, "", lines[seq(end + 1, length(lines))])
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
    repo  = repo,
    state = "closed",
    sort  = "updated",
    direction = "desc",
    .limit = 20
  ),
  error = function(e) list()
)

merged_prs <- tibble(
  number    = map_int(raw_merged_prs, "number"),
  title     = map_chr(raw_merged_prs, "title"),
  html_url  = map_chr(raw_merged_prs, "html_url"),
  author    = map_chr(raw_merged_prs, c("user", "login")),
  merged_at = map_chr(raw_merged_prs, function(p) p$merged_at %||% NA_character_)
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
    owner  = owner,
    repo   = repo,
    state  = "closed",
    since  = since_7d,
    .limit = 20
  ),
  error = function(e) list()
)

closed_issues <- tibble(
  number   = map_int(raw_closed_issues, "number"),
  title    = map_chr(raw_closed_issues, "title"),
  html_url = map_chr(raw_closed_issues, "html_url"),
  author   = map_chr(raw_closed_issues, c("user", "login")),
  is_pr    = map_lgl(raw_closed_issues, function(i) !is.null(i$pull_request))
) |>
  filter(author == user, !is_pr)

# Fetch issues opened in the last 7 days (by hrlai)
raw_new_issues <- tryCatch(
  gh(
    "/repos/{owner}/{repo}/issues",
    owner  = owner,
    repo   = repo,
    state  = "open",
    since  = since_7d,
    .limit = 20
  ),
  error = function(e) list()
)

new_issues <- tibble(
  number     = map_int(raw_new_issues, "number"),
  title      = map_chr(raw_new_issues, "title"),
  html_url   = map_chr(raw_new_issues, "html_url"),
  author     = map_chr(raw_new_issues, c("user", "login")),
  created_at = map_chr(raw_new_issues, "created_at"),
  is_pr      = map_lgl(raw_new_issues, function(i) !is.null(i$pull_request))
) |>
  filter(
    author == user,
    !is_pr,
    ymd_hms(created_at) >= ymd_hms(since_7d)
  )

# Build context summary for the LLM
fmt_items <- function(df, number_col = "number", title_col = "title", url_col = "html_url") {
  if (nrow(df) == 0) return("(none)")
  map_chr(seq_len(nrow(df)), function(i) {
    glue("  - #{df[[number_col]][i]}: {df[[title_col]][i]} ({df[[url_col]][i]})")
  }) |> paste(collapse = "\n")
}

context_block <- glue(
  "Merged PRs (last 7 days):\n{fmt_items(merged_prs)}\n\n",
  "Closed issues (last 7 days):\n{fmt_items(closed_issues)}\n\n",
  "Newly opened issues (last 7 days):\n{fmt_items(new_issues)}"
)

prompt <- glue(
  "You are helping a researcher write the opening paragraph of their weekly progress report. ",
  "Write 2–4 sentences in the first person (e.g. 'I merged ...', 'I plan to ...'). ",
  "Reference actual issue and PR numbers where relevant (e.g. '#123'). ",
  "Describe recent achievements and planned near-term focus based only on the data below. ",
  "Do not add bullet points, headings, or any markdown formatting — plain prose only.\n\n",
  "{context_block}"
)

description_content <- tryCatch({
  chat <- ellmer::chat_anthropic(model = "claude-opus-4-5")
  response <- chat$chat(prompt)
  trimws(response)
}, error = function(e) {
  message("LLM call failed: ", conditionMessage(e))
  "*[Auto-draft failed — please update manually.]*"
})

lines <- splice_section(lines, "## Lastest description", description_content)

# ══════════════════════════════════════════════════════════════════════════════
# Section A — Deterministic: rewrite '## Required elements ("Waiting for")'
# ══════════════════════════════════════════════════════════════════════════════

raw_prs <- gh(
  "/repos/{owner}/{repo}/pulls",
  owner = owner,
  repo  = repo,
  state = "open",
  .limit = Inf
)

prs <-
  tibble(
    number        = map_int(raw_prs, "number"),
    title         = map_chr(raw_prs, "title"),
    html_url      = map_chr(raw_prs, "html_url"),
    author        = map_chr(raw_prs, c("user", "login")),
    created_at    = map_chr(raw_prs, "created_at") |> ymd_hms(),
    reviewers     = map(raw_prs, \(p) map_chr(p$requested_reviewers, "login"))
  ) |>
  filter(
    author == user,
    map_lgl(reviewers, \(r) length(r) > 0)
  ) |>
  mutate(
    age_days      = as.numeric(difftime(Sys.time(), created_at, units = "days")),
    urgent        = age_days > 7,
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
message("Updated '", qmd, "' — Lastest description and Waiting for sections refreshed.")
