library(tidyverse)
library(openalexR)
library(rcrossref)
library(htmltools)
library(glue)

id = "0000-0001-8956-4053"

fetch_pubs = function(id, missing_dois, pubs_ignore) {

  # pull pubs with openalexR using orcid
  missing = oa_fetch(entity = "works", doi = missing_dois, verbose = TRUE)

  works_all = oa_fetch(
    entity = "works",
    author.orcid = c(id),
    verbose = TRUE) %>%
    bind_rows(missing) %>%
    mutate(display_name = str_to_sentence(display_name),
           display_name = gsub("\\.", "", display_name),
           doi = gsub("https://doi.org/", "", doi))

  duplicates = works_all %>%
    group_by(display_name) %>%
    summarize(n = n()) %>%
    filter(n > 1)

  duplicate_keep = works_all %>%
    filter(display_name %in% duplicates$display_name) %>%
    arrange(display_name, desc(publication_date)) %>%
    group_by(display_name) %>%
    fill(so, .direction = "updown") %>%
    mutate(keep = ifelse((row_number() == 1 & is.na(so)) | (!is.na(so) & !is.na(so_id)), 1, 0)) %>%
    select(keep, everything()) %>%
    filter(keep == 1) %>%
    select(-keep)

  works_subset = works_all %>%
    filter(!(is.na(doi) & !grepl("W3100995196", id)) & !grepl(pubs_ignore, id)) %>%
    filter(!display_name %in% duplicates$display_name) %>%
    bind_rows(duplicate_keep) %>%
    select(id, type, title, author, ab, publication_date, so, doi, url, pdf_url, oa_url, publication_year, counts_by_year) %>%
    mutate(type = ifelse(type == "article" & grepl("osf.io", doi), "preprint", type),
           doi = ifelse(is.na(doi), url, doi)) %>%
    mutate(citation = ifelse(!type =="dissertation", cr_cn(dois = doi, format = "text", style = "apa"), "Cosme, D. (2020). Behavioral and Neural Effects of Self-determined Choice on Goal Pursuit."))

  return(works_subset)

}

# functions adapted from https://github.com/jhelvy/jhelvy_quarto/blob/main/_common.R

make_altmetric <- function(pub) {
  altmetric <- ""
  if (!is.na(pub$doi)) {
    altmetric <- glue::glue('<div data-badge-type="donut" data-doi="{pub$doi}" data-hide-no-mentions="true" class="altmetric-embed"></div>')
  }
  return(altmetric)
}

make_dimension <- function(pub) {
  dimension <- ""
  if (!is.na(pub$doi)) {
    style_tag <- glue("<style>
      #badge {{
        display: inline-block;
      }}
    </style>")
    dimension <- glue::glue('{style_tag}<span id="badge" span class="__dimensions_badge_embed__" data-doi="{pub$doi}" data-hide-zero-citations="false" data-legend="hover-right" data-style="large_rectangle"></span>')
  }
  return(dimension)
}

icon_link <- function(
    icon = NULL,
    text = NULL,
    url = NULL,
    class = "icon-link",
    target = "_blank"
) {
  if (!is.null(icon)) {
    text <- make_icon_text(icon, text)
  }
  return(htmltools::a(
    href = url, text, class = class, target = target, rel = "noopener"
  ))
}

make_icon_text <- function(icon, text) {
  return(HTML(paste0(make_icon(icon), " ", text)))
}

make_icon <- function(icon) {
  return(tag("i", list(class = icon)))
}

make_icons <- function(pub) {
  html <- c()
  if (!is.na(pub$doi)) {
    dimension = make_dimension(pub)
    html <- c(html, dimension)
  }
  if (!is.na(pub$url)) {
    html <- c(html, as.character(icon_link(
      icon = "fas fa-external-link-alt",
      text = "View",
      url  = pub$url
    )))
  }
  if (!is.na(pub$pdf_url)) {
    html <- c(html, as.character(icon_link(
      icon = "fa fa-file-pdf",
      text = "PDF",
      url  = pub$pdf_url
    )))
  }
  return(paste(html, collapse = " "))
}

markdown_to_html <- function(text) {
  if (is.null(text)) { return(text) }

  # Replace the author names with underlined last names
  text <- gsub(
    pattern = "\\\\\\*([^,]+), ([^,]+)",
    replacement = "<u>\\\\*\\1</u>, \\2",
    text
  )
  text <- gsub(
    pattern = "\\\\\\*\\\\\\*([^,]+), ([^,]+)",
    replacement = "<u>\\\\*\\\\*\\1</u>, \\2",
    text
  )

  # Render the text as HTML
  return(HTML(markdown::renderMarkdown(text = text)))
}

make_pub <- function(pub, index = NULL) {
  header <- FALSE
  altmetric <- make_altmetric(pub)
  if (is.null(index)) {
    cite <- pub$citation
    icons <- make_icons(pub)
  } else {
    cite <- glue::glue('{index}) {pub$citation}')
    icons <- glue::glue('<ul style="list-style: none;"><li>{make_icons(pub)}</li></ul>')
    if (index == 1) { header <- TRUE }
  }
  # return(markdown_to_html(cite))
  return(htmltools::HTML(glue::glue(
    '<div class="grid">
    <div class="g-col-11"> {markdown_to_html(cite)} </div>
    <div class="g-col-1"> {altmetric} </div>
    </div>
    <div class="g-col-1"> {icons}</div>'
  )))
}

make_pub_list <- function(pubs, type = "") {
  if (type == "preprint") {
    x <- pubs %>%
      filter(type == "preprint") %>%
      arrange(desc(publication_date))

  } else {
    x <- pubs %>%
      filter(!type == "preprint") %>%
      arrange(desc(publication_date))
  }

  pub_list <- list()
  for (i in 1:nrow(x)) {
    pub_list[[i]] <- make_pub(x[i,], index = i)
  }
  return(htmltools::HTML(paste(unlist(pub_list), collapse = "")))
}
