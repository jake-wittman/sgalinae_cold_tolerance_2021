# Use to sort the .bib file once all entires are present
# The readbib function may not play well with characters with accents (like bürkner)
# May need to sort those manually

library(RefManageR)
BibOptions(sorting = "nyt")
bib_file <- ReadBib(here::here("manuscript/references.bib"))


# Don't run this till you know which references were ignored!!!
sorted_bib <- sort(bib_file, sorting = BibOptions()$sorting)
WriteBib(sorted_bib, file = "manuscript/references.bib")
