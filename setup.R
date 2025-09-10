usethis::create_tidy_package(path = "./",
                             copyright_holder = c("Lucas da Cunha Godoy",
                                                  "Jackson Lautier"))

usethis::use_git()

## To re-render the README run:
devtools::build_readme()

## website?
usethis::use_pkgdown()
usethis::use_pkgdown_github_pages()
