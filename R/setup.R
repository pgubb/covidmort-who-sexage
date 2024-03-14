
library(usethis)

usethis::create_github_token()

gitcreds::gitcreds_set()

usethis::create_from_github(
  "https://github.com/pgubb/covidmort-who-sexage.git",
  destdir = "/Users/paulgubbins/Documents/Github"
)


usethis::create_from_github(
  "https://github.com/pgubb/fsdkenya-effects.git",
  destdir = "/Users/paulgubbins/Documents/Github"
)
