# tidy_pod

I love podcasts and data analysis, so I decided to use David Smith's podsearch website and the tools in the `tidytext` package to analyze some of my favorite podcasts.

## Getting the data

The `scrape.R` script takes the raw HTML from the podsearch site and converts it into a tidy dataframe. I scraped every available episode (as of 2017-06-06) of the following podcasts: 

- Accidental Tech Podcast (222 episodes - 1 still processing)
- Cortex (51 episodes)
- Hypercritical (72 episodes - 28 still processing)
- The Talk Show (190 episodes)

