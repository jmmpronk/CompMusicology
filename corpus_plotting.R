library(tidyverse)
library(spotifyr)
library(dplyr)
library(ggthemes)
library(pals)

the_beatles <- get_playlist_audio_features("", "37i9dQZF1DZ06evO2iBPiw")

led_zeppelin <- get_playlist_audio_features("", "37i9dQZF1DZ06evO1NyWWI")

queen <- get_playlist_audio_features("", "37i9dQZF1DZ06evO0ENBD2")

pink_floyd <- get_playlist_audio_features("", "37i9dQZF1DZ06evO07zaak")

the_rolling_stones <- get_playlist_audio_features("", "37i9dQZF1DZ06evO19s0CZ")

eagles <- get_playlist_audio_features("", "37i9dQZF1DZ06evO0jXJRu")

aerosmith <- get_playlist_audio_features("", "37i9dQZF1DZ06evO4x3X2w")

bon_jovi <- get_playlist_audio_features("", "37i9dQZF1DZ06evO3154GY")

the_doors <- get_playlist_audio_features("", "37i9dQZF1DZ06evO19UBIk")

lynyrd_skynyrd <- get_playlist_audio_features("", "37i9dQZF1DZ06evO2ObHwc")

uk <-
  bind_rows(
    the_beatles |> mutate(band = "The Beatles"),
    led_zeppelin |> mutate(band = "Led Zeppelin"),
    queen |> mutate(band = "Queen"),
    pink_floyd |> mutate(band = "Pink Floyd"),
    the_rolling_stones |> mutate(band = "The Rolling Stones")
  )

us <-
  bind_rows(
    eagles |> mutate(band = "Eagles"),
    aerosmith |> mutate(band = "Aerosmith"),
    bon_jovi |> mutate(band = "Bon Jovi"),
    the_doors |> mutate(band = "The Doors"),
    lynyrd_skynyrd |> mutate(band = "Lynyrd Skynyrd")
  )

british <- get_playlist_audio_features("", "4N1tKwmDmPXqCfmBj49Tr9")

american <- get_playlist_audio_features("", "4EEyUrDeY2tLDd3CfGZskk")

corpus <-
  bind_rows(
    uk |> mutate(country = "UK"),
    us |> mutate(country = "US")
  )

corpus_per_band <- corpus %>%
  group_by(band) %>%
  summarize(meanEnergy = mean(energy), meanValence = mean(valence))

count_group <- data.frame(user=factor(rep(1:50, 2)), 
                          count=sample(100, 100, replace=T), 
                          group=factor(rep(LETTERS[1:20], 5)))

library(RColorBrewer)
cols <- colorRampPalette(brewer.pal(9, "Set1"))
ngroups <- length(unique(count_group$group))


corpus |>
  ggplot(aes(x = acousticness, fill = band)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1) +
  facet_wrap(~country) + scale_fill_manual(values = cols(ngroups))

corpus |> ggplot(aes(x = valence, y = energy, fill = country, color = country)) + geom_point(alpha = 0.6, shape = 21, stroke = 1) + theme_tufte() + xlim(0, 1) + ylim(0, 1) + geom_smooth()

corpus_per_band |> ggplot(aes(x = meanValence, y = meanEnergy, color=band, fill = band)) + geom_point(shape = 23, size = 4, alpha=0.5) + theme_tufte() + xlim(0, 1) + ylim(0, 1)

selection <- aerosmith %>%
  filter(valence < 0.1, energy > 0.9)

reqd <- as.vector(c("track.id"))

Result <- selection[,reqd]
Result
