#' Search Spotify for a artist/song combination.
#'
#' Return essential metadata as a tibble containing
#' artist, track name, album name, track number, release date and spotify track uri.
#'
#' @param artist string
#' @param track string
#'
#' @return tibble(artist, track name, album name, track number, release date and spotify track uri)
#' @export
#' @importFrom spotifyr search_spotify
#' @import dplyr
#' @import stringr
#' @importFrom utils adist
#'
#' @examples
#' quick_search_spotify("Rush","Tom Sawyer")
quick_search_spotify <- function(artist,track){
  search_string = paste0("track:",track," artist:",artist)
  result <- spotifyr::search_spotify(search_string,market="US")
  metadata <- result$tracks$items %>% as_tibble()
  if(nrow(metadata)==0){
    # nothing found
    return(tibble(spot_artist=NA,
                  spot_track=NA,
                  spot_album=NA,
                  track_number= NA,
                  release_date=NA,
                  track_uri=NA)
    )
  } else{
    metadata <- metadata[1,] %>%
      mutate(artist = artists[[1]]$name[1]) %>%
      transmute(spot_artist=artist,
                spot_track=name,
                spot_album=album.name,
                track_number,
                release_date=album.release_date,
                track_uri=uri)


  }
  return(metadata)
}

#' Search Spotify for a artist/song combination.
#'
#' Return essential metadata as a tibble containing
#' artist, track name, album name, track number, release date and spotify track uri.
#' This will try to iteratively loosen the search parameters if no result is returned.
#' First, it will see if the artist can be stripped to a single name.
#' Then it will try using just the longest word in the song title and finally
#' the second longest.  Will return a tibble with all NAs if search fails.
#' False positives with song words in common are rejected if the whole title is
#' substially different.
#' Vectorized along artist and track name.
#' It's not perfect.
#' @param artist string
#' @param track string with song name
#' @param progress logical. If true, show track being searched and whether search is successful
#'
#' @return tibble(artist, track name, album name, track number, release date and spotify track uri)
#' @import dplyr
#' @export
#' @examples
#' fuzzy_search_spotify("Rush","Tom Sawyer")
fuzzy_search_spotify <- function(artist,track,progress=FALSE){
  # spotify won't do fuzzy searches but does partial completions well
  # so we can fake fuzzy
  if(length(artist) != length(track)){
    stop('Artist and track lists not the same length')  #Error condition
  }
  retval_all <- tibble()
  for (n in 1:length(artist)){
    if(progress) cat(artist[n]," ",track[n],"\n")
    # Try 1
    retval <- quick_search_spotify(artist[n],track[n])
    if(is.na(retval[1])){
      # Try 2
      #alt artist search term stripping "and the..." kinds of names
      artist <-str_remove(artist[n],"( and the .+)|(&.+)|(w\\/.+)|(featuring .+)")
      retval <- quick_search_spotify(artist[n],track[n])
      if(is.na(retval[1])){
        # Try 3. Reduce song name to longest word in song name
        # order words in track name by length
        track_words <- tibble(token = unlist(strsplit(track[n],split = " "))) %>%
          mutate(len = str_length(token)) %>%
          arrange(desc(len))
        retval <- quick_search_spotify(artist[n],track_words$token[1])
      }
      if(is.na(retval[1])){
        # Try 4 Reduce song name to second longest word in song name. End there.
        retval <- quick_search_spotify(artist,track_words$token[2])
        if (is.na(retval[1])) break
      }
    }
    # We got a match. Now check for false positives
    front_str <- stringr::str_sub(track[n],end=10)
    back_str <-stringr::str_sub(str_remove_all(retval$spot_track,"[[:punct:]]"),end=10)
    correct = adist(front_str,
                    back_str,
                    ignore.case = TRUE)
    # if title dissimilarity index is too great, reject.
    if(correct > 5) {
      retval <- quick_search_spotify("bogus artist","sfkasfwrq")
    } else{
      if(progress) cat("success!\n")
    }
    retval_all <- bind_rows(retval_all,retval)
  }

  return(retval_all)
}

