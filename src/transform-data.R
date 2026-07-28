library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(geosphere)
library(worrms)

# LOAD DATA
# obtain decimal degrees of coordinate from Pangaea event list
pangaea_ps152_event <- read_tsv("https://www.pangaea.de/expeditions/events/PS152?format=textfile") %>% clean_names()
# data provided by chiara
cp_data <- here("data", "01_raw", "FINAL_wobec-sample-logger_PS152_4_FISH_20-07-26.xlsx")
cp_event <- read_excel(cp_data, sheet = "event")
cp_occ <- read_excel(cp_data, sheet = "material")


# EVENT
event <- cp_event %>%
  left_join(pangaea_ps152_event, by = c("stationID" = "event_label")) %>%
  mutate(
    eventType = case_when(is.na(comment) == FALSE ~ paste0("station - ", comment),
                          TRUE ~ "station"),
    parentEventID = "https://www.wikidata.org/entity/Q136876043", 
    # latitude = start, latitude_end = end
    decimalLatitude = sprintf("%.4f",(latitude + latitude_end.y) / 2), 
    decimalLongitude = sprintf("%.4f",(longitude + longitude_end.y) / 2), 
    geodeticDatum = "WGS84",
    coordinateUncertaintyInMeters = case_when(
      is.na(latitude) | is.na(longitude) | is.na(latitude_end.y) | is.na(longitude_end.y) ~ NA_character_, TRUE ~ as.character(round(
        geosphere::distHaversine(
          cbind(longitude, latitude),
          cbind(longitude_end.y, latitude_end.y)
        ) / 2
      ))),
    footprintWKT = case_when(
      is.na(latitude) | is.na(longitude) ~ NA_character_,
      is.na(latitude_end.y) | is.na(longitude_end.y) ~ paste0(
        "POINT (", longitude, " ", latitude, ")"
      ), 
      latitude == latitude_end.y & longitude == longitude_end.y ~ paste0(
        "POINT (", longitude, " ", latitude, ")"), 
      TRUE ~ paste0("LINESTRING (", longitude, " ", latitude, ", ",
        longitude_end.y, " ", latitude_end.y, ")"
      )
    ),
    footprintSRS = case_when(
      !is.na(footprintWKT) ~ "EPSG:4326",
      TRUE ~ NA_character_
    ),
    georeferenceRemarks = case_when(
      !is.na(coordinateUncertaintyInMeters) ~
        "coordinateUncertaintyInMeters calculated as half the start-end Haversine distance. No additional gear- or protocol-specific buffer was applied because method-specific uncertainty information was unavailable.",
      TRUE ~ NA_character_
      ),
    samplingProtocol = case_when(
      method_device == "Agassiz Trawl" ~ paste(method_device, o2a_registry_uri, "https://zenodo.org/records/17912454", sep = " | "),
      method_device == "Pelagic fish trawl" ~ paste(method_device, o2a_registry_uri, "https://zenodo.org/records/17935761", sep = " | "),
      TRUE ~ paste(method_device, o2a_registry_uri, sep = " | ")
    ),
    eventDate = if_else(
      as.Date(date_start) == as.Date(date_end),
      format(as.Date(date_start), "%Y-%m-%d"),
      paste0(
        # lubridate::tz(event$date_start) shows it's UTC
        format(date_start, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        "/",
        format(date_end, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
        )
      ),
    eventTime = if_else(
      # these are columns created in Excel because time_start, time_end are in inconsistent format.
      time_start_ming == time_end_ming,
      # lubridate::tz(event$time_start) shows it's UTC
      format(time_start_ming, "%H:%M:%SZ", tz = "UTC"),
      paste0(
        format(time_start_ming, "%H:%M:%SZ", tz = "UTC"),
        "/",
        format(time_end_ming, "%H:%M:%SZ", tz = "UTC")
      )
    )
  ) %>% # mutate first so that expedition Event is not filled with the above
  add_row(
    eventID = "https://www.wikidata.org/entity/Q136876043",
    parentEventID = "",
    eventType = "expedition",
    eventDate = "2025-12-15/2026-02-02"
    ) %>%
  # remove empty columns
  select(
    where(\(x) {
      if (is.character(x)) {
        !all(is.na(x) | x == "")
      } else {
        !all(is.na(x))
      }
    })
  )


# OCCURRENCE
cp_occ_clean <- cp_occ %>%
  select(-verbatimIdentification) %>%
  rename(
    materialEntityType = content,
    organismQuantity = objectQuantity,
    organismQuantityType = objectQuantityUnit,
    recordedBy = collectedBy
  ) %>%
  mutate(
    # remove \r\n from IDs
    derivedFromMaterialEntityID = str_remove_all(
      derivedFromMaterialEntityID, "\\r\\n"
    ),
    materialEntityID = str_remove_all(
      materialEntityID, "\\r\\n"
    ),
    isPartOfMaterialEntityID = str_remove_all(
      isPartOfMaterialEntityID, "\\r\\n"
    ),
    verbatimIdentification = scientificName,
    occurrenceID = materialEntityID,
    organismID = derivedFromMaterialEntityID,
    materialEntityType = case_when(
      derivationType == "specimen" ~ "specimen",
      str_detect(materialEntityType, "^whole body ") ~ "whole body",
      TRUE ~ materialEntityType
    ),
    preparations = case_when(
      str_detect(organismQuantityType, "^whole body in ") ~ 
        str_replace(organismQuantityType, "^whole body in\\s+", ""),
      str_detect(organismQuantityType, "^whole body ") ~ 
        str_replace(organismQuantityType, "^whole body\\s+", ""),
      TRUE ~ preparations
      ),
    recordedByID = case_when(
      recordedBy == "Chiara Papetti" ~ "https://orcid.org/0000-0002-4567-459X",
      TRUE ~ NA_character_
    ),
    identifiedByID = case_when(
      identifiedBy == "Chiara Papetti" ~ "https://orcid.org/0000-0002-4567-459X",
      TRUE ~ NA_character_
    ),
    identificationQualifier = case_when(
      str_detect(scientificName, "\\bsp\\.$") ~ "sp.",
      TRUE ~ NA_character_
    ),
    scientificName = scientificName %>%
      str_remove("\\s*\\([^)]*\\)") %>%  # remove bracketed text
      str_remove("\\s+sp\\.$") %>%      # remove trailing sp.
      str_squish(),
    scientificName = case_when(
      scientificName == "Macrourus withsoni" ~ "Macrourus whitsoni",
      TRUE ~ scientificName
    ),
    basisOfRecord = case_when(
      materialEntityType == "specimen" ~ "PreservedSpecimen",
      materialEntityType == "whole body" ~ "PreservedSpecimen",
      TRUE ~ "MaterialEntity"
    ),
    occurrenceStatus = "present",
    organismQuantityType = case_when(
      organismQuantityType %in% c(
        "individual",
        "whole body in EtOH",
        "whole body in PFA 4%",
        "whole body frozen",
        "tag"
      ) & organismQuantity > 1 ~ "individuals",
      organismQuantityType %in% c(
        "individual",
        "whole body in EtOH",
        "whole body in PFA 4%",
        "whole body frozen",
        "tag"
      ) & organismQuantity <= 1 ~ "individual",
      TRUE ~ "sample"
    ),
  ) %>%
  # remove empty columns
  select(where(~ !all(is.na(.) | . == ""))) %>%
  mutate(occurrenceRemarks = NA_character_) %>%
  # remove duplicated rows at this point. some specimen is also listed as whole body, but with different preparations (specimen has empty preparations). 
  distinct()  

# TAXON MATCH
# get unique scientificName for taxon match
names_to_match <- cp_occ_clean %>%
  distinct(scientificName) %>%
  pull(scientificName)

# taxon match
worms_matches <- wm_records_names(names_to_match)

# wm_records_names returns a list of data frames, this transforms it into a table.
worms_table <- map2_dfr(
  names_to_match,
  worms_matches,
  function(input_name, res) {
    if (is.null(res) || nrow(res) == 0) {
      tibble(
        scientificName_clean = input_name,
        AphiaID = NA_integer_,
        scientificname = NA_character_,
        authority = NA_character_,
        status = NA_character_,
        rank = NA_character_,
        kingdom = NA_character_,
        phylum = NA_character_,
        class = NA_character_,
        order = NA_character_,
        family = NA_character_,
        genus = NA_character_,
        lsid = NA_character_
      )
    } else {
      res %>%
        as_tibble() %>%
        mutate(scientificName_clean = input_name)
    }
  }
) %>% transmute(
  scientificName_clean,
  scientificNameID = lsid,
  scientificNameAuthorship = authority,
  kingdom,
  phylum,
  class,
  order,
  family,
  genus,
  taxonRank = rank
)


# POPULATE EMPTY EVENTID
# create an eventID lookup table because eventID is not populated for derived material entities, only at specimen/ecological data level.
event_lookup <- cp_occ_clean %>%
  filter(!is.na(eventID), eventID != "") %>%
  distinct(derivedFromMaterialEntityID, eventID) %>%
  add_count(derivedFromMaterialEntityID, name = "n_eventID") %>%
  # filter(n_eventID == 1) %>%
  select(derivedFromMaterialEntityID, eventID_lookup = eventID)


occ <- cp_occ_clean %>%
  left_join(
    event_lookup,
    by = "derivedFromMaterialEntityID",
    relationship = "many-to-many"
  ) %>%
  mutate(
    eventID = case_when(
      is.na(eventID) | str_squish(eventID) == "" ~ eventID_lookup,
      TRUE ~ eventID
    ),
    occurrenceID = as.character(occurrenceID),
    occurrenceRemarks = as.character(occurrenceRemarks)
  ) %>%
  select(-eventID_lookup) %>%
  
  # Drop occurrences without an eventID
  # filter(
  #   !is.na(eventID),
  #   str_squish(eventID) != ""
  # ) %>%
  
  # Remove specimen when a preferred derivation type exists
  mutate(
    derivationType_clean = str_to_lower(
      str_squish(derivationType)
    )
  ) %>%
  group_by(occurrenceID) %>%
  filter(
    is.na(occurrenceID) |
      str_squish(occurrenceID) == "" |
      !(
        derivationType_clean == "specimen" &
          any(
            derivationType_clean %in%
              c("tagged individual", "tissue sample of"),
            na.rm = TRUE
          )
      )
  ) %>%
  ungroup() %>%
  select(-derivationType_clean) %>%
  
  # Identify duplicated occurrenceIDs
  add_count(occurrenceID, name = "occurrenceID_n") %>%
  mutate(
    duplicated_occurrenceID =
      occurrenceID_n > 1 &
      !is.na(occurrenceID) &
      str_squish(occurrenceID) != "",
    
    # An ID can only be prefixed when eventID is available
    can_prefix_occurrenceID =
      duplicated_occurrenceID &
      !is.na(eventID) &
      str_squish(eventID) != "",
    
    duplication_note = case_when(
      can_prefix_occurrenceID ~ str_c(
        "The original occurrenceID (",
        occurrenceID,
        ") was duplicated; eventID was prepended to create a unique identifier."
      ),
      
      duplicated_occurrenceID ~ str_c(
        "The occurrenceID (",
        occurrenceID,
        ") is duplicated, but no eventID was available to create a unique identifier."
      ),
      
      TRUE ~ NA_character_
    ),
    
    # Preserve existing remarks and append the new note
    occurrenceRemarks = case_when(
      !duplicated_occurrenceID ~ occurrenceRemarks,
      
      is.na(occurrenceRemarks) |
        str_squish(occurrenceRemarks) == "" ~
        duplication_note,
      
      TRUE ~ str_c(
        occurrenceRemarks,
        duplication_note,
        sep = "; "
      )
    ),
    
    # Prefix duplicated occurrenceIDs
    occurrenceID = if_else(
      can_prefix_occurrenceID,
      str_c(eventID, occurrenceID, sep = "_"),
      occurrenceID
    )
  ) %>%
  select(
    -occurrenceID_n,
    -duplicated_occurrenceID,
    -can_prefix_occurrenceID,
    -duplication_note
  ) %>%
  
  # Put identifier columns first
  relocate(
    eventID,
    occurrenceID,
    organismID
  ) %>%
  
  # Add taxonomy after resolving occurrence duplicates
  left_join(
    worms_table,
    by = c("scientificName" = "scientificName_clean"),
    relationship = "many-to-many"
  )

# occurrence record with eventID that doesn't match to any eventID in event table
occ %>%
  anti_join(
    event %>% select(eventID),
    by = "eventID"
  ) %>%
  distinct(eventID)

duplicates <- occ %>%
  count(occurrenceID, name = "n") %>%
  filter(n > 1) %>%
  arrange(desc(n))

occurrences_without_eventID <- occ %>%
  filter(is.na(eventID) | trimws(eventID) == "")


# EMOF
emof <- event %>%
  select(
    eventID,
    depth_start_in_metres,
    depth_end_in_metres
  ) %>%
  pivot_longer(
    cols = c(depth_start_in_metres, depth_end_in_metres),
    names_to = "depth_field",
    values_to = "measurementValue"
  ) %>%
  filter(!is.na(measurementValue)) %>%
  mutate(
    measurementValue = as.integer(round(measurementValue)),
    measurementType = case_when(
      depth_field == "depth_start_in_metres" ~
        "Depth (spatial coordinate) of sampling event start relative to water surface in the water body",
      depth_field == "depth_end_in_metres" ~
        "Depth (spatial coordinate) of sampling event end relative to water surface in the water body"
    ),
    measurementTypeID = case_when(
      depth_field == "depth_start_in_metres" ~
        "http://vocab.nerc.ac.uk/collection/P01/current/DXPHPRST/",
      depth_field == "depth_end_in_metres" ~
        "http://vocab.nerc.ac.uk/collection/P01/current/DXPHPRXN/"
    ),
    measurementUnit   = "Metres",
    measurementUnitID =
      "http://vocab.nerc.ac.uk/collection/P06/current/ULAA/",
    measurementID = paste(eventID, depth_field, sep = "_")
  ) %>%
  select(
    eventID,
    measurementID,
    measurementType,
    measurementTypeID,
    measurementValue,
    measurementUnit,
    measurementUnitID
  )


# write to files
data_dir = here("data", "03_output")
write_tsv(occ, here(data_dir, "occurrence.txt"), na = "")
write_tsv(event, here(data_dir, "event.txt"), na = "")
write_tsv(emof, here(data_dir, "emof.txt"), na = "")

