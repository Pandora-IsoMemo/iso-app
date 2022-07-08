test_that("Test module importData", {
  testServer(importDataServer,
             {
               # Arrange
               print("test empty data input")
               # Act
               session$setInputs(openPopup = TRUE)
               expect_equal(session$returned(), list())
             })

  testServer(importDataServer,
             {
               # Arrange
               print("test import data from ckan")
               # Act
               session$setInputs(
                 openPopup = TRUE,
                 source = "ckan",
                 ckanRecord = "CIMA: Compendium Isotoporum Medii Aevi",
                 ckanResource = "CIMA Humans 29.05.2021",
                 type = "xlsx",
                 rownames = FALSE,
                 accept = TRUE
               )
               expect_equal(
                 names(session$returned()),
                 "https://pandoradata.earth/dataset/cbbc35e0-af60-4224-beea-181be10f7f71/resource/f7581eb1-b2b8-4926-ba77-8bc92ddb4fdb/download/cima-humans.xlsx"
               )
               expect_true(all(
                 c("Entry.ID", "Reference", "Link", "DOI") %in% names(session$returned()[[1]])
               ))
               expect_true(nrow(session$returned()[[1]]) > 100)

               expect_equal(
                 colnames(session$returned()[[1]])[1:10],
                 c(
                   "Entry.ID",
                   "Submitter.ID",
                   "Context.ID",
                   "Individual.ID",
                   "Sample.ID",
                   "Sex",
                   "Age.Category",
                   "Min..Age.(yrs)",
                   "Max..Age.(yrs)",
                   "Sampled.Element"
                 )
               )
             })
})

test_that("cutAllLongStrings function", {
  testData <-
    structure(
      list(
        Entry.ID = c(1, 2, 3, 4, 5, 6),
        Context.ID = c(
          NA_character_,
          NA_character_,
          NA_character_,
          NA_character_,
          NA_character_,
          NA_character_
        ),
        Individual.ID = c(
          "Høre kranie",
          "Ringebu 3A",
          "Bergen",
          "Uvdal",
          "Ringebu 3B",
          "102"
        ),
        Sample.ID = c(NA, NA, NA,
                      NA, NA, "VHM 24"),
        Sex = c(NA, NA, NA, NA, NA, "M"),
        Latitude = c(
          61.153097,
          61.527761,
          60.393642,
          60.273504,
          61.527761,
          58.385741
        ),
        Longitude = c(8.80468, 10.14467, 5.319837,
                      8.243344, 10.14467, 13.646216),
        Reference = c(
          "Åberg, G., Fosse, G., Stray, H. (1998). Man, nutrition and mobility: A comparison of teeth and bone from the Medieval era and the present from Pb and Sr isotopes. The Science of the Total Environment 224: 109-119.",
          "Åberg, G., Fosse, G., Stray, H. (1998). Man, nutrition and mobility: A comparison of teeth and bone from the Medieval era and the present from Pb and Sr isotopes. The Science of the Total Environment 224: 109-119.",
          "Åberg, G., Fosse, G., Stray, H. (1998). Man, nutrition and mobility: A comparison of teeth and bone from the Medieval era and the present from Pb and Sr isotopes. The Science of the Total Environment 224: 109-119.",
          "Åberg, G., Fosse, G., Stray, H. (1998). Man, nutrition and mobility: A comparison of teeth and bone from the Medieval era and the present from Pb and Sr isotopes. The Science of the Total Environment 224: 109-119.",
          "Åberg, G., Fosse, G., Stray, H. (1998). Man, nutrition and mobility: A comparison of teeth and bone from the Medieval era and the present from Pb and Sr isotopes. The Science of the Total Environment 224: 109-119.",
          "Åborg, D.C. (2013). Hierarchy through Diet. Stable isotope analysis of male graves of the estate church graveyard in Varnhem. Unpublished BA dissertation: Stockholm University."
        ),
        Link = c(
          "https://www.sciencedirect.com/science/article/pii/S0048969798003477",
          "https://www.sciencedirect.com/science/article/pii/S0048969798003477",
          "https://www.sciencedirect.com/science/article/pii/S0048969798003477",
          "https://www.sciencedirect.com/science/article/pii/S0048969798003477",
          "https://www.sciencedirect.com/science/article/pii/S0048969798003477",
          "http://www.diva-portal.org/smash/record.jsf?pid=diva2%3A622264&dswid=-9506"
        ),
        Publication.Year = c(1998,
                             1998, 1998, 1998, 1998, 2013),
        IRMS.Lab.Institution.Stable.Sulphur.Measurement = c(NA,
                                                            NA, NA, NA, NA, "Stockholm University")
      ),
      row.names = c(NA, 6L),
      class = "data.frame"
    )

  expect_equal(
    cutAllLongStrings(testData, cutAt = 30),
    structure(
      list(
        Entry.ID = c(1, 2, 3, 4, 5, 6),
        Context.ID = c(
          NA_character_,
          NA_character_,
          NA_character_,
          NA_character_,
          NA_character_,
          NA_character_
        ),
        Individual.ID = c(
          "Høre kranie",
          "Ringebu 3A",
          "Bergen",
          "Uvdal",
          "Ringebu 3B",
          "102"
        ),
        Sample.ID = c(NA, NA, NA, NA,
                      NA, "VHM 24"),
        Sex = c(NA, NA, NA, NA, NA, "M"),
        Latitude = c(
          61.153097,
          61.527761,
          60.393642,
          60.273504,
          61.527761,
          58.385741
        ),
        Longitude = c(8.80468,
                      10.14467, 5.319837, 8.243344, 10.14467, 13.646216),
        Reference = c(
          "Åberg, G., Fosse, G., Stray, H...",
          "Åberg, G., Fosse, G., Stray, H...",
          "Åberg, G., Fosse, G., Stray, H...",
          "Åberg, G., Fosse, G., Stray, H...",
          "Åberg, G., Fosse, G., Stray, H...",
          "Åborg, D.C. (2013). Hierarchy ..."
        ),
        Link = c(
          "https://www.sciencedirect.com/...",
          "https://www.sciencedirect.com/...",
          "https://www.sciencedirect.com/...",
          "https://www.sciencedirect.com/...",
          "https://www.sciencedirect.com/...",
          "http://www.diva-portal.org/sma..."
        ),
        Publication.Year = c(1998,
                             1998, 1998, 1998, 1998, 2013),
        IRMS.Lab.Institution.Stable... = c(NA,
                                           NA, NA, NA, NA, "Stockholm University")
      ),
      class = "data.frame",
      row.names = c(NA,
                    -6L)
    )
  )
})


test_that("Test formatColumnNames()", {
  vNames <- c("abc", "12hgf", "#j.f", "jg-$jhfl+4", "abc.(237)")

  expect_equal(formatColumnNames(vNames, isTest = TRUE),
               c("abc", "x12hgf", "j.f", "jg..jhfl.4", "abc..237."))
})
