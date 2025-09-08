meta2024 <- data.frame(
  Title = "JASPAR2024",
  Description = paste0("Data package for JASPAR2024, ",
                       "a popular database of transcription",
                       "factor motif profiles"),
  BiocVersion = "3.18",
  Genome = NA,
  SourceType = "MySQL",
  SourceUrl = "https://jaspar2022.genereg.net/download/database/JASPAR2024.sqlite",
  SourceVersion = "Nov 10 20121",
  Species = NA,
  TaxonomyId = NA,
  Coordinate_1_based = NA,
  DataProvider = paste0("JASPAR is brought to you by a collaborative effort of",
                        "several research labs and it is licensed under the",
                        "Creative Commons Attribution 4.0 International License"),
  Maintainer = "Damir Baranasic <damir.baranasic@lms.mrc.ac.uk>",
  RDataClass = "JASPAR2024",
  DispatchClass = "FilePath",
  ResourceName = "JASPAR2024",
  Location_Prefix = "https://jaspar.elixir.no/",
  RDataPath = "download/database/JASPAR2024.sqlite",
  Tags = "AnnotationData:FunctionalAnnotation:AnnotationHub"
)

meta2022 <- data.frame(
  Title = "JASPAR2022",
  Description = paste0("Data package for JASPAR2022, ",
                       "a popular database of transcription",
                       "factor motif profiles"),
  BiocVersion = "3.15",
  Genome = NA,
  SourceType = "MySQL",
  SourceUrl = "https://jaspar2022.genereg.net/download/database/JASPAR2022.sqlite",
  SourceVersion = "Nov 10 2021",
  Species = NA,
  TaxonomyId = NA,
  Coordinate_1_based = NA,
  DataProvider = paste0("JASPAR is brought to you by a collaborative effort of",
                        "several research labs and it is licensed under the",
                        "Creative Commons Attribution 4.0 International License"),
  Maintainer = "Damir Baranasic <damir.baranasic@lms.mrc.ac.uk>",
  RDataClass = "JASPAR2022",
  DispatchClass = "FilePath",
  ResourceName = "JASPAR2022",
  Location_Prefix = "https://jaspar2022.genereg.net/",
  RDataPath = "download/database/JASPAR2022.sqlite",
  Tags = "AnnotationData:FunctionalAnnotation:AnnotationHub"
)

meta <- rbind(meta2024, meta2025)

write.csv(meta, file=paste0(system.file("extdata",
                                        package=pkgname, lib.loc=libname),
                            "/", "metadata.csv", row.names=FALSE))
