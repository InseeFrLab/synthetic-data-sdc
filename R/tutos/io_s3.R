library(aws.s3)
library(dplyr)
library(arrow)
# library(here)

BUCKET = "projet-donnees-synthetiques"
BUCKET_SIM = file.path(BUCKET, "simulations")

# Les fichiers du bucket 
aws.s3::get_bucket(BUCKET, region = "")
aws.s3::put_bucket(BUCKET_SIM, region = "")


# Export en csv (à privilégier pour interopérabilité)

FILE_KEY_OUT_S3 = "iris.csv"

aws.s3::s3write_using(
  iris,
  FUN = readr::write_csv, 
  append = TRUE,
  object = FILE_KEY_OUT_S3,
  bucket = BUCKET_SIM,
  opts = list("region" = "")
)
aws.s3::get_bucket(BUCKET_SIM, region = "")

# Export d'objets R en RData

s3save(iris,
       object = "iris.RData", 
       bucket = BUCKET,
       opts = list("region" = ""))

# Export d'objets R en rds

FILE_KEY_OUT_S3 = "iris.RDS"

aws.s3::s3write_using(
  iris,
  FUN = saveRDS,
  object = FILE_KEY_OUT_S3,
  bucket = BUCKET,
  opts = list("region" = "")
)

get_bucket(BUCKET, region = "")

# Export d'objets au format parquet (Interopérable Python etc également )
FILE_KEY_OUT_S3 = "iris.parquet"

aws.s3::s3write_using(
  iris,
  FUN = arrow::write_parquet,
  object = FILE_KEY_OUT_S3,
  bucket = BUCKET,
  opts = list("region" = "")
)

get_bucket(BUCKET, region = "")

# ou par ligne de commande
# install.packages("here")
# library(here)
# system(paste0("mc cp ", here("iris.parquet"), " s3/", BUCKET, "/iris.parquet"))

# Import csv avec read_csv

FILE_KEY_IN_S3 = "iris.csv"

iris_csv <- aws.s3::s3read_using(
  FUN = readr::read_csv,
  object = FILE_KEY_IN_S3,
  bucket = BUCKET,
  opts = list("region" = "")
)
str(iris_csv)

# Import csv avec fread (très rapide sur grosses données) 

FILE_KEY_IN_S3 = "iris.csv"

iris_fread <- aws.s3::s3read_using(
  FUN = data.table::fread,
  object = FILE_KEY_IN_S3,
  bucket = BUCKET,
  opts = list("region" = "")
)
str(iris_fread) # => format data.table

# Import RData

FILE_KEY_IN_S3 = "iris.RData"

aws.s3::s3load(
  object = FILE_KEY_IN_S3,
  bucket = BUCKET,
  opts = list("region" = "")
) # ne marche pas

# Import RDS

FILE_KEY_IN_S3 = "iris.RDS"

iris_rds <- aws.s3::s3read_using(
  FUN = readRDS,
  object = FILE_KEY_IN_S3,
  bucket = BUCKET,
  opts = list("region" = "")
)
str(iris_rds)

# Import csv avec fread (très rapide sur grosses données) 

FILE_KEY_IN_S3 = "iris.csv"

iris_fread <- aws.s3::s3read_using(
  FUN = data.table::fread,
  object = FILE_KEY_IN_S3,
  bucket = BUCKET,
  opts = list("region" = "")
)
str(iris_fread) # => format data.table


# Import parquet (très rapide sur grosses données) 
FILE_KEY_OUT_S3 = "iris.parquet"

iris_parquet <- aws.s3::s3read_using(
  FUN = arrow::read_parquet,
  object = FILE_KEY_OUT_S3,
  bucket = BUCKET,
  opts = list("region" = "")
)
str(iris_parquet) # tibble


# Import connexion à un fichier parquet (pas d'import direct des données)
FILE_KEY_IN_S3 = "iris.parquet"

bucket <- arrow::s3_bucket(
  bucket = BUCKET,
  access_key = Sys.getenv("AWS_ACCESS_KEY_ID"),
  secret_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
  session_token = Sys.getenv("AWS_SESSION_TOKEN"),
  scheme = "https",
  endpoint_override = Sys.getenv("AWS_S3_ENDPOINT"),
  region = Sys.getenv("AWS_DEFAULT_REGION")
)
bucket$ls("")
# data_pqt <- open_dataset(bucket$path("repo_data_pqt"))
iris_conn <- read_parquet(bucket$path("iris.parquet"), as_data_frame = FALSE)

str(iris_conn) # objet Arrow
iris_conn # structure des données dans la connexion

# Interaction avec syntaxe dplyr
iris_conn %>%
  filter(Species == "versicolor") %>% 
  collect()

