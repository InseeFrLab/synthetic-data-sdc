if (!requireNamespace("aws.s3", quietly = TRUE)) install.packages("aws.s3"); library(aws.s3)
if (!requireNamespace("synthpop", quietly = TRUE)) install.packages("synthpop"); library(synthpop)

# Import des données -----------------------------------------------------------
FILE_KEY_IN_S3_1 <- "20240512_sim_synthpop_sample_cart_ctree_parametric_bag_rf_500_sims.RDS"
BUCKET = "projet-donnees-synthetiques"
BUCKET_SIM_1 = file.path(BUCKET, "simulations")

data <- aws.s3::s3read_using(
  FUN = readRDS,
  object = FILE_KEY_IN_S3_1,
  bucket = BUCKET_SIM_1,
  opts = list("region" = "")
)
str(data, max.level=1)
methodes <- which(names(data) != "original")
# ------------------------------------------------------------------------------

vars <- c("sex", "age", "edu", "marital", "income", "ls", "wkabint")
ods <- SD2011[, vars]
my.seed <- 17914709
sds.default <- syn(ods, seed = my.seed)

"
Imaginons qu'on veuille synthétiser toutes les variables sauf wkabint et que :
- On exclut ls des predicteurs de marital
- On utilise income comme predicteur de ls, edu, marital mais on ne la synthetise pas.
- On utilise une regression logistique polytomique pour générer marital.
"

"
1) Dans la visit sequence on met tout sauf l'emplacement de wkabint qui ici est 7
2) Dans le vecteur method on va mettre polyreg a l'emplacment de marital et on ne 
va rien mettre à la position de wkabint et income (car non synthétisées)
3) On met le paramètre drop.not.used sur FALSE pour que le modèle garde l'information
des variables non synthétisées.
"
visit.sequence.ini <- c(1, 2, 5, 6, 4, 3)
method.ini <- c("sample", "ctree", "ctree", "polyreg", "", "ctree", "")
sds.ini <- syn(data = ods,
               visit.sequence = visit.sequence.ini,
               method = method.ini,
               m = 0,
               drop.not.used = FALSE)

sds.ini$predictor.matrix

"
4) On modifie la predictor matrix au croisement entre marital et ls pour que ls 
ne soit pas utilsé comme predicteur de marital
"
predictor.matrix.corrected <- sds.ini$predictor.matrix
predictor.matrix.corrected["marital", "ls"] <- 0
predictor.matrix.corrected




