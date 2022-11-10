runapi <- FALSE

require(rccola)
require(Hmisc)
require(data.table)
require(qs)
getRs('importREDCap.r')   # fetch cleanupREDCap function from Github

# Before running this script install Shawn Garbett's branch of redcapAPI
# cd ~
# sudo R
# installGithubPac('spgarbet/redcapAPI')
# installGithubPac defined at https://hbiostat.org/attach/.Rprofile
# If you don't care where you install it just run this in R:
# devtools::install_github('spgarbet/redcatAPI', build_vignettes=TRUE)
# Also install Shawn's latest rccola using spgarbet/rccola

# Define vector of patient IDs to drop in all datasets
drop_ids <- 737

api_url<- 'https://redcap.vanderbilt.edu/api/'

baseline <-
  .q(contact_information,
     consent_documentation,
     demographics,
     pregnancy_assessment,
     inclusionexclusion,
     address_verification,
     randomization,
     pharmacy_confirmation,
     randomization_result,
     shipment_tracking,
     sdv,
     payment_form)

longitudinal <-
  .q(participant_reported_outcomes,
     pro_metadata,
     participant_status,
     pi_data_verification,
     ad_hoc_report,
     healthcare_encounter,
     concomitant_therapy,
     adverse_event_form,
     dcri_sae_report,
     dcri_ae_review,
     participant_contact_log,
     unblinding_request,
     unblinding_result,
     sdv,
     vcc_ctom_unblinding_result,
     notes_to_file,
     results_reporting,
     adjudication)

if(runapi) {
# Fetch baseline-only forms
when <- Sys.time()
B <- new.env()
drinkREDCap(
  'eds',
  keyring="activ6",
  url=api_url,
  envir=B,
  forms=list(eds=cbaseline),
  fields='record_id',
  events='screening_arm_1',
  checkboxLabels=TRUE)   # 180s
B <- as.list(B)
# Remove eds. from start of dataset names
names(B) <- sub('^eds\\.', '', names(B))
sapply(B, dim)
attr(B, 'export_time') <- when
qsave(B, 'B.qs')

L <- new.env()
drinkREDCap(
  'eds',
  keyring="activ6",
  url=api_url,
  envir=L,
  forms=list(eds=longitudinal),
  fields='record_id',
  checkboxLabels=TRUE)   # 539s
L <- as.list(L)
# Remove eds. from start of dataset names
names(L) <- sub('^eds\\.', '', names(L))
sapply(L, dim)   # make sure no. of rows is always the same
attr(L, 'export_time') <- when
qsave(L, 'L.qs')

# Other datasets
O <- new.env()   # These are not used for now
drinkREDCap(
  .q(interested, deviations, database, econsent, operation)[-c(3,5)],
  keyring='active6',
  url=api_url,
  envir=O,
  forms=list(interested= .q(interested, site_assignment),
             deviations= .q(protocol_deviation),
             # database  = .q(user), # not avail to me
             econsent  = .q(participant_information, consent, consent_spanish, consent_v5, consent_v5_spanish, 
                            consent_v6, consent_v6_spanish)),
             # operation = .q(reporting_dataset)), # not avail
  fields='record_id',
  checkboxLabels=TRUE)   # 209s
O <- as.list(O)
attr(O, 'export_time') <- when
qsave(O, 'O.qs')

# Retrieve site information
S <- new.env()
drinkREDCap('site', keyring='active6', url=api_url, envir=S,
            forms='basic_site_information', checkboxLabels=TRUE)  # 5s
S <- as.list(S)
attr(S, 'export_time') <- when
qsave(S, 'S.qs')

} else {
  B <- qread('B.qs')
  L <- qread('L.qs')
  S <- qread('S.qs')
  when <- attr(B, 'export_time')
}

g <- function(x) 'record_id' %in% names(x)
sapply(B, g)
sapply(L, g)
g <- function(x) 'redcap_event_name' %in% names(x)
sapply(B, g)
sapply(L, g)
g <- function(x) 'patid' %in% names(x)
sapply(B, g)
sapply(L, g)
g <- function(x) length(unique(x$redcap_event_name))
sapply(B, g)
sapply(L, g)
g <- function(x) 'redcap_repeat_instrument' %in% names(x)
sapply(B, g)
sapply(L, g)

table(B$contact_information$redcap_event_name)

# Make all the baseline data frames data tables with record_id
# as the key.  Remove 4 variables from them

for(n in names(B)) {
  setDT(B[[n]], key='record_id')
  B[[n]][, .q(redcap_event_name, redcap_repeat_instrument,
              redcap_repeat_instance, redcap_survey_identifier) := NULL]
}

# For all longitudinal forms restructure the event variable and convert
# to data.table with key of record_id and day
# Delete original variable along with redcap_survey_identifier

etrans <- function(event) {
  event     <- sub('_arm_1', '', event)
  event[event == 'screening'] <- 'day_-1'
  sub('day_([1-9])$', 'day_0\\1', event)
}
  
for(n in names(L)) {
  L[[n]]$day <- etrans(L[[n]]$redcap_event_name)
  setDT(L[[n]], key=.q(record_id, day))
  L[[n]][, .q(redcap_event_name, redcap_survey_identifier) := NULL]
}

# Put baseline, longitudinal, and site data tables in one large R object
R <- c(B, L, S)


# Remove html tags from variable labels and convert all sequences of multiple choices
# into single multiple choice variables using Hmisc::mChoice

R <- lapply(R, cleanupREDCap)

# Remove patients in drop_ids
if(length(drop_ids)) {
  Rorig <- copy(R)
  rec <- rep(0, length(R))
  names(rec) <- names(R)
  for(n in names(R)) {
    r <- R[[n]]
    if('record_id' %nin% names(r)) next
    m <- r[, sum(record_id %in% drop_ids)]
    rec[[n]] <- m
    if(m == 0) next
    R[[n]] <- r[record_id %nin% drop_ids]
  }
  cat('\nNumber of records dropped due to drop_ids:\n\n')
  print(rec)
}

# Add export date/time as an attribute
attr(R, 'export_time') <- when

qsave(R, 'R.qs')    # 11B; 5.2MB using saveRDS(..., compress='xz')

# Overview all the datasets in R
multDataOverview(R, ~ record_id)

# Store in https://vumc365.sharepoint.com/:f:/r/sites/activ6-blinded-analyses/Shared%20Documents/unblinded?csf=1&web=1&e=mzGcKV

# To fetch R install the qs package and run R <- qread('R.qs')
# To get the export date/time stamp run attr(R, 'export_time')
# To get an overview of R:
# require(Hmisc); getRs('reptools.r')
# m <- multDataOverview(R, ~ record_id)
# print(m) if you want to see which datasets each variable is in (other than record_id)

# Examples of use of the new race_eth multiple choice variable

r <- R$demographics$race_eth
levels(r)
summary(r)
# Get frequency table of all combinations, numeric form
table(r)
# Same but with labels
table(as.character(r))
# Count number of persons with 'American Indian or Alaska Native' as a choice
sum(inmChoice(r, 1))
sum(inmChoice(r, 'American Indian or Alaska Native'))
# Count number of persons in either of two categories
sum(inmChoice(r, c('American Indian or Alaska Native', 'White')))
sum(inmChoice(r, 'White'))
# Count number in both categories
sum(inmChoice(r, 'American Indian or Alaska Native') &
    inmChoice(r, 'White'))
sum(inmChoice(r, c('American Indian or Alaska Native',
                   'White'), condition='all'))
      


d <- R$eds.participant_reported_outcomes
# Compute fraction of NAs or '' across variables, per row
setDT(d)
ismiss <- function(x)
  if(is.character(x) || is.factor(x)) is.na(x) | trimws(x) == '' else is.na(x)

## Replace each variable with missingness indicator
di <- d[, lapply(.SD, ismiss)]
apply(! di, 2, sum)
fna <- apply(di, 1, sum) / ncol(di)
hist(fna, nclass=100)

# Get list of variables that were ever missing then for these find obs
# that are missing on all of them
m <- apply(di, 2, sum)

# Find records with > 0.95 missing variables of variables that were ever missingand compute number of
# non-NAs per variable
w <- di[fna > 0.95, ]
apply(! w, 2, sum)
describe(d$meds_hist_checks___6)


# Get all metadata from REDCap using rccola metaREDCap function
# Uses key_get from the keyring package which is required by rccola

apikey <- keyring::key_get(service='rccola', username='eds', keyring='activ6')
meta <- metaREDCap(apikey, url=api_url)
head(meta)
qsave(meta, 'meta.qs')
