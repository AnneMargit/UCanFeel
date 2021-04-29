# export school thingies

# run once:
# install.packages('stringr')

setwd('D:\\data\\u-can-feel')
data = read.csv2('responses_consent_otr_2021-04-06.csv')

# Filteren op ingevulde metingen
data = data[data$completed_at != "",]

# Filteren op school
data = data[data$v8child == "" | data$v10parent == "",]

# filteren op toestemming kind | (toestemming ouders & toestemming ouders voor persoonsgegevens)
data = data[(data$v3parent == "Ja, ik geef toestemming voor deelname van mijn kind aan het onderzoek. Ik geef de onderzoekers ook toestemming om mijn kind te vragen over zijn/haar geloofsovertuiging en over zijn/haar gezondheid. Deze toestemming loopt tot 01-09-2023." & data$v5parent == "Ja, ik geef toestemming voor de verwerking van de persoonsgegevens van mijn kind, zoals vermeld in de onderzoeksinformatie. Ik weet dat ik of mijn kind tot 01-09-2023 kan vragen om de gegevens van mijn kind te laten verwijderen, zoals uitgelegd in de informatiebrief. Ook als mijn kind besluit om te stoppen met deelname, kan ik of mijn kind hierom vragen.") | data$v3child == "Ja, ik geef toestemming om mee te doen aan het onderzoek. Ik geef de onderzoekers ook toestemming om mij te vragen over mijn geloofsovertuiging en over mijn gezondheid. Deze toestemming loopt tot 01-09-2023, tenzij ik besluit om eerder te stoppen. Ik geef ook toestemming voor het verwerken van mijn persoonsgegevens zoals uitgelegd in de informatiebrief.",]

mobile_phones <- NULL
emails <- NULL

EMAIL_REGEX <- '^([\\w+\\-]\\.?)+@[a-z\\d\\-]+(\\.[a-z]+)*\\.[a-z]+$'
MOBILE_PHONE_REGEX <- '^06[0-9]{8}$'

normalize_mobile_phone <- function(mobile_phone_orig) {
  mobile_phone <- gsub(" ", "", mobile_phone_orig)
  mobile_phone <- gsub("-", "", mobile_phone)
  if (!stringr::str_detect(mobile_phone, MOBILE_PHONE_REGEX)) {
    stop(paste('phone number is not valid:', mobile_phone_orig))
  }
  return(mobile_phone)
}

normalize_email <- function(email_orig) {
  email <- gsub(" ", "", email_orig)
  email <- gsub("-", "", email)
  if (!stringr::str_detect(email, stringr::regex(EMAIL_REGEX, ignore_case = TRUE))) {
    stop(paste('email is not valid:', email_orig))
  }
  return(email)
}

for (row_index in nrow(data):1) {
  row <- data[row_index,]
  mobile_phone <- NA
  email <- NA
  if (row$v1 == "Ouder van een leerling van 15 of jonger") {
    # get email and mobile phone from parent columns
    mobile_phone <- row$v8parent
    email <- row$v9parent
  } else if (row$v1 == "Leerling van 16 of ouder") {
    # get email and mobile phone from child columns 
    mobile_phone <- row$v6child
    email <- row$v7child
  }
  mobile_phone <- normalize_mobile_phone(mobile_phone)
  email <- normalize_email(email)
  if (mobile_phone %in% mobile_phones) {
    print(paste('mobile_phone already in list, skipping...', mobile_phone))
  } else {
    mobile_phones <- c(mobile_phones, mobile_phone)
    emails <- c(emails, email)
  }
}

# generate R code
result <- data.frame(email = emails, mobile_phone=mobile_phones, stringsAsFactors = FALSE)

# View(result)

final_string <- paste('PHONE_NUMBERS = %w[',paste(mobile_phones, collapse=' '),']', sep = '')
cat(final_string)
