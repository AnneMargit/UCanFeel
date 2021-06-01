# First data collection exploration

library(dplyr)

setwd('D:\\data\\u-can-feel')
data_other_factors = read.csv2('responses_other_factors_2021-05-10.csv', na.strings=c(""," ","NA"))
data_school = read.csv2('responses_school_2021-05-10.csv', na.strings=c(""," ","NA"))
data_wellbeing = read.csv2('responses_wellbeing_2021-05-10.csv', na.strings=c(""," ","NA"))
data_socialnetworks = read.csv2('responses_socialnetworks_2021-05-10.csv', na.strings=c(""," ","NA"))
data_demografie = read.csv2('responses_demografie_2021-05-10.csv', na.strings=c(""," ","NA"))
data_people = read.csv2('people_2021-05-31.csv', na.strings=c(""," ","NA"))
data_consent = read.csv2('responses_consent_otr_2021-05-31.csv', na.strings=c(""," ","NA"))

data_other_factors %>% summarise(complete = sum(!is.na(completed_at)))
data_school %>% summarise(complete = sum(!is.na(completed_at)))
data_wellbeing %>% summarise(complete = sum(!is.na(completed_at)))
data_socialnetworks %>% summarise(complete = sum(!is.na(completed_at)))
data_demografie %>% summarise(complete = sum(!is.na(completed_at)))


MOBILE_PHONE_REGEX <- '^06[0-9]{8}$'


normalize_mobile_phone <- function(mobile_phone_orig) {
  mobile_phone <- gsub(" ", "", mobile_phone_orig)
  mobile_phone <- gsub("-", "", mobile_phone)
  if (!stringr::str_detect(mobile_phone, MOBILE_PHONE_REGEX)) {
    stop(paste('phone number is not valid:', mobile_phone_orig))
  }
  return(mobile_phone)
}

data_consent$v6child <- normalize_mobile_phone(data_consent$v6child)

data_demografie <- data_demografie %>%
  select(-c(response_id, filled_out_for_id, protocol_subscription_id, measurement_id, invitation_set_id,
            open_from, created_at, updated_at, locale))

data_demografie2 <- data_demografie %>%
  rename(id = filled_out_by_id,
         start_demo = opened_at,
         complete_demo = completed_at,
         geslacht = v1,
         leeftijd = v2,
         schooljaar = v3,
         nu_havo = v4_a_havo,
         nu_vmbo_b = v4_a_vmbo_basis,
         nu_vmbo_g = v4_a_vmbo_gemengd,
         nu_vmbo_k = v4_a_vmbo_kader,
         nu_vmbo_tl = v4_a_vmbo_tl,
         nu_vwo = v4_a_vwo_atheneum_of_gymnasium,
         start_havo = v4_b_havo,
         start_vmbo_b = v4_b_vmbo_basis,
         start_vmbo_g = v4_b_vmbo_gemengd,
         start_vmbo_k = v4_b_vmbo_kader,
         start_vmbo_tl = v4_b_vmbo_tl,
         start_vwo = v4_b_vwo_atheneum_of_gymnasium,
         advies_havo = v4_c_havo,
         advies_vmbo_b = v4_c_vmbo_basis,
         advies_vmbo_g = v4_c_vmbo_gemengd,
         advies_vmbo_k = v4_c_vmbo_kader,
         advies_vmbo_tl = v4_c_vmbo_tl,
         advies_vwo = v4_c_vwo_atheneum_of_gymnasium,
         advies_anders = v4_c_anders,
         advies_geen = v4_c_geen_advies,
         advies_weetniet = v4_c_ik_weet_het_niet_meer,
         eenhuis = v5,
         eenhuis_ouders = v6_a,
         eenhuis_andere_volwassenen = v6_b_andere_volwassenen,
         eenhuis_andere_kinderen = v6_b_andere_kinderen,
         eenhuis_stiefouder = v6_b_een_stiefouder_of_vriend_vriendin_van_je_ouder,
         eenhuis_broerzus = v6_b_half_broer_tjes_of_zus_jes,
         eenhuis_opaoma = v6_b_opa_of_oma,
         eenhuis_niemand = v6_b_niemand_anders,
         eerstehuis_nachten = v7_a,
         eerstehuis_ouders = v7_b,
         eerstehuis_andere_kinderen = v7_c_andere_kinderen,
         eerstehuis_stiefouder = v7_c_een_stiefouder_of_vriend_vriendin_van_je_ouder,
         eerstehuis_halfbroerzus = v7_c_half_broer_tjes_of_zus_jes,
         eerstehuis_niemand = v7_c_niemand_anders,
         eerstehuis_stiefbroerzus = v7_c_stiefbroer_tjes_of_stiefzus_jes,
         tweedehuis = v7_d,
         tweedehuis_nachten = v8_a,
         tweedehuis_ouders = v8_b,
         tweedehuis_andere_kinderen = v8_c_andere_kinderen,
         tweedehuis_stiefouder = v8_c_een_stiefouder_of_vriend_vriendin_van_je_ouder,
         tweedehuis_halfbroerzus = v8_c_half_broer_tjes_of_zus_jes,
         tweedehuis_niemand = v8_c_niemand_anders,
         tweedehuis_stiefbroerzus = v8_c_stiefbroer_tjes_of_stiefzus_jes,
         tweedehuis_pleegouder = v8_c_pleegouder_s,
         derdehuis = v8_d
         )

data_demografie3 <- data_demografie2 %>%
  dplyr::mutate(niveau_nu = case_when(nu_havo == "true" ~ "havo",
                                      nu_vmbo_b == "true" ~ "vmbo_b",
                                      nu_vmbo_k == "true" ~ "vmbo_k",
                                      nu_vmbo_g == "true" ~ "vmbo_g",
                                      nu_vmbo_tl == "true" ~ "vmbo_tl",
                                      nu_vwo == "true" ~ "vwo"))
                  
data_demografie3 <- data_demografie3 %>%
  dplyr::mutate(niveau_start = case_when(start_havo == "true" ~ "havo",
                                         start_vmbo_b == "true" ~ "vmbo_b",
                                         start_vmbo_k == "true" ~ "vmbo_k",
                                         start_vmbo_g == "true" ~ "vmbo_g",
                                         start_vmbo_tl == "true" ~ "vmbo_tl",
                                         start_vwo == "true" ~ "vwo"))

data_demografie3 <- data_demografie3 %>%
  dplyr::mutate(niveau_advies = case_when(advies_havo == "true" ~ "havo",
                                         advies_vmbo_b == "true" ~ "vmbo_b",
                                         advies_vmbo_k == "true" ~ "vmbo_k",
                                         advies_vmbo_g == "true" ~ "vmbo_g",
                                         advies_vmbo_tl == "true" ~ "vmbo_tl",
                                         advies_vwo == "true" ~ "vwo",
                                         advies_anders == "true" ~ "anders",
                                         advies_geen == "true" ~ "geen",
                                         advies_weetniet == "true" ~ "weetniet"))

data_demografie3 <- data_demografie3 %>%
  dplyr::mutate(schooljaar = case_when(schooljaar == "Vijfde klas" ~ "5",
                                          schooljaar == "Vierde klas" ~ "4",
                                          schooljaar == "Derde klas" ~ "3",
                                          schooljaar == "Tweede klas" ~ "2"))

data_demografie3 <- data_demografie3 %>%
  dplyr::mutate(geslacht = case_when(geslacht == "Jongen" ~ "0",
                                       geslacht == "Meisje" ~ "1",
                                       geslacht == "Anders" ~ "2"))
                                       

data_demografie4 <- data_demografie3 %>%
  select(-c(7:27))
 

data_demografie4$eenhuis[data_demografie4$eenhuis == "1 huis"] <- "1"
data_demografie4$eenhuis[data_demografie4$eenhuis == "2 of meer huizen"] <- "2"

data_demografie4$eenhuis_ouders[data_demografie4$eenhuis_ouders == "Ja, beide ouders"] <- "2"
data_demografie4$eenhuis_ouders[data_demografie4$eenhuis_ouders == "Ja, een van beide ouders"] <- "1"
data_demografie4$eenhuis_ouders[data_demografie4$eenhuis_ouders == "Nee, <u>geen</u> van beide ouders"] <- "0"

data_demografie4$eerstehuis_nachten[data_demografie4$eerstehuis_nachten == "7 nachten per week"] <- "7"
data_demografie4$eerstehuis_nachten[data_demografie4$eerstehuis_nachten == "6 nachten per week"] <- "6"
data_demografie4$eerstehuis_nachten[data_demografie4$eerstehuis_nachten == "5 nachten per week"] <- "5"
data_demografie4$eerstehuis_nachten[data_demografie4$eerstehuis_nachten == "4 nachten per week"] <- "4"
data_demografie4$eerstehuis_nachten[data_demografie4$eerstehuis_nachten == "3 nachten per week"] <- "3"
data_demografie4$eerstehuis_nachten[data_demografie4$eerstehuis_nachten == "2 nachten per week"] <- "2"
data_demografie4$eerstehuis_nachten[data_demografie4$eerstehuis_nachten == "1 nacht per week"] <- "1"
data_demografie4$eerstehuis_nachten[data_demografie4$eerstehuis_nachten == "Minder dan 1 nacht per week"] <- "0"

data_demografie4$eerstehuis_ouders[data_demografie4$eerstehuis_ouders == "Ja, beide ouders"] <- "2"
data_demografie4$eerstehuis_ouders[data_demografie4$eerstehuis_ouders == "Ja, een van beide ouders"] <- "1"
data_demografie4$eerstehuis_ouders[data_demografie4$eerstehuis_ouders == "Nee, <u>geen</u> van beide ouders"] <- "0"

data_demografie4$tweedehuis[data_demografie4$tweedehuis == "Ja"] <- "1"
data_demografie4$tweedehuis[data_demografie4$tweedehuis == "Nee"] <- "0"

data_demografie4$derdehuis[data_demografie4$derdehuis == "Ja"] <- "1"
data_demografie4$derdehuis[data_demografie4$derdehuis == "Nee"] <- "0"

data_demografie4$tweedehuis_nachten[data_demografie4$tweedehuis_nachten == "7 nachten per week"] <- "7"
data_demografie4$tweedehuis_nachten[data_demografie4$tweedehuis_nachten == "6 nachten per week"] <- "6"
data_demografie4$tweedehuis_nachten[data_demografie4$tweedehuis_nachten == "5 nachten per week"] <- "5"
data_demografie4$tweedehuis_nachten[data_demografie4$tweedehuis_nachten == "4 nachten per week"] <- "4"
data_demografie4$tweedehuis_nachten[data_demografie4$tweedehuis_nachten == "3 nachten per week"] <- "3"
data_demografie4$tweedehuis_nachten[data_demografie4$tweedehuis_nachten == "2 nachten per week"] <- "2"
data_demografie4$tweedehuis_nachten[data_demografie4$tweedehuis_nachten == "1 nacht per week"] <- "1"
data_demografie4$tweedehuis_nachten[data_demografie4$tweedehuis_nachten == "Minder dan 1 nacht per week"] <- "0"

data_demografie4$tweedehuis_ouders[data_demografie4$tweedehuis_ouders == "Ja, beide ouders"] <- "2"
data_demografie4$tweedehuis_ouders[data_demografie4$tweedehuis_ouders == "Ja, een van beide ouders"] <- "1"
data_demografie4$tweedehuis_ouders[data_demografie4$tweedehuis_ouders == "Nee, <u>geen</u> van beide ouders"] <- "0"

data_demografie4[data_demografie4 == "true"] <- "1"

names(data_demografie4)

cols <- c("geslacht", "schooljaar", "eenhuis", "eenhuis_ouders", "eenhuis_andere_kinderen", "eenhuis_andere_volwassenen", "eenhuis_stiefouder","eenhuis_broerzus",
          "eenhuis_niemand", "eenhuis_opaoma", "eerstehuis_nachten", "eerstehuis_ouders",
          "eerstehuis_andere_kinderen", "eerstehuis_stiefouder", "eerstehuis_halfbroerzus", "eerstehuis_niemand",
          "eerstehuis_stiefbroerzus", "tweedehuis", "tweedehuis_nachten", "tweedehuis_ouders",
          "tweedehuis_andere_kinderen", "tweedehuis_stiefouder", "tweedehuis_halfbroerzus", "tweedehuis_niemand",
          "tweedehuis_pleegouder", "tweedehuis_stiefbroerzus", "derdehuis", "niveau_nu",
          "niveau_start" , "niveau_advies" )

data_demografie4 <- data_demografie4 %>%
  mutate_each_(funs(factor(.)),cols)

data_demografie4 <- data_demografie4 %>%
  filter(!is.na(id))

### School
data_school <- data_school %>%
select(-c(response_id, filled_out_for_id, protocol_subscription_id, measurement_id, invitation_set_id,
          open_from, created_at, updated_at, locale))

# TR = Teacher relationships
# SC = School connectedness 
# AS = Academic support
# OD = Order and discipline
# ASF = Academic satisfaction

data_school2 <- data_school %>%
  mutate(TR = (v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9)/ 9,
         SC = (v10 + v11)/ 2,
         AS = (v12 + v13 + v14 + v15 + v16 + v17)/6,
         OD = (v18 + v19 + v20 + v21 + v22 + v23 + v24)/7,
         ASF = (v25 + v26)/2,
         AE_Stress = (v35 + v36 + v37 + v38 + v39 + v40 + v41 + v42)/ 8)

data_school3 <- data_school2 %>%
  select(-c(v1:v26, v35:v42))

data_school4 <- data_school3 %>%
  rename(id = filled_out_by_id,
         start_school = opened_at,
         complete_school = completed_at,
         schoolwerk_moeilijk = v27,
         cijfers = v28,
         gemist = v29,
         gemist_school_gesloten = v29_a_de_school_was_gesloten_bijvoorbeeld_vanwege_een_staking_of_een_studiedag_voor_leraren,
         gemist_weer = v29_a_er_was_zwaar_weer_bijvoorbeeld_een_storm,
         gemist_afspraak = v29_a_ik_had_een_afspraak_bijvoorbeeld_met_de_huisarts_of_een_specialist,
         gemist_anders = v29_a_ik_had_een_andere_reden,
         gemist_ouders_rust = v29_a_ik_had_vrij_gekregen_van_mijn_ouders_bijvoorbeeld_om_rust_te_krijgen,
         gemist_ouders_anders = v29_a_ik_mocht_om_andere_redenen_thuisblijven_van_mijn_ouders_bijvoorbeeld_om_thuis_te_helpen,
         gemist_moeilijk = v29_a_ik_vond_het_moeilijk_om_naar_school_te_gaan_of_daar_te_blijven_bijvoorbeeld_omdat_je_bang_was,
         gemist_spijbelen = v29_a_ik_was_aan_het_spijbelen,
         gemist_ziek = v29_a_ik_was_ziek_bijvoorbeeld_grieperig_of_lag_in_het_ziekenhuis,
         gemist_gezin = v29_a_ons_gezin_had_iets_dringends_bijvoorbeeld_een_begrafenis_of_iemand_moest_naar_het_ziekenhuis,
         extra_bijles = v30_ja_bijles,
         extra_extraining = v30_ja_examentraining,
         extra_hw = v30_ja_huiswerkbegeleiding,
         extra_anders = v30_ja_iets_anders,
         extra_nee = v30_nee,
         hobbies_tijd = v31,
         vorigjaar_leren = v32,
         moeite_school = v33, 
         extra_corona = v34)
         
data_school4[data_school4 == "true"] <- "1"  

data_school4$schoolwerk_moeilijk[data_school4$schoolwerk_moeilijk == "Heel makkelijk"] <- "0"
data_school4$schoolwerk_moeilijk[data_school4$schoolwerk_moeilijk == "Behoorlijk makkelijk"] <- "1"
data_school4$schoolwerk_moeilijk[data_school4$schoolwerk_moeilijk == "Een beetje makkelijk"] <- "2"
data_school4$schoolwerk_moeilijk[data_school4$schoolwerk_moeilijk == "Een beetje moeilijk"] <- "3"
data_school4$schoolwerk_moeilijk[data_school4$schoolwerk_moeilijk == "Behoorlijk moeilijk"] <- "4"
data_school4$schoolwerk_moeilijk[data_school4$schoolwerk_moeilijk == "Heel moeilijk"] <- "5"

data_school4$cijfers[data_school4$cijfers == "Ik had vorig jaar geen cijfers (bijvoorbeeld omdat je nog op de basisschool zat)"] <- "0"
data_school4$cijfers[data_school4$cijfers == "Vooral onvoldoendes"] <- "1"
data_school4$cijfers[data_school4$cijfers == "Vooral zessen"] <- "2"
data_school4$cijfers[data_school4$cijfers == "Vooral zevens"] <- "3"
data_school4$cijfers[data_school4$cijfers == "Vooral achten"] <- "4"
data_school4$cijfers[data_school4$cijfers == "Vooral negens"] <- "5"

data_school4$gemist[data_school4$gemist == "Ik heb geen lessen gemist"] <- "0"
data_school4$gemist[data_school4$gemist == "Ik heb een halve dag gemist"] <- "1"
data_school4$gemist[data_school4$gemist == "Ik heb 1 schooldag gemist"] <- "2"
data_school4$gemist[data_school4$gemist == "Ik heb 1.5 tot 2.5 dagen gemist"] <- "3"
data_school4$gemist[data_school4$gemist == "Ik heb 3 tot 7 dagen gemist"] <- "4"
data_school4$gemist[data_school4$gemist == "Ik heb 7.5 tot 9.5 dagen gemist"] <- "5"
data_school4$gemist[data_school4$gemist == "Ik heb alle lessen gemist"] <- "6"

data_school4$hobbies_tijd[data_school4$hobbies_tijd == "Nee"] <- "0"
data_school4$hobbies_tijd[data_school4$hobbies_tijd == "Ja, een beetje minder tijd"] <- "1"
data_school4$hobbies_tijd[data_school4$hobbies_tijd == "Ja, veel minder tijd"] <- "2"

data_school4$vorigjaar_leren[data_school4$vorigjaar_leren == "Veel minder"] <- "0"
data_school4$vorigjaar_leren[data_school4$vorigjaar_leren == "Iets minder"] <- "1"
data_school4$vorigjaar_leren[data_school4$vorigjaar_leren == "Ongeveer even veel"] <- "2"
data_school4$vorigjaar_leren[data_school4$vorigjaar_leren == "Iets meer"] <- "3"
data_school4$vorigjaar_leren[data_school4$vorigjaar_leren == "Veel meer"] <- "4"

data_school4$moeite_school[data_school4$moeite_school == "Nee, helemaal niet"] <- "0"
data_school4$moeite_school[data_school4$moeite_school == "Nee, niet echt"] <- "1"
data_school4$moeite_school[data_school4$moeite_school == "Ja, een beetje"] <- "2"
data_school4$moeite_school[data_school4$moeite_school == "Ja, heel erg"] <- "3"

data_school4$extra_corona[data_school4$extra_corona == "Nee"] <- "0"
data_school4$extra_corona[data_school4$extra_corona == "Ja, voor een deel"] <- "1"
data_school4$extra_corona[data_school4$extra_corona == "Ja, helemaal"] <- "2"


data_school4 <- data_school4 %>%
  filter(!is.na(id))
  
data_new <- left_join(data_demografie4, data_school4, by ="id")  

### well-being

data_welbevinden <- data_wellbeing %>%
  select(-c(response_id, filled_out_for_id, protocol_subscription_id, measurement_id, invitation_set_id,
            open_from, created_at, updated_at, locale))


data_welbevinden2 <- data_welbevinden %>%
  mutate(v4r = 100 - v4,
         v5r = 100 - v5)

data_welbevinden3 <- data_welbevinden2  %>%
  mutate_at(vars(12:36), ~replace(.,.== "Nooit", "0" ))

data_welbevinden3 <- data_welbevinden3  %>%
  mutate_at(vars(12:36), ~replace(.,.== "Soms", "1" ))

data_welbevinden3 <- data_welbevinden3  %>%
  mutate_at(vars(12:36), ~replace(.,.== "Vaak", "2" ))

data_welbevinden3 <- data_welbevinden3  %>%
  mutate_at(vars(12:36), ~replace(.,.== "Altijd", "3" ))

data_welbevinden3[12:36] <- lapply(data_welbevinden3[12:36], as.numeric)

data_welbevinden3[data_welbevinden3 == "true"] <- "1"  

data_welbevinden3 <- data_welbevinden3  %>%
  rename(id = filled_out_by_id,
         start_welb = opened_at,
         complete_welb = completed_at,
         happy = v1,
         f_bloed = v35_bloed_of_een_wond_zien,
         f_prik = v35_een_prik_of_vaccinatie_krijgen,
         f_geen = v35_geen_van_deze_dingen,
         f_donker = v35_het_donker,
         f_hoogtes = v35_hoogtes_zoals_een_dak_een_balkon_een_brug_of_een_trap,
         f_zhuis = v35_in_een_ziekenhuis_zijn,
         f_dier = v35_insecten_slangen_honden_of_andere_dieren,
         f_klruimte = v35_kleine_ruimtes_zoals_grotten_tunnels_kasten_of_liften,
         f_dokter = v35_naar_de_tandarts_of_dokter_gaan,
         f_water = v35_stilstaand_water_bijvoorbeeld_een_zwembad_of_meer,
         f_vliegen = v35_vliegen_of_vliegtuigen,
         f_weer = v35_zwaar_weer_bijvoorbeeld_stormen_onweer_of_bliksem,
         f_dier_bang = v35_a,
         f_dier_vermijd = v35_b,
         f_water_bang = v36_a,
         f_water_vermijd = v36_b,
         f_dokter_bang = v37_a,
         f_dokter_vermijd = v37_b,
         f_klruimte_bang = v38_a,
         f_klruimte_vermijd = v38_b,
         f_hoogtes_bang = v39_a,
         f_hoogtes_vermijd = v39_b,
         f_vliegen_bang = v40_a,
         f_vliegen_vermijd = v40_b,
         f_donker_bang = v41_a,
         f_donker_vermijd = v41_b,
         f_beperking = v42_a,
         f_vanstreek = v42_b,
         f_leeftijd_weet = v42_c,
         f_leeftijd_exact = v42_d,
         f_leeftijd_ong = v42_e,
         f_laatstekeer = v43)

data_welbevinden3 <- data_welbevinden3 %>%
  mutate(lifesat = (v2 + v3 + v4r + v5r + v6 + v7 + v8)/7,
         rcads_anx = v10 + v11 + v14 + v15 + v17 + v19 + v20 + v22 + v25 + v26 + v28 + v30 + v31 + v33,
         rcads_dep = v9 + v12 + v16 + v18 + v21 + v23 + v24 + v27 + v29 + v32,
         distress_precorona = (v34_a + v34_b + v34_c + v34_d + v34_e + v34_f)/6,
         distress_nu = (v34_g + v34_h + v34_i + v34_j + v34_k + v34_l)/6)
  
data_welbevinden4 <- data_welbevinden3 %>%
  select(-c(5:48))

data_welbevinden4 <- data_welbevinden4 %>%
  filter(!is.na(id))

data_new2 <- left_join(data_new, data_welbevinden4, by = "id")

#### social networks

data_social <- data_socialnetworks %>%
  select(-c(response_id, filled_out_for_id, protocol_subscription_id, measurement_id, invitation_set_id,
            open_from, created_at, updated_at, locale))

data_social2 <- data_social %>%
  mutate(support_so = (v3_a + v3_b + v3_c + v3_d)/4,
         support_fam = (v4 + v5 + v8 + v10)/4,
         support_friends = (v6 + v7 + v9 + v11)/4,
         support_class = (v12 + v13 + v14 + v15)/4)

data_social3 <- data_social2 %>%
  select(-c(7:22))

data_social4 <- data_social3 %>%
  rename(id = filled_out_by_id,
         start_social = opened_at,
         complete_social = completed_at,
         vrienden_klas = v1_a,
         vrienden_school = v1_b,
         verkering = v2,
         pest_freq = v16,
         pest_erg = v17,
         pest_sterker = v18_a,
         pest_pop = v18_b,
         pest_expres = v19,
         pest_cool = v20_a,
         pest_wraak = v20_b,
         pest_start = v21,
         pest_vert = v22,
         pest_vert_volw = v22_b_een_andere_volwassene_op_school_bijvoorbeeld_de_jeugdverpleegkundige,
         pest_vert_vriend = v22_b_een_vriend_of_vriendin,
         pest_vert_anders = v22_b_iemand_anders,
         pest_vert_mentor = v22_b_mijn_mentor,
         pest_vert_ouder = v22_b_mijn_ouder_s_of_verzorger_s,
         pest_thuis = v23,
         pest_zelf = v24,
         pest_mentor_gepraat = v25,
         pest_stop_volw = v26,
         pest_stop_leerl = v27,
         pest_mentor_gedaan = v28)
         

data_social4$verkering[data_social4$verkering == "Ja"] <- "1"
data_social4$verkering[data_social4$verkering == "Nee"] <- "0"

data_social4$pest_freq[data_social4$pest_freq == "Ik ben niet gepest"] <- "0"
data_social4$pest_freq[data_social4$pest_freq == "Ik ben één of twee keer gepest"] <- "1"
data_social4$pest_freq[data_social4$pest_freq == "Ik ben twee of drie keer per maand gepest"] <- "2"
data_social4$pest_freq[data_social4$pest_freq == "Ik ben ongeveer één keer per week gepest"] <- "3"
data_social4$pest_freq[data_social4$pest_freq == "Ik ben meerdere keren per week gepest"] <- "4"

data_social4$pest_erg[data_social4$pest_erg == "Helemaal niet erg"] <- "0"
data_social4$pest_erg[data_social4$pest_erg == "Niet zo erg"] <- "1"
data_social4$pest_erg[data_social4$pest_erg == "Een beetje erg"] <- "2"
data_social4$pest_erg[data_social4$pest_erg == "Best erg"] <- "3"
data_social4$pest_erg[data_social4$pest_erg == "Heel erg"] <- "4"

data_social4$pest_sterker[data_social4$pest_sterker == "Ik ben veel sterker"] <- "0"
data_social4$pest_sterker[data_social4$pest_sterker == "Ik ben iets sterker"] <- "1"
data_social4$pest_sterker[data_social4$pest_sterker == "We zijn allebei even sterk"] <- "2"
data_social4$pest_sterker[data_social4$pest_sterker == "Degene die me gepest heeft is iets sterker"] <- "3"
data_social4$pest_sterker[data_social4$pest_sterker == "Degene die me gepest heeft is veel sterker"] <- "4"

data_social4$pest_pop[data_social4$pest_pop == "Ik ben veel populairder"] <- "0"
data_social4$pest_pop[data_social4$pest_pop == "Ik ben iets populairder"] <- "1"
data_social4$pest_pop[data_social4$pest_pop == "We zijn allebei even populair"] <- "2"
data_social4$pest_pop[data_social4$pest_pop == "Degene die me gepest heeft is iets populairder"] <- "3"
data_social4$pest_pop[data_social4$pest_pop == "Degene die me gepest heeft is veel populairder"] <- "4"

data_social4$pest_expres[data_social4$pest_expres == "Helemaal niet zeker"] <- "0"
data_social4$pest_expres[data_social4$pest_expres == "Niet zo zeker"] <- "1"
data_social4$pest_expres[data_social4$pest_expres == "Een beetje zeker"] <- "2"
data_social4$pest_expres[data_social4$pest_expres == "Zeker"] <- "3"
data_social4$pest_expres[data_social4$pest_expres == "Heel zeker"] <- "4"

data_social4$pest_cool[data_social4$pest_cool == "Helemaal niet waar"] <- "0"
data_social4$pest_cool[data_social4$pest_cool == "Niet waar"] <- "1"
data_social4$pest_cool[data_social4$pest_cool == "Een beetje waar"] <- "2"
data_social4$pest_cool[data_social4$pest_cool == "Waar"] <- "3"
data_social4$pest_cool[data_social4$pest_cool == "Helemaal waar"] <- "4"

data_social4$pest_wraak[data_social4$pest_wraak == "Helemaal niet waar"] <- "0"
data_social4$pest_wraak[data_social4$pest_wraak == "Niet waar"] <- "1"
data_social4$pest_wraak[data_social4$pest_wraak == "Een beetje waar"] <- "2"
data_social4$pest_wraak[data_social4$pest_wraak == "Waar"] <- "3"
data_social4$pest_wraak[data_social4$pest_wraak == "Helemaal waar"] <- "4"

data_social4$pest_start[data_social4$pest_start == "Eén of twee weken geleden"] <- "0"
data_social4$pest_start[data_social4$pest_start == "Ongeveer een maand geleden"] <- "1"
data_social4$pest_start[data_social4$pest_start == "Ongeveer een half jaar geleden"] <- "2"
data_social4$pest_start[data_social4$pest_start == "Ongeveer een jaar geleden"] <- "3"
data_social4$pest_start[data_social4$pest_start == "Meer dan een jaar geleden"] <- "4"

data_social4$pest_vert[data_social4$pest_vert == "Nee"] <- "0"
data_social4$pest_vert[data_social4$pest_vert == "Ja"] <- "1"

data_social4$pest_thuis[data_social4$pest_thuis == "Nee"] <- "0"
data_social4$pest_thuis[data_social4$pest_thuis == "Ja"] <- "1"

data_social4[data_social4 == "true"] <- "1"

data_social4$pest_zelf[data_social4$pest_zelf == "Ik heb niemand gepest"] <- "0"
data_social4$pest_zelf[data_social4$pest_zelf == "Eén of twee keer"] <- "1"
data_social4$pest_zelf[data_social4$pest_zelf == "Ongeveer twee of drie keer per maand"] <- "2"
data_social4$pest_zelf[data_social4$pest_zelf == "Ongeveer één keer per week"] <- "3"
data_social4$pest_zelf[data_social4$pest_zelf == "Een paar keer per week"] <- "4"

data_social4$pest_mentor_gepraat[data_social4$pest_mentor_gepraat == "Nee, mijn leraren hebben daar niet met mij over gepraat"] <- "0"
data_social4$pest_mentor_gepraat[data_social4$pest_mentor_gepraat == "Ja, mijn leraren hebben daar één keer met mij over gepraat"] <- "1"
data_social4$pest_mentor_gepraat[data_social4$pest_mentor_gepraat == "Ja, mijn leraren hebben daar meerdere keren met mij over gepraat"] <- "2"

data_social4$pest_stop_volw[data_social4$pest_stop_volw == "Bijna nooit"] <- "0"
data_social4$pest_stop_volw[data_social4$pest_stop_volw == "Zo nu en dan"] <- "1"
data_social4$pest_stop_volw[data_social4$pest_stop_volw == "Soms"] <- "2"
data_social4$pest_stop_volw[data_social4$pest_stop_volw == "Meestal"] <- "3"
data_social4$pest_stop_volw[data_social4$pest_stop_volw == "Bijna altijd"] <- "4"

data_social4$pest_stop_leerl[data_social4$pest_stop_leerl == "Bijna nooit"] <- "0"
data_social4$pest_stop_leerl[data_social4$pest_stop_leerl == "Zo nu en dan"] <- "1"
data_social4$pest_stop_leerl[data_social4$pest_stop_leerl == "Soms"] <- "2"
data_social4$pest_stop_leerl[data_social4$pest_stop_leerl == "Meestal"] <- "3"
data_social4$pest_stop_leerl[data_social4$pest_stop_leerl == "Bijna altijd"] <- "4"

data_social4$pest_mentor_gedaan[data_social4$pest_mentor_gedaan == "Niets of zo goed als niets"] <- "0"
data_social4$pest_mentor_gedaan[data_social4$pest_mentor_gedaan == "Heel weinig"] <- "1"
data_social4$pest_mentor_gedaan[data_social4$pest_mentor_gedaan == "Een beetje"] <- "2"
data_social4$pest_mentor_gedaan[data_social4$pest_mentor_gedaan == "Behoorlijk veel"] <- "3"
data_social4$pest_mentor_gedaan[data_social4$pest_mentor_gedaan == "Heel veel"] <- "4"

data_social4[6:26] <- lapply(data_social4[6:26], as.numeric)

data_social4 <- data_social4 %>%
  filter(!is.na(id))

data_new3 <- left_join(data_new2, data_social4, by = "id")

#### Other factors

data_other <- data_other_factors %>%
  select(-c(response_id, filled_out_for_id, protocol_subscription_id, measurement_id, invitation_set_id,
            open_from, created_at, updated_at, locale))

data_other2 <- data_other  %>%
  mutate_at(vars(13:32), ~replace(.,.== "Nee", "0" ))

data_other2 <- data_other2  %>%
  mutate_at(vars(13:32), ~replace(.,.== "Ja", "1" ))

data_other2 <- data_other2  %>%
  mutate_at(vars(55:59), ~replace(.,.== "Nooit of bijna nooit", "0" ))
data_other2 <- data_other2  %>%
  mutate_at(vars(55:59), ~replace(.,.== "Zo nu en dan", "1" ))
data_other2 <- data_other2  %>%
  mutate_at(vars(55:59), ~replace(.,.== "Soms", "2" ))
data_other2 <- data_other2  %>%
  mutate_at(vars(55:59), ~replace(.,.== "Vaak", "3" ))
data_other2 <- data_other2  %>%
  mutate_at(vars(55:59), ~replace(.,.== "Heel vaak", "4" ))

data_other2[13:32] <- lapply(data_other2[13:32], as.numeric)
data_other2[55:59] <- lapply(data_other2[55:59], as.numeric)

data_other3 <- data_other2 %>%
  mutate(geloof_wb = (v5_a + v5_b + v5_c + v5_d + v5_e)/5,
         life_events = (v6 + v7 + v8 + v9 + v10 + v11 + v12 + v13 + v14 +
                          v15 + v16 + v17 + v18 + v19 + v20 + v21 + v22 +
                          v23 + v24 + v25),
         media_dis = (v28_a + v28_b + v28_c)/3)
  
data_other4 <- data_other3 %>%
  rename(id = filled_out_by_id,
         start_other = opened_at,
         complete_other = completed_at,
         geloof = v1,
         geloof_gezin = v2,
         geloof_kerk = v3,
         geloof_school = v4,
         life_v6 = v6,
         life_v7= v7,
         life_v8 = v8,
         life_v9 = v9,
         life_v10 = v10,
         life_v11 = v11,
         life_v12 = v12,
         life_v13 = v13,
         life_v14 = v14,
         life_v15 = v15,
         life_v16 = v16,
         life_v17 = v17,
         life_v18 = v18,
         life_v19 = v19,
         life_v20 = v20,
         life_v21 = v21,
         life_v22 = v22,
         life_v23 = v23,
         life_v24 = v24,
         life_v25 = v25,
         media_soms_fb = v25_a_facebook,
         media_soms_anders = v25_a_iets_anders,
         media_soms_inst = v25_a_instagram,
         media_soms_pint = v25_a_pinterest,
         media_soms_snap = v25_a_snapchat,
         media_soms_tik = v25_a_tiktok,
         media_soms_twitch = v25_a_twitch,
         media_soms_twitt = v25_a_twitter,
         media_soms_wa = v25_a_whatsapp,
         media_soms_yt = v25_a_youtube,
         media_dag_fb = v25_b_facebook,
         media_dag_anders = v25_b_iets_anders,
         media_dag_inst = v25_b_instagram,
         media_dag_pint = v25_b_pinterest,
         media_dag_snap = v25_b_snapchat,
         media_dag_tik = v25_b_tiktok,
         media_dag_twitch = v25_b_twitch,
         media_dag_twitt = v25_b_twitter,
         media_dag_wa = v25_b_whatsapp,
         media_dag_yt = v25_b_youtube,
         media_26_a = v26_a,
         media_26_b = v26_b,
         media_27_a = v27_a,
         media_27_b = v27_b,
         media_28_a = v28_a,
         media_28_b = v28_b,
         media_28_c = v28_c)

data_other4 <- data_other4 %>%
  filter(!is.na(id))

data_new4 <- left_join(data_new3, data_other4, by = "id")

data_people <- data_people %>%
  select(person_id, team_name)

data_people <- data_people %>%
  rename(id = person_id,
         school = team_name)

data_people$school <- as.factor(data_people$school)

levels(data_people$school)

data_ucanfeel <- data_new4

save(data_ucanfeel, file = "data_ucanfeel.Rdata")

### Informed consent











