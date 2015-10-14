


phoneNumberCleaner <- function(){
library(stringr)
library(dplyr)
library(sqldf)

#clean-up the environment
rm(list = ls())


#this I will change to read.csv(file.choose())
loaded_data <- read.csv(file.choose(), stringsAsFactors = F, header = T)


strings <- as.character(loaded_data$Supplier.contact.Phone)
strings <- tolower(strings)

strings <-
 str_replace_all(string = strings, pattern = "[0-9]x[0-9]",replacement = "ext")
strings <-
 str_replace_all(string = strings, pattern = "[t]:[0-9]",replacement = "ext")
strings <-
 str_replace_all(string = strings, pattern = " x ",replacement = "ext")
strings <- str_replace_all(strings, fixed("ext"), " extension ")


#Non-Numeric Characters to be removed
replacement_values <-
 c(
 "-","/","(",")",";",".","\n","|",",,","â€","[[:punct:]]",":","+",",", "[[:space:]]", "\\s", "[:blank:]"
 )


#loop over the values to replace non-numeric characters
for (i in replacement_values) {
 strings <- str_replace_all(strings, fixed(i), " ")
}

#remove white-space
strings <- str_replace_all(strings, fixed(" "), "")


#check for text inside the strings
text <- grep("[A-z]", strings)


#change class to data frame for joining and merging
strings <- as.data.frame(strings)
colnames(strings) <- c("strings")


#reads the country code from the original file (if available)
strings$Unique.Identifier <- loaded_data$Unique.Identifier
strings$provided_country_code <- loaded_data$Country.Code
strings$Raw_Data <- loaded_data$Supplier.contact.Phone
strings$provided_country_code <- str_replace_all(strings$provided_country_code, fixed(" "), "")


#did this to check for values that are are too long or too short
string_len <- nchar(x = as.character(strings$strings), allowNA = T)

strings <- cbind(string_len, strings)

strings$Error_Value <- NA

strings[which(string_len < 9),"Error_Value"] <- "Not Enough Characters"
strings[which(string_len > 14),"Error_Value"] <-
 "Too Many Characters (may contain more than one number)"
strings[grep("ext", strings$strings),"Error_Value"] <- "Contains an Extension"

strings$sub_3 <- substr(strings$strings, 1,3)
strings$sub_2 <- substr(strings$strings, 1,2)

#some strings contain leading international 011 need to remove to check for country codes
zero_one_one <- which(strings$sub_3 == "011")

zero_one_one_replace <-
 substr(strings[zero_one_one,"strings"],start = 3, stop = nchar(as.character(strings[zero_one_one,3])))
strings$strings <- as.character(strings$strings)
strings[zero_one_one,"strings"] <- zero_one_one_replace

#get left 3 digits to comare with N American codes where len of string = 10
strings$sub_3 <- substr(strings$strings, 1,3)




# Create Reference data for looking up countries and languages spoken in that country

Countries <-
 c(
 'United States','Canada', 'Russia', 'Kazakhstan', 'Egypt', 'South Africa', 'Greece', 'Netherlands',
 'Belgium', 'France', 'Spain', 'Hungary', 'Italy', 'Romania', 'Switzerland', 'Austria', 'United Kingdom', 'Denmark',
 'Sweden', 'Norway', 'Poland', 'Germany', 'Peru', 'Mexico', 'Cuba', 'Argentina', 'Brazil', 'Chile', 'Colombia', 'Venezuela',
 'Malaysia', 'Australia', 'Indonesia', 'Philippines', 'New Zealand or Pitcairn', 'Singapore', 'Thailand', 'Japan', 'South Korea', 'Vietnam', 'China', 'Turkey',
 'India', 'Pakistan', 'Afghanistan', 'Sri Lanka', 'Myanmar', 'Iran', 'South Sudan', 'Western Sahara or Morocco', 'Algeria', 'Tunisia', 'Libya',
 'Gambia', 'Senegal', 'Mauritania', 'Mali', 'Guinea', 'Ivory Coast', 'Burkina Faso', 'Niger', 'Togo', 'Benin', 'Mauritius', 'Liberia', 'Sierra Leone',
 'Ghana', 'Nigeria', 'Chad', 'Central African Republic', 'Cameroon', 'Cape Verde', 'Sao Tome and Principe', 'Equatorial Guinea', 'Gabon', 'Republic of the Congo or Bahamas', 'Democratic Republic of the Congo',
 'Angola', 'Guinea-Bissau', 'British Indian Ocean Territory or Barbados', 'Seychelles', 'Sudan', 'Rwanda', 'Ethiopia', 'Somalia', 'Djibouti', 'Kenya',
 'Tanzania', 'Uganda', 'Burundi', 'Mozambique', 'Zambia', 'Madagascar', 'Reunion or Mayotte', 'Zimbabwe', 'Namibia', 'Malawi', 'Lesotho', 'Botswana', 'Antigua and Barbuda or Swaziland',
 'Comoros', 'Saint Helena', 'Eritrea', 'Aruba', 'Faroe Islands', 'Greenland', 'Gibraltar', 'Portugal', 'Luxembourg', 'Ireland', 'Iceland', 'Albania', 'Malta', 'Cyprus',
 'Finland', 'Bulgaria', 'Lithuania', 'Latvia', 'Estonia', 'Moldova', 'Armenia', 'Belarus', 'Andorra', 'Monaco', 'San Marino', 'Vatican', 'Ukraine', 'Serbia', 'Montenegro',
 'Kosovo', 'Croatia', 'Slovenia', 'Bosnia and Herzegovina', 'Macedonia', 'Czech Republic', 'Slovakia', 'Liechtenstein', 'Falkland Islands', 'Belize', 'Guatemala', 'El Salvador', 'Honduras',
 'Nicaragua', 'Costa Rica', 'Panama', 'Saint Pierre and Miquelon', 'Haiti', 'Saint Barthelemy or Saint Martin', 'Bolivia', 'Guyana', 'Ecuador', 'Paraguay', 'Suriname', 'Uruguay', 'Netherlands Antilles or Curacao',
 'East Timor or N. Mariana Islands', 'Antarctica', 'Brunei', 'Nauru', 'Papua New Guinea', 'Tonga', 'Solomon Islands', 'Vanuatu', 'Fiji', 'Palau', 'Wallis and Futuna', 'Cook Islands', 'Niue', 'Samoa',
 'Kiribati', 'New Caledonia', 'Tuvalu', 'French Polynesia', 'Tokelau', 'Micronesia', 'Marshall Islands', 'North Korea', 'Hong Kong', 'Macao', 'Cambodia', 'Laos', 'Bangladesh', 'Taiwan', 'Maldives',
 'Lebanon', 'Jordan', 'Syria', 'Iraq', 'Kuwait', 'Saudi Arabia', 'Yemen', 'Oman', 'Palestine', 'United Arab Emirates', 'Israel', 'Bahrain',
 'Qatar', 'Bhutan', 'Mongolia', 'Nepal', 'Tajikistan', 'Turkmenistan', 'Azerbaijan', 'Georgia', 'Kyrgyzstan', 'Uzbekistan', 'Anguilla', 'British Virgin Islands', 'U.S. Virgin Islands', 'Cayman Islands',
 'Bermuda', 'Grenada', 'Turks and Caicos Islands', 'Montserrat', 'Guam', 'American Samoa', 'Sint Maarten', 'Saint Lucia', 'Dominica', 'Saint Vincent and the Grenadines', 'Puerto Rico', 'Puerto Rico', 'Dominican Republic', 'Dominican Republic', 'Dominican Republic',
 'Trinidad and Tobago', 'Saint Kitts and Nevis', 'Jamaica'
 )


code <- c(
 1,1, 7, 7,20, 27, 30, 31, 32, 33, 34, 36, 39, 40, 41, 3, 44,
 45, 46, 47, 48, 49, 51, 52, 53, 54, 55, 56, 57, 58, 60, 61, 62, 63, 64, 65, 66, 81, 82, 84, 86, 90, 91, 92, 93,
 94, 95, 98, 211, 212, 213, 216, 218, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240,
 241, 242, 243, 244, 245, 246, 248, 249, 250, 251, 252, 253, 254, 255, 256, 257, 258, 260, 261, 262, 263, 264, 265, 266, 267, 268, 269, 290,
 291, 297, 298, 299, 350, 351, 352, 353, 354, 355, 356, 357, 358, 359, 370, 371, 372, 373, 374, 375, 376, 377, 378, 379, 380, 381, 382, 383,
 385, 386, 387, 389, 420, 421, 423, 500, 501, 502, 503, 504, 505, 506, 507, 508, 509, 590, 591, 592, 593, 595, 597, 598, 599, 670, 672, 673,
 674, 675, 676, 677, 678, 679, 680, 681, 682, 683, 685, 686, 687, 688, 689,
 690, 691, 692, 850, 852, 853, 855, 856, 880, 886, 960, 961, 962, 963, 964, 965, 966, 967, 968, 970, 971, 972, 973, 974, 975, 976, 977, 992,
 993, 994, 995, 996, 998, 264, 284, 340, 345, 441, 473, 649, 664, 671, 684, 721, 758, 767, 784, 787, 939, 809, 829, 849, 868, 869, 876
)

languages <-
 c('English','English','Russian','Russian', 'Arabic (official), English and French widely understood by educated classes', 
 'IsiZulu (official) 22.7%, IsiXhosa (official) 16%, Afrikaans (official) 13.5%, English (official) 9.6%, Sepedi (official) 9.1%, Setswana (official) 8%, Sesotho (official) 7.6%, Xitsonga (official) 4.5%, siSwati (official) 2.5%, Tshivenda (official) 2.4%, isiNdebele (official) 2.1%, sign language 0.5%, other 1.6% (2011 est.)', 
 'Greek (official) 99%, other (includes English and French) 1%', 
 'Dutch (official)', 'Dutch (official) 60%, French (official) 40%, German (official) less than 1%', 
 'French (official) 100%, rapidly declining regional dialects and languages (Provencal, Breton, Alsatian, Corsican, Catalan, Basque, Flemish)', 
 'Castilian Spanish (official) 74%, Catalan 17%, Galician 7%, and Basque 2%', 
 'Hungarian (official) 99.6%, English 16%, German 11.2%, Russian 1.6%, Romanian 1.3%, French 1.2%, other 4.2%', 
 'Italian (official), German (parts of Trentino-Alto Adige region are predominantly German-speaking), French (small French-speaking minority in Valle dAosta region), Slovene (Slovene-speaking minority in the Trieste-Gorizia area)', 
 'Romanian (official) 85.4%, Hungarian 6.3%, Romany (Gypsy) 1.2%, other 1%, unspecified 6.1% (2011 est.)', 
 'German (official) 64.9%, French (official) 22.6%, Italian (official) 8.3%, Serbo-Croatian 2.5%, Albanian 2.6%, Portuguese 3.4%, Spanish 2.2%, English 4.6%, Romansch (official) 0.5%, other 5.1%', 
 'German (official nationwide) 88.6%, Turkish 2.3%, Serbian 2.2%, Croatian (official in Burgenland) 1.6%, other (includes Slovene, official in South Carinthia, and Hungarian, official in Burgenland) 5.3% (2001 est.)', 
 'English', 'Danish, Faroese, Greenlandic (an Inuit dialect), German (small minority)', 
 'Swedish (official), small Sami- and Finnish-speaking minorities', 
 'Bokmal Norwegian (official), Nynorsk Norwegian (official), small Sami- and Finnish-speaking minorities', 
 'Polish (official) 98.2%, Silesian 1.4%, other 1.1%, unspecified 1.3%', 
 'German (official)', 'Spanish (official) 84.1%, Quechua (official) 13%, Aymara (official) 1.7%, Ashaninka 0.3%, other native languages (includes a large number of minor Amazonian languages) 0.7%, other (includes foreign languages and sign language) 0.2% (2007 est.)', 
 'Spanish only 92.7%, Spanish and indigenous languages 5.7%, indigenous only 0.8%, unspecified 0.8%', 
 'Spanish (official)', 'Spanish (official), Italian, English, German, French, indigenous (Mapudungun, Quechua)', 
 'Portuguese (official and most widely spoken language)', 
 'Spanish 99.5% (official), English 10.2%, indigenous 1% (includes Mapudungun, Aymara, Quechua, Rapa Nui), other 2.3%, unspecified 0.2%', 
 'Spanish (official)', 'Spanish (official), numerous indigenous dialects', 
 'Bahasa Malaysia (official), English, Chinese (Cantonese, Mandarin, Hokkien, Hakka, Hainan, Foochow), Tamil, Telugu, Malayalam, Panjabi, Thai', 
 'English 76.8%, Mandarin 1.6%, Italian 1.4%, Arabic 1.3%, Greek 1.2%, Cantonese 1.2%, Vietnamese 1.1%, other 10.4%, unspecified 5% (2011 est.)', 
 'Bahasa Indonesia (official, modified form of Malay), English, Dutch, local dialects (of which the most widely spoken is Javanese)', 
 'Filipino (official; based on Tagalog) and English (official); eight major dialects - Tagalog, Cebuano, Ilocano, Hiligaynon or Ilonggo, Bicol, Waray, Pampango, and Pangasinan', 
 'English', 'Mandarin (official) 36.3%, English (official) 29.8%, Malay (official) 11.9%, Hokkien 8.1%, Tamil (official) 4.4%, Cantonese 4.1%, Teochew 3.2%, other Indian languages 1.2%, other Chinese dialects 1.1%, other 1.1% (2010 est.)', 
 'Thai (official) 90.7%, Burmese 1.3%, other 8%', 
 'Japanese', 'Korean', 'Vietnamese (official), English (increasingly favored as a second language), some French, Chinese, and Khmer, mountain area languages (Mon-Khmer and Malayo-Polynesian)', 
 'Standard Chinese or Mandarin (official; Putonghua, based on the Beijing dialect), Yue (Cantonese), Wu (Shanghainese), Minbei (Fuzhou), Minnan (Hokkien-Taiwanese), Xiang, Gan, Hakka dialects, minority languages (see Ethnic groups entry)', 
 'Turkish (official), Kurdish, other minority languages', 
 'Hindi 41%, Bengali 8.1%, Telugu 7.2%, Marathi 7%, Tamil 5.9%, Urdu 5%, Gujarati 4.5%, Kannada 3.7%, Malayalam 3.2%, Oriya 3.2%, Punjabi 2.8%, Assamese 1.3%, Maithili 1.2%, other 5.9%', 
 'Punjabi 48%, Sindhi 12%, Saraiki (a Punjabi variant) 10%, Pashto (alternate name, Pashtu) 8%, Urdu (official) 8%, Balochi 3%, Hindko 2%, Brahui 1%, English (official; lingua franca of Pakistani elite and most government ministries), Burushaski, and other 8%', 
 'Afghan Persian or Dari (official) 50%, Pashto (official) 35%, Turkic languages (primarily Uzbek and Turkmen) 11%, 30 minor languages (primarily Balochi and Pashai) 4%, much bilingualism, but Dari functions as the lingua franca', 
 'Sinhala (official and national language) 74%, Tamil (national language) 18%, other 8%', 
 '', 'Persian (official), Azeri Turkic and Turkic dialects, Kurdish, Gilaki and Mazandarani, Luri, Balochi, Arabic, other', 
 'English (official), Arabic (includes Juba and Sudanese variants), regional languages include Dinka, Nuer, Bari, Zande, Shilluk', 
 '', 'Arabic (official), French (lingua franca), Berber dialects: Kabylie Berber (Tamazight), Chaouia Berber (Tachawit), Mzab Berber, Tuareg Berber (Tamahaq)', 
 'Arabic (official, one of the languages of commerce), French (commerce), Berber (Tamazight)', 
 'Arabic (official), Italian, English (all widely understood in the major cities); Berber (Nafusi, Ghadamis, Suknah, Awjilah, Tamasheq)', 
 '', 'French (official), Wolof, Pulaar, Jola, Mandinka', 'Arabic (official and national), Pulaar, Soninke, Wolof (all national languages), French, Hassaniya (a variety of Arabic)', 
 'French (official), Bambara 46.3%, Peul/foulfoulbe 9.4%, Dogon 7.2%, Maraka/soninke 6.4%, Malinke 5.6%, Sonrhai/djerma 5.6%, Minianka 4.3%, Tamacheq 3.5%, Senoufo 2.6%, unspecified 0.6%, other 8.5%', 
 'French (official)', 'French', 'French (official), native African languages belonging to Sudanic family spoken by 90% of the population', 
 'French (official), Hausa, Djerma', 'French (official, the language of commerce), Ewe and Mina (the two major African languages in the south), Kabye (sometimes spelled Kabiye) and Dagomba (the two major African languages in the north)', 
 'French (official), Fon and Yoruba (most common vernaculars in south), tribal languages (at least six major ones in north)', 
 'Creole 86.5%, Bhojpuri 5.3%, French 4.1%, two languages 1.4%, other 2.6% (includes English, the official language, which is spoken by less than 1% of the population), unspecified 0.1% (2011 est.)', 
'English 20% (official), some 20 ethnic group languages few of which can be written or used in correspondence', 
'English (official, regular use limited to literate minority), Mende (principal vernacular in the south), Temne (principal vernacular in the north), Krio (English-based Creole, spoken by the descendants of freed Jamaican slaves who were settled in the Freetown area, a lingua franca and a first language for 10% of the population but understood by 95%)', 
'Asante 16%, Ewe 14%, Fante 11.6%, Boron (Brong) 4.9%, Dagomba 4.4%, Dangme 4.2%, Dagarte (Dagaba) 3.9%, Kokomba 3.5%, Akyem 3.2%, Ga 3.1%, other 31.2%', 
'English (official), Hausa, Yoruba, Igbo (Ibo), Fulani, over 500 additional indigenous languages', 
'French (official), Arabic (official), Sara (in south), more than 120 different languages and dialects', 
'French (official), Sangho (lingua franca and national language), tribal languages', 
'24 major African language groups, English (official), French (official)', 
'', 'Portuguese 98.4% (official), Forro 36.2%, Cabo Verdian 8.5%, French 6.8%, Angolar 6.6%, English 4.9%, Lunguie 1%, other (including sign language) 2.4%', 
'Spanish (official) 67.6%, other (includes French (official), Fang, Bubi) 32.4% (1994 census)', 
'French (official), Fang, Myene, Nzebi, Bapounou/Eschira, Bandjabi', 
'', 'Kikongo, Lingala', 'Portuguese (official), Bantu and other African languages', 
'Crioulo 90.4%, Portuguese 27.1% (official), French 5.1%, English 2.9%, other 2.4%', 
'','Seychellois Creole (official) 89.1%, English (official) 5.1%, French (official) 0.7%, other 3.8%, unspecified 1.4% (2010 est.)', 
'Arabic (official), English (official), Nubian, Ta Bedawie, Fur', 
'Kinyarwanda only (official, universal Bantu vernacular) 93.2%, Kinyarwanda and other language(s) 6.2%, French (official) and other language(s) 0.1%, English (official) and other language(s) 0.1%, Swahili (or Kiswahili, used in commercial centers) 0.02%, other 0.03%, unspecified 0.3% (2002 est.)', 
'Oromo (official working language in the State of Oromiya) 33.8%, Amharic (official national language) 29.3%, Somali (official working language of the State of Sumale) 6.2%, Tigrigna (Tigrinya) (official working language of the State of Tigray) 5.9%, Sidamo 4%, Wolaytta 2.2%, Gurage 2%, Afar (official working language of the State of Afar) 1.7%, Hadiyya 1.7%, Gamo 1.5%, Gedeo 1.3%, Opuuo 1.2%, Kafa 1.1%, other 8.1%, English (major foreign language taught in schools), Arabic (2007 est.)', 
'Somali (official), Arabic (official, according to the Transitional Federal Charter), Italian, English', 
'French (official), Arabic (official), Somali, Afar', 
'English (official), Kiswahili (official), numerous indigenous languages', 
'Kiswahili or Swahili (official), Kiunguja (name for Swahili in Zanzibar), English (official, primary language of commerce, administration, and higher education), Arabic (widely spoken in Zanzibar), many local languages', 
'English (official national language, taught in grade schools, used in courts of law and by most newspapers and some radio broadcasts), Ganda or Luganda (most widely used of the Niger-Congo languages, preferred for native language publications in the capital and may be taught in school), other Niger-Congo languages, Nilo-Saharan languages, Swahili, Arabic', 
'Kirundi 29.7% (official), Kirundi and other language 9.1%, French (official) and French and other language 0.3%, Swahili and Swahili and other language 0.2% (along Lake Tanganyika and in the Bujumbura area), English and English and other language 0.06%, more than 2 languages 3.7%, unspecified 56.9% (2008 est.)', 
'Emakhuwa 25.3%, Portuguese (official) 10.7%, Xichangana 10.3%, Cisena 7.5%, Elomwe 7%, Echuwabo 5.1%, other Mozambican languages 30.1%, other 4% (1997 census)', 
'Bembe 33.4%, Nyanja 14.7%, Tonga 11.4%, Lozi 5.5%, Chewa 4.5%, Nsenga 2.9%, Tumbuka 2.5%, Lunda (North Western) 1.9%, Kaonde 1.8%, Lala 1.8%, Lamba 1.8%, English (official) 1.7%, Luvale 1.5%, Mambwe 1.3%, Namwanga 1.2%, Lenje 1.1%, Bisa 1%, other 9.2%, unspecified 0.4%', 
'French (official), Malagasy (official), English', 
'', 'Shona (official; most widely spoken), Ndebele (official, second most widely spoken), English (official; traditionally used for official business), 13 minority languages (official; includes Chewa, Chibarwe, Kalanga, Koisan, Nambya, Ndau, Shangani, sign language, Sotho, Tonga, Tswana, Venda, and Xhosa)', 
'Oshiwambo languages 48.9%, Nama/Damara 11.3%, Afrikaans 10.4% (common language of most of the population and about 60% of the white population), Otjiherero languages 8.6%, Kavango languages 8.5%, Caprivi languages 4.8%, English (official) 3.4%, other African languages 2.3%, other 1.7%', 
'English (official), Chichewa (common), Chinyanja, Chiyao, Chitumbuka, Chilomwe, Chinkhonde, Chingoni, Chisena, Chitonga, Chinyakyusa, Chilambya', 
'Sesotho (official) (southern Sotho), English (official), Zulu, Xhosa', 
'Setswana 78.2%, Kalanga 7.9%, Sekgalagadi 2.8%, English (official) 2.1%, Sesarwa 1.9%, Sempukushu 1.7%, other 5.1%, unspecified 0.2% (2001 est.)', 
'', 'Arabic (official), French (official), Shikomoro (official; a blend of Swahili and Arabic) (Comorian)', 
'', 'Tigrinya (official), Arabic (official), English (official), Tigre, Kunama, Afar, other Cushitic languages', 
'Papiamento (a Spanish-Portuguese-Dutch-English dialect) 69.4%, Spanish 13.7%, English (widely spoken) 7.1%, Dutch (official) 6.1%, Chinese 1.5%, other 1.7%, unspecified 0.4% (2010 est.)', 
'Faroese (derived from Old Norse), Danish', 'Greenlandic (East Inuit) (official), Danish (official), English', 
'English (used in schools and for official purposes), Spanish, Italian, Portuguese', 
'Portuguese (official), Mirandese (official, but locally used)', 
'Luxembourgish (official administrative language and national language (spoken vernacular)) 88.8%, French (official administrative language) 4.2%, Portuguese 2.3%, German (official administrative language) 1.1%, other 3.5% (2011 est.)', 
'English (official, the language generally used), Irish (Gaelic or Gaeilge) (official, spoken by approximately 38.7% of the population as a first or second language in 2011; mainly spoken in areas along the western coast)', 
'Icelandic, English, Nordic languages, German widely spoken', 
'Albanian 98.8% (official - derived from Tosk dialect), Greek 0.5%, other 0.6% (including Macedonian, Roma, Vlach, Turkish, Italian, and Serbo-Croatian), unspecified 0.1% (2011 est.)', 
'Maltese (official) 90.1%, English (official) 6%, multilingual 3%, other 0.9% (2005 est.)', 
'Greek (official) 80.9%, Turkish (official) 0.2%, English 4.1%, Romanian 2.9%, Russian 2.5%, Bulgarian 2.2%, Arabic 1.2%, Filippino 1.1%, other 4.3%, unspecified 0.6% (2011 est.)', 
'Finnish (official) 89%, Swedish (official) 5.3%, Russian 1.3%, other 4.4% (2014 est.)', 
'Bulgarian (official) 76.8%, Turkish 8.2%, Roma 3.8%, other 0.7%, unspecified 10.5% (2011 est.)', 
'Lithuanian (official) 82%, Russian 8%, Polish 5.6%, other 0.9%, unspecified 3.5% (2011 est.)', 
'Latvian (official) 56.3%, Russian 33.8%, other 0.6% (includes Polish, Ukrainian, and Belarusian), unspecified 9.4%', 
'Estonian (official) 68.5%, Russian 29.6%, Ukrainian 0.6%, other 1.2%, unspecified 0.1% (2011 est.)', 
'Moldovan 58.8% (official; virtually the same as the Romanian language), Romanian 16.4%, Russian 16%, Ukrainian 3.8%, Gagauz 3.1% (a Turkish language), Bulgarian 1.1%, other 0.3%, unspecified 0.4%', 
'Armenian (official) 97.9%, Kurdish (spoken by Yezidi minority) 1%, other 1% (2011 est.)', 
'Russian (official) 70.2%, Belarusian (official) 23.4%, other 3.1% (includes small Polish- and Ukrainian-speaking minorities), unspecified 3.3% (2009 est.)', 
'Catalan (official), French, Castilian, Portuguese', 'French (official), English, Italian, Monegasque', 
'Italian', 'Italian', 'Ukrainian (official) 67.5%, Russian (regional language) 29.6%, other (includes small Crimean Tatar-, Moldavian-, and Hungarian-speaking minorities) 2.9% (2001 est.)', 
'Serbian (official) 88.1%, Hungarian 3.4%, Bosnian 1.9%, Romany 1.4%, other 3.4%, undeclared or unknown 1.8%', 
'Serbian 42.9%, Montenegrin (official) 37%, Bosnian 5.3%, Albanian 5.3%, Serbo-Croat 2%, other 3.5%, unspecified 4% (2011 est.)', 
'Albanian (official) 94.5%, Bosnian 1.7%, Serbian (official) 1.6%, Turkish 1.1%, other 0.9% (includes Romani), unspecified 0.1%', 
'Croatian (official) 95.6%, Serbian 1.2%, other 3% (including Hungarian, Czech, Slovak, and Albanian), unspecified 0.2% (2011 est.)', 
'Slovenian (official) 91.1%, Serbo-Croatian 4.5%, other or unspecified 4.4%, Italian (official, only in municipalities where Italian national communities reside), Hungarian (official, only in municipalities where Hungarian national communities reside) (2002 census)', 
'Bosnian (official), Croatian (official), Serbian (official)', 
'Macedonian (official) 66.5%, Albanian (official) 25.1%, Turkish 3.5%, Roma 1.9%, Serbian 1.2%, other 1.8% (2002 est.)', 
'Czech (official) 95.4%, Slovak 1.6%, other 3% (2011 census)', 
'Slovak (official) 78.6%, Hungarian 9.4%, Roma 2.3%, Ruthenian 1%, other or unspecified 8.8% (2011 est.)', 
'German 94.5% (official) (Alemannic is the main dialect), Italian 1.1%, other 4.3% (2010 est.)', 
'', 'English 62.9% (official), Spanish 56.6%, Creole 44.6%, Maya 10.5%, German 3.2%, Garifuna 2.9%, other 1.8%, unknown 0.3%, none 0.2% (cannot speak)', 
'Spanish (official) 60%, Amerindian languages 40%', 'Spanish (official), Nahua (among some Amerindians)', 
'Spanish (official), Amerindian dialects', 'Spanish (official) 95.3%, Miskito 2.2%, Mestizo of the Caribbean coast 2%, other 0.5%', 
'Spanish (official), English', 'Spanish (official), indigenous languages (including Ngabe, Bugle, Kuna, Embera, Wounaan, Naso Tjerdi, and Bri Bri)', 
'French (official)', 'French (official), Creole (official)', '', 
'Spanish (official) 60.7%, Quechua (official) 21.2%, Aymara (official) 14.6%, foreign languages 2.4%, Guarani (official) 0.6%, other native languages 0.4%, none 0.1%', 
'English (official), Guyanese Creole, Amerindian languages (including Caribbean and Arawak languages), Indian languages (including Caribbean Hindustani, a dialect of Hindi), Chinese (2014 est.)', 
'Spanish (Castillian) 93% (official), Quechua 4.1%, other indigenous 0.7%, foreign 2.2%', 
'Spanish (official), Guarani (official)', 'Dutch (official), English (widely spoken), Sranang Tongo (Surinamese, sometimes called Taki-Taki, is native language of Creoles and much of the younger population and is lingua franca among others), Caribbean Hindustani (a dialect of Hindi), Javanese', 
'Spanish (official), Portunol, Brazilero (Portuguese-Spanish mix on the Brazilian frontier)', 
'', '', '', 'Malay (official), English, Chinese dialects', 
'Nauruan 93% (official, a distinct Pacific Island language), English 2% (widely understood, spoken, and used for most government and commercial purposes), other 5% (includes I-Kiribati 2% and Chinese 2%)', 
'Tok Pisin (official), English (official), Hiri Motu (official), some 836 indigenous languages spoken (about 12% of the worlds total); most languages have fewer than 1,000 speakers', 
'English and Tongan 87%, Tongan (official) 10.7%, English (official) 1.2%, other 1.1%, unspecified 0.03% (2006 est.)', 
'Melanesian pidgin (in much of the country is lingua franca), English (official but spoken by only 1%-2% of the population), 120 indigenous languages', 
'local languages (more than 100) 63.2%, Bislama (official; creole) 33.7%, English (official) 2%, French (official) 0.6%, other 0.5% (2009 est.)', 
'English (official), Fijian (official), Hindustani', 
'Palauan (official on most islands) 66.6%, Carolinian 0.7%, other Micronesian 0.7%, English (official) 15.5%, Filipino 10.8%, Chinese 1.8%, other Asian 2.6%, other 1.3%', 
'Wallisian (indigenous Polynesian language) 58.9%, Futunian 30.1%, French (official) 10.8%, other 0.2% (2003 census)', 
'English (official) 86.4%, Cook Islands Maori (Rarotongan) (official) 76.2%, other 8.3%', 
'Niuean (official) 46% (a Polynesian language closely related to Tongan and Samoan), Niuean and English 32%, English (official) 11%, Niuean and others 5%, other 6% (2011 est.)', 
'Samoan (Polynesian) (official), English', 'I-Kiribati, English (official)', 'French (official), 33 Melanesian-Polynesian dialects', 
'Tuvaluan (official), English (official), Samoan, Kiribati (on the island of Nui)', 
'French (official) 61.1%, Polynesian (official) 31.4%, Asian languages 1.2%, other 0.3%, unspecified 6% (2002 census)', 
'Tokelauan 93.5% (a Polynesian language), English 58.9%, Samoan 45.5%, Tuvaluan 11.6%, Kiribati 2.7%, other 2.5%, none 4.1%, unspecified 0.6%', 
'', 'Marshallese (official) 98.2%, other languages 1.8% (1999 census)', 
'Korean', 'Cantonese (official) 89.5%, English (official) 3.5%, Putonghua (Mandarin) 1.4%, other Chinese dialects 4%, other 1.6% (2011 est.)', 
'', 'Khmer (official) 96.3%, other 3.7% (2008 est.)', 'Lao (official), French, English, various ethnic languages', 'Bangla 98.8% (official, also known as Bengali), other 1.2% (2011 est.)', 
'Mandarin Chinese (official), Taiwanese (Min), Hakka dialects', 
'Dhivehi (official, dialect of Sinhala, script derived from Arabic), English (spoken by most government officials)', 
'Arabic (official), French, English, Armenian', 'Arabic (official), English (widely understood among upper and middle classes)', 
'Arabic (official), Kurdish, Armenian, Aramaic, Circassian (widely understood); French, English (somewhat understood)', 
'Arabic (official), Kurdish (official), Turkmen (a Turkish dialect) and Assyrian (Neo-Aramaic) are official in areas where they constitute a majority of the population), Armenian', 
'Arabic (official), English widely spoken', 'Arabic (official)', 
'Arabic (official)', 'Arabic (official), English, Baluchi, Urdu, Indian dialects', 
'Arabic', 'Arabic (official), Persian, English, Hindi, Urdu', 
'Hebrew (official), Arabic (used officially for Arab minority), English (most commonly used foreign language)', 
'Arabic (official), English, Farsi, Urdu', 'Arabic (official), English commonly used as a second language', 
'Sharchhopka 28%, Dzongkha (official) 24%, Lhotshamkha 22%, other 26% (includes foreign languages) (2005 est.)', 
'Khalkha Mongol 90% (official), Turkic, Russian (1999)', 
'Nepali (official) 44.6%, Maithali 11.7%, Bhojpuri 6%, Tharu 5.8%, Tamang 5.1%, Newar 3.2%, Magar 3%, Bajjika 3%, Urdu 2.6%, Avadhi 1.9%, Limbu 1.3%, Gurung 1.2%, other 10.4%, unspecified 0.2%', 
'Tajik (official), Russian widely used in government and business', 
'Turkmen (official) 72%, Russian 12%, Uzbek 9%, other 7%', 
'Azerbaijani (Azeri) (official) 92.5%, Russian 1.4%, Armenian 1.4%, other 4.7% (2009 est.)', 
'Georgian (official) 71%, Russian 9%, Armenian 7%, Azeri 6%, other 7%', 
'Kyrgyz (official) 71.4%, Uzbek 14.4%, Russian (official) 9%, other 5.2% (2009 est.)', 
'Uzbek (official) 74.3%, Russian 14.2%, Tajik 4.4%, other 7.1%', 'English (official)', 'English (official)', 'English', 'English (official) 90.9%, Spanish 4%, Filipino 3.3%, other 1.7%, unspecified 0.1% (2010 est.)', 
'English (official), Portuguese', 'English (official), French patois', 
'English (official)', 'English','English 43.6%, Filipino 21.2%, Chamorro 17.8%, other Pacific island languages 10%, Asian languages 6.3%, other 1.1% (2010 est.)', 
'Samoan 88.6% (closely related to Hawaiian and other Polynesian languages), English 3.9%, Tongan 2.7%, other Pacific islander 3%, other 1.8%', 
'English (official) 67.5%, Spanish 12.9%, Creole 8.2%, Dutch (official) 4.2%, Papiamento (a Spanish-Portuguese-Dutch-English dialect) 2.2%, French 1.5%, other 3.5% (2001 census)', 
'English (official), French patois', 'English (official), French patois', 'English, French patois', 
'Spanish, English', 'Spanish, English', 'Spanish (official)', 
'Spanish (official)', 'Spanish (official)','English (official), Caribbean Hindustani (a dialect of Hindi), French, Spanish, Chinese', 
'English (official)', 'English, English patois'
)
two_letter_country_code <- c('US','CA', 'RU','KZ', 'EG', 'ZA', 'GR', 
 'NL', 'BE', 'FR', 'ES', 'HU', 'IT', 'RO', 'CH', 'AT', 'GB', 'DK', 'SE', 'NO', 'PL', 
 'DE', 'PE', 'MX', 'CU', 'AR', 'BR', 'CL', 'CO', 'VE', 'MY', 'AU', 'ID', 'PH', 
 'NZ', 'SG', 'TH', 'JP', 'KR', 'VN', 'CN', 'TR', 'IN', 'PK', 'AF', 'LK', 
 'MM', 'IR', 'SS', '', 'DZ', 'TN', 'LY', 'GM', 'SN', 'MR', 'ML', 'GN', 
 'CI', 'BF', 'NE', 'TG', 'BJ', 'MU', 'LR', 'SL', 'GH', 'NG', 'TD', 
 'CF', 'CM', 'CV', 'ST', 'GQ', 'GA', 'CG or BS', 'CD', 'AO', 'GW', 
 '', 'SC', 'SD', 'RW', 'ET', 'SO', 'DJ', 'KE', 'TZ', 'UG', 'BI', 
 'MZ', 'ZM', 'MG', '', 'ZW', 'NA', 'MW', 'LS', 'BW', '', 'KM', 'SH', 
 'ER', 'AW', 'FO', 'GL', 'GI', 'PT', 'LU', 'IE', 'IS', 'AL', 'MT', 'CY', 
 'FI', 'BG', 'LT', 'LV', 'EE', 'MD', 'AM', 'BY', 'AD', 'MC', 'SM', 
 'VA', 'UA', 'RS', 'ME', 'XK', 'HR', 'SI', 'BA', 'MK', 'CZ', 'SK', 'LI', 
 'FK', 'BZ', 'GT', 'SV', 'HN', 'NI', 'CR', 'PA', 'PM', 'HT', '', 'BO', 
 'GY', 'EC', 'PY', 'SR', 'UY', '', '', 'AQ', 'BN', 'NR', 'PG', 'TO', 'SB', 
 'VU', 'FJ', 'PW', 'WF', 'CK', 'NU', 'WS', 'KI', 'NC', 'TV', 'PF', 'TK', 
 'FM', 'MH', 'KP', 'HK', 'MO', 'KH', 'LA', 'BD', 'TW', 'MV', 'LB', 'JO', 
 'SY', 'IQ', 'KW', 'SA', 'YE', 'OM', 'PS', 'AE', 'IL', 'BH', 'QA', 'BT', 
 'MN', 'NP', 'TJ', 'TM', 'AZ', 'GE', 'KG', 'UZ', 'AI', 'VG', 'VI', 'KY', 
 'BM', 'GD', 'TC', 'MS', 'GU', 'AS', 'SX', 'LC', 'DM', 'VC', 'PR', 
 'PR', 'DO', 'DO', 'DO', 'TT', 'KN', 'JM'
)

Country_codes <- as.data.frame(cbind(Countries, code, two_letter_country_code, languages))

States <-
 c(
 'NJ', 'DC', 'CT', 'MB', 'AL', 'WA', 'ME', 'ID', 'CA', 'TX', 'NY', 'CA', 'TX', 'PA', 'OH', 'IL', 'MN', 'IN', 'IL', 'LA', 'ON', 'MS', 'GA', 'MI', 'OH', 'BC', 'FL', 'MD', 'MI', 'BC', 'AL', 'NC', 'WA', 'TX', 'AL', 'IN', 'WI', 'PA', 'MI', 'KY', 'PA', 'VA', 'MI', 'TX', 'OH', 'ON', 'MD', 'DE',
 'CO', 'WV', 'FL', 'SK', 'WY', 'NE', 'IL', 'CA', 'IL', 'MI', 'MO', 'NY', 'KS', 'IN', 'LA', 'IA', 'MN', 'FL', 'CA', 'TX', 'OH', 'IL', 'AL', 'NC',
 'LA', 'MA', 'VI', 'CA', 'ON', 'NY', 'MA', 'FL', 'WA', 'TX', 'ON', 'CA', 'OH', 'UT', 'FL', 'RI', 'NE', 'AB', 'GA', 'OK', 'MT', 'FL', 'CA', 'TX', 'MD', 'PA', 'MA', 'WI', 'CA', 'ON', 'MO', 'QC', 'OH', 'TN', 'CA', 'WA', 'TX', 'MB', 'TX', 'VA', 'UT', 'ON', 'QC', 'OH', 'CA', 'MD', 'QC', 'IL', 'TX', 'GA', 'CT', 'GA', 'AR', 'AZ', 'QC', 'PA', 'AR', 'KY', 'OR', 'LA',
 'NM', 'NB', 'MN', 'MA', 'WA', 'CA', 'TX', 'OH', 'QC', 'IA', 'NY', 'MI', 'NY', 'ON', 'AZ', 'CA', 'OK', 'VA', 'OR', 'ON', 'NJ', 'MO', 'CA', 'FL', 'CA', 'IA', 'WA', 'OH', 'PA', 'VA', 'MO', 'IN', 'NM', 'QC', 'OK', 'NY', 'MI',
 'AB', 'MS', 'AZ', 'NH', 'BC', 'SD', 'KY', 'NY', 'WI', 'NJ', 'PA', 'MN', 'ON', 'OH', 'TN', 'MI', 'MA', 'IL', 'CA', 'KS', 'AZ', 'CA', 'CA', 'CA', 'TN', 'IL',
 'NY', 'MO', 'SK', 'IA', 'NY', 'ON', 'CA', 'MN', 'CA', 'MO', 'CA', 'MS', 'CA', 'MP', 'GU', 'GA', 'MI', 'WV', 'TX', 'FL', 'ND', 'NV', 'VA', 'NC', 'ON', 'GA', 'CA', 'IL', 'NL', 'IA', 'TX', 'CA', 'WI', 'NY', 'PA', 'NY', 'CO', 'CO', 'PA', 'NV', 'FL', 'TN',
 'NJ', 'MI', 'TX', 'OH', 'CA', 'FL', 'VA', 'CA', 'GA', 'MN', 'CA', 'IN', 'MS', 'GA', 'FL', 'IL', 'MA', 'NV', 'BC', 'IL', 'AB', 'MA', 'NS', 'KS', 'FL', 'PR', 'UT', 'VT', 'SC',
 'VA', 'CA', 'TX', 'ON', 'HI', 'MI', 'IN', 'FL', 'PA', 'IL', 'MO', 'TX', 'CA', 'QC', 'AB', 'NC', 'TX', 'CA', 'TX', 'PA', 'SC', 'NY', 'IL', 'NJ', 'FL', 'NJ', 'MA',
 'CA', 'KY', 'CT', 'NJ', 'FL', 'SC', 'TN', 'YT', 'AR', 'IL', 'QC', 'PA', 'TN', 'NS', 'TX', 'FL', 'ON', 'MI', 'AK', 'NJ', 'CA', 'NC', 'GA', 'KS', 'NY', 'TX', 'CA', 'NY', 'OK', 'NC', 'WI', 'CA', 'FL', 'AZ', 'NY', 'TN', 'CA', 'TX', 'OH', 'PR', 'TX', 'FL',
 'MI', 'CA', 'CA', 'MN', 'FL', 'TX', 'NM', 'CT', 'CO', 'OR', 'TX', 'NJ', 'MO', 'MA', 'TX', 'NC', 'NC', 'LA', 'MI', 'Toll Free','Toll Free','Toll Free','Toll Free'
 )

area_codes <-
 c(
 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 212, 213, 214, 215, 216, 217, 218, 219, 224, 225, 226, 228, 229, 231,
 234, 236, 239, 240, 248, 250, 251, 252, 253, 254, 256, 260, 262, 267, 269, 270, 272, 276, 278, 281, 283, 289, 301, 302, 303,
 304, 305, 306, 307, 308, 309, 310, 312, 313, 314, 315, 316, 317, 318, 319, 320, 321, 323, 325, 330, 331, 334, 336, 337, 339, 340,
 341, 343, 347, 351, 352, 360, 361, 365, 369, 380, 385, 386, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 412, 413, 414, 415,
 416, 417, 418, 419, 423, 424, 425, 430, 431, 432, 434, 435, 437, 438, 440, 442, 443, 450, 464, 469, 470, 475, 478, 479, 480, 481,
 484, 501, 502, 503, 504, 505, 506, 507, 508, 509, 510, 512, 513, 514, 515, 516, 517, 518, 519, 520, 530, 539, 540, 541, 548, 551,
 557, 559, 561, 562, 563, 564, 567, 570, 571, 573, 574, 575, 579, 580, 585, 586, 587, 601, 602, 603, 604, 605, 606, 607, 608, 609,
 610, 612, 613, 614, 615, 616, 617, 618, 619, 620, 623, 626, 627, 628, 629, 630, 631, 636, 639, 641, 646, 647, 650, 651, 657,
 660, 661, 662, 669, 670, 671, 678, 679, 681, 682, 689, 701, 702, 703, 704, 705, 706, 707, 708, 709,
 712, 713, 714, 715, 716, 717, 718, 719, 720, 724, 725, 727, 731, 732, 734, 737, 740, 747, 754, 757,
 760, 762, 763, 764, 765, 769, 770, 772, 773, 774, 775, 778, 779, 780, 781, 782, 785, 786, 787,
 801, 802, 803, 804, 805, 806, 807, 808, 810, 812, 813, 814, 815, 816, 817, 818, 819, 825, 828, 830, 831, 832,
 835, 843, 845, 847, 848, 850, 856, 857, 858, 859, 860, 862, 863, 864, 865, 867, 870, 872, 873,
 878, 901, 902, 903, 904, 905, 906, 907, 908, 909, 910, 912, 913, 914, 915, 916, 917, 918, 919, 920, 925, 927, 928, 929,
 931, 935, 936, 937, 939, 940, 941, 947, 949, 951, 952, 954, 956, 957, 959, 970, 971, 972, 973, 975, 978, 979, 980, 984, 985, 989, 800, 888, 866, 877
 )


N_America_codes <- as.data.frame(cbind(States, area_codes))
N_America_codes$languages <- "English"

strings[which((strings$sub_3 %in% area_codes) & (strings$sub_3 %in% code)),"Error_Value"] <- "Area Code Country Code Ambiguous"

Country_codes$two_letter_country_code <- as.character(Country_codes$two_letter_country_code)

N_America_Numbers <- sqldf(
 "select s.*, NAC.states, NAC.area_codes
 from strings s
 left join N_America_codes NAC
 on nac.area_codes = s.sub_3
 where (s.provided_country_code = 'US'
 or s.provided_country_code = 'CA')"
)


N_America_Numbers$languages <- "English"

int_numbers <- sqldf(
  "select s.*, c.Countries, c.languages, c.code
  from strings s
  left join Country_codes c
  on s.provided_country_code = c.two_letter_country_code
where s.provided_country_code not like '%US%'
  and s.provided_country_code not like '%CA%'"
)

colnames(N_America_Numbers)<- c("string_len", "strings","Unique Identifier", "provided_country_code","Raw_Data",  "Error_Value", "sub_3",                
                                "sub_2", "Location","Phone Code", "languages")

colnames(int_numbers)<- c("string_len", "strings","Unique Identifier","provided_country_code","Raw_Data",  "Error_Value", "sub_3"   ,"sub_2", "Location", "languages","Phone Code")

N_America_Numbers$Error_Value <- NA

N_America_Numbers$strings <- paste("1", N_America_Numbers$strings, sep = "-")
int_numbers[is.na(int_numbers$Error_Value),"strings"]<- paste("011",int_numbers[is.na(int_numbers$Error_Value),"strings"], sep = "-")


test<- rbind(int_numbers, N_America_Numbers)
test<- unique(test)
# names(test)
output <-  test[,c("Unique Identifier", "provided_country_code","Phone Code", "Raw_Data","strings", "Error_Value", "Location", "languages")]
output[grep("extension", output[,"strings"]),"Error_Value"] <- "Contains an Extension"


output[is.na(output$Error_Value),"Error_Value"]<- ""
output[(output$Error_Value != ""),"strings"]<- ""


                  
colnames(output)<-c("Unique Identifier", "provided_country_code","Phone Code", "Raw_Data","Cleaned Number", "Error_Value", "Location", "languages")
date <- Sys.Date()
setwd("C:\\Users\\Eric\\Documents\\R\\Task Automation\\PhoneNumberCleaning")
output[,"Cleaned Number"]<- str_replace_all(output[,"Cleaned Number"], "[^0-9]", "")
write.csv(output, paste(date, "CleanedPhoneNumbers.csv", sep = "-"), row.names = F)


}


system.time(phoneNumberCleaner())
