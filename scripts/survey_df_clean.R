# Title:   qualtrics.R
# Date:    2023-12-15
# Author:  Daniel Weitzel
# Email:   daniel.weitzel@colostate.edu
# Purpose: Collect data from Qualtrics and prepare it for analysis 

# Working directory
setwd(githubdir)
setwd("extreme_recall2")

# Libraries
library(tidyverse)
library(readxl)

## Bonica_ids 
list_bonica_closed <- c(# Republicans
  "cand625nominee", "cand3331", "cand3334nominee", "cand105110", "cand584", "cand144919", "cand992", "cand110286", "cand1403", "cand418", "cand108459", "cand837", "cand100270nominee", "cand1573",
  # Democrats
  "cand221nominee", "cand3333nominee", "cand1572nominee", "cand760", "cand3335nominee", "cand158985", "cand159005", "cand1235", "cand140679","cand1616", "cand744", "cand146299", "cand1600", "cand41775")
list_bonica_open <- c("cand10", "cand1002", "cand100270nominee", "cand1023", "cand105110", "cand108459", "cand110286", "cand1119", "cand1161", "cand1235", "cand130816", "cand131167", "cand1312", "cand132239", "cand137716", "cand1403", "cand140679", "cand142635", "cand143949", "cand144919", "cand145542", "cand149269", "cand1503", "cand152762", "cand153422", "cand1572nominee", "cand1572nominee", "cand1588", "cand158985", "cand159005", "cand1592", "cand159281", "cand159285", 'cand1600', "cand1603", 'cand160447', "cand1635", "cand166080", 'cand221nominee', "cand23468", "cand28657", "cand303", "cand3330", "cand3331", 'cand3333nominee',"cand3334nominee", "cand3335nominee", 'cand335', 'cand402', "cand41326", "cand41360", "cand41775", "cand43832", "cand545", "cand584", "cand61168", "cand616", 'cand625nominee', "cand69030231", "cand760", "cand82171", "cand837", "cand909", "cand968", "cand98927")

df_open_ids <- read_csv("data/politicians/names_open_to_id_coded.csv")
df_open_ids_complicated <- read_csv("data/politicians/names_open_to_id.csv")

## Load the politicians data 
df_politicians <- read_excel("data/politicians/politicians_attributes.xlsx") |> 
  drop_na(bonica.rid) |> 
  dplyr::select(bonica.rid, race_ethnicity, recency, most_recent_office)

## Load and process DIME data
df_dime_closed_merge <-
  read_csv("data/cf_scores/dime_recipients_1979_2020.csv.gz") |> 
  mutate(party = ifelse(party.orig == "100", "Democrat",
                        ifelse(party.orig == "200", "Republican", NA))) |> 
  dplyr::select(bonica.rid, election, cycle, fecyear, name, lname, fname, 
                cand.gender:irt.cfscore, party, seat) |> 
  mutate(name_f = as.factor(name)) |> 
  filter(bonica.rid %in% list_bonica_closed) |> 
  group_by(bonica.rid) |> 
  filter(cycle == max(cycle)) |> 
  mutate(drop = ifelse(bonica.rid == "cand1235" & seat == "federal:senate", "drop",
                       ifelse(bonica.rid == "cand41775" & seat == "federal:house", "drop", "keep")),
         executive = str_detect(seat, "president|governor")) |> # this classifies the last race
  filter(drop == "keep") |> 
  dplyr::select(-drop) |>  
  left_join(df_politicians) |> 
  rename(party_name = party)

df_dime_open_merge <-
  read_csv("data/cf_scores/dime_recipients_1979_2020.csv.gz") |> 
  mutate(party = ifelse(party.orig == "100", "Democrat",
                        ifelse(party.orig == "200", "Republican", NA))) |> 
  dplyr::select(bonica.rid, election, cycle, fecyear, name, lname, fname, 
                cand.gender:irt.cfscore, party, seat) |> 
  mutate(name_f = as.factor(name)) |> 
  filter(bonica.rid %in% list_bonica_open) |> 
  group_by(bonica.rid) |> 
  filter(cycle == max(cycle)) |> 
  mutate(drop = ifelse(bonica.rid == "cand1235" & seat == "federal:senate", "drop",
                       ifelse(bonica.rid == "cand41775" & seat == "federal:house", "drop", "keep")),
         executive = str_detect(seat, "president|governor")) |> # this classifies the last race
  filter(drop == "keep") |> 
  dplyr::select(-drop) |>  
  left_join(df_politicians) |> 
  rename(party_name = party)

rm(df_politicians)


## Load the data
## Process the survey data so it's ready for analysis (remove text, make vars numeric etc)
df_er_clean <- read_csv("data/lucid/er2_lucid_jan2023.csv") |> 
  dplyr::select(-c(RecipientLastName:ExternalReference)) |> 
  filter(Q1.1 != "I do not agree") |> 
  mutate(passed_ac = ifelse(Q4.1_1 == "Extremely interested" &
                              Q4.1_2 == "Very interested", TRUE, FALSE),
         partisan = ifelse(str_detect(Q2.4, "Republic"), "Republican",
                           ifelse(str_detect(Q2.4, "Democrat"), "Democrat", NA)),
         partisan = ifelse(Q2.1 == "Democrat", "Democrat",
                           ifelse(Q2.1 == "Republican", "Republican", partisan)),
         pol_interest = as.numeric(str_remove_all(Q2.5_1,"Passionately interested |Not at all interested ")))

## Reduce to only the vars with data about the participants we need
df_er_participants <-
  df_er_clean |> 
  dplyr::select(-c(Q5.1_1:Q5.1_3, Q6.1_1:Q6.1_3, 
                   Q5.2_71:Q5.2_85, Q6.2_37:Q6.2_52, 
                   contains("_DO_")))

## Process the open text responses. 
df_er_open <- 
  df_er_clean |> 
  dplyr::select(ResponseId, Q5.1_1:Q5.1_3, Q6.1_1:Q6.1_3) |> 
  pivot_longer(cols = -ResponseId,
               names_to = "question_open",
               values_to = "politicians_open") |> 
  separate(question_open, into = c("question_stem", "number"), sep = "_", remove = FALSE) |> 
  mutate(party_open_q = ifelse(str_detect(question_open, "5"), "Democrats", "Republicans")) |> 
  group_by(ResponseId, question_stem) |> 
  mutate(number = as.numeric(number),
         politicians_open_count = sum(!is.na(politicians_open))) |> 
  ungroup() |> 
  #unite("party_open", c(party_open, number), sep = "_", remove = FALSE) |> 
  dplyr::select(-question_stem) |> 
  mutate(politicians_open_lower = str_to_lower(politicians_open)) |> 
  group_by(politicians_open_lower) |> 
  add_count(politicians_open) |> 
  #distinct() |> 
  ungroup() |> 
  mutate(politican_clean = ifelse(str_detect(politicians_open_lower,"olta|cara|bennett|gates|kavanaugh|mussolini|acn|sam|cruise|sandler|hire|unila|iris|ami|kiara|none|nike|gucci|bathroom|nikeb|boo|valente|dde|maga|honest|nond|hjjh|fcjj|rizzler|gyat|ohio|fanum|skibidi|gigachad|politic|election|future|mone|luis|ggvyv|yvvygv|vgvuy|yhgvyfgvy|vfv|fuck|orange|them|one|like|king|nice|idk|wisconsin|michigan|manager|sarah|nothing|available|butt|only|caller|first|linnie|nine|good|know|gjsas|sjkhsik|sjoiusi|jgjy|fjfj|forgot|forgo|nobe|nonenone|rudg|nome|mark|hitler|telling|no true|determined|strong|bruce|musk|one|pay|attention|putin|jong|queen|buŕr|white|then|fifth|foreign|rug|tf|rush do|jean pierre|zombies|antonio|fish|naci|bert|cara|naive|puppets|unsure|manipulative|scam|egocentric|democrat|republicans|xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|asshole|asshole|nonw|satan|antichrist|epstein|dianne|alex|jim|dave|evelyn|isabella|pololce|vice|remember|conservatives|liberals|moderates|contemporary|taxes|freedoms|first|nonr|mind|gay|ass|homos|trim|guy|dang|anti|derrick|familia|henry|pution|nobody|neutral|not|sure|somebody|crooks|insurrectionists|jail|trustworthy|reliable|dishonest|speaker|jone|non|the|dominican"), "Spam", NA),
         politican_clean = ifelse(str_length(politicians_open_lower) < 3, "Spam", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "biswn|sleepy joe|binden|biddn|bden|joe b|biden|blden|bidon|biedin|baby joe|beiden|baiden|bidden|bidien|boden|buden"), "Joseph Biden", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "oboma|obomba|obama|o'bama|obsma|barack|omaba"), "Barack Obama", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "ocasio cortez|aoc"), "Alexandria Ocasio Cortez", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "abbott|abbot|abot|abott"), "Greg Abbott", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "de saintes|desantis|de santis|destantis|aantis|de santis| de santos|desantes|desania|desantes|desanties|desantus|dessntis|desanttis|desantos|desantis|desantia|desanttis|dessntis|destantis"), "Ron DeSantis", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "camala|camella|harris|haris|hariss|camilla|kalama|kamala|kalmara|kamale|kamila|kamilia|kamillakammala|kammila"), "Kamala Harris", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "lockdown donald|thrump|trump|trumph|trumt|trup|trunp|trumo|teump|trimp"), "Donald Trump", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "clinton"), "Hillary Clinton, ambiguous", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "bill clinton|bill clintin|william clinton|bill clinyon|bill cliton|bill clonton"), "Bill Clinton", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "hillary|hilary|hilory|hilliary"), "Hillary Clinton, unambiguous", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "curz|cruz"), "Ted Cruz", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "bernie|sanders"), "Bernie Sanders", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "mcain|mccain|cain"), "John McCain", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "benjamin harrison"), "Benjamin Harrison", politican_clean), 
         politican_clean = ifelse(str_detect(politicians_open_lower, "jaime harrison"), "Jaime Harrison", politican_clean), 
         politican_clean = ifelse(str_detect(politicians_open_lower, "nicole beus harris"), "Nicole Beus Harris", politican_clean), 
         politican_clean = ifelse(str_detect(politicians_open_lower, "teddy kennedy|ted kennedy"), "ted Kennedy", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "shumer|shummer|schumer"), "Chuck Schumer", politican_clean), 
         politican_clean = ifelse(str_detect(politicians_open_lower, "mitch|maconellj|mcconel|mcconell|mikconll|oconnell|connell"), "Mitch Mcconnell", politican_clean), 
         politican_clean = ifelse(str_detect(politicians_open_lower, "roosevelt"), "Theodore Roosevelt", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "haley|hayley|haley|niki|halley|haly|haley|nikki|hailey|halley"), "Nikki Haley", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "nancy|palosey|polesi|polosi|poleski|pelo|piloski|poloci|poloski"), "Nancy Pelosi", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "romeny|rommey|romney"), "Mitt Romney", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "nixon|nixion"), "Richard Nixon", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "palin|palen"), "Sarah Palin", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "jim jordan"), "Jim Jordan", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "mark kelly"), "Mark Kelly", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "kevin|mccarthey|mccarthy"), "Kevin McCarthy", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "robert kennedy|robert kenney|robert f kennedy jr"), "Robert F Kennedy ", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "john f robert"), "John F Robert", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "carter"), "Jimmy Carter", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "buttegig|buttigieg|buddigegg"), "Pete Buttigieg", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "booker"), "Cory Booker", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "pres bush|busch|geotge bush|george bush|bush"), "George W Bush, ambiguous", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "bush one|hw bush|bush senior|h w bush|bush sr"), "George HW Bush", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "george w bush|bush jr|bush two|john w bush|boush"), "George W Bush, unambiguous", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "barrasso"), "John Barrasso", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "hassan,"), "Maggie Hassan", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "boebert"), "Lauren Boebert", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "rubio"), "Mark Rubio", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "santas|santos"), "George Santos", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "manchin"), "Joe Manchin", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "marjorie|majorie taylor green|taylor greene|green|mtg"), "Marjorie Taylor Greene", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "pence|pience|pense"), "Mike Pence", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "warren"), "Elizabeth Warren", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "hakeem|jefferies"), "Hakeem Jeffries", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "gore"), "Al Gore", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "emmer|emmere"), "Tom Emmer", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "tim scott"), "Tim Scott", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "tim ryan"), "Tim Ryan", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "tim burton"), "Tim Burton", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "jefferson"), "Thomas Jefferson", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "susan collins"), "Susan Collins", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "ron paul"), "Ron Paul", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "rick scott"), "Rick Scott", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "cheney|chaney"), "Dick Cheney", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "ben carson"), "Ben Carson", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "abraham|abriham|lincoln"), "Abraham Lincoln", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "kinzinger"), "Adam Kinzinger", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "franken"), "Al Franken", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "andrew johnson"), "Andrew Johnson", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "shwarseeneggar|schwarzenegger"), "Arnold Schwarzenegger", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "asa hutcinson"), "Asa Hutchinson", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "barbara lee"), "Barbara Lee", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "ben carson"), "Ben Carson", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "bill lee"), "Bill Lee", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "dole"), "Bob Dole", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "menendez"), "Bob Menendez", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "kemp"), "Brian Kemp", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "newsom|newsome|newsum"), "Gavin Newsom", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "kennedy|kennady"), "John F. Kennedy ambiguous", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "jfk|john  kennedy|john f kennedy|president kennedy|john kenndy|john kennedy"), "John F. Kennedy, unambiguous", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "liz chaney|liz chanie|liz cheney|liz cheney|liz cheney|liz cheyney"), "Liz Cheney", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "graham|lindsey"), "Lindsey Graham", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "fetterman"), "John Fetterman", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "john kasich"), "John Kasich", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "scalise"), "Steve Scalise", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "ronna"), "Ronna Mcdaniel", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "durbin|durban"), "Richard Durbin", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "rudy guiliani"), "Rudy Guiliani", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "tulsi gabbard"), "Tulsi Gabbard", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "benjamin franklin"), "Benjamin Franklin", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "katherine clark"), "Katherine Clark", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "klobashar|klobuchar"), "Amy Klobuchar", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "schemer|schmuer|schummer|scimmer|shumir"), "Chuck Schumer", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "cristie|chirsty|christi|christie|cristy|kristie|christy"), "Chris Christie", politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "raegan|ragan|ragen|raygun|reagan|reagen|regan"), "Ronald Reagan",  politican_clean),
         politican_clean = ifelse(str_detect(politicians_open_lower, "vivik|vivek|vivak|ramaswami|ramishwami|rama swamy|ramaswamy"), "Vivek Ramaswamy", politican_clean)) |> 
  left_join(df_open_ids_complicated) |> 
  left_join(df_open_ids) |> 
  mutate(politican_clean = coalesce(politican_clean, politican_clean2),
         bonica.rid = coalesce(bonica.rid, bonica.rid2)) |> 
  dplyr::select(-ends_with("2")) |> 
  left_join(df_dime_open_merge) |> 
  left_join(df_er_participants)



rm(df_open_ids, df_open_ids_complicated)

## Process the closed responses and merge the other data with that 
df_er_closed <-
  df_er_clean |> 
  dplyr::select(ResponseId, Q5.2_71:Q5.2_85, Q6.2_37:Q6.2_52) |> 
  pivot_longer(cols = -ResponseId,
               names_to = "question_closed",
               values_to = "politicians_closed") |> 
  mutate(bonica.rid = case_when(# Democrats
    politicians_closed == "Joseph Biden" ~ "cand221nominee", 
    politicians_closed == "Bill Clinton" ~ "cand3333nominee", 
    politicians_closed == "Hillary Clinton" ~ "cand1572nominee", 
    politicians_closed == "John F. Kennedy" ~ "", #empty 
    politicians_closed == "Nancy Pelosi" ~ "cand760",
    politicians_closed == "Barack Obama" ~ "cand3335nominee", 
    politicians_closed == "Kamala Harris" ~ "cand158985", 
    politicians_closed == "Elizabeth Warren" ~ "cand159005", 
    politicians_closed == "Bernie Sanders" ~ "cand1235", 
    politicians_closed == "Alexandria Ocasio-Cortez" ~ "cand140679",
    politicians_closed == "Dianne Feinstein" ~ "cand1616", 
    politicians_closed == "John Lewis" ~ "cand744",  
    politicians_closed == "Ilhan Omar" ~ "cand146299",
    politicians_closed == "Joe Manchin" ~ "cand1600", 
    politicians_closed == "Tulsi Gabbard" ~ "cand41775",
    # Republicans
    politicians_closed == "John McCain" ~ "cand625nominee", # alternative: cand625
    politicians_closed == "Ronald Reagan" ~ "cand3331", 
    politicians_closed == "George W. Bush" ~ "cand3334nominee", 
    politicians_closed == "Ted Cruz" ~ "cand105110", 
    politicians_closed == "Mitch McConnell" ~ "cand584", 
    politicians_closed == "Mitt Romney" ~ "cand144919", # alternative: cand100440
    politicians_closed == "Michele Bachmann" ~ "cand992", 
    politicians_closed == "Greg Abbott" ~ "cand110286", 
    politicians_closed == "Lindsey Graham" ~ "cand1403", 
    politicians_closed == "Newt Gingrich" ~ "cand418", 
    politicians_closed == "Nikki Haley" ~ "cand108459", 
    politicians_closed == "Mike Pence" ~ "cand837", 
    politicians_closed == "Donald Trump" ~ "cand100270nominee", 
    politicians_closed == "Richard Nixon" ~ "", # empty
    politicians_closed == "Lisa Murkowski" ~ "cand1573")) |> 
  separate(question_closed, into = c("question_stem", "number"), sep = "_", remove = FALSE) |> 
  mutate(party = ifelse(str_detect(question_closed, "5"), "dem", "rep"),
         number = as.numeric(number),
         number = ifelse(party == "dem", number-70, number-36)) |> 
  group_by(ResponseId, question_stem) |> 
  mutate(politicians_closed_count = sum(!is.na(politicians_closed))) |> 
  ungroup() |> 
  #unite("party_closed", c(party_closed, number), sep = "_", remove = FALSE) |> 
  dplyr::select(-question_stem) |>
  #drop_na(politicians_closed) |> 
  left_join(df_dime_closed_merge) |> 
  left_join(df_er_participants)

## Issues
df_er_issues_open <- 
  df_er_clean |> 
  dplyr::select(ResponseId, Q9.1_1:Q9.1_3,Q10.1_1:Q10.1_3) |> 
  pivot_longer(cols = -ResponseId,
               names_to = "question_open",
               values_to = "issues_open") |> 
  separate(question_open, into = c("question_stem", "number"), sep = "_", remove = FALSE) |> 
  mutate(party = ifelse(str_detect(question_open, "5"), "dem", "rep")) |> 
  group_by(ResponseId, question_stem) |> 
  mutate(number = as.numeric(number),
         issues_open_count = sum(!is.na(issues_open))) |> 
  ungroup() |> 
  #unite("party_open", c(party_open, number), sep = "_", remove = FALSE) |> 
  dplyr::select(-question_stem) |> 
  mutate(issue_coded = ifelse(str_detect(issues_open, "woman|Woman|women|Women"), "Women", NA_character_),
         issue_coded = ifelse(str_detect(issues_open, "AntiChoice|Pro choice|Prochoice|Pro life|Pro Choice|abortion|Abortion|Abotion|Aboriton|Roe vs wade|Pro Life|Reproductive|Repo"), "Abortion", issue_coded),
         issue_coded = ifelse(str_detect(issues_open, "Disability|Accessibility|Inclusiveness|CRT|Inclusive|minorites|Homophobia|color|civil|right|Civil Right|LGBT|equity|Gender|gender|Lgbtq|Black|black|Social Justice|Equity|Diversity|diversity|inclusivity|Racism|racism|Woke|woke|Equal|equal|LGBTQ|BLM|Poc|Migrants|race|Race|racial|Racial|Same sex|same sex|Gay|gay|trans|Trans|Black lives matter"), "Minority Rights", issue_coded),
         issue_coded = ifelse(str_detect(issues_open, "Gum|gun|Gun|Second|second|firearm|firearms"), "2nd Ammendment", issue_coded),
         issue_coded = ifelse(str_detect(issues_open, "Drill|Fuel|Solar|Green|Nature|sustainability|CLimate|climate|Climate|environ|Environ|warming|Warming"), "Climate/Environment", issue_coded),
         issue_coded = ifelse(str_detect(issues_open, "Trump|trump|TRUMP"), "Trump (pro/anti)", issue_coded),
         issue_coded = ifelse(str_detect(issues_open, "Biden|biden|BIDEN|Byden"), "Biden (pro/anti)", issue_coded),
         issue_coded = ifelse(str_detect(issues_open, "Education|education"), "Education", issue_coded),
         issue_coded = ifelse(str_detect(issues_open, "Christ|Conservative|values|Liberal|Values|conservative|progressive|Patriot|liberal|Traditional|Moral|moral|Relig|relig"), "Trad. Values", issue_coded),
         issue_coded = ifelse(str_detect(issues_open, "Energy|energy|Oil|oil|Gas|gas"), "Energy", issue_coded),
         issue_coded = ifelse(str_detect(issues_open, "Jail|Safety|Court|Violence|Law|law|Order|order|home land security|Police|police|drug|Drug|crime|Crime|justice|Justice|Public safety"), "Police/Crime", issue_coded),
         issue_coded = ifelse(str_detect(issues_open, "Coronavirus|programs|Hand|Homelessness|Housing|Public|senior|Services|Social|class|Vaccines|Covid|healtcare|Heath|low income|Poverty|poverty|homeless|healthcare|underprivileged|Social security|Social Security|social security|welfare|Welfare|Medi|medi|Health|health"), "Welfare/Healthcare", issue_coded),
         issue_coded = ifelse(str_detect(issues_open, "funding|Spend|reform|Budject|Income|income|Ecomony|econ|gas|Gas|Econ|budge|Loans|loans|Recession|Transport|Treasury|trade|Trade|Prices|prices|Bills|Economy|economy|Deficit|Class|jobs|Jobs|Spending|spending|Cost|cost|budget|Budget|Wages|wages|Unemployment|unemployment|Inflation|inflation|Tax|tax|Retirement|Debt|debt|unions|Exonomics|Corporate|Employment|Job|Closing|funding|Vacations|forgiveness|Infrastructure|Price|involvement|Corporate|richer|TAXES|Union|money|Money|restraint|buisness|Privatization|ECONOMY|shutdown"), "Economy/Finances", issue_coded),
         issue_coded = ifelse(str_detect(issues_open, "IMIGRATION|Immagration|Immerigation|illegals|Imigration|Illegals|Mexico|Immigr|Boader|boader|Immigration|immigration|immigrants|imirgation|Imagation|Visa|visa|Border|border|Boarder|boarder"), "Immigration/Border", issue_coded),
         issue_coded = ifelse(str_detect(issues_open, "Impeach|impeach|Disinformation|nominee|white house|White House|White house|Gerry|gerry|allegations|Democracy|democracy|immunity|Voting|voting|Voter|voter|Election|election|interference|Interference|Insurrection|insurrection|Maga|Cover up|Totalitarianism|dictatorship|fascist|No clear|messaging|coordination|January|Fascism|Term|Disagreements|Extremism|haley|Misinformation|Obstruction|DEMOCRACY|primary|primary|candidate|swamp|Lago|polls|senate|president|Bush|Clinton|Hilary|Hillary|Obama"), "Election/Democracy", issue_coded),
         issue_coded = ifelse(str_detect(issues_open, "Defense|security|Isolationism|Security|countries|UKAICRAIN|oversea|Protection|global|Afghanistan|World|Russia|peace|Peace|isreal|Ukrainian|National security|national security|National Security|china|China|Isarel|Israel|israel|WAR|veterans|Us safty|Ukraine|Military|military|Conflict|conflcit|war|War|Crisis|crisis|Terror|terror|Weapon|weapon|Foreign|foreign "), "Foreign Policy/Conflicts/Military", issue_coded),
         issue_coded = ifelse(str_detect(issues_open, "Accountability|vigilantes|first|Arrogance|intrest|autocratic|Bigotry|Billionaires|Middle|Bullied|Censorship|Cheating|compromise|Control|Corruption|chaos|Dividing|Elitist|state|Evil|Fighting|peers|pockets|For the ple|sexual|everything|Getting along|Getting things done|goverment over people|freedoms|Greed|Helping|Hipicrats|Homophobic|Honest|honest|cry|Infighting|machine|down|rude|Liar|Liberty|Lies|fear|resolved|losers|Make money|moderation|lies|extremists|Nonextremis|intrest|caring|Obstructionism|Okay|minded|stupid|outspoken|Patriots|Personal|greed|Power|power|Prejudice|Pushing an agenda|respect|richer|Rights|Save the US|Scamming|Sensitive|Sex|solid|Solid|bulling|opposite|Stormy|Subliminal|Suppression|American people|allengation|appreciate|hate|work more with|soft|Weak|together|Worried|Dumb|dumb|Irrational|Hate|rights for criminals|partisan|Partisan|grift|Entitlement|hearts|disbanded|overhaul|blood|traitors|protecting criminals|Propaganda|Theifs|Themselves|Selfish|Rewrite|Unamerican|Trustworthiness|treason|Treason|corruption|bankruptcy|leadership|Thieved|Too arrogant|lying|integrity|hte republicans|folk|cheat on their wives|devil|Not for Americans|Cover ups|Criminals|Lying|Unconstitutional|meanness"), "Valence", issue_coded),
         issue_coded = ifelse(str_detect(issues_open, "Big  government|corporations|deregulation|Government control|limited government|Cannabis|Change|Freedom|Government|Left wing|Leisure|Liberal|Marijuana|Marijuana|Policies|Domestic|speech|Help|Meat|People|leaks"), "Other", issue_coded),
         issue_coded = ifelse(str_detect(issues_open, "None|Nothing|Slaves|Beer and money|none|image|nonr|I don\'t know|Unsure|Allies|Not sure|No central platform|Idk|I don\'t|Pay attention|Not a clue at|All|No more thoughts|none come to mind|Had|John|I need government assistance|The government|I do not really think about it|Same as above|Same|Don\'t|The lack of votes we are getting as Wilkes clone|American built on|That about sums it up for me|Don\'t know"), "No issue", issue_coded)) |> 
  left_join(df_er_participants)



## State politicians 
df_er_state_open <- 
  df_er_clean |> 
  dplyr::select(ResponseId,  Q11.1_1:Q11.1_3,Q12.1_1:Q12.1_3) |> 
  pivot_longer(cols = -ResponseId,
               names_to = "question_open",
               values_to = "politicians_state_open") |> 
  separate(question_open, into = c("question_stem", "number"), sep = "_", remove = FALSE) |> 
  mutate(party = ifelse(str_detect(question_open, "5"), "dem", "rep")) |> 
  group_by(ResponseId, question_stem) |> 
  mutate(number = as.numeric(number),
         politicians_state_open_count = sum(!is.na(politicians_state_open))) |> 
  ungroup() |> 
  #unite("party_open", c(party_open, number), sep = "_", remove = FALSE) |> 
  dplyr::select(-question_stem) |> 
  left_join(df_er_participants)


#rm(df_dime_merge)

