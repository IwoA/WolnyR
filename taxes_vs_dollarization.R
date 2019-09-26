library(rdbnomics)
library(dplyr)
library(ggplot2)

## Rozdział‚ 4 - korelacja dolaryzacji [Reinhard, Rogoff 2003] i tax revenues

#df_api i df to to samo
df_api <- rdb_by_api_link("https://api.db.nomics.world/v22/series/IMF/GFSR?limit=1000&offset=0&q=government%20revenue&observations=1&align_periods=1&dimensions=%7B%22FREQ%22%3A%5B%22A%22%5D%2C%22REF_SECTOR%22%3A%5B%22S13%22%5D%2C%22UNIT_MEASURE%22%3A%5B%22XDC_R_B1GQ%22%5D%2C%22CLASSIFICATION%22%3A%5B%22W0_S1_G11%22%5D%7D") %>%
  filter(!is.na(value))

df <- rdb('IMF', 'GFSR', mask = 'A..S13.XDC_R_B1GQ.W0_S1_G11') %>% filter(!is.na(value))

# kopia danych na wszelki wypadek
#write.csv(df, "taxes.csv")

df$REF_AREA <- as.factor(df$REF_AREA)
# levels(df$REF_AREA)
taxes <- df %>% select(REF_AREA, period, value) %>% filter(period < "2002-01-01" & period >= "1980-01-01")
#filter(taxes, value == min(taxes$value))

# ggplot(taxes, aes(x = period, y = value, color=REF_AREA)) + geom_line() + ggtitle("Dochody podatkowe jako % PKB")

taxes_mean <- taxes %>% group_by(REF_AREA) %>% summarise(srednia = mean(value))

# Zmiana nazw krajów ze skrótów na pełne
taxes_mean$REF_AREA <- countrycode::countrycode(taxes_mean$REF_AREA, "iso2c", "country.name", nomatch = NULL)
# filter(codelist_panel, iso2c == "MX")[1,1] #sposób na sprawdzanie jaki kraj

# wyciąganie tabeli z pdfa 
library(tabulizer)


strony <- NULL
for (i in 57:59) {  # Na tych stronach są interesujące tabele
  strony[[i-56]] <- as.data.frame(extract_tables("addicted_to_dollars.pdf", pages = i), stringsAsFactors = FALSE)
  strony[[i-56]] <- strony[[i-56]][-c(1:4),]
  colnames(strony[[i-56]]) <- c("geo", "fx_deposits", "debt_fx/ext_debt", "composite")
}

tmp <- rbind(strony[[1]], strony[[2]], strony[[3]])
tmp$geo <- gsub("[0-9]", "", tmp$geo)
tmp$geo <- sub(" ", "", tmp$geo)
dollarization <- select(tmp, "geo", "fx_deposits")
dol_tax <- left_join(dollarization, taxes_mean, by = c("geo" = "REF_AREA"))
dol_tax$geo <- as.factor(dol_tax$geo)
dol_tax$fx_deposits <- as.numeric(dol_tax$fx_deposits)
dol_tax_graph <- filter(dol_tax, !(is.na(srednia)))

ggplot(dol_tax_graph, aes(x = fx_deposits, y = srednia)) + geom_point() + 
  geom_text(label = dol_tax_graph$geo, vjust = -.3, hjust = -.1) +
 # geom_smooth(method = "lm") +
  scale_x_continuous(limits = c(0,9), breaks = c(0,2,4,6,8)) +
  scale_y_continuous(limits = c(10,35), breaks = c(10, 15, 20, 25, 30)) +
  theme_minimal() +
  labs(x = "Indeks depozytów walut obcych**", y = "Przeciętne dochody podatkowe jako %PKB")
