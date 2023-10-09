library(tidyverse) # manipulação e visualizacao dos dados
library(netmeta)   # meta analises em rede
library(readxl)    # ler arquivo do excel
library(writexl)   # salvar excel
library(patchwork) # to join plots
library(cowplot) 

# Pareadas subgrupos ----

dfsubgrupos <- read_excel("data/subgruposppt.xlsx")

dfsubgrupos <- dfsubgrupos %>% 
  rename(IC95LL = `IC95-L`,
         IC95UL = `IC95-U`,
         inconsistency = `I² (%)`) |> 
  separate(k, c("k", "nested")) |> 
  mutate(tau2 = round(dfsubgrupos$tau2, digits = 1),
         moderator = as.factor(moderator),
         category = as.factor(category),
         category = fct_reorder(category, k),
         k = as.numeric(k),
         nested = case_when(nested == "" ~ ", nested"),
         nested = replace_na(nested,""),
         outline = 100)


dfsubgrupos$moderator <-
  factor(
    dfsubgrupos$moderator,
    levels =  c(
      "Mice",
      "Rat",
      "Sex",
      "Strain",
      "Stress",
      "Light cycle",
      "All TCA",
      "All SSRI",
      "All SNRI",
      "All MAOI",
      "MAOI",
      "All NDRI",
      "NDRI",
      "All TeCA",
      "Route of administration",
      "FST Protocol",
      "Analysis method",
      "Tests before FST"))

dfsubgrupos$category <-
  factor(
    dfsubgrupos$category,
    levels =  c("Mice",
                "Male",
                "Female",
                "Both sexes",
                "Swiss",
                "CD-1",
                "C57BL",
                "ddY",
                "BALB",
                "LACA",
                "OF1",
                "NMRI",
                "Sabra",
                "BKTO",
                "DBA/2",
                "B6SJL (R406W)",
                "Stress",
                "No stress",
                "Rat",
                "Wistar",
                "Sprague Dawley",
                "Long Evans",
                "Flinders sensitive",
                "CD-COBS",
                "Wistar kyoto",
                "Flinders resistant",
                "12/12 normal",
                "12/12",
                "Natural",
                "12/12 reverse",
                "11/14",
                "All TCA",
                "imipramine",
                "desipramine",
                "amitriptilyne",
                "clomipramine",
                "nortriptyline",
                "All SSRI",
                "fluoxetine",
                "sertraline",
                "paroxetine",
                "escitalopram",
                "citalopram",
                "fluvoxamine",
                "All SNRI",
                "venlafaxine",
                "tramadol",
                "desvenlafaxine",
                "reboxetine",
                "sibutramine",
                "All MAOI",
                "selegiline",
                "moclobemide",
                "bupropion",
                "All TeCA",
                "maprotiline",
                "mianserin",
                "amoxapine",
                "Intraperitoneal",
                "Oral",
                "Gavage",
                "Subcutaneous",
                "Microinjection",
                "Oral (food)",
                "Intranasal",
                "T6’ + S4’",
                "T6’",
                "PT15’ + T6’ + S4’",
                "PT15’ + T6’",
                "PT15’ + T5’",
                "T5’",
                "PT13’ + T6’",
                "PT5’ + T5’",
                "T5’ + S4’",
                "T7’ + S6’",
                "T6’ + S5’",
                "T9’",
                "T10’",
                "PT15’ + T?",
                "T15’ + S5’",
                "Video analysis",
                "Manual",
                "No",
                "Yes",
                "T15’",
                "PT?’ + T6’ + S4’",
                "PT15’ + T6’ + S5’",
                "No info"))

est_c <- dfsubgrupos %>%
  filter(species == "Mice",
         type == "Intervention") %>%
  ggplot(aes(
    x = fct_reorder(category, k),
    y = GES,
    ymin = ifelse(IC95LL > -2, IC95LL, -2),
    ymax = ifelse(IC95UL < 10, IC95UL, 10),
    color = "#bb9825"
  )) +
  geom_rect(fill = "white",xmin = 0,xmax = Inf,
            ymin = -Inf,ymax = 0, color = "white") +
  geom_rect(fill = "grey100",xmin = 0,xmax = Inf,
            ymin = 0.01,ymax = .19, color = "grey100") +
  geom_rect(fill = "grey98",xmin = 0,xmax = Inf,
            ymin = 0.2,ymax = .49, color = "grey98") +
  geom_rect(fill = "grey96",xmin = 0,xmax = Inf,
            ymin = 0.5,ymax = .79, color = "grey96") +
  geom_rect(fill = "grey94",xmin = 0,xmax = Inf,
            ymin = 0.8,ymax = 1.19, color = "grey94") +
  geom_rect(fill = "grey92",xmin = 0,xmax = Inf,
            ymin = 1.2,ymax = 1.99, color = "grey92") +
  geom_rect(fill = "grey90",xmin = 0,xmax = Inf,
            ymin = 2,ymax = Inf, color = "grey90") +
  geom_pointrange() +
  scale_y_continuous(limits = c(-2, 12)) +
  labs(x = "", y = "Global Effect Size") +
  scale_colour_manual(values = "#bb9825") +
  geom_hline(yintercept = 0, lty = 2, linewidth = .2) +
  facet_grid(fct_inorder(moderator) ~ ., scales = "free", space = "free") +
  geom_text(
    aes(label = paste(
      "k = ",
      k,
      fct_reorder(nested, k),
      sep = ""
    )),
    y = Inf - 1,
    color = "black",
    size = 2.5,
    hjust = 1
  ) +
  coord_flip() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.title.x = element_text(size = 8, color = "black", face = "bold", family = "Playfair"),
    axis.text.x = element_text(size = 8, color = "black"),
    axis.text.y = element_text(size = 8, color = "black"),
    axis.ticks.length=unit(0.1,"cm"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(linewidth = .2, color = "black"),
    axis.line.x = element_line(linewidth = .2, colour = "black", linetype = 1),
    plot.background = element_rect(colour = "white"), 
  )

het_c <- dfsubgrupos %>%
  filter(species == "Mice",
         type == "Intervention") %>%
  ggplot(aes(
    x = fct_reorder(category, k)
  )) +
  geom_bar(aes(y = inconsistency, fill = "inconsistency"), stat = "identity") +
  geom_bar(aes(y = outline, fill = "outline"), stat = "identity", position = "identity", alpha = 0, linewidth = .1, color = "black") +
  scale_y_continuous(limits = c(0, 120), breaks = c(0, 50, 100)) +
  labs(x = "", y = "I²(%) tau²") +
  scale_fill_manual(values = c("inconsistency" = "#bb9825", "outline" = "black"), guide = "none") +
  geom_text(
    aes(label = tau2),
    y = 100,
    color = "black",
    size = 2.5, 
    hjust = -.1
  ) +
  facet_grid(fct_inorder(moderator) ~ ., scales = "free", space = "free") +
  coord_flip() +
  theme_void() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.title = element_text(size = 8, color = "black", face = "bold", vjust = -.1),
    axis.text.x = element_text(size = 8, color = "black", vjust = -.3),
    axis.ticks.length=unit(0.1,"cm"),
    axis.ticks.x = element_line(linewidth = .2, color = "black"),
    axis.line.x = element_line(linewidth = .2, colour = "black", linetype = 1),
    plot.margin = margin(-5, 0, 0, 0)
  )


label_c <- 
  ggplot() +
  annotate(
    "text", label = paste(c("Compound", "Via")),
    x = c(1,1), y = c(32.9, 3.5),     
    size = 3,
    family = "sans",
    hjust = 0,
    vjust = -1,
    fontface = "bold",
    colour = "black"
  ) +
  scale_y_continuous(limits = c(1, 33), position = "right") +
  scale_x_continuous(limits = c(1, 3)) +
  theme_void()


label_direction <- 
  ggplot() +
  annotate(
    "text", label = paste(c("Favours to","control","antidepressants")),
    x = c(0,-0.5,0.5), y = c(2,1.5,1.5),     
    size = 3,
    family = "sans",
    hjust = c(0.5,1,0),
    vjust = 0,
    fontface = "bold",
    colour = "black"
  ) +
  geom_segment(aes(x=-.5, y = 1, xend = -1.5, yend=1), arrow = arrow(length=unit(0.1, 'cm'))) +
  geom_segment(aes(x=.5, y =1, xend = 1.5, yend=1), arrow = arrow(length=unit(0.1, 'cm'))) +
  scale_y_continuous(limits = c(1, 5)) +
  scale_x_continuous(limits = c(-2, 12)) +
  theme_void() +
  theme(plot.margin = margin(t = 0,  # Top margin
                       r = 0,  # Right margin
                       b = 0,  # Bottom margin
                       l = 0))

layout <-"
#DDDDDD##
CAAAAAABB
CAAAAAABB
CAAAAAABB
CAAAAAABB
"

plot_int_c <- est_c + het_c + label_c + label_direction + plot_layout(design = layout)

save_plot(filename = "plot_int_c.png",
          plot = plot_int_c,
          dpi = 600,
          path = "figure",
          base_height = 6, base_width = 8)


est_r <- dfsubgrupos %>%
  filter(species == "Rat",
         type == "Intervention") %>%
  ggplot(aes(
    x = fct_reorder(category, k),
    y = GES,
    ymin = ifelse(IC95LL > -2, IC95LL, -2),
    ymax = ifelse(IC95UL < 10, IC95UL, 10),
    color = "#0c73a3"
  )) +
  geom_rect(fill = "white",xmin = 0,xmax = Inf,
            ymin = -Inf,ymax = 0, color = "white") +
  geom_rect(fill = "grey100",xmin = 0,xmax = Inf,
            ymin = 0.01,ymax = .19, color = "grey100") +
  geom_rect(fill = "grey98",xmin = 0,xmax = Inf,
            ymin = 0.2,ymax = .49, color = "grey98") +
  geom_rect(fill = "grey96",xmin = 0,xmax = Inf,
            ymin = 0.5,ymax = .79, color = "grey96") +
  geom_rect(fill = "grey94",xmin = 0,xmax = Inf,
            ymin = 0.8,ymax = 1.19, color = "grey94") +
  geom_rect(fill = "grey92",xmin = 0,xmax = Inf,
            ymin = 1.2,ymax = 1.99, color = "grey92") +
  geom_rect(fill = "grey90",xmin = 0,xmax = Inf,
            ymin = 2,ymax = Inf, color = "grey90") +
  geom_pointrange() +
  scale_y_continuous(limits = c(-2, 12)) +
  labs(x = "", y = "Global Effect Size") +
  scale_colour_manual(values = "#0c73a3") +
  geom_hline(yintercept = 0, lty = 2, linewidth = .2) +
  facet_grid(fct_inorder(moderator) ~ ., scales = "free", space = "free") +
  geom_text(
    aes(label = paste(
      "k = ",
      k,
      fct_reorder(nested, k),
      sep = ""
    )),
    y = Inf - 1,
    color = "black",
    size = 2.5,
    hjust = 1
  ) +
  coord_flip() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.title.x = element_text(size = 8, color = "black", face = "bold"),
    axis.text.x = element_text(size = 8, color = "black"),
    axis.text.y = element_text(size = 8, color = "black"),
    axis.ticks.length=unit(0.1,"cm"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(linewidth = .2, color = "black"),
    axis.line.x = element_line(linewidth = .2, colour = "black", linetype = 1)
  )

het_r <- dfsubgrupos %>%
  filter(species == "Rat",
         type == "Intervention") %>%
  ggplot(aes(
    x = fct_reorder(category, k)
  )) +
  geom_bar(aes(y = inconsistency, fill = "inconsistency"), stat = "identity") +
  geom_bar(aes(y = outline, fill = "outline"), stat = "identity", position = "identity", alpha = 0, linewidth = .1, color = "black") +
  scale_y_continuous(limits = c(0, 120), breaks = c(0, 50, 100)) +
  labs(x = "", y = "I²(%) tau²") +
  scale_fill_manual(values = c("inconsistency" = "#0c73a3", "outline" = "black"), guide = "none") +
  geom_text(
    aes(label = tau2),
    y = 100,
    color = "black",
    size = 2.5,
    hjust = -.1
  ) +
  facet_grid(fct_inorder(moderator) ~ ., scales = "free", space = "free") +
  coord_flip() +
  theme_void() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.title = element_text(size = 8, color = "black", face = "bold", vjust = -.1),
    axis.text.x = element_text(size = 8, color = "black", vjust = -.3),
    axis.ticks.length=unit(0.1,"cm"),
    axis.ticks.x = element_line(linewidth = .2, color = "black"),
    axis.line.x = element_line(linewidth = .2, colour = "black", linetype = 1),
    plot.margin = margin(-5, 0, 0, 0)
  )


label_r <- 
  ggplot() +
  annotate(
    "text", label = paste(c("Compound", "Via")),
    x = c(1,1), y = c(32.9, 5.6),  
    size = 3,
    family = "sans",
    hjust = 0,
    vjust = -1,
    fontface = "bold",
    colour = "black"
  ) +
  scale_y_continuous(limits = c(1, 33), position = "right") +
  scale_x_continuous(limits = c(1, 3)) +
  theme_void() +
  theme(plot.margin = margin(t = 0,  # Top margin
                           r = 0,  # Right margin
                           b = 0,  # Bottom margin
                           l = 0))


label_direction <- 
  ggplot() +
  annotate(
    "text", label = paste(c("Favours to","control","antidepressants")),
    x = c(0,-0.5,0.5), y = c(2,1.5,1.5),     
    size = 3,
    family = "sans",
    hjust = c(0.5,1,0),
    vjust = 0,
    fontface = "bold",
    colour = "black"
  ) +
  geom_segment(aes(x=-.5, y = 1, xend = -1.5, yend=1), arrow = arrow(length=unit(0.1, 'cm'))) +
  geom_segment(aes(x=.5, y =1, xend = 1.5, yend=1), arrow = arrow(length=unit(0.1, 'cm'))) +
  scale_y_continuous(limits = c(1, 5)) +
  scale_x_continuous(limits = c(-2, 12)) +
  theme_void()

layout <-"
#DDDDDD##
CAAAAAABB
CAAAAAABB
CAAAAAABB
CAAAAAABB
"

plot_int_r <- est_r + het_r + label_r + label_direction + plot_layout(design = layout)

save_plot(filename = "plot_int_r.png",
          plot = plot_int_r,
          dpi = 600,
          path = "figure",
          base_height = 6, base_width = 8)


# Em rede camundongo -----


df_c <- read_excel("data/df_c.xlsx") # carregar df editado

df_c <- df_c %>% 
  mutate(atd_type = as.factor(atd_type),
         atd_type2 = as.factor(atd_type2),
         comparator = as.factor(comparator))


# Calcular tamanho de efeito em SDM hedges g 

Efeito_c <- pairwise(list(as.character(comparator), as.character(atd_type), as.character(atd_type2)),
                     n = list(ctr_n_corr, atd_n_round, atd_n_round2),
                     mean = list(ctr_mean, atd_mean, atd_mean2),
                     sd = list(ctr_sd, atd_sd, atd_sd2),
                     data = df_c, studlab = label, sm = "SMD")

Efeito_c

# ver quantos braços cada estudo

as.matrix(table(Efeito_c$label))

# meta-analise em rede

nma_c <- netmeta(
  data = Efeito_c,
  studlab = label,
  TE = TE,
  seTE = seTE,
  treat1 = treat1,
  treat2 = treat2,
  sm = "SMD",
  method.tau = "REML",
  random = TRUE,
  fixed = FALSE,
  details.chkmultiarm = TRUE,
  tol.multiarm = .5,
  reference.group = "vehicle",
  sep.trts = " vs ",
  small = "good"
)

nma_c


# calcular a inconsistência total com base no modelo completo de efeitos aleatórios de interação de design por tratamento

decomp.design(nma_c)

# plot: definir rótulos e entrada do tamanho da amostra

Efeito_c %>% 
  group_by(atd_type) %>% 
  summarise(sum(atd_n_round)) # acessar total n de acada tratamento

Efeito_c %>% 
  group_by(atd_type2) %>% 
  summarise(sum(atd_n_round2)) # acessar total n de acada tratamento


Efeito_c %>% 
  group_by(comparator) %>% 
  summarise(sum(ctr_n_corr)) # acessar total n de acada tratamento

pointsizes <- c(51, 8, 10, 82, 38, 114) # add n de cada tratamento na ordem do rotulo 

sqrtpointsizes <- sqrt(pointsizes / 2)

# plotar e salvar rede

png("figure/rede_c.png", height = 2000, width = 2000, res = 300)

netgraph(
  nma_c,
  seq = nma_c$trts,
  labels = nma_c$trts,
  points = TRUE,
  cex = 1.5,
  cex.points = sqrtpointsizes,
  multiarm = FALSE,
  thickness = "number.of.studies",
  lwd = 2,
  plastic = FALSE,
  col = "black",
  col.points = "#bb9825",
  start = "circle",
  iterate = FALSE
)

dev.off()

d.evidence <- direct.evidence.plot(nma_c, random = TRUE)


plot(d.evidence)


# rank NMA estimates using P-scores (R?cker & Schwarzer, 2015) - aqui o parametro de smallvalues é inverso (pq é referente aos valores obtidos no objeto nma_c)

png("Fig/ranking_c.png", height = 400, width = 600)

randomnetrank <- netrank(nma_c, small.values = "good")

randomnetrank

plot(
  name = "Ranqueamento",
  randomnetrank,
  random = TRUE,
  col = "black",
  low = "#0c73a3",
  high = "#82c236",
  legend = TRUE,
  angle = 45,
  hjust.x = 1,
  vjust.x = 1,
  hjust.y = 1,
  vjust.y = 0,
  nchar.trts = 12,
  main.face = "bold",
  axis.size = 12
)

dev.off()

# Forestplot com todos tratammentos versus controle - aqui o parametro de smallvalues é inverso

png("figure/forest_nma_c.png", height = 800, width = 2000, res = 300)

forest(nma_c,
       leftcols = c("studlab", "k", "pscore"),
       small.values = "good",
       sortva = -Pscore,
       reference.group = "vehicle",
       drop.reference.group = TRUE,
       equal.size = FALSE,
       label.left = "Favours antidepressant",
       label.right = "Favours vehicle",
       smlab = paste("Duration of\n immobility"))

dev.off()

# TABELA COM COMPARACOES
# NEGATIVO em favor da coluna, POSITIVO em favor da linha (triangulo inferior)

compable_c <- netleague(nma_c, 
                        bracket = "(",
                        digits = 2,
                        seq = randomnetrank)

compable_c

write_xlsx(compable_c$random, "data/tablenma_c.xlsx")


# Ver como comparações contribuiram para as outras

nma_contrib_c <- netcontrib(nma_c)

nma_contrib_c <- as.data.frame(nma_contrib_c$random)
nma_contrib_c$comp <- row.names(nma_contrib_c) # adicionar nome das linhas como coluna na copia
nma_contrib_c <- nma_contrib_c %>%
  select(comp, everything())

write_xlsx(nma_contrib_c, "data/nma_contrib_c.xlsx")
nma_contrib_c

# Teste de consistencia entre evidencia direta e indireta 
# using node-splitting (Dias et al., 2010) back-calculation method König et al. (2013)

randomsplitobject <- netsplit(nma_c, digits = 2)
randomsplitobject

png("Fig/split_c.png", height = 800, width = 600)

netsplit(nma_c) %>% forest(show = "with.direct",
                           label.left = "Favorece 1º tratamento",
                           label.right = "Favorece 2º tratamento")

dev.off()

# Em rede ratos ----- 

df_r <- read_excel("data/df_r.xlsx") # carregar df editado


# MUDAR NOME DOS TRATAMENTOS

df_r <- df_r %>% 
  mutate(atd_type = as.factor(atd_type),
         atd_type2 = as.factor(atd_type2),
         atd_type3 = as.factor(atd_type3),
         atd_type4 = as.factor(atd_type4),
         comparator = as.factor(comparator))


# Calcular tamanho de efeito em SDM hedges g


Efeito_r <- pairwise(list(as.character(comparator), as.character(atd_type), as.character(atd_type2), as.character(atd_type3), as.character(atd_type4)),
                     n = list(ctr_n_corr, atd_n_round, atd_n_round2, atd_n_round3, atd_n_round4),
                     mean = list(ctr_mean, atd_mean, atd_mean2, atd_mean3, atd_mean4),
                     sd = list(ctr_sd, atd_sd,atd_sd2, atd_sd3, atd_sd4),
                     data = df_r, studlab = label, sm = "SMD")

Efeito_r

# ver quantos braços cada estudo
as.matrix(table(Efeito_r$label))

# meta-analise em rede


nma_r <- netmeta(
  data = Efeito_r,
  studlab = label,
  TE = TE,
  seTE = seTE,
  treat1 = treat1,
  treat2 = treat2,
  sm = "SMD",
  method.tau = "REML",
  random = TRUE,
  fixed = FALSE,
  reference.group = "vehicle",
  sep.trts = " vs ",
  small = "good"
)

nma_r 


# calcular a inconsistência total com base no modelo completo de efeitos aleatórios de interação de design por tratamento

decomp.design(nma_r)

# plot: definir rótulos e entrada do tamanho da amostra


Efeito_r %>% 
  group_by(atd_type) %>% 
  summarise(sum(atd_n_round)) # acessar total n de cada tratamento

Efeito_r %>% 
  group_by(atd_type2) %>% 
  summarise(sum(atd_n_round2)) # acessar total n de acada tratamento

Efeito_r %>% 
  group_by(atd_type3) %>% 
  summarise(sum(atd_n_round3)) # acessar total n de acada tratamento

Efeito_r %>% 
  group_by(atd_type4) %>% 
  summarise(sum(atd_n_round4)) # acessar total n de acada tratamento

Efeito_r %>% 
  group_by(comparator) %>% 
  summarise(sum(ctr_n_corr)) # acessar total n de acada tratamento


pointsizes <- c(50, 30, 16, 50, 90, 97, 30, 198, 60, 258) # add n de cada tratamento na ordem do rotulo 

sqrtpointsizes <- sqrt(pointsizes / 2)

# plotar e salvar rede

png("figure/rede_r.png", height = 2000, width = 2000, res = 300)

netgraph(
  nma_r,
  seq = nma_r$trts,
  labels = nma_r$trts,
  points = TRUE,
  cex = 1.5,
  cex.points = sqrtpointsizes,
  multiarm = FALSE,
  thickness = "number.of.studies",
  plastic = FALSE,
  col = "black",
  col.points = "#0c73a3",
  start = "circle",
  iterate = FALSE,
  lwd = 2
)

dev.off()

# em 3d

netgraph(
  nma_r,
  labels = nma_r$trts,
  points = TRUE,
  cex = 1,
  cex.points = sqrtpointsizes,
  multiarm = FALSE,
  thickness = "number.of.studies",
  plastic = FALSE,
  col = "#bb9825",
  col.points = "#FE7700",
  start = "circle",
  iterate = FALSE,
  seq = nma_r$trts,
  dim = "3d"
)


# visualizacao de evi direta e indireta

d_evidence <- plot.netcomparison(nma_r, random = TRUE)

d_evidence


# rank NMA estimates using P-scores (R?cker & Schwarzer, 2015) 

randomnetrank <- netrank(nma_r, small.values = "good")
randomnetrank

png("figure/ranking_r.png", height = 1800, width = 2200, res = 300)

plot(
  name = "Ranqueamento",
  randomnetrank,
  random = TRUE,
  col = "black",
  low = "white",
  high = "#0c73a3",
  legend = TRUE,
  angle = 45,
  hjust.x = 1,
  vjust.x = 1,
  hjust.y = 1,
  vjust.y = 0,
  nchar.trts = 12,
  main.face = "bold",
  axis.size = 12
)

dev.off()

# Forestplot com todos tratammentos versus controle

png("figure/forest_nma_r.png",  height = 1050, width = 2030, res = 300)

forest(nma_r,
       leftcols = c("studlab", "k", "pscore"),
       small.values = "good",
       sortva = -Pscore,
       reference.group = "vehicle",
       drop.reference.group = TRUE,
       equal.size = FALSE,
       label.left = "Favours antidepressant",
       label.right = "Favours vehicle",
       smlab = paste("Duration of\n immobility"))

dev.off()

# TABELA COM COMPARACOES
# NEGATIVO em favor da coluna, POSITIVO em favor da linha (triangulo inferior)

compable_r <- netleague(nma_r, 
                        bracket = "(",
                        digits = 2,
                        seq = randomnetrank)

compable_r

write_xlsx(compable_r$random, "data/tablenma_r.xlsx")


# ver como comparações contribuiram  para as outras )

nma_contrib_r <- netcontrib(nma_r) #CONFERIR DEPOIS

nma_contrib_r <- as.data.frame(nma_contrib_r$random)
nma_contrib_r$comp <- row.names(nma_contrib_r) # adicionar nome das linhas como coluna na copia
nma_contrib_r <- nma_contrib_r %>%
  select(comp, everything())

write_xlsx(nma_contrib_r, "data/nma_contrib_r.xlsx")



# Teste de consistencia entre evidencia direta e indireta 
# using node-splitting (Dias et al., 2010) back-calculation method König et al. (2013)

randomsplitobject <- netsplit(nma_r)
randomsplitobject

png("Fig/split_r.png", height = 1250, width = 600)

netsplit(nma_r) %>% forest(show = "with.direct",
                           label.left = "Favorece 1º tratamento",
                           label.right = "Favorece 2º tratamento")

dev.off()
