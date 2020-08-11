#devtools::install_github("daijiang/rtrees")

#LOAD LIBRARIES
library(rtrees)
library(ape)



test_fish_list = tibble::tibble(
  species = c("Serrasalmus_geryi", "Careproctus_reinhardti", "Gobiomorphus_coxii", 
              "Periophthalmus_barbarus", "Prognichthys_glaphyrae", "Barathronus_bicolor", 
              "Knipowitschia_croatica", "Rhamphochromis_lucius", "Neolissochilus_tweediei", 
              "Haplochromis_nyanzae", "Astronesthes_micropogon", "Sanopus_reticulatus"),
  genus = c("Serrasalmus", "Careproctus", "Gobiomorphus", "Periophthalmus",
            "Prognichthys", "Barathronus", "Knipowitschia", "Rhamphochromis", 
            "Neolissochilus", "Haplochromis", "Astronesthes", "Sanopus"),
  family = c("Serrasalmidae", "Liparidae", "Eleotridae", "Gobiidae", 
             "Exocoetidae", "Aphyonidae", "Gobiidae", "Cichlidae", 
             "Cyprinidae", "Cichlidae", "Stomiidae", "Batrachoididae")
)



test_fish_list


sp_list_df(sp_list = c("Periophthalmus_barbarus", "Barathronus_bicolor"),
           taxon = "fish")

test_tree = get_tree(sp_list = test_fish_list,
                    tree = tree_fish, # either 
                    taxon = "fish", # or
                    scenario = "S1",
                    show_grafted = TRUE)


plot(ladderize(test_tree), no.margin = T)


tree1 <- read.nexus("tree1.nexus")
A <- ape::vcv.phylo(test_tree)
A <- A/max(A)

