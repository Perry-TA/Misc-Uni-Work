
# _______________________________________________________________
# MAMMALS AND EVOLUTION PRACTICAL 1: PHYLOGENETICS
# _______________________________________________________________
# January 2014
# Chris Faulkes and Steven Le Comber
# _______________________________________________________________

# _______________________________________________________________
# PART TWO: Amniote inter-relationships
# _______________________________________________________________
#
#
# clear workspace and load library
rm(list=ls())
library(ade4)

tree.only<-"((((((bird,mammal)A,crocodile)B,turtle)C,lizard)D,lungfish)E,salamander,ancestor)F;"
tree.trait01<-"((((((bird_1,mammal_1)A,crocodile_1)B,turtle_1)C,lizard_1)D,lungfish_0)E,salamander_0,ancestor_0)F;"
tree.trait02<-"((((((bird_1,mammal_1)A,crocodile_1)B,turtle_1)C,lizard_1)D,lungfish_0)E,salamander_0,ancestor_0)F;"
tree.trait03<-"((((((bird_0,mammal_1)A,crocodile_1)B,turtle_1)C,lizard_1)D,lungfish_0)E,salamander_0,ancestor_0)F;"
tree.trait04<-"((((((bird_1,mammal_1)A,crocodile_0)B,turtle_0)C,lizard_0)D,lungfish_1)E,salamander_0,ancestor_0)F;"
tree.trait05<-"((((((bird_1,mammal_1)A,crocodile_0)B,turtle_0)C,lizard_0)D,lungfish_N)E,salamander_N,ancestor_0)F;"
tree.trait06<-"((((((bird_1,mammal_0)A,crocodile_1)B,turtle_1)C,lizard_1)D,lungfish_1)E,salamander_0,ancestor_0)F;"
tree.trait07<-"((((((bird_N,mammal_1)A,crocodile_1)B,turtle_N)C,lizard_0)D,lungfish_0)E,salamander_0,ancestor_0)F;"
tree.trait08<-"((((((bird_1,mammal_0)A,crocodile_1)B,turtle_0)C,lizard_0)D,lungfish_N)E,salamander_0,ancestor_0)F;"
tree.trait09<-"((((((bird_1,mammal_1)A,crocodile_1)B,turtle_1)C,lizard_0)D,lungfish_0)E,salamander_0,ancestor_0)F;"
tree.trait10<-"((((((bird_1,mammal_2)A,crocodile_0)B,turtle_0)C,lizard_0)D,lungfish_0)E,salamander_0,ancestor_0)F;"
tree.trait11<-"((((((bird_2,mammal_2)A,crocodile_2)B,turtle_2)C,lizard_1)D,lungfish_N)E,salamander_N,ancestor_0)F;"
tree.trait12<-"((((((bird_1,mammal_0)A,crocodile_1)B,turtle_N)C,lizard_0)D,lungfish_0)E,salamander_0,ancestor_0)F;"
tree.trait13<-"((((((bird_N,mammal_0)A,crocodile_1)B,turtle_1)C,lizard_1)D,lungfish_0)E,salamander_0,ancestor_0)F;"
tree.trait14<-"((((((bird_1,mammal_N)A,crocodile_1)B,turtle_0)C,lizard_0)D,lungfish_0)E,salamander_0,ancestor_0)F;"
tree.trait15<-"((((((bird_1,mammal_1)A,crocodile_1)B,turtle_0)C,lizard_0)D,lungfish_0)E,salamander_0,ancestor_0)F;"
tree.trait16<-"((((((bird_0,mammal_0)A,crocodile_1)B,turtle_1)C,lizard_1)D,lungfish_1)E,salamander_0,ancestor_0)F;"
tree.trait17<-"((((((bird_1,mammal_1)A,crocodile_0)B,turtle_1)C,lizard_0)D,lungfish_0)E,salamander_0,ancestor_0)F;"
tree.trait18<-"((((((bird_0,mammal_1)A,crocodile_1)B,turtle_0)C,lizard_0)D,lungfish_0)E,salamander_0,ancestor_0)F;"
tree.trait19<-"((((((bird_0,mammal_1)A,crocodile_1)B,turtle_0)C,lizard_1)D,lungfish_0)E,salamander_1,ancestor_0)F;"
tree.trait20<-"((((((bird_1,mammal_1)A,crocodile_0)B,turtle_0)C,lizard_0)D,lungfish_N)E,salamander_0,ancestor_0)F;"
#
trees<-rbind(tree.trait01,tree.trait02,tree.trait03,tree.trait04,tree.trait05,tree.trait06,tree.trait06,tree.trait08,tree.trait09,tree.trait10,
             tree.trait11,tree.trait12,tree.trait13,tree.trait14,tree.trait15,tree.trait16,tree.trait17,tree.trait18,tree.trait19,tree.trait20)
#
#	Taxon			1	2	3	4	5	6	7	8	9	10	11	12	13	14	15	16	17	18	19	20
#	0	ancestor	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0
#	1	lungfish	0	0	0	1	N	1	0	N	0	0	N	0	0	0	0	1	0	0	0	N
#	2	salamander	0	0	0	0	N	0	0	0	0	0	N	0	0	0	0	0	0	0	1	0
#	3	lizard		1	1	1	0	0	1	0	0	0	2	1	0	1	0	0	1	0	0	1	0
#	4	turtle		1	1	1	0	0	1	N	0	1	0	2	N	1	0	0	1	1	0	0	0
#	5	crocodile	1	1	1	0	0	1	1	1	1	0	2	1	1	1	1	1	0	1	1	0
#	6	bird		1	1	0	1	1	1	N	1	1	1	2	1	N	1	1	0	1	0	0	1
#	7	mammal		1	1	1	1	1	0	1	0	1	2	2	0	0	N	1	0	1	1	1	1
#
#	0 = not present
#	1 = present or advanced condition present
#	2 = supra-advanced condition
#	N = condition unknown or inapplicable
#
#
# Key to traits
#	1	Cleidoic egg
#	2	Atlas/axis complex
#	3	Hand with five fingers
#	4	Aortic trunk with valves
#	5	High metabolic rate
#	6	Single occipital
#	7	Thecodont teeth
#	8	Single eustachian tube
#	9	Single erect penis
#	10	Epiphyses
#	11	Calcareous egg shells
#	12	Uncinate process on ribs
#	13	Hooked fifth metatarsal
#	14	Mandibular fenestra
#	15	Large premaxilla
#	16	Postorbital
#	17	Median vomer
#	18	Secondary palate
#	19	Teeth marginal
#	20	In-turned femoral head
#
# _______________________________________________________________
# STOP FIRST SET OF COMMANDS HERE
# _______________________________________________________________





# _______________________________________________________________
#	Question 10 Draw the tree
# _______________________________________________________________
par(mfrow=c(1,1),mar=c(1,1,1,1))
cgf.phy <- newick2phylog(tree.only)
plot(cgf.phy, clabel.l = 1, clabel.n = 1, f = 0.8,cleaves=1,cnodes=1,sub="initial tree",possub="topleft")
#
#
#
#
# end question 10 here





# _______________________________________________________________
#	Question 11 Redraw the tree as a radial phylogeny
# _______________________________________________________________
par(mfrow=c(1,1),mar=c(1,1,1,1))
cgf.phy <- newick2phylog(tree.only)
radial.phylog(cgf.phy)
#
#
#
#
# end question 11 here


# _______________________________________________________________
#	Question 12 Compare traits 6 and 16
# _______________________________________________________________
dev.new(width=16, height=4)
par(mfrow=c(1,2),mar=c(1,1,1,1))
type.size=1
cgf.phy <- newick2phylog(trees[6,])
plot(cgf.phy, clabel.l = type.size, clabel.n = type.size, f = 0.8,cleaves=1,cnodes=1,sub="Trait 6",possub="topleft",csub=type.size)
cgf.phy <- newick2phylog(trees[16,])
plot(cgf.phy, clabel.l = type.size, clabel.n = type.size, f = 0.8,cleaves=1,cnodes=1,sub="Trait 16",possub="topleft",csub=type.size)
#
#
#
#
# end question 12 here






# _______________________________________________________________
#	Question 13 Compare traits 1, 2 and 3
# _______________________________________________________________
dev.new(width=16, height=6)
par(mfrow=c(1,3),mar=c(2,2,2,2),oma=c(5,5,5,5))
type.size=1.6
cgf.phy <- newick2phylog(trees[1,])
plot(cgf.phy, clabel.l = type.size, clabel.n = type.size, f = 0.8,cleaves=1,cnodes=1,sub="Trait 1",possub="topleft",csub=type.size)
cgf.phy <- newick2phylog(trees[2,])
plot(cgf.phy, clabel.l = type.size, clabel.n = type.size, f = 0.8,cleaves=1,cnodes=1,sub="Trait 2",possub="topleft",csub=type.size)
cgf.phy <- newick2phylog(trees[3,])
plot(cgf.phy, clabel.l = type.size, clabel.n = type.size, f = 0.8,cleaves=1,cnodes=1,sub="Trait 3",possub="topleft",csub=type.size)
#
#
#
#
# end question 13 here





# _______________________________________________________________
#	Question 14 Compare traits 5 and 20
# _______________________________________________________________
dev.new(width=16, height=4)
par(mfrow=c(1,2),mar=c(1,1,1,1))
type.size=1
cgf.phy <- newick2phylog(trees[5,])
plot(cgf.phy, clabel.l = type.size, clabel.n = type.size, f = 0.8,cleaves=1,cnodes=1,sub="Trait 5",possub="topleft",csub=type.size)
cgf.phy <- newick2phylog(trees[20,])
plot(cgf.phy, clabel.l = type.size, clabel.n = type.size, f = 0.8,cleaves=1,cnodes=1,sub="Trait 20",possub="topleft",csub=type.size)
#
#
#
#
# end question 14 here


# ends









