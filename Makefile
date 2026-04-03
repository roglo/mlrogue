# $Id: Makefile,v 1.80 2018/07/20 11:42:16 deraugla Exp $

OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
OCOPTS=
GRAM_OBJS=gram/stream.cmo gram/ploc.cmo versdep.cmo plexing.cmo gramext.cmo fstream.cmo gram/grammar.cmo plexer.cmo
ROBOBJS=rob_position.cmo rob_misc.cmo rob_object.cmo rob_monster.cmo rob_path.cmo rob_action.cmo robot.cmo
OBJS=$(GRAM_OBJS) $(ROBOBJS) rogue_def.cmo keyboard_def.cmo ustring.cmo efield.cmo rfield.cmo imisc.cmo imonster.cmo object.cmo level.cmo translate.cmo curses.cmo rogbotio.cmo init.cmo dialogue.cmo misc.cmo finish.cmo monster.cmo attack.cmo move.cmo use.cmo main.cmo 
LIBS=unix.cma
ROGBOT_OBJS=$(GRAM_OBJS) $(ROBOBJS) rogbot.cmo
SRCS=$(OBJS:.cmo=.ml)
ROGBOT_SRCS=rogbot.ml
EXT=

all: opt

out: rogue.out rogbot.out
	if [ -f rogue ]; then mv rogue rogue.bak; fi
	cp rogue.out rogue
	cp rogbot.out rogbot

opt: rogue.opt rogbot.opt
	if [ -f rogue ]; then mv rogue rogue.bak; fi
	cp rogue.opt rogue
	cp rogbot.opt rogbot

clean:
	rm -f *.cm[oix] *.ppo *.defo *.o gram/*.cm[iox] gram/*.o
	rm -f rogue rogbot *.opt *.out

depend:
	-ocamldep -I gram *.ml *.mli gram/*.ml gram/*.mli > .depend.new
	mv .depend .depend.old
	mv .depend.new .depend

rogue.out: $(EXT) $(OBJS)
	$(OCAMLC) -g -I +unix $(LIBS) $(OBJS) -o rogue.out

rogue.opt: $(EXT) $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) -I +unix $(LIBS:.cma=.cmxa) $(OBJS:.cmo=.cmx) -o $@

rogbot.out: $(ROGBOT_OBJS)
	$(OCAMLC) -g -I +unix $(LIBS) $(ROGBOT_OBJS) -o $@

rogbot.opt: $(ROGBOT_OBJS:.cmo=.cmx)
	$(OCAMLOPT) -I +unix $(LIBS:.cma=.cmxa) $(ROGBOT_OBJS:.cmo=.cmx) -o $@

$(OBJS) $(OBJS:.cmo=.cmx): $(EXT)

.SUFFIXES: .ml .mli .cmo .cmx .cmi .def .defo

.ml.cmo:
	$(OCAMLC) -g $(OCOPOTS) -I gram -I +unix -c $*.ml

.ml.cmx:
	$(OCAMLOPT) -g $(OCOPOTS) -I gram -I +unix -c $*.ml

.mli.cmi:
	$(OCAMLC) -g $(OCOPOTS) -I gram -I +unix -c $*.mli

.def.defo:
	@touch $@

include .depend
