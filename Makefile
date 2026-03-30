# $Id: Makefile,v 1.80 2018/07/20 11:42:16 deraugla Exp $

OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
OCOPTS=
CAMLP5_OBJS=ploc.cmo versdep.cmo plexing.cmo gramext.cmo fstream.cmo grammar.cmo plexer.cmo
ROBOBJS=rob_position.cmo rob_misc.cmo rob_object.cmo rob_monster.cmo rob_path.cmo rob_action.cmo robot.cmo
OBJS=$(CAMLP5_OBJS) $(ROBOBJS) ustring.cmo efield.cmo rfield.cmo imisc.cmo imonster.cmo object.cmo level.cmo translate.cmo curses.cmo rogbotio.cmo init.cmo dialogue.cmo misc.cmo finish.cmo monster.cmo attack.cmo move.cmo use.cmo main.cmo 
LIBS=unix.cma
ROGBOT_OBJS=$(CAMLP5_OBJS) $(ROBOBJS) rogbot.cmo
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
	rm -f *.cm[oix] *.ppo *.defo *.o
	rm -f rogue rogbot *.opt *.out

depend:
	-ocamldep *.ml *.mli > .depend.new
	mv .depend .depend.old
	mv .depend.new .depend

rogue.out: $(EXT) $(OBJS)
	$(OCAMLC) -g $(LIBS) $(OBJS) -o rogue.out

rogue.opt: $(EXT) $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(LIBS:.cma=.cmxa) $(OBJS:.cmo=.cmx) -o $@

rogbot.out: $(ROGBOT_OBJS)
	$(OCAMLC) -g $(LIBS) $(ROGBOT_OBJS) -o $@

rogbot.opt: $(ROGBOT_OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(LIBS:.cma=.cmxa) $(ROGBOT_OBJS:.cmo=.cmx) -o $@

$(OBJS) $(OBJS:.cmo=.cmx): $(EXT)

.SUFFIXES: .ml .mli .cmo .cmx .cmi .def .defo

.ml.cmo:
	$(OCAMLC) -g $(OCOPOTS) -c $*.ml

.ml.cmx:
	$(OCAMLOPT) -g $(OCOPOTS) -c $*.ml

.mli.cmi:
	$(OCAMLC) -g $(OCOPOTS) -c $*.mli

.def.defo:
	@touch $@

include .depend
