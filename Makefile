# $Id: Makefile,v 1.80 2018/07/20 11:42:16 deraugla Exp $

OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
OCOPTS=
ROBOBJS=rob_position.cmo rob_misc.cmo rob_object.cmo rob_monster.cmo rob_path.cmo rob_action.cmo robot.cmo
OBJS=$(ROBOBJS) ustring.cmo efield.cmo rfield.cmo imisc.cmo imonster.cmo object.cmo level.cmo translate.cmo curses.cmo rogbotio.cmo init.cmo dialogue.cmo misc.cmo finish.cmo monster.cmo attack.cmo move.cmo use.cmo main.cmo 
LIBS=unix.cma -I $$(camlp5 -where) gramlib.cma
ROGBOT_OBJS=$(ROBOBJS) rogbot.cmo
SRCS=$(OBJS:.cmo=.ml)
ROGBOT_SRCS=rogbot.ml
EXT=ext/pa_more.cmo ext/pa_if_match.cmo
CAMLP5=camlp5r
CAMLP5OPTS=-I ext

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
	rm -f *.cm[oix] *.ppo *.defo *.o ext/*.ppo ext/*.cm[oi]
	rm -f rogue rogbot *.opt *.out

depend: $(EXT)
	for i in *.mli $(SRCS) $(ROGBOT_SRCS); do \
	  $(CAMLP5) $(CAMLP5OPTS) pr_depend.cmo $$i; \
	done > .depend.new
	grep '#use' *.ml | sed -e 's/.ml:/.cmo:/' \
	  -e 's/#use "/ /' -e 's/def";/defo/' >> .depend.new
	grep '#use' *.ml | sed -e 's/.ml:/.cmx:/' \
	  -e 's/#use "/ /' -e 's/def";/defo/' >> .depend.new
	grep '#use' *.def | sed -e 's/.def:/.defo:/' \
	  -e 's/#use "/ /' -e 's/def";/defo/' >> .depend.new
	mv .depend .depend.old
	mv .depend.new .depend

i18n: $(EXT) ext/pr_transl.cmo
	@(cat $(SRCS) | egrep 'm_name =' | \
	sed -e 's/^.*m_name = "//' -e s'/".*$$//'; \
	cat $(SRCS) | egrep 't_title =' | \
	sed -e 's/^.*t_title = "//' -e s'/".*$$//'; \
	cat $(SRCS) | egrep 't_mess =' | \
	sed -e 's/^.*t_mess = "//' -e s'/".*$$//'; \
	cat $(SRCS) | grep 'o_title =' | \
	sed -e 's/^.*o_title = "//' -e s'/".*$$//'; \
	cat init.ml | sed -n -e '/value gems/,/|]/p' | \
	tail +2 | sed -e 's/\[|//' -e 's/|]//' -e 's/"//g' | \
	tr ';' '\n' | sed -e 's/ //g'; \
	cat init.ml | sed -n -e '/value wand_materials/,/|]/p' | \
	tail +2 | sed -e 's/\[|//' -e 's/|]//' -e 's/"//g' | \
	tr ';' '\n' | sed -e 's/ //g'; \
	cat object.ml | sed -n -e '/value colours/,/|]/p' | \
	tail +2 | sed -e 's/\[|//' -e 's/|]//' -e 's/"//g' | \
	tr ';' '\n' | sed -e 's/ //g'; \
	for i in $(SRCS); do \
	  camlp5r $(CAMLP5OPTS) pr_transl.cmo $$i; \
	done) | \
	sed -e 's/ $$/ ./' | grep -v '^$$' | \
	LC_ALL=C sort -f | uniq

rogue.out: $(EXT) $(OBJS)
	$(OCAMLC) -g $(LIBS) $(OBJS) -o rogue.out

rogue.opt: $(EXT) $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(LIBS:.cma=.cmxa) $(OBJS:.cmo=.cmx) -o $@

rogbot.out: $(ROGBOT_OBJS)
	$(OCAMLC) -g $(LIBS) $(ROGBOT_OBJS) -o $@

rogbot.opt: $(ROGBOT_OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(LIBS:.cma=.cmxa) $(ROGBOT_OBJS:.cmo=.cmx) -o $@

$(OBJS) $(OBJS:.cmo=.cmx): $(EXT)

ext/%.cmo: ext/%.ml
	camlp5r $(CAMLP5OPTS) -loc loc $< -o ext/$*.ppo
	$(OCAMLC) $(OCOPTS) -I $$(camlp5 -where) -c -impl ext/$*.ppo
	rm -f ext/$*.ppo

.SUFFIXES: .ml .mli .cmo .cmx .cmi .def .defo

.ml.cmo:
	$(CAMLP5) $(CAMLP5OPTS) $< -o $*.ppo
	$(OCAMLC) -g $(OCOPTS) -I $$(camlp5 -where) -c -impl $*.ppo
	rm -f $*.ppo

.ml.cmx:
	$(CAMLP5) $(CAMLP5OPTS) $< -o $*.ppo
	$(OCAMLOPT) $(OCOPTS) -I $$(camlp5 -where) -c -impl $*.ppo
	rm -f $*.ppo

.mli.cmi:
	$(CAMLP5) $(CAMLP5OPTS) $< -o $*.ppi
	$(OCAMLC) -g $(OCOPTS) -c -intf $*.ppi
	rm -f $*.ppi

.def.defo:
	@touch $@

include .depend
