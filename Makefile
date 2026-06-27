# $Id: Makefile,v 1.80 2018/07/20 11:42:16 deraugla Exp $

CAMLC=camlc
OCOPTS=
ROBOBJS=util.zo rob_position.zo rob_misc.zo rob_object.zo rob_monster.zo rob_path.zo rob_action.zo robot.zo
OBJS=$(ROBOBJS) ustring.zo efield.zo rfield.zo imisc.zo imonster.zo object.zo level.zo translate.zo curses.zo rogbotio.zo init.zo dialogue.zo misc.zo finish.zo monster.zo attack.zo move.zo use.zo main.zo 
LIBS=-custom unix.zo -lunix
ROGBOT_OBJS=$(ROBOBJS) rogbot.zo
SRCS=$(OBJS:.zo=.ml)
ROGBOT_SRCS=rogbot.ml
EXT=ext/pa_more.zo ext/pa_if_match.zo

all: out

out: rogue.out rogbot.out
	if [ -f rogue ]; then mv rogue rogue.bak; fi
	cp rogue.out rogue
	cp rogbot.out rogbot

clean:
	rm -f *.z[oi]
	rm -f rogue rogbot *.out

depend:
	./camldep.sh *.mli *.ml > .depend.new
	mv .depend .depend.old
	mv .depend.new .depend

i18n: $(EXT) ext/pr_transl.zo
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
	  camlp5r $(CAMLP5OPTS) pr_transl.zo $$i; \
	done) | \
	sed -e 's/ $$/ ./' | grep -v '^$$' | \
	LC_ALL=C sort -f | uniq

rogue.out: $(OBJS)
	$(CAMLC) -g $(LIBS) $(OBJS) -o rogue.out

rogbot.out: $(ROGBOT_OBJS)
	$(CAMLC) -g $(LIBS) $(ROGBOT_OBJS) -o $@

rogbot.opt: $(ROGBOT_OBJS:.zo=.cmx)
	$(CAMLCOPT) $(LIBS:.cma=.cmxa) $(ROGBOT_OBJS:.zo=.cmx) -o $@

ext/%.zo: ext/%.ml
	camlp5r $(CAMLP5OPTS) -loc loc $< -o ext/$*.ppo
	$(CAMLC) $(OCOPTS) -I $$(camlp5 -where) -c -impl ext/$*.ppo
	rm -f ext/$*.ppo

.SUFFIXES: .ml .mli .zo .zi .def .defo

.ml.zo:
	$(CAMLC) $(OCOPTS) -c $<

.mli.zi:
	$(CAMLC) -c $<

.def.defo:
	@touch $@

include .depend
