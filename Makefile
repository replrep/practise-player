.PHONY: cli

cli:
	sbcl --noinform --disable-ldb --lose-on-corruption \
		--no-sysinit --no-userinit --noprint --disable-debugger \
		--eval '(load "prepare-for-exe-dump.lisp")' \
		--eval "(sb-ext:save-lisp-and-die \"practise-player-cli\" :toplevel #'practise-player-cli:run :executable t)"


#		:compression 9)"




