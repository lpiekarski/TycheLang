all:
	stack ghc -- --make Tyche/Main.hs -o interpreter
grammar:
	happy -gca Tyche/Par.y
	alex -g Tyche/Lex.x

clean:
	-rm -f Tyche/*.log Tyche/*.aux Tyche/*.hi Tyche/*.o Tyche/*.dvi

distclean: clean
	-rm -f Tyche/Doc.* Tyche/Lex.* Tyche/Par.* Tyche/Layout.* Tyche/Skel.* Tyche/Print.* Tyche/Test.* Tyche/Abs.* Tyche/Test Tyche/ErrM.* Tyche/SharedString.* Tyche/ComposOp.* Tyche/tyche.dtd Tyche/XML.* Makefile*
		-rmdir -p Tyche/
