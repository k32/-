PARSER = LinkGrammar

PREPROC = cpp -nostdinc -CC -P

build/tsar.index : build/tsar.out
	$(PARSER) -o build/tsar build/tsar.out

build :
	mkdir -p build

build/tsar.out : build ../ru.grammar tsar.dict
	$(PREPROC) -I .. -I $(dicts_dir) -o build/tsar.out tsar.dict

.PHONY : clean
clean :
	rm -rf build
