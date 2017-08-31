
prg-out-dir := $(shell stack path --dist-dir)
prg-file := $(prg-out-dir)/build/bench/bench
prg-prof-file := $(prg-out-dir)/build/prof/prof

channel-params := 123456 1000000 10 +RTS -N2 -s -p -h -RTS
define save-profiling =
cp prof.hp $@.hp
cp prof.prof $@.prof
rm prof.hp
rm prof.prof
hp2pretty $@.hp
endef

.PHONY : prg
prg :
	stack build threads-post:exe:bench

# tag-prof-build-begin
.PHONY : prg-prof
prg-prof:
	stack build threads-post:exe:prof --executable-profiling --library-profiling  
# tag-prof-build-end

strict1-params := +RTS -N2 -s -RTS 5477555 100000 1

generated-resources/bench1.txt : $(prg-file) prg
	stack exec bench -- bench strict1 $(strict1-params) 0 > $@ 2> $@.mem

generated-resources/bench2.txt : $(prg-file) prg
	stack exec bench -- bench strict1 $(strict1-params) 1 > $@ 2> $@.mem

generated-resources/fusion-bench.txt : $(prg-file) prg
	stack exec bench -- bench fusion1 175557 500000 +RTS -N2 -RTS > $@ 

generated-resources/chan-bench.txt : $(prg-file) prg
	stack exec bench -- bench chan1 175557 500000 +RTS -N2 -RTS > $@ 

generated-resources/fusion-bench-prof.txt : $(prg-prof-file) prg-prof
	stack exec prof -- bench fusion1 175557 500000 +RTS -N2 -RTS > $@ 

generated-resources/channel1.txt : $(prg-prof-file) prg-prof
	stack exec prof -- test channels default $(channel-params) 2> $@
	$(save-profiling)

generated-resources/channel2.txt : $(prg-prof-file) prg-prof
	stack exec prof -- test channels bounded $(channel-params) 2> $@
	$(save-profiling)

generated-resources/channel3.txt : $(prg-prof-file) prg-prof
	stack exec prof -- test channels mvar $(channel-params) 2> $@
	$(save-profiling)

all : prg prg-prof generated-resources/channel1.txt generated-resources/channel2.txt generated-resources/channel3.txt generated-resources/bench1.txt generated-resources/bench2.txt generated-resources/fusion-bench.txt

.PHONY : clean
clean :
	stack clean

