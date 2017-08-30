
prg-out-dir := $(shell stack path --dist-dir)
prg-file := $(prg-out-dir)/build/threads-post/threads-post

# Otpions for profiling/RAM analysis.
# NOTE: disabled because they are in conflict with threadscope options
channel-params := 123456 10000pr00 10 +RTS -N2 -s -p -h -RTS
stack-params := --executable-profiling --library-profiling  --ghc-options="-fprof-auto"  
define save-profiling =
cp threads-post.hp $@.hp
cp threads-post.prof $@.prof
hp2pretty $@.hp
endef


# MAYBE specify two different executables in the cabal file

# Options for threadscope analysis
# NOTE: they are in conflict with 
# channel-params := 123456 1000000 10 +RTS -N2 -s -ls -RTS
# stack-params := --ghc-options="-eventlog"  
# define save-profiling =
# cp threads-post.eventlog $@.eventlog
# endef

.PHONY : prg
prg :
	stack build $(stack-params)

strict1-params := +RTS -N2 -s -RTS 5477555 100000 1

generated-resources/bench1.txt : $(prg-file) prg
	stack exec threads-post -- bench strict1 $(strict1-params) 0 > $@ 2> $@.mem

generated-resources/bench2.txt : $(prg-file) prg
	stack exec threads-post -- bench strict1 $(strict1-params) 1 > $@ 2> $@.mem

generated-resources/channel1.txt : $(prg-file) prg
	stack exec threads-post -- test channels default $(channel-params) 2> $@
	$(save-profiling)

generated-resources/channel2.txt : $(prg-file) pro
	stack exec threads-post -- test channels bounded $(channel-params) 2> $@
	$(save-profiling)

generated-resources/channel3.txt : $(prg-file) prg
	stack exec threads-post -- test channels mvar $(channel-params) 2> $@
	$(save-profiling)

all : prg generated-resources/channel1.txt generated-resources/channel2.txt generated-resources/channel3.txt generated-resources/bench1.txt generated-resources/bench2.txt

.PHONY : clean
clean :
	stack clean

