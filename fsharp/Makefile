STEPS = step0_repl.fs step1_read_print.fs step2_eval.fs #step3_env.fs \
	#step4_if_fn_do.fs step5_tco.fs step6_file.fs step7_quote.fs \
	#step8_macros.fs step9_try.fs stepA_mal.fs
MODULES = types.fs reader.fs printer.fs #env.fs core.fs error.fs readline.fs 

FSC = fsharpc
FSCFLAGS = --nologo
STEP_BINS = $(STEPS:%.fs=%.exe)
LAST_STEP_BIN = $(word $(words $(STEP_BINS)),$(STEP_BINS))
MAL_LIB = mal.dll

all: $(STEP_BINS) #mal.exe

mal.exe: $(LAST_STEP_BIN)
	cp $< $@

$(MAL_LIB): $(MODULES)
	$(FSC) $(FSCFLAGS) -a $(MODULES) -o $@

$(STEP_BINS): %.exe: %.fs $(MAL_LIB)
	$(FSC) $(FSCFLAGS) -r:$(MAL_LIB) $< -o $@

clean:
	rm -rf $(STEP_BINS) $(MAL_LIB) mal.exe

stats: $(MODULES) stepA_mal.fs
	@wc $^

stats-lisp: env.fs core.fs stepA_mal.fs
	@wc $^

.PHONY: all clean stats stats-lisp

