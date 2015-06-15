STEPS = step0_repl.fs #step1_read_print.fs step2_eval.fs step3_env.fs \
        #step4_if_fn_do.fs step5_tco.fs step6_file.fs step7_quote.fs \
	#step8_macros.fs step9_try.fs stepA_mal.fs

STEP_BINS = $(STEPS:%.fs=%.exe)
LAST_STEP_BIN = $(word $(words $(STEP_BINS)),$(STEP_BINS))

all: $(STEP_BINS) #mal

mal: $(LAST_STEP_BIN)
	cp $< $@

$(STEP_BINS): %.exe: %.fs $(MAL_LIB)
	fsharpc $<

clean:
	rm -rf $(STEP_BINS) mal

stats: types.fs error.fs readline.fs reader.fs printer.fs env.fs core.fs stepA_mal.fs
	@wc $^

stats-lisp: env.fs core.fs stepA_mal.fs
	@wc $^

.PHONY: all clean stats stats-lisp

