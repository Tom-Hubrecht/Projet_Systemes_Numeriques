BIN=simulator

all:
	@$(MAKE) -C src/
	mv src/$(BIN) ./

clean:
	rm -f $(BIN)
	@$(MAKE) -C src/ clean

clean_cmx:
	rm -f $(BIN)
	@$(MAKE) -C src/ clean_cmx

ship: clean
	tar -czf projet_sysnum.tgz src/ Makefile README.md

.PHONY: all clean clean_cmx

