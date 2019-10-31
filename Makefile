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

.PHONY: all clean clean_cmx

