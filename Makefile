BIN=simulator

all:
	@$(MAKE) -C src/
	mv src/$(BIN) ./

clean:
	rm -f $(BIN)
	@$(MAKE) -C src/ clean

clean_all:
	rm -f $(BIN)
	@$(MAKE) -C src/ clean_all

.PHONY: all clean

