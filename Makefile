all:
	@$(MAKE) -C ./src/

clean:
	rm simulator
	@$(MAKE) -C ./src/ clean

clean_all:
	rm simulator
	@$(MAKE) -C ./src/ clean_all

.PHONY: all clean

