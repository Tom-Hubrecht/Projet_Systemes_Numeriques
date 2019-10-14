all:
	@$(MAKE) -C ./src/

clean:
	rm simulator
	@$(MAKE) -C ./src/ clean

.PHONY: all clean

