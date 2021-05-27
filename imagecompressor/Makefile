##
## EPITECH PROJECT, 2021
## B-FUN-400-STG-4-1-compressor-auguste.thomann
## File description:
## Makefile
##

BINARY_PATH 	:=	$(shell stack path --local-install-root --allow-different-user)
NAME 			= 	imageCompressor

all:
	stack build --allow-different-user
	cp $(BINARY_PATH)/bin/$(NAME)-exe ./$(NAME)

clean:
	stack clean --allow-different-user
	rm -rf .stack-work
	rm -rf ImageCompressor.cabal
	rm -rf stack.yaml.lock
	rm -rf .vscode
	rm -rf src/.vscode

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re
