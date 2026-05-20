EXEC = similaridade

MAIN = similaridade.hs

# Regra padrão executada ao rodar apenas "make"
all: build

# Regra para compilar o projeto
build:
	ghc --make $(MAIN) -o $(EXEC)

# Regra para limpar os arquivos gerados pela compilação (.o, .hi e o executável)
clean:
	rm -f *.o *.hi $(EXEC)