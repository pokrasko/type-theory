CC = ghc
NAMES = TT1 TT2 TT3
LIB = Expression Free Reader Substitution Writer
LIBFILES = $(patsubst %,%.hs,$(LIB))

all: $(NAMES)

clean:
	rm -f $(NAMES)
	rm -f $(patsubst %,%.o,$(NAMES))
	rm -f $(patsubst %,%.hi,$(NAMES))
	rm -f $(patsubst %,%.o,$(LIB))
	rm -f $(patsubst %,%.hi,$(LIB))

TT1: TT1.hs Expression.hs Reader.hs Writer.hs
	$(CC) $<

TT2: TT2.hs Expression.hs Free.hs Reader.hs
	$(CC) $<

TT3: TT3.hs Expression.hs Free.hs Reader.hs Substitution.hs Writer.hs
	$(CC) $<
