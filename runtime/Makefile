# Inputs
CXX ?= clang++
TARGET ?= MACOS_ARM64
AR ?= llvm-ar


INCLUDEDIR := include
BUILDDIR := build
SRCDIR := src
OBJS :=\
  $(BUILDDIR)/list.o\
  $(BUILDDIR)/tuple.o\
  $(BUILDDIR)/array.o\
  $(BUILDDIR)/bytearray.o\
  $(BUILDDIR)/date.o\
  $(BUILDDIR)/debugger.o\
  $(BUILDDIR)/dictionary.o\
  $(BUILDDIR)/record.o\
  $(BUILDDIR)/number.o\
  $(BUILDDIR)/math.o\
  $(BUILDDIR)/string.o\
  $(BUILDDIR)/char.o\
  $(BUILDDIR)/boolean.o\
  $(BUILDDIR)/event-loop.o\
  $(BUILDDIR)/process.o\
  $(BUILDDIR)/file.o\
  $(BUILDDIR)/directory.o\
  $(BUILDDIR)/stdio.o\
  $(BUILDDIR)/http.o\
  $(BUILDDIR)/url.o\
  $(BUILDDIR)/apply-pap.o\
  $(BUILDDIR)/stack.o\
  $(BUILDDIR)/gc.o\
  $(BUILDDIR)/crypto.o\
  $(BUILDDIR)/sha256.o\
  $(BUILDDIR)/md5.o
LIBTARGET := $(BUILDDIR)/libruntime.a

build: prepare build/libruntime.a print_done

prepare:
	@mkdir -p build

print_done:
	@echo "build done.\noutput: build/libruntime.a"

build/stack.o:
	@echo "build assembly module for stack.o"
	@if [ $(TARGET) = "MACOS_ARM64" ]; then\
		as "$(SRCDIR)/stack/stack-arm64-apple-darwin.s" -o "$(BUILDDIR)/stack.o";\
	elif [ $(TARGET) = "MACOS_X64" ]; then\
		nasm -f macho64 "$(SRCDIR)/stack/stack-x64-apple-darwin.s" -o "$(BUILDDIR)/stack.o";\
	elif [ $(TARGET) = "LINUX_X64" ] || [ $(TARGET) = "MUSL_X64" ]; then\
		nasm -f elf64 "$(SRCDIR)/stack/stack-x64-linux.s" -o "$(BUILDDIR)/stack.o";\
	elif [ $(TARGET) = "WIN_X64" ]; then\
		nasm -f win64 "$(SRCDIR)/stack/stack-x64-win64.s" -o "$(BUILDDIR)/stack.o";\
	fi

$(BUILDDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -DCURL_STATICLIB -DPCRE2_STATIC -I$(INCLUDEDIR) -std=c++14 -O3 -fPIC $(CXXFLAGS) -c $< -o $@

build/libruntime.a: $(OBJS)
	$(AR) rc $@ $^
