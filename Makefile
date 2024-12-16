# Environment variable to enable or disable code which demonstrates the behavior change
# in Xcode 7 / Clang 3.7, introduced by DR1467 and described here:
# https://llvm.org/bugs/show_bug.cgi?id=23812
# Defaults to on in order to act as a warning to anyone who's unaware of the issue.
CXX = g++
CXXFLAGS = -std=c++17 -Wall -O2

ifneq ($(JSON11_ENABLE_DR1467_CANARY),)
CXXFLAGS += -DJSON11_ENABLE_DR1467_CANARY=$(JSON11_ENABLE_DR1467_CANARY)
endif

test: json11.cpp json11.hpp test.cpp
	$(CXX) $(CXXFLAGS) json11.cpp test.cpp -o test

clean:
	rm -f test

.PHONY: clean