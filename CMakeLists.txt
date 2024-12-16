cmake_minimum_required(VERSION 3.12)
project(json11 VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

option(JSON11_BUILD_TESTS "Build unit tests" OFF)
option(JSON11_ENABLE_DR1467_CANARY "Enable canary test for DR 1467" OFF)

add_library(json11 json11.cpp)
target_include_directories(json11 PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_compile_options(json11 PRIVATE -fPIC -Wall)

if(JSON11_BUILD_TESTS)
  if(JSON11_ENABLE_DR1467_CANARY)
    add_definitions(-DJSON11_ENABLE_DR1467_CANARY=1)
  endif()
  
  add_executable(json11_test test.cpp)
  target_link_libraries(json11_test json11)
endif()

install(TARGETS json11 DESTINATION lib/${CMAKE_LIBRARY_ARCHITECTURE})
install(FILES "${CMAKE_CURRENT_SOURCE_DIR}/json11.hpp" 
        DESTINATION include/${CMAKE_LIBRARY_ARCHITECTURE})