cmake_minimum_required(VERSION 3.5)
project(phivm)

set(CMAKE_C_STANDARD 11)

include_directories(include)

file(GLOB SOURCES src/bmpio.c
                  src/common.c
                  src/draw.c
                  src/phimem.c
                  src/phistack.c
                  src/phivm.c)

add_executable(phivm ${SOURCES})
