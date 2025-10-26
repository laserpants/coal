#!/bin/bash
cd $(dirname "$0") || exit 1
clang-format --style=Mozilla -i lib.c 
