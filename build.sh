#!/bin/bash

rm -rf solutions/
raco scribble --htmls --dest . solutions.scrbl && open solutions/index.html

