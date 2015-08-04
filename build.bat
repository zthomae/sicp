@echo off
if exist solutions rd /s /q solutions
"C:\Program Files\Racket\raco.exe" scribble --htmls --dest . solutions.scrbl