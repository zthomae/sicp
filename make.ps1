param (
  [string]$mode = "build"
)

switch ($mode) {
  "deps" {
    Set-Location sicp
    raco.exe pkg install --skip-installed
    Set-Location ..
    raco.exe pkg install --auto --skip-installed cover mock mock-rackunit
  }
  "build" {
    if (Test-Path .\docs) {
      Remove-Item .\docs -Force -Recurse
    }
    raco.exe scribble --htmls --dest . docs.scrbl
  }
  "cover" {
    $test_files = @(Get-ChildItem -Path *-test.rkt -Recurse -Force)
    $source_files = @($test_files -replace "-test.rkt", ".rkt")
    raco.exe cover $test_files $source_files
  }
}
