# NPSETr 1.1.0

* ✨ Added functions for linear mixed models, GAMs, and GAMMs
* 🎨 Minor revisions to code format/structure as suggested by JP Schmit and Charlie Wainright
* 📝 Polished documentation

# NPSETr 1.0.0

* Added a `NEWS.md` file to track changes to the package.  
* Correction to `calc_change_cumu()`. Prior version used position to subtract off first pin reading from the rest, but had not first arranged by date - so the wrong reading could be subtracted. This version incorporates arranging so should be correct.  
* Changed package name to NPSETr to better reflect potential usage since the package also works with NETN and NCRN SET data. May incorporate SECN and SFCN in the future.
