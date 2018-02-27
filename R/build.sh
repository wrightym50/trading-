# There are two separate scripts one to generate the documentation
# and another to install the library. The installation process
# depends on the output of the documentation step. For some reason
# that I don't understand this doesn't always work if the two steps
# are done in the same R script. It seems that there's some caching
# going on so the documentation process has to complete to avoid
# that problem
rm NAMESPACE
./document.R
./install.R
