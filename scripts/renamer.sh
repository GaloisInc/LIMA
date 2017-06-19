find . -not \( -path './.git*' \) -type f -exec sed -i bak 's/slim/lima/g' \{\} \;
find . -not \( -path './.git*' \) -type f -exec sed -i bak 's/SLIM/LIMA/g' \{\} \;
find . -name '*bak' -delete
