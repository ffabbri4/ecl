export CFLAGS="-O0 -g"
export LDFLAGS='-g'

make distclean >/tmp/ecl-clean.txt 2>&1

./configure --prefix=/usr/local/gitlab-ecl --enable-unicode --enable-threads --with-__thread=no --enable-rpath --with-system-gmp --with-gmp-prefix=/usr/pkg --enable-boehm=included --with-dffi=system >/tmp/ecl-config.txt 2>&1

# Note: parallel build seems broken with ECL, don't use -j for now
nice make >/tmp/ecl-build.txt 2>&1

alert
