The packages from `init.el` require emacs 25+.

#### Compiling emacs 26 from source

```
wget https://ftp.gnu.org/pub/gnu/emacs/emacs-26.1.tar.xz
xz emacs-26.1.tar.xz
tar -xvf emacs-26.1.tar
yum install gnutls-devel.x86_64 ncurses-devel -y
make
```

as root:
```
make install 
```

git clone this repo.

add to `.bashrc`

```
alias em='emacs -nw'
```

Profit.
