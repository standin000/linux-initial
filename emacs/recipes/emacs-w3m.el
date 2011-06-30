(name :emacs-w3m
      :type http-tar
      :options ("xzf")
      :module "emacs-w3m"
      :url "http://cvs.namazu.org/emacs-w3m.tar.gz?view=tar"
      :build ("autoconf" "./configure" "make")
      :build/darwin `("autoconf" ,(concat "./configure --with-emacs=" el-get-emacs) "make")
      :info "doc"
      :features "w3m-load")
