(:name :dpans2texi
      :type http-tar
      :options ("xzf")
      :module "dpans2texi"
      :url "http://users-phys.au.dk/harder/dpans2texi-1.05.tar.gz"
      :build ("./configure" "make wget" "make" "sudo make install"))
