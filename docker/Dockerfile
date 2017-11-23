# Build
# sudo docker build -t ar-base .
#
# Dev
# sudo docker rm c-ar; sudo docker run --name c-ar -v <srcdir>:/src-in ar-base /src/build && sudo docker commit c-ar ar
#
# Run
# sudo docker run -d --rm ar bash -c 'sleep 1; xvfb-run sol --automate --host <host>:12345'

FROM ubuntu:artful

ENV LC_ALL C.UTF-8

RUN echo 'Acquire::http { Proxy "http://apt-cache:3142"; };' >> /etc/apt/apt.conf.d/99proxy
RUN echo 'Dir::Cache "";' >> /etc/apt/apt.conf.d/99nocache
RUN echo 'Dir::Cache::archive "";' >> /etc/apt/apt.conf.d/99nocache
RUN echo 'Dir::Cache::pkgcache "";' >> /etc/apt/apt.conf.d/99nocache
RUN echo 'Dir::Cache::srcpkgcache "";' >> /etc/apt/apt.conf.d/99nocache

RUN apt-get -y update
RUN apt-get -y install curl rsync
RUN mkdir /src

RUN apt-get -y install \
  autoconf \
  automake \
  debhelper \
  dh-autoreconf \
  flex \
  libffi-dev \
  libgc-dev \
  libgmp-dev \
  libltdl-dev \
  libncurses5-dev \
  libreadline6-dev \
  libtool \
  libunistring-dev \
  pkg-config \
  texinfo \
  && mkdir /src/guile \
  && cd /src/guile \
  && curl -LO https://ftp.gnu.org/gnu/guile/guile-2.2.0.tar.gz \
  && tar xvf guile-2.2.0.tar.gz \
  && cd guile-2.2.0 \
  && mkdir build \
  && cd build \
  && ../configure \
  && make -j9 install \
  && rm -rf /src/guile

RUN apt-get -y install \
  desktop-file-utils \
  gnome-pkg-tools \
  gobject-introspection \
  intltool \
  libcairo2-dev \
  libcanberra-gtk3-dev \
  libgirepository1.0-dev \
  libglib2.0-dev \
  libgtk-3-dev \
  librsvg2-dev \
  libsm-dev \
  lsb-release \
  pkg-config \
  yelp-tools \
  libgconf2-dev \
  xvfb

ADD build /src
RUN chmod 744 /src/build
