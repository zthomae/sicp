FROM jackfirth/racket:6.8
MAINTAINER Zach Thomae <zach@thomae.co>

RUN apt-get update
RUN apt-get install -y build-essential libglib2.0-dev libfontconfig1-dev libcairo2-dev libpango1.0-dev libjpeg62-dev libgtk2.0-dev xvfb
RUN raco pkg install --auto sicp

CMD ["/bin/bash"]