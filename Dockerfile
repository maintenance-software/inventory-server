FROM debian:buster
RUN mkdir -p /opt/auth-provider/
# ARG BINARY_PATH
WORKDIR /opt/auth-provider
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libpq-dev
COPY target /opt/auth-provider
COPY config /opt/auth-provider/config
COPY webroot /opt/auth-provider/webroot
CMD ["/opt/auth-provider/auth-provider"]
