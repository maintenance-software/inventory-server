FROM ubuntu:18.04
RUN mkdir -p /opt/inventory-server/
ARG BINARY_PATH
WORKDIR /opt/inventory-server
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libpq-dev
COPY "$BINARY_PATH" /opt/inventory-server
COPY static /opt/inventory-server/static
COPY config /opt/inventory-server/config
COPY webapps /opt/inventory-server/webapps
CMD ["/opt/inventory-server/inventory-server"]
