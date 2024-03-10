FROM --platform=linux/amd64 haskell:9.6.3 as build-stage

WORKDIR /opt/app

COPY . .

RUN stack install --local-bin-path .

FROM --platform=linux/amd64 ubuntu:20.04

WORKDIR /bin/

RUN apt-get update -y --assume-yes && apt-get upgrade -y --assume-yes && apt-get install -y --assume-yes libpq-dev

COPY --from=build-stage /opt/app/phonebook-exe /bin/

ENTRYPOINT ["/bin/phonebook-exe"]
