FROM --platform=linux/amd64 haskell:9.6.3 as build-stage

WORKDIR /opt/example

COPY . .

RUN stack install phonebook

CMD ["phonebook-exe"]
