FROM haskell:9.6.3

WORKDIR /opt/example

COPY . .

RUN stack build

CMD ["stack", "exec", "phonebook-exe"]
