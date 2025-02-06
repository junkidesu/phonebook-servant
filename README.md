# Phonebook API

A simple REST API written with the <a href="https://www.servant.dev/">Servant</a> web framework. In older versions, SQLite was used as the DB for sake of simpicity (<a href="https://hackage.haskell.org/package/sqlite-simple-0.4.18.2">sqlite-simple</a>). The project was re-written to use [Beam](https://haskell-beam.github.io/beam/).

## Technologies Used

- [Haskell](https://www.haskell.org/)
- [Servant](https://docs.servant.dev/en/stable/index.html) (API)
- [PostgreSQL](https://www.postgresql.org/) (DB)
- [Beam](https://haskell-beam.github.io/beam/) (Interaction with the DB)
- [Docker](https://www.docker.com/) 
- [Minio](https://min.io/) (Storage)
- [GitHub Actions](https://docs.github.com/en/actions) (CI/CD)

## Getting Started

### Prerequisites
#### Build Tools

To build and run the application locally, ensure that the following are installed:

- [Stack](https://docs.haskellstack.org/en/stable/)
- [Cabal](https://cabal.readthedocs.io/en/stable/)
- [Docker](https://www.docker.com/)

Stack and Cabal can be installed either independently or with the [GHCup](https://www.haskell.org/ghcup/) tool.

#### Services

The application uses PostgreSQL for the DB and Minio for storage. Thus, running instances of PostgreSQL and Minio are required (either local or remote).

#### Environment Variables

See [`.env.sample`](./.env.sample) to see the environment variables that must be set. You can either place them in a `.env` file, or supply them directly to the executable.

### Build and Start Executable

At the root of the repository, run the following:

```sh
$ stack install
$ phonebook-exe
```

### Start in Container

You may start the application along with a local PostgreSQL server using Docker Compose.

```sh
$ docker compose -f docker-compose.dev.yml up
```

You still need to provide the necessary environment variables, though.

## Documentation

Once the application is started, the Swagger documentation of the API is available locally at http://localhost:3003/swagger-ui.
