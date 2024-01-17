# Phonebook API

A simple REST API written with the <a href="https://www.servant.dev/">Servant</a> web framework. For sake of simplicity, SQLite was used as the DB (<a href="https://hackage.haskell.org/package/sqlite-simple-0.4.18.2">sqlite-simple</a>).

## Prerequisites

<ul>
    <li>
        <a href="https://docs.haskellstack.org/en/stable">
            Stack
        </a>
    </li>
    <li>
        <a href="https://www.haskell.org/cabal/">
            Cabal
        </a>
    </li>
</ul>

## Getting Started

Build the project (will also install the required dependencies)

```
stack build
```

Start the server

```
stack exec phonebook-exe
```
