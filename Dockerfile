FROM haskell

WORKDIR /app

RUN cabal update
COPY ./advent-of-code2023.cabal /app/advent-of-code2023.cabal
RUN cabal build --only-dependencies -j4

COPY ./input/* /app/

COPY . /app
RUN cabal install

CMD ["advent-of-code2023"]
