FROM haskell:8.10.7

WORKDIR /owen
COPY ./ .

RUN stack setup
RUN stack build

CMD [ "stack exec owenbot-exe" ]
